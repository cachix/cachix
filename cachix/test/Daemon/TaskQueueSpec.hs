module Daemon.TaskQueueSpec where

import Cachix.Daemon.TaskQueue
import Cachix.Daemon.Types.TaskQueue (Prioritized (..))
import Protolude
import Test.Hspec

data PriorityTask = PriorityTask
  { priority :: Int,
    taskId :: Int
  }
  deriving stock (Eq, Show)

instance Ord PriorityTask where
  compare t1 t2 = compare (priority t1) (priority t2)

spec :: Spec
spec = do
  describe "TaskQueue" $ do
    describe "basic operations" $ do
      it "creates an empty queue" $ do
        queue <- atomically newTaskQueue
        result <- atomically $ tryWriteTask queue (PriorityTask 1 1)
        result `shouldBe` Just True

      it "writes and reads a single task" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          writeTask q (PriorityTask 1 1)
          return q
        task <- atomically $ readTask queue
        task `shouldBe` Just (PriorityTask 1 1)

      it "tryWriteTask returns Just True on success" $ do
        queue <- atomically newTaskQueue
        result <- atomically $ tryWriteTask queue (PriorityTask 1 1)
        result `shouldBe` Just True

      it "tryWriteTask returns Just False when closed" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          closeTaskQueue q
          return q
        result <- atomically $ tryWriteTask queue (PriorityTask 1 1)
        result `shouldBe` Just False

    describe "priority ordering" $ do
      it "returns higher priority items first" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          writeTask q (PriorityTask 1 1)
          writeTask q (PriorityTask 3 2)
          writeTask q (PriorityTask 2 3)
          return q

        task1 <- atomically $ readTask queue
        task1 `shouldBe` Just (PriorityTask 3 2)

        task2 <- atomically $ readTask queue
        task2 `shouldBe` Just (PriorityTask 2 3)

        task3 <- atomically $ readTask queue
        task3 `shouldBe` Just (PriorityTask 1 1)

      it "maintains FIFO ordering within same priority" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          writeTask q (PriorityTask 1 1)
          writeTask q (PriorityTask 1 2)
          writeTask q (PriorityTask 1 3)
          return q

        task1 <- atomically $ readTask queue
        task1 `shouldBe` Just (PriorityTask 1 1)

        task2 <- atomically $ readTask queue
        task2 `shouldBe` Just (PriorityTask 1 2)

        task3 <- atomically $ readTask queue
        task3 `shouldBe` Just (PriorityTask 1 3)

      it "handles mixed priorities with FIFO within each priority level" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          writeTask q (PriorityTask 2 1)
          writeTask q (PriorityTask 1 2)
          writeTask q (PriorityTask 2 3)
          writeTask q (PriorityTask 3 4)
          writeTask q (PriorityTask 1 5)
          return q

        task1 <- atomically $ readTask queue
        task1 `shouldBe` Just (PriorityTask 3 4)

        task2 <- atomically $ readTask queue
        task2 `shouldBe` Just (PriorityTask 2 1)

        task3 <- atomically $ readTask queue
        task3 `shouldBe` Just (PriorityTask 2 3)

        task4 <- atomically $ readTask queue
        task4 `shouldBe` Just (PriorityTask 1 2)

        task5 <- atomically $ readTask queue
        task5 `shouldBe` Just (PriorityTask 1 5)

    describe "queue lifecycle" $ do
      it "writeTask is a no-op when queue is closed" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          closeTaskQueue q
          writeTask q (PriorityTask 1 1)
          return q
        task <- atomically $ readTask queue
        task `shouldBe` Nothing

      it "readTask returns Nothing when queue is closed and empty" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          closeTaskQueue q
          return q
        task <- atomically $ readTask (queue :: TaskQueue PriorityTask)
        task `shouldBe` Nothing

      it "can read existing items after queue is closed" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          writeTask q (PriorityTask 1 1)
          writeTask q (PriorityTask 2 2)
          closeTaskQueue q
          return q

        task1 <- atomically $ readTask queue
        task1 `shouldBe` Just (PriorityTask 2 2)

        task2 <- atomically $ readTask queue
        task2 `shouldBe` Just (PriorityTask 1 1)

        task3 <- atomically $ readTask queue
        task3 `shouldBe` Nothing

      it "close is idempotent" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          closeTaskQueue q
          closeTaskQueue q
          closeTaskQueue q
          return q
        task <- atomically $ readTask (queue :: TaskQueue PriorityTask)
        task `shouldBe` Nothing

    describe "edge cases" $ do
      it "handles single item queue correctly" $ do
        queue <- atomically $ do
          q <- newTaskQueue
          writeTask q (PriorityTask 5 1)
          return q
        task <- atomically $ readTask queue
        task `shouldBe` Just (PriorityTask 5 1)

      it "maintains ordering with large number of items" $ do
        let items = [1 .. 100] :: [Int]
        queue <- atomically $ do
          q <- newTaskQueue
          forM_ items $ \i -> writeTask q (PriorityTask (i `mod` 5) i)
          return q

        results <- replicateM 100 (atomically $ readTask queue)
        let justResults = catMaybes results
        length justResults `shouldBe` 100

        let grouped = groupBy (\a b -> priority a == priority b) justResults
        forM_ grouped $ \grp -> do
          let priorities = map priority grp
          priorities `shouldSatisfy` \case
            [] -> True
            (p : rest) -> all (== p) rest

    describe "sequence number overflow" $ do
      it "handles wraparound from maxBound to minBound correctly" $ do
        let maxSeq = maxBound :: Int32
            minSeq = minBound :: Int32
            taskOld = Prioritized (PriorityTask 1 1) maxSeq
            taskNew = Prioritized (PriorityTask 1 2) minSeq
        taskOld `compare` taskNew `shouldBe` GT

      it "maintains FIFO across overflow boundary" $ do
        let nearMax = maxBound - 2 :: Int32
            tasks =
              [ Prioritized (PriorityTask 1 4) (nearMax + 3),
                Prioritized (PriorityTask 1 2) (nearMax + 1),
                Prioritized (PriorityTask 1 1) nearMax,
                Prioritized (PriorityTask 1 3) (nearMax + 2)
              ]
        let sorted = sortBy (comparing Down) tasks
        map (taskId . pTask) sorted `shouldBe` [1, 2, 3, 4]

      it "circular comparison works for sequences far apart" $ do
        let s1 = 100 :: Int32
            s2 = s1 + 1000000
            taskOld = Prioritized (PriorityTask 1 1) s1
            taskNew = Prioritized (PriorityTask 1 2) s2
        taskOld `compare` taskNew `shouldBe` GT
