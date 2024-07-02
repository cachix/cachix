module Daemon.ProtocolSpec where

import Cachix.Client.Daemon.Protocol (splitMessages)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "split messages" $ do
    forM_ testCases $ \(input, output) ->
      it (show input) $
        splitMessages input `shouldBe` output

testCases :: [(ByteString, ([ByteString], ByteString))]
testCases =
  [ ("hello\n", (["hello"], "")),
    ("hello\nthere\n", (["hello", "there"], "")),
    ("hello\nthere", (["hello"], "there")),
    ("", ([], "")),
    ("\n", ([], "")),
    ("\n\n", ([], "")),
    ("\nhello\n", (["hello"], ""))
  ]
