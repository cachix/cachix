# NarinfoBatch Technical Specification

## Overview

The `NarinfoBatch` module implements a batching system for narinfo (Nix Archive Info) queries to optimize performance by reducing network overhead and improving cache efficiency.

## Core Functionality

### Batching Strategy
The system accumulates multiple individual path queries into batches and processes them together. Batches are triggered by:
- **Size threshold**: When accumulated paths reach the configured limit
- **Time threshold**: After a maximum wait time has elapsed  
- **Immediate mode**: For zero-latency processing when batching is disabled

### Caching
- Maintains a TTL-based cache of positive narinfo lookups
- Only caches paths that exist (not missing paths)
- Automatic expiration and cleanup of stale entries
- Configurable size limits and TTL duration

### Request Flow
1. Submit requests with store paths to check
2. Check cache for each path to separate cached vs uncached paths
3. Accumulate uncached paths for batch processing
4. Process batch when threshold is met
5. Update cache with results
6. Distribute responses back to original requests

## Configuration

Key configuration options:
- `nboMaxBatchSize`: Maximum paths per batch (default: 100)
- `nboMaxWaitTime`: Maximum wait time before processing (default: 2s) 
- `nboCacheTTL`: Cache entry lifetime (default: 5min)
- `nboMaxCacheSize`: Maximum cache entries (default: unlimited)

## Usage

```haskell
-- Initialize
manager <- newNarinfoBatchManager options callback
startBatchProcessor manager processBatchFunction

-- Submit requests  
submitBatchRequest manager requestId storePaths

-- Shutdown
stopBatchProcessor manager
```

The external `processBatchFunction` receives a list of store paths and returns a tuple of `([StorePath], [StorePath])` representing all paths in the dependency closure and missing paths respectively.
