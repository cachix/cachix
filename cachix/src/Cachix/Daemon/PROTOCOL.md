# Cachix Daemon Protocol

This document describes the socket protocol for communicating with the Cachix Daemon.

## Overview

The Cachix Daemon accepts connections on a Unix domain socket and communicates via newline-delimited JSON messages. Clients can push store paths to a binary cache, monitor push progress, and coordinate daemon shutdown.

**Default socket location**:
- `$XDG_RUNTIME_DIR/cachix/cachix-daemon.sock` (e.g., `/run/user/$UID/cachix/cachix-daemon.sock` on systemd systems)
- Falls back to `$XDG_CACHE_HOME/cachix/cachix-daemon.sock` if `XDG_RUNTIME_DIR` is not set

**Environment override**: `CACHIX_DAEMON_SOCKET`

## Transport

- **Connection**: Unix domain socket (AF_UNIX)
- **Message format**: UTF-8 JSON
- **Delimiter**: Newline character (`\n`) after each message
- **Buffering**: Line-buffered
- **Liveness detection**: Client sends pings every ~2 seconds; considers daemon stalled if no pong received within 20 seconds

## Message Format

All messages are JSON objects terminated with a newline character. The protocol uses Haskell's `Aeson` library encoding, which means union types are encoded as:

```json
{
  "tag": "ConstructorName",
  "contents": { /* optional data */ }
}
```

For simple variants without data, only the `tag` field is present:

```json
{ "tag": "ConstructorName" }
```

## Client → Daemon Messages

### ClientPushRequest

Request the daemon to push store paths to a binary cache.

```json
{
  "tag": "ClientPushRequest",
  "contents": {
    "storePaths": ["/nix/store/abc123-package-1.0", "/nix/store/def456-package-1.0"],
    "subscribeToUpdates": true
  }
}
```

**Fields**:
- `storePaths` (array of strings): List of Nix store paths to push
- `subscribeToUpdates` (boolean):
  - `true`: Receive `DaemonPushEvent` messages for progress updates
  - `false`: Fire-and-forget; daemon queues the push but sends no events

**Response**: The daemon responds with `DaemonPushEvent` messages (if subscribed) and eventually `DaemonExit` or error messages.

### ClientStop

Request the daemon to shutdown gracefully.

```json
{ "tag": "ClientStop" }
```

The daemon will:
1. Stop accepting new push requests
2. Complete in-flight operations
3. Send `DaemonExit` with exit code 0

### ClientPing

Liveness check. The daemon responds with `DaemonPong`.

```json
{ "tag": "ClientPing" }
```

The client sends ping messages to monitor daemon health. The daemon should respond immediately with `DaemonPong`.

## Daemon → Client Messages

### DaemonPong

Response to `ClientPing`.

```json
{ "tag": "DaemonPong" }
```

### DaemonExit

Notifies the client that the daemon is shutting down.

```json
{
  "tag": "DaemonExit",
  "contents": {
    "exitCode": 0,
    "exitMessage": null
  }
}
```

**Fields**:
- `exitCode` (integer): Exit code (0 for success, non-zero for failure)
- `exitMessage` (string or null): Optional human-readable exit message

### DaemonPushEvent

Progress update for a push operation.

```json
{
  "tag": "DaemonPushEvent",
  "contents": {
    "eventTimestamp": "2025-11-07T12:34:56.789123Z",
    "eventPushId": "550e8400-e29b-41d4-a716-446655440000",
    "eventMessage": {
      "tag": "PushStarted"
    }
  }
}
```

**Fields**:
- `eventTimestamp` (ISO8601 string): UTC timestamp of the event
- `eventPushId` (UUID string): Unique identifier for this push request
- `eventMessage` (object): The specific event type (see below)

**Event types**:

#### PushStarted
Emitted when push operation begins.

```json
{ "tag": "PushStarted" }
```

#### PushStorePathAttempt
Attempt to push a single store path, possibly a retry.

```json
{
  "tag": "PushStorePathAttempt",
  "contents": [
    "/nix/store/abc123-package-1.0",
    1048576,
    { "retryCount": 0 }
  ]
}
```

**Fields**:
- `contents[0]` (string): Store path being pushed
- `contents[1]` (integer): Size in bytes
- `contents[2]` (object): Retry status
  - `retryCount` (integer): Number of retries attempted (0 on first attempt)

#### PushStorePathProgress
Upload progress for a store path.

```json
{
  "tag": "PushStorePathProgress",
  "contents": [
    "/nix/store/abc123-package-1.0",
    524288,
    1048576
  ]
}
```

**Fields**:
- `contents[0]` (string): Store path being pushed
- `contents[1]` (integer): Bytes uploaded so far
- `contents[2]` (integer): Total bytes to upload

#### PushStorePathDone
Store path successfully pushed.

```json
{
  "tag": "PushStorePathDone",
  "contents": ["/nix/store/abc123-package-1.0"]
}
```

**Fields**:
- `contents[0]` (string): Store path that was pushed

#### PushStorePathFailed
Failed to push a store path after all retries.

```json
{
  "tag": "PushStorePathFailed",
  "contents": [
    "/nix/store/abc123-package-1.0",
    "HTTP 403: Access Denied"
  ]
}
```

**Fields**:
- `contents[0]` (string): Store path that failed
- `contents[1]` (string): Error message

#### PushFinished
All store paths have been processed (succeeded or failed).

```json
{ "tag": "PushFinished" }
```

### DaemonError

Indicates an error in processing the client message.

```json
{
  "tag": "DaemonError",
  "contents": {
    "tag": "UnsupportedCommand",
    "contents": "Unknown message type"
  }
}
```

**Error types**:
- `UnsupportedCommand`: The client sent an unknown message type

## Typical Message Flow

### Push with Progress Updates

```
Client                           Daemon
  |                               |
  |-- ClientPushRequest --------> |
  |    (subscribeToUpdates: true) |
  |                               |
  | <--- DaemonPushEvent ------   |
  |      (PushStarted)            |
  |                               |
  | <--- DaemonPushEvent ------   |
  |      (PushStorePathAttempt)   |
  |                               |
  | <--- DaemonPushEvent ------   |
  |      (PushStorePathProgress)  |
  |                               |
  | <--- DaemonPushEvent ------   |
  |      (PushStorePathProgress)  |
  |                               |
  | <--- DaemonPushEvent ------   |
  |      (PushStorePathDone)      |
  |                               |
  | <--- DaemonPushEvent ------   |
  |      (PushFinished)           |
  |                               |
```

### Ping/Pong (Liveness Check)

```
Client          Daemon
  |              |
  |-- Ping ----> |
  |              |
  | <--- Pong -- |
  |              |
```

The client periodically sends ping messages to detect if the daemon has become unresponsive. If the client does not receive a pong response within 20 seconds, it considers the daemon connection stalled and raises an error. The client sends pings approximately every 2 seconds.

### Graceful Shutdown

```
Client          Daemon
  |              |
  |-- Stop ----> |
  |              |
  | <-- Exit --- |
  |              |
```

The daemon stops accepting new push requests and completes in-flight operations before sending `DaemonExit`.

## Error Handling

- **Invalid JSON**: The daemon logs an error and ignores the malformed message
- **Unsupported message type**: The daemon sends `DaemonError` with `UnsupportedCommand`
- **Connection loss**: The daemon removes the client socket and cleans up associated state
- **Client-side timeout**: If the client does not receive a daemon pong response within 20 seconds, it raises a `SocketStalled` error
- **Stale client detection**: Clients should implement ping/pong handling to detect daemon staleness

## Implementation Notes

### Message Parsing

Messages may arrive in partial chunks. Use the newline character (`\n`) as a delimiter to split complete messages. The `splitMessages` function in `Protocol.hs` shows how to handle buffering:

```haskell
splitMessages :: ByteString -> ([ByteString], ByteString)
```

This function splits incoming data on newlines and returns:
- A list of complete messages
- Remaining incomplete data to be prepended to the next chunk

### Subscriptions

When `subscribeToUpdates` is `false`, the client should not expect push events. The push is queued and processed asynchronously, but the client receives no progress information.

### Push Request IDs

Each `DaemonPushEvent` includes a `eventPushId` UUID. Clients receiving events from multiple concurrent pushes should use this ID to correlate events with their original requests.

### Retries

The daemon automatically retries failed pushes with exponential backoff. The `PushStorePathAttempt` event includes the retry count so clients can track retry attempts.

## References

- `Cachix.Daemon.Protocol`: Core message type definitions
- `Cachix.Daemon.Types.PushEvent`: Push event type definitions
- `Cachix.Daemon.Client`: Example client implementation
- `cachix/test/Daemon/ProtocolSpec.hs`: Protocol serialization tests
