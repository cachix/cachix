https://github.com/cachix/cachix/blob/master/README.md

## Pin information

Get a pin's metadata and latest revision:

```sh
cachix pins get mycache 'mypin:1.0.0'
cachix pins get mycache 'mypin:1.0.0' --json
```

The JSON output contains the pin name, creation time, retention policy, and
`lastRevision`. The latest revision includes its store path, revision number,
creation time, artifacts, and `orphaned` status. An orphaned revision has no
store path metadata in the cache. Its store path lacks the name suffix and its
artifact links are unavailable.

Pin names are literal: a colon does not select a revision. Public caches allow
anonymous reads. Private caches require an authentication token with read access.
A missing pin produces a nonzero exit status.

The command uses the existing pin listing API and selects the exact pin name
locally. It does not require a server upgrade.
