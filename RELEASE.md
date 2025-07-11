# Release Checklist

- Edit [CHANGELOG.md](./cachix/CHANGELOG.md)

  Diff: https://github.com/cachix/cachix/compare \
  Fetch messages: `git log --ancestry-path <old_tag>..HEAD`
- Update the [README.md](./README.md) with any `cachix --help` changes
- Bump cachix/cachix.cabal and cachix-api/cachix-api.cabal versions and commit
- Push to GitHub `git push`
- Update link to `https://cachix.org/api/v1/install`
