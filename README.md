[![Test](https://github.com/cachix/cachix/workflows/Test/badge.svg)](https://github.com/cachix/cachix/actions)
[![Hackage](https://img.shields.io/hackage/v/cachix.svg)](https://hackage.haskell.org/package/cachix)

## [Cachix](https://cachix.org) - Hosted Nix binary caches: Build Nix packages once and share them for good

<img src="https://raw.githubusercontent.com/cachix/cachix/master/logo.png" width="200">

# CLI interface

```
$ cachix --help
cachix.org command interface

Usage: cachix [--host URI] [-c|--config CONFIGPATH] [-v|--verbose] (COMMAND |
              (-V|--version))
  Sign into https://cachix.org to get started.

Available options:
  -h,--help                Show this help text
  --host URI               Host to connect to (default: https://cachix.org)
  -c,--config CONFIGPATH   Cachix configuration
                           file (default: "~/.config/cachix/cachix.dhall")
  -v,--verbose             Verbose mode
  -V,--version             Show cachix version

Available commands:
  authtoken                Configure token for authentication to cachix.org
  generate-keypair         Generate keypair for a binary cache
  push                     Upload Nix store paths to the binary cache
  use                      Configure nix.conf to enable binary cache during
                           builds

```


## Installation

1. Install Cachix client using Nix:

```
    $ nix-env -iA cachix -f https://cachix.org/api/v1/install
```

2. Login via https://www.cachix.org/api/v1/login to start using the service

## Development

Install Cachix from master:

```
    $ nix-env -if https://github.com/cachix/cachix/tarball/master --substituters https://cachix.cachix.org --trusted-public-keys cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=
```

## Support

- [Documentation][https://docs.cachix.org]
- [#cachix@freenode](https://webchat.freenode.net/?channels=cachix) IRC
- [support@cachix.org](mailto:support@cachix.org)
- [Gitter](https://gitter.im/cachix/Lobby)

## Changelog

- [Cachix changelog](./cachix/CHANGELOG.md) for the command
- [API changelog](./cachix-api/CHANGELOG.md) (Haskell)
