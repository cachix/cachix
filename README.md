[![Test](https://github.com/cachix/cachix/workflows/Test/badge.svg)](https://github.com/cachix/cachix/actions)
[![Hackage](https://img.shields.io/hackage/v/cachix.svg)](https://hackage.haskell.org/package/cachix)

## [Cachix](https://cachix.org) - Nix binary cache hosting: Never build software twice.

<img src="https://user-images.githubusercontent.com/126339/130430964-7794b915-89d1-4b08-94be-3d32444dc8b2.png" width="200">

```
$ cachix --help
https://cachix.org command line interface

Usage: cachix [--host URI] [-c|--config CONFIGPATH] [-v|--verbose] 
              (COMMAND | (-V|--version))
  To get started log in to https://app.cachix.org

Available options:
  -h,--help                Show this help text
  --host URI               Host to connect to (default: https://cachix.org)
  -c,--config CONFIGPATH   Cachix configuration
                           file (default: "/home/ielectric/.config/cachix/cachix.dhall")
  -v,--verbose             Verbose mode
  -V,--version             Show cachix version

Available commands:
  authtoken                Configure authentication token for communication to
                           HTTP API
  generate-keypair         Generate signing key pair for a binary cache
  push                     Upload Nix store paths to a binary cache
  watch-exec               Run a command while it's running watch /nix/store for
                           newly added store paths and upload them to a binary
                           cache
  watch-store              Indefinitely watch /nix/store for newly added store
                           paths and upload them to a binary cache
  use                      Configure a binary cache by writing nix.conf and
                           netrc files

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
    $ nix-env -if https://github.com/cachix/cachix/tarball/master --substituters 'https://cache.nixos.org https://cachix.cachix.org' --trusted-public-keys 'cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY='
```

Or with Nix 2.4+:

```
    $ nix profile install github:cachix/cachix/latest
```

## Support

- [Documentation](https://docs.cachix.org)
- [#cachix@matrix.org](https://matrix.to/#/#cachix:matrix.org) IRC
- [support@cachix.org](mailto:support@cachix.org)

## Changelog

- [Cachix changelog](./cachix/CHANGELOG.md) for the command
- [API changelog](./cachix-api/CHANGELOG.md) (Haskell)
