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

```sh
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

2. Login via https://www.cachix.org/api/v1/login to start using the service

## Development

Install Cachix from master:

```sh
nix-env -if https://github.com/cachix/cachix/tarball/master --substituters 'https://cache.nixos.org https://cachix.cachix.org' --trusted-public-keys 'cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY='
```

Or with Nix 2.4+:

```sh
nix profile install github:cachix/cachix/latest
```

### Dev setup for VSCodium

1. [Enable flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes)

1. Set up [direnv](https://nix.dev/tutorials/declarative-and-reproducible-developer-environments#setting-up-direnv) - complete steps `1` and `2`. 

1. Enter this repo:
    ```sh
    git clone https://github.com/cachix/cachix
    cd cachix
    ```

1. Allow `direnv` work here. This will trigger `nix develop` when you `cd` into the directory:
    ```sh
    direnv allow
    ```

1. Build the project with stack for the first time (required for HLS to work):
    ```sh
    stack build
    ```

1. Open [VSCodium](https://vscodium.com/) with necessary extensions, shell tools, and settings:
    ```sh
    codium .
    ```

1. You may want to check the installed extensions to find out how to use them

## Support

- [Documentation](https://docs.cachix.org)
- [#cachix@matrix.org](https://matrix.to/#/#cachix:matrix.org)
- [support@cachix.org](mailto:support@cachix.org)

## Changelog

- [Cachix changelog](./cachix/CHANGELOG.md) for the command
- [API changelog](./cachix-api/CHANGELOG.md) (Haskell)