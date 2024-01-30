[![Test](https://github.com/cachix/cachix/workflows/Test/badge.svg)](https://github.com/cachix/cachix/actions)
[![Hackage](https://img.shields.io/hackage/v/cachix.svg)](https://hackage.haskell.org/package/cachix)

## [Cachix](https://cachix.org) - Nix binary cache hosting: Never build software twice.

<img src="https://user-images.githubusercontent.com/126339/130430964-7794b915-89d1-4b08-94be-3d32444dc8b2.png" width="200">

```
$ cachix --help
https://cachix.org command line interface

Usage: cachix [--hostname URI] [-c|--config CONFIGPATH] [-v|--verbose]
              (-V|--version)

  To get started log in to https://app.cachix.org

Available options:
  -h,--help                Show this help text
  --hostname URI           Host to connect to (default: https://cachix.org)
  -c,--config CONFIGPATH   Cachix configuration file
                           (default: "/home/domen/.config/cachix/cachix.dhall")
  -v,--verbose             Verbose mode
  -V,--version             Show cachix version

Available commands:
  authtoken                Configure an authentication token for Cachix
  config                   Manage configuration settings for cachix
  daemon                   Run a daemon that listens to push requests over a
                           unix socket
  generate-keypair         Generate a signing key pair for a binary cache
  push                     Upload Nix store paths to a binary cache
  import                   Import the contents of a binary cache from an
                           S3-compatible object storage service into Cachix,
                           e.g. s3://localhost:9000/mybucket
  pin                      Pin a store path to prevent it from being garbage
                           collected
  watch-exec               Run a command while watching /nix/store for newly
                           added store paths and upload them to a binary cache
  watch-store              Watch /nix/store for newly added store paths and
                           upload them to a binary cache
  use                      Configure a binary cache in nix.conf
  remove                   Remove a binary cache from nix.conf
  deploy                   Manage remote Nix-based systems with Cachix Deploy
```


## Installation

1. Install the Cachix client using Nix:

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

Also available as `pkgs.cachix` in [nixpkgs](https://github.com/NixOS/nixpkgs).

2. Login via https://www.cachix.org/api/v1/login to start using the service

## Development

Install Cachix from master:

```bash
nix-env -if https://github.com/cachix/cachix/tarball/master --substituters 'https://cache.nixos.org https://cachix.cachix.org' --trusted-public-keys 'cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY='
```

Or with Nix 2.4+:

```bash
nix profile install github:cachix/cachix/latest --accept-flake-config
```

## Support

- [Documentation](https://docs.cachix.org)
- [#cachix@matrix.org](https://matrix.to/#/#cachix:matrix.org)
- [Discord](https://discord.com/invite/naMgvexb6q)
- [support@cachix.org](mailto:support@cachix.org)

## Changelog

- [Cachix changelog](./cachix/CHANGELOG.md) for the command
- [API changelog](./cachix-api/CHANGELOG.md) (Haskell)
