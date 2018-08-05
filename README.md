
# https://cachix.org api and cli interface

[![Build Status](https://travis-ci.com/cachix/cachix.svg?branch=master)](https://travis-ci.com/cachix/cachix)
[![Hackage](https://img.shields.io/hackage/v/cachix.svg)](https://hackage.haskell.org/package/cachix)
[![Cachix](https://img.shields.io/badge/cachix-cachix-blue.svg)](https://cachix.cachix.org)


<img src="https://cachix.org/images/logo.png" width="400">

Binary Cache as a Service - Build Nix packages once and share them for good.

```
$ cachix --help
cachix.org command interface

Usage: cachix [-h|--host URI] [-v|--verbose] (COMMAND | (-V|--version))
  Sign into https://cachix.org to get started.

Available options:
  -h,--help                Show this help text
  -h,--host URI            Host to connect to (default: https://cachix.org)
  -v,--verbose             Verbose mode
  -V,--version             Show cachix version

Available commands:
  authtoken                Configure token for authentication to cachix.org
  create                   Create a new binary cache
  push                     Upload Nix store paths to the binary cache
  use                      Configure nix.conf to enable binary cache during
                           builds
```


## Installation

1. Install Cachix client using Nix:

```
    $ nix-env -iA cachix -f https://github.com/NixOS/nixpkgs/tarball/db557aab7b690f5e0e3348459f2e4dc8fd0d9298
```

2. Login via https://www.cachix.org/api/v1/login to start using the service

## Development

Install Cachix from master:

```
    $ nix-env -if https://github.com/cachix/cachix/tarball/master --substituters https://cachix.cachix.org --trusted-public-keys cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=
```

## Support

- [#cachix@freenode](https://webchat.freenode.net/?channels=cachix) IRC
- [domen@enlambda.com](mailto:domen@enlambda.com)
- https://github.com/cachix/feedback for open discussion about the service
