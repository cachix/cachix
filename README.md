<img src="https://cachix.org/images/logo.png" width="500">

# https://cachix.org api and cli interface

[![Build Status](https://travis-ci.com/cachix/cachix.svg?branch=master)](https://travis-ci.com/cachix/cachix)
[![Hackage](https://img.shields.io/hackage/v/cachix.svg)](https://hackage.haskell.org/package/cachix)

Binary Cache as a Service - Build Nix packages once and share them for good.


## Installation

1. Install Cachix client using Nix 2.0 or greater:

```
    $ nix-env -if https://github.com/cachix/cachix/tarball/master --substituters https://cachix.cachix.org --trusted-public-keys cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=
```

2. Login via https://www.cachix.org/api/v1/login to start using the service


## Support

- #cachix on Freenode IRC
- domen@enlambda.com
- https://github.com/cachix/feedback for open discussion about the service
