# https://cachix.org api and client

[![Build Status](https://travis-ci.org/cachix/cachix.svg?branch=master)](https://travis-ci.org/cachix/cachix)
[![Hackage](https://img.shields.io/hackage/v/cachix.svg)](https://hackage.haskell.org/package/cachix)

Binary cache as a service - Build Nix packages once and share them for good.


## Installation

1. Install Cachix client

   $ nix-env -if https://github.com/cachix/cachix/tarball/master --substituters https://cachix.cachix.org --trusted-public-keys cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=

2. Login via https://www.cachix.org/api/v1/login to start using the service


## Support

- #cachix on Freenode IRC
- domen@enlambda.com
- https://github.com/cachix/feedback for open discussion about the service
