# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.4.1] - 2023-03-31

### Fixed

- Open Nix database in read-only mode instead of immutable.

## [1.4] - 2023-03-26

### Changed

- Rewrote C++ bindings to Nix in Haskell, reducing the closure and making it easy to
statically compile Cachix.

## [1.3.3] - 2023-03-18

### Fixed

- Fix watch-exec exiting too soon.

## [1.3.2] - 2023-03-17

### Fixed

- Fix build on GHC 9.4

## [1.3.1] - 2023-03-09

### Fixed

- Signal handling in watch-exec & watch-store commands.

## [1.3] - 2023-03-06

### Added

- Upload nars using multiple parts, improving bandwidth speed and parallelization.

### Changed

- Bump defaults jobs to 8.
- Improve 401 erros by showing the body of backend response.
- Deploy: improve agent startup and shutdown.

### Fixed

- Unblock interrupt signal on darwin (possibly also Linux).
- Deploy: wait for the logs to finish when activating.

## [1.2] - 2023-01-06

### Added

- NARs are now streamed without invoking an external process, so if
you have a lot of small files, there should be some significant performance improvements

- `cachix deploy activate` now by default waits for the agents to be deployed, displays the logs and exists if any deployments fail.
  If you'd like to keep the old behaviour pass `--async` flag.

### Changed

- We no longer pin Nix to speed up version bumps of Nix

### Fixed

- A number of improvements to stability of the websocket connection used in cachix deploy.

- Fixed a regression in `cachix deploy activate`, requiring `--agent` flag that should be optional.

- Fixed a C++ crash that would sometimes happen on exceptions in rare conditions.

## [1.1] - 2022-12-16

### Added 

- Use ZSTD compresion method by default and allow overriding it via `--compression-method` back to XZ. You can also change the default permanently on your binary cache settings page.

- Cachix Deploy got a complete rewrite with correctness in mind and reliablity.

- Cachix Deploy agent now supports --bootstrap that awaits a new agent to spawn and then shuts down.

- Cachix Deploy now supports [home-manager](https://github.com/nix-community/home-manager/pull/3380)

- Generated NixOS module now uses the naming of Nix settings as introduced in NixOS 22.05.

## [1.0.1] - 2022-09-24

### Added 

- `cachix config`: allow setting hostname

## [1.0.0] - 2022-09-06

- Cachix Deploy: auto rollback if the agent can't connect to the backend service anymore

- Cachix Deploy: allow specifying `rollbackScript`: https://docs.cachix.org/deploy/reference

- Cachix Deploy: report `system` and closure size from the agent

- Cachix Deploy: lock deployments so there's only one active at the time

- Cachix Deploy: disable negative narinfo caching

## [0.8.1] - 2022-07-26

- Cachix Deploy: retry exceptions every 1s instead of exponentially 

### Fixed

## [0.8.0] - 2022-07-10

### Fixed

- Cachix Deploy: properly fix disconnection issues
- Cachix Deploy: deployments are now a separate process so cachix agent can be upgraded at any time


## [0.7.1] - 2022-06-27

### Fixed

- Previous release didn't filter out all invalid paths as intended

## [0.7.0] - 2022-01-12

### Added 

- Cachix Deploy support

### Fixed

- #386: use /run/current-system/nixos-version to check if we're running NixOS
- Filter out invalid paths when pushing

### Changed

- watch-store command: use systemd notifications while shutting down

## [0.6.1] - 2021-06-21

### Fixed

- Fix "Empty Stream" error
- #379 & #362: redirect cachix output to stderr
- #380: support having tilde in filepath of the config file

### Changed

- Factor out Store into hercules-ci-cnix-store
- `cachix authtoken` reads from stdin if no token is provided
- improved error message in case nar db hash mismatch happens
- Support LTS-18.0 Stackage

## [0.6.0] - 2021-01-08

### Changed

- Watching nix store doesn't push .drv files anymore
- `cachix push -w` has been moved to `cachix watch-store`
- `cachix create` has been removed
- Retries now take a multiple of seconds instead of multiple of 100ms

### Added

- watch-exec allows to run a command and push all new store paths added meanwhile
- GHC 8.10 support

### Fixed

- Watching /nix/store now uses queue to bulk query what is missing in binarycache and
  a queue for pushing
- Instructions for NixOS trusted users were inaccurate
- Retry fetching binary cache

## [0.5.1] - 2020-11-09

### Fixed

- Regression: use auth token when using signing key with private caches 
- Configure netrc even if cachix config doesn't exist

## [0.5.0] - 2020-11-06

### Added

- Allow specifying output directory to write nix.conf and netrc files. 
- Allow pushing without a Signing key using only auth token
- Allow setting auth token via `$CACHIX_AUTH_TOKEN` shell variable

### Fixed

- Watch store command now pushes the full closure of each store path
- Support groups when parsing trusted-users from nix.conf

## [0.3.8] - 2020-06-03

### Added

- `cachix push --omit-deriver` if you'd like not to reveal Deriver field to Cachix

### Changed

- Retries are now exponential to offload the server a bit

### Fixed

- #308: Test failure due to dependency bump

- Pretty print exceptions

- Create nixos directory before checking if it's writable

- A number of error messaging improvements:

  * Don't suggest creating a cache when generating keypair as it's the wrong order

  * #290: Explain what is going on when there's no signing key

  * #262: improve instructions when on NixOS and the user is untrusted


## [0.3.7] - 2020-03-12

### Added

- Allow specifying number of parallel jobs when pushing: cachix push mycache -j8

### Changed

- Print stderr during streaming of nars    

### Fixed

- Use max up to 8 cores to prevent performance issues
  on machines with lots of cores

- Get deriver and references from C++ structures rather
  then shelling out (slight performance improvement)

- #251: Assert nar hash before creating narinfo
    
  Fixes rare but annoying bug of "bad nar archive" from Nix.
  Never managed to reproduce. One theory is that the path disappears as
  its deleted by GC or nix-store --delete.

- Print correct path when passing --nixos-folder and encountering an error


## [0.3.6] - 2020-02-22

### Fixed

- #275: support LTS-13.x and GHC 8.2.2

## [0.3.5] - 2020-01-10

### Added

- #265: print store path size before compressing/pushing

### Fixed

- #258: don't swallow exceptions while --watch-store
- NixOS: instruct to use sudo

## [0.3.4] - 2019-10-01

### Fixed

- #240: push: prefer store paths as command arguments over stdin 

  Some programming languages like go and nodejs always open
  stdin pipe, which confuses cachix to think there's stdin content.
  
  Sadly checking stdin for contents is tricky since we get
  into the whole buffering mess.

## [0.3.3] - 2019-09-25

### Fixed

- #236: fix hang during push
- #233: push: if multiple caches exist, pick the latest signing key

## [0.3.2] - 2019-09-16

- #232: brownbag release due to a bug in release process

## [0.3.1] - 2019-09-16

### Fixed

- #229: correctly assert write permissions for NixOS
- #228: flush stderr before exiting

## [0.3.0] - 2019-09-03

### Changed

- #222 allow NixOS installation only as root
- #215 LTS-14.0 support and for development replace stack2nix with haskell.nix
- #216 add bulk store path querying as a performance optimization
- #222 drop support for Nix 2.0.1 or lower

### Fixed

- #222 Provide guidance if /etc/nixos is not writable
- #222 Human friendly exception reporting
- #212 Use C++ to determine closure, avoids shell argument limit size

## [0.2.1] - 2019-07-05

### Added

- #180 support servant 0.16 @domenkozar
- #200 add `cachix push --compression-level` and default to 2 @roberth
  This should cut pushing time by a significant factor.

### Changed

- #194 remove shadowed -h option @roberth
- #180 reduce retries limit to 3 @domenkozar
- #192 extract `cachix push` into a proper library module @roberth

### Fixed

- #187 create possibly missing directories for netrc @domenkozar
- #185 fix command line completions @roberth
- #196 improve build time by working around a GHC bug @roberth

## [0.2.0] - 2019-03-04

### Added

- #71 new command `cachix generate-keypair` with
  clearer instructions how to handle the secrets @domenkozar
- #24 #168 Private binary cache support @domenkozar @roberth

### Changed

- #158 #160 #166 #91 Improve NixOS experience by
  writing out nixos modules @domenkozar
- #170 Get rid of amazonka dependency @domenkozar

## [0.1.3] - 2018-11-26

### Changed

- #77 retry transient HTTP exceptions @domenkozar
- #151 prevent mingled  store paths output @domenkozar
- #141 prevent unncessary warning about /etc/nix missing @domenkozar
- #142 ditch hpack @domenkozar

## [0.1.2] - 2018-09-27

### Changed

- #132 error handling for readProcess invocations @domenkozar
- #130 only warn about not supporting groups if user is not trusted @domenkozar
- #128 Generate https://cache.nixos.org when run as root on NixOS @yegortimoschenko
- #121 bail out if narSize is 0 @domenkozar
- #123 support passing --config @domenkozar
- #123 no more spurious warning messages when using "cachix use" @domenkozar
- #105 pass https://cache.nixos.org explicitly @domenkozar

## [0.1.1] - 2018-08-03

### Added

- #102 Nix 1.0 support @domenkozar

### Changed

- #105 Always specify cachix.nixos.org on NixOS @domenkozar
- #98, #95 Use LTS-12.X and Servant 0.14.1 @domenkozar

## [0.1.0.2] - 2018-07-06

### Changed

- #95 Upgrade to servant-0.14.1 @domenkozar

## [0.1.0.1] - 2018-07-05

### Changed

- #92 Add build-tool-depends where needed @domenkozar
- #90 HLint fixes @domenkozar
- #86 Improve Cabal description and synopsis @domenkozar
- #87 Support fsnotify 0.3.x @domenkozar

## [0.1.0.0] - 2018-07-01

### Added

- Initial release @domenkozar

[Unreleased]: https://github.com/cachix/cachix/compare/v0.3.8...HEAD
[0.3.8]: https://github.com/cachix/cachix/compare/v0.3.7...v0.3.8
[0.3.7]: https://github.com/cachix/cachix/compare/v0.3.6...v0.3.7
[0.3.6]: https://github.com/cachix/cachix/compare/v0.3.5...v0.3.6
[0.3.5]: https://github.com/cachix/cachix/compare/v0.3.4...v0.3.5
[0.3.4]: https://github.com/cachix/cachix/compare/v0.3.3...v0.3.4
[0.3.3]: https://github.com/cachix/cachix/compare/v0.3.2...v0.3.3
[0.3.2]: https://github.com/cachix/cachix/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/cachix/cachix/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/cachix/cachix/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/cachix/cachix/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/cachix/cachix/compare/v0.1.3...v0.2.0
[0.1.3]: https://github.com/cachix/cachix/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/cachix/cachix/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/cachix/cachix/compare/v0.1.0.2...v0.1.1
[0.1.0.2]: https://github.com/cachix/cachix/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/cachix/cachix/compare/v0.1.0.0...v0.1.0.1
