#!/usr/bin/env bash

set -euo pipefail

get_version () {
  local version
  version=$(sed -ne '/version/ s/version:\s\+\([0-9]\+\.[0-9]\+\.[0-9]\+\)/\1/p' "$1")
  echo "$version"
}

set_version() {
  sed -i -E "/version/ s/(version:\s+)([0-9]+\.[0-9]+\.[0-9]+)/\1$1/" "$2"
}

cachix_version=$(get_version './cachix/cachix.cabal')
cachix_api_version=$(get_version './cachix-api/cachix-api.cabal')
printf 'Current versions\n'
printf '    Cachix:     %s\n' "$cachix_version"
printf '    Cachix API: %s\n' "$cachix_api_version"
read -pr 'Enter the new Cachix version: ' new_cachix_version
printf '\n'

set_version "$new_cachix_version" './cachix/cachix.cabal'
set_version "$new_cachix_version" './cachix-api/cachix-api.cabal'

new_version_string="v$new_cachix_version"
git add cachix/cachix.cabal cachix-api/cachix-api.cabal
git commit --quiet --message "Release cachix $new_version_string"
git tag "$new_version_string" --edit --message "Release $new_version_string"

printf '\nUpdated to %s\n' "$new_version_string"
printf 'Check that the commits and tags are correct. Then push to GitHub:\n\n'
printf 'git push && git push --tags'
