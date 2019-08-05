#pragma once

// inline-c-cpp doesn't seem to handle namespace operator or template
// syntax so we help it a bit for now. This definition can be inlined
// when it is supported by inline-c-cpp.
typedef nix::ref<nix::Store> refStore;
typedef nix::ref<const nix::ValidPathInfo> refValidPathInfo;
typedef nix::PathSet::iterator PathSetIterator;
