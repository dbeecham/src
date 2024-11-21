with builtins;
with getFlake("nixpkgs");

lib.mkMerge ([
  ./a.nix
  ./b.nix
])
