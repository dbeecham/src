with builtins;
with getFlake("nixpkgs");

{

  a = lib.mergeAttrsList ([
        (import ./a.nix)
        (import ./b.nix)
      ]);

  b = lib.mergeAttrsList (map import [
    ./a.nix ./b.nix
  ]);

}
