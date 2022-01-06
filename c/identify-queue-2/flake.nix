{

  inputs = {
    # updated 2021-12-01
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "c30bbcfae7a5cbe44ba4311e51d3ce24b5c84e1b";
    };
  };


  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "example";
      src = self;
      depsBuildBuild = [ 
        nixpkgs.legacyPackages.x86_64-linux.ragel
      ];
      buildInputs = [ 
      ];
      installFlags = [ "DESTDIR=$(out)" "PREFIX=/" ];
    };

    devShell = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      packages = [
        nixpkgs.legacyPackages.x86_64-linux.kconfig-frontends
      ] ++ self.defaultPackage.x86_64-linux.depsBuildBuild ++ self.defaultPackage.x86_64-linux.buildInputs;
    };

  };

}
