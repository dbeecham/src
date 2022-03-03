{
  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "example";
      src = self;
      depsBuildBuild = [ 
        nixpkgs.legacyPackages.x86_64-linux.ragel 
      ];
      buildInputs = [ 
        nixpkgs.legacyPackages.x86_64-linux.z3
      ];
      installFlags = [ "DESTDIR=$(out)" "PREFIX=/" ];
    };
  };
}
