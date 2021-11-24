{
  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "sqlite0";
      src = ./.;
      buildDepsDeps = [ 
        nixpkgs.legacyPackages.x86_64-linux.ragel
      ];
      buildInputs = [ 
        nixpkgs.legacyPackages.x86_64-linux.sqlite
      ];
      installFlags = [ "DESTDIR=$(out)" "PREFIX=/" ];
    };
  };
}
