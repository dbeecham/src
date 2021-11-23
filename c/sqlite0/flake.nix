{
  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "sqlite0";
      src = ./.;
      nativeBuildInputs = [ nixpkgs.legacyPackages.x86_64-linux.ragel ];
      buildInputs = [ 
        
        nixpkgs.legacyPackages.x86_64-linux.sqlite3

        ];
      installFlags = [ "DESTDIR=$(out)" "PREFIX=/" ];
    };
  };
}
