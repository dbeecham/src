{
  outputs = { self, nixpkgs, ... }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "hw";
      src = ./.;
      buildInputs = with nixpkgs.legacyPackages.x86_64-linux; [
        (python38.withPackages ( pkgs: with pkgs; [ psycopg2 ] ))
      ];
    };
  };
}
