{
  outputs = { nixpkgs, ... }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation {
      name = "psycopghw";
      src = ./.;
      buildInputs = with nixpkgs.legacyPackages.x86_64-linux; [
        python.withPackages ( pkgs: with pkgs; [ psycopg2 ] );
      ];
    };
  };
}
