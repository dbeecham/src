{
  outputs = {self, nixpkgs }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.buildRustCrate {
      crateName = "small";
      version = "0.1.0";
      src = ./.;
    };
  };
}
