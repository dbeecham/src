{
  outputs = {self, nixpkgs }: {
    defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.buildRustCrate {
      crateName = "natstest";
      version = "0.0.1";
      src = ./.;
    };
  };
}
