{ 
    outputs = { self, nixpkgs }: { 
        defaultPackage.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation { 
            name = "hw"; 
            src = ./.; 
            buildInputs = [  
              nixpkgs.legacyPackages.x86_64-linux.linux.dev
            ]; 
            xinstallFlags = [ "DESTDIR=$(out)" "PREFIX=/" ]; 
        }; 
    }; 
}
