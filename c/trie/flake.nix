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



      # other useful stuff

      # specify cflags (not sure if this is better then makeFlagsArray?)
      NIX_CFLAGS_COMPILE = toString ([
        "-DSOMETHING"
        "-DSOMETHING_ELSE"
      ]);

      # additional arguments to make (no spaces, see below)
      makeFlags = [ "PREFIX=$(out)" ];

      # same as makeFlags, but only used in the build phase (no spaces, see below)
      buildFlags = [ "CONFIG_USE_SOMETHING=y" ];

      # additional arguments to make (use this instead of makeFlags, buildFlags
      # if the arguments contain spaces since those cannot be passed through
      # env variables)
      preBuild = ''
        makeFlagsArray+=(
          CFLAGS="-O0 -g"
          LDFLAGS="-lfoo -lbar"
          CONFIG_SOMETHING=y
        )
        buildFlagsArray+=(
          CONFIG_SOMETHING="foo bar"
        )
      '';

      # specific configure phase (same with buildPhase)
      configurePhase = ''
        runHook preConfigure
        ./configure --something
        runHook postConfigure
      '';

      # multiple or specific outputs
      outputs = [ "bin" "out" "info" "doc" "dev" ];
    };

    devShell = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      packages = [
        nixpkgs.legacyPackages.x86_64-linux.kconfig-frontends
      ] ++ self.defaultPackage.x86_64-linux.depsBuildBuild ++ self.defaultPackage.x86_64-linux.buildInputs;
    };

  };

}
