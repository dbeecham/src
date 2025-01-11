with builtins;
with getFlake("nixpkgs");
lib.evalModules {
    modules = [
        ({lib, config, options, specialArgs}: {
            options.foo = lib.mkOption {
                type = with lib.types; str;
                default = "no";
                apply = x: x + "!";
            };
        })

        { foo = "yes"; }
    ];
}
