{
    description = "My first Flake. Yippee!";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
        home-manager.url = "github:nix-community/home-manager/release-23.11";
        home-manager.inputs.nixpkgs.follows = "nixpkgs";
    };

    # inputs hace referencia a los parámetros pasados en los corchetes
    outputs = inputs @ { self, nixpkgs, home-manager, ... }: 
    let
        lib = nixpkgs.lib;
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
        # user = "delta";
    in {
        nixosConfigurations = {
            nixos = lib.nixosSystem {
                inherit system;
                modules = [ ./system/configuration.nix ];
            };
        };
        homeConfigurations = {
            delta = home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                modules = [ ./home-config/home.nix ];
            };
        };
    };
}
