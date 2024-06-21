{
  description = "My first Flake, yippe!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
  let
    lib = nixpkgs.lib;
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    # user = "delta";

  in {
    nixosConfigurations = {
      # SHOULD BE YOUR SYSTEM'S HOSTNAME 
      nixos = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [ ./system/configuration.nix ];
      };
    };

    # homeConfigurations = {
    #   # SHOULD BE THE USER'S NAME
    #   delta = home-manager.lib.homeManagerConfiguration {
    #     inherit pkgs;
    #     modules = [ ./home-manager/home.nix ];
    #   };
    # };
  };
}
