{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  
  outputs = { self, nixpkgs, home-manager, darwin, emacs-overlay }: 
    let
      overlays = [emacs-overlay.overlay];
    in {
      darwinConfigurations."Jacks-MBP" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = overlays;
          }
          ./hosts/Jacks-MBP/default.nix
        ];
      };
    };
}
