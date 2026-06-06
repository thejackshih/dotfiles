let
  sources = import ./npins;
  pkgs = import sources.nixpkgs {};
  darwin = import sources.nix-darwin {};
in
pkgs.mkShell {
  packages = [
    darwin.darwin-rebuild
  ];
  shellHook = ''
alias install="sudo darwin-rebuild switch \
-I darwin=${builtins.toString sources.nix-darwin.outPath} \
-I darwin-config=${builtins.toString ./. + "/default.nix"} \
-I nixpkgs=${builtins.toString sources.nixpkgs.url}"
'';
}
