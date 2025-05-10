{ ... }:
let
  sources = import ./npins;
  pkgs = import sources.nixpkgs {};
  home-manager-nix-darwin-module = (import sources.home-manager {}).path + "/nix-darwin";
in
{
  imports = [home-manager-nix-darwin-module];
  nix = {
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    nixPath = [
      "nixpkgs=${sources.nixpkgs.url}"
      "darwin=${sources.nix-darwin.outPath.outPath}"
      "darwin-config=${builtins.toString ./. + "/default.nix"}"
    ];
  };

  programs.zsh.enable = true;

  users.users.jack = {
    name = "jack";
    home = "/Users/jack";
  };

  networking.hostName = "Jacks-MacBook-Pro";

  fonts.packages = with pkgs;[
    fira
    fira-code
  ];

  system = {
    stateVersion = 5;
    defaults = {
      NSGlobalDomain = {
        _HIHideMenuBar = false;
        AppleFontSmoothing = 0;
        KeyRepeat = 2;
        InitialKeyRepeat = 15;
        ApplePressAndHoldEnabled = false;
      };
      dock = {
        autohide = true;
        mru-spaces = false;
        orientation = "bottom";
        persistent-apps = [
          "/System/Applications/Launchpad.app"
        ];
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };


  homebrew = {
    enable = true;
    onActivation.autoUpdate = true;
    onActivation.cleanup = "uninstall";

    brews = [];
    casks = [
      "1password"
      "adobe-digital-editions"
      "betterdisplay"
      "calibre"
      "raycast"
      "sidequest"
      "surfshark"
      "multiviewer-for-f1"
    ];
    taps = [
      "homebrew/bundle"
      "homebrew/cask-fonts"
      "homebrew/services"
      "d12frosted/emacs-plus"
    ];
  };


  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.jack = {pkgs, ...}: {
    home.stateVersion = "24.11";
    home.packages = with pkgs; [
      coreutils
      npins
      emacs
    ];
    home.file = {
      emacs = {
        source = ../../ext/init.el;
        target = ".emacs.d/init.el";
        enable = false;
      };
    };
    programs = {
      zsh.enable = true;
      direnv = {
        enable = false;
        enableZshIntegration = false;
        nix-direnv.enable = false;
      };
    };
  };
}
