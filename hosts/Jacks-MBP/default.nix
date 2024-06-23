{ pkgs, ...}:
{
  nix = {
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  services.nix-daemon.enable = true;
  programs.zsh.enable = true;

  users.users.jack = {
    name = "jack";
    home = "/Users/jack";
  };

  networking.hostName = "Jacks-MacBook-Pro";

  fonts.packages = with pkgs;[
    _0xproto
    _0xpropo
  ];

  system = {
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
      emacs-pgtk
    ];
    home.file = {
      emacs = {
        source = ../../ext/init.el;
        target = ".emacs.d/init.el";
        enable = true;
      };
    };
    programs = {
      zsh.enable = true;
      direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
      };
    };
  };
}
