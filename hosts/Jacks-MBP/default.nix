{ pkgs, ...}:
{
  services.nix-daemon.enable = true;
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  programs.zsh.enable = true;

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  users.users.jack = {
    name = "jack";
    home = "/Users/jack";
  };

  home-manager.users.jack = {pkgs, ...}: {
    home.stateVersion = "23.11";
    home.packages = with pkgs; [
      coreutils
      coreutils-prefixed
      emacs29-pgtk
    ];
    home.file = {
      vim = {
        source = ../../vim/vimrc;
        target = ".vimrc";
        enable = false;
      };
      neovim = {
        source = ../../nvim/init.vim;
        target = ".config/nvim/init.vim";
        enable = true;
      };
    };
  };

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [];
  };

  networking.hostName = "Jacks-MacBook-Pro";

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

    brews = [
      "fvm"
      "cocoapods"
      "dart"
    ];
    casks = [
      "1password"
      "adobe-digital-editions"
      "betterdisplay"
      "calibre"
      "raycast"
      "virtual-desktop-streamer"
      "android-studio"
      "google-chrome"
      "utm"
    ];
    taps = [
      "d12frosted/emacs-plus"
      "dart-lang/dart"
      "leoafarias/fvm"
      "homebrew/bundle"
      "homebrew/cask-fonts"
      "homebrew/services"
    ];
  };
}
