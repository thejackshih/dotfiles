{ pkgs, ...}:
{
  nix = {
    package = pkgs.nixVersions.nix_2_21;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  services.nix-daemon.enable = true;
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
      emacs-unstable-pgtk
    ];
    home.file = {
      # vim = {
      #   source = ../../vim/vimrc;
      #   target = ".vimrc";
      #   enable = true;
      # };
      # neovim = {
      #   source = ../../nvㄍim/init.vim;
      #   target = ".config/nvim/init.vim";
      #   enable = true;
      # };
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

  # fonts = {
  #   fontDir.enable = true;
  #   fonts = with pkgs; [];
  # };

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
      # "fvm"
      # "cocoapods"
      # "dart"
    ];
    casks = [
      "1password"
      "adobe-digital-editions"
      "betterdisplay"
      "calibre"
      "raycast"
      "font-fira-code"
    ];
    taps = [
      # "dart-lang/dart"
      # "leoafarias/fvm"
      "homebrew/bundle"
      "homebrew/cask-fonts"
      "homebrew/services"
    ];
  };
}
