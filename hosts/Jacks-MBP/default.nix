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

  fonts = {
    fontDir.enable = true;
    fonts = [];
  };

  system = {
    defaults = {
      NSGlobalDomain = {
        _HIHideMenuBar = false;
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
      {
        name = "emacs-plus@30";
        args = [];
        link = true;
      }
    ];
    casks = [
      "1password"
      "adobe-digital-editions"
      "calibre"
      "alfred"
      "rectangle"
      "font-ibm-plex"
    ];
    taps = [
      "d12frosted/emacs-plus"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-drivers"
      "homebrew/cask-fonts"
      "homebrew/core"
      "homebrew/services"
    ];
  };
  
}
    
