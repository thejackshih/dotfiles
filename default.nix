{ lib, config, options, pkgs, ... }:
let
  sources = import ./npins;
  home-manager-nix-darwin-module = (import sources.home-manager {}).path + "/nix-darwin";
in
{
  imports = [home-manager-nix-darwin-module];

  nixpkgs = {
    source = sources.nixpkgs;
  };

  nix = {
    package = pkgs.nixVersions.latest;
    channel.enable = false;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    nixPath = [
      {
        nixpkgs = "${sources.nixpkgs.url}";
        darwin = "${sources.nix-darwin.outPath.outPath}";
        darwin-config = "${config.environment.darwinConfig}";
      }
    ];
  };

  environment = {
    darwinConfig = "${builtins.toString ./. + "/default.nix"}";
    shells = [
      pkgs.zsh
      pkgs.bash
      "/etc/profiles/per-user/jack/bin/bash"
      "/etc/profiles/per-user/jack/bin/zsh"
    ];
  };

  networking = {
    hostName = "Jacks-MacBook-Pro";
  };

  system = {
    stateVersion = 6;
    primaryUser = "jack";
    defaults = {
      NSGlobalDomain = {
        _HIHideMenuBar = false;
        AppleFontSmoothing = 0;
        ApplePressAndHoldEnabled = false;
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        "com.apple.trackpad.scaling" = 2.0;
      };
      WindowManager = {
        EnableTiledWindowMargins = false;
      };
      dock = {
        autohide = true;
        autohide-time-modifier = 0.3;
        expose-animation-duration = null;
        launchanim = true;
        mru-spaces = false;
        orientation = "bottom";
        persistent-apps = [
          "/System/Applications/Apps.app"
          "/Users/jack/Applications/Home Manager Apps/Emacs.app"
        ];
        persistent-others = [];
        tilesize = 32;
        show-recents = false;
        wvous-br-corner = 1;
      };
      finder = {
        _FXSortFoldersFirst = true;
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        FXEnableExtensionChangeWarning = false;
        FXPreferredViewStyle = "clmv";
        ShowPathbar = true;
      };
      menuExtraClock = {
        Show24Hour = true;
        ShowAMPM = false;
        ShowDate = 2;
        ShowDayOfWeek = false;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
    };
    taps = [
      "d12frosted/emacs-plus"
    ];
    brews = [];
    casks = [
      "1password"
      "adobe-digital-editions"
      "calibre"
      "multiviewer-for-f1"
      "surfshark"
      "firefox"
      "obs"
      "betterdisplay"
    ];
  };

  fonts = {
    packages = with pkgs;[
      _0xproto
      sarasa-gothic
      # mplus-outline-fonts.githubRelease
      # maple-mono.variable
      # maple-mono.CN
    ];
  };

  programs = {
    bash = {
      enable = true;
    };
    zsh = {
      enable = true;
    };
  };

  users = {
    users.jack = {
      name = "jack";
      home = "/Users/jack";
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.jack = { config, lib, pkgs, ... }: {
      home = {
        stateVersion = "25.11";
        packages = with pkgs; [
          coreutils
          (lib.hiPrio pkgs.uutils-coreutils-noprefix) # `lib.hiPrio` is used to avoid potential conflict with `coreutils-full` (also see https://discourse.nixos.org/t/how-to-use-uutils-coreutils-instead-of-the-builtin-coreutils/8904/15?u=malix)
          emacs
          npins
          gemini-cli
          nixd
          nix-search-cli
        ];
        file = {
          emacs-early-init = {
            enable = true;
            source = config.lib.file.mkOutOfStoreSymlink "${builtins.toString ./. + "/emacs/early-init.el"}";
            target = ".emacs.d/early-init.el";
          };
          emacs-init = {
            enable = true;
            source = config.lib.file.mkOutOfStoreSymlink "${builtins.toString ./. + "/emacs/init.el"}";
            target = ".emacs.d/init.el";
          };
        };
      };

      services = {
        emacs = {
          enable = false;
        };
      };

      programs = {
        git = {
          enable = true;
          userEmail = "randomdize@gmail.com";
          userName = "Jack Shih";
        };
        bash = {
          enable = true;
          bashrcExtra =
            ''
              export LC_ALL="en_US.UTF-8"
            '';
          shellAliases = lib.mkMerge [
            {
              reset-launchpad = "rm $(getconf DARWIN_USER_DIR)com.apple.dock.launchpad/db/*;killall Dock";
            }
            (lib.mkIf config.services.emacs.enable {
              restart-emacs = "launchctl kickstart -k gui/$(id -u)/org.nix-community.home.emacs";
            })
          ];
        };
        zsh = {
          enable = true;
          shellAliases = lib.mkMerge [
            {
              reset-launchpad = "rm $(getconf DARWIN_USER_DIR)com.apple.dock.launchpad/db/*;killall Dock";
            }
            (lib.mkIf config.services.emacs.enable {
              restart-emacs = "launchctl kickstart -k gui/$(id -u)/org.nix-community.home.emacs";
            })
          ];
        };
        direnv = {
          enable = true;
          enableBashIntegration = true;
          enableZshIntegration = true;
          nix-direnv.enable = true;
        };
      };
    };
  };
}
