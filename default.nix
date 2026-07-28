{ lib, config, options, pkgs, ... }:
let
  sources = import ./npins;
  home-manager-nix-darwin-module = (import sources.home-manager {}).path + "/nix-darwin";
  emacs-overlay = (import sources.emacs-overlay);
  my-username = "jack";
in
{
  imports = [home-manager-nix-darwin-module];

  nixpkgs = {
    source = sources.nixpkgs;
    overlays = [emacs-overlay];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    package = pkgs.lixPackageSets.latest.lix;
    channel = {
      enable = false;
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    nixPath = [
      {
        nixpkgs = "${sources.nixpkgs.url}";
        darwin = "${sources.nix-darwin.outPath}";
        darwin-config = "${config.environment.darwinConfig}";
      }
    ];
  };

  environment = {
    darwinConfig = "${builtins.toString ./. + "/default.nix"}";
    shells = [
      pkgs.zsh
      pkgs.bash
      "/etc/profiles/per-user/${my-username}/bin/bash"
      "/etc/profiles/per-user/${my-username}/bin/zsh"
    ];
  };

  system = {
    stateVersion = 6;
    primaryUser = my-username;
    defaults = {
      NSGlobalDomain = {
        _HIHideMenuBar = false;
        AppleFontSmoothing = 0;
        ApplePressAndHoldEnabled = false;
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        "com.apple.trackpad.scaling" = 3.0;
      };
      ".GlobalPreferences" = {
        "com.apple.mouse.scaling" = 3.0;
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
      CustomUserPreferences = {
        NSGlobalDomain = {
          # Hide macOS Tahoe's menu icons
          # ref: https://512pixels.net/2026/03/hide-macos-tahoes-menu-icons-with-this-one-simple-trick/
          NSMenuEnableActionImages = false;
        };
        "com.apple.symbolichotkeys" = {
          AppleSymbolicHotKeys = {
            # Select Previous Input Source
            "60" = {
              enabled = true;
              value = {
                # cmd + space
                parameters = [32 49 1048576];
                type = "standard";
              };
            };
            # Select next source in input menu
            "61" = {
              enabled = false;
            };
            # Show Spotlight Search
            "64" = {
              enabled = true;
              value = {
                # option + cmd + space
                parameters = [32 49 1572864];
                type = "standard";
              };
            };
            # Show Finder search window
            "65" = {
              enabled = false;
            };
          };
        };
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
    activationScripts = {
      postActivation = {
        # run activateSettings -u as user to apply keyboard shortcut change without logout
        # ref: https://zameermanji.com/blog/2021/6/8/applying-com-apple-symbolichotkeys-changes-instantaneously/
        text = ''
          sudo -u ${my-username} /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
        '';
      };
    };
  };

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "none";
    };
    taps = [
      "d12frosted/emacs-plus"
    ];
    brews = [];
    casks = [
      "1password"
      "adobe-digital-editions"
      "calibre"
      "multiviewer"
      "surfshark"
      "firefox"
      "obs"
      "betterdisplay"
      "nvidia-geforce-now"
      "microsoft-edge"
      "antigravity"
    ];
  };

  fonts = {
    packages = with pkgs;[
      sarasa-gothic
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
    users = {
      ${my-username} = {
        name = my-username;
        home = "/Users/${my-username}";
      };
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      ${my-username} = { config, lib, pkgs, ... }: {
        home = {
          stateVersion = "26.11";
          packages = with pkgs; [
            coreutils
            # (lib.hiPrio pkgs.uutils-coreutils-noprefix) # `lib.hiPrio` is used to avoid potential conflict with `coreutils-full` (also see https://discourse.nixos.org/t/how-to-use-uutils-coreutils-instead-of-the-builtin-coreutils/8904/15?u=malix)
            emacs
            npins
            nixd
            nix-search-cli
            gcc
            antigravity-cli
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
            settings = {
              user = {
                email = "randomdize@gmail.com";
                name = "Jack Shih";
              };
            };
          };
          bash = {
            enable = true;
            bashrcExtra =
              ''
                export LC_ALL="en_US.UTF-8"
                export LC_CTYPE="en_US.UTF-8"
                export LANG="en_US.UTF-8"
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
            envExtra =
              ''
                export LANG="en_US.UTF-8"
                export LC_CTYPE="en_US.UTF-8"
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
          direnv = {
            enable = true;
            enableBashIntegration = true;
            enableZshIntegration = true;
            nix-direnv = {
              enable = true;
            };
          };
        };
      };
    };
  };
}
