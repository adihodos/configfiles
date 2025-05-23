{
  description = "My (surely/clueless) epic NIX flake";

  inputs = {
    #nixpkgs = {url = "github:NixOS/nixpkgs/nixos-23.11";};
    nixpkgs = {url = "github:NixOS/nixpkgs/nixos-24.11";};

    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    home-manager = {
      #url = "github:nix-community/home-manager/release-23.11";
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nur = {
    #   url = "github:nix-community/NUR";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    stylix = {
      url = "github:danth/stylix/release-24.11";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      #inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
    setupOptions = {
      system = {
        compositor = "x11";
        systemType = "x86_64-linux";
        hostname = "B5X64-NIX-EE-XEON";
        profile = "personal";
        timezone = "Europe/Bucharest";
        locale = "en_US.UTF-8";
        extraLocale = "ro_RO.UTF-8";
        editor = "emacs";
      };

      user = {
        username = "adi";
        name = "Adi Hodos";
        description = "Just me I guess";
        email = "adi.hodos@gmail.com";
        editor = "emacsclient";
        spawnEditor = "emacsclient -c -a 'emacs'";
      };
    };

    pkgs = import nixpkgs {
      system = setupOptions.system.systemType;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };

      overlays = [
        (import self.inputs.emacs-overlay)
      ];
    };

    pkgs-unstable = import inputs.nixpkgs-unstable {
      system = setupOptions.system.systemType;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    };

    findModules = dir:
      builtins.concatLists (builtins.attrValues (builtins.mapAttrs
        (name: type:
          if type == "regular"
          then [
            {
              name = builtins.elemAt (builtins.match "(.*)\\.nix" name) 0;
              value = dir + "/${name}";
            }
          ]
          else if
            (builtins.readDir (dir + "/${name}"))
            ? "default.nix"
          then [
            {
              inherit name;
              value = dir + "/${name}";
            }
          ]
          else findModules (dir + "/${name}")) (builtins.readDir dir)));
  in {
    nixosModules = builtins.listToAttrs (findModules ./modules);

    nixosConfigurations = {
      ${setupOptions.system.hostname} = lib.nixosSystem {
        system = setupOptions.system.systemType;

        specialArgs = {
          inherit inputs;
          inherit pkgs;
          inherit pkgs-unstable;
          inherit setupOptions;
        };

        modules =
          __attrValues self.nixosModules
          ++ [
            inputs.stylix.nixosModules.stylix
            ./nixos/configuration.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                backupFileExtension = "hmbkp";

                extraSpecialArgs = {
                  inherit inputs pkgs;
                  inherit pkgs-unstable;
                  inherit setupOptions;
                };

                users.${setupOptions.user.username} = {
                  imports = [./home/home.nix];
                };
              };
            }
          ];
      };
    };
  };
}
