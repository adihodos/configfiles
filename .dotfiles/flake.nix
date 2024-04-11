{
  description = "My (surely) epic NIX flake";

  inputs = {
	  nixpkgs = {
		  url = "github:NixOS/nixpkgs/nixos-23.11";
	  };
	  home-manager.url = "github:nix-community/home-manager/release-23.11";
	  home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ...}:
	  let
		  lib = nixpkgs.lib;
      setupOptions = {
        system = {
          compositor = "x11";
          systemType = "x86_64-linux";
          hostname = "B4X64-NIX-EE-VM";
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
          font = {
            serif = "Roboto";
            mono = "Iosevka Nerd Font Mono Medium";
          };
        };
      };

		  pkgs = import nixpkgs.legacyPackages.${setupOptions.system.systemType} {
        system = setupOptions.systemType;
          # Allow unfree packages
        config = {
          allowUnfree = true;
          allowUnfreePredicate = (_: true);
        };
      };
	  in
      {
	      nixosConfigurations = {
		      ${setupOptions.system.hostname} = lib.nixosSystem {
            system = setupOptions.system.systemType;
			      modules = [ ./configuration.nix ];
            specialArgs = {
              inherit setupOptions;
            };
		      };
	      };

	      homeConfigurations = {
		      adi = home-manager.lib.homeManagerConfiguration {
			      inherit pkgs;
			      modules = [ ./home.nix ];
            extraSpecialArgs = {
              inherit setupOptions;
            };
		      };
	      };
      };
}
