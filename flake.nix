{
  description = "Demo - Interactive presentation framework for Haskell developers";

  # Automatic Cachix configuration for all developers
  # Requires: accept-flake-config = true in ~/.config/nix/nix.conf
  # Or user will be prompted to accept on first use
  nixConfig = {
    extra-substituters = [
      "https://haskell.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell.cachix.org-1:m2M2sVFTqOK5cuCy9NMKcTxKgoOgyAyC/8u1EXGgkF8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ] (
      system:
      let
        isAarch64Darwin = system == "aarch64-darwin";
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        lib = pkgs.lib;

        # On Apple Silicon (aarch64-darwin), we must compile ALL Haskell packages
        # with -finter-module-far-jumps to avoid the PAGE21 relocation bug when
        # hint loads them dynamically at runtime.
        # See: https://downloads.haskell.org/ghc/9.10-latest/docs/users_guide/using-optimisation.html
        hsPkgs =
          if isAarch64Darwin then
            pkgs.haskellPackages.override {
              overrides = self: super: {
                # Apply far-jumps flag to ALL packages via mkDerivation override
                mkDerivation =
                  args:
                  super.mkDerivation (
                    args
                    // {
                      configureFlags = (args.configureFlags or [ ]) ++ [
                        "--ghc-option=-finter-module-far-jumps"
                      ];
                    }
                  );
              };
            }
          else
            pkgs.haskellPackages;

        # Build the demo package
        demoPackage = hsPkgs.callCabal2nix "demo" ./. { };

        # Extend haskellPackages to include the demo library
        # This is critical so hint can find Demo.Core.DSL at runtime
        hsPkgsWithDemo = hsPkgs.extend (
          self: super: {
            demo = demoPackage;
          }
        );

        # GHC with all packages needed by the hint interpreter at runtime
        # This is required because hint dynamically loads and interprets Haskell code
        # MUST include `demo` so presentations can import Demo.Core.DSL!
        hintEnv = hsPkgsWithDemo.ghcWithPackages (
          ps: with ps; [
            # THE DEMO LIBRARY ITSELF - required for presentations!
            demo

            # Core packages used by Demo.Core.Types and DSL
            aeson
            lens
            text
            containers
            bytestring
            mtl
            transformers

            # Advanced abstractions used by the framework
            uniplate
            free
            comonad
            contravariant

            # Used by presentations and UI
            brick
            vty
            async
            stm

            # Development packages that may be needed
            vector
            unordered-containers
          ]
        );
      in
      {
        # Package outputs
        packages = {
          # The hint environment with all runtime dependencies
          inherit hintEnv;

          # The demo package built with cabal2nix
          demo = demoPackage;

          # Default package is the demo executables
          default = demoPackage;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Use our hint environment for development too
            hintEnv

            # Haskell toolchain
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            hsPkgs.ghcid
            hsPkgs.fourmolu
            hsPkgs.hoogle
            hsPkgs.cabal-gild

            # Essential tools
            pkg-config
            zlib.dev
            zlib
            ncurses
            git
            tmux
            tree

            # Cachix support (for pushing builds)
            cachix
            age
          ];

          shellHook = ''
            echo "Demo Framework Development Shell"
            echo "Run 'cabal build' to build the project"
            echo "Run 'cabal test' to run tests"
            echo ""
            echo "To install demo system-wide via home-manager:"
            echo "  See README.md for instructions"
          '';
        };
      }
    )
    // {
      # Home-manager module for easy system integration
      # Usage in flake.nix:
      #   inputs.demo.url = "github:USER/demo";
      #
      # Usage in home.nix:
      #   imports = [ inputs.demo.homeManagerModules.default ];
      #   programs.demo.enable = true;
      homeManagerModules.default =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          cfg = config.programs.demo;
          system = pkgs.system;
          demoPkgs = self.packages.${system};
        in
        {
          options.programs.demo = {
            enable = lib.mkEnableOption "Demo presentation framework";

            package = lib.mkOption {
              type = lib.types.package;
              default = demoPkgs.demo;
              description = "The demo package to install";
            };

            hintEnv = lib.mkOption {
              type = lib.types.package;
              default = demoPkgs.hintEnv;
              description = "GHC environment with packages needed by hint interpreter";
            };
          };

          config = lib.mkIf cfg.enable {
            home.packages = [
              # Wrap demo binaries to set correct PATH for hint/GHC
              # This ensures hint finds the correctly-configured GHC with packages
              # Note: We don't include hintEnv as a package because it contains
              # the demo binary and would conflict. The wrapper's PATH prefix
              # is sufficient for hint to find the right GHC tools.
              (pkgs.runCommand "demo-wrapped" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
                mkdir -p $out/bin
                for bin in demo slides notes elaboration; do
                  makeWrapper ${cfg.package}/bin/$bin $out/bin/$bin \
                    --prefix PATH : ${cfg.hintEnv}/bin
                done
              '')
            ];
          };
        };

      # Overlay for use with nixpkgs
      overlays.default = final: prev: {
        demo = self.packages.${prev.system}.demo;
        demo-hint-env = self.packages.${prev.system}.hintEnv;
      };
    };
}
