{
  description = "Demo - Interactive presentation framework for Haskell developers";

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
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        lib = pkgs.lib;
        hsPkgs = pkgs.haskellPackages;

        # Build the demo package first
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
        # Source files for hint to compile from (avoids PAGE21 bug)
        demoSrc = pkgs.runCommand "demo-src" { } ''
          mkdir -p $out/src
          cp -r ${./src}/* $out/src/
        '';
      in
      {
        # Package outputs
        packages = {
          # The hint environment with all runtime dependencies
          inherit hintEnv;

          # Demo source files for hint interpreter
          inherit demoSrc;

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

            srcPath = lib.mkOption {
              type = lib.types.package;
              default = demoPkgs.demoSrc;
              description = "Demo source files for hint to compile from";
            };
          };

          config = lib.mkIf cfg.enable {
            home.packages = [
              # Wrap demo binaries to set correct PATH for hint/GHC
              # This fixes the PAGE21 relocation bug on Apple Silicon by ensuring
              # hint finds the correctly-configured GHC with packages
              (pkgs.runCommand "demo-wrapped" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
                mkdir -p $out/bin
                for bin in demo slides notes elaboration; do
                  makeWrapper ${cfg.package}/bin/$bin $out/bin/$bin \
                    --prefix PATH : ${cfg.hintEnv}/bin \
                    --set DEMO_SRC_PATH "${cfg.srcPath}/src"
                done
              '')
              cfg.hintEnv # GHC with all packages for hint to use
              cfg.srcPath # Source files for hint to compile from
            ];

            # Also set session variables for other tools that might need them
            home.sessionVariables = {
              DEMO_SRC_PATH = "${cfg.srcPath}/src";
            };
          };
        };

      # Overlay for use with nixpkgs
      overlays.default = final: prev: {
        demo = self.packages.${prev.system}.demo;
        demo-hint-env = self.packages.${prev.system}.hintEnv;
      };
    };
}
