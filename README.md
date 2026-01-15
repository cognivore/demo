# Demo - Interactive Presentation Framework for Haskell Developers

Demo is a presentation framework for live coding demonstrations. It provides an interactive shell that shows system commands and GHCI expressions, letting presenters talk about what's happening and run commands with streaming output.

## Features

- **Interactive Shell**: Display and run system commands with real-time streaming output
- **GHCI Mode**: Evaluate Haskell expressions and store results in variables (`$v1`, `$v2`, ...)
- **Variable Passing**: Use outputs from GHCI in shell commands and vice versa
- **Speaker Notes**: Synchronized notes view via IPC
- **Code Elaboration**: Display file fragments alongside your presentation
- **Content-Addressed Sessions**: tmux sessions are named by content hash, preventing conflicts

## Installation

### Via Home-Manager (Recommended)

Add demo to your flake inputs and import the home-manager module:

```nix
# flake.nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    demo.url = "github:YOUR_USERNAME/demo";  # Replace with actual repo
  };

  outputs = { self, nixpkgs, home-manager, demo, ... }: {
    homeConfigurations."your-username" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        demo.homeManagerModules.default
        {
          programs.demo.enable = true;
        }
      ];
    };
  };
}
```

Then run:

```bash
home-manager switch
```

This installs:
- `demo` - Main launcher (creates tmux sessions)
- `slides` - Standalone presenter view
- `notes` - Speaker notes view
- `elaboration` - Code fragment view
- `hintEnv` - GHC with all packages needed for presentation interpretation

### Via Nix Profile (Quick Install)

```bash
# Install demo and its runtime dependencies
nix profile install github:YOUR_USERNAME/demo#demo
nix profile install github:YOUR_USERNAME/demo#hintEnv
```

### Development

```bash
git clone https://github.com/YOUR_USERNAME/demo
cd demo
nix develop
cabal build
cabal test
```

## Usage

### Creating a Presentation

Create a Haskell file (e.g., `MyPresentation.hs`):

```haskell
module MyPresentation (presentation) where

import Demo.Core.DSL

presentation :: Presentation
presentation = mkPresentation "My Talk" $ do

  slide "Introduction" $ do
    note "Welcome to my presentation!"
    system "echo 'Hello, World!'"

  slide "GHCI Demo" $ do
    note "Let's compute something in GHCI"
    ghci "map (*2) [1..5]"
    ghciTyped "sum [1..100]" "Int"

  slide "Variable Passing" $ do
    note "We can use GHCI results in shell commands"
    ghci "42 * 2"
    system "echo 'The answer is: $v1'"

  slide "Code Walkthrough" $ do
    note "Let's look at some code"
    elaborate "src/MyModule.hs" (10, 25) "Function definition"
```

### Running Presentations

```bash
# Full tmux session with slides, elaboration, and notes
demo ./MyPresentation.hs

# Attach to existing session
demo ./MyPresentation.hs attach

# Terminate session
demo ./MyPresentation.hs fin

# Standalone modes
slides ./MyPresentation.hs       # Just the presenter view
notes ./MyPresentation.hs        # Just notes (connects to slides if running)
notes --recall ./MyPresentation.hs  # Notes in standalone mode
elaboration ./MyPresentation.hs  # Just elaboration view
```

### Keybindings (Slides)

| Key | Action |
|-----|--------|
| `Space` | Run current command |
| `n` / `→` | Next slide |
| `p` / `←` | Previous slide |
| `g` | Enter GHCI mode |
| `s` | Enter System mode |
| `q` / `Esc` | Quit |

### In GHCI Mode

Type Haskell expressions and press Enter. Results are stored as `$v1`, `$v2`, etc.

## DSL Reference

### Slide Combinators

```haskell
-- Create a slide with a title
slide :: Text -> SlideBuilder () -> PresentationBuilder ()

-- Add speaker notes
note :: Text -> SlideBuilder ()

-- Add a system command
system :: Text -> SlideBuilder ()

-- Add a GHCI expression
ghci :: Text -> SlideBuilder ()

-- Add a typed GHCI expression (with type annotation)
ghciTyped :: Text -> Text -> SlideBuilder ()

-- Add a code elaboration (file reference)
elaborate :: FilePath -> (Int, Int) -> Text -> SlideBuilder ()
```

### Variable Substitution

In system commands, use `$v1`, `$v2`, etc. to reference previous outputs:

```haskell
slide "Variables" $ do
  ghci "[1,2,3]"           -- Stored as $v1
  system "echo '$v1'"      -- Prints: [1,2,3]
  ghci "length $v1"        -- Uses $v1 in GHCI
```

## Architecture

Demo uses several advanced Haskell patterns:

- **Monadic DSL**: State monad for declarative presentation building
- **Comonadic Zippers**: Efficient slide and command navigation
- **Free Monads**: Separating effect description from interpretation
- **Contravariant Functors**: Type-safe event handler composition
- **Profunctor Optics**: Safe, composable data access
- **Unix Socket IPC**: Multi-view synchronization

## Project Structure

```
demo/
├── app/                    # Executable entry points
│   ├── Demo.hs            # Main launcher
│   ├── Slides.hs          # Presenter view
│   ├── Notes.hs           # Notes view
│   └── Elaboration.hs     # Code view
├── src/Demo/
│   ├── Core/              # Core types and DSL
│   │   ├── Types.hs       # Data types
│   │   ├── DSL.hs         # Presentation DSL
│   │   ├── Zipper.hs      # Comonadic navigation
│   │   ├── FreeUI.hs      # Free monad actions
│   │   ├── Handlers.hs    # Contravariant handlers
│   │   └── Variable.hs    # Variable substitution
│   ├── Interpreter/       # Command execution
│   │   ├── Ghci.hs        # GHCI via hint
│   │   └── System.hs      # Shell commands
│   ├── IPC/               # Inter-process communication
│   │   ├── Protocol.hs    # Message types
│   │   ├── Server.hs      # Broadcast server
│   │   └── Client.hs      # Client connections
│   ├── UI/                # Brick TUI
│   │   ├── Common.hs      # Shared widgets
│   │   ├── Slides.hs      # Presenter UI
│   │   ├── Notes.hs       # Notes UI
│   │   └── Elaboration.hs # Code UI
│   └── Loader.hs          # Presentation loading
├── examples/              # Example presentations
├── test/                  # Test suite
└── flake.nix             # Nix flake
```

## License

MIT
