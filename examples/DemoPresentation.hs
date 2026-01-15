{- |
Module      : DemoPresentation
Description : A presentation that presents the Demo framework itself
Copyright   : (c) Demo Author, 2024
License     : MIT

This is a self-referential example presentation that demonstrates all the
features of the Demo presentation framework by presenting the framework itself.

Run with: demo ./examples/DemoPresentation.hs
Or standalone: slides ./examples/DemoPresentation.hs
-}
module DemoPresentation (presentation) where

import Demo.Core.DSL

-- | The main presentation showcasing Demo's capabilities
presentation :: Presentation
presentation = mkPresentation "Demo: A Live Coding Presentation Framework" $ do

  ---------------------------------------------------------------------------
  -- Section 1: Introduction
  ---------------------------------------------------------------------------

  slide "Welcome to Demo" $ do
    note $ unlines
      [ "Demo is a presentation framework for live coding demonstrations."
      , ""
      , "Key features:"
      , "  • Interactive shell for system commands"
      , "  • GHCI mode for Haskell evaluation"
      , "  • Variable passing between modes ($v1, $v2, ...)"
      , "  • IPC for synchronized notes and elaboration views"
      , ""
      , "Press SPACE to run commands, N/P to navigate slides."
      ]
    system "echo '🎬 Welcome to Demo!'"
    system "echo 'A presentation framework for live coding'"

  slide "Project Structure" $ do
    note $ unlines
      [ "Demo is structured as a Cabal project with multiple components:"
      , ""
      , "• Library: Core types, DSL, IPC, UI"
      , "• Executables: demo, slides, notes, elaboration"
      , "• Tests: Unit, integration, and E2E"
      ]
    system "tree -L 2 --dirsfirst -I 'dist-*'"
    elaborate "demo.cabal" (1, 25) "Project configuration"

  ---------------------------------------------------------------------------
  -- Section 2: The DSL
  ---------------------------------------------------------------------------

  slide "The DSL: Building Presentations" $ do
    note $ unlines
      [ "Presentations are built using a monadic DSL."
      , ""
      , "Key combinators:"
      , "  • mkPresentation - creates a presentation"
      , "  • slide - adds a slide"
      , "  • system - system command"
      , "  • ghci - GHCI expression"
      , "  • note - speaker notes"
      , "  • elaborate - code fragment reference"
      ]
    system "head -50 examples/DemoPresentation.hs"
    elaborate "src/Demo/Core/DSL.hs" (1, 30) "DSL Module"

  slide "DSL Implementation" $ do
    note $ unlines
      [ "The DSL uses the State monad for building:"
      , ""
      , "• PresentationBuilder = State [Slide]"
      , "• SlideBuilder = State Slide"
      , ""
      , "Also supports Free monad interpretation for"
      , "more flexible/testable pipelines."
      ]
    elaborate "src/Demo/Core/DSL.hs" (83, 120) "State-based builders"

  ---------------------------------------------------------------------------
  -- Section 3: System Commands
  ---------------------------------------------------------------------------

  slide "System Commands" $ do
    note $ unlines
      [ "System commands run in a shell with streaming output."
      , ""
      , "The output is displayed in real-time as the"
      , "command executes, enabling live demonstrations"
      , "of build processes, tests, etc."
      ]
    system "echo 'Running build...'; sleep 0.5; echo 'Compiling...'; sleep 0.5; echo 'Done!'"
    system "echo 'Lines: '$(wc -l < src/Demo/Core/Types.hs)"
    elaborate "src/Demo/Interpreter/System.hs" (27, 55) "Command execution"

  ---------------------------------------------------------------------------
  -- Section 4: GHCI Mode
  ---------------------------------------------------------------------------

  slide "GHCI Mode" $ do
    note $ unlines
      [ "Press 'g' to enter GHCI mode."
      , ""
      , "In GHCI mode, you can evaluate Haskell expressions."
      , "Results are stored in variables ($v1, $v2, ...) that"
      , "can be referenced in subsequent system commands."
      , ""
      , "Try: 1 + 1"
      , "Then: \"Hello, \" ++ \"Demo!\""
      ]
    ghci "map (*2) [1..5]"
    ghciTyped "sum [1..100]" "Int"
    elaborate "src/Demo/Interpreter/Ghci.hs" (50, 90) "GHCI evaluation"

  slide "Variable Passing" $ do
    note $ unlines
      [ "Variables enable powerful workflows:"
      , ""
      , "1. Compute something in GHCI"
      , "2. Use result in shell commands"
      , "3. Or vice versa!"
      , ""
      , "Variables use Aeson for JSON serialization."
      ]
    ghci "42 * 2"
    system "echo 'The answer from GHCI: $v1'"
    elaborate "src/Demo/Core/Variable.hs" (43, 65) "Variable substitution"

  ---------------------------------------------------------------------------
  -- Section 5: Advanced Features
  ---------------------------------------------------------------------------

  slide "Zipper Navigation" $ do
    note $ unlines
      [ "Demo uses zippers for efficient navigation."
      , ""
      , "Zippers are comonads that provide:"
      , "  • O(1) access to current focus"
      , "  • O(1) movement left/right"
      , "  • No bounds checking needed!"
      , ""
      , "The ListZipper is generic and reusable."
      ]
    system "grep -A 15 'data ListZipper' src/Demo/Core/Zipper.hs"
    elaborate "src/Demo/Core/Zipper.hs" (1, 50) "Zipper implementation"

  slide "IPC Protocol" $ do
    note $ unlines
      [ "Multiple views communicate via Unix sockets:"
      , ""
      , "• slides -> notes: Slide changes"
      , "• slides -> elaboration: File references"
      , ""
      , "Messages are JSON-encoded with length framing."
      ]
    system "grep 'data IPCMessage' src/Demo/IPC/Protocol.hs -A 20"
    elaborate "src/Demo/IPC/Protocol.hs" (30, 70) "Message types"

  slide "Free Monad Actions" $ do
    note $ unlines
      [ "UI actions are described using a Free monad."
      , ""
      , "This separates description from interpretation:"
      , "  • Pure description of effects"
      , "  • Flexible interpreters (IO, pure for testing)"
      , "  • Easy to add logging, metrics, etc."
      ]
    elaborate "src/Demo/Core/FreeUI.hs" (32, 60) "UIActionF functor"

  slide "Contravariant Handlers" $ do
    note $ unlines
      [ "Event handlers are contravariant functors."
      , ""
      , "Why? They consume events, not produce them."
      , ""
      , "This enables:"
      , "  • Type-safe handler composition"
      , "  • Filtering, mapping, combining"
      , "  • Divisible/Decidable instances"
      ]
    elaborate "src/Demo/Core/Handlers.hs" (48, 80) "Handler type"

  ---------------------------------------------------------------------------
  -- Section 6: Demonstration
  ---------------------------------------------------------------------------

  slide "Live Demo: Building Demo" $ do
    note $ unlines
      [ "Let's build and test Demo itself!"
      , ""
      , "Watch the streaming output as cabal builds"
      , "all the components."
      ]
    system "cabal build all 2>&1 | tail -20"

  slide "Live Demo: Running Tests" $ do
    note $ unlines
      [ "Demo has comprehensive tests:"
      , ""
      , "• Unit tests for variable substitution"
      , "• Property tests with QuickCheck"
      , "• Integration tests for IPC"
      , "• End-to-end feature tests"
      ]
    system "cabal test 2>&1 | grep -E '(✔|✘|examples|failures)'"

  ---------------------------------------------------------------------------
  -- Section 7: Conclusion
  ---------------------------------------------------------------------------

  slide "Architecture Summary" $ do
    note $ unlines
      [ "Demo demonstrates advanced Haskell patterns:"
      , ""
      , "✓ Monadic DSL for declarative presentation building"
      , "✓ Comonadic zippers for efficient navigation"
      , "✓ Free monads for separating description from effect"
      , "✓ Contravariant functors for type-safe handlers"
      , "✓ Profunctor optics for safe, composable access"
      , "✓ IPC via Unix sockets for multi-view sync"
      , ""
      , "All with comprehensive tests!"
      ]
    system "echo ''"
    system "echo '  ╔══════════════════════════════════════╗'"
    system "echo '  ║  Thank you for watching Demo Demo!   ║'"
    system "echo '  ╚══════════════════════════════════════╝'"
    system "echo ''"

  slide "Get Started!" $ do
    note $ unlines
      [ "Create your own presentation:"
      , ""
      , "1. Create MyPresentation.hs"
      , "2. Import Demo.Core.DSL"
      , "3. Define `presentation :: Presentation`"
      , "4. Run: demo ./MyPresentation.hs"
      , ""
      , "Happy presenting! 🎉"
      ]
    system "echo 'Create presentations like a boss!'"
    system "echo ''"
    system "echo 'module MyPresentation where'"
    system "echo 'import Demo.Core.DSL'"
    system "echo ''"
    system "echo 'presentation = mkPresentation \"My Talk\" $ do'"
    system "echo '  slide \"Hello\" $ system \"echo Hello!\"'"
