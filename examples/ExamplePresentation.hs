module ExamplePresentation where

import Demo.Core.DSL

presentation :: Presentation
presentation = mkPresentation "Example Demo" $ do
  slide "Introduction" $ do
    note "Welcome to this example presentation! Press Space to run commands."
    system "echo 'Hello from the demo framework!'"

  slide "System Commands" $ do
    note "Here we demonstrate running system commands with streaming output."
    system "echo 'First command'"
    system "ls -la"
    system "echo 'Counting...' && sleep 1 && echo '1' && sleep 1 && echo '2' && sleep 1 && echo '3'"

  slide "File Viewing" $ do
    note "The elaboration pane shows relevant code snippets."
    elaborate "examples/ExamplePresentation.hs" (1, 15) "Presentation definition"
    system "echo 'Check the elaboration pane on the right!'"

  slide "Variable Passing" $ do
    note "GHCI results can be passed to system commands using $v1, $v2, etc."
    ghci "[1,2,3,4,5]"
    system "echo 'The list is: $v1'"

  slide "Conclusion" $ do
    note "Thanks for watching! Press 'q' to quit."
    system "echo 'Demo complete!'"
