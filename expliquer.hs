-- =========================================================
-- Retro 70s-Style TODO CLI (In-Memory) — With Modify Task
-- Pure Haskell (base only). Compile: ghc -o app app.hs
-- =========================================================

import System.IO              -- For buffering control
-- import System.IO doesn’t run anything—it just gives you access to file and handle functions like openFile, hFlush, or stdout.
-- Without it, those names aren’t in scope, so you can’t use them.
-- It’s basically a toolbox unlock.

import Control.Concurrent     (threadDelay)  -- For tiny delays in animations
-- That import gives you access to threadDelay, a function that pauses the current thread.
-- It takes microseconds as input (threadDelay 1000000 = 1 second).
-- So it’s how you add timing/sleep effects in Haskell.
-- closest thing to threadDelay is in javascript the setTimeOut method (or await new Promise(r => setTimeout(r, ms))) [In new Promise(r => setTimeout(r, ms)), the timeout calls r() when it finishes.
-- Calling r() resolves the promise so await can continue.]
-- But unlike Haskell, JS doesn't block the main thread, instead it schedules code it in the stack to run later
-- the threadDelay method blocks for n number of seconds, it won't move forward until time passes, it's like you hit PAUSE on the main thread, you see.

-- | Entry point
main :: IO ()
--This declares the type of main: an IO action returning nothing (() = unit).
-- Every Haskell program starts from main.
-- It tells the compiler “this is the entry point.”
main = do 
-- Introduces a do block to sequence IO actions.
-- Without do, you can’t neatly run multiple side effects in order.
-- Think of it as a way to write imperative-looking code in Haskell.

  hSetBuffering stdout NoBuffering
  -- This sets the output stream (stdout) to unbuffered mode.
  -- It makes text appear immediately instead of waiting for a newline or buffer fill.
  -- Crucial when you want instant feedback on screen.

  hSetBuffering stdin  NoBuffering
  -- This sets the input stream (stdin) to unbuffered mode.
-- It lets your program read characters as soon as you type, without hitting Enter.
-- Handy for real-time input, like games or keypress-driven tools.
-- -------------------
-- What is buffering ?
-- -------------------
-- Buffering is when input or output is stored in a temporary memory area (a “buffer”) before being processed.
-- Buffered mode waits until the buffer is full (or you hit Enter) before sending/reading data.
-- Unbuffered mode skips the wait—data is handled immediately, character by character.

  bootAnimation          -- Show retro boot sequence
  menuLoop []            -- Start menu loop with empty tasks

--------------------------------------------------------------------------------
-- Boot Animation
--------------------------------------------------------------------------------
bootAnimation :: IO ()
-- Declares bootAnimation as an IO action returning nothing.
-- It’s the type signature, like a promise of what it does [Languages like OCaml, F#, also let you write separate type annotations like Haskell.]
-- Signals this function has side effects (printing).

bootAnimation = do
-- Starts a do block to sequence multiple IO actions.
-- Allows running several putStrLn calls in order.
--The `do` here is a **syntax for sequencing actions** in a monad (like `IO`).

-- ----------------
-- What is Monad ?
-- ----------------
-- A monad is a **design pattern** that lets you chain computations while handling extra context.
-- In Haskell, `IO` is a monad—so you can sequence input/output safely.

-- so the do block lets you write multiple commands in order, as if imperative, even in a pure functional language.
-- Other languages don’t have `do` exactly, but similar constructs exist (like `async/await` in JS or Rust).

-- that's why I had to declare this type signature `bootAnimation :: IO ()` to tell Haskell that `bootAnimation` is an IO monad action.
-- It signals that the function involves side effects (printing) and can be sequenced with other IO actions using `do`.
-- Remember that: Haskell is pure, so functions normally can’t change the world or depend on it.
-- Side effects (like printing or reading input) are wrapped in monads like IO to keep purity intact.
 

  putStrLn "=========================================="
  putStrLn "      *** WELCOME TO RETRO TERMINAL ***   "
  putStrLn "=========================================="
  putStrLn ""

  slowPrint "Initializing system..."
  slowPrint "Loading core modules..."
  slowPrint "Mounting virtual disk..."
  slowPrint "Starting Task Manager v1.0 [1979 Edition]..."
  slowPrint "Boot sequence complete. Press any key to continue."

  _ <- getLine  -- Wait for Enter key
  -- Reads a line from input but ignores it (_ discards the result).
  -- Here it waits for the user to press Enter.
  -- Useful for pausing the program without storing input.

  return ()
  -- Wraps a value (()) into the IO monad.
  -- Doesn’t produce side effects—it just ends the action cleanly.
  -- and that's often used to satisfy type requirements at the end of a do block.
  -- At the end of a do block, Haskell expects the final action to produce the monad’s type (IO () here).
  -- Even if you don’t need to return anything meaningful, return () ensures the types line up.

slowPrint :: String -> IO ()
-- Declares slowPrint takes a String and returns an IO action.
-- The IO monad indicates it has side effects.
-- The () means it doesn’t produce a meaningful value.
slowPrint str = do
  -- Begins a do block to sequence multiple IO actions.
  -- Allows the function to run the steps one after another.
  -- Keeps the code readable like imperative style.
putStrLn str
  -- Prints the string to the console with a newline.
  -- This is the main visible effect of the function.
  -- It’s wrapped in IO because printing affects the outside world.
threadDelay 500000 -- 0.5s delay
-- Pauses the current thread for 500,000 microseconds (0.5 seconds) after printing the current line. so if we call slowPrint repeatedly, each line waits half a second before the next appears.
-- Creates a “slow print” effect before continuing.
-- Still part of the IO sequence, so it respects order.

--------------------------------------------------------------------------------
-- Menu Loop
--------------------------------------------------------------------------------

menuLoop :: [String] -> IO ()
menuLoop tasks = do
  putStrLn ""
  putStrLn "┌========================================┐"
  putStrLn "│         RETRO TODO TASK MANAGER        │"
  putStrLn "├========================================┤"
  putStrLn "│ 1 ▸ ADD TASK                           │"
  putStrLn "│ 2 ▸ VIEW TASK                          │"
  putStrLn "│ 3 ▸ MODIFY TASK                        │"
  putStrLn "│ 4 ▸ DELETE TASK                        │"
  putStrLn "│ 5 ▸ QUIT                               │"
  putStrLn "└========================================┘"
  putStr   "SELECT ▸ "
  hFlush stdout

  choice <- getLine
  case choice of
    "1" -> do tasks' <- addTask tasks
              menuLoop tasks'           -- Create
    "2" -> viewTasks tasks >> menuLoop tasks  -- Read
    "3" -> do tasks' <- modifyTask tasks
              menuLoop tasks'           -- Update
    "4" -> do tasks' <- deleteTask tasks
              menuLoop tasks'           -- Delete
    "5" -> farewell
    _   -> do beep
              putStrLn ">>> INVALID OPTION <<<"
              menuLoop tasks

--------------------------------------------------------------------------------
-- View Tasks
--------------------------------------------------------------------------------

viewTasks :: [String] -> IO ()
viewTasks [] = putStrLn "\n[!] NO TASKS YET"
viewTasks ts = do
  putStrLn "\n┌───────────── TASKS ─────────────┐"
  mapM_ (\(i,t) -> putStrLn $ " " ++ show i ++ " ▸ " ++ t) (zip [1..] ts)
  putStrLn "└─────────────────────────────────┘"

--------------------------------------------------------------------------------
-- Add Task
--------------------------------------------------------------------------------

addTask :: [String] -> IO [String]
addTask tasks = do
  putStr "\nNEW TASK ▸ "
  hFlush stdout
  t <- getLine
  slowPrint "[✓] STORED"
  return $ tasks ++ [t]

--------------------------------------------------------------------------------
-- Delete Task
--------------------------------------------------------------------------------

deleteTask :: [String] -> IO [String]
deleteTask [] = putStrLn "\n[!] NO TASKS TO DELETE" >> return []
deleteTask tasks = do
  viewTasks tasks
  putStr "REMOVE ▸ "
  hFlush stdout
  numStr <- getLine
  let parsed = reads numStr :: [(Int,String)]
  case parsed of
    (n,_):_ | n >= 1 && n <= length tasks -> do
      let (front,back) = splitAt (n-1) tasks
      let updated = front ++ drop 1 back
      slowPrint "[✓] REMOVED"
      return updated
    _ -> beep >> putStrLn ">>> INVALID INDEX <<<" >> return tasks

--------------------------------------------------------------------------------
-- Modify Task
--------------------------------------------------------------------------------

modifyTask :: [String] -> IO [String]
modifyTask [] = putStrLn "\n[!] NO TASKS TO MODIFY" >> return []
modifyTask tasks = do
  viewTasks tasks
  putStr "MODIFY ▸ "
  hFlush stdout
  numStr <- getLine
  let parsed = reads numStr :: [(Int,String)]
  case parsed of
    (n,_):_ | n >= 1 && n <= length tasks -> do
      putStr "NEW TEXT ▸ "
      hFlush stdout
      newText <- getLine
      let (front, (_:back)) = splitAt (n-1) tasks
      let updated = front ++ [newText] ++ back
      slowPrint "[✓] MODIFIED"
      return updated
    _ -> beep >> putStrLn ">>> INVALID INDEX <<<" >> return tasks

--------------------------------------------------------------------------------
-- Farewell / Exit Screen
--------------------------------------------------------------------------------

farewell :: IO ()
farewell = do
  putStrLn "\n┌───────────────────────────────┐"
  putStrLn   "│       END OF SESSION           │"
  putStrLn   "│       THANK YOU, OPERATOR      │"
  putStrLn   "└───────────────────────────────┘"
  putStr "\BEL"

--------------------------------------------------------------------------------
-- Extra: Tiny Retro Beep helper
--------------------------------------------------------------------------------

beep :: IO ()
beep = putStr "\BEL"
