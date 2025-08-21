-- =========================================================
-- Retro 70s-Style TODO CLI (In-Memory) — With Modify Task
-- Pure Haskell (base only). Compile: ghc -o app app.hs
-- =========================================================

import System.IO              -- For buffering control
import Control.Concurrent     (threadDelay)  -- For tiny delays in animations

-- | Entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering

  bootAnimation          -- Show retro boot sequence
  menuLoop []            -- Start menu loop with empty tasks

--------------------------------------------------------------------------------
-- Boot Animation
--------------------------------------------------------------------------------

bootAnimation :: IO ()
bootAnimation = do
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
  return ()

slowPrint :: String -> IO ()
slowPrint str = do
  putStrLn str
  threadDelay 500000 -- 0.5s delay

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
