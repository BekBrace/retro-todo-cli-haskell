# Retro 70s-Style CLI To-Do List Manager in Haskell

A nostalgic retro command-line **To-Do List Manager** written in pure Haskell.  
Inspired by 70s terminals, it features ASCII art, cinematic delays, and a “Press any key to continue” boot animation.  

This is **version 2** of my simple To-Do application in Haskell.  
Why version 2? Because I redesigned it to feel retro — complete with fake booting animation, old-school terminal vibes, and a touch of nostalgia.  

---

## Features

- Retro 70s-style CLI design with boxes and ASCII graphics  
- Boot animation with slow-print messages and keypress pause  
- Full **CRUD** functionality (Create, Read, Update, Delete)  
- Tasks stored **in memory** (no database needed)  
- Recursive menu loop (no mutable state)  
- Tiny retro beep for invalid input  

---

## Requirements

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)  
- Works on any terminal supporting ASCII characters  

---

## How to Run
Clone the repository and run the program as follows:

```bash
git clone https://github.com/yourusername/retro-haskell-todo.git
cd retro-haskell-todo
ghc -o todo v2.hs
./todo
```
```diff
┌========================================┐
│         RETRO TODO TASK MANAGER        │
├========================================┤
│ 1 ▸ ADD TASK                           │
│ 2 ▸ VIEW TASKS                         │
│ 3 ▸ MODIFY TASK                        │
│ 4 ▸ DELETE TASK                        │
│ 5 ▸ QUIT                               │
└========================================┘
```
Thank you very much, and feel free to clone the repo for your own use, fork it with a pull request and to change anything just let me review and I don't mind improving upon it with you.
Have a productive day, everyone.
