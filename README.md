# reduce

Reduce is a Haskell library for reducing inputs with some properties. To 
use the library to build a simple line delta-debugger you can write:

```haskell
main = do 
  file : cmd <- getArgs
  content <- lines <$> readFile file
  result <- ddmin (run cmd) content
  
  writeFile "output" (unlines result)
  
  where 
   run cmd lines = do
     writeFile "output" (unlines result)
     -- run cmd on output
     -- if 0 return True else return False
```

## Build

To build run `stack build`, to test run `stack test`, and to get the
documentation run `stack haddock`.
