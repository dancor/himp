main = do
  args <- getArgs
  print' args
  hFlush stdout
