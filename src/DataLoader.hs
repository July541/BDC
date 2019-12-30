module DataLoader where

import Words

loadData :: FilePath -> IO WordDict
loadData path = do
  contents <- readFile path
  return $ map buildOneItem (lines contents)
    where
      chs = init . words
      en = last . words
      mergeChs line = (++) <$> chs line <*> [" "]
      buildOneItem line = WordItem (concat $ mergeChs line) (en line)
