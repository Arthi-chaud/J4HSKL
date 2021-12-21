{-|
Module      : J4HSL.PArser
Description : Parser for JSON Data
Copyright   : (c) Arthui-chaud, 2021
License     : GPL-3
Maintainer  : arthur.jamet@gmail.com
Stability   : experimental
Portability : POSIX

-}
module J4HSKL.Parser where

import J4HSKL.Data
import BasicParser

parseJSON :: Parser JSONValue 
parseJSON = Parser (const Nothing)