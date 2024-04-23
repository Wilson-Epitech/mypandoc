{--
-- MYPANDOC PROJECT, 2024
-- myPandoc
-- File description:
-- pandoc proj
--}

module MyPandoc (myPandoc) where

import Configuration (Conf)
import System.Exit (exitSuccess)

myPandoc :: Conf -> IO ()
myPandoc conf = print conf
