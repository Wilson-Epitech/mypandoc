{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- Configuration
--}

module Configuration (Format(..), Conf(..), defaultConf) where

data Format = XML | JSON | MD deriving (Eq, Show)

data Conf = Conf
    {
        ifile :: Maybe String,
        oformat :: Maybe Format,
        ofile :: Maybe String,
        iformat :: Maybe Format
    } deriving (Eq, Show)

defaultConf :: Conf
defaultConf = Conf Nothing Nothing Nothing Nothing
