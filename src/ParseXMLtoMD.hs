{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- ParseXMLtoMD
--}

module ParseXMLtoMD (parseXMLtoMD) where

-- parseDocument -- File --
parseDocument :: [String] -> [String]
parseDocument file = file

-- parseXMLtoMD -- File --
parseXMLtoMD :: [String] -> [String]
parseXMLtoMD file@("<document>" : _) | last file /= "</document>" = []
                                | otherwise = parseDocument (tail (init file))
parseXMLtoMD _ = []
