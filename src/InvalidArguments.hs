{--
-- MYPANDOC PROJECT, 2024
-- mypandoc
-- File description:
-- ErrorHandling
--}

module InvalidArguments (invalidArguments) where

import System.IO (hPutStr, hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

invalidArguments :: IO ()
invalidArguments = 
    hPutStrLn stderr "Error: Invalid arguments." >>
    hPutStr stderr "Usage ./mypandoc -i ifile " >>
    hPutStrLn stderr "-f oformat [-o ofile] [-e iformat]" >>
    hPutStrLn stderr "\tifile    path to the file to convert" >>
    hPutStrLn stderr "\toformat  output format (xml, json, markdown)" >>
    hPutStrLn stderr "\tofile    path to the output file" >>
    hPutStrLn stderr "\tiformat  input format (xml, json, markdown)" >>
    exitWith (ExitFailure 84)
