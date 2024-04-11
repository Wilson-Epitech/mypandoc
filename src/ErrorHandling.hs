{--
-- MYPANDOC PROJECT, 2024
-- error handling
-- File description:
-- pandoc proj
--}

module ErrorHandling (checkErrors) where
import System.IO
import System.Exit

displayError :: String -> IO a
displayError av1 = do
    hPutStrLn stderr $ "USAGE: " ++ av1
    exitWith (ExitFailure 84)

checkErrors :: [String] -> IO ()
checkErrors args = do
    let usageMsg = "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n\n"
                ++ "\tifile    path to the file to convert\n"
                ++ "\toformat  output format (xml, json, markdown)\n"
                ++ "\tofile    path to the output file\n"
                ++ "\tiformat  input format (xml, json, markdown)"
    displayError usageMsg
