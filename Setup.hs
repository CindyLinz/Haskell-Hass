import Distribution.Simple
import Distribution.PackageDescription
import System.Cmd

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
    system "rm -f src/Parser.hs src/Parser.info"
    putStrLn "happy -i -o src/Parser.hs src/Parser.y"
    system "happy -i -o src/Parser.hs src/Parser.y"
    return emptyHookedBuildInfo
  }
