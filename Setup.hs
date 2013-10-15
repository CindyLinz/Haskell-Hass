import Distribution.Simple
import Distribution.PackageDescription
import System.Cmd

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
    putStrLn "happy src/Parser.y"
    system "happy src/Parser.y"
    return emptyHookedBuildInfo
  }
