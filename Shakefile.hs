{-# LANGUAGE RecordWildCards #-}
import Clash.Shake
import Clash.Shake.Xilinx as Xilinx
import Clash.Shake.Intel as Intel
import qualified Clash.Shake.SymbiFlow as SymbiFlow

import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import Data.Foldable (forM_)

outDir :: FilePath
outDir = "_build"

data Flags = UseSymbiFlow deriving Eq

flags = [Option "" ["symbiflow"] (NoArg $ Right UseSymbiFlow) "Use SymbiFlow instead of vendor toolchain"]

main :: IO ()
main = shakeArgsWith shakeOptions{ shakeFiles = outDir } flags $ \flags targets -> pure $ Just $ withTargets targets $ do
    useConfig "build.mk"

    let useSymbiFlow = UseSymbiFlow `elem` flags
        xilinx7
          | useSymbiFlow = SymbiFlow.xilinx7
          | otherwise = Xilinx.vivado

    let boards =
            [ ("nexys-a7-50t", xilinx7 nexysA750T)
            , ("papilio-pro", Xilinx.ise papilioPro)
            , ("papilio-one", Xilinx.ise papilioOne)
            , ("de0-nano", Intel.quartus de0Nano)
            ]

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    (clash, kit) <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        "Pong"
        [ "-Wno-partial-type-signatures"
        , "-fclash-intwidth=32" -- To play nicely with Spartan 3 and 6
        ] $
        return ()
    phony "clashi" $ clash ["--interactive", "src/Pong.hs"]

    forM_ boards $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name) ("target" </> name) "Top"
        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):phonies
