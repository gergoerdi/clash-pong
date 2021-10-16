{-# LANGUAGE RecordWildCards #-}
import Clash.Shake
import Clash.Shake.Xilinx as Xilinx
import Clash.Shake.Intel as Intel

import Development.Shake
import Development.Shake.FilePath
import Data.Foldable (forM_)

outDir :: FilePath
outDir = "_build"

targets =
    [ ("nexys-a7-50t", Xilinx.vivado nexysA750T)
    , ("papilio-pro", Xilinx.ise papilioPro)
    , ("papilio-one", Xilinx.ise papilioOne)
    , ("de0-nano", Intel.quartus de0Nano)
    ]

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    kit@ClashKit{..} <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        "Pong"
        [ "-Wno-partial-type-signatures"
        , "-fclash-intwidth=32" -- To play nicely with Spartan 3 and 6
        ] $
        return ()
    phony "clashi" $ clash ["--interactive", "src/Pong.hs"]

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name) ("target" </> name) "Top"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):phonies
