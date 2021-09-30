{-# LANGUAGE RecordWildCards #-}
import Clash.Shake
import Clash.Shake.Xilinx
import Clash.Shake.Intel

import Development.Shake
import Development.Shake.FilePath
import Data.Foldable (forM_)

outDir :: FilePath
outDir = "_build"

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

    let targets =
            [ ("nexys-a7-50t", xilinxVivado nexysA750T)
            , ("papilio-pro", xilinxISE papilioPro)
            , ("papilio-one", xilinxISE papilioOne)
            , ("de0-nano", intelQuartus de0Nano)
            , ("arrow-deca", intelQuartus arrowDECA)
            ]

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name) ("target" </> name) "Top"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):phonies
