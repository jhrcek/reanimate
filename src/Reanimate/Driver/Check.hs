{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Driver.Check
  ( checkEnvironment
  ) where

import           Control.Exception            (SomeException, handle)
import           Control.Monad
import           Data.Maybe
import           Data.Version
import           Reanimate.Misc               (runCmd_)
import           System.Directory             (findExecutable)
import           System.IO
import           System.IO.Temp
import           Text.ParserCombinators.ReadP
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Text.Printf

--------------------------------------------------------------------------
-- Check environment

checkEnvironment :: IO ()
checkEnvironment = do
    putStrLn "reanimate checks:"
    runCheck "Has ffmpeg" hasFFmpeg
    runCheck "Has dvisvgm" hasDvisvgm
    runCheck "Has povray" hasPovray
    runCheck "Has blender" hasBlender
    runCheck "Has inkscape" hasInkscape
    runCheck "Has convert" hasConvert
    runCheck "Has LaTeX" hasLaTeX
    runCheck ("Has LaTeX package '"++ "babel" ++ "'") $ hasTeXPackage "latex"
      "[english]{babel}"
    forM_ latexPackages $ \pkg ->
      runCheck ("Has LaTeX package '"++ pkg ++ "'") $ hasTeXPackage "latex" $
        "{"++pkg++"}"
    runCheck "Has XeLaTeX" hasXeLaTeX
    forM_ xelatexPackages $ \pkg ->
      runCheck ("Has XeLaTeX package '"++ pkg ++ "'") $ hasTeXPackage "xelatex" $
        "{"++pkg++"}"
  where
    latexPackages =
      ["preview"
      ,"amsmath"
      --,"amssymb"
      --,"dsfont"
      --,"setspace"
      --,"relsize"
      --,"textcomp"
      --,"mathrsfs"
      --,"calligra"
      --,"wasysym"
      --,"ragged2e"
      --,"physics"
      --,"xcolor"
      --,"textcomp"
      --,"xfrac"
      --,"microtype"
      ]
    xelatexPackages =
      ["ctex"]
    runCheck msg fn = do
      printf "  %-35s" (msg ++ ":")
      val <- fn
      case val of
        Left err -> print $ Doc.red $ Doc.text err
        Right ok -> print $ Doc.green $ Doc.text ok

-- latex, dvisvgm, xelatex

hasLaTeX :: IO (Either String String)
hasLaTeX = hasProgram "latex"

hasXeLaTeX :: IO (Either String String)
hasXeLaTeX = hasProgram "xelatex"

hasDvisvgm :: IO (Either String String)
hasDvisvgm = hasProgram "dvisvgm"

hasPovray :: IO (Either String String)
hasPovray = hasProgram "povray"

hasFFmpeg :: IO (Either String String)
hasFFmpeg = checkMinVersion minVersion <$> ffmpegVersion
  where
    minVersion = Version [4,1,3] []

hasBlender :: IO (Either String String)
hasBlender = checkMinVersion minVersion <$> blenderVersion
  where
    minVersion = Version [2,80] []

hasInkscape :: IO (Either String String)
hasInkscape = checkMinVersion minVersion <$> inkscapeVersion
  where
    minVersion = Version [0,92] []

hasConvert :: IO (Either String String)
hasConvert = checkMinVersion minVersion <$> convertVersion
  where
    minVersion = Version [7,0,0] []

ffmpegVersion :: IO (Maybe Version)
ffmpegVersion = extractVersion "ffmpeg" ["-version"] $ \line ->
      case take 3 $ words line of
        ["ffmpeg", "version", vs] -> vs
        _ -> ""

blenderVersion :: IO (Maybe Version)
blenderVersion = extractVersion "blender" ["--version"] $ \line ->
    case take 2 (words line) of
      ["Blender", vs] -> vs
      _ -> ""

inkscapeVersion :: IO (Maybe Version)
inkscapeVersion = extractVersion "inkscape" ["--version"] $ \line ->
    case take 2 $ words line of
      ["Inkscape", vs] -> vs
      _ -> ""

convertVersion :: IO (Maybe Version)
convertVersion = extractVersion "convert" ["-version"] $ \line ->
    case take 3 $ words line of
      ["Version:", "ImageMagick", vs] -> vs
      _ -> ""

checkMinVersion :: Version -> Maybe Version -> Either String String
checkMinVersion _minVersion Nothing = Left "no"
checkMinVersion minVersion (Just vs)
  | vs < minVersion = Left $ "too old: " ++ showVersion vs ++ " < " ++ showVersion minVersion
  | otherwise       = Right (showVersion vs)

extractVersion :: FilePath -> [String] -> (String -> String) -> IO (Maybe Version)
extractVersion execPath args outputFilter = do
  mbPath <- findExecutable execPath
  case mbPath of
    Nothing -> return Nothing
    Just path -> do
      ret <- runCmd_ path args
      case ret of
        Left{} -> return $ Just noVersion
        Right out ->
          pure $ Just $ fromMaybe noVersion $ parseVS $ outputFilter out
  where
    noVersion = Version [] []
    parseVS vs = listToMaybe $ reverse
      [ v | (v, _) <- readP_to_S parseVersion vs ]

hasTeXPackage :: FilePath -> String -> IO (Either String String)
hasTeXPackage exec pkg = handle (\(_::SomeException) -> return $ Left "n/a") $
    withSystemTempDirectory "reanimate" $ \tmp_dir -> withTempFile tmp_dir "test.tex" $ \tex_file tex_handle -> do
      hPutStr tex_handle tex_document
      hPutStr tex_handle $ "\\usepackage" ++ pkg ++ "\n"
      hPutStr tex_handle "\\begin{document}\n"
      hPutStr tex_handle "blah\n"
      hPutStr tex_handle tex_epilogue
      hClose tex_handle
      ret <- runCmd_ exec ["-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
      return $ case ret of
        Right{} -> Right "OK"
        Left{}  -> Left "missing"
  where
    tex_document = "\\documentclass[preview]{standalone}\n"
    tex_epilogue =
      "\n\
      \\\end{document}"

hasProgram :: String -> IO (Either String String)
hasProgram exec = do
  mbPath <- findExecutable exec
  return $ case mbPath of
    Nothing   -> Left $ "'" ++ exec ++ "' not found"
    Just path -> Right path
