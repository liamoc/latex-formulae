{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Image.LaTeX.Render
       ( -- * Rendering Formulas
         imageForFormula
       , Formula, Baseline
         -- * Errors
       , RenderError (..)
         -- * Options
         -- ** Environment Options
       , EnvironmentOptions (..)
       , defaultEnv
       , TempDirectoryHandling (..)
         -- ** Formula Options
       , FormulaOptions (..)
       , displaymath
       , math
       )
       where

import Codec.Picture
import Data.Maybe
import Control.Error.Util
import Data.List
import System.IO.Temp
import System.FilePath
import System.Process
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad
import System.Exit
import Control.Exception
import Control.Arrow(second)
import Control.Applicative
import Data.Monoid
import Prelude

-- | This type contains all possible errors than can happen while rendering an equation.
--   It includes all IO errors that can happen as well as more specific errors.
data RenderError = ImageIsEmpty -- ^ The equation produced an empty image
                 | CannotDetectBaseline -- ^ The baseline marker could not be found
                 | LaTeXFailure String -- ^ @latex@ returned a nonzero error code
                 | DVIPSFailure String -- ^ @dvips@ returned a nonzero error code
                 | IMConvertFailure String -- ^ @convert@ returned a nonzero error code
                 | IOException IOException -- ^ An 'IOException' occurred while managing the temporary files used to convert the equation
                 | ImageReadError String -- ^ The PNG image from ImageMagick could not be read by JuicyPixels.
                 deriving (Show)


data TempDirectoryHandling = UseSystemTempDir { nameTemplate :: String }
                           -- ^ A temporary directory with a name based on the given template will be created in the system temporary files location
                           | UseCurrentDir    { nameTemplate :: String }
                           -- ^ A temporary directory with a name based on the given template will be created in the current directory

data EnvironmentOptions
     = EnvironmentOptions { latexCommand :: String -- ^ Command to use for @latex@, default is @latex@
                          , dvipsCommand :: String -- ^ Command to use for @dvips@, default is @dvips@
                          , imageMagickCommand :: String -- ^ Command to use for ImageMagick's @convert@, default is @convert@
                          , latexArgs :: [String] -- ^ Any additional arguments for @latex@
                          , dvipsArgs :: [String] -- ^ Any additional arguments for @dvips@
                          , imageMagickArgs :: [String] -- ^ Any additional arguments for @convert@
                          , tempDir :: TempDirectoryHandling -- ^ How to handle temporary files
                          , tempFileBaseName :: String -- ^ The base name to use for the temporary files.
                          }

data FormulaOptions
     = FormulaOptions { preamble :: String -- ^ LaTeX preamble to use. Put your @\usepackage@ commands here.@ commands here.
                       , environment :: String -- ^ LaTeX environment in which the equation will be typeset, usually @math@ or @displaymath@
                       , dpi :: Int -- ^ DPI for the image to be rendered at. ~200 is good for retina displays, ~100 works OK for non-retina displays.
                       }

-- | Use the @amsmath@ package, the @displaymath@ environment, and 200dpi.
displaymath :: FormulaOptions
displaymath = FormulaOptions "\\usepackage{amsmath}" "displaymath" 200

-- | Use the @amsmath@ package, the @math@ environment, and 200dpi.
math :: FormulaOptions
math = FormulaOptions "\\usepackage{amsmath}\\usepackage{amsfonts}\\usepackage{stmaryrd}" "math" 200

-- | Sensible defaults for system environments. Works if @dvips@, @convert@, and @latex@ are recent enough and in your @$PATH@.
defaultEnv :: EnvironmentOptions
defaultEnv = EnvironmentOptions "latex" "dvips" "convert" [] [] [] (UseSystemTempDir "latex-eqn-temp") "working"

-- | A LaTeX formula, e.g @x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}@ for the quadratic formula. Do not include any @$@s to denote the environment, just
--   specify the environment in the 'FormulaOptions' instead.
type Formula = String

-- | Number of pixels from the bottom of the image to the typesetting baseline. Useful for setting your formulae inline with text.
type Baseline = Int

hoistE :: Monad m => Either e a -> ExceptT e m a
hoistE = ExceptT . return

-- | Convert a formula into a JuicyPixels 'DynamicImage', also detecting where the typesetting baseline of the image is.
imageForFormula :: EnvironmentOptions -> FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage))
imageForFormula (EnvironmentOptions {..}) (FormulaOptions {..}) eqn =
    bracket getCurrentDirectory setCurrentDirectory $ const $ withTemp $ \temp -> runExceptT $ do
      let doc = mconcat ["\\nonstopmode\n",
                 "\\documentclass[12pt]{article}\n",
                 "\\pagestyle{empty}\n", preamble,
                 "\\begin{document}\n",
                 "\\begin{", environment, "}\n",
                 ".",eqn,
                 "\\end{", environment, "}\n",
                 "\\end{document}\n"]
      io $ writeFile (temp </> tempFileBaseName <.> "tex") doc
      io $ setCurrentDirectory temp
      (c,o,e) <- io $ flip (readProcessWithExitCode latexCommand) "" $ latexArgs ++ [tempFileBaseName <.> "tex"]
      io $ removeFile (tempFileBaseName <.> "tex") 
      io $ removeFile (tempFileBaseName <.> "aux")
      when (c /= ExitSuccess) $ do
        io $ removeFile (tempFileBaseName <.> "dvi")
        throwE $ LaTeXFailure (o ++ "\n" ++ e)
      (c',o',e') <- io $ flip (readProcessWithExitCode dvipsCommand) "" $ dvipsArgs ++ ["-q", "-E", "-o", tempFileBaseName <.> "ps", tempFileBaseName <.> "dvi"]
      io $ removeFile (tempFileBaseName <.> "dvi")
      when (c' /= ExitSuccess) $ throwE $ DVIPSFailure (o' ++ "\n" ++ e')
      (c'', o'', e'') <- io $ flip (readProcessWithExitCode imageMagickCommand) "" $
                                [ "-density", show dpi
                                , "-bordercolor", "none"
                                , "-border", "1x1"
                                , "-trim"
                                , "-background", "none"
                                , "-splice","1x0"
                                ] ++ imageMagickArgs ++
                                [ tempFileBaseName <.> "ps", tempFileBaseName <.> "png" ]
      io $ removeFile (tempFileBaseName <.> "ps")
      when (c'' /= ExitSuccess) $ throwE $ IMConvertFailure (o'' ++ "\n" ++ e'')
      imgM <- io $ readImage (tempFileBaseName <.> "png")
      img <- withExceptT ImageReadError $ hoistE imgM
      io $ removeFile $ tempFileBaseName <.> "png"
      hoistE $ postprocess img
  where
    io = withExceptT IOException . tryIO
    withTemp a = case tempDir of
      UseSystemTempDir f -> withSystemTempDirectory f a
      UseCurrentDir f -> withTempDirectory "." f a

postprocess :: DynamicImage -> Either RenderError (Int, DynamicImage)
postprocess (ImageY8 i)     = second ImageY8     <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageY16 i)    = second ImageY16    <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYF i)     = second ImageYF     <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYA8 i)    = second ImageYA8    <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYA16 i)   = second ImageYA16   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGB8 i)   = second ImageRGB8   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGB16 i)  = second ImageRGB16  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBF i)   = second ImageRGBF   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBA8 i)  = second ImageRGBA8  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBA16 i) = second ImageRGBA16 <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYCbCr8 i) = second ImageYCbCr8 <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageCMYK8 i)  = second ImageCMYK8  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageCMYK16 i) = second ImageCMYK16 <$> postprocess' i (pixelAt i 0 0)


postprocess' :: (Eq a, Pixel a) => Image a -> a -> Either RenderError (Int, Image a)
postprocess' img bg
  = do startX <- note ImageIsEmpty $ listToMaybe $ dropWhile isEmptyCol [0.. imageWidth img - 1]
       let (dotXs, postXs) = break isEmptyCol [startX .. imageWidth img]
       postX <- note CannotDetectBaseline $ listToMaybe postXs
       let postY = (+ 2) $ average $ dotXs >>= (\x -> takeWhile (not . isEmpty x) (dropWhile (isEmpty x) [0..imageHeight img - 1]))
           average = uncurry div . foldl' (\(s,c) e -> (e+s,c+1)) (0,0)
           newHeight = imageHeight img
           newWidth  = imageWidth img - postX + 3
           baseline  = imageHeight img - postY
       let image = generateImage (pixelAt' . (+ postX)) newWidth newHeight
       return (baseline, image)
  where
    isEmptyCol x = all (isEmpty x) [0.. imageHeight img - 1]
    isEmpty x = (== bg) . pixelAt img x
    pixelAt' x y | x < imageWidth img && y < imageHeight img = pixelAt img x y
                 | otherwise = bg

