{-# LANGUAGE LambdaCase #-}
module Image.LaTeX.Render.Pandoc
       ( -- * Data URIs
         convertFormulaDataURI
       , convertAllFormulaeDataURI
         -- * Separate Files
       , convertFormulaFiles
       , convertAllFormulaeFiles
         -- ** Name Supplies
       , NameSupply
       , newNameSupply
         -- * Options
       , PandocFormulaOptions(..)
       , ShrinkSize
       , defaultPandocFormulaOptions
         -- ** Error display functions
       , hideError
       , displayError
         -- * Generalised versions
         -- ** Data URIs
       , convertFormulaDataURIWith
       , convertAllFormulaeDataURIWith
         -- ** Files
       , convertFormulaFilesWith
       , convertAllFormulaeFilesWith
       ) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Image.LaTeX.Render
import Codec.Picture
import Control.Applicative
import Data.IORef
import System.FilePath

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as BS

dims :: Image a -> (Int,Int)
dims = liftA2 (,) imageWidth imageHeight

dimensions :: DynamicImage -> (Int,Int)
dimensions (ImageRGB8 i)   = dims i
dimensions (ImageRGBA8 i)  = dims i
dimensions (ImageRGB16 i)  = dims i
dimensions (ImageRGBA16 i) = dims i
dimensions (ImageY8 i)     = dims i
dimensions (ImageY16 i)    = dims i
dimensions (ImageYA8 i)    = dims i
dimensions (ImageYA16 i)   = dims i
dimensions _               = error "Unsupported image format somehow!"


-- | All options pertaining to the actual display of formulae. 
data PandocFormulaOptions = PandocFormulaOptions
        { shrinkBy       :: ShrinkSize
          -- ^ Denominator for all dimensions. Useful for displaying high DPI images in small sizes, for retina displays. Otherwise set to 1.
        , errorDisplay   :: RenderError -> Inline
          -- ^ How to display various errors (such as LaTeX errors). Usually this can just be @displayError@ but you may wish @hideError@
          --   to avoid putting potentially secure information into the output page.
        , formulaOptions :: MathType -> FormulaOptions
          -- ^ LaTeX environment settings, including the preamble, for each equation type (display and inline)
        }

-- | A set of sensible defaults for formula options.
defaultPandocFormulaOptions :: PandocFormulaOptions
defaultPandocFormulaOptions = PandocFormulaOptions
   { shrinkBy = 2
   , errorDisplay = displayError
   , formulaOptions = \case DisplayMath -> displaymath; _ -> math
   }

-- | Denominator for various dimensions. For high DPI displays, it can be useful to use values of 2 or 4, so that the dimensions
--   of the image are a fraction of the actual image size, and the image appears more crisp. Otherwise, a value of 1 will always
--   produce sensible, if somewhat pixelated results.
type ShrinkSize = Int

-- | Convert a formula in a pandoc document to an image, embedding the image into the HTML using Data URIs.
convertFormulaDataURI
  :: EnvironmentOptions           -- ^ System environment settings
  -> PandocFormulaOptions         -- ^ Formula display settings
  -> Inline -> IO Inline
convertFormulaDataURI = convertFormulaDataURIWith . imageForFormula

-- | Convert all formulae in a pandoc document to images, embedding the images into the HTML using Data URIs.
convertAllFormulaeDataURI
  :: EnvironmentOptions           -- ^ System environment settings
  -> PandocFormulaOptions         -- ^ Formula display settings
  -> Pandoc -> IO Pandoc
convertAllFormulaeDataURI e = walkM . convertFormulaDataURI e

-- | A generalisation of 'convertFormulaDataURI' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaDataURIWith
  :: (FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage)))
     -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
  -> PandocFormulaOptions -- ^ Formula display settings
  -> Inline -> IO Inline
convertFormulaDataURIWith f o (Math t s) = f (formulaOptions o t) s >>= \case
   Left e -> return $ errorDisplay o e
   Right (b,i) -> let
       Right bs = encodeDynamicPng i
       dataUri = "data:image/png;base64," ++ BS.unpack (B64.encode bs)
       (ow,oh) = dimensions i
       (w,h) = (ow `div` shrinkBy o, oh `div` shrinkBy o)
     in return $ RawInline (Format "html") $
        "<img width="  ++ show w ++
            " alt=\"" ++ processAltString s ++ "\"" ++
            " height=" ++ show h ++
            " src=\""  ++ dataUri ++ "\"" ++
            " class="  ++ (case t of InlineMath -> "inline-math"; _ -> "display-math") ++
            " style=\"margin:0; vertical-align:-" ++ show (b `div` shrinkBy o) ++ "px;\"/>"
   where processAltString = (>>= \case
                 '<'  -> "&lt;"
                 '>'  -> "&gt;"
                 '&'  -> "&amp;"
                 '"'  -> "&quot;"
                 '\'' -> "&39;"
                 '\n' -> " "
                 '\r' -> " "
                 '\t' -> " "
                 x    -> [x])
convertFormulaDataURIWith _ _ x = return x

-- | A generalisation of 'convertAllFormulaeDataURI' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertAllFormulaeDataURIWith
  :: (FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage)))
     -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
  -> PandocFormulaOptions -- ^ Formula display settings
  -> Pandoc -> IO Pandoc
convertAllFormulaeDataURIWith f = walkM . convertFormulaDataURIWith f

-- | If we use files for the images, we need some way of naming the image files we produce
--   A NameSupply provides us with a source of unique names via an ever-increasing integer.
--   It's important that any invocation of 'convertFormulaFiles' or 'convertAllFormulaeFiles'
--   that shares the same image storage directory will also use the same name supply, or they
--   will overwrite each others images.
type NameSupply = IORef Int

-- | Create a new name supply.
newNameSupply :: IO NameSupply
newNameSupply = newIORef 0

-- | A generalisation of 'convertFormulaFiles' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertFormulaFilesWith
  :: (FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage)))
     -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
  -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
  -> FilePath                     -- ^ Name of image directory where images will be stored
  -> PandocFormulaOptions         -- ^ Formula display settings
  -> Inline -> IO Inline
convertFormulaFilesWith f ns bn o (Math t s) = f (formulaOptions o t) s >>= \case
   Left e -> return $ errorDisplay o e
   Right (b,i) -> do
     fn <- readIORef ns
     modifyIORef ns (+1)
     let uri = bn </> show fn <.> "png"
         (ow,oh) = dimensions i
         (w,h) = (ow `div` shrinkBy o, oh `div` shrinkBy o)
     _ <- writeDynamicPng uri i
     return $ RawInline (Format "html") $
        "<img width="  ++ show w ++
            " height=" ++ show h ++
            " src=\""  ++ uri ++ "\"" ++
            " class="  ++ (case t of InlineMath -> "inline-math"; _ -> "display-math") ++
            " style=\"margin:0; vertical-align:-" ++ show (b `div` shrinkBy o) ++ "px;\"/>"
convertFormulaFilesWith _ _ _ _ x = return x

-- | Convert a formula in a pandoc document to an image, storing the images in a separate directory.
convertFormulaFiles
  :: EnvironmentOptions           -- ^ System environment settings
  -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
  -> FilePath                     -- ^ Name of image directory where images will be stored
  -> PandocFormulaOptions         -- ^ Formula display settings
  -> Inline -> IO Inline
convertFormulaFiles = convertFormulaFilesWith . imageForFormula

-- | Convert every formula in a pandoc document to an image, storing the images in a separate directory.
convertAllFormulaeFiles
  :: EnvironmentOptions           -- ^ System environment settings
  -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
  -> FilePath                     -- ^ Name of image directory where images will be stored
  -> PandocFormulaOptions         -- ^ Formula display settings
  -> Pandoc -> IO Pandoc
convertAllFormulaeFiles eo ns fp = walkM . convertFormulaFiles eo ns fp

-- | A generalisation of 'convertAllFormulaeFiles' which allows the actual image rendering
--   function to be customised, so that (e.g) caching can be added or other image processing.
convertAllFormulaeFilesWith
  :: (FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage)))
     -- ^ Function that renders a formula, such as @imageForFormula defaultEnv@
  -> NameSupply                   -- ^ Unique file name supply. Reuse this for every invocation that shares the same image directory.
  -> FilePath                     -- ^ Name of image directory where images will be stored
  -> PandocFormulaOptions         -- ^ Formula display settings
  -> Pandoc -> IO Pandoc
convertAllFormulaeFilesWith x y a = walkM . convertFormulaFilesWith x y a

-- | Render all errors simply as "Error"
hideError :: RenderError -> Inline
hideError = const $ Str blank
  where
    blank = "Error"

-- | Render errors nicely, in order to show any problems clearly, with all information intact.
displayError :: RenderError -> Inline
displayError ImageIsEmpty           = pandocError [Str "The rendered image was empty"]
displayError CannotDetectBaseline   = pandocError [Str "Cannot detect baseline in rendered image"]
displayError (LaTeXFailure str)     = pandocError [Str "LaTeX failed:", LineBreak, Code nullAttr str]
displayError (DVIPSFailure str)     = pandocError [Str "DVIPS failed:", LineBreak, Code nullAttr str]
displayError (IMConvertFailure str) = pandocError [Str "convert failed:", LineBreak, Code nullAttr str]
displayError (ImageReadError str)   = pandocError [Str "Error reading image:", LineBreak, Code nullAttr str]
displayError (IOException e)        = pandocError [Str "IO Exception:", LineBreak, Code nullAttr $ show e]

pandocError :: [Inline] -> Inline
pandocError = Strong . (Emph [Str "Error:"] :)
