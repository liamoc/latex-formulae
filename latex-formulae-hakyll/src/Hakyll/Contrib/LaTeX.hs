{-# LANGUAGE ViewPatterns #-}
-- | This module provides a basic framework to render LaTeX formulae inside Pandoc documents
--   for Hakyll pages.
--
--   See the latex-formulae page on GitHub for more information.
--
--      https://github.com/liamoc/latex-formulae#readme
--
module Hakyll.Contrib.LaTeX
       ( initFormulaCompilerDataURI
       , CacheSize
       , compileFormulaeDataURI
       ) where

import Image.LaTeX.Render
import Image.LaTeX.Render.Pandoc
import Text.Pandoc.Definition
import Hakyll.Core.Item
import Hakyll.Core.Compiler
import Data.Char
import qualified Data.Cache.LRU.IO as LRU
import Control.Applicative
import Prelude

-- | Number of formula images to keep in memory during a @watch@ session.
type CacheSize = Integer

-- | Creates a formula compiler with caching. Can be used as in the following minimal example:
--
-- >    main = do
-- >       renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
-- >       hakyll $
-- >         match "posts/*.markdown" $ do
-- >           route $ setExtension "html"
-- >           compile $ pandocCompilerWithTransformM (renderFormulae defaultPandocFormulaOptions)
--
initFormulaCompilerDataURI :: CacheSize -> EnvironmentOptions
                           -> IO (PandocFormulaOptions -> Pandoc -> Compiler Pandoc)
initFormulaCompilerDataURI cs eo = do
    mImageForFormula <- curry <$> memoizeLru (Just cs) (uncurry drawFormula)
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          mImageForFormula x y
    return $ \fo -> unsafeCompiler . convertAllFormulaeDataURIWith eachFormula fo
  where
    drawFormula x y = do
      putStrLn "      drawing..."
      imageForFormula eo x y

-- | A formula compiler that does not use caching, which works in a more drop-in fashion, as in:
--
-- > compile $ pandocCompilerWithTransformM (compileFormulaeDataURI defaultEnv defaultPandocFormulaOptions)
--
compileFormulaeDataURI :: EnvironmentOptions
                       -> PandocFormulaOptions
                       -> Pandoc -> Compiler Pandoc
compileFormulaeDataURI eo po =
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          putStrLn   "      drawing..."
          imageForFormula eo x y
    in unsafeCompiler . convertAllFormulaeDataURIWith eachFormula po

equationPreview :: String -> String
equationPreview (dropWhile isSpace -> x)
      | length x <= 16 = x
      | otherwise      = take 16 $ filter (/= '\n') x ++ "..."

memoizeLru :: Ord a => Maybe Integer -> (a -> IO b) -> IO (a -> IO b)
memoizeLru msize action = do
    lru <- LRU.newAtomicLRU msize
    return $ \arg -> do
        mret <- LRU.lookup arg lru
        case mret of
            Just ret -> return ret
            Nothing -> do
                ret <- action arg
                LRU.insert arg ret lru
                return ret
