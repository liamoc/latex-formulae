import Image.LaTeX.Render.Pandoc
import Image.LaTeX.Render
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter $ convertFormulaDataURI defaultEnv defaultPandocFormulaOptions

