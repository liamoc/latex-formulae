import Image.LaTeX.Render.Pandoc
import Image.LaTeX.Render
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter $ convertFormulaDataURI 2 defaultEnv fopts
 where fopts InlineMath  = math
       fopts DisplayMath = displaymath

