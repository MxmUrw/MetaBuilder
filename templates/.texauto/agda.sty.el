(TeX-add-style-hook
 "agda.sty"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8x") ("fontenc" "T1") ("tipa" "safe")))
   (TeX-run-style-hooks
    "ucs"
    "inputenc"
    "fontspec"
    "unicode-math"
    "fontenc"
    "amsfonts"
    "amssymb"
    "tipa"
    "catchfilebetweentags"
    "xstring")
   (TeX-add-symbols
    '("skiparg" ["argument"] 0)
    '("AgdaRef" ["argument"] 1)
    '("AFn" 1)
    '("ADT" 1)
    '("AgdaIndent" 1)
    '("AgdaTrailingText" 0)
    '("AgdaHole" 1)
    '("AgdaTerminationProblem" 1)
    '("AgdaArgument" 1)
    '("AgdaRecord" 1)
    '("AgdaPrimitive" 1)
    '("AgdaPostulate" 1)
    '("AgdaModule" 1)
    '("AgdaMacro" 1)
    '("AgdaFunction" 1)
    '("AgdaField" 1)
    '("AgdaDatatype" 1)
    '("AgdaCoinductiveConstructor" 1)
    '("AgdaInductiveConstructor" 1)
    '("AgdaGeneralizable" 1)
    '("AgdaBound" 1)
    '("AgdaFormat" 2)
    '("AgdaPrimitiveType" 1)
    '("AgdaNoSpaceMath" 1)
    '("AgdaMonoFontStyle" 1)
    '("AgdaBoundFontStyle" 1)
    '("AgdaCommentFontStyle" 1)
    '("AgdaStringFontStyle" 1)
    '("AgdaKeywordFontStyle" 1)
    '("AgdaFontStyle" 1)
    '("AgdaLink" 1)
    '("AgdaTarget" 1)
    '("AgdaTargetHelper" 1)
    '("AgdaLookup" 3)
    '("AgdaList" 0)
    '("DeclareUnicodeCharacter" 2)
    "AgdaColourScheme"
    "AgdaComment"
    "AgdaOption"
    "AgdaPragma"
    "AgdaKeyword"
    "AgdaString"
    "AgdaNumber"
    "AgdaSymbol"
    "AgdaOperator"
    "AgdaFixityOp"
    "AgdaDottedPattern"
    "AgdaUnsolvedMeta"
    "AgdaIncompletePattern"
    "AgdaError"
    "AgdaCatchallClause"
    "AgdaDeadcode"
    "AgdaMathRMSubscript"
    "AgdaMathBFSubscript"
    "AgdaSansSubscript"
    "AgdaMathRM"
    "AgdaMathBF"
    "AgdaSans"
    "AgdaCodeStyle"
    "mathindent"
    "Agda"
    "AgdaNoSpaceAroundCode"
    "AgdaSpaceAroundCode"
    "AgdaSpace"
    "NegAgdaSpace"
    "AgdaIndentSpace"
    "blank"
    "AgdaUnderscore"
    "section"
    "subparagraph"
    "paragraph"
    "subsubsection"
    "subsection"
    "AgdaOpenBracket"
    "AgdaCloseBracket"
    "AgdaHide")
   (LaTeX-add-labels
    "Agda@DoNotSuppressSpaceAfter@#1")
   (LaTeX-add-environments
    "nscenter"
    "leveldown"
    "levelup"
    "AgdaAlign"
    "AgdaSuppressSpace"
    "AgdaMultiCode")
   (LaTeX-add-counters
    "AgdaIndex")
   (LaTeX-add-lengths
    "AgdaEmptySkip"
    "AgdaEmptyExtraSkip"))
 :latex)

