(TeX-add-style-hook
 "article.tex"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("ulem" "normalem") ("agda" "amakuri") ("dot2texi" "debug")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "agda"
    "dot2texi"
    "tikz-cd"
    "tikz"
    "tabularx"
    "enumitem"
    "geometry"
    "amsthm"
    "thmtools")
   (TeX-add-symbols
    '("AB" 1)
    "AD"
    "AIC"
    "AF"
    "AFd"
    "AP"
    "AR"
    "AK")
   (LaTeX-add-thmtools-declaretheoremstyles
    "mystyle")
   (LaTeX-add-thmtools-declaretheorems
    "theorem"
    "lemma"
    "corollary"
    "definition"
    "example"
    "remark"
    "notation"))
 :latex)

