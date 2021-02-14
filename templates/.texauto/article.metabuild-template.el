(TeX-add-style-hook
 "article.metabuild-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("ulem" "normalem") ("agda" "amakuri") ("dot2texi" "debug")))
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
    "enumitem"))
 :latex)

