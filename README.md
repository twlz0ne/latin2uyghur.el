# latin2uyghur.el

Uyghur <==> Latin Alphabet Converter.

Credit to @anwarmamat for the original work on https://cis.temple.edu/~anwar/code/latin2uyghur.html

## Installation

``` elisp
(quelpa '(latin2uyghur
          :repo "twlz0ne/latin2uyghur.el"
          :fetcher github))
```

## Usage

``` elisp
(latin2uyghur-u2l "يېڭى يېزىق ئۆرۈگۈچ")
;; => "yëngi yëziq örügüc"

(latin2uyghur-l2u "yëngi yëziq örügüc")
;; => "يېڭى يېزىق ئۆرۈگۈچ"
```

