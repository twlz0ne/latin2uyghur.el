;;; latin2uyghur.el --- Uyghur <==> Latin Alphabet Converter -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/10/29
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/latin2uyghur.el
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Uyghur <==> Latin Alphabet Converter.
;; Credit to @anwarmamat for the original work on https://cis.temple.edu/~anwar/code/latin2uyghur.html

;; ## Installation

;; ``` elisp
;; (quelpa '(latin2uyghur
;;           :repo "twlz0ne/latin2uyghur.el"
;;           :fetcher github))
;; ```

;; ## Usage

;; ``` elisp
;; (latin2uyghur-u2l "يېڭى يېزىق ئۆرۈگۈچ")
;; ;; => "yëngi yëziq örügüc"

;; (latin2uyghur-l2u "yëngi yëziq örügüc")
;; ;; => "يېڭى يېزىق ئۆرۈگۈچ"
;; ```

;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2020/10/29  Initial version.

;;; Code:

(require 'subr-x)

(defvar latin2uyghur-uyghur-vowels
  '("a" "e" "i" "o" "u" "ü" "ö" "ë"))

(defvar latin2uyghur-punctuations
  '(" " "," "." ":" "?" "'" "\"" "\n"))

(defvar latin2uyghur-alphabets
  '(("a"  . "\u0627")
    ("e"  . "\u06D5")
    ("b"  . "\u0628")
    ("p"  . "\u067E")
    ("t"  . "\u062A")
    ("j"  . "\u062C")
    ("ch" . "\u0686")
    ("x"  . "\u062E")
    ("d"  . "\u062F")
    ("r"  . "\u0631")
    ("z"  . "\u0632")
    ("zh" . "\u0698")
    ("s"  . "\u0633")
    ("sh" . "\u0634")
    ("gh" . "\u063A")
    ("f"  . "\u0641")
    ("q"  . "\u0642")
    ("k"  . "\u0643")
    ("g"  . "\u06AF")
    ("ng" . "\u06AD")
    ("l"  . "\u0644")
    ("m"  . "\u0645")
    ("n"  . "\u0646")
    ("h"  . "\u06BE")
    ("o"  . "\u0648")
    ("u"  . "\u06C7")
    ("ö"  . "\u06C6") 
    ("ü"  . "\u06C8") 
    ("w"  . "\u06CB")
    ("ë"  . "\u06D0") 
    ("i"  . "\u0649")
    ("y"  . "\u064A")
    ("?"  . "\u061F")
    ("."  . "\u065C")
    (","  . "\u060C"))
  "Letters for uyghur to latin.")

(defun latin2uyghur-u2l (string)
  "Convert uyghur STRING to latin."
  (string-join
   (mapcar (lambda (char)
             (or (car (rassoc char latin2uyghur-alphabets)) char))
           (mapcar (lambda (char)
                     (char-to-string char))
                   (remove 1574 (string-to-list string))))
   ""))

(defun latin2uyghur-l2u (string)
  "Convert latin STRING to uyghur."
  (let ((chars '()))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (condition-case _err
          (while t
            (let* ((char1 (or (ignore-errors
                                (char-to-string (char-after))) ""))
                   (char2 (or (ignore-errors
                                (char-to-string (char-after (1+ (point))))) ""))
                   (dchar (cdr (assoc (concat char1 char2)
                                      latin2uyghur-alphabets))))
              ;; add vowel
              (if (and (= (point) (point-min))
                       (member char1 latin2uyghur-uyghur-vowels))
                  (push "\u0626" chars)
                (if (and (member
                          (or (ignore-errors (char-to-string (char-before))) "")
                          latin2uyghur-punctuations)
                         (member char1 latin2uyghur-uyghur-vowels))
                    (push "\u0626" chars)))
              ;; add converted char
              (push (or dchar (cdr (assoc char1 latin2uyghur-alphabets)) char1)
                    chars)
              ;; forward double char (gh, ch, zh ....)
              (when dchar
                (forward-char)))
            (forward-char))
        (end-of-buffer)))
    (string-join (reverse chars) "")))

(provide 'latin2uyghur)

;;; latin2uyghur.el ends here
