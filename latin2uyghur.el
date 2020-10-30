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

(defcustom latin2uyghur-letter-h "x"
  "Use x/h as h."
  :group 'latin2uyghur
  :type 'string)

(defcustom latin2uyghur-letter-q "ch"
  "Use ch/q as q."
  :group 'latin2uyghur
  :type 'string)

(defcustom latin2uyghur-letter-sh "sh"
  "Use sh/x as sh."
  :group 'latin2uyghur
  :type 'string)

(defcustom latin2uyghur-letter-e "e"
  "Letter e/ë as e."
  :group 'latin2uyghur
  :type 'string)

(defvar latin2uyghur-uyghur-vowels
  '("a" "e" "i" "o" "u" "\u00FC" "\u00F6" "\u00EB" "\u00E9" "\u00E8"))

(defvar latin2uyghur-punctuations
  '(" " "," "." ":" "?" "'" "\"" "\u000A"))

(defun latin2uyghur-l2u-letters ()
  "Letters for latin to uyghur."
  `(("a"      . "\u0627")
    ("b"      . "\u0628")
    ("s"      . "\u0633")
    ("d"      . "\u062F")
    ("f"      . "\u0641")
    ("g"      . "\u06AF")
    ("i"      . "\u0649")
    ("j"      . "\u062C")
    ("k"      . "\u0643")
    ("l"      . "\u0644")
    ("m"      . "\u0645")
    ("n"      . "\u0646")
    ("o"      . "\u0648")
    ("\u00F6" . "\u06C6") ;; o
    ("p"      . "\u067E")
    ("ng"     . "\u06AD")
    ("c"      . "\u0686") ;; q
    ("r"      . "\u0631")
    ("s"      . "\u0633")
    ("t"      . "\u062A")
    ("u"      . "\u06C7")
    ("\u00FC" . "\u06C8") ;; v
    ("w"      . "\u06CB")
    ("y"      . "\u064A")
    ("z"      . "\u0632")
    ,@(if (string= latin2uyghur-letter-h "x")
          '(("h" . "\u06BE")
            ("x" . "\u062E")) ;; x is h in uyghur
        '(("h" . "\u062E")
          ("x" . "\u0634")))
    ,@(if (string= latin2uyghur-letter-q "ch")
          '(("ch" . "\u0686" ) ;; q
            ("q" . "\u0642")) ;; kh in hasim 
        '(("q" . "\u0686")))
    ,@(if (string= latin2uyghur-letter-sh "sh")
          '(("sh" . "\u0634")) ;; sh as xam
        '(("x" . "\u0634")))
    ,@(if (string= latin2uyghur-letter-e "e")
          '(("e"      . "\u06D5")  ;; ë
            ("\u00EB" . "\u06D0")  ;; ë
            ("\u00E9" . "\u06D0")  ;; ë
            ("\u00E8" . "\u06D0")) ;; ë
        '(("\u00EB" . "\u06D5")   ;; ë
          ("\u00E9" . "\u06D5")   ;; ë
          ("\u00E8" . "\u06D5")   ;; ë
          ("e"      . "\u06D0")))))

(defun latin2uyghur-u2l-letters ()
  "Letters for uyghur to latin."
  `(("a"      . "\u0627")
    ("b"      . "\u0628")
    ("s"      . "\u0633")
    ("d"      . "\u062F")
    ("f"      . "\u0641")
    ("g"      . "\u06AF")
    ("i"      . "\u0649")
    ("j"      . "\u062C")
    ("k"      . "\u0643")
    ("l"      . "\u0644")
    ("m"      . "\u0645")
    ("n"      . "\u0646")
    ("o"      . "\u0648")
    ("\u00F6" . "\u06C6") ;; o
    ("p"      . "\u067E")
    ("ng"     . "\u06AD")
    ("c"      . "\u0686") ;; q
    ("r"      . "\u0631")
    ("s"      . "\u0633")
    ("t"      . "\u062A")
    ("u"      . "\u06C7")
    ("\u00FC" . "\u06C8") ;; v
    ("w"      . "\u06CB")
    ("y"      . "\u064A")
    ("z"      . "\u0632")
    ("zh"     . "\u0698")
    ("gh"     . "\u063A")
    ("?"      . "\u061F")
    ("."      . "\u065C")
    (","      . "\u060C")
    ,@(if (string= latin2uyghur-letter-h "x")
          '(("h" . "\u06BE")  ;; h
            ("x" . "\u062E")) ;; x is h in uyghur
        '(("h" . "\u062E")   ;; x is h in uyghur
          ("x" . "\u0634"))) ;; sh as xam	
    ,@(if (string= latin2uyghur-letter-q "ch")
          '(("ch" . "\u0686")  ;; q
            ("q"  . "\u0642")) ;; kh in hasim 
        '(("q" . "\u0686"))) ;; q
    ,@(if (string= latin2uyghur-letter-sh "sh")
          '(("sh" . "\u0634")) ;; sh as xam
        '(("x" . "\u0634"))) ;; sh as xam
    ,@(if (string= latin2uyghur-letter-e "e")
          '(("e"      . "\u06D5")  ;; ë
            ("\u00EB" . "\u06D0")  ;; ë
            ("\u00E9" . "\u06D0")  ;; ë
            ("\u00E8" . "\u06D0")) ;; ë
        '(("\u00EB" . "\u06D5") ;; ë
          ("\u00E9" . "\u06D5") ;; ë
          ("\u00E8" . "\u06D5") ;; ë
          ("e"      . "\u06D0")))))

(defun latin2uyghur-u2l (string)
  "Convert uyghur STRING to latin."
  (let ((l2u-letters (latin2uyghur-u2l-letters)))
    (string-join
     (mapcar (lambda (char)
               (or (car (rassoc char l2u-letters)) char))
             (mapcar (lambda (char)
                       (char-to-string char))
                     (remove 1574 (string-to-list string))))
     "")))

(defun latin2uyghur-l2u (string)
  "Convert latin STRING to uyghur."
  (let ((u2l-letters (latin2uyghur-u2l-letters))
        (chars '()))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (condition-case _err
          (while t
            (let* ((char1 (or (ignore-errors
                                (char-to-string (char-after))) ""))
                   (char2 (or (ignore-errors
                                (char-to-string (char-after (1+ (point))))) ""))
                   (uchar (cdr (assoc (concat char1 char2) u2l-letters))))
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
              (push (or uchar (cdr (assoc char1 u2l-letters)) char1) chars)
              ;; forward double char (gh, ch, zh ....)
              (when uchar
                (forward-char)))
            (forward-char))
        (end-of-buffer)))
    (string-join (reverse chars) "")))

(provide 'latin2uyghur)

;;; latin2uyghur.el ends here
