;;; latin2uyghur-test.el --- Test latin2uyghur -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'latin2uyghur)

(when noninteractive
  (transient-mark-mode))

(ert-deftest latin2uyghur-test-u2l-1 ()
  (should
   (string= "yëngi yëziq örügüc" (latin2uyghur-u2l "يېڭى يېزىق ئۆرۈگۈچ"))))

(ert-deftest latin2uyghur-test-l2u-1 ()
  (should
   (string= "يېڭى يېزىق ئۆرۈگۈچ" (latin2uyghur-l2u "yëngi yëziq örügüc"))))

(provide 'latin2uyghur-test)

;;; latin2uyghur-test.el ends here
