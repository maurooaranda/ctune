;;; ctune-tests.el --- tests for ctune.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Mauro Aranda

;; Author: Mauro Aranda <tbb@tbb-desktop>
;; Keywords: 

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

;;; Code:
(require 'ert)
(require 'ctune)

(ert-deftest ctune-test-symbol-at-point ()
  "Test that symbol at point reports a symbol if at it, nil if at a whitespace."
  (with-temp-buffer
    (c-mode)
    (insert "#ifndef FOO_H\n#include <foo.h>\n#endif\n")
    (goto-char (point-min))
    (search-forward "FOO")
    (should (string= (ctune-symbol-at-point-strict t) "FOO_H"))
    (forward-char 2)
    (should-not (string= (ctune-symbol-at-point-strict t) "FOO_H"))))

(provide 'ctune-tests)
;;; ctune-tests.el ends here
