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

(defmacro ctune-test-with-buffer (&rest body)
  "Evaluate BODY in a temporary buffer with major-mode `c-mode'."
  `(with-temp-buffer
     (c-mode)
     ,@body))

(ert-deftest ctune-test-symbol-at-point ()
  "Test that symbol at point reports a symbol if at it, nil if at a whitespace."
  (ctune-test-with-buffer
   (insert "#ifndef FOO_H\n#include <foo.h>\n#endif\n")
   (goto-char (point-min))
   (search-forward "FOO")
   (should (string= (ctune-symbol-at-point-strict t) "FOO_H"))
   (forward-char 2)
   (should-not (string= (ctune-symbol-at-point-strict t) "FOO_H"))))

(ert-deftest ctune-test-add-noise-macro-error ()
  "Test that `ctune--add-noise-macro' fails upon a bad argument."
  (ctune-test-with-buffer
   (should-error (ctune--add-noise-macro "FOO" 'bar))))

(ert-deftest ctune-test-add-noise-macro ()
  "Test that adding a noise macro works."
  (ctune-test-with-buffer
   (should-not c-noise-macro-names)
   (ctune--add-noise-macro "_GL_ATTRIBUTE_PURE" 'c-noise-macro-names)
   (should (string= "_GL_ATTRIBUTE_PURE" (car c-noise-macro-names)))))

(ert-deftest ctune-test-remove-noise-macro ()
  "Test that removing a noise macro works."
  (ctune-test-with-buffer
   (should-not c-noise-macro-names)
   (ctune--add-noise-macro "_GL_ATTRIBUTE_PURE" 'c-noise-macro-names)
   (should (string= "_GL_ATTRIBUTE_PURE" (car c-noise-macro-names)))
   (ctune--add-noise-macro "_GL_ATTRIBUTE_PURE" 'c-noise-macro-names t)
   (should-not c-noise-macro-names)))

(ert-deftest ctune-test-save-directory-variable-error ()
  (ctune-test-with-buffer
   (should-error (ctune-save-directory-variable major-mode 'foo))))

(provide 'ctune-tests)
;;; ctune-tests.el ends here
