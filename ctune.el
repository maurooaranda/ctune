;;; ctune.el --- Tune out CC Mode Noise Macros -*- lexical-binding: t -*-

;; Copyright (C) 2019 Mauro Aranda.

;; Author: Mauro Aranda <maurooaranda@gmail.com>
;; Version: 0.2
;; Package-Version: 0.2
;; Package-Requires: ((emacs "26.1"))
;; Keywords: c convenience
;; URL:  https://github.com/maurooaranda/ctune
;; Created: 20 May 2019.

;; This file is NOT part of GNU Emacs.

;; ctune is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ctune is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ctune.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ctune: A package to tune out CC Mode Noise Macros.

;; When working with projects that extensively use C-style Macros,
;; the buffer you are editing can contain 'Noise Macros' (e.g., C Macros that
;; define GCC attributes).  These 'Noise Macros' (see the CC Mode Manual for
;; more info) can confuse CC Mode, causing bad indentation, fontification, etc.
;; This package is an attempt to make the addition and removal of these
;; entities somewhat more automated than what CC Mode offers.

;; At the time of writing this package, I know of three ways of handling the
;; issue.
;; 1) Editing `dir-locals-file' by hand: Find the Noise Macro that is bothering
;; you, and add an entry for the correspondent major mode key in the alist of
;; `dir-locals-file'.  You might be working in a project that already contains
;; such entry, so changing the `dir-locals-file' is easy, although somewhat
;; tedious.

;; 2) Adding the entries manually, while editing the buffer: You notice the
;; Noise Macro, it bothers you, and then you add the entry to the list, like
;; this: (add-to-list 'c-noise-macro-names '("NOISE_MACRO_THAT_BOTHERS_ME"))
;; This is OK, but perhaps you make a mistake and use `c-noise-macro-names'
;; when `c-noise-macro-with-parens-names' was needed, or it bothers you to
;; have to type all the Noise Macro name, or killing and yanking the name still
;; bothers you, etc.  Plus, after adding the Noise Macro to its right variable
;; you might forgot that you need to eval (c-make-noise-macro-regexps).
;; Then, you might want to save the values you added, so you can type
;; M-x add-dir-local-variable to do the saving.

;; 3) Modify `c-noise-macro-names' and `c-noise-macro-with-parens-names' in
;; your init file: I don't like this option, but it does the job.  It's pretty
;; easy and straight-forward.

;; ctune tries to handle both 3 ways by providing an easy way to add the Noise
;; Macro to the right variable and save the values in the `dir-locals-file'
;; file.

;;; Suggestions/Ideas:

;; GNU C Coding Conventions, and other coding conventions as well, say that
;; macro names should be all upper case.  Maybe check that when asked to add
;; a macro name (but ctune should not enforce it, so there could be a user
;; option to turn off the query).

;;; Bugs:

;;; Code:

;; Dependencies:

;; Emacs 26.1 or more:
(when (or (not (fboundp 'version<))
	  (version< emacs-version "26.1"))
  (error "Emacs 26.1 or more required"))

;; Require cl-lib only when saving.
;; We use cl-set-exclusive-or, but that is autoloaded, so we don't load
;; cl-lib until needed.
(eval-when-compile
  (require 'cl-lib))

;; Silence the byte compiler.
(defvar c-noise-macro-names)
(defvar c-noise-macro-with-parens-names)

(declare-function c-make-noise-macro-regexps "cc-vars.el")

;; Customizable options:

(defgroup ctune nil
  "Customizable group of ctune options."
  :group 'c)

;; Maybe users don't want to write to `dir-locals-file', so let them choose.
(defcustom ctune-save-noise-macros-automatically 'ask
  "Specify how saving of CC Noise Macros in `dir-locals-file' is done.
With a value of nil, ctune doesn't modify never the `dir-locals-file', unless
you execute `ctune-save-noise-macros'.  With a value of t, ctune saves the
new values of the CC Noise Macros when killing the buffer or killing Emacs.
When set to 'ask, you will be prompted about saving the new values when
killing the buffer or killing Emacs."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "Ask me" ask))
  :group 'ctune
  :version "26.1"
  :package-version '(ctune . "0.1"))

(defcustom ctune-kbd-alist
  '((ctune-add-noise-macro . "C-c C-#"))
  "Association list of keybindings for the minor mode `ctune-mode'.
KEYS are the command names, while VALUES are the strings that represent the
keybinding to bind to the command."
  :type 'alist
  :group 'ctune
  :version "26.1"
  :package-version '(ctune . "0.1"))

;; Variables:

;; The following variables are useful for checking if there was a modification
;; in the Noise Macros values.  This way, we avoid unnecessary saving, and/or
;; unnecessary prompting.
;; Both variables are made local when activating the minor mode.
(defvar ctune-prev-noise-macro-names nil
  "Hold the previous (or first) value of `c-noise-macro-names'.")

(defvar ctune-prev-noise-macro-with-parens-names nil
  "Hold the previous (or first) value of `c-noise-macro-with-parens-names'.")

;; We check for both "with-parens" and "without-parens" before effectively
;; saving, or even prompting.  So we need to store the appropriate variables
;; to save.
(defvar ctune-save-these-vars nil
  "List that holds what variables of Noise Macros to save.
That is, can contain nil, 'with-parens, 'without-parens or both.")

;; Helper functions:

;; I find `thing-at-point' buggy in this case, so this should be a replacement.
(defun ctune-symbol-at-point-strict (&optional no-properties)
  "Find the symbol at point, returning it as a string.
Doesn't report a symbol on a whitespace, as `thing-at-point' does in some
occasions.
Argument NO-PROPERTIES means the same as in `thing-at-point'."
  (let ((orig (point))
	text beg end)
    (ignore-errors
      (save-excursion
	;; `thing-at-point' checks for more things, but let's see if this is
	;; enough.
	(setq end (progn (forward-symbol 1) (point)))
	(setq beg (progn (forward-symbol -1) (point)))
	(when (<= beg orig) ; there was a symbol at point.
	  (setq text (buffer-substring beg end))
	  (when (and text no-properties (sequencep text))
	    (set-text-properties 0 (length text) nil text))
	  text)))))

(defsubst ctune--reset-values ()
  "Reset values for next saving attempt."
  (setq ctune-prev-noise-macro-names c-noise-macro-names
	ctune-prev-noise-macro-with-parens-names
	c-noise-macro-with-parens-names
	ctune-save-these-vars nil))

(defsubst ctune--add-noise-macro (macro-name macro-names-list &optional removep)
  "Add or remove the string MACRO-NAME to the list variable MACRO-NAMES-LIST.
MACRO-NAMES-LIST should be one of `c-noise-macro-names' or
`c-noise-macro-with-parens-names'.
With optional argument REMOVEP non-nil, remove it."
  (if removep
      ;; Don't report removal, when the Noise Macro didn't exist.
      (if (not (member macro-name (symbol-value macro-names-list)))
	  (error "%s is not a Noise Macro" macro-name)
	(set macro-names-list (delete macro-name (symbol-value
						  macro-names-list)))
	(c-make-noise-macro-regexps)
	(message "%s removed from Noise Macros" macro-name))
    ;; Don't unify the (message ...) thing, because we want to report the action
    ;; if we are sure we updated the CC Noise Macros,
    ;; and not only the variables.
    (add-to-list macro-names-list macro-name t)
    (c-make-noise-macro-regexps)
    (message "%s added to Noise Macros" macro-name))
  ;; We don't mess with the buffer contents, but force fontification of the
  ;; buffer in a smart way.  The region passed as argument is small, so it
  ;; shouldn't be a problem if `macro-name' wasn't a Noise Macro.
  (run-hook-with-args 'after-change-functions (point) (1+ (point)) 0))

(defsubst ctune-save-directory-variable (mode sym)
  "Add the value holded by SYM to the MODE entry in the `dir-locals-file'."
  (add-dir-local-variable mode sym (symbol-value sym))
  ;; Don't freak out: current-buffer is the `dir-locals-file' buffer.
  (write-file (expand-file-name buffer-file-name) nil)
  (kill-buffer))

(defun ctune-save-noise-macros-maybe ()
  "Save Noise Macros, if the user wants to.
Only ask the user when `ctune-save-noise-macros-automatically' is 'ask.
This function is hooked into `kill-buffer-hook' and `kill-emacs-hook'."
  ;; Don't even bother checking the values, if user doesn't want to save.
  (when ctune-save-noise-macros-automatically
    (if (cl-set-exclusive-or ctune-prev-noise-macro-names
			     c-noise-macro-names :test #'equal)
	(add-to-list 'ctune-save-these-vars 'without-parens))
    (if (cl-set-exclusive-or ctune-prev-noise-macro-with-parens-names
			     c-noise-macro-with-parens-names :test #'equal)
	(add-to-list 'ctune-save-these-vars 'with-parens))
    (if (and ctune-save-these-vars
	     (or (eq ctune-save-noise-macros-automatically t)
		 (yes-or-no-p
		   "Save the CC Noise Macros to the directory locals file? ")))
	(ctune-save-noise-macros)
      ;; If we didn't save, but `ctune-save-these-vars' was filled with some
      ;; values, we reset it here.
      (setq ctune-save-these-vars nil))))

;; Commands:

(defun ctune-add-noise-macro (&optional removep)
  "Add or remove the macro name at point to the Noise Macro names of CC Mode.
With a prefix argument REMOVEP non-nil, remove the macro name.

The command finds if it should modify either `c-noise-macro-names' or
`c-noise-macro-with-parens-names', by looking forward for the presence of
an opening parenthesis.

Warning: DO NOT call it when point is not in a macro name, you will confuse it.
CC Mode imposes no limits on what you can put as a Noise Macro, and neither
does this command."
  (interactive "P")
  (unless (memq major-mode '(c-mode c++-mode objc-mode))
    (error "Major mode %s not supported!" major-mode))
  (let ((name (if (nth 8 (syntax-ppss)) ; Are we in a string or a comment?
		  (error "Point can't be at a comment or a string!")
		;; Don't use `thing-at-point', because it has troubles when
		;; point is at a whitespace.  There's an easy workaround, but
		;; it is not worth it.
		(ctune-symbol-at-point-strict t)))
	with-parens)
    (if name
	(save-excursion
	  (forward-symbol 1)
	  ;; Account for possible white space after the name of the macro,
	  ;; or even a escaped newline.
	  (skip-chars-forward "[:space:]\\\\\n")
	  (setq with-parens (eq (char-after) ?\())
	  (ctune--add-noise-macro name
				  (if with-parens
				      'c-noise-macro-with-parens-names
				    'c-noise-macro-names)
				  removep))
      (error "No symbol at point!"))))

(defun ctune-save-noise-macros ()
  "Save the new values of Noise Macros to the `dir-locals-file'.
Saves the values of `c-noise-macro-names' and `c-noise-macro-with-parens-names'
as `add-dir-local-variable' would do interactively."
  (interactive)
  (unless (memq major-mode '(c-mode c++-mode objc-mode))
    (error "Major mode %s not supported!" major-mode))
  (save-excursion
    ;; No point in writing a loop for this two variables.  But sadly, we need
    ;; to write the file and kill the buffer each time.
    ;; In `ctune-save-noise-macros-maybe', we checked if saving the Noise Macros
    ;; is needed.  So we only check here if we were called interactively.
    (let ((already-checked (not (called-interactively-p 'any))))
      (if already-checked
	  (when (memq 'without-parens ctune-save-these-vars)
	    (ctune-save-directory-variable major-mode 'c-noise-macro-names))
	(when (cl-set-exclusive-or ctune-prev-noise-macro-names
				   c-noise-macro-names
				   :test #'equal)
	  (ctune-save-directory-variable major-mode 'c-noise-macro-names)))
      (if already-checked
	  (when (memq 'with-parens ctune-save-these-vars)
	    (ctune-save-directory-variable major-mode
					   'c-noise-macro-with-parens-names))
	(when (cl-set-exclusive-or ctune-prev-noise-macro-with-parens-names
				   c-noise-macro-with-parens-names
				   :test #'equal)
	  (ctune-save-directory-variable major-mode
					 'c-noise-macro-with-parens-names)))))
  (ctune--reset-values))

;; Minor mode:

(defvar ctune-minor-mode-map
  (let ((map (make-sparse-keymap))
	binding)
    (when (setq binding (cdr (assq 'ctune-add-noise-macro ctune-kbd-alist)))
      (define-key map (kbd binding) #'ctune-add-noise-macro))
    (when (setq binding (cdr (assq 'ctune-save-noise-macros ctune-kbd-alist)))
      (define-key map (kbd binding) #'ctune-save-noise-macros))
    map)
  "Keymap for `ctune-mode'.
The keybindings can be customized by modifying the user option
`ctune-kbd-alist'.")

;;;###autoload
(define-minor-mode ctune-mode
  "Minor mode for easily managing CC Noise Macros, project-wide.
To add a CC Noise Macro, navigate to the identifier and type
\\[ctune-add-noise-macro].  If you want to
remove the identifier from the CC Noise Macro lists, just pass a prefix argument
to the `ctune-add-noise-macro' command.
For saving the changes, either customize the option
`ctune-save-noise-macros-automatically' to a value of your choice, or use the
command `ctune-save-noise-macros'.  This command will save the changed values
of `c-noise-macro-names' and `c-noise-macro-with-parens-names' to the
correspondent `dir-locals-file'."
  :lighter " ctune" :group 'ctune :keymap ctune-minor-mode-map
  (let ((supported (memq major-mode '(c-mode c++-mode objc-mode))))
    (cond ((and supported (buffer-file-name))
	   (if ctune-mode
	       (progn
		 ;; In order to work either when `ctune-mode' is activated from
		 ;; a CC Mode hook or being toggled on in an already loaded
		 ;; buffer,  we need the `dir-local-variables-alist' to be
		 ;; updated.
		 ;; If we don't do this, we can end up saving a stale value of
		 ;; CC Noise Macros in `ctune-prev-noise-macro-names' and
		 ;; `ctune-prev-noise-macro-with-parens-names'.
		 (hack-dir-local-variables)
		 ;; We need a copy of the lists, and not the actual list,
		 ;; so that changes are independent.
		 (setq-local ctune-prev-noise-macro-names
			     (copy-sequence
			      (cdr (assq 'c-noise-macro-names
					 dir-local-variables-alist))))
		 (setq-local ctune-prev-noise-macro-with-parens-names
			     (copy-sequence
			      (cdr (assq 'c-noise-macro-with-parens-names
					 dir-local-variables-alist))))
		 ;; If `c-noise-macro-names' or/and
		 ;; `c-noise-macro-with-parens-names' haven't been made local
		 ;; (e.g., there's no entry for them in `dir-locals-file'
		 ;; or in the file local variables), we make them local here.
		 (make-local-variable 'c-noise-macro-names)
		 (make-local-variable 'c-noise-macro-with-parens-names)
		 ;; Add the ctune function for saving Noise Macros.
		 (dolist (hook '(kill-buffer-hook kill-emacs-hook))
		   (add-hook hook #'ctune-save-noise-macros-maybe nil t)))
	     (dolist (hook '(kill-buffer-hook kill-emacs-hook))
	       (remove-hook hook #'ctune-save-noise-macros-maybe t))))
	  (t
	   (setq ctune-mode nil)
	   (unless supported
	     (error "Major mode %s not supported!" major-mode))))))

(provide 'ctune)

;;; Version Improvements:
;; This list of changes is not exhaustive, but should be good enough for
;; reference.
;;
;; Version 0.2:
;; - Automatic fontification after changing CC Noise Macros.
;; - Don't activate `ctune' in non-visiting file buffers.
;; 
;; Version 0.1:
;; - First released version of `ctune'.
;; - Added all the basic functionality.

;;; ctune.el ends here
