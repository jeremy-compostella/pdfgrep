;;; pdfgrep.el --- run `pdfgrep' and display the results. -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: October 2017
;; Keywords: extensions mail pdf grep
;; Homepage: https://github.com/jeremy-compostella/pdfgrep
;; Package-Version: 1.4
;; Package-Requires: ((emacs "24.4"))

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

;;; Commentary:

;; This package provides the Emacs "grep" facilities for the pdfgrep
;; program.

;;; Code:

(require 'grep)

(defgroup pdfgrep nil
  "Run `pdfgrep' and display the results."
  :group 'tools
  :group 'processes)

(defcustom pdfgrep-buffer-name "*pdfgrep*"
  "Pdfgrep search buffer."
  :type '(string))

(defcustom pdfgrep-ignore-case t
  "PDFGrep ignore case option."
  :type '(boolean))

(defcustom pdfgrep-options " -H -n "
  "Default options appended to `pdfgrep-program' when invoking the command.
Not including `pdfgrep-ignore-case'."
  :type '(string))

(defcustom pdfgrep-ignore-errors nil
  "Redirect pdfgrep command errors to /dev/null."
  :type '(boolean))

(defvar pdfgrep-history '()
  "Command History list for PDFGrep.")

(defvar pdfgrep-program "pdfgrep"
  "The default pdfgrep program.")

(defun pdfgrep-default-command ()
  "Compute the default pdfgrep command for `pdfgrep'."
  (let ((cmd (concat pdfgrep-program pdfgrep-options
		     (when pdfgrep-ignore-case
		       "-i "))))
    (if pdfgrep-ignore-errors
	(cons (concat cmd " 2>/dev/null") (1+ (length cmd)))
      cmd)))

(defun pdfgrep-current-page-and-match ()
  "Return the current match page number and match string."
  (with-current-buffer pdfgrep-buffer-name
    (cons (cadr (compilation--message->loc (compilation-next-error 0)))
	  (let* ((cur (buffer-substring (line-beginning-position)
					(line-end-position)))
		 (start (text-property-any 0 (length cur) 'font-lock-face
					   'match cur)))
	    (substring cur start (next-property-change start cur))))))

(defvar doc-view-doc-type)
(declare-function doc-view-goto-page "doc-view")

(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-isearch-hl-matches "ext:pdf-isearch")
(declare-function pdf-isearch-search-page "ext:pdf-isearch")

(defun pdfgrep-goto-locus (_msg _mk _end-mk)
  "Jump to a match corresponding.
_MSG, _MK and _END-MK parameters are ignored.  This function is
used to advice `compilation-goto-locus'."
  (when (and (eq major-mode 'doc-view-mode)
	     (eq doc-view-doc-type 'pdf))
    (doc-view-goto-page (car (pdfgrep-current-page-and-match))))
  (when (eq major-mode 'pdf-view-mode)
    (let ((meta (pdfgrep-current-page-and-match)))
      (pdf-view-goto-page (car meta))
      (when (cdr meta)
	(pdf-isearch-hl-matches nil (pdf-isearch-search-page (cdr meta)) t)))))

;;;###autoload
(define-minor-mode pdfgrep-mode
  "Toggle PDFGrep mode.

With a prefix argument ARG, enable PDFGrep mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  (if pdfgrep-mode
      (advice-add 'compilation-goto-locus :after #'pdfgrep-goto-locus)
    (advice-remove 'compilation-goto-locus #'pdfgrep-goto-locus)))

(defun pdfgrep (command-args)
  "Run pdfgrep with user-specified COMMAND-ARGS, collect output in a buffer.
You can use \\[next-error], or RET in the `pdfgrep-buffer-name'
buffer, to go to the lines where PDFGrep found matches.  To kill
the PDFGrep job before it finishes, type \\[kill-compilation]."
  (interactive (list (read-shell-command "Run pdfgrep (like this): "
					 (pdfgrep-default-command)
					 'pdfgrep-history)))
  (unless pdfgrep-mode
    (error "PDFGrep is not enabled, run `pdfgrep-mode' first."))
  (unless (executable-find "pdfgrep")
    (error "The 'pdfgrep' command not available on your system."))
  (compilation-start command-args 'grep-mode
		     (lambda (_x) pdfgrep-buffer-name)))

(provide 'pdfgrep)

;;; pdfgrep.el ends here
