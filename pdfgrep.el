;;; pdfgrep.el --- run `pdfgrep' and display the results

;; Copyright (C) 2017-2018 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Created: October 2017
;; Keywords: extensions mail pdf grep
;; Homepage: https://github.com/jeremy-compostella/pdfgrep
;; Package-Version: 1.0
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
  :group 'pdfgrep)

(defcustom pdfgrep-context-length 100
  "Pdfgrep default context length, option `-C'."
  :group 'pdfgrep)

(defcustom pdfgrep-ignore-case t
  "Pdfgrep ignore case option."
  :group 'pdfgrep)

(defvar pdfgrep-history '()
  "History list for pdfgrep.")

(defvar pdfgrep-program (purecopy "pdfgrep")
  "The default pdfgrep program.")

(defun pdfgrep-default-command ()
  "Compute the default pdfgrep command for \\[pdfgrep]."
  (concat pdfgrep-program " -n "
	  (when pdfgrep-ignore-case
	    "-i ")
	  (when pdfgrep-context-length
	    (format "-C %d " pdfgrep-context-length))))

(define-derived-mode pdfgrep-mode grep-mode "PdfGrep"
  "`pdfgrep-mode' is an equivalent of `grep-mode' for PDF
document.  It is based on the pdfgrep program.")

(defun pdfgrep (command-args)
  "Run Pdfgrep with user-specified COMMAND-ARGS, collect output in a buffer.
You can use C-x ` (M-x next-error), or RET in the *pdfgrep*
buffer, to go to the lines where Pdfgrep found matches.  To kill
the Pdfgrep job before it finishes, type C-c C-k."
  (interactive (list (read-shell-command "Run pdfgrep (like this): "
					 (pdfgrep-default-command)
					 'pdfgrep-history)))
  (compilation-start command-args 'pdfgrep-mode))

(defun pdfgrep-current-page-and-match ()
  "Return the current match page number and match string."
  (with-current-buffer pdfgrep-buffer-name
    (cons (cadr (compilation--message->loc (compilation-next-error 0)))
	  (let* ((cur (buffer-substring (line-beginning-position)
					(line-end-position)))
		 (start (text-property-any 0 (length cur) 'font-lock-face
					   'match cur)))
	    (substring cur start (next-property-change start cur))))))

(defun pdfgrep-goto-locus (msg mk end-mk)
  "Jump to a match corresponding.
MSG, MK and END-MK arguments are ignored.  This function is used
to advice `compilation-goto-locus'."
  (when (and (eq major-mode 'doc-view-mode)
	     (eq doc-view-doc-type 'pdf))
    (doc-view-goto-page (car (pdfgrep-current-page-and-match))))
  (when (eq major-mode 'pdf-view-mode)
    (let ((meta (pdfgrep-current-page-and-match)))
      (pdf-view-goto-page (car meta))
      (when (cdr meta)
	(pdf-isearch-hl-matches nil (pdf-isearch-search-page (cdr meta)) t)))))

(advice-add 'compilation-goto-locus :after #'pdfgrep-goto-locus)

(provide 'pdfgrep)

;;; pdfgrep.el ends here
