;;; pdfgrep-tests.el --- Test for pdf-grep   -*- lexical-binding: t; -*-


;;; Commentary:

;;; Code:


(require 'ert)
(require 'pdfgrep)

(defun pth/pos-at-end (cmd)
  "Position at end of CMD. Counting begins at 1."
  (1+ (length cmd)))

(ert-deftest pdfgrep-tests/pdfgrep-default-command/ignore-case ()
  (let ((cmd "pdfgrep -H -n -i "))
    (should (equal (pdfgrep-default-command) cmd))))


(ert-deftest pdfgrep-tests/pdfgrep-default-command/no-ignore-case ()
  (let ((pdfgrep-ignore-case 'nil)
        (cmd "pdfgrep -H -n "))
    (should (equal (pdfgrep-default-command) cmd))))

(ert-deftest pdfgrep-tests/pdfgrep-default-command/no-ignore-case-pdf-view-mode ()
  (let* ((pdfgrep-ignore-case 'nil)
         (major-mode 'pdf-view-mode)
         (buffer-file-name "/some/path/to/test.pdf")
         (base-cmd "pdfgrep -H -n ")
         (cmd (concat base-cmd buffer-file-name)))
    (should (equal (pdfgrep-default-command) `(,cmd . ,(pth/pos-at-end cmd))))))

(ert-deftest pdfgrep-tests/pdfgrep-default-command/no-ignore-case-doc-view-mode ()
  (let* ((pdfgrep-ignore-case 'nil)
         (major-mode 'doc-view-mode)
         (buffer-file-name "/some/path/to/test.pdf")
         (base-cmd "pdfgrep -H -n ")
         (cmd (concat base-cmd buffer-file-name)))
    (should (equal (pdfgrep-default-command) `(,cmd . ,(pth/pos-at-end cmd))))))

(ert-deftest pdfgrep-tests/pdfgrep-default-command/ignore-case-ignore-errors ()
  (let* ((pdfgrep-ignore-errors t)
         (base-cmd "pdfgrep -H -n -i ")
         (ignore-errors " 2>/dev/null")
         (cmd (concat base-cmd ignore-errors)))
    (should (equal (pdfgrep-default-command) `(,cmd . ,(pth/pos-at-end base-cmd))))))


(ert-deftest pdfgrep-tests/pdfgrep-default-command/no-ignore-case-ignore-errors ()
  (let* ((pdfgrep-ignore-errors t)
         (pdfgrep-ignore-case 'nil)
         (base-cmd "pdfgrep -H -n ")
         (ignore-errors " 2>/dev/null")
         (cmd (concat base-cmd ignore-errors)))
    (should (equal (pdfgrep-default-command) `(,cmd . ,(pth/pos-at-end base-cmd))))))

(ert-deftest pdfgrep-tests/pdfgrep-default-command/no-ignore-case-pdf-view-mode ()
  (let* ((pdfgrep-ignore-errors t)
         (pdfgrep-ignore-case 'nil)
         (major-mode 'pdf-view-mode)
         (buffer-file-name "/some/path/to/test.pdf")
         (base-cmd "pdfgrep -H -n ")
         (ignore-errors " 2>/dev/null")
         (cmd (concat base-cmd buffer-file-name ignore-errors)))
    (should (equal (pdfgrep-default-command) `(,cmd . ,(pth/pos-at-end cmd))))))

(ert-deftest pdfgrep-tests/pdfgrep-default-command/no-ignore-case-doc-view-mode ()
  (let* ((pdfgrep-ignore-errors t)
         (pdfgrep-ignore-case 'nil)
         (major-mode 'doc-view-mode)
         (buffer-file-name "/some/path/to/test.pdf")
         (base-cmd "pdfgrep -H -n ")
         (ignore-errors " 2>/dev/null")
         (cmd (concat base-cmd buffer-file-name ignore-errors)))
    (should (equal (pdfgrep-default-command) `(,cmd . ,(pth/pos-at-end cmd))))))

(provide 'pdfgrep-tests)

;;; pdfgrep-tests.el ends here
