;;; html2org.el --- Parse Html to org mode with shr.   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp

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

;;

;;; Code:

(require 'shr)
(require 'org)
(require 'gnus-art)

(defgroup html2org nil "Convert html to org.")

(defcustom html2org-shift-heading-level 0
  "Shift heading levels by a positive or negative integer.
For example, with shift-heading-level=-1,
level 2 headings become level 1 headings,
and level 3 headings become level 2 headings.
Headings cannot have a level less than 1,
so a heading that would be shifted below level 1 becomes a regular paragraph."
  :group 'html2org
  :type 'int)

(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.
Return the temporary output buffer which command is writing to
during execution.
When the command is finished, call CALLBACK with the resulting
output as a string.
Synopsis:(async-shell-command-to-string \"echo hello\" (lambda (s) (message \"RETURNED (%s)\" s)))"
  (let ((output-buffer (generate-new-buffer " *temp*"))
        (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun html2org-fetch-url (url)
  "Fetch URL."
  (interactive "sUrl: ")
  (async-shell-command-to-string
   (format "curl %s" url)
   (lambda (html) (html2org html))))

(defun html2org (&optional html)
  "Convert HTML buffer/string/file and return as org string.
Optional argument HTML:
1. If HTML is a valid file, will convert the HTML file to org string.
2. If HTML is a string, will convert the HTML string to org string."
  (interactive)
  (let* ((html (or html (buffer-string))))
    (with-temp-buffer
      (let ((shr-bullet "- ")
            (shr-table-vertical-line "|")
            (shr-width 100000)
            (shr-use-fonts nil)
            (shr-external-rendering-functions
             '((figure . shr-tag-figure)
               (dt . shr-tag-dt)
               (li . shr-tag-li)
               (p . html2org-tag-p)
               (a . html2org-tag-a)
               (h6 . html2org-tag-h6)
               (h5 . html2org-tag-h5)
               (h4 . html2org-tag-h4)
               (h3 . html2org-tag-h3)
               (h2 . html2org-tag-h2)
               (h1 . html2org-tag-h1)
               (svg . shr-tag-svg)
               (strong . shr-tag-strong)
               (u . shr-tag-u)
               (em . shr-tag-em)
               (pre . shr-tag-pre)
               (title . html2org-tag-title)
               (b . html2org-tag-b)
               (img . shr-tag-img)
               (ul . shr-tag-ul)
               (li . shr-tag-li))))
        (html2org-buffer html)))))

(defun html2org-buffer (html)
  "Convert HTML to org mode buffer."
  (pop-to-buffer "*html2org*")
  (with-current-buffer "*html2org*"
    (erase-buffer)
    (shr-insert-document
     (with-temp-buffer
       (insert html)
       (libxml-parse-html-region (point-min) (point-max))))
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (org-mode)))


(defun html2org-tag-title (dom)
  "Parse tag title.
Argument DOM dom."
  (insert "#+TITLE: ")
  (shr-generic dom))

(defun html2org-tag-headline (level dom)
  "Parse tag headline.
Argument LEVEL is headline level.
Argument DOM dom."
  (dotimes (_ (+ level html2org-shift-heading-level)) (insert "*"))
  (insert " ")
  (shr-generic dom)
  (shr-ensure-newline))

(defun html2org-tag-h1 (dom)
  "Parse tag h1.
Argument DOM dom."
  (html2org-tag-headline 1 dom))

(defun html2org-tag-h2 (dom)
  "Parse tag h2.
Argument DOM dom."
  (html2org-tag-headline 2 dom))

(defun html2org-tag-h3 (dom)
  "Parse tag h3.
Argument DOM dom."
  (html2org-tag-headline 3 dom))

(defun html2org-tag-h4 (dom)
  "Parse tag h4.
Argument DOM dom."
  (html2org-tag-headline 4 dom))

(defun html2org-tag-h5 (dom)
  "Parse tag h5.
Argument DOM dom."
  (html2org-tag-headline 5 dom))

(defun html2org-tag-h6 (dom)
  "Parse tag h6.
Argument DOM dom."
  (html2org-tag-headline 6 dom))

(defun html2org-validate-link-p (link)
  "Check LINK if validate."
  (string-match gnus-button-url-regexp link))

(defun html2org-tag-a (dom)
  "Parse tag a.
Argument DOM dom."
  (let ((link (dom-attr dom 'href))
        (text (dom-text dom)))
    (when (html2org-validate-link-p link)
      (insert "[")
      (insert (format "[%s]" link))
      (when (> (length text) 0)
        (insert (format "[%s]" text)))
      (insert "]"))))

(defun html2org-tag-b (dom)
  "Parse tag b.
Argument DOM dom."
  (insert (format "*%s*" (dom-text dom))))

(provide 'html2org)
;;; html2org.el ends here
