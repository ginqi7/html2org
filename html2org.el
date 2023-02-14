;;; html2org.el --- Convert HTML to org mode.        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;;; Installation:
;;
;; Manual:
;;
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/html2org/
;; ```
;; git clone git@github.com:ginqi7/html2org.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/html2org/")
;; (require 'html2org)
;; ```

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `html2org-fetch-url'
;;    Fetch URL.
;;    Keybinding: M-x html2org-fetch-url
;;  `html2org'
;;    Convert HTML buffer/string/file and return as org string.
;;    Keybinding: M-x html2org
;;  `html2org-paste-from-clipboard'
;;    In MacOs: 1. copy contents from browser.
;;    Keybinding: M-x html2org-paste-from-clipboard
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `html2org-shift-heading-level'
;;    Shift heading levels by a positive or negative integer.
;;    default = 0
;;  `html2org-retrieve-command'
;;    Command for fetch web url.
;;    default = "curl"

;;
;;; Code:


(require 'shr)
(require 'org)
(require 'gnus-art)
(require 'language-detection)
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

(defcustom html2org-retrieve-command "curl"
  "Command for fetch web url."
  :group 'html2org
  :type 'string)

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
   (format "%s %s" html2org-retrieve-command url)
   (lambda (html) (html2org html))))

(defun html2org (&optional html)
  "Convert HTML buffer/string/file and return as org string.
Optional argument HTML:
1. If HTML is a valid file, will convert the HTML file to org string.
2. If HTML is a string, will convert the HTML string to org string."
  (interactive)
  (let* ((html (or html (buffer-string))))
    (let ((shr-bullet "- ")
          (shr-table-vertical-line "|")
          (shr-stylesheet '((border-collapse . "separate")))
          (shr-width 100000)
          (shr-use-fonts nil)
          (shr-external-rendering-functions
           '((blockquote . html2org-tag-blockquote)
             (pre . html2org-tag-pre)
             (code . html2org-tag-code)
             (img . html2org-tag-img)
             (li . html2org-tag-li)
             (hr . html2org-tag-hr)
             (a . html2org-tag-a)
             (h6 . html2org-tag-h6)
             (h5 . html2org-tag-h5)
             (h4 . html2org-tag-h4)
             (h3 . html2org-tag-h3)
             (h2 . html2org-tag-h2)
             (h1 . html2org-tag-h1)
             (title . html2org-tag-title)
             (b . html2org-tag-b)
             (strong . html2org-tag-b))))
      (html2org-buffer html))))

(defun html2org-text (html)
  "Convert HTML to org mode text."
  (with-temp-buffer
    (shr-insert-document
     (with-temp-buffer
       (insert html)
       (libxml-parse-html-region (point-min) (point-max))))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun html2org-buffer (html)
  "Convert HTML to org mode buffer."
  (pop-to-buffer "*html2org*")
  (with-current-buffer "*html2org*"
    (erase-buffer)
    (insert (html2org-text html))
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
  (html2org-ensure-blank)
  (insert "*")
  (shr-generic dom)
  (insert "*")
  (insert " "))

(defun html2org-tag-hr (_dom)
  "Parse tag hr.
Argument DOM dom."
  (insert "-----"))


(defun html2org-tag-li (dom)
  "Parse tag li DOM."
  (shr-ensure-newline)
  (let ((start (point)))
    (let* ((bullet
            (if (numberp shr-list-mode)
                (prog1
                    (format "%d. " shr-list-mode)
                  (setq shr-list-mode (1+ shr-list-mode)))
              (car shr-internal-bullet)))
           (width
            (if (numberp shr-list-mode)
                (shr-string-pixel-width bullet)
              (cdr shr-internal-bullet))))
      (insert bullet)
      (shr-mark-fill start)
      (let ((shr-indentation (+ shr-indentation width)))
        (put-text-property start
                           (1+ start)
                           'shr-continuation-indentation shr-indentation)
        (put-text-property start
                           (1+ start)
                           'shr-prefix-length
                           (length bullet))
        (shr-generic dom))))
  (unless (bolp) (insert "\n")))


(defun html2org-tag-img (dom &optional _url)
  "Parse img DOM."
  (let ((url (dom-attr dom 'src)))
    (insert (format "[[%s]]" url))))

(defun html2org-tag-pre (dom)
  "Parse img pre DOM."
  (let ((code (dom-texts dom)))
    (shr-ensure-newline)
    (insert
     (format "#+begin_src %s" (language-detection-string code)))
    (shr-tag-pre dom)
    (insert "#+end_src")
    (shr-ensure-newline)))

(defun html2org-tag-code (dom)
  "Parse img code DOM."
  (shr-tag-code dom))

(defun html2org-tag-blockquote (dom)
  "Parse blockquote DOM."
  (shr-ensure-newline)
  (insert "#+begin_quote \n")
  (shr-generic dom)
  (insert "#+end_quote")
  (shr-ensure-newline))

(defun html2org-ensure-blank()
  "Ensure there are a blank or in line begin."
  (let ((before-string (string (preceding-char))))
    (unless (or (bolp) (string= " " before-string) (insert " "))
      )))

(defun html-from-clipboard ()
  "Get HTML from clipboard for macOS."
  (let* ((result
          (condition-case err
              ;; hex-encoded string:
              ;;           < m e t a ......>
              ;; «data HTML3C6D657461......3E»
              ;;(do-applescript "the clipboard as «class HTML»")
              (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"'")
            (error
             ;; assume it's user's fault
             (user-error "Can't get HTML data from the clipboard: %s"
                         (error-message-string err)))))
         (data (substring result 10 -1))
         (html
          (decode-coding-string
           (apply #'unibyte-string
                  (mapcar
                   (lambda (x) (string-to-number x 16))
                   (seq-partition data 2)))
           'utf-8)))
    html))

(defun html2org-paste-from-clipboard ()
  "In MacOs: 1. copy contents from browser.
2. convert html to org mode.
3. paste org contents."
  (interactive)
  (insert (html2org-text (html-from-clipboard))))

(provide 'html2org)
;;; html2org.el ends here
