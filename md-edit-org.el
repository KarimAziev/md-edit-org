;;; md-edit-org.el --- Edit markdown in org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/md-edit-org
;; Version: 0.1.0
;; Keywords: outlines convenience docs
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Edit markdown in org mode.

;;; Keymaps

;; `md-edit-org-edit-mode-map'
;;      Keymap used in `md-edit-org-edit-buffer'.

;;; Commands

;; M-x `md-edit-org-edit-markdown-in-org'
;;      Edit current markdown buffer in `org-mode'.
;;      A new buffer is created with original content converted to org,
;;      and the buffer is switched into an `org-mode'.
;;      When done, exit with \<md-edit-org-edit-mode-map>`\[md-edit-org-done]'.
;;      The edited text will then converted to markdown and replace the area
;;      in the original markdown buffer.
;;      To exit without changes - \<md-edit-org-edit-mode-map>\[md-edit-org-discard].

;; M-x `md-edit-org-discard'
;;      Kill `md-edit-org-edit-buffer'.
;;      This command should be used in `md-edit-org-edit-buffer'.

;; M-x `md-edit-org-done'
;;      Replace content in markdown buffer with edited and exit.

;; M-x `md-edit-org-save'
;;      Replace content in markdown buffer with edited in org and converted back.

;;; Customization

;; `md-edit-org-persistent-message'
;;      Non-nil means show persistent exit help message while editing src examples.
;;      The message is shown in the header-line, which will be created in the
;;      first line of the window showing the editing buffer.

;; `md-edit-org-pandoc-executable'
;;      The path to the pandoc executable.

;; `md-edit-org-pandoc-output-type'
;;      Markdown output type for `pandoc'.

;;; Functions

;; `md-edit-org-kill-org-buffer'
;;      Kill buffer `md-edit-org-edit-buffer' if it is live.
;;      Should be called inside `md-edit-org-orig-buffer'.

;; `md-edit-org-pandoc-from-string' (string input-type output-type &rest options)
;;      Execute `pandoc' on STRING in INPUT-TYPE to OUTPUT-TYPE additional OPTIONS.

;;; Variables

;; `md-edit-org-edit-buffer'
;;      Editable buffer for `md-edit-org-orig-buffer' in `org-mode'.

;; `md-edit-org-orig-buffer'
;;      Markdown buffer.

;;; Code:

(require 'subr-x)
(require 'edit-indirect)

(defcustom md-edit-org-pandoc-output-type "gfm"
  "Markdown output type for `pandoc'."
  :type '(choice :tag "Pandoc output type"
                 (string :tag "GitHub-Flavored Markdown" "gfm")
                 (string :tag "Pandoc’s Markdown" "markdown")
                 (string :tag "MultiMarkdown" "markdown_mmd")
                 (string :tag "PHP Markdown Extra" "markdown_phpextra")
                 (string :tag "original unextended Markdown" "markdown_strict")
                 (string :tag "Other"))
  :group 'org-md)

(defcustom md-edit-org-pandoc-executable (executable-find "pandoc")
  "The path to the pandoc executable."
  :type 'string
  :group 'org-md)

(defun md-edit-org-pandoc-from-string (string input-type output-type &rest options)
  "Execute `pandoc' on STRING in INPUT-TYPE to OUTPUT-TYPE additional OPTIONS."
  (setq options (delete nil (flatten-list options)))
  (let ((args (append
               (list md-edit-org-pandoc-executable t t nil)
               (list "-f" input-type "-t" output-type) options)))
    (with-temp-buffer
      (insert string)
      (if
          (eq 0 (apply #'call-process-region (append (list (point-min)
                                                           (point-max))
                                                     args)))
          (buffer-string)
        (minibuffer-message "Couldn't convert markdown to org: %s"
                            (buffer-string))
        nil))))

(defun md-edit-org-git-commit-setup ()
  "Add `md-edit-org-git-commit' to `git-commit-mode-hook'."
  (add-hook 'git-commit-mode-hook 'md-edit-org-git-commit))

(defun md-edit-org-cleanup (&optional _beg _end)
  "Remove hooks from parent buffer."
  (remove-hook 'edit-indirect-after-creation-hook
               'md-edit-org-after-creation t)
  (remove-hook 'edit-indirect-before-commit-functions
               'md-edit-org-cleanup t))

(defun md-edit-org-indirect-org-to-md ()
  "Transofrm org content of current buffer to markdown."
  (let* ((content (buffer-substring-no-properties (point-min)
                                                  (point-max)))
         (rep (md-edit-org-pandoc-from-string content
                                              "org"
                                              md-edit-org-pandoc-output-type)))
    (replace-region-contents (point-min)
                             (point-max)
                             (lambda ()
                               rep))
    (md-edit-org-fix-md-links)))

(defun md-edit-org-fix-md-links ()
  "Transorm inline links with images in org buffer to markdown."
  (save-excursion
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward "\\\\[!]\\(\\\\\\)?" nil t 1)
        (replace-match "!")))
    (save-excursion
      (while (re-search-forward "[<]\\(https?:[^>]+\\)[>]" nil t 1)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (rep))
          (setq rep (buffer-substring-no-properties (1+ beg)
                                                    (1- end)))
          (replace-region-contents beg end (lambda ()rep)))))
    (save-excursion
      (while (re-search-forward "\\(>\\)?\\\\]" nil t 1)
        (replace-match "]")))
    (save-excursion
      (while (re-search-forward "\\\\\\[" nil t 1)
        (replace-match "[" nil nil nil 0)))
    (save-excursion
      (while
          (re-search-forward
           "\\[\\!\\[\\[\\(https?://[^]]+\\)\\]\\[\\([^]]+\\)\\]+"
           nil t 1)
        (let ((link (match-string-no-properties 1))
              (text (match-string-no-properties 2)))
          (replace-match (format "[![%s](%s)]" text link)))))))

(defun md-edit-org-after-creation ()
  "Convert md content to org and add hook to transform it back before commit."
  (require 'org)
  (let* ((content (buffer-substring-no-properties (point-min)
                                                  (point-max)))
         (rep (md-edit-org-pandoc-from-string
               (replace-regexp-in-string
                "\\(!\\)\\[" "\\\\!["
                content)
               md-edit-org-pandoc-output-type
               "org")))
    (replace-region-contents (point-min)
                             (point-max)
                             (lambda () rep))
    (org-mode)
    (add-hook 'edit-indirect-before-commit-hook
              'md-edit-org-indirect-org-to-md
              nil t)))

(defun md-edit-org-mark-region (start end)
  "Mark region between START and END."
  (goto-char start)
  (push-mark start nil t)
  (goto-char end))

;;;###autoload
(defun md-edit-org ()
  "Edit current markdown buffer in `org-mode'.

A new buffer is created with original content converted to org,
and the buffer is switched into an `org-mode'."
  (interactive)
  (add-hook 'edit-indirect-before-commit-functions 'md-edit-org-cleanup nil t)
  (add-hook 'edit-indirect-after-creation-hook 'md-edit-org-after-creation nil t)
  (if
      (and (use-region-p)
           (region-active-p))
      (edit-indirect-region
       (region-beginning)
       (region-end)
       'display-buffer)
    (edit-indirect-region
     (point-min)
     (point-max)
     'display-buffer)))

(provide 'md-edit-org)
;;; md-edit-org.el ends here