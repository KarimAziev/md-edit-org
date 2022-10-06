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
;;      Replace content in `md-edit-org-orig-buffer' with converted `md-edit-org-edit-buffer'.

;; M-x `md-edit-org-save'
;;      Replace content in `md-edit-org-orig-buffer' with converted `md-edit-org-edit-buffer'.

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

(defcustom md-edit-org-persistent-message t
  "Non-nil means show persistent exit help message while editing src examples.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'org-md
  :type 'boolean)

(defvar-local md-edit-org-orig-buffer nil
  "Markdown buffer.")

(defvar-local md-edit-org-edit-buffer nil
  "Editable buffer for `md-edit-org-orig-buffer' in `org-mode'.")

(defvar md-edit-org-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'md-edit-org-done)
    (define-key map (kbd "C-c '") #'md-edit-org-done)
    (define-key map (kbd "C-c C-k") #'md-edit-org-discard)
    (define-key map [remap save-buffer] #'md-edit-org-save)
    map)
  "Keymap used in `md-edit-org-edit-buffer'.")

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

(defun md-edit-org-kill-org-buffer ()
  "Kill buffer `md-edit-org-edit-buffer' if it is live.
Should be called inside `md-edit-org-orig-buffer'."
  (when (and md-edit-org-edit-buffer
             (bufferp md-edit-org-edit-buffer)
             (buffer-live-p md-edit-org-edit-buffer))
    (kill-buffer md-edit-org-edit-buffer))
  (setq md-edit-org-edit-buffer nil))

;;;###autoload
(defun md-edit-org-save ()
  "Replace content in `md-edit-org-orig-buffer' with converted `md-edit-org-edit-buffer'."
  (interactive)
  (let* ((current-buff (current-buffer))
         (org-content (with-current-buffer current-buff
                        (buffer-substring-no-properties (point-min)
                                                        (point-max))))
         (buff (buffer-local-value 'md-edit-org-orig-buffer current-buff))
         (converted-content (md-edit-org-pandoc-from-string
                             org-content
                             "org"
                             md-edit-org-pandoc-output-type)))
    (when converted-content
      (with-current-buffer buff
        (replace-region-contents (point-min)
                                 (point-max)
                                 (lambda ()
                                   converted-content))))))

;;;###autoload
(defun md-edit-org-done ()
  "Replace content in `md-edit-org-orig-buffer' with converted `md-edit-org-edit-buffer'."
  (interactive)
  (let ((buff (current-buffer))
        (orig-buff))
    (setq orig-buff (buffer-local-value 'md-edit-org-orig-buffer buff))
    (md-edit-org-save)
    (kill-buffer buff)
    (when orig-buff
      (pop-to-buffer-same-window orig-buff))))

;;;###autoload
(defun md-edit-org-discard ()
  "Kill `md-edit-org-edit-buffer'.
This command should be used in `md-edit-org-edit-buffer'."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-current-buffer))

;;;###autoload
(defun md-edit-org-edit-markdown-in-org ()
  "Edit current markdown buffer in `org-mode'.

A new buffer is created with original content converted to org,
and the buffer is switched into an `org-mode'.

When done, exit with \\<md-edit-org-edit-mode-map>`\\[md-edit-org-done]'.
The edited text will then converted to markdown and replace the area
in the original markdown buffer.

To exit without changes - \\<md-edit-org-edit-mode-map>\\[md-edit-org-discard]."
  (interactive)
  (let* ((md-buffer (current-buffer))
         (org-buffer-name (concat
                           "md-edit-org-edit-"
                           (buffer-name
                            md-buffer))))
    (with-current-buffer md-buffer
      (add-hook 'kill-buffer-hook 'md-edit-org-kill-org-buffer nil t)
      (setq md-edit-org-edit-buffer (get-buffer-create org-buffer-name))
      (setq md-edit-org-orig-buffer md-buffer)
      (let* ((md-content (buffer-substring-no-properties (point-min)
                                                         (point-max)))
             (pos (point))
             (org-content
              (if
                  (string-empty-p (string-trim md-content))
                  md-content
                (md-edit-org-pandoc-from-string md-content
                                           md-edit-org-pandoc-output-type
                                           "org"))))
        (with-current-buffer md-edit-org-edit-buffer
          (erase-buffer)
          (insert org-content)
          (org-mode)
          (use-local-map
           (let ((map (copy-keymap
                       md-edit-org-edit-mode-map)))
             (set-keymap-parent map (current-local-map))
             map))
          (setq buffer-read-only nil)
          (set-buffer-modified-p nil)
          (setq buffer-undo-list nil)
          (when md-edit-org-persistent-message
            (setq header-line-format
	                (substitute-command-keys
	                 "Edit, then exit with `\\[md-edit-org-done]' or abort with \
`\\[md-edit-org-discard]'")))
          (setq header-line-format (or (format "Editing %s" md-buffer)))
          (setq md-edit-org-orig-buffer md-buffer)
          (when (> (point-max) pos)
            (goto-char pos)))
        (pop-to-buffer-same-window md-edit-org-edit-buffer t)))))

(provide 'md-edit-org)
;;; md-edit-org.el ends here