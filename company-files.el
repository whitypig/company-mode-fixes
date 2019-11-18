;;; company-files.el --- company-mode completion backend for file names

;; Copyright (C) 2009-2011, 2014-2015  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-files nil
  "Completion backend for file names."
  :group 'company)

(defcustom company-files-exclusions nil
  "File name extensions and directory names to ignore.
The values should use the same format as `completion-ignored-extensions'."
  :type '(const string)
  :package-version '(company . "0.9.1"))

(defcustom company-files-directory-annotation "[d]"
  ""
  :type '(string)
  :group 'company-files)

(defcustom company-files-file-annotation "[f]"
  ""
  :type '(string)
  :group 'company-files)

(defcustom company-files-recursive-candidates nil
  "When nil, completion is performed using candidates that are
obtained in a similar way as when you do
\"find . -maxdepth 1 -name 'prefix*' \"."
  :type '(boolean)
  :group 'company-files)

(defcustom company-files-use-flat-filename-completion t
  "When non-nil, completion is done for a prefix that do _not_ contain any slash.

Consider the following directory tree:

./
|-- foo1
|-- foo2
`-- foo3

When this variable is set to t and you type \"fo\", then \"foo1\",
\"foo2\" and \"foo3\" are shown up as candidates.  Sometimes this can
be annoying and in that case, set this variable to nil to inhibit this
behavior."
  :type '(boolean)
  :group 'company-files)

(defcustom company-files-trigger-post-completion t
  "When non-nil, triggers completion using `company-files' after you
choose a candidate.  This is especially useful when
`company-files-recursive-candidates' is nil."
  :type '(boolean)
  :group 'company-files)

(defun company-files--directory-files (dir prefix)
  ;; Don't use directory-files. It produces directories without trailing /.
  (condition-case err
      (let ((comp (sort (file-name-all-completions prefix dir)
                        (lambda (s1 s2) (string-lessp (downcase s1) (downcase s2))))))
        (when company-files-exclusions
          (setq comp (company-files--exclusions-filtered comp)))
        (if (equal prefix "")
            (delete "../" (delete "./" comp))
          comp))
    (file-error nil)))

(defun company-files--exclusions-filtered (completions)
  (let* ((dir-exclusions (cl-delete-if-not #'company-files--trailing-slash-p
                                           company-files-exclusions))
         (file-exclusions (cl-set-difference company-files-exclusions
                                             dir-exclusions)))
    (cl-loop for c in completions
             unless (if (company-files--trailing-slash-p c)
                        (member c dir-exclusions)
                      (cl-find-if (lambda (exclusion)
                                    (string-suffix-p exclusion c))
                                  file-exclusions))
             collect c)))

(defconst company-files--invalid-characters "$?<>/\"*\t\r\n|;:"
  "")

(defvar company-files--invalid-filename-characters-regexp
  (format "[%s]" company-files--invalid-characters)
  "")

(defvar company-files--valid-filename-regexp
  (format "[^%s]+" company-files--invalid-characters)
  "")

(defvar company-files--regexps
  (let* ((root (if (eq system-type 'windows-nt)
                   "[a-zA-Z]:/"
                 "/"))
         (regs (list "\\.\\{1,2\\}/"
                     "~/"
                     (format "%s/" company-files--valid-filename-regexp)
                     root))
         (begin (concat "\\(?:"
                        (mapconcat #'identity regs "\\|")
                        "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
          (concat "'\\(" begin "[^'\n]*\\)")
          (concat "\\(?:[ \t=]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))

(defun company-files--get-prefix ()
  (or (company-files--grab-existing-name)
      (and company-files-use-flat-filename-completion
           (company-files--grab-flat-name))))

(defun company-files--grab-existing-name ()
  ;; Grab the file name.
  ;; When surrounded with quotes, it can include spaces.
  (let (file dir)
    (and (cl-dolist (regexp company-files--regexps)
           (when (setq file (company-grab-line regexp 1))
             (cl-return file)))
         (company-files--connected-p file)
         (setq dir (file-name-directory file))
         (not (string-match "//" dir))
         (file-exists-p dir)
         file)))

(defun company-files--grab-flat-name ()
  ""
  (let* ((end (point))
         (beg (or
               (save-excursion
                 ;; Todo: What to do when filename contains spaces and is
                 ;; quoted with " or ' or \."
                 (and
                  (re-search-backward
                   (format "[ ]\\|%s"
                           company-files--invalid-filename-characters-regexp)
                   (line-beginning-position) t)
                  (1+ (point))))
               (line-beginning-position)))
         (prefix (buffer-substring-no-properties beg end)))
    (when (not (string-match-p company-files--invalid-filename-characters-regexp prefix))
      prefix)))

(defun company-files--connected-p (file)
  (or (not (file-remote-p file))
      (file-remote-p file nil t)))

(defun company-files--trailing-slash-p (file)
  ;; `file-directory-p' is very expensive on remotes. We are relying on
  ;; `file-name-all-completions' returning directories with trailing / instead.
  (let ((len (length file)))
    (and (> len 0) (eq (aref file (1- len)) ?/))))

(defvar company-files--completion-cache nil)

(cl-defun company-files--collect-candidates (prefix &key (recursive t))
  ;; (message "DEBUG: company-files--collect-candidates, prefix=%s" prefix)
  (cond
   ((not (string-match-p company-files--invalid-filename-characters-regexp prefix))
    (company-files--directory-files "." prefix))
   (t
    (company-files--complete prefix :recursive recursive))))

(cl-defun company-files--complete (prefix &key (recursive t))
  (let* ((dir (file-name-directory prefix))
         (file (file-name-nondirectory prefix))
         (key (list file
                    (expand-file-name dir)
                    (nth 5 (file-attributes dir))))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (unless (company-file--keys-match-p key (car company-files--completion-cache))
      (let* ((candidates (mapcar (lambda (f) (concat dir f))
                                 (company-files--directory-files dir file)))
             (directories (and recursive
                               (unless (file-remote-p dir)
                                 (cl-remove-if-not
                                  (lambda (f)
                                    (and (company-files--trailing-slash-p f)
                                         (not (file-remote-p f))
                                         (company-files--connected-p f)))
                                  candidates))))
             (children (and recursive
                            directories
                            (cl-mapcan
                             (lambda (d)
                               (mapcar (lambda (c) (concat d c))
                                       (company-files--directory-files d "")))
                             directories))))
        (setq company-files--completion-cache
              (cons key (append candidates children)))))
    (all-completions prefix
                     (cdr company-files--completion-cache))))

(defun company-file--keys-match-p (new old)
  (and (equal (cdr old) (cdr new))
       (string-prefix-p (car old) (car new))))

(defun company-files--annotation (candidate)
  (let ((fmt "  %s"))
    (cond
     ((file-directory-p candidate)
      (format fmt company-files-directory-annotation))
     (t
      (format fmt company-files-file-annotation)))))

(defun company-files--post-completion (candidate)
  (cond
   ((and company-files-trigger-post-completion
         (company-files--trailing-slash-p candidate))
    (ignore-errors
      (company-begin-backend 'company-files
                             (lambda (arg)
                               ;; arg is a selected candidate.
                               (and (company-files--trailing-slash-p arg)
                                    (company-begin-backend 'company-files))))))
   ((company-files--trailing-slash-p candidate)
    (delete-char -1))))

;;;###autoload
(defun company-files (command &optional arg &rest ignored)
  "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-files))
    (prefix (company-files--get-prefix))
    (candidates
     (company-files--collect-candidates
      arg :recursive company-files-recursive-candidates))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg)))
                    1))
    (post-completion (company-files--post-completion arg))
    (sorted t)
    (annotation (company-files--annotation arg))
    (no-cache t)))

(provide 'company-files)
;;; company-files.el ends here
