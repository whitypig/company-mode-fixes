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
                        #'company-files--candidates-sorter)))
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

(defvar company-files--completion-data nil)

(defun company-files--capf-prefix ()
  ;; (message "DEBUG: --capf-prefix called")
  (pcase (company-files--completion-at-point)
    (`(,prefix ,collection ,predicate)
     (car (setq company-files--capf-completion-data
                (list prefix collection predicate))))
    (_ nil)))

(defun company-files--shell-mode-p ()
  (memq major-mode company-files--shell-modes))

(defun company-files--get-prefix ()
  (cond
   ((company-files--shell-mode-p)
    (company-files--capf-prefix))
   ((company-files--should-complete-p)
    (or (company-files--grab-existing-name)
        (and company-files-use-flat-filename-completion
             (company-files--grab-flat-name))))
   (t nil)))

(defcustom company-files-complete-always nil
  "Non-nil means filename completion is always triggered."
  :type '(boolean)
  :group 'company-files)

(defcustom company-files-trigger-major-modes '()
  "A list of major-modes in which filename completion is provided.

Add major-mode in which you want to get filename completion. Note that
when current cursor position is in a string literal or comment,
filename completion gets activated regardless of this value.

Also note that `company-files-complete-always' overrides this
value. This means that if you set `company-files-complete-always' to
non-nil, filename completion is offered even if
`company-files-trigger-major-modes' is empty."
  :type '(list)
  :group 'company-files)

(defconst company-files--shell-modes '(eshell-mode shell-mode term-mode)
  "")

(defun company-files--should-complete-p ()
  "Return non-nil if filename completion should be provided."
  (or
   ;; A user prefer to always get completion.
   company-files-complete-always
   ;; In most cases, we need filename completion when we are in a
   ;; string or comment.
   (company-in-string-or-comment)
   ;; Other special cases
   (memq major-mode company-files-trigger-major-modes)))

(defun company-files--grab-line (regexp &optional num)
  (if (save-excursion
        (re-search-backward regexp (line-beginning-position) t))
      (match-string-no-properties (or num 0))
    ""))

(defun company-files--grab-existing-name ()
  ;; Grab the file name.
  ;; When surrounded with quotes, it can include spaces.
  ;; TODO: Looks buggy, so needs to be fixed or rewritten.
  (cl-loop for regexp in company-files--regexps
           for file = (company-files--grab-line regexp 1)
           for dir = (and file
                          (company-files--connected-p file)
                          (file-name-directory file))
           when (and dir
                     (not (string-match "//" dir))
                     (file-exists-p dir))
           return file))

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
    (when (and
           (stringp prefix)
           (> (length prefix) 0)
           (not (string-match-p company-files--invalid-filename-characters-regexp
                                prefix)))
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
   ((not (string-match-p company-files--invalid-filename-characters-regexp
                         prefix))
    ;; PREFIX can be considered as a simple filename, so return files in
    ;; current directory.
    (company-files--directory-files "." prefix))
   (t
    (company-files--complete prefix :recursive recursive))))

(require 'comint)

(defun company-files--completion-at-point ()
  "Copied from and modified `completion-at-point' in
  \"minibuffer.el\"."
  (let* ((completion-at-point-functions (list 'comint-filename-completion t))
         (res (save-excursion
               (run-hook-wrapped 'completion-at-point-functions
                                 #'completion--capf-wrapper 'optimist))))
    (pcase res
      ;; We ignore this case.
      (`(,_ . ,(and (pred functionp) f)) nil)
      ;; This case is what we want. Return (prefix collection).
      (`(,hookfun . (,start ,end ,collection . ,plist))
       ;; (prefix collection predicate)
       (list (pcase (buffer-substring-no-properties start end)
               ((pred (string-match "\\`[']\\(.*\\)\\'")) (match-string 1))
               (pre pre))
             collection (plist-get plist :predicat)))
      ;; Fallback case
      (_ nil))))

(defun company-files--candidates-sorter (s1 s2)
  (string< (downcase s1) (downcase s2)))

(defun company-files--capf-candidates (prefix)
  ;; (message "DEBUG: --capf-candidates called, prefix=%s" prefix)
  (pcase company-files--capf-completion-data
    (`(,(and pre (guard (string= pre prefix))) ,collection ,predicate)
     ;; (cl-loop for elt in (sort (all-completions pre collection predicate)
     ;;                           (lambda (x y) (string< (downcase x) (downcase y))))
     ;;          do (message "DEBUG: prefix=|%s|, elt=|%s|" prefix elt))
     (cl-loop with len = (length prefix)
              for elt in (sort (all-completions pre collection predicate)
                               #'company-files--candidates-sorter)
              when (not (string-match-p "\\`[.]\\{1,2\\}/\\'" elt))
              ;; Ignore "./" and "../"
              collect
              (pcase elt
                ((guard (string-suffix-p "/" prefix)) (concat prefix elt))
                ((and (let pos (cl-position ?/ prefix :from-end t))
                      (guard pos))
                 ;; prefix="/some/dire/fi", elt="file"
                 ;; then candidate = "le"
                 (concat prefix (substring elt (- len pos 1))))
                ;; TODO: What should we return from default case?
                (_ (concat prefix elt)))))
    (_ nil)))

(cl-defun company-files--complete (prefix &key (recursive t))
  ;; (message "DEBUG: --complete called, prefix=%s" prefix)
  (cond
   ((memq major-mode company-files--shell-modes)
    ;; collect candidates using company-capf.el
    (company-files--capf-candidates prefix))
   (t
    (company-files--complete-1 prefix :recursive recursive))))

(cl-defun company-files--complete-1 (prefix &key (recursive t))
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
