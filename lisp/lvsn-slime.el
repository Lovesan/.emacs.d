;;;; lvsn-slime.el --- SLIME utils -*- lexical-binding: t -*-

;; Author: Dmitry Ignatiev
;; URL: https://github.com/Lovesan/.emacs.d
;; Created: 20 March 2024

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(defvar lvsn-lisp-program "sbcl"
  "Lisp implementation to connect ")

(use-package lvsn-commons
             :demand t)

;;; Structoral editing
(use-package lispy
             :ensure t
             :demand t
             :hook
             (emacs-lisp-mode . lispy-mode)
             (lisp-mode . lispy-mode)
             (lisp-interaction-mode . lispy-mode)
             :config
             (setq lispy-colon-p nil))

;;; Superior Lisp Interaction Mode for Emacs
(use-package slime
             :demand t
             :ensure t
             :config
             (setq inferior-lisp-program lvsn-lisp-program)
             (setq slime-words-of-encouragement
                   (list (concat "These are your father's parentheses. "
                                 "Elegant weapons. For a more... civilized age.")))
             (slime-setup '(slime-repl
                            slime-fuzzy
                            slime-fancy-inspector
                            slime-fancy-trace
                            slime-indentation
                            slime-mdot-fu
                            slime-asdf
                            slime-sbcl-exts
                            slime-quicklisp
                            slime-scratch))
             (setq slime-load-failed-fasl 'always)
             (setq slime-net-coding-system 'utf-8-unix)
             (setq lisp-indent-function 'common-lisp-indent-function)
             (setq slime-description-autofocus t))

;;; auto-complete integration for SLIME
(use-package ac-slime
             :demand t
             :ensure t
             :config
             (add-to-list 'ac-modes 'slime-mode 'slime-repl-mode)
             :hook
             (slime-mode . set-up-slime-ac)
             (slime-repl-mode . set-up-slime-ac))

(define-common-lisp-style "lvsn-indent-style"
  "My custom indent style."
  (:inherit "modern")
  (:variables
   (lisp-loop-indent-subclauses t))
  (:indentation
   (if (4 2 2))
   (define (&lambda 2))
   (with-gensyms ((&whole 4 &rest 1) &body))
   (once-only (as with-gensyms))
   (dolist* (as dolist))
   (defglobal (as defvar))
   (switch (as case))
   (defhelper (as defun))
   (uiop:define-package (as defpackage))
   (define-package (as defpackage))
   (definline (as defun))
   (test (4 2))
   (pprint-dotnet-object (as print-unreadable-object))
   (exception-case (as handler-case))
   (exception-bind (as handler-bind))
   (define-dotnet-object-printer (as defmethod))
   (when-let (as let))
   (when-let* (as let*))
   (define-global-var (as defvar))
   (define-global-var* (as defvar))
   (define-global-parameter (as defvar))
   (define-global-parameter* (as defvar))
   (define-dotnet-callable-class (4 4 &rest (&whole 2 &rest 1)))
   (defwinfun (as defcfun))
   (definline (as defun))
   (defhandle (as defstruct))
   (with-ientry (as with-slots))))

(setq common-lisp-style-default "lvsn-indent-style")

(defvar lvsn-scratch-file "scratch.lisp")

(defun lvsn--open-scratch-file ()
  (find-file (concat lvsn-default-directory "/" lvsn-scratch-file)))

(defvar lvsn--slime-was-open nil)

(defun lvsn--find-repl-buffer ()
  (cl-loop for buf in (buffer-list)
           for name = (buffer-name buf)
           when (string-prefix-p "*slime-repl " name)
           return buf))

(defun lvsn--init-slime-windows (arg)
  "Opens a scratch file in a new pane"
  (unless lvsn--slime-was-open
    (let ((buf (lvsn--find-repl-buffer)))
      (when buf
        (let ((wnd (get-buffer-window buf)))
          (when wnd
            (setq lvsn--slime-was-open t)
            (select-window wnd)
            (delete-other-windows)
            (split-window-horizontally)
            (lvsn--open-scratch-file)
            (neotree)
            (redisplay t)))))))

(defun lvsn-slime-edit-definition ()
  (interactive)
  (mouse-set-point last-input-event)
  (redisplay t)
  (call-interactively 'slime-edit-definition))

(defun lvsn-slime-compile-and-load-file ()
  (interactive)
  (save-buffer)
  (slime-compile-and-load-file))

(defun lvsn-revert-to-repl ()
  (interactive)
  (let ((slime-buf (lvsn--find-repl-buffer)))
    (if (eq (current-buffer) slime-buf)
        (mode-line-other-buffer)
        (switch-to-buffer slime-buf))))

(defun lvsn-add-bike-highlighting ()
  (font-lock-add-keywords
   nil
   '(("\\[\\([:]\\(?:\\s_\\|\\w\\)+\\)\\>"
      (1 font-lock-type-face)))))

(let ((spaces-regex (regexp-opt-charset '(?\r ?\n ?\t ?\s)))
      (var-regex (regexp-opt '("define-constant"
                               "define-global-var"
                               "define-global-parameter"
                               "define-global-var*"
                               "define-global-parameter*")
                             'symbols))
      (%def-regex (regexp-opt '("define-")
                              nil))
      (misc-macro-regex (regexp-opt '("define-"
                                      "with-"
                                      "without-"
                                      "do-")
                                    nil))
      (misc-defines-regex (regexp-opt '("defvop"
                                        "definline")
                                      'symbols))
      (symbol-regex "\\(\\(?:\\w\\|\\s_\\)+\\)")
      (symbol-regex-nocapture "\\(?:\\(?:\\w\\|\\s_\\)+\\)")
      )
  (setq lvsn-package-prefix-regex
        (concat "(" symbol-regex "\\:"))
  (setq lvsn-qualified-defglobal-regex
        (concat "(" symbol-regex "\\:" var-regex spaces-regex "+" symbol-regex))
  (setq lvsn-unqualified-defglobal-regex
        (concat "(" var-regex spaces-regex "+" symbol-regex))
  (setq lvsn-qualified-def-regex
        (concat "(" symbol-regex "\\:\\(" %def-regex symbol-regex-nocapture "\\)\\>"
                spaces-regex "+" symbol-regex))
  (setq lvsn-unqualified-def-regex
        (concat "(\\(" %def-regex symbol-regex-nocapture "\\)\\>"
                spaces-regex "+" symbol-regex))
  (setq lvsn-qualified-misc-macro-regex
        (concat "(" symbol-regex "\\:\\(" misc-macro-regex symbol-regex-nocapture "\\)\\>"))
  (setq lvsn-unqualified-misc-macro-regex
        (concat "(\\(" misc-macro-regex symbol-regex-nocapture "\\)\\>"))
  (setq lvsn-constant-regex
        (concat "\\_<\\([+]" symbol-regex-nocapture "[+]\\)\\_>"))
  (setq lvsn-global-regex
        (concat "\\_<\\([-]" symbol-regex-nocapture "[-]\\)\\_>"))
  (setq lvsn-misc-defines-regex
        (concat "(" misc-defines-regex spaces-regex "+"))
  (setq lvsn-misc-defines-regex-full
        (concat "(" misc-defines-regex spaces-regex "+" symbol-regex "\\_>"))
  t)

(defun lvsn-add-slime-highlighting ()
  (font-lock-add-keywords
   nil
   `((,lvsn-qualified-defglobal-regex
      (1 font-lock-type-face)
      (2 font-lock-keyword-face)
      (3 font-lock-variable-name-face))
     (,lvsn-unqualified-defglobal-regex
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face))
     (,lvsn-qualified-def-regex
      (1 font-lock-type-face)
      (2 font-lock-keyword-face)
      (3 font-lock-type-face))
     (,lvsn-unqualified-def-regex
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     (,lvsn-qualified-misc-macro-regex
      (1 font-lock-type-face)
      (2 font-lock-keyword-face))
     (,lvsn-unqualified-misc-macro-regex
      (1 font-lock-keyword-face))
     (,lvsn-package-prefix-regex
      (1 font-lock-type-face))
     (,lvsn-constant-regex
      (1 font-lock-variable-name-face))
     (,lvsn-global-regex
      (1 font-lock-variable-name-face))
     (,lvsn-misc-defines-regex
      (1 font-lock-keyword-face))
     (,lvsn-misc-defines-regex-full
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)))
   t))

(add-hook 'slime-mode-hook 'lvsn-add-slime-highlighting)
(add-to-list 'window-buffer-change-functions 'lvsn--init-slime-windows)

(defvar lvsn-bike-extensions t
  "Whether to enable additional syntax for bike library.")

(when lvsn-bike-extensions
  (add-hook 'slime-mode-hook 'lvsn-add-bike-highlighting)
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table))

(defvar lvsn-start-slime t
  "Whether to start SLIME on startup")

(provide 'lvsn-slime)

(when lvsn-start-slime
  (slime))

;; vim: ft=lisp et!
