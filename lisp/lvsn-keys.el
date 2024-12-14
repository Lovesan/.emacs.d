;;;; lvsn-keys.el --- Keybinding utils -*- lexical-binding: t -*-

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

(require 'cl-lib)

(use-package lvsn-slime
             :demand t)

;;; Require included rebinder package
(use-package rebinder
             :demand t
             :config
             (rebinder-hook-to-mode 't 'after-change-major-mode-hook))

(defun lvsn--call-interactively-inhibit-kill-ring (fun &rest args)
  "Call FUN interactively saving the kill-ring when called interactively.
Otherwise call FUN with ARGS."
  (if (interactive-p)
      (let ((kill-ring '("dummy")) ; Dummy value in case FUN tries to append.
            (kill-ring-yank-pointer nil))
        (call-interactively fun))
      (apply fun args)))

(defun lvsn--inhibit-kill-ring-in (cmd)
  (advice-add cmd :around #'lvsn--call-interactively-inhibit-kill-ring))

(defun lvsn-c-up ()
  (interactive)
  (previous-line 5))

(defun lvsn-c-down ()
  (interactive)
  (next-line 5))

(defun lvsn-copy ()
  (interactive)
  (when (and mark-active (mark))
    (let ((start (mark)) (end (point)))
      (or (<= start end)
          (setq start (prog1 end (setq end start))))
      (copy-region-as-kill start end 'region)
      (setq mark-active t
            deactivate-mark nil))))

(defun lvsn-cut ()
  (interactive)
  (when (and mark-active (mark))
    (let ((start (mark)) (end (point)))
      (or (<= start end)
          (setq start (prog1 end (setq end start))))
      (copy-region-as-kill start end 'region)
      (delete-region start end))))

(defun lvsn-paste ()
  (interactive)
  (when (and mark-active (mark))
    (let ((start (mark)) (end (point)))
      (or (<= start end)
          (setq start (prog1 end (setq end start))))
      (delete-region start end)))
  (yank))

(defun lvsn-neotree-change-root ()
  (interactive)
  (mouse-set-point last-input-event)
  (redisplay t)
  (call-interactively 'neotree-change-root))

(defun lvsn-open-file ()
  (interactive)
  (if (display-graphic-p)
      (let (last-nonmenu-event)
        (menu-find-file-existing))
      (call-interactively 'find-file)))

(defun lvsn-elisp-goto-symbol ()
  (interactive)
  (mouse-set-point last-input-event)
  (redisplay t)
  (call-interactively 'lispy-goto-symbol))

(defun lvsn-insert-e ()
  (interactive)
  (insert "e"))

(defun lvsn-insert-colon ()
  (interactive)
  (insert ":"))

;;; Windows behaviour for killring
(lvsn--inhibit-kill-ring-in 'backward-kill-word)
(lvsn--inhibit-kill-ring-in 'kill-word)
(lvsn--inhibit-kill-ring-in 'kill-line)

;;; Rebind C-x to C-e and C-c to C-d
(define-key global-map (kbd "C-e") (rebinder-dynamic-binding "C-x"))
(define-key global-map (kbd "C-d") (rebinder-dynamic-binding "C-c"))
(define-key lispy-mode-map (kbd "C-e") (rebinder-dynamic-binding "C-x"))
(define-key lispy-mode-map (kbd "C-d") (rebinder-dynamic-binding "C-c"))

(define-key rebinder-mode-map (kbd "C-x") 'lvsn-cut)
(define-key rebinder-mode-map (kbd "C-c") 'lvsn-copy)
(define-key global-map (kbd "C-a") 'mark-whole-buffer)
(define-key global-map (kbd "C-v") 'lvsn-paste)
(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-w") 'kill-buffer)
(define-key global-map (kbd "C-z") 'undo-tree-undo)
(define-key global-map (kbd "C-y") 'undo-tree-redo)
(define-key global-map (kbd "C-o") 'lvsn-open-file)
(define-key global-map (kbd "C-q") 'save-buffers-kill-terminal)
(define-key global-map (kbd "<C-f4>") 'kill-buffer)
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ;; turn off font-dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(define-key global-map (kbd "<f4>") 'lvsn-revert-to-repl)
(define-key global-map (kbd "<C-next>") 'ace-window)
(define-key global-map (kbd "<M-left>") 'previous-buffer)
(define-key global-map (kbd "<M-right>") 'next-buffer)
(define-key global-map (kbd "<mouse-4>") 'previous-buffer)
(define-key global-map (kbd "<mouse-5>") 'next-buffer)
(define-key global-map (kbd "C-f") 'isearch-forward)
(define-key global-map (kbd "C-S-f") 'isearch-backward)
(define-key global-map (kbd "<C-down-mouse-1>")  nil)
(define-key global-map (kbd "<C-up>") 'lvsn-c-up)
(define-key global-map (kbd "<C-down>") 'lvsn-c-down)
(define-key global-map (kbd "<f2>") 'list-buffers)
(define-key global-map (kbd "<f8>") 'neotree-toggle)
;;; In addition to M-x
(define-key global-map (kbd "<menu>") 'execute-extended-command)
(define-key global-map (kbd "<apps>") 'execute-extended-command)

(define-key neotree-mode-map (kbd "<C-mouse-1>") 'lvsn-neotree-change-root)

(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

(define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)

(define-key lispy-mode-map (kbd "[") 'lispy-brackets)
(define-key lispy-mode-map (kbd "<M-left>") 'previous-buffer)
(define-key lispy-mode-map (kbd "<M-right>") 'next-buffer)
(define-key lispy-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key lispy-mode-map (kbd "e") nil)

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "<f11>") 'xref-pop-marker-stack)
(define-key emacs-lisp-mode-map (kbd "<f12>") 'lispy-goto-symbol)
(define-key emacs-lisp-mode-map (kbd "<C-mouse-1>") 'lvsn-elisp-goto-symbol)
(define-key emacs-lisp-mode-map (kbd "<mouse-4>") 'xref-pop-marker-stack)
(define-key emacs-lisp-mode-map (kbd "<mouse-5>") 'xref-go-forward)

(define-key slime-mode-map (kbd "<f1>") 'slime-describe-symbol)
(define-key slime-mode-map (kbd "<f3>") 'slime-apropos-all)
(define-key slime-mode-map (kbd "<f5>") 'slime-eval-last-expression)
(define-key slime-mode-map (kbd "<f6>") 'lvsn-slime-compile-and-load-file)
(define-key slime-mode-map (kbd "<f11>") 'slime-pop-find-definition-stack)
(define-key slime-mode-map (kbd "<f12>") 'slime-edit-definition)
(define-key slime-mode-map (kbd "<C-mouse-1>") 'lvsn-slime-edit-definition)
(define-key slime-mode-map (kbd "<mouse-4>") 'slime-pop-find-definition-stack)
(define-key slime-mode-map (kbd "<mouse-5>") 'xref-go-forward)
(define-key slime-mode-map (kbd "<M-up>") 'lispy-backward)
(define-key slime-mode-map (kbd "<M-down>") 'lispy-flow)
(define-key slime-mode-map (kbd "C-x m") 'hs-toggle-hiding)
(define-key slime-mode-map (kbd "C-M-d") 'slime-disassemble-symbol)

(define-key slime-repl-mode-map (kbd "<f1>") 'slime-describe-symbol)
(define-key slime-repl-mode-map (kbd "<f3>") 'slime-apropos-all)
(define-key slime-repl-mode-map (kbd "<f12>") 'slime-edit-definition)
(define-key slime-repl-mode-map (kbd "<C-mouse-1>") 'lvsn-slime-edit-definition)
(define-key slime-repl-mode-map (kbd "<mouse-4>") 'slime-pop-find-definition-stack)
(define-key slime-repl-mode-map (kbd "<mouse-5>") 'xref-go-forward)
(define-key slime-repl-mode-map (kbd "C-M-d") 'slime-disassemble-symbol)

(provide 'lvsn-keys)

;; vim: ft=lisp et!
