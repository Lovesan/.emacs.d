;;; lvsn-commons.el --- Common init logic -*- lexical-binding: t -*-

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
(require 'display-line-numbers)
(require 'bs)
(require 'ibuffer)
(require 'package)
(require 'use-package)

;;; Configure package manager URL
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Initialize package manager
(package-initialize)

;;; Undo history
(use-package undo-tree
             :demand t
             :ensure t
             :config
             ;; Enable undo-tree globally
             (global-undo-tree-mode)
             :custom
             (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;;; Editorconfig support
(use-package editorconfig
             :demand t
             :ensure t
             :config
             ;; Enable editorconfig mode globally
             (editorconfig-mode 1))

;;; Highlights parens
(use-package highlight-parentheses
             :demand t
             :ensure t
             :init
             (show-paren-mode)
             :config
             ;; Enable highlight-parentheses globally
             (define-globalized-minor-mode global-highlight-parentheses-mode
               highlight-parentheses-mode
               (lambda ()
                 (highlight-parentheses-mode t)))
             (global-highlight-parentheses-mode t))

;;; Highlights parens as rainbow
(use-package rainbow-delimiters
             :demand t
             :ensure t
             :hook
             ;; Enable rainbow delimiters in lisp modes
             (lisp-interaction-mode . rainbow-delimiters-mode)
             (emacs-lisp-mode . rainbow-delimiters-mode)
             (lisp-mode . rainbow-delimiters-mode))

;;; Allows for quickly switching frames
(use-package ace-window
             :demand t
             :ensure t)

;;; Icons.
;;; IMPORTANT:
;;;  On first start, run: M-x all-the-icons-install-fonts
(use-package all-the-icons
             :demand t
             :ensure t
             :if (display-graphic-p))

;;; The left panel
(use-package neotree
             :ensure t
             :demand t
             :config
             (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
             ;; Hide Lisp fasls by regex
             (add-to-list 'neo-hidden-regexp-list "\\.fas[bl]?$")
             (add-to-list 'neo-hidden-regexp-list "\\.wx\\(32\\|64\\)fsl$"))

;;; Tooltips
(use-package pos-tip
             :demand t
             :ensure t)

;;; Auto-complete
(use-package auto-complete
             :demand t
             :ensure t
             :config
             (setq ac-auto-show-menu t)
             (setq ac-use-comphist t)
             (setq ac-quick-help-delay 1)
             (setq ac-quick-help-prefer-pos-tip nil)
             (define-globalized-minor-mode real-global-auto-complete-mode
               auto-complete-mode (lambda ()
                                    (if (not (minibufferp (current-buffer)))
                                        (auto-complete-mode 1))))
             (real-global-auto-complete-mode t))

(use-package magit
             :demand t
             :ensure t)

(use-package markdown-mode
             :demand t
             :ensure t
             :mode ("README\\.md\\'" . gfm-mode)
             :init (setq markdown-command "multimarkdown"))

(defun lvsn-os-windows-p ()
  "Returns non-nil value in case we are running on windows"
  (eq system-type 'windows-nt))

(defvar lvsn-default-directory
  (if (lvsn-os-windows-p)
      "C:/Dev/Lisp"
      "~/Dev/Lisp")
  "Default directory to CD to")

(setq default-directory lvsn-default-directory)

(mkdir lvsn-default-directory t)

(cd lvsn-default-directory)

(defvar lvsn-default-windows-font '(:family "Consolas" :height 110)
  "Default font on windows")

(defvar lvsn-default-unix-font '(:family "JetBrains Mono" :height 110)
  "Default font on unices")

;;; Set default font
(if (lvsn-os-windows-p)
    (progn
      (apply 'set-face-attribute 'default nil lvsn-default-windows-font)
      (setq w32-use-visible-system-caret nil))
    (apply 'set-face-attribute 'default nil lvsn-default-unix-font))

(setq-default cursor-type 'bar)

;;; Maximize window on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized)

;;; Disable initial logo
(setq inhibit-splash-screen t)

;;; Default to UTF-8 everywhere
(set-language-environment 'UTF-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

;;; Indent with spaces on default
(setq-default indent-tabs-mode nil)

;;; Default tab width
(setq-default tab-width 4)
(setq indent-line-function 'indent-relative)
(setq tab-stop-list (number-sequence 4 200 4))
;;; Tab makes a completion
(setq tab-always-indent 'complete)

;;; Windows behaviour
(delete-selection-mode 1)

;; Disable tool bar
(tool-bar-mode -1)

;;; For convenience
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Setup scrolling
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-margin            10)
(transient-mark-mode 1) ;; No region when it is not highlighted

;;; Setup clipboard related things
(setq select-enable-clipboard t)
(setq select-enable-primary nil)
(setq mouse-drag-copy-region nil)

;;; Setup backup files
(setq-default make-backup-files nil)
(setq-default auto-save-defaults t)
(setq auto-save-defaults nil)
(setq backup-inhibitd t)
(define-globalized-minor-mode global-auto-save-mode
  auto-save-visited-mode
  (lambda ()
    (auto-save-visited-mode t)))
(setq auto-save-timeout 1)
(global-auto-save-mode t)

;;; `list-buffers' command for convenience
(defalias 'list-buffers 'ibuffer)

;;; Setup line numbers on the left
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode neotree-mode slime-repl-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(provide 'lvsn-commons)
