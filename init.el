;;;; init.el --- Emacs startup -*- lexical-binding: t -*-

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

;;; `lisp' directory contains actual init scripts and logic
;;; N.B. On Windows, you MUST set HOME enviroment variable value to your user directory
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Uncomment to change default initial dir.
;; (setq lvsn-default-directory "~/Dev/Lisp")

;;; Uncomment to change the executable name,
;;;  in case you are using different Common Lisp implementation
;; (setq lvsn-lisp-progam "sbcl")

;;; Uncomment in case you want to disable syntax etc. for bike library
;; (setq lvsn-bike-extensions nil)

;;; Uncomment to disable SLIME startup
;; (setq lvsn-start-slime nil)

;;; Uncomment the following and change the value in case you want to open different
;;;  initial file when starting SLIME.
;;; Initial file comes from lvsn-default-directory
;; (setq lvsn-scratch-file "scratch.lisp")

;;; Uncomment to change the default font on Windows
;; (setq lvsn-default-windows-font '(:family "Consolas" :height 110))
;;; Uncomment to change the default font on Unices
;; (setq lvsn-default-unix-font '(:family "JetBrains Mono" :height 110))

(require 'lvsn-commons)
(require 'lvsn-slime)
(require 'lvsn-keys)

;;; IMPORTANT:
;;;  On first start, run: M-x all-the-icons-install-fonts
;;;   to complete all-the-icons package installation
