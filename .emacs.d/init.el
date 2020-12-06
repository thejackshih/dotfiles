;; melpa
(require 'package)
(add-to-list `package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)
;; end melpa

;; macos shell environment
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
;; end macos shell

;; theme
(unless (package-installed-p 'monokai-theme)
  (package-install 'monokai-theme))
(load-theme 'monokai t)
;; end theme

;; emacs striped
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; end striped

(setq display-line-numbers 'relative)

;; default font
(set-face-attribute 'default nil
		    :family "Liberation Mono"
		    :height 140
		    :weight 'normal
		    :width 'normal)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
