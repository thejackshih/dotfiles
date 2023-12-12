(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)

(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq native-comp-async-report-warnings-errors nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; (when (string= system-type "darwin")
;;   (setq dired-use-ls-dired t
;;         insert-directory-program "/etc/profiles/per-user/jack/bin/gls"
;;         dired-listing-switches "-aBhl --group-directories-first"))


(defvar --custom-el (concat user-emacs-directory "custom.el"))
(if (not (file-exists-p --custom-el))
    (make-empty-file --custom-el))
(setq custom-file --custom-el)
(load --custom-el)


(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `((".*" . ,--backup-directory)))


(set-face-attribute 'default nil
		    :height 120
		    :weight 'normal
		    :width 'normal)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("LC_CTYPE" "NIX_PROFILES" "NIX_SSL_CERT_FILE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")
(use-package markdown-mode
  :ensure t)
(use-package magit)
(use-package eglot
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs
	       '((dart-mode dart-ts-mode) . ("fvm" "dart" "language-server" "--client-id" "emacs.eglot-dart")))
  :hook
  (python-ts-mode . eglot-ensure)
  (dart-mode . eglot-ensure))
(use-package vterm
  :ensure t)

(use-package nimbus-theme
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package monokai-theme
  :ensure t)

(use-package nordic-night-theme
  :ensure t)

(use-package darktooth-theme
  :ensure t)

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t
      corfu-quit-no-match 'separator)
  )

(use-package org
  :ensure t)

(use-package hima-theme
  :ensure t)

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(use-package dart-mode
  :ensure t)

(use-package nov
  :ensure t)

(use-package perfect-margin
  :ensure t)

(use-package eat
  :ensure t)
