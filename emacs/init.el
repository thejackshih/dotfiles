(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setopt straight-use-package-by-default t)
(straight-use-package 'use-package)

(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(blink-cursor-mode 0)
(setopt initial-scratch-message "")
(setopt inhibit-startup-message t)
(setopt visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(load-theme 'modus-vivendi-tinted)
(toggle-frame-fullscreen)

(setopt use-short-answers t)
(setopt use-dialog-box nil)
(setopt native-comp-async-report-warnings-errors nil)
(setopt warning-suppress-log-types '((files missing-lexbind-cookie)))
(setopt delete-by-moving-to-trash t)
(setopt vc-follow-symlinks t)

(setq read-process-output-max (* 1024 1024))

(let ((custom-path (concat user-emacs-directory "custom.el")))
  (unless (file-exists-p custom-path)
    (make-empty-file custom-path))
  (setopt custom-file custom-path)
  (load custom-file))

(let ((backup-path (concat user-emacs-directory "backups")))
  (unless (file-exists-p backup-path)
    (make-directory backup-path t))
  (setopt backup-directory-alist `((".*" . ,backup-path))))

(let ((autosave-directory (concat user-emacs-directory "autosaves")))
  (unless (file-exists-p autosave-directory)
    (make-directory autosave-directory t))
  (setopt auto-save-file-name-transforms `((".*" ,autosave-directory t))))

(set-face-attribute 'default nil
		    :family "Martian Mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x pgtk))
          (daemonp))
  :config
  (dolist (var '("LC_CTYPE"
                 "NIX_PROFILES"
                 "NIX_SSL_CERT_FILE"
                 "__NIX_DARWIN_SET_ENVIRONMENT_DONE"
                 "LSP_USE_PLISTS"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-git-executable "/etc/profiles/per-user/jack/bin/git"))

(use-package org
  :custom
  (org-log-done 'time)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :hook
  (org-mode . (lambda ()
                (setopt buffer-face-mode-face '(:family "Sarasa Mono TC"))
                (buffer-face-mode))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 10)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package envrc
  :config
  (envrc-global-mode))

(use-package ox-hugo
  :after ox)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (python-ts-mode . lsp)
         ;; (go-ts-mode . lsp)
         ;; (nix-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode)
                    :priority 0
                    :server-id 'nixd))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :after lsp-mode)
;; (use-package dap-LANGUAGE) to load the adapter dap for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "/etc/profiles/per-user/jack/bin/clisp")
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl))
  :hook (common-lisp-mode . slime))

(use-package rainbow-delimiters
  :hook (elisp-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :mode (("\\.bb\\'" . clojure-mode)
         ("\\.clj\\'" . clojure-mode)))

(use-package cider
  :init
  ;; (setq cider-preferred-build-tool 'clojure-cli)
  :config
  ;; Register Babashka as a known Clojure CLI tool
  ;; (setq cider-clojure-cli-global-options ""
  ;; cider-babashka-parameters "nrepl-server")
  :hook (clojure-mode . cider-mode)
  )

(use-package paredit
  :hook (clojure-mode . paredit-mode))

(use-package web-mode
  :mode "\\.vue\\'")

(use-package ghostel
  :commands ghostel)

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq visual-fill-column-center-text t)
  :straight (visual-fill-column
             :type git
             :host codeberg
             :repo "joostkremers/visual-fill-column"))

(defun my-darwin-rebuild ()
  "Async Call darwin rebuild"
  (interactive)
  (async-shell-command "sudo darwin-rebuild switch"))

(defun my-launch-app ()
  "start application"
  (interactive)
  (progn
    (make-frame `((parent-frame . ,(selected-frame))
                  (undecorated . t)
                  (minibuffer . only)
                  (left . ,(/ (- (frame-pixel-width (selected-frame)) (* 40 (frame-char-width))) 2))
                  (top . 0)))
    (unwind-protect
        (let* ((apps (append
                      (directory-files "/Applications" nil ".app")
                      (directory-files "~/Applications" nil ".app")
                      (directory-files "/system/Applications" nil ".app")
                      (directory-files "/system/Applications/Utilities" nil ".app")))
               (apps-without-extension (mapcar (lambda (x) (string-replace ".app" "" x)) apps))
               (vertico-count 30)
               (resize-mini-frames t)
               (max-mini-windows-height 0.8)
               (window-min-width 40)
               (user-choice (completing-read "Select a app:" apps-without-extension nil t)))
          (with-environment-variables (("__NIX_DARWIN_SET_ENVIRONMENT_DONE" ""))
            (start-process "launcher" nil "open" "-a" user-choice)))
      (delete-frame)
      )))
;; (global-set-key (kbd "M-s-<SPC>") 'my-launch-app)

(use-package agent-shell
  :ensure t
  :config
  (setopt agent-shell-antigravity-acp-command '("/Users/jack/agy-acp-server/agy_acp_server.par"))
  (setq agent-shell-antigravity-authentication
	(agent-shell-antigravity-make-authentication :login t)))
