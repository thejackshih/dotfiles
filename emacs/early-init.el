(defun restore-gc-cons-threshold ()
  (setq gc-cons-threshold (* 128 1024 1024)
	gc-cons-percentage 0.1))
(setenv "LSP_USE_PLISTS" "true")
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook #'restore-gc-cons-threshold -99)
(setq package-enable-at-startup nil)
