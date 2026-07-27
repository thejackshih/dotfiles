(defun restore-gc-cons-threshold ()
  (setopt gc-cons-threshold (* 128 1024 1024)
	gc-cons-percentage 0.1))
(setenv "LSP_USE_PLISTS" "true")
(setopt gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setopt package-enable-at-startup nil)
(add-hook 'emacs-startup-hook #'restore-gc-cons-threshold -99)
