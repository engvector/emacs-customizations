;;--------------------------------------
;; This file loads specific emacs configuration/customization files.
;;--------------------------------------

;; Helper function. Accepts a list of files and loads them using a base path
(defun load-files (base-path files)
  (mapcar (lambda (file) (load-file (concat base-path file)))
	  files))

;; The base path from where all customization files should be loaded
(setq settings-root-path "~/.emacs.d/customize/")
(setq code-test-root-path "./customize/")

;; The files list
(setq files-list '(
		   ;; Basic customizations
		   "secure-pkg-source.el" ;; https for package installation
		   "global-settings.el"
		   "global-keyboard-shortcuts.el"
		   ))

;; Certain customizations install packages using "use-package"
(package-install 'use-package)

;; Load all customizations
(load-files code-test-root-path files-list)
