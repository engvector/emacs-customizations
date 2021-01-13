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

		   ;; Customzations for all modes
		   "window-mgmt.el"
		   "completion.el"
		   "editing.el"
		   "tools.el"
		   "git.el"

		   ;; Language specific customizations
		   "ansible.el"
		   "cpp.el"
		   "shell.el"
		   "markdown.el"
		   "csharp.el"
		   "org.el"
		   "ledger.el"

		   ;; Language Server Protocol(LSP) & Debug Adapter Protocol(DAP)
		   "lsp-support.el"
		   "dap-support.el"
		   ))

;; Certain customizations install packages using "use-package"
(package-install 'use-package)

;; Load all customizations
(load-files code-test-root-path files-list)

;;--------------------------------------
;; Emacs customizations filled using
;; customization forms
;;--------------------------------------
;; Variable customizations - mainly org agenda view and capture templates
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   '(("A" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo ""
		((org-agenda-overriding-header "=* INBOX *=")
		 (org-agenda-files
		  '("~/Documents/todo/inbox.org"))))
       (alltodo "" nil))
      nil)))
 '(org-capture-templates
   '(("g" "Capture a GOAL" entry
      (file "~/Documents/todo/goals.org")
      (file "~/Documents/todo/templates/goal-capture.txt"))
     ("t" "Add a TASK to inbox" entry
      (file "~/Documents/todo/inbox.org")
      (file "~/Documents/todo/templates/task-capture.txt"))
     ("h" "Capture a THOUGHT. Something that needs further elaboration later." entry
      (file "~/Documents/todo/inbox.org")
      (file "~/Documents/todo/templates/thought-capture.txt"))
     ("r" "Capture a REMINDER" entry
      (file "~/Documents/todo/inbox.org")
      (file "~/Documents/todo/templates/reminder-capture.txt")
      :time-prompt t)
     ("m" "Schedule a MEETING" entry
      (file "~/Documents/todo/inbox.org")
      (file "~/Documents/todo/templates/meeting-capture.txt"))
     ("p" "Capture a REPEATing task." entry
      (file "~/Documents/todo/inbox.org")
      (file "~/Documents/todo/templates/repeat-capture.txt"))))
 '(org-log-reschedule 'time)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-refile-targets '((org-agenda-files :maxlevel . 9)))
 '(package-selected-packages
   '(winner-mode vagrant-tramp ob-tangle pdf-tools highlight-indent-guides autopair magit yaml-mode which-key use-package treemacs markdown-mode leuven-theme ledger-mode ivy-hydra helpful google-this git-timemachine flymake-yaml counsel-projectile company babel ansible all-the-icons)))

;; Customized faces/appearance
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight regular :family "IBM Plex Mono"))))
 '(annotate-annotation ((t (:inherit fixed-pitch :background "coral" :foreground "black"))))
 '(annotate-annotation-secondary ((t (:inherit fixed-pitch :background "coral1" :foreground "black" :slant italic))))
 '(annotate-highlight ((t (:underline "coral"))))
 '(annotate-highlight-secondary ((t (:underline "coral1"))))
 '(cursor ((t (:inherit default :background "blue"))))
 '(fixed-pitch ((t (:weight regular :family "IBM Plex Mono"))))
 '(line-number ((t (:inherit fixed-pitch :inherit shadow))))
 '(line-number-current-line ((t (:inherit fixed-pitch :inherit shadow :background "gray80"))))
 '(org-block ((t (:inherit fixed-pitch :background "#FFFFEA" :extend t))))
 '(org-block-begin-line ((t (:inherit shadow :background "#F0F0FF" :extend t))))
 '(org-block-end-line ((t (:inherit shadow :background "#F0F0FF" :extend t))))
 '(org-document-info ((t (:height 1.2))))
 '(org-document-info-keyword ((t (:inherit shadow :height 1.2))))
 '(org-document-title ((t (:height 1.3 :weight bold))))
 '(org-drawer ((t (:inherit shadow :slant italic))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
 '(org-level-7 ((t (:inherit outline-7 :height 1))))
 '(org-level-8 ((t (:inherit outline-8 :height 1))))
 '(org-property-value ((t (:inherit shadow :slant italic))) t)
 '(org-special-keyword ((t (:inherit org-drawer :weight bold))))
 '(region ((t (:inherit highlight :background "gray70"))))
 '(variable-pitch ((t (:weight light :family "Arial" :height 1.2)))))
