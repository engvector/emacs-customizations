;; Lots of customizations on base org-package
(use-package org
  :ensure t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (C . t)
     (shell . t)
     (emacs-lisp . t)
     (dot . t)
     ;; Include other languages here...
     ))

  :init
  (setq org-indent-mode nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t
        org-hide-leading-stars t) ;; Hide leading stars
  (setq org-indent-indentation-per-level 2) ;; Set org indentation to 2.
  
  :config
  ;; Org-mode customizations
  (setq org-use-speed-commands 1)
  (setq org-agenda-start-with-clockreport-mode t)
  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 8 :compact t :stepskip0 t :fileskip0 t)))
  (setq org-clock-into-drawer "CLOCKING")
  (setq org-agenda-archives-mode t)
  (setq org-log-done t)
  (setq org-fontify-done-headline t)
  ;; Load all org-mode files in my work directory
  (setq org-agenda-files 
        (directory-files-recursively
	 "/Users/vinayvenkatesh/Documents/Career" ".+org$"))

  ;; Whenever new org-mode file is opened, check if is 
  ;; in org-agenda-files list and add it if not.
  (defun org-mode-file-hook ()
    (if nil (member (buffer-file-name (current-buffer))
		    org-agenda-files)
      (progn
	(add-to-list 'org-agenda-files
		     (buffer-file-name (current-buffer)) t)
	(setq org-agenda-files (remove nil org-agenda-files))
	)))
  (add-hook 'org-mode-hook 'org-mode-file-hook)

  ;; Log all reschedules
  (setq org-log-reschedule 'time)
  
  ;; Todo workflow settings
  (setq org-todo-keyword-faces
        '(
          ("TODO" . "red")
          ("STARTED" . "violet")
          ("WAITING" . "orange")
          ("DEFERRED" . "pink1")
          ("DONE" . "green3")
          ("DELEGATED" . "cyan2")
          ("CANCELLED" . "blue")
          )
        )
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "WAITING" "DEFERRED" "|" "DONE"
                    "DELEGATED" "CANCELLED")))


  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  (put 'narrow-to-region 'disabled nil)
  ;; Include path to homebrew's installation of dot command
  (setenv "PATH"
          (concat
           "/usr/local/Cellar/graphviz/2.42.2/bin/" ":"
           "/Library/TeX/texbin/latex" ":"
           (getenv "PATH")))
  ;; Enable embedded images in org-mode
  (add-hook
   'org-babel-after-execute-hook
   'org-display-inline-images 'append)
  (org-display-inline-images)
  (setq org-startup-with-inline-images t)
  
  (defadvice org-update-refile-targets (around org-refile)
    (progn
      (ivy-mode t)
      (setq org-refile-targets nil)
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps 't)
      (setq org-log-into-drawer t)
      (setq org-log-refile 'time)
      (org-agenda-log-mode)
      (setq org-refile-targets '((org-agenda-files :maxlevel . 9)))))
  
  :bind*
  (("\C-cl" . 'org-store-link)
   ("\C-ca" . 'org-agenda)
   ("\C-cc" . 'org-capture)
   ))

(require 'org)
(require 'ob-tangle)
(require 'org-tempo)

;; Define recurring tasks using shortcuts in the entry
(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :ensure t
  :config
  (setq org-recur-finish-done t
        org-recur-finish-archive t)
  ;; rebind 'd' key in org-agenda (default: org-agenda-day-view)
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)
  :bind* ("C-c d" . 'org-recur-finish))

;; Show org-file as slides
(use-package org-tree-slide
  :ensure t
  :bind*
  (("<C-s-left>" . 'org-tree-slide-move-previous-tree)
   ("<C-s-right>" . 'org-tree-slide-move-next-tree)))

;; Note taking extension to org-mode. Can be used with files that
;; emacs can load in its window.
(use-package org-noter
  :ensure t
  :config (setq org-noter-auto-save-last-location t))

;; Use special bullet icons instead of the default '*'
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  )

;; Use the default "..." ellipsis
(setq org-ellipsis nil)
