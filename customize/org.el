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
        (quote (:link t :maxlevel 8 :compact t)))
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
;;  (add-hook 'org-mode-hook 'variable-pitch-mode)

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
