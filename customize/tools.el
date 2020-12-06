;; File/Folder navigation
(use-package treemacs
  :ensure t
  :config
  (treemacs-set-width 80)
  (setq frame-title-format '((:eval (projectile-project-name))))
  (global-prettify-symbols-mode t))

;; Google for the word under cursor
(use-package google-this
  :ensure t
  :config (google-this-mode)
  :bind ("C-c /" . 'google-this)
  )

;; RSS
(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config (setq rmh-elfeed-org-files
                (list "~/.emacs.d/elfeed.org")))

;; Integration of tramp mode with Vagrant
(use-package vagrant-tramp
  :ensure t)

;; Function to quickly get my IP address.
(defun what-is-my-ip ()
  (interactive)
  (message "IP: %s"
           (with-current-buffer (url-retrieve-synchronously "https://api.ipify.org")
             (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))
