(use-package yaml-mode :ensure t)
(use-package ansible
  :ensure t
  :init (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))
(use-package flymake-yaml
  :ensure t
  :init (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  (add-hook 'yaml-mode-hook (lambda () (variable-pitch-mode -1))))

(require 'url)
;; Copy ob-ansible.el if it doesn't exist. This is used to support org-babel for ansible
(unless (file-exists-p "~/.emacs.d/ob-ansible.el")
  (url-copy-file "https://raw.githubusercontent.com/zweifisch/ob-ansible/master/ob-ansible.el"
		 "~/.emacs.d/ob-ansible.el"))
(load-file "~/.emacs.d/ob-ansible.el")
