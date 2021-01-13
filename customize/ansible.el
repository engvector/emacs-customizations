;;; ansible.el --- Packages and customization for Ansible -*- lexical-binding: t -*-

;; Author: Vinay Venkatesh
;; Maintainer: Vinay Venkatesh
;; Version: 0.1

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Packages and configuration for using Emacs as Ansible IDE.

;;; Code:

(require 'url)

(require 'use-package)

;; Copy ob-ansible.el if it doesn't exist. This is used to support org-babel for ansible
(unless (file-exists-p "~/.emacs.d/ob-ansible.el")
  (url-copy-file "https://raw.githubusercontent.com/zweifisch/ob-ansible/master/ob-ansible.el"
		 "~/.emacs.d/ob-ansible.el"))
(load-file "~/.emacs.d/ob-ansible.el")

(use-package yaml-mode :ensure t)

(use-package flymake-yaml
  :ensure t
  :init (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  (add-hook 'yaml-mode-hook (lambda () (variable-pitch-mode -1))))

(use-package ansible
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda ()
			       (progn
				 (ansible 1)
				 (yas-load-directory "~/.emacs.d/elpa/ansible-20201001.838/")
				 (ansible-doc-mode)))))

(use-package ansible-doc
  :ensure t
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))

(use-package company-ansible
  :ensure t
  :config
  (add-to-list 'company-backends 'company-ansible))

;; Custom flycheck integration
;; Linter must be installed first - "pip3.9 install ansible-lint"
(flycheck-define-checker ansible
  "Ansible linter using ansible-lint tool"
  :command ("ansible-lint" source-original)
  :error-patterns
  ((warning line-start (message) line-end
    line-start (file-name) ":" line line-end
    (optional line-start (message)  line-end)))
  :modes (ansible))

;;; ansible.el ends here
