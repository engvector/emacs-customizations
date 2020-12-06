;; Ensure 'package' is present
(require 'package)

;; TLS settings
(require 'cl)
(setq tls-checktrust t)

;; Find the python command on current OS
(setq python (or (executable-find "py.exe")
                 (executable-find "python3")
                 ))

;; If certificate validation error occurs when installing packages
;; on a terminal, run 'sudo
;; /Library/Developer/CommandLineTools/usr/bin/pip3 install certifi' and
;; restart emacs

;; Find the file that contains all the CAs
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
   (setq tls-program
         (list
          (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                  (if (eq window-system 'w32) ".exe" "") trustfile)))
   (setq gnutls-verify-error t)
   (setq gnutls-trustfiles (list trustfile)))

;; Required nowadays to avoid connection errors with ELPA/MELPA
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Setup package URLs
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Rewrite package managers list
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

;; Initialize the archive
(package-initialize)

;; Refresh package contents if required
(unless (and
	 (file-exists-p "~/.emacs.d/elpa/archives/gnu")
	 (file-exists-p "~/.emacs.d/elpa/archives/melpa")
	 (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))
