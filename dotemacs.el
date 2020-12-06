;; This file loads specific emacs configuration/customization files.

;; Use HTTPS for package installation
(load-file "./.emacs.d/secure-pkg-source.el")
;; Configure global settings
(load-file "./.emacs.d/global-settings.el")

;; Remaining configuration requires the 'use-package' package.
(package-install 'use-package)
