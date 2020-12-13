;;; dap-support --- DAP (Debug Adapter Support) for programming
;;; Commentary:
;;  DAP mode packages and configuration for supported languages

;; Global DAP package
(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  (global-set-key (kbd "<f6>") 'dap-debug)
  (global-set-key (kbd "<f10>") 'dap-next)
  (global-set-key (kbd "<f11>") 'dap-step-in)
  (global-set-key (kbd "<f12>") 'dap-step-out)
  (global-set-key (kbd "<f9>") 'dap-breakpoint-add)
  (global-set-key (kbd "<f8>") 'dap-breakpoint-delete))

;;--------------------------------------
;; Language specific DAP packages
;;--------------------------------------
;; Java
;; Make sure you install JDK (not just JRE) from Oracle site.
(require 'dap-java)
(dap-register-debug-provider
 "java"
 (lambda (conf)
   (plist-put conf :debugPort 58888)
   (plist-put conf :host "localhost")
   conf))
(setq dap-java-java-command "/bin/zsh -e /usr/bin/java")
(dap-register-debug-template "My Runner"
			     (list :type "java"
                                   :request "launch"
                                   :args ""
                                   :env '(("DEV" . "1"))))

;;--------------------------------------
;; Python
;; First run `pip install "ptvsd>=4.2"`
(require 'dap-python)
(setq dap-python-executable "python3.9")
