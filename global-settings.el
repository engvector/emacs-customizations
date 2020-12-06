;; Remove menubar, toolbar and scrollbars from the window.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show the file name and major mode in the title bar. This doesn't apply to emacs launched in Terminal.
(setq-default frame-title-format '("%b [%m]"))

;; ask for y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Deleting files moves it to trash instead of removing them outright.
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Don't create a new frame everytime you open a file
(setq ns-pop-up-frames nil)

;; Show column number in statusbar
(setq column-number-mode t)

;; Highlighting matching parentheses across all modes.
(show-paren-mode t)

;; Highlight the current line
(global-hl-line-mode 1)

;; Show line number in all buffers
(global-display-line-numbers-mode)

;; enable highlighting colors when a piece is text is selected (for copy/cut)
(transient-mark-mode 1)
;; Enable overwrite / delete highlighted region
(delete-selection-mode t)

(setq org-hide-emphasis-markers t)

;; Don't show emacs welcome page
(setq inhibit-startup-message t)

;; Block bell rings from emacs by replacing it with a dummy function
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell t)
