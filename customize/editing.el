;; Spellchecking
(use-package flyspell
  :ensure t

  :init
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq ispell-program-name "ispell")
  (setq ispell-local-dictionary "english")
  (setq ispell-local-dictionary-alist
        '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

  :bind
  ("C-$" . 'flyspell-correct-word-before-point))

;; Project management help
(use-package projectile
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'projectile-mode))

;; Use icons for some of the characters
(use-package all-the-icons
  :ensure t)

;; Completion (code/text) support
(use-package company
  :ensure t
  :bind (("M-SPC" . company-complete))
  :config
  (global-company-mode)
  )

;; Highlight indent guides (vertical lines showing start / end of an indented block)
(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  :init
  (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Configuration for ibuffer mode (used in 'see all buffers'
(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 30 30 :left :elide))))

;;--------------------------------
;; Yank and Pop helpers
;;--------------------------------
;; Indent sensitive yanking
(defvar yank-indent-modes '(prog-mode
                            org-mode
                            sgml-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))


(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))


(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

