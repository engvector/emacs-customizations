;;; evil.el --- All Evil mode related customizations
;;; Commentary:
;;; Evil mode configures Emacs to use vim style keybindings.
;; This helps mitigate carpel tunnel syndrome since you don't have to bend and
;; twist your fingers in odd ways to use Emacs keybindings.

;;; Code:

(use-package undo-tree :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode t)

  ;; change cursor movement to those used in gaming
  (define-key evil-normal-state-map (kbd "w") 'previous-line)
  (define-key evil-normal-state-map (kbd "s") 'next-line)
  (define-key evil-normal-state-map (kbd "a") 'backward-char)
  (define-key evil-normal-state-map (kbd "d") 'forward-char)

  ;; Similar to gaming, use characters around w-s-a-d for extended movement commands.
  (define-key evil-normal-state-map (kbd "e") 'evil-forward-word-begin)
  (define-key evil-normal-state-map (kbd "q") 'evil-backward-word-begin)
  (define-key evil-normal-state-map (kbd "z") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "c") 'end-of-line)
  ;; Change 'insert-before' to 'insert-after'
  (define-key evil-normal-state-map (kbd "i") 'evil-append)
  ;; Change evil's redo mapping
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "r") 'evil-redo)
  ;; Remap 'S' to save the current file
  (define-key evil-normal-state-map (kbd "S") 'save-buffer)
  ;; Remap 'D' to delete whole line
  (define-key evil-normal-state-map (kbd "D") 'evil-delete-whole-line)

  ;; Remove interpretation of 'j' in insert mode.
  (define-key evil-insert-state-map (kbd "j") '(lambda() (interactive) (insert "j")))

  ;; -- Visual Mode customizations --
  ;; Since we will be using 'a' to move left, we have to remap all a-* functions.
  ;; We will be using 'e' instead of 'a'.
  (define-key evil-visual-state-map (kbd "e \"") 'evil-a-double-quote)
  (define-key evil-visual-state-map (kbd "e '") 'evil-a-single-quote)
  (define-key evil-visual-state-map (kbd "e (") 'evil-a-paren)
  (define-key evil-visual-state-map (kbd "e )") 'evil-a-paren)
  (define-key evil-visual-state-map (kbd "e <") 'evil-an-angle)
  (define-key evil-visual-state-map (kbd "e >") 'evil-an-angle)
  (define-key evil-visual-state-map (kbd "e B") 'evil-a-curly)
  (define-key evil-visual-state-map (kbd "e W") 'evil-a-WORD)
  (define-key evil-visual-state-map (kbd "e [") 'evil-a-bracket)
  (define-key evil-visual-state-map (kbd "e ]") 'evil-a-bracket)
  (define-key evil-visual-state-map (kbd "e `") 'evil-a-back-quote)
  (define-key evil-visual-state-map (kbd "e b") 'evil-a-paren)
  (define-key evil-visual-state-map (kbd "e o") 'evil-a-symbol)
  (define-key evil-visual-state-map (kbd "e p") 'evil-a-paragraph)
  (define-key evil-visual-state-map (kbd "e s") 'evil-a-sentence)
  (define-key evil-visual-state-map (kbd "e t") 'evil-a-tag)
  (define-key evil-visual-state-map (kbd "e w") 'evil-a-word)
  (define-key evil-visual-state-map (kbd "e {") 'evil-a-curly)

  ;; use the same cursor movements in visual mode too
  (define-key evil-visual-state-map (kbd "w") 'previous-line)
  (define-key evil-visual-state-map (kbd "s") 'next-line)
  (define-key evil-visual-state-map (kbd "a") 'backward-char)
  (define-key evil-visual-state-map (kbd "d") 'forward-char)
  )

;;; d-evil.el ends here
