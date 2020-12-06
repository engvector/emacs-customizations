;; Record all window layout changes
;; "C-c <right>" and "C-c <left>" lets you move between these changes.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Helper functio - lock window to a specific buffer.
(define-minor-mode sticky-buffer-mode "Make the current window always display
    this buffer."  nil " sticky" nil (set-window-dedicated-p (selected-window)
							     sticky-buffer-mode))
