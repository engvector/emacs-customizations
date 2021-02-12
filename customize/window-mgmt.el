;; package --- Customizations for window management.

;;; Commentary:
;; Custom minor mode and other changes to suit my window management needs.

;;; Code:

;; Record all window layout changes
;; "C-c <right>" and "C-c <left>" lets you move between these changes.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Helper function - lock window to a specific buffer.
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil
  " sticky"
  nil
  (progn
    (set-window-dedicated-p (selected-window) sticky-buffer-mode)
    (setq-default window-size-fixed t)))

;;; window-mgmt.el ends here

