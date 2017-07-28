;;; my-visual.el --- my visual setup

;;; Commentary:
;;; My theming, font, visual package and menu configuration.

;;; Code:

(require 'the-cycle "~/.emacs.d/Settings/TheCycle/the-cycle.el")
(require 'rainbow-delimiters)
(require 'powerline)

;; (thecycle-add-theme-to-list dark-theme-list 'gruvbox-dark-soft)
;; (thecycle-add-theme-to-list dark-theme-list 'gruvbox-dark-medium)
;; (thecycle-add-theme-to-list dark-theme-list 'gruvbox-dark-hard)
;; (thecycle-add-theme-to-list light-theme-list 'gruvbox-light-soft)
;; (thecycle-add-theme-to-list light-theme-list 'gruvbox-light-medium)
;; (thecycle-add-theme-to-list light-theme-list 'gruvbox-light-hard)

;; set Terminus font
(set-frame-font "xos4 Terminus 12")

;; theme
(powerline-center-theme)
(load-theme 'gruvbox-dark-medium t)

;; Display, Troggle, enable...
(define-global-minor-mode my-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'term-mode 'shell-mode 'eshell-mode)))
      (linum-mode t))))

(my-linum-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(defvar linum-format "%d ")
(toggle-scroll-bar -1)
;; End

;; Display columns
(setq column-number-mode t)

;; no startup msg
(setq inhibit-startup-message t) ; Disable startup message
;; no scratch msg
(setq initial-scratch-message ";;;  Evenings, Mornings, And a Couple of Saturdays  ;;;\n\n")

;; Add highlight matching parenthesis
(show-paren-mode 1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(provide 'my-visual)
;;; my-visual.el ends here
