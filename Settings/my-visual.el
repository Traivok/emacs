;;; my-visual.el --- my visual setup

;;; Commentary:
;;; My theming, font, visual package and menu configuration.

;;; Code:

(require 'rainbow-delimiters)
(require 'powerline)

(require 'theme-cycle)

;; inline flycheck errors

(theme-cycle-add-theme-to-group 'theme-cycle-dark-group 'gruvbox-dark-soft)
(theme-cycle-add-theme-to-group 'theme-cycle-dark-group 'gruvbox-dark-medium)
(theme-cycle-add-theme-to-group 'theme-cycle-dark-group 'gruvbox-dark-hard)
(theme-cycle-add-theme-to-group 'theme-cycle-light-group 'gruvbox-light-soft)
(theme-cycle-add-theme-to-group 'theme-cycle-light-group 'gruvbox-light-medium)
(theme-cycle-add-theme-to-group 'theme-cycle-light-group 'gruvbox-light-hard)

;; set Terminus font
(add-to-list 'default-frame-alist
	     '(font . "xos4 Terminus 12"))

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
