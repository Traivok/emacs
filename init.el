;;; init.el --- init file

;;; Commentary:
;;; Melpa
;;; Load from ~/.emacs.d/Settings/
;;; Visual, font and theme configuration and general setups like
;;; show-paren, smart-comment

;;; Code:

(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; install and update packages
(require 'my-installed-packages "~/.emacs.d/Settings/my-installed-packages.el")
;; end

;;          Load packages and theirs configurations          ;;
(defun load-directory (dir)
  "Load all *.el files at (DIR)."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/Settings/")
;; End

;;            Set backup directory
(setq backup-directory-alist `(("." . "/home/ricardo/Development/.backup")))
(setq delete-old-versions t
  kept-new-versions 2
  kept-old-versions 2
  version-control t)
(put 'scroll-left 'disabled nil)
;; End

;; Add highlight matching parenthesis
(show-paren-mode 1)

;; open.h files as c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; set Terminus font
(set-frame-font "xos4 Terminus 12")

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

;; theme
(require 'powerline)
(powerline-center-theme)
(load-theme 'gruvbox-dark-medium t)

;; smart comment
(add-to-list 'load-path "~/emacs.d")
(require 'smart-comment)
;; end

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" default)))
 '(package-selected-packages
   (quote
    (0blayout powerline gruvbox-theme yasnippet smart-comment org-bullets org-ac magit flycheck-rtags flycheck-irony company-rtags company-irony-c-headers company-irony company-c-headers cmake-mode cmake-ide auto-highlight-symbol))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
