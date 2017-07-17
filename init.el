(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;          Load packages and theirs configurations          ;;
(defun load-directory (dir)
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
(setq linum-format "%d ")
(toggle-scroll-bar -1)
;; End

;; Display columns
(setq column-number-mode t)

;; no startup msg  
(setq inhibit-startup-message t) ; Disable startup message

;; load theme
(load-theme 'monokai t)

;; refresh packages
;;(package-refresh-contents)

;; clipboard bug
;(setq x-select-enable-clipboard nil)
