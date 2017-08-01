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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Org/index.org" "~/Org/Disciplinas.org" "~/Org/Rotina.org")))
 '(safe-local-variable-values
   (quote
    ((eval with-eval-after-load
	   (quote flycheck)
	   (setq-default flycheck-disabled-checkers
			 (quote
			  (emacs-lisp-checkdoc))))
     (org-use-property-inheritance . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
