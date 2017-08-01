;;; init.el --- init file

;;; Commentary:
;;; Melpa
;;; Load from ~/.emacs.d/Settings/
;;; Visual, font and theme configuration and general setups like
;;; show-paren, smart-comment

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; Code:

(require 'package)
(package-initialize)

(require 'org)

(org-babel-do-load-languages ;; auto load elisp
 'org-babel-load-languages
 '((emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil) ;; do not ask before evaluate

(org-babel-load-file ;; load setup
 (expand-file-name "setup.org"
                   user-emacs-directory))

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
