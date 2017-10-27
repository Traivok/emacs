;;; init.el --- init file

;;; Commentary:
;;; Melpa
;;; Load from ~/.emacs.d/Settings/
;;; Visual, font and theme configuration and general setups like
;;; show-paren, smart-comment

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Org/Rotina.org" "~/Org/index.org" "~/Org/IC.org" "~/Org/Estudo.org" "~/Org/Disciplinas.org")))
 '(package-selected-packages
   (quote
    (ag xref-js2 multiple-cursors js2-refactor js2-mode fontawesome highlight-indent-guides nasm-mode monokai-theme rainbow-mode i3wm yasnippet theme-cycle smart-comment rainbow-delimiters powerline package-build org-bullets org-alert org-ac magit gruvbox-theme flycheck-rtags flycheck-package flycheck-irony company-rtags company-irony-c-headers company-irony company-c-headers cmake-mode cmake-ide brainfuck-mode auto-highlight-symbol 0blayout)))
 '(safe-local-variable-values
   (quote
    ((eval org-disable-alert)
     (org-use-property-inheritance . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
