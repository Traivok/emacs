;;; installed-packages.el --- my installed packages
;;; Commentary:
;;; if any package of this list was not found, then this file will install it automatically

;;; Code:

(defvar my-package-list nil "All my packages list.")

(setq my-package-list '(auto-highlight-symbol cmake-ide cmake-mode company-c-headers irony company-irony company-irony-c-headers
					      rtags company-rtags flycheck-irony flycheck-rtags magit monokai-theme smart-comment yasnippet
					      midnight ido org darcula-theme))

;; update database
(unless package-archive-contents (package-refresh-contents))

;; check if any package of my-package-list isn't installed and install it
(dolist (my-package my-package-list)
  (unless (package-installed-p my-package)
    (package-install my-package)))

;; update packages
(when (not package-archive-contents)
    (package-refresh-contents))

(provide 'my-installed-packages)
;;; my-installed-packages.el ends here
