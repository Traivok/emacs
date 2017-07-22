;;; installed-packages.el --- my installed packages
;;; Commentary:
;;; if any package of this list was not found, then this file will install it automatically

;;; Code:

(defvar my-package-list '(auto-highlight-symbol cmake-ide cmake-mode company-c-headers irony company-irony company-irony-c-headers
						rtags company-rtags flycheck-irony flycheck-rtags magit monokai-theme smart-comment yasnippet)
  "All my packages list.")

;; check if any package of my-package-list isn't installed and install it
(dolist (my-package my-package-list)
  (when (require 'my-package nil 'noerror)
      (package-install 'my-package)))
;; end

;; update packages
(when (not package-archive-contents)
    (package-refresh-contents))
;; end

(provide 'my-installed-packages)
;;; my-installed-packages.el ends here
