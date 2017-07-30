;;; my-org.el --- my org setup
;;; Commentary:
;;; Just my org mode setup

;;; Code:

(require 'org)
(require 'org-ac)
(require 'org-bullets)
(require 'org-alert)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise.
Get N-DONE by the org statistics hook.
If N-NOT-DONE = 0, then done, else todo."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(org-ac/config-default)

(setq  org-hide-leading-stars t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; more exports options
(require 'ox-md)
(require 'ox-beamer)

;; highlight
(setq org-src-fontify-natively t)

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; opens pdf in qdpfview and html in firefox
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
	 (delete '("\\.x?html?\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "foxitreader %s"))
	 (add-to-list 'org-file-apps '("\\.x?html?\\'" . "/usr/bin/firefox --new-tab %s"))))

;; log by time done tasks
(setq org-log-done 'time)
(defun my-org-archive-done-tasks ()
  "Archive done tasks automatically."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; org alert
(org-alert-enable)
(setq alert-default-style 'libnotify)

(provide 'my-org)
;;; my-org.el ends here
