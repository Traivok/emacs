;;; my-org.el --- my org setup
;;; Commentary:
;;; Just my org mode setup

;;; Code:

(require 'org)
(require 'org-ac)
(require 'org-bullets)
(require 'org-alert)

(defun org-file-path (filename)
  "Return the absolute address of an org FILENAME, given its relative name."
  (concat (file-name-as-directory org-directory) filename))


(setq org-directory "~/Org")
(defvar org-index-file (org-file-path "index.org"))
(setq org-agenda-files (list org-index-file))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an 'org-mode' item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

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
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "qpdfview %s"))
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

(add-hook 'org-mode-hook
	  (lambda () (org-update-statistics-cookies t)))

(defun worf-delete-subtree (arg)
  "Delete subtree or ARG chars."
  (interactive "p")
  (if (and (looking-at "\\*")
           (looking-back "^\\**" (line-beginning-position)))
      (org-cut-subtree)
    (delete-char arg)))

;; define an advice
(defadvice worf-delete-subtree (after my-org-update-parent-todo ())
  (org-update-parent-todo-statistics))
;; activate all advices to this function
(ad-activate 'worf-delete-subtree)

(define-key org-mode-map (kbd "C-d") 'worf-delete-subtree)

(define-key org-agenda-mode-map (kbd "C-c m") 'org-agenda-month-view)

;; (setq org-use-property-inheritance t)

(provide 'my-org)
;;; my-org.el ends here
