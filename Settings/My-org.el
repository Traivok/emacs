;;; my-org.el --- my org setup
;;; Commentary:
;;; Just my org mode setup

;;; Code:

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(provide 'my-org)
;;; my-org.el ends here
