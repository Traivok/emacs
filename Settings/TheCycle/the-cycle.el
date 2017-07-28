;;; the-cycle.el --- Theme Cycle

;;; Autor: José Ricardo Alves Figueirôa

;;; Commentary:
;;; Navigate between themes, disable them all and so on.
;;; Interactive functions:
;;; (disable-all-themes)
;;; (switch-between-themes (key))

;;; Code:

;; disable all themes
(defun disable-all-themes ()
  "Disable all themes."
  (interactive)
  (dolist (cur-theme custom-enabled-themes)
    (disable-theme cur-theme)))

(defvar light-theme-list (list) "All of your light themes.")

(defvar dark-theme-list (list) "All of your dark themes.")
(defvar theme-list (list 'light-theme-list 'dark-theme-list) "All of your theme lists.")

(setq light-theme-list (list))

(defun thecycle-add-theme-to-list (your-list value)
  "Add to YOUR-LIST a VALUE."
  (add-to-list your-list (list value)))

(thecycle-add-theme-to-list 'light-theme-list 'gruvbox-light-soft)
(thecycle-add-theme-to-list 'light-theme-list 'gruvbox-light-medium)
(thecycle-add-theme-to-list 'light-theme-list 'gruvbox-light-hard)
(thecycle-add-theme-to-list 'dark-theme-list 'gruvbox-dark-soft)
(thecycle-add-theme-to-list 'dark-theme-list 'gruvbox-dark-medium)
(thecycle-add-theme-to-list 'dark-theme-list 'gruvbox-dark-hard)

(defun thecycle-direction-from-key (key)
  "Return directionection based on KEY value."
  (cond
   ((eq key ?a) 1)
   ((eq key ?d) 2)
   ((eq key ?w) 3)
   ((eq key ?s) 4)
   (t nil)))

(defun thecycle-switch-between-lists (themes direction)
  "Switch between elements of THEMES and DIRECTION rules the switching directionection.")

(defun thecycle-switch-between-themes (themes direction)
  "Switch between elements of THEMES and DIRECTION rules the switching directionection."
  (let*
    ((len (length themes))
     (pos (cl-position custom-enabled-themes themes :test 'equal))
     (inc (cond
	   ((= direction 1) 1)
	   ((= direction 2) -1)
	   (t 0)))
     (new-pos (cond
	       ((< (+ inc pos) 0) (- len 1))
	       (t (% (+ inc pos) len )))))
    (message "%s %s %s %s" len pos inc new-pos)
    (disable-all-themes)
    (dolist (cur-theme (nth new-pos themes))
      (load-theme cur-theme t)
      (message "loading %s" cur-theme))))

;; begin function
(defun thecycle-switch-theme (key)
  "Switch themes KEY W and S will switch between dark and light.
KEY A and D will be switch between soft, medium, and hard, if its provided."
  (interactive "cHit wasd: ")
  ;; get the directionection
  (let ((direction (thecycle-direction-from-key key)))
    (cond
     ;; switching between dark themes
     ((and (member custom-enabled-themes dark-theme-list) (member direction '(1 2)))
      (thecycle-switch-between-themes dark-theme-list direction)
       )
     ;; ((and (member custom-enabled-themes dark-theme-list) (member direction '(1 2))

      ;; (thecycle-switch-between-themes dark-theme-list direction))
     ;; switching between light themes
     ;; ((and (member custom-enabled-themes light-theme-list) (member direction '(1 2)))
      ;; (thecycle-switch-between-themes light-theme-list direction))
     ;; if switching between lists
    ;; ((member direction '(3 4))
     ;; (thecycle-switch-between-lists theme-list direction))
    ;; else, do nothing
    (t (push key unread-command-events)
     ))
    ))
;; end

(provide 'the-cycle)
;;; the-cycle.el ends here
