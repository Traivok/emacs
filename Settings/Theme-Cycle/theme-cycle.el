;;; theme-cycle.el --- Theme Cycle

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
(defvar theme-pattern-list (list) "All of your theme lists.")

(setq theme-pattern-list nil)

(defun theme-cycle-add-theme-to-list (your-list value)
  "Add to YOUR-LIST a VALUE."
  (delete (symbol-value your-list) theme-pattern-list)
  (add-to-list your-list (list value))
  (add-to-list 'theme-pattern-list  (symbol-value your-list))
  )

(theme-cycle-add-theme-to-list 'light-theme-list 'gruvbox-light-soft)
(theme-cycle-add-theme-to-list 'light-theme-list 'gruvbox-light-medium)
(theme-cycle-add-theme-to-list 'light-theme-list 'gruvbox-light-hard)
(theme-cycle-add-theme-to-list 'dark-theme-list 'gruvbox-dark-soft)
(theme-cycle-add-theme-to-list 'dark-theme-list 'gruvbox-dark-medium)
(theme-cycle-add-theme-to-list 'dark-theme-list 'gruvbox-dark-hard)

(defun theme-cycle-direction-from-key (key)
  "Return directionection based on KEY value."
  (cond
   ((eq key ?a) 1)
   ((eq key ?d) 2)
   ((eq key ?w) 3)
   ((eq key ?s) 4)
   (t nil)))

(defun theme-cycle-get-new-element (your-list element direction up down) ;; improve this function
  "Return a element based on YOUR-LIST, ELEMENT, DIRECTION, UP and DOWN."
  (let*
      ((len (length your-list))
       (pos (cl-position element your-list :test 'equal))
       (inc (cond
	     ((= direction up) 1)
	     ((= direction down) -1)
	     (t 0)))
       (new-pos (cond
		 ((< (+ inc pos) 0) (- len 1))
		 (t (% (+ inc pos) len )))))
    (nth new-pos your-list)))

(defun theme-cycle-find-in-patterns (search-for)
  "SEARCH-FOR a theme and return in what list it's."
  (message "\n\nGet called for: %s" search-for)

  (let ((out-ret nil))
    (dolist (theme-group theme-pattern-list)
      (dolist (cur-theme theme-group)
	(when (or (equal search-for cur-theme) (member search-for cur-theme))
      (setq-local out-ret theme-group))))
    out-ret))

(defun theme-cycle-switch-between-lists (themes direction)
  "Switch between THEMES and DIRECTION rules the switching directionection."
  (let ((cur-list (theme-cycle-find-in-patterns custom-enabled-themes)))))

(defun theme-cycle-switch-between-themes (themes direction)
  "Switch between elements of THEMES and DIRECTION rules the switching directionection."
  (let ((new-theme (theme-cycle-get-new-element themes custom-enabled-themes direction 1 2)))
    (disable-all-themes)
    (dolist (cur-theme new-theme)
      (load-theme cur-theme t)
      (message "Loading: %s theme." cur-theme))))

;; begin function
(defun theme-cycle-switch-theme (key)
  "Switch themes KEY W and S will switch between dark and light.
KEY A and D will be switch between soft, medium, and hard, if its provided."
  (interactive "cHit wasd: ")
  ;; get the directionection
  (let ((direction (theme-cycle-direction-from-key key)))
    (cond
     ;; switching between dark themes
     ((and (member custom-enabled-themes dark-theme-list) (member direction '(1 2)))
      (progn (theme-cycle-switch-between-themes dark-theme-list direction)
	     (call-interactively 'theme-cycle-switch-theme)))
     ;; switching between light themes
     ((and (member custom-enabled-themes light-theme-list) (member direction '(1 2)))
      (progn (theme-cycle-switch-between-themes light-theme-list direction)
	     (call-interactively 'theme-cycle-switch-theme)))
     ((member direction '(3 4))
     (theme-cycle-switch-between-lists theme-pattern-list direction))
    ;; else, do nothing
    (t (push key unread-command-events)  ))))
;; end

(provide 'theme-cycle)
;;; theme-cycle.el ends here
