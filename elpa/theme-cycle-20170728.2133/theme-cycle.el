;;; theme-cycle.el --- Provides a switching theme enviroment.

;; Copyright (c) 2017 Free Software Foundation, Inc.

;; Autor: José Ricardo Alves Figueirôa <jraf@cin.ufpe.br>
;; Version: 1.0
;; Package-Version: 20170728.2133
;; Package-Requires: ((emacs "24.3"))
;; Keywords: faces
;; URL: https://github.com/Traivok/emacs-theme-cycle

;;; Commentary:
;; This package provides interactive functions to disable all themes.
;; Switch between themes in some group (list) or between the groups itself.

;; To configure it
;; (load-file "path/to/theme.cycle.el")
;; (theme-cycle-add-theme-to-group 'dark-theme-list 'some-dark-theme)

;;; Notes:
;; I'm to working with unitary lists because custom-enabled-theme return lists
;; But i pretend to implement multiple enabled themes later.

;;; Summary
;; Global Variables
;; Auxiliary function (intended for internal auxiliary use)
;; Internal (intended for internal use)
;; Interface (user interface with this package)
;; Interactive (interactive funcions)

;;; Code:

(require 'cl-lib)

;;; GLOBAL VARIABLES ;;;
(defvar theme-cycle-light-group nil "All of your light themes.")
(defvar theme-cycle-dark-group nil "All of your dark themes.")
(defvar theme-cycle-all-groups nil "All of your theme groups.")
(defvar theme-cycle-switch-delay 0.25 "Switching delay.")
;; END ;;;

;;; AUXILIARY FUNCTIONS ;;;
(defun theme-cycle-direction-by-key (key)
  "Return direction based on KEY value.
Return values can be (+1, -1 or nil), +1 when key points to forwards, -1 when key points to backwards, nil when neither."
  (cond
   ((member key '(?a ?s)) -1) ;; previous
   ((member key '(?d ?w)) +1) ;; next
   (t nil)))

(defun theme-cycle-orientation-by-key (key)
  "Return direction based on KEY value.
Return values can be (+1, -1 or nil), +1 when key refer to horizontal, -1 when key points vertical, nil when neither."
  (cond
   ((member key '(?a ?d)) +1)
   ((member key '(?w ?s)) -1)
   (t nil)))

(defun theme-cycle-get-new-element (your-list element direction)
  "Return a element based on YOUR-LIST, ELEMENT, DIRECTION."
  (let*
      ((len (length your-list)) ;; get the length of list
       (pos (cl-position element your-list :test 'equal)) ;; postition of current theme
       (inc (if direction direction 0)) ;; and the direction (+1, -1, 0)
       (new-pos (cond ;; and the position of new item
		 ((< (+ inc pos) 0) (- len 1)) ;; if exceeds min bound, then the new pos is the last item
		 (t (% (+ inc pos) len))))) ;; check exceeds the max bound
    (nth new-pos your-list))) ;; return

(defun theme-cycle-what-group (search-for)
  "SEARCH-FOR a theme and return the first group that it appears."
  (let ((what-group nil))
    (dolist (theme-group theme-cycle-all-groups)
      (dolist (cur-theme theme-group)
	(when (subsetp search-for cur-theme)
      (setq-local what-group theme-group))))
    what-group))
;;; END ;;;

;;; INTERNAL-FUNCTIONS ;;;
(defun theme-cycle-switch-between-groups (direction)
  "Switch between THEMES and DIRECTION rules the switching directionection."
  (let ((cur-group nil))
    ;; searching the theme group that contains the current enabled theme
    (dolist (group theme-cycle-all-groups)
      (message "Searching if %s %s" custom-enabled-themes group)
      (when (member custom-enabled-themes group)
    	(setq-local cur-group group)))
    ;; this ugly code occurs because custom-enabled-theme returns lists
    ;; so i'm working with unitary list, but i pretend to fix it later
    (let* ((next-group (theme-cycle-get-new-element theme-cycle-all-groups cur-group direction))
	   (next-theme (nth 0 (nth 0 next-group))))
      ;; loading next theme
      (theme-cycle-disable-all-themes)
      (message "Loading: %s theme" next-theme)
      (load-theme next-theme t))))

(defun theme-cycle-switch-between-themes (group direction)
  "Switch between theme of GROUP and DIRECTION rules the switching directionection."
  (let ((new-theme (theme-cycle-get-new-element group custom-enabled-themes direction)))
    (theme-cycle-disable-all-themes)
    (dolist (cur-theme new-theme)
      (load-theme cur-theme t)
      (message "Loading: %s theme." cur-theme))))
;;; END ;;;

;;; INTERFACE ;;;
(defun theme-cycle-add-theme-to-group (your-group theme)
  "Add to YOUR-GROUP a THEME."
  (setq theme-cycle-all-groups (delete (symbol-value your-group) theme-cycle-all-groups))
  (add-to-list your-group (list theme))
  (add-to-list 'theme-cycle-all-groups  (symbol-value your-group)))
;;; END ;;;

;;; INTERACTIVE FUNCTIONS ;;;

;; switch between themes group
(defun theme-cycle-switch (key)
  "Switch themes KEY W and S will switch between dark and light.
KEY A and D will be switch between soft, medium, and hard, if its provided."
  (interactive "cHit wasd: ")
  ;; get the direction (previous/next) and orientation (vertical/horizontal)
  (message "Current theme %s" custom-enabled-themes)
  (sleep-for theme-cycle-switch-delay)
  (let* ((orientation (theme-cycle-orientation-by-key key))
	(direction (theme-cycle-direction-by-key key))
	(cur-group (theme-cycle-what-group custom-enabled-themes)))
    ;; body
    (cond ((not (and orientation direction)) (push key unread-command-events)) ;; invalid key
	  ;; horizontal orientation (between themes)
	  ((= orientation 1)
    	   (progn
	     (theme-cycle-switch-between-themes cur-group direction)
	   (call-interactively 'theme-cycle-switch)))
	  ;; vertical orientation (between groups)
	  ((= orientation -1)
	   (progn
	     (theme-cycle-switch-between-groups direction)
	     (call-interactively 'theme-cycle-switch)))
	  (t (progn
	   (push key unread-command-events))
	     (message "  ERROR:\n
Key: %s\n
Orientation: %s\n
Direction: %s\n
Current Group: %s\n
Custom-Enabled-Theme: %s\n
All-Theme-Group: %s" key orientation direction cur-group custom-enabled-themes theme-cycle-all-groups)))))
;; end

;;; INTERACTIVE
(defun theme-cycle-disable-all-themes ()
  "Disable all themes."
  (interactive)
  (dolist (cur-theme custom-enabled-themes)
    (disable-theme cur-theme)))
;;; END ;;;

(provide 'theme-cycle)
;;; theme-cycle.el ends here
