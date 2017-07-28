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

(defun thecycle-switch-between-themes (themes)
  "Switch between elements of THEMES."
  (let
      (
       (len (length theme-list))
       (pos (cl-position custom-enabled-themes themes :test 'equal))
       )
    (message "%d %s" len pos)
    )
  )

(defun thecycle-switch-theme ()
  "Switch themes KEY up/w and down/s will switch between dark and light.
KEY left/a and right/d will be switch between soft, medium, and hard, if its provided."
  (interactive)
  (if (member custom-enabled-themes dark-theme-list)
      (thecycle-switch-between-themes dark-theme-list)
    (message "not")
    ;; (switch-between-list-themes light-theme-list)
    ))


(provide 'the-cycle)
;;; the-cycle.el ends here

