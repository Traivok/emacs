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

(defvar light-theme-list (list 'gruvbox-light-soft 'gruvbox-light-medium 'gruvbox-light-hard))
(defvar dark-theme-list (list 'gruvbox-dark-soft 'gruvbox-dark-medium 'gruvbox-dark-hard))
(defvar theme-list (list 'light-theme-list 'dark-theme-list))

(defun switch-between-list-themes (theme-list)
  "Switch between elements of THEME-LIST."
  (let
      (
       (len (length '(theme-list)))
       (pos (cl-position 'custom-enabled-themes '(theme-list)))
       )
    (message "%d %s" len pos)
    

      )
  )

(defun switch-theme ()
  "Switch themes KEY up/w and down/s will switch between dark and light.
KEY left/a and right/d will be switch between soft, medium, and hard, if its provided."
  (interactive)
  (if (or (member custom-enabled-themes dark-theme-list) (equal custom-enabled-themes custom-enabled-themes))
      (switch-between-list-themes dark-theme-list)
    (message "not")
    ;; (switch-between-list-themes light-theme-list)
    ))

(provide 'the-cycle)
;;; the-cycle.el ends here

