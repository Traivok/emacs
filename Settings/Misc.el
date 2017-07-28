;;; Misc.el --- miscellanea
;;; Commentary:
;;; Enviroment configuration

;;; Code:

;; midnight mode, for killing unused buffer
(require 'midnight)
(require 'my-functions "~/.emacs.d/Settings/my-functions.el")
(setq midnight-period (* 2 60 60))

;; ido
(require 'ido)
(defvar ido-enable-flex-matching t "Flexible string matching.")
(defvar ido-every-where t "Always ido.")

(setq ido-file-extensions-order '(".hpp" ".cpp" ".c" ".el"))

(ido-mode t)
;; end

;; mpsyt
(defun open-mpsyt (&optional wsearch)
  "Opens mpsyt in shell and search for &WSEARCH."
  (interactive)
  (let ((mpsyt-cmd (if wsearch (concat "mpsyt /" wsearch) "mpsyt")))
    (wterm-other-window 'shell)
    (rename-buffer "Mpsyt")
    (insert mpsyt-cmd)
    (comint-send-input nil t)))

(defun open-cmus ()
  "Opens cmus."
  (interactive)
  (shell-other-window)
  (rename-buffer "Cmus")
  (insert "cmus")
  (comint-send-input nil t))
;; end
  
(provide 'Misc)
;;; Misc.el ends here
