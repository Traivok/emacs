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
    (shell-other-window)
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
  
;; kill unused buffer
(require 'tempbuf "~/.emacs.d/Settings/tempbuf.el")
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'after-change-major-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)

(defun my-term-on-tempbuf-expire ()
  "Killing idle terminal."
  (when (get-buffer-process (current-buffer))
    (comint-send-eof)))

(defun my-term-mode-patch ()
  "Turning tempbuf on."
  (turn-on-tempbuf-mode)
  (add-hook 'tempbuf-expire-hook 'my-term-on-tempbuf-expire
	    nil t))
(add-hook 'term-mode-hook 'my-term-mode-patch)
;; end

(provide 'Misc)
;;; Misc.el ends here
