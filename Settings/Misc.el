;;; Misc.el --- miscellanea
;;; Commentary:
;;; Enviroment configuration

;;; Code:

(require 'midnight)
(setq midnight-period (* 2 60 60))

(add-to-list 'clean-buffer-list-kill-regexps '("magit"))

;; ido
(require 'ido)
(defvar ido-enable-flex-matching t "Flexible string matching.")
(defvar ido-every-where t "Always ido.")

(setq ido-file-extensions-order '(".hpp" ".cpp" ".c" ".el"))

(ido-mode t)

;; end

(provide 'Misc)
;;; Misc.el ends here
