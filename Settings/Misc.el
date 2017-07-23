;;; Misc.el --- miscellanea
;;; Commentary:
;;; Enviroment configuration

;;; Code:

(require 'midnight)
(setq midnight-period (* 2 60 60))

(add-to-list 'clean-buffer-list-kill-regexps '("magit"))


(provide 'Misc)
;;; Misc.el ends here
