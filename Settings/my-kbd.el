;;; my-kbd.el --- keybindings setup

;;; Commentary:
;;; Binding:
;;; (v/h)-resize, company-complete, yas-insert-snippet,
;;; move lines, eval-(region/buffer), move-text-(up/down)
;;; smart-comment, magit, goto setups files/folder

;;; Code:

;; window resize
(global-set-key (kbd "C-c w v") 'v-resize)
(global-set-key (kbd "C-c w h") 'h-resize)
;; end

;; C/C++ completion
(require 'cc-mode)
(define-key c-mode-base-map [?\M-\r] 'company-complete)
(global-set-key (kbd "C-c y") 'yas-insert-snippet)
;; end

;; Irony
(define-key c-mode-base-map (kbd "C-c t") 'irony-get-type)
;; end

;; Goto setup files and folders
(require 'my-functions "~/.emacs.d/Settings/my-functions.el")
(global-set-key (kbd "C-c i f") 'goto-init-file)
(global-set-key (kbd "C-c i d") 'goto-setup-dir)
;; end

;; Magit
(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c p") 'magit-push)
;; end

;; Move lines
(global-set-key [\C-\S-up] 'move-text-up)
(global-set-key [\C-\S-down] 'move-text-down)
;; end

;; evaluation
(global-set-key (kbd "M-_") 'eval-region)
(global-set-key (kbd "M-+") 'eval-buffer)
;; end

;; smart comment
(global-set-key (kbd "M-;") 'smart-comment)
;; end

;; open shell
(global-set-key (kbd "C-c i s") 'shell-other-window)

(provide 'my-kbd)
;;; my-kbd.el ends here
