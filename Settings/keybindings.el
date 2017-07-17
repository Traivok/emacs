;; window resize
(global-set-key (kbd "C-c w v") 'v-resize)
(global-set-key (kbd "C-c w h") 'h-resize)
;; end

;; C/C++ completion
(require 'cc-mode)
(define-key c-mode-base-map [?\M-\r] 'company-complete)
;; end

;; Move lines
(global-set-key [\C-\S-up] 'move-text-up)
(global-set-key [\C-\S-down] 'move-text-down)
;; end

;; smart comment
(global-set-key (kbd "M-;") 'smart-comment)
;; end
