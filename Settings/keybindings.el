;; window resize
(global-set-key (kbd "C-c w h") 'h-resize)
(global-set-key (kbd "C-c w v") 'v-resize)
;; end

;; C/C++ completion 
(define-key c-mode-map [?\M-\r] 'company-complete)
(define-key c++-mode-map [?\M-\r] 'company-complete)
;; end

;; Move lines
(global-set-key [\C-\S-up] 'move-text-up)
(global-set-key [\C-\S-down] 'move-text-down)
;; end
