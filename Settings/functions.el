;; Move lines
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))
;; End

;; Makes it so that when you start typing when a region is active,
;; it gets replaced with your text.
(delete-selection-mode t)

;; Enlarge or shrink windows
(defun v-resize (key)
  "interactively resize the window"  
  (interactive "cHit [/] to enlarge/shrink") 
  (cond                                  
   ((eq key (string-to-char "["))                      
    (enlarge-window-horizontally 1)             
    (call-interactively 'v-resize)) 
   ((eq key (string-to-char "]"))                      
    (enlarge-window-horizontally -1)            
    (call-interactively 'v-resize)) 
   (t (push key unread-command-events))))

(defun h-resize (key)
  "interactively resize the window"  
  (interactive "cHit [/] to enlarge/shrink") 
  (cond                                  
   ((eq key (string-to-char "["))                      
    (enlarge-window 1)             
    (call-interactively 'h-resize)) 
   ((eq key (string-to-char "]"))                      
    (enlarge-window -1)            
    (call-interactively 'h-resize)) 
   (t (push key unread-command-events))))
;; end

;; magit clone
(defun mclone ()
  (interactive)
  (setq-local username (read-string "Username: "))
  (message "Username: %s" username)
  (sleep-for 0.5)
  (setq-local reponame (read-string "reponame: "))
  (message "Repository: %s" reponame)
  (sleep-for 0.5)
  (setq-local git-url "https://github.com/")
  (if (y-or-n-p (concat git-url username "/" reponame))
      (magit-clone (concat git-url username "/" reponame) (concat default-directory "/" reponame) )))  
;; end

;; smart comment
(add-to-list 'load-path "~/emacs.d")
(require 'smart-comment)
;; end
