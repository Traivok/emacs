;;; my-functions.el --- my functions

;;; I did not create all these functions
;;; thanks to google, stacks and so on

;;; Commentary:
;;; Functions:
;;; (v/h)-resize, mclone, move-text-(up/down)
;;; rename-file-and-buffer, goto init file and settings directory

;;; Code:

;; Move lines
(defun move-text-internal (arg)
  "If (ARG) up, move the line up, elif (ARG) down, move the line down."
  (cond  ((and mark-active transient-mark-mode)
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
   "Move region (ARG) or current line arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (ARG) or current line  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))
;; End

;; Makes it so that when you start typing when a region is active,
;; it gets replaced with your text.
(delete-selection-mode t)

;; Enlarge or shrink windows
(defun h-resize (key)
  "Interactively resize the window horizontally (KEY) ] to enlarge and [ to shrink."
  (interactive "cHit [/] to enlarge/shrink")
  (cond
   ((eq key (string-to-char "["))
    (enlarge-window-horizontally 1)
    (call-interactively 'h-resize))
   ((eq key (string-to-char "]"))
    (enlarge-window-horizontally -1)
    (call-interactively 'h-resize))
   (t (push key unread-command-events))))

(defun v-resize (key)
  "Interactively resize the window vertically (KEY) ] to enlarge and [ to shrink."
  (interactive "cHit [/] to enlarge/shrink")
  (cond
   ((eq key (string-to-char "["))
    (enlarge-window 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "]"))
    (enlarge-window -1)
    (call-interactively 'v-resize))
   (t (push key unread-command-events))))
;; end

;; magit clone
(defun mclone ()
  "Clone from https://github.com/username/reponame repository to /current/directory/reponame folder."
  (interactive)
  (let* ((username (read-string "Username: "))
	(reponame (read-string "Reponame: "))
	(github-url "https://github.com/")
	(clone-url (concat github-url username "/" reponame))
	(clone-dir (concat default-directory reponame)))
    (if (y-or-n-p clone-url)
    (magit-clone clone-url clone-dir))))
;; end

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
;;end

;; Go to .emacs init and setup dir.
(defun goto-init-file ()
  "Go to init.el file."
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

(defun goto-setup-dir ()
  "Go to Settings/ file."
  (interactive)
  (find-file-existing "~/.emacs.d/Settings/"))
;; end

(provide 'my-functions)
;;; my-functions.el ends here
