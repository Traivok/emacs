
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(defvar my-package-list nil "All my packages list.")

(setq my-package-list '(cmake-ide cmake-mode company-c-headers irony company-irony company-irony-c-headers ;; c
                                  rtags company-rtags flycheck-irony flycheck-rtags ;; c
                                  midnight org org-ac org-bullets org-alert ;; org
                                  auto-minor-mode ido magit smart-comment yasnippet auto-highlight-symbol ;; utility
                                  rainbow-mode multiple-cursors js2-mode js2-refactor xref-js2 ag web-mode ac-js2
                                  markdown-mode flymd ;; markdown
                                  gruvbox-theme powerline rainbow-delimiters)) ;; themes

(unless package-archive-contents (package-refresh-contents))

;; check if any package of my-package-list isn't installed and install it
(dolist (my-package my-package-list)
  (unless (package-installed-p my-package)
    (package-install my-package)))

;; update packages
(when (not package-archive-contents)
    (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/node-ac/")

(require 'midnight)
(require 'ido)
(require 'web-mode)
(require 'auto-minor-mode)

(setq user-full-name "José Ricardo A. Figueirôa"
      user-mail-address "jraf@cin.ufpe.br")

(setq explicit-shell-file-name "/bin/bash")

(delete-selection-mode t)

(setq backup-directory-alist `(("." . "/home/ricardo/Development/.backup")))
(setq delete-old-versions t
  kept-new-versions 2
  kept-old-versions 2
  version-control t)
(put 'scroll-left 'disabled nil)

(setq midnight-period (* 2 60 60))

(defvar ido-enable-flex-matching t "Flexible string matching.")
(defvar ido-every-where t "Always ido.")

(setq ido-file-extensions-order '(".hpp" ".cpp" ".c" ".el"))  

(ido-mode t)

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(add-to-list 'auto-minor-mode-alist '("\\.css\\'" . rainbow-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

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

(setq x-select-enable-clipboard t)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(setq flymd-output-directory "/home/ricardo/Development/.md-preview")
(setq flymd-close-buffer-delete-temp-files t)

(require 'rainbow-delimiters)
(require 'powerline)
(require 'auto-highlight-symbol)

(add-to-list 'default-frame-alist
             '(font . "xos4 Terminus-12"))

(powerline-center-theme)

(load-theme 'gruvbox-dark-soft t)

;; Add highlight matching parenthesis
(show-paren-mode 1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)  

;; auto highlighting
(global-auto-highlight-symbol-mode t)

;; Display, Troggle, enable...
(define-global-minor-mode my-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'term-mode 'shell-mode 'eshell-mode)))
      (linum-mode t))))

(my-linum-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(defvar linum-format "%d  ")
(toggle-scroll-bar -1)
;; End

;; Display columns
(setq column-number-mode t)

;; no startup msg
(setq inhibit-startup-message t) ; Disable startup message
;; scratch msg
(setq initial-scratch-message ";;;  Evenings, Mornings, And a Couple of Saturdays  ;;;\n\n")

(require 'company)
(require 'flycheck)
(require 'company-c-headers)
(require 'irony)
(require 'company-rtags)
(require 'company-irony)
(require 'rtags)
(require 'company-rtags)
(require 'yasnippet)
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)
(require 'node-ac-mode)

(add-hook 'after-init-hook 'global-company-mode)

(global-flycheck-mode)

(electric-pair-mode t)
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
(add-hook 'c-mode-hook 'c++-mode-hook
          (lambda ()
            (define-key c-mode-map "\"" 'electric-pair)
            (define-key c-mode-map "(" 'electric-pair)
            (define-key c-mode-map "[" 'electric-pair)
            (define-key c-mode-map "{" 'electric-pair)
            (define-key c++-mode-map "\"" 'electric-pair)
            (define-key c++-mode-map "(" 'electric-pair)
            (define-key c++-mode-map "[" 'electric-pair)
            (define-key c++-mode-map "{" 'electric-pair)
            (define-key c++-mode-map "<" 'electric-pair)))

(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")

(yas-global-mode 1)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))  

(add-hook 'flycheck-mode-hook 'flycheck-irony-setup)

(setq irony-additional-clang-options '("-std=c++14" "-Wall" "-Wextra"))
(add-hook 'c++-mode-hook (lambda () (defvar flycheck-gcc-language-standard "c++14" "Set GCC standart to C++14")))
(add-hook 'c++-mode-hook (lambda () (defvar flycheck-clang-language-standard "c++14" "Set Clang standart to C++14")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook 'ac-js2-mode) ;; autocompletion
(add-hook 'js2-mode-hook
          (lambda ()
                    (local-set-key (kbd "C-.") 'node-ac-auto-complete)
                        (local-set-key (kbd "C-c C-d") 'node-ac-show-document)
                        (local-set-key (kbd "C-c C-j") 'node-ac-jump-to-definition)))

(setq ac-js2-evaluate-calls t) ;; evaluate for candidates

(require 'magit)

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

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
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

;; source http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (when (yes-or-no-p (concat "Do you really want to delete " (buffer-file-name)))
    (let ((filename (buffer-file-name)))
      (when filename
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer)))))))

(defun goto-init-file ()
  "Go to init.el file."
  (interactive)
  (find-file-existing "~/.emacs.d/setup.org"))

(defun disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (wterm-other-window 'shell))

(defun eshell-other-window ()
  "Open a 'eshell' instance in a new window."
  (interactive)
  (wterm-other-window 'eshell))

(defun wterm-other-window (wterm)
  "Open some (what term) WTERM in a new window."
  (interactive)
  (let ((buf (funcall wterm))))
  (delete-other-windows))

(defun kill-match-buffer-y (regexp)
  "Kill buffer that its name match with REGEXP."
  (interactive "sKill buffers matching with this regexp WITHOUT asking: ")
  (dolist (bf (buffer-list))
    (let ((bf-name (buffer-name bf)))
      (if (string-match-p regexp bf-name)
          (kill-buffer bf)))))

(require 'org)
(require 'org-ac)
(require 'org-bullets)
(require 'org-alert)
;; more exports options
(require 'ox-md)
(require 'ox-beamer)

(defun org-file-path (filename)
  "Return the absolute address of an org FILENAME, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-directory "~/Org")
(defvar org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))  

(setq org-src-fontify-natively t)

(setq  org-hide-leading-stars t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an 'org-mode' item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise.
Get N-DONE by the org statistics hook.
If N-NOT-DONE = 0, then done, else todo."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-mode-hook
          (lambda () (org-update-statistics-cookies t)))

(defun worf-delete-subtree (arg)
  "Delete subtree or ARG chars."
  (interactive "p")
  (if (and (looking-at "\\*")
           (looking-back "^\\**" (line-beginning-position)))
      (org-cut-subtree)
    (delete-char arg)))

;; define an advice
(defadvice worf-delete-subtree (after my-org-update-parent-todo ())
  (org-update-parent-todo-statistics))
;; activate all advices to this function
(ad-activate 'worf-delete-subtree)

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(org-ac/config-default)

(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (delete '("\\.x?html?\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "qpdfview %s"))
         (add-to-list 'org-file-apps '("\\.x?html?\\'" . "/usr/bin/firefox --new-tab %s"))))
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (delete '("\\.x?html?\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "qpdfview %s"))
         (add-to-list 'org-file-apps '("\\.x?html?\\'" . "/usr/bin/firefox --new-tab %s"))))

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-log-done 'time)
(defun my-org-archive-done-tasks ()
  "Archive done tasks automatically."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;(org-alert-enable)
;(setq alert-default-style 'libnotify)

(require 'org)
(require 'cc-mode)
(require 'magit)

(global-set-key (kbd "C-c w v") 'v-resize)
(global-set-key (kbd "C-c w h") 'h-resize)
;; end

;; C/C++ completion
(define-key c-mode-base-map [?\M-\r] 'company-complete)
(global-set-key (kbd "C-c y") 'yas-insert-snippet)
;; end

;; Irony
(define-key c-mode-base-map (kbd "C-c t") 'irony-get-type)
;; end

;; Goto setup files and folders
(global-set-key (kbd "C-c o f") 'goto-init-file)
(global-set-key (kbd "C-c o d") 'goto-setup-dir)
;; end

;; Rename and Delete files
(global-set-key (kbd "C-c R") 'rename-file-and-buffer)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

;; Magit
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

;;window navigation
(global-set-key (kbd "C-c w j>")
                (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-c w l")
                (lambda () (interactive) (other-window +1)))
;;end

;; fowarding paragrah
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
;; end

;; open shell
(global-set-key (kbd "C-c o s") 'shell-other-window)
(global-set-key (kbd "C-c o e") 'eshell-other-window)

;; select all without losting cursor position
(global-set-key (kbd "C-c m a") (lambda () (interactive)
                                  (point-to-register ?r
                                                     '(file . (buffer-file-name (other-buffer)))
                                                     )
                                (mark-whole-buffer)))

(global-set-key (kbd "C-c m r") (lambda () (interactive)
                                (jump-to-register ?r
                                                     '(file . (buffer-file-name (other-buffer)))
                                                     )))
;; indent all
(global-set-key (kbd "C-c i") (lambda () (interactive)
                                ;; save cursor position
                                (point-to-register ?r
                                                   '(file . (buffer-file-name (other-buffer)))
                                                   )

                                (mark-whole-buffer)
                                (indent-for-tab-command)                                
                                
                                ;; return to cursor position
                                (jump-to-register ?r
                                                  '(file . (buffer-file-name (other-buffer)))
                                                  )))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive) ;; archive completed tasks
(define-key org-mode-map (kbd "C-d") 'worf-delete-subtree) ;; delete subtree and update it's parent
(define-key org-agenda-mode-map (kbd "C-c m") 'org-agenda-month-view)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
