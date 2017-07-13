;; company / irony mode
(add-hook 'after-init-hook 'global-company-mode)
(global-flycheck-mode)

(require 'rtags)
(require 'company-rtags)

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
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))

(cmake-ide-setup)

(set-variable 'rtags-path '"~/Development/rtags/bin")

;; autopair
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
	    (define-key c++-mode-map "{" 'electric-pair)))
;; end

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
;; end

;; auto highlighting
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
;; end
