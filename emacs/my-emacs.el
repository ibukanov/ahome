;;; local emacs configuration -*- lexical-binding: t; -*-

(let ((this-file-dir (file-name-directory load-file-name)))

  (require 'package)
  (let ((melpa
	 (if nil '("melpa" . "https://melpa.org/packages/")
	   '("melpa-stable" . "https://stable.melpa.org/packages/"))))
    (add-to-list 'package-archives melpa t))
  
  (package-initialize)

  (load (concat this-file-dir "uset.el"))
  (uset-init)

  (load custom-file t t)
  )






;;; Customizations for all modes in CC Mode.
(defun my-c-initialization-hook ()

  (c-add-style
   "personal"
   '((c-tab-always-indent        . t)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist
      . ((substatement-open after)
	 (brace-list-open)))
     (c-hanging-colons-alist
      . ((member-init-intro before)
	 (inher-intro)
	 (case-label after)
	 (label after)
	 (access-label after)))
     (c-cleanup-list
      . (scope-operator
	 empty-defun-braces
	 defun-close-semi))
     (c-offsets-alist
      . ((arglist-close . c-lineup-arglist)
	 (inline-open . 0)
	 (substatement-open . 0)
	 (case-label        . +)
	 (block-open        . 0)
	 (case-label . *)
	 (statement-case-intro . *)
	 (statement-case-open . 0)
	 (statement-cont . (c-lineup-assignments +))
	 (arglist-cont . c-lineup-argcont)
	 (innamespace . 0)
	 (access-label . /)
	 (member-init-intro . *)
				    ))
     (c-echo-syntactic-information-p . t)
     (fill-column . 78)))

  (c-add-style
   "c-tabs8"
   '("personal"
     (c-basic-offset . 8)
     (indent-tabs-mode . t)
     (c-offsets-alist
      . ((case-label . 0)
	 (label . 0)
	 (statement-case-intro . +)
	 (statement-case-open . +)))))

  (c-add-style
   "c-indent2"
   '("personal"
     (c-basic-offset . 2)))

  ;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
)

(defun my-c-mode-common-hook ()

  (local-unset-key "\M-q")
  (local-set-key (vector ?\M-q ?q) 'c-fill-paragraph)

  ;; add my personal style and set it for the current buffer

  (c-set-style "personal")

;; other customizations
  (setq tab-width 8)
  ;; this will make sure spaces are used instead of tabs
  (setq indent-tabs-mode nil)
  (let ((file-path (buffer-file-name)))
    (cond
     ((string-match
       "\\<\\(mosh\\)/.*\\.\\(cc\\|h\\)" file-path)
      (setq c-basic-offset 2)
      (setq indent-tabs-mode 't))
     ((string-match
       "\\<\\(closure-compiler\\)/.*\\.java" file-path)
      (setq c-basic-offset 2)
      (setq indent-tabs-mode nil))
     ((string-match
       "/\\(netcat-openbsd[-.0-9a-z]*\\|nc\\)/.*\\.\\(cc?\\|h\\)" file-path)
      (c-set-style "c-tabs8"))
     ((string-match "/firejail/" file-path) (c-set-style "c-tabs8"))
     ((string-match "/s/x\.cpp" file-path) (c-set-style "c-indent2"))
     ))

  ;; I like hungry-delete but dislike automatic new lines
  (c-toggle-hungry-state 1)
  (c-toggle-auto-state -1)
  (setq c-backslash-column 78)
  (abbrev-mode -1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  ;; <enter> and newline and ident
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
)

;(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;;; js-mode customization
(defun my-js-initialization-hook ()
  (let ((file-path (buffer-file-name)))
    (cond
     ((string-match  "/closure/" file-path)
      (setq tab-width 2)
      (setq js-indent-level 2)
      (setq indent-tabs-mode nil))
     ('t
      (setq tab-width 8)
      (setq indent-tabs-mode nil)))))

;(add-hook 'js-mode-hook 'my-js-initialization-hook)

(defun my-shell-mode-hook ()
  (setq sh-indentation 8)
  (setq sh-basic-offset 8)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  (setq indent-tab-mode 't))

;;; shell
;(add-hook 'sh-mode-hook 'my-shell-mode-hook)

;;; php
;(autoload 'php-mode "php-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;;; rust
;(autoload 'rust-mode "rust-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;; typescript
;; If use bundled typescript.el,
;(autoload 'typescript-mode "typescript" nil t)
;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;(require 'tss)

;;; Erlang
;(autoload 'erlang-mode "erlang" nil t)
;(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;;; purescript
;(require 'purescript-mode-autoloads nil t)
;(add-to-list 'Info-default-directory-list "~/p/emacs/purescript-mode/")

;;; Golang
;(require 'go-mode-autoloads nil t)

(defun my-golang-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode 1))

;(add-hook 'go-mode-hook 'my-golang-hook)

;; Key binding
;(setq tss-popup-help-key "C-:")
;(setq tss-jump-to-definition-key "C->")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
;(tss-config-default)

;;; Erlang mode customization. For now just workaround a misfeature in
;;; the mode that binds erlang-fill-paragraph unconditionally to A-q
;;; rather than to the current bindings of erlang-fill-paragraph.
(defun my-erlang-mode-hook ()
  (local-unset-key "\M-q")
  (local-set-key (vector ?\M-q ?q) 'erlang-fill-paragraph))

;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)





