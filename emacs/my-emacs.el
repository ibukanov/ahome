;;; local emacs configuration -*- lexical-binding: t; -*-

(push "~/a/emacs" load-path)
(push "~/a/emacs/external" load-path)
(push "~/p/emacs/purescript-mode" load-path)

(load "my-emacs-lib")

(require 'cl)

;;quilt support
;(require 'quilt)

;;; Spellers in order of preference
(let ((spellers '("hunspell" "aspell" "myspell" "ispell")))
  (while (and spellers
	      (not (let ((path (executable-find (car spellers))))
		     (and path (setq-default ispell-program-name path)))))
    (setq spellers (cdr spellers))))

;;; Version management

; Disable it, shell command works better for me
(setq vc-handled-backends nil)

;;; Remote access

(setq tramp-default-method "ssh")
;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;; Backup

(defconst my-backup-dir-path "~/.emacs.d/backup")
(setq backup-directory-alist `((".*" . ,my-backup-dir-path)))
(setq auto-save-file-name-transforms `((".*" ,my-backup-dir-path t)))

(setq create-lockfiles nil) ; works since emacs 24.3

; I prefer to see changes in files immediately
(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)

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
   "runit-java"
   '("personal"
     (c-offsets-alist
      . ((case-label . 0)
	 (statement-case-intro . +)
	 (statement-case-open . 0)))))

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

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
)

(defun runit-file-p (file-path)
  (string-match  "\\<\\(SMODL\\|Carassius\\|sds\\)/.*" file-path))

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
     ((runit-file-p file-path)
      (setq enable-local-variables nil
	    enable-local-eval nil
	    tab-width 4
	    indent-tabs-mode 't)

      (c-set-style "runit-java"))
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
     ((string-match
       "/s/x\.cpp" file-path)
      (c-set-style "c-indent2"))
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
  (when (fboundp 'flyspell-prog-mode)
    (flyspell-prog-mode))
)

(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;;; js-mode customization
(defun my-js-initialization-hook ()
  (let ((file-path (buffer-file-name)))
    (cond
     ((runit-file-p file-path)
      (setq tab-width 4)
      (setq indent-tabs-mode 't))
     ((string-match  "/closure/" file-path)
      (setq tab-width 2)
      (setq js-indent-level 2)
      (setq indent-tabs-mode nil))
     ('t
      (setq tab-width 8)
      (setq indent-tabs-mode nil)))))

(add-hook 'js-mode-hook 'my-js-initialization-hook)

(defun my-shell-mode-hook ()
  (setq sh-indentation 8)
  (setq sh-basic-offset 8)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  (setq indent-tab-mode 't))

;;; shell
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

;;; php
;(autoload 'php-mode "php-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)

;;; rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;; typescript
;; If use bundled typescript.el,
(autoload 'typescript-mode "typescript" nil t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;(require 'tss)

;;; Erlang
(autoload 'erlang-mode "erlang" nil t)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;;; purescript
(require 'purescript-mode-autoloads nil t)
(add-to-list 'Info-default-directory-list "~/p/emacs/purescript-mode/")

;;; Golang
(require 'go-mode-autoloads nil t)

(defun my-golang-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode 1))

(add-hook 'go-mode-hook 'my-golang-hook)

;; Key binding
;(setq tss-popup-help-key "C-:")
;(setq tss-jump-to-definition-key "C->")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
;(tss-config-default)

;;; Shell customization
(defun my-shell-mode-hook ()
  (setq comint-input-ignoredups t)
  (dirtrack-mode t)
  (setq dirtrack-list '("^\\(@[a-zA-Z0-9-]+ ?\\)?\\([~|/].*?\\)>" 2))
  (my-shell-keybindings "shell"))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(defun my-gdb-mode-hook ()
  (my-shell-keybindings "gdb"))
(add-hook 'gdb-mode-hook 'my-gdb-mode-hook)

;;; Erlang mode customization. For now just workaround a misfeature in
;;; the mode that binds erlang-fill-paragraph unconditionally to A-q
;;; rather than to the current bindings of erlang-fill-paragraph.
(defun my-erlang-mode-hook ()
  (local-unset-key "\M-q")
  (local-set-key (vector ?\M-q ?q) 'erlang-fill-paragraph))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;; Eshell customization
(defvar my-eshell-initilized nil)

(defun my-eshell-mode-hook ()
  (when (not my-eshell-initilized)
    (setq eshell-scroll-to-bottom-on-output nil)
    (setq eshell-scroll-show-maximum-output t)
    (setq eshell-output-filter-functions
	  (cons `eshell-postoutput-scroll-to-bottom
		eshell-output-filter-functions))
    (setq my-eshell-initilized t))
  (my-shell-keybindings "eshell")

  ;; Kill too-smart completion that can connect to the server
  (let ((f (intern-soft "pcomplete/cvs")))
    (when f
      (unintern f obarray)))
  )

(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)

(defsubst eshell/e (filename)
  (find-file filename))
;; (defun eshell/p (command &rest args)
;;   (if (file-attributes command)
;;       (find-file command)
;;     (apply 'my-paged-process command args)))

(defun my-term-mode-hook()
  (auto-fill-mode -1)
  (setq tab-width 8)
  ;; Switch the prefix key to C-x to have less suprises
  (term-set-escape-char ?\C-x)
  (define-key term-raw-map "\C-c" 'term-interrupt-subjob)
  (define-key term-raw-escape-map "\C-h" 'help-command)
  (define-key term-raw-escape-map "\C-y" 'term-paste)

  ;; Make C-x C-x in the raw mode to send C-x
  (define-key term-raw-escape-map "\C-x"
    (lambda() (interactive) (term-send-raw-string "\C-x")))

  ;; make sure that ^X^K returns to the char mode
  (define-key term-mode-map "\C-x\C-k" 'term-char-mode)

  ;; Paste command in the line mode switches to raw mode and then paste
  (define-key term-mode-map "\C-y"
    (lambda() (interactive) (term-char-mode) (term-paste)))
)

(add-hook 'term-mode-hook 'my-term-mode-hook)

;;; GUI SETUP

;; Disable initial message
(setq inhibit-startup-message t)

;; Mode line customization

(defconst my-home-dir (subst-char-in-string ?\\ ?/ (getenv "HOME") t))

; show column number
(column-number-mode 1)

(defun my-short-buffer-path ()
  (let ((path (buffer-file-name)))
    (if (not path)
	""
      (let ((l (length my-home-dir)))
	(cond
	 ((not (eq t (compare-strings path 0 l my-home-dir 0 l t)))
	  path)
	 ((= l (length path))
	  "~/")
	 ((not (eq t (compare-strings path l (+ l 1) "/" 0 1 t)))
	  path)
	 (t (substring path (+ l 1))))))))

;; Insert file path if available before the last element of the list
;;; which is - filler by default
(let* ((full-path '(:eval
		    (let ((path (my-short-buffer-path)))
		      (if (= 0 (length path))
			  ""
			(concat " " path " ")))))
       (end (last mode-line-format))
       (my-format-tail (list full-path (car end))))
  ;; my-format-tail is a list of tail customization plus old tail element,
  ;; set mode-line-format end to this new list
  (setcar end (car my-format-tail))
  (setcdr end (cdr my-format-tail)))

;; (require 'uniquify)
;(setq uniquify-buffer-name-style 'post-forward)

;; Scrolling
(setq scroll-preserve-screen-position t)
(setq compilation-scroll-output t)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 10000)

;; Buffer navigation

; Show buffer list in the current buffer
(setq same-window-buffer-names
      (append
       '("*Buffer List*")
       same-window-buffer-names))

(setq Man-notify-method 'pushy)

;; Keyboard

(transient-mark-mode 1)

(setq history-delete-duplicates t)
(setq history-length 1000)
(setq set-mark-command-repeat-pop 1)
(setq undo-limit 2000000)
(setq undo-strong-limit (* undo-limit 2))

; navigating by visual lines
(global-visual-line-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

; clipboard
(defun copy-to-x (start end)
  (interactive "r")
  (let (exe args (need-display nil))
    ;; TODO support xsel
    (cond
     ((setq exe (executable-find "x-clip"))
      (setq need-display t)
      (setq args '("-silent" "-i" "-selection" "clipboard")))
     ((setq exe (executable-find "putclip"))
      (setq args '("--dos"))))
    (cond
     ((not exe)
      (message "Cannot find executable to copy to clipboard"))
     ((and need-display (not (getenv "DISPLAY")))
      (message "DISPLAY variable is not set"))
     ((let ((exit-asap-buffer-name
	     (if (>= emacs-major-version 22) 0 nil)))
	(apply 'call-process-region
	       start end exe nil exit-asap-buffer-name nil args))))))

(defun cygwin-copy-to-clipboard (txt &optional push)
  (let ((process-connection-type nil))
    (let ((p (start-process "putclip" nil "putclip")))
      (process-send-string p txt)
      (process-send-eof p))))

(when (and (not interprogram-cut-function) (executable-find "putclip"))
  (setq interprogram-cut-function 'cygwin-copy-to-clipboard))

(setq x-select-enable-clipboard t)

(when (not window-system)
  (load "osc52.el")
  (osc52-set-cut-function))

; Activate standard copy-paste
(cua-mode t)

(defvar user-keys-mode-map (make-sparse-keymap) "user-keys-mode keymap.")

(defun user-keys--empty-command ()
  "do-nothing action to remove a default keybinding"
  (interactive))

(defvar user-keys-prefix "M-q" "Custom keybindings prefix in `kbd' format")

(defun user-keys-reset ()
  "Empty the mode keymap to remove all custom keybindings."
  (setcdr user-keys-mode-map nil))

(defun user-keys-disable (key-string)
  (define-key user-keys-mode-map (kbd key-string) 'user-keys--empty-command))

(defun user-keys-add (key-string command)
  (define-key user-keys-mode-map (kbd key-string) command))

(defun user-keys-make-command-wrap (lookup)
  (lambda (&rest args)
    "Wraped command"
    (interactive (advice-eval-interactive-spec
		  (let ((cmd (funcall lookup)))
		    (when cmd (cadr (interactive-form cmd))))))
    (let ((cmd (funcall lookup)))
      (when cmd (apply cmd args)))))

(defun user-keys-alias (global-key-string alias-key-string)
  (let ((global-key (kbd global-key-string)))
    (define-key
      user-keys-mode-map
      (kbd alias-key-string)
      (user-keys-make-command-wrap
       (lambda () (lookup-key (current-global-map) global-key))))))

(defun user-keys-subkey (subkey-string command)
  (define-key user-keys-mode-map (vconcat (kbd user-keys-prefix) (kbd subkey-string)) command))

(defun user-keys-find-overwritten (key-string)
  (let* ((key (kbd key-string))
	 (local-map (current-local-map))
	 (cmd (and local-map (lookup-key local-map key))))
    (or cmd (lookup-key (current-global-map) key))))

(defun user-keys-overwritten-prefix-command ()
  (user-keys-make-command-wrap (lambda () (user-keys-find-overwritten user-keys-prefix))))

(define-minor-mode user-keys-mode
  "A minor mode that makes user-defined keys a priority over mode-specific or global keybindings."
  :global t
  :lighter "")

;(define-globalized-minor-mode user-keys-mode user-keys-global-mode user-keys-mode)

(add-to-list 'emulation-mode-map-alists `((user-keys-mode . ,user-keys-mode-map)))

(user-keys-mode t)

(progn
  (user-keys-reset)
  
  (setcdr user-keys-mode-map nil)

  ;; As Cua takes C-v, provide an alternative for scrolling. For now I
  ;; use C-p/C-n as those are bound to line movements by default and
  ;; so far keyboards always comes with arrow keys.
  (user-keys-alias "<prior>" "C-p")
  (user-keys-alias "<next>" "C-n")

  ;; C-space may not be available in some window managers, so use
  ;; Alt-Space for marking as well.
  (user-keys-alias "C-SPC" "M-SPC")

  (user-keys-subkey "s" 'my-2-window-split)

  ;; Bindings to call the action overriden with user-keys-prefix
  (user-keys-subkey "q" (user-keys-overwritten-prefix-command))

  (user-keys-subkey "c" 'compile)
  (user-keys-subkey "e" 'eshell)
  (user-keys-subkey "g" 'grep)
  (user-keys-subkey "r" 'rgrep)

  ;; C-x k <enter> is too long for buffer kill, so make "prefix k" as
  ;; shortcut.
  ;;
  ;; (user-keys-subkey "k" (lambda () (interactive) (kill-buffer nil)))

  (user-keys-subkey "t" (lambda () (interactive) (my-run-terminal "term")))

  (mapc (lambda (subkey-string)
	  (let ((shell-buffer-name (concat "shell_" subkey-string)))
	    (user-keys-subkey
	     subkey-string
	     (lambda () (interactive) (my-run-shell shell-buffer-name)))))
	'("1" "2" "3" "4" "5" "6"))

  ;; It is way too easy to type this accidentally
  (user-keys-disable "<insert>")
  (user-keys-disable "<insertchar>")

  ;; TODO use what is <insert> is bound to currently
  (user-keys-subkey
   "<insert>"
   (user-keys-make-command-wrap (lambda () (user-keys-find-overwritten "<insert>"))))


  ;; Remove the binding for keyboard-escape-quit. It is way too easy
  ;; to trigger it via autorepeat of the escape key.
  (user-keys-disable (kbd "ESC ESC ESC"))

  (user-keys-subkey
   "i"
   (user-keys-make-command-wrap (lambda () (and (fboundp 'ispell-region) 'ispell-region))))

  (user-keys-subkey "f" 'my-toggle-fullscreen)

  )

; Dynamic abbrev should copy the word it finds verbatim
(setq dabbrev-case-replace nil)

(when (fboundp 'ido-mode)
  (ido-mode 'buffers))

; Ignore accidental hiting of insert
;(global-unset-key (kbd "<insert>"))
;(global-unset-key (kbd "<insertchar>"))

; Remove the binding for keyboard-escape-quit. It is way too easy to trigger
; it via autorepeat of the escape key.
(global-unset-key (kbd "ESC ESC ESC"))


; Bindings for the debugger
(let ((dbg-prefix 'f5))
  (global-unset-key (vector dbg-prefix))
  (mapcar (lambda (pair)
	    (global-set-key (vector dbg-prefix (car pair)) (cdr pair)))
	  '((?b . gud-break)
	    (?d . gud-remove)
	    (?f . gud-finish)
	    (?j . gud-jump)
	    (?l . gud-refresh)
	    (?n . gud-next)
	    (?p . gud-print)
	    (?r . gud-cont)
	    (?s . gud-step)
	    (?t . gud-tbreak)
	    (?u . gud-until)
	    (?w . gud-watch)
	    (?< . gud-up)
	    (?> . gud-down))))

; Allow to use M-<left> to restore the previous window configuration
;(when (fboundp 'winner-mode)
;  (setq winner-dont-bind-my-keys 1)
;  (winner-mode 1)
;)

;; Context-dependent defaults for find-find etc.
;(when (fboundp 'ffap-bindings)
;  (ffap-bindings))

(if window-system
    (progn
      ;; This is necessary to get normal Backspace and Delete operation
      ;; in all cases under x-windows
      (normal-erase-is-backspace-mode 1)

      ;; Deal with Emacs weirdness so C-S-tab always works
      ;; (define-key local-function-key-map [C-S-iso-lefttab] [C-S-tab])
      )

  ;; Do some substitutions to enable various function keys to work under
  ;; customized xterm as they do in X-Windows client.
  ;; TODO: check for the standard for C-home/C-end

  (substitute-key-definition [deletechar] "\C-d" function-key-map)
  (substitute-key-definition [insertchar] [insert] function-key-map)
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[5;5~" [C-prior])
  (define-key function-key-map "\e[6;5~" [C-next])
  (define-key function-key-map "\e[1;5~" [C-home])
  (define-key function-key-map "\e[4;5~" [C-end])
  (define-key function-key-map "\e[4;5~" [C-end])
  (define-key function-key-map (kbd "C-^") [C-return])

  ;; Cygwin's mintty
  ;; (define-key function-key-map "\e[1;5I" [C-tab])
  ;; (define-key function-key-map "\e[1;6I" [C-S-tab])
)

(defun my-shell-keybindings(name)
  (cond
   ((equal name "eshell")
    (local-set-key [?\C-p] 'eshell-previous-input)
    (local-set-key [?\C-n] 'eshell-next-input)
    (local-set-key [up] 'previous-line)
    (local-set-key [down] 'next-line)
    ; smart home: move before the prompt first
    (local-set-key [remap move-beginning-of-line]
		   (lambda ()
		     (interactive)
		     (let ((saved-column (current-column)))
		       (eshell-bol)
		       (when (= saved-column (current-column))
			 (beginning-of-line))))))
   ((or (equal name "shell") (equal name "gdb"))
    (local-set-key [?\e ?p] 'comint-previous-matching-input-from-input)
    (local-set-key [?\e ?n] 'comint-next-matching-input-from-input))
   ))

;; Window management
(setq truncate-partial-width-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold 250)

;(add-hook 'after-make-frame-functions 'my-on-new-frame-hook)

;; No blinking cursors
(blink-cursor-mode 0)
(menu-bar-mode 0)
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;(desktop-load-default)
;(desktop-read)

;; disable bell
(setq ring-bell-function 'ignore)

(when (fboundp 'savehist-mode)
  (savehist-mode 1))

(load "my-faces")

(defun my-initial-window-config(frame kind)

  (let ((tty (frame-parameter frame 'tty)))

    (when (and (not tty) (equal (getenv "DESKTOP_SESSION") "xfce"))
      (set-frame-parameter frame 'fullscreen 'maximized))

    ;; Let terminal to decide the background color
    (when tty (set-face-background 'default "unspecified-bg" frame))

     (message "Initial window config frame-width=%s frame-height=%s"
	      (frame-width frame) (frame-height frame))
     (if (>= (frame-width frame) 140)
	 (my-2-window-split)
       (delete-other-windows (frame-selected-window frame))))
)

(add-to-list
 'after-make-frame-functions
 (lambda (frame)
   (run-at-time
    (if (frame-parameter frame 'tty) "50 milliseconds" "300 milliseconds")
    nil 'my-initial-window-config frame 0)))

;; Setup the initial frame if it has a windows system or tty and then
;; start the emacs server. For the window GUI we delay initialization
;; to to apply the setup after frame's GUI settles. For tty we can do
;; the setup immediately.
;;
;; nil value of window-system does not imply tty. When emacs starts in
;; deamon mode it creates pseudo-frame without window system or tty.

(let ((frame (selected-frame))
      (daemon-initialization nil))
  (cond ((frame-parameter frame 'window-system)
	 (run-at-time "1600 milliseconds" nil 'my-initial-window-config frame 0))
	((frame-parameter frame 'tty)
	 (my-initial-window-config frame 0))
	(t (setq daemon-initialization t)))

  (unless daemon-initialization
    (require 'server)
    (unless (my-server-running-p)
      (server-start))))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t t)

(require 'package)
(package-initialize)

;; This is for testing - normally pc-bufsw should be loaded as a package
;;(push "~/p/pc-bufsw" load-path)
;;(require 'pc-bufsw-autoloads)
;;(pc-bufsw-default-keybindings)
;;(global-set-key [f6] 'pc-bufsw-mru)
;;(global-set-key [f5] 'pc-bufsw-lru)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
