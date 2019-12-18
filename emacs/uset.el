;;; uset - user Emacs setup -*- lexical-binding: t; -*-

(defvar uset--missing-package-list nil
  "List of package that should be installed.")

(defun uset--report-missing-packages ()
  (when uset--missing-package-list
    (nreverse uset--missing-package-list)
    (message
     (concat "Missing packages were detected."
	     " Use the following code to install them:\n"
	     "(progn\n%s)")
     (mapconcat (lambda (package) (format "  (package-install '%s)" package))
		uset--missing-package-list "\n"))
    (setq uset--missing-package-list nil)))

(defun uset-check-package (package)
  (if (package-installed-p package)
      t
    (setq uset--missing-package-list (cons package uset--missing-package-list))
    (add-hook 'after-init-hook 'uset--report-missing-packages)
    nil))

(defun uset--backup-revert-vc ()
  (let ((backup-dir (concat user-emacs-directory "backup"))
	(autosave-dir (concat user-emacs-directory "autosave")))
    (unless (file-directory-p backup-dir)
      (mkdir backup-dir))
    (unless (file-directory-p autosave-dir)
      (mkdir autosave-dir))
    (setq backup-directory-alist `((".*" . ,backup-dir)))
    (setq auto-save-file-name-transforms `((".*" ,autosave-dir t))))

  (setq create-lockfiles nil) ; works since emacs 24.3

  ;; See changes in files and directories immediately
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  (setq vc-handled-backends '(Git Hg SVN))
  
  (unless (uset-check-package 'magit)
    (autoload 'magit-status "magit"))
  (global-set-key (kbd "C-x g") 'magit-status))

;;; Shell customization

(defun uset--add-proc-output (proc str)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (setq buffer-read-only nil)
      (insert str))))

(defun uset--add-sentinel (proc str)
  (uset--add-proc-output proc (concat "----- " str)))

(defun uset--run-process (command &rest args)
  (uset--run-process-impl default-directory command args))

(defun uset--run-dir-process (dir command &rest args)
  "Show the output of the given command in a buffer executing the command from
the given directory. If there is no args and command is a non-executable file,
then just show that file."
  (if args
      (uset--run-process-impl dir command args)
    (let ((f (expand-file-name command dir)))
      (if (and (file-readable-p f) (not (file-executable-p f)))
	  (find-file f)
	(uset--run-process-impl dir command args)))))

(defun uset--run-process-impl (command-dir command args)
  (let* ((outname (concat "*" command " " (if args (car args) "output") "*"))
	 (outbuf (get-buffer-create outname)))
    (with-current-buffer outbuf
      (let ((old-proc (get-buffer-process (current-buffer))))
	(when old-proc
	  (if (or (not (eq (process-status old-proc) 'run))
		  (yes-or-no-p
		   (format "A %s process is running; kill it? "
			   (process-name old-proc))))
	      (condition-case ()
		  (progn
		    (interrupt-process old-proc)
		    (sit-for 1)
		    (delete-process old-proc))
		(error nil))
	    (error "Cannot have two processes in `%s' at once"
		   (buffer-name))
	    )))
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))
      (setq default-directory command-dir)
      (set-buffer-modified-p nil)
      (set-buffer outbuf)
      (insert command-dir "> " command)
      (let ((cursor args))
	(while cursor
	  (insert " " (car cursor))
	  (setq cursor (cdr cursor))))
      (newline))
    (switch-to-buffer-other-window outbuf)
    (let ((outwin (get-buffer-window outbuf)))
      (set-window-start outwin (point-min))
      (set-window-point outwin (point-min)))
    (let* ((process-environment
	    (append
	     (if (and (boundp 'system-uses-terminfo)
		      system-uses-terminfo)
		 (list "TERM=dumb" "TERMCAP="
		       (format "COLUMNS=%d" (window-width)))
	       (list "TERM=emacs"
		     (format "TERMCAP=emacs:co#%d:tc=unknown:"
			     (window-width))))
	     (cons "PAGER=cat" (cons "EMACS=t" process-environment))))
	   (proc (apply 'start-process command outbuf command args)))
      (set-process-filter proc 'uset--add-proc-output)
      (set-process-sentinel proc 'uset--add-sentinel))))

(defun uset--run-term (terminal-name)
  (let* ((terminal-buffer-name (concat "*" terminal-name "*"))
	 (tbuf (get-buffer terminal-buffer-name))
	 (program (getenv "SHELL")))
    (when (not tbuf)
      ;; The term function uses hardcoded terminal buffer name. As a
      ;; workaround I expand its body here. Also I set default-directory
      ;; so new shells always starts in the home dir.
      (let ((default-directory (concat (getenv "HOME") "/")))
	(set-buffer (make-term terminal-name program))
	(term-mode)
	(term-char-mode)
	(setq tbuf (get-buffer terminal-buffer-name))))
    (switch-to-buffer tbuf)))

(defun uset--run-shell (name)
  ;; Make sure that the shell will appear in the current window
  (setq name (concat "*" name "*"))
  (let ((buf (get-buffer name)))
    (when (not buf)
      (setq buf (get-buffer-create name)))
    (select-window (display-buffer buf))
    (shell buf)))

(defun uset--shell-keybindings(name)
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

(defun uset--shell-hook ()
  (setq comint-input-ignoredups t)
  (dirtrack-mode t)
  (setq dirtrack-list '("^\\(@[a-zA-Z0-9-]+ ?\\)?\\([~|/].*?\\)>" 2))
  (uset--shell-keybindings "shell"))


;;; Eshell customization
(defun uset--eshell-hook ()
  (setq eshell-scroll-to-bottom-on-output nil)
  (setq eshell-scroll-show-maximum-output t)
  (setq eshell-output-filter-functions
	(cons `eshell-postoutput-scroll-to-bottom
	      eshell-output-filter-functions))
  (uset--shell-keybindings "eshell")

  ;; Kill too-smart completion that can connect to the server
  (let ((f (intern-soft "pcomplete/cvs")))
    (when f
      (unintern f obarray))))

(defsubst eshell/e (filename)
  (find-file filename))
;; (defun eshell/p (command &rest args)
;;   (if (file-attributes command)
;;       (find-file command)
;;     (apply 'uset--run-process command args)))

(defun u-ssetup--gdb-hook ()
  (uset--shell-keybindings "gdb"))

(defun uset--term-hook ()
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
    (lambda() (interactive) (term-char-mode) (term-paste))))

(defun uset--init-shells ()
  (add-hook 'shell-mode-hook 'uset--shell-hook)
  (add-hook 'eshell-mode-hook 'uset--eshell-hook)
  (add-hook 'gdb-mode-hook 'u-ssetup--gdb-hook)
  (add-hook 'term-mode-hook 'uset--term-hook))

;;; Common programming hook
(defun uset--common-programming-hook ()
  (when (fboundp 'flyspell-prog-mode)
    (flyspell-prog-mode))
  (show-paren-mode t))

(defun uset--sh-mode-hook ()
  (setq sh-basic-offset 2)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  (setq sh-indent-after-continuation 'always)
  (setq sh-indent-for-continuation '++)
  (setq indent-tab-mode nil))
  
(defun uset--init-ocaml ()
  (uset-check-package 'tuareg)
  )

(defun uset--init-ptogramming ()
  (add-hook 'prog-mode-hook 'uset--common-programming-hook)

  ;; At the tail to run after the default value
  (add-hook 'sh-mode-hook 'uset--sh-mode-hook t)

  (uset--init-ocaml)

  )

;;; GUI SETUP

(defvar uset--main-prefix "M-q" "Custom keybindings prefix in `kbd' format")
(defvar uset--debug-prefix "<f5>" "Prefix for debug keybindings in `kbd' format")

(defvar uset-keys-mode-map (make-sparse-keymap) "user-defined global keymap.")

(define-minor-mode uset-keys-mode
  "A minor mode that makes user-defined keys a priority over mode-specific or global keybindings."
  :keymap 'uset-keys-mode-map
  :global t
  :lighter "")

(defun uset--empty-command ()
  "do-nothing action to remove a default keybinding"
  (interactive))

(defun uset--disable-key (key-string)
  (define-key uset-keys-mode-map (kbd key-string) 'uset--empty-command))

(defun uset--add-key (key-string command)
  (define-key uset-keys-mode-map (kbd key-string) command))

(defun uset--wrap-command (lookup)
  (lambda (&rest args)
    "Lookup and call a function interactively.  The lookup should
be a function that return an interactive function. Typically the
lookup calls lookup-keys etc. to get the current bindings for a
particular key that `uset-keys-mode-map` hides or overrides."
    (interactive (let ((cmd (funcall lookup)))
		   (when cmd
		     (let ((interactive-spec (cadr (interactive-form cmd))))
		       (when interactive-spec
			 (advice-eval-interactive-spec interactive-spec))))))
    (let ((cmd (funcall lookup)))
      (when cmd (apply cmd args)))))

(defun uset--alias-key (global-key-string alias-key-string)
  (let ((global-key (kbd global-key-string)))
    (define-key
      uset-keys-mode-map
      (kbd alias-key-string)
      (uset--wrap-command
       (lambda () (key-binding global-key))))))

(defun uset--add-subkey (subkey-string command)
  (define-key uset-keys-mode-map (vconcat (kbd uset--main-prefix) (kbd subkey-string)) command))

(defun uset--add-debug-key (subkey-command-cons)
  (let ((key (vconcat (kbd uset--debug-prefix) (kbd (car subkey-command-cons)))))
    (define-key uset-keys-mode-map key (cdr subkey-command-cons))))

(defun uset--find-default-command (key-string)
  (let* ((key (kbd key-string))
	 (local-map (current-local-map))
	 (cmd (and local-map (lookup-key local-map key))))
    (or cmd (lookup-key (current-global-map) key))))

(defun uset--init-keyboard ()
  (setcdr uset-keys-mode-map nil)

  ;; As Cua takes C-v, provide an alternative for scrolling. For now I
  ;; use C-p/C-n as those are bound to line movements by default and
  ;; so far keyboards always comes with arrow keys.
  (uset--alias-key "<prior>" "C-p")
  (uset--alias-key "<next>" "C-n")

  ;; C-space may not be available in some window managers, so use
  ;; Alt-Space for marking as well.
  ;;(uset--alias-key "C-SPC" "M-SPC")

  (uset--add-subkey "s" 'uset--2-window-split)

  ;; Bindings to call the action overriden with uset--main-prefix
  (uset--add-subkey
   "q"
   (uset--wrap-command
    (lambda () (uset--find-default-command uset--main-prefix))))

  (uset--add-subkey "c" 'compile)
  (uset--add-subkey "e" 'eshell)
  (uset--add-subkey "g" 'grep)
  (uset--add-subkey "r" 'rgrep)

  ;; C-x k <enter> is too long for buffer kill, so make "prefix k" as
  ;; shortcut.
  (when nil
    (uset--add-subkey "k" (lambda () (interactive) (kill-buffer nil))))
  
  (uset--add-subkey "t" (lambda () (interactive) (uset--run-term "term")))

  (mapc (lambda (subkey-string)
	  (let ((shell-buffer-name (concat "shell_" subkey-string)))
	    (uset--add-subkey
	     subkey-string
	     (lambda () (interactive) (uset--run-shell shell-buffer-name)))))
	'("1" "2" "3" "4" "5" "6"))

  ;; It is way too easy to type insert accidentally, so rebind as subkey
  ;; and remove the global
  (uset--add-subkey
   "<insert>"
   (uset--wrap-command
    (lambda () (uset--find-default-command "<insert>"))))

  (uset--disable-key "<insert>")
  (uset--disable-key "<insertchar>")

  ;; Remove the binding for keyboard-escape-quit. It is way too easy
  ;; to trigger it via autorepeat of the escape key.
  (uset--disable-key (kbd "ESC ESC ESC"))

  (uset--add-subkey
   "i"
   (uset--wrap-command
    (lambda () (and (fboundp 'ispell-region) 'ispell-region))))

  ;; Bindings for the debugger
  (mapc 'uset--add-debug-key
	'(("b" . gud-break)
	  ("d" . gud-remove)
	  ("f" . gud-finish)
	  ("j" . gud-jump)
	  ("l" . gud-refresh)
	  ("n" . gud-next)
	  ("p" . gud-print)
	  ("r" . gud-cont)
	  ("s" . gud-step)
	  ("t" . gud-tbreak)
	  ("u" . gud-until)
	  ("w" . gud-watch)
	  ("<" . gud-up)
	  (">" . gud-down)))

  (add-to-list 'emulation-mode-map-alists
	       `((uset-keys-mode . ,uset-keys-mode-map)))

  (uset-keys-mode t)

  ;; Use Emacs default cut-and-paste but show the region when it is active.
  (cua-mode nil)
  (cua-selection-mode nil)
  (transient-mark-mode 1)

  ;; Map Ctrl-z to the undo. Note that the default action for Ctrl-z
  ;; is also bound to C-x C-z, so use that to suspends Emacs frames.
  (uset--alias-key "C-/" "C-z")

  (setq set-mark-command-repeat-pop 1)
  (setq undo-limit 2000000)
  (setq undo-strong-limit (* undo-limit 2))

  ;; Dynamic abbrev should copy the word it finds verbatim
  (setq dabbrev-case-replace nil)

  ;; Allow to use M-<left> to restore the previous window configuration
  ;;(when (fboundp 'winner-mode)
  ;;  (setq winner-dont-bind-my-keys 1)
  ;;  (winner-mode 1)
  ;;)

  ;; Context-dependent defaults for find-find etc.
  ;;(when (fboundp 'ffap-bindings)
  ;;  (ffap-bindings))

  (if window-system
      (progn
	;; This is necessary to get normal Backspace and Delete operation
	;; in all cases under x-windows
	(normal-erase-is-backspace-mode 1)

	;; Deal with weird key maps
	(substitute-key-definition [C-S-iso-lefttab] [C-S-tab] function-key-map)
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
    ))

(defun uset--init-buffers ()
  ;; Show buffer list in the current buffer
  (setq same-window-buffer-names
	(cons "*Buffer List*" same-window-buffer-names))
  (setq Man-notify-method 'pushy)

  ;; (require 'uniquify)
  ;; (setq uniquify-buffer-name-style 'post-forward)

  (when (fboundp 'ido-mode)
    (ido-mode 'buffers))

  (setq pc-bufsw-other-windows :skip)
  (when (uset-check-package 'pc-bufsw)
    (pc-bufsw t)))

;; Mode line customization

(defconst uset--modeline-home (subst-char-in-string ?\\ ?/ (getenv "HOME") t))

(defun uset--get-modeline-path ()
  (let ((path (buffer-file-name)))
    (if (not path)
	(setq path "")
      (let ((l (length uset--modeline-home)))
	(when (eq t (compare-strings path 0 l uset--modeline-home 0 l t))
	  (if (= l (length path))
	      (setq path "~/")
	    (setq path (substring path (+ l 1)))))))
    (if (string= "" path)
	""
      (concat " " path " "))))

(defun uset--init-modeline ()
  ;; Insert file path if available before the last element of the list
  ;; which is - filler by default
  (let* ((full-path '(:eval (uset--get-modeline-path)))
	 (last-cons (last mode-line-format))
	 (last-format (car last-cons)))
    (setcar last-cons full-path)
    (setcdr last-cons (cons last-format nil)))
  (column-number-mode 1))

(defun uset--is-daemon-startup-frame(frame)
  (not (or (frame-parameter frame 'window-system)
	   (frame-parameter frame 'tty) nil)))

(defun uset--split-window-in-two ()
  "Split window sensibly in two.

Assuming the frame has a single window, split it in two
horizontally with the left window getting at least 80 colons. If
the frame is too narrow for sensible horizontal split, split
vertically.

Return the new window.
"
  (let*
      ((window (selected-window))
       (frame (window-frame window))
       (width (frame-width frame))
       (decoration-width (if window-system 3 2))
       (usable-width (max 1 (- width decoration-width)))
       (new-window
	(if (< usable-width 140)
	    (split-window-vertically)
	  (let ((left-width (+ 80 decoration-width)))
	    (when (> usable-width (+ 160 decoration-width))
	      (setq left-width (min (+ 100 decoration-width) (- usable-width 80))))
	    (split-window-horizontally left-width))))
       ;; Show the scratch buffer in the new window for now
       (new-window-buffer (get-buffer "*scratch*")))
    (when new-window-buffer
      (set-window-buffer new-window new-window-buffer))))
    
(defun uset--2-window-split ()
  (interactive)
  (delete-other-windows)
  (uset--split-window-in-two))

(defun uset--initial-window-config(frame)
  (let ((tty (frame-parameter frame 'tty)))
    ;; Let terminal to decide the background color
    (when tty
      (set-face-background 'default "unspecified-bg" frame))
    (when nil
      (message "Initial window config frame-width=%s frame-height=%s"
	       (frame-width frame) (frame-height frame))
      (if (>= (frame-width frame) 140)
	  (uset--2-window-split)
	(delete-other-windows (frame-selected-window frame))))
    ))

(defun uset--get-main-source-window ()
  "Return the window that should be primary used for sources"
  (let ((window (window-at 0 0)))
    ;; Avoid minibuffers
    (if (window-minibuffer-p window)
	(get-mru-window)
      window)))

(defun uset--file-like-buffer (buffer)
  (or (buffer-file-name buffer)
      (let ((name (buffer-name buffer)))
	(string-equal name "*Locate*"))))

(defun uset--get-window-to-show-buffer (buffer)
  "Select or create window to show a buffer without window."
  (cond
   ((uset--file-like-buffer buffer)
    (uset--get-main-source-window))
   ((one-window-p)
    (uset--split-window-in-two))
   ((= 2 (length (window-list)))
    (next-window (uset--get-main-source-window)))
   (t
    ;; 3 and more windows are something special, use the
    ;; main window to preserve the context of extra
    ;; window.
    (uset--get-main-source-window))))

(defun uset--display-new-buffer (buffer display-buffer-action)
  "Main hook to select or create window for the buffer.
FORCE-OTHER-WINDOW is ignored."
  (or
   (get-buffer-window buffer)
   (let ((window (uset--get-window-to-show-buffer buffer)))
     (set-window-buffer window buffer)
     window)))

(defun uset--init-window-config ()
  (add-to-list 'after-make-frame-functions 'uset--initial-window-config)

  ;; Add the hook to display-buffer-alist rather than setting
  ;; display-buffer-base-action so the hook takes precedence over the
  ;; action argument of display-buffer.
  (add-to-list 'display-buffer-alist (cons ".*" (list 'uset--display-new-buffer)) t)
  
  (let ((frame (selected-frame)))
    (if (uset--is-daemon-startup-frame frame)
	(setq should-start-server nil)
      (uset--initial-window-config frame))))
  
(defun uset--cygwin-copy-to-clipboard (txt &optional push)
  (let ((process-connection-type nil))
    (let ((p (start-process "putclip" nil "putclip")))
      (process-send-string p txt)
      (process-send-eof p))))

(defun uset--server-start ()
  (require 'server)

  ;; Under cygwin (server-running-p) hungs when the server
  ;; runs. So there I just check so we just check for emacs server
  ;; socket
  (unless (if (not (string= system-type "cygwin"))
	      (server-running-p)
	    (file-exists-p (expand-file-name server-name server-socket-dir)))
    (server-start)))

(defun uset-init ()
  (let ((should-start-server t))

    (setq enable-local-variables :safe)

    (setq inhibit-startup-message t)

    (uset--backup-revert-vc)

    (uset--init-shells)

    (uset--init-ptogramming)

    (uset--init-buffers)

    (uset--init-modeline)

    (setq truncate-partial-width-windows nil)
    (setq split-height-threshold nil)
    (setq split-width-threshold 250)

    ;;(add-hook 'after-make-frame-functions 'my-on-new-frame-hook)

    (setq scroll-preserve-screen-position t)
    (setq compilation-scroll-output t)
    (setq auto-window-vscroll nil)
    (setq scroll-conservatively 10000)
    (scroll-bar-mode 0)

    ;(menu-bar-mode 0)
    (when (display-graphic-p)
      (tool-bar-mode -1))

    ;; navigating by visual lines
    (global-visual-line-mode t)
    (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

    ;; No blinking cursors
    (blink-cursor-mode 0)

    ;; disable bell
    (setq ring-bell-function 'ignore)

    (uset--init-keyboard)

    (when (fboundp 'savehist-mode)
      (savehist-mode 1))

    (setq history-delete-duplicates t)
    (setq history-length 1000)

    (desktop-save-mode 1)

    (uset--init-window-config)
    
    (setq custom-file (concat user-emacs-directory "custom.el"))

    (when (not interprogram-cut-function)
      (when (string= system-type "cygwin")
	(when (executable-find "putclip")
	  (setq interprogram-cut-function 'uset--cygwin-copy-to-clipboard))))

    (when should-start-server
      (uset--server-start))

    ))

(defun uset-kill-all-file-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (when (buffer-file-name b)
      (kill-buffer b))))

(defun uset-copy-to-x (start end)
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
