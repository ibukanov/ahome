
(defvar my-paged-last-buffer nil)

(defun my-paged-append-output (proc str)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (setq buffer-read-only nil)
      (insert str))))

(defun my-paged-sentinel (proc str)
  (my-paged-append-output proc (concat "----- " str)))

(defun my-paged-process (command &rest args)
  (my-paged-process-impl default-directory command args))

(defun my-dir-paged-process (dir command &rest args)
  "Show the output of the given command in a buffer executing the command from
the given directory. If there is no args and command is a non-executable file,
then just show that file."
  (if args
      (my-paged-process-impl dir command args)
    (let ((f (expand-file-name command dir)))
      (if (and (file-readable-p f) (not (file-executable-p f)))
	  (find-file f)
	(my-paged-process-impl dir command args)))))

(defun my-paged-process-impl (command-dir command args)
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
      (set-process-filter proc 'my-paged-append-output)
      (set-process-sentinel proc 'my-paged-sentinel)
      (setq my-paged-last-buffer outbuf))))

(defun my-kill-all-file-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (when (buffer-file-name b)
      (kill-buffer b))))


(defun my-patch-delete-trailing-whitespace ()
  (interactive "*")
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\+.*?\\( +\\)$" nil t)
	(skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
	(delete-region (match-beginning 1) (match-end 1))))))


(defun my-run-terminal (terminal-name)
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

(defun my-run-shell (name)
  ;; Make sure that the shell will appear in the current window
  (setq name (concat "*" name "*"))
  (let ((buf (get-buffer name)))
    (when (not buf)
      (setq buf (get-buffer-create name)))
    (switch-to-buffer buf)
    (shell buf)))

(defun my-2-window-split ()
  (interactive)
  (let* ((window (selected-window))
	 (frame (window-frame window))
	 (width (frame-width frame))
	 (decoration-width (if window-system 3 2))
	 (usable-width (max 1 (- width decoration-width)))
	 (left-panel-width
	  (cond ((or (<= usable-width 40) (>= usable-width 190))
		 (/ (+ 1 usable-width) 2))
		((>= usable-width 173)
		 95)
		(t (+ 17 (/ (- usable-width 17) 2)))))
	 (right-panel-width (- usable-width left-panel-width)))
    (delete-other-windows window)
    (message "window split %s -> %s | %s"
	     usable-width left-panel-width right-panel-width)
    (split-window window (+ decoration-width left-panel-width) t)))


(defun my-unescape-xml (start end)
  "Replace “&lt;” and other chars in XML with actual characters.
This works on the current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&lt;" nil t) (replace-match "<" nil t))
    (goto-char (point-min))
    (while (search-forward "&gt;" nil t) (replace-match ">" nil t))
    (goto-char (point-min))
    (while (search-forward "&amp;" nil t) (replace-match "&" nil t))
    (goto-char (point-min))
    (while (search-forward "&quot;" nil t) (replace-match "\"" nil t))
    )
  )

;; Under cygwin (server-running-p) hungs when the server runs. So there I just check  so we just check for emacs server socket

(defun my-server-running-p ()
  (if (not (string= system-type "cygwin"))
      (server-running-p)
    (file-exists-p (expand-file-name server-name server-socket-dir))))

