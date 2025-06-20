(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
Returns the buffer object.
New buffer is named untitled, untitled<2>, etc.

Warning: new buffer is not prompted for save when killed, see `kill-buffer'.
Or manually `save-buffer'

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Created: 2017-11-01
Version: 2022-04-05"
  (interactive)
  (let ((xbuf (generate-new-buffer "untitled")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode)
    xbuf
    ))

(defgroup shebang nil
  "Shebang."
  :group 'extensions)

(defcustom shebang-env-path "/usr/bin/env"
  "Path to the env executable."
  :type 'string
  :group 'shebang)

(defcustom shebang-interpretor-map
  '(("sh" . "bash")
    ("py" . "python3")
    ("js" . "deno run")
    ("mjs" . "deno run")
    ("ts" . "deno run")
    ("rb" . "ruby"))
  "Alist of interpretors and their paths."
  :type '(alist :key-type (string :tag "Extension")
								:value-type (string :tag "Interpreter"))
  :group 'shebang)

(defun guess-shebang-command ()
  "Guess the command to use for the shebang."
  (let ((ext (file-name-extension (buffer-file-name))))
    (or (cdr (assoc ext shebang-interpretor-map))
				ext)))

(defun insert-shebang ()
  "Insert shebang line at the top of the buffer."
  (interactive)
  (goto-char (point-min))
  (insert (format "#!%s %s" shebang-env-path (guess-shebang-command)))
  (newline))


(defgroup run nil
  "Run."
  :group 'extensions)

(defcustom run-ext-command-map
  '(("sh" . "bash")
    ("py" . "python3")
    ("js" . "deno run")
    ("ts" . "deno run")
    ("mjs" . "deno run")
    ("rb" . "ruby"))
  "Alist of interpretors and their paths."
  :type '(alist :key-type (string :tag "Extension")
								:value-type (string :tag "Command"))
  :group 'run)

(defun get-command (file)
  "Get command for executing FILE.

Return the FILE when the file is executable.
Return the command from the run-ext-command-map otherwise"
  (if (file-executable-p file)
			file
    (let ((ext (file-name-extension file)))
      (format "%s %s" (cdr (assoc ext run-ext-command-map)) file))))

(defun run-buffer ()
  "Run the current buffer."
  (interactive)
  (when (not (buffer-file-name)) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* (
         ($outputb "*run output*")
         (resize-mini-windows nil)
         ($fname (buffer-file-name))
         ($cmd (get-command $fname))
         )
    (progn
      (message "Running %s" $cmd)
      (shell-command $cmd $outputb)
      )))

(provide 'functions)
