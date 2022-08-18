(defcustom xinput--regex-line-device "^[^[:alnum:]]*\\(.*\\).*id=\\([[:digit:]]*\\)"
  "Regex used to match device's name and id"
  :type 'regexp)

(defcustom xinput--regex-line-props  "^[^[:alnum:]]*\\(.*\\):[^[:alnum:]]\\(.*\\)"
  "Regex used to match props name and value"
  :type 'regexp)

(defun xinput--get-devices-list ()
  (let ((result))
	(with-temp-buffer
	  (insert (shell-command-to-string "xinput"))
	  (goto-char (point-min))
	  (while (not (eobp))
		(let* ((line (thing-at-point 'line)) 
			   (match (string-match xinput--regex-line-device line)))
		  (push `(,(string-trim (match-string 1 line)) ,(match-string 2 line)) result))
		(forward-line)))
	result))


(defun xinput--format-devices-list (devices)
  (let ((result))
	(dolist (device devices)
	  (push `(,(cadr device) ,(vector  (xinput--propertize-id (cadr device))
									   (xinput--propertize-name (car device))))
			result))
	result))

(defun xinput--propertize-id (id)
  (propertize id 'face 'bold))

(defun xinput--propertize-name (name)
  (propertize name 'font-lock-face 'font-lock-comment-face))

(defun xinput-view-device (device)
  (interactive
   (list (or (tabulated-list-get-id)
			 (read-string "Device ID: "))))
  (with-current-buffer (get-buffer-create (format "*xinput-device*" device))
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert (shell-command-to-string (format "xinput list-props %s" device)))
	  (goto-char (point-min)))
	(special-mode)
	(switch-to-buffer-other-window (current-buffer))))

(defun xinput-view-device (device)
  (interactive
   (list (or (tabulated-list-get-id)
			 (read-string "Device ID: "))))
  (with-current-buffer (get-buffer-create (format "*xinput-device* %s" device))
	(setq xinput--global-device-id device)
	(xinput-props-mode)
	(switch-to-buffer (current-buffer))))

(defun xinput-set-prop ()
  (interactive)
  (let ((prop-name (tabulated-list-get-id)))
	(shell-command (read-shell-command "Run: "
						 (format "xinput set-prop %s \"%s\" "
								 xinput--global-device-id
								 prop-name)))))

(define-derived-mode xinput-mode tabulated-list-mode "XInput"
  "Special mode managing xinput"
  (setq mode-name "Xinput")
  (setq major-mode 'xinput-mode)
  (use-local-map xinput-mode-map)
  (setq tabulated-list-format [("id" 10 t)
							   ("Name" 60 t)])

  (setq tabulated-list-entries (xinput--format-devices-list (xinput--get-devices-list)))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode 1)
  (run-mode-hooks 'xinput-mode-hook))


(defun xinput--get-props (device)
  (let ((result))
	(with-temp-buffer
	  (insert (shell-command-to-string (format "xinput list-props %s | sed 1d" device)))
	  (goto-char (point-min))
	  (while (not (eobp))
		(let* ((line (thing-at-point 'line)) 
			   (match (string-match xinput--regex-line-props line)))
		  (push `(,(string-trim (match-string 1 line)) ,(match-string 2 line)) result))
		(forward-line)))
	result))

(defun xinput--format-props-list (props)
  (let ((result))
	(dolist (prop props)
	  (push `(,(xinput--build-prop-id (car prop)) ,(vector  (xinput--propertize-name (car prop)) (cadr prop)))
			result))
	result))

(defun xinput--build-prop-id (prop)
  (replace-regexp-in-string
   " \([[:digit:]]*\)$"
   ""
   prop))

(define-derived-mode xinput-props-mode tabulated-list-mode "XInput-props"
  "Special mode managing xinput"
  ;; (defvar-local xinput-device-id 'nil)
  (setq mode-name "Xinput-props")
  (setq major-mode 'xinput-props-mode)
  (use-local-map xinput-props-mode-map)
  (setq tabulated-list-format [("Prop" 70 t)
							   ("Value" 60 t)])

  (setq tabulated-list-entries (xinput--format-props-list (xinput--get-props xinput--global-device-id)))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode 1)
  (run-mode-hooks 'xinput-props-mode-hook))

(let ((map xinput-mode-map))
  (evil-define-key 'motion map
	(kbd "RET") #'xinput-view-device))

(let ((map xinput-props-mode-map))
  (evil-define-key 'motion map
	(kbd "RET") #'xinput-set-prop))
