

;; Author: Florent Teissier <teissierflorent@gmail.com>
;; Keywords: xinput, keyboard, mouse

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a small package interfacing xinput command
;; It displays the output of xinput in a table and allows
;; you to set props to a specific device

;;; Code:

(defcustom xinput--regex-line-device "^[^[:alnum:]]*\\(.*\\).*id=\\([[:digit:]]*\\)\t\\[\\(.*\\).*\]"
  "Regex used to match device's name and id and master/slave "
  :type 'regexp)

(defcustom xinput--regex-line-props  "^[^[:alnum:]]*\\(.*\\):[^[:alnum:]]\\(.*\\)"
  "Regex used to match props name and value"
  :type 'regexp)

(defcustom xinput--props-buffer-name "*xinput-props*"
  "Name of main xinput props buffer "
  :type 'string)

(defcustom xinput--main-buffer-name "*xinput*"
  "Name of main xinput buffer "
  :type 'string)

(defun xinput--get-devices-list ()
  "Run xinput and parse the output to get a list of elements of form
'(NAME ID)"
  (let ((result))
	(with-temp-buffer
	  (insert (shell-command-to-string "xinput"))
	  (goto-char (point-min))
	  (while (not (eobp))
		(let* ((line (thing-at-point 'line)) 
			   (match (string-match xinput--regex-line-device line)))
		  (push `(,(string-trim (match-string 1 line))
				  ,(match-string 2 line)
				  ,(match-string 3 line))
				result))
		(forward-line)))
	result))


(defun xinput--format-devices-list (devices)
  "Format the list of DEVICES of form '((NAME ID) (NAME ID)) for
tabulated-list-mode."
  (let ((result))
	(dolist (device devices)
	  (push `(,(cadr device) ,(vector  (xinput--propertize-id (cadr device))
									   (xinput--propertize-name (car device))
									   (propertize (caddr device) 'font-lock-face 'font-lock-doc-face)))
			result))
	result))

(defun xinput--propertize-id (id)
  (propertize id 'face 'bold))

(defun xinput--propertize-name (name)
  (propertize name 'font-lock-face 'font-lock-doc-face))

(defun xinput-view-device-props (device)
  (interactive
   (list (or (tabulated-list-get-id)
			 (read-string "Device ID: "))))
  (with-current-buffer (get-buffer-create (format "%s %s" xinput--props-buffer-name device))
	(setq xinput--global-device-id device)
	(xinput-props-mode)
	(switch-to-buffer (current-buffer))))

(defun xinput-set-prop ()
  (interactive)
  (let ((prop-name (tabulated-list-get-id)))
	(shell-command (read-shell-command "Run: "
						 (format "xinput set-prop %s \"%s\" "
								 xinput--global-device-id
								 prop-name)))
	(xinput-props-mode)))

(define-derived-mode xinput-mode tabulated-list-mode "XInput"
  "Special mode managing xinput"
  (setq mode-name "Xinput")
  (setq major-mode 'xinput-mode)
  (use-local-map xinput-mode-map)
  (setq tabulated-list-format [("id" 10 t)
							   ("Name" 60 t)
							   ("Master/Slave" 60 t)])

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

(defun xinput ()
  "Point of entry, create a buffer and trigger xinput-mode"
  (interactive)
  (let ((buff (get-buffer-create xinput--main-buffer-name)))
	(with-current-buffer buff
	  (xinput-mode))
	(pop-to-buffer-same-window buff)))

(if (boundp 'evil-mode)
	(progn
	  (let ((map xinput-mode-map))
		(evil-define-key 'motion map (kbd "RET") #'xinput-view-device-props))
	  (let ((map xinput-props-mode-map))
		(evil-define-key 'motion map (kbd "RET") #'xinput-set-prop))
	  )
  (progn
	(let ((map xinput-mode-map))
	  (define-key map (kbd "RET") #'xinput-view-device-props))

	(let ((map xinput-props-mode-map))
	  (define-key map (kbd "RET") #'xinput-set-prop))
	))

(provide 'xinput)
