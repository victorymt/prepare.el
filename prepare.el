;; prepare file list
;; prepare file mode
;; w: 编辑模式
;; C-r: 进入只读模式
;; n/p
;; RET : 跳转

(defvar prepare-buf-name "*prepare*")
(defvar prepare-file "~/.emacs.d/.prepare")
(defvar prepare/keymap (make-sparse-keymap))
(define-key prepare/keymap (kbd "RET") 'prepare/map)

(defun prepare-next-line ()
  (interactive)
  (if buffer-read-only
      (next-line)
    (insert "n")))

(defun prepare-previous-line ()
  (interactive)
  (if buffer-read-only
      (previous-line)
    (insert "p")))

(defun turn-to-write ()
  (interactive)
  (if buffer-read-only
      (setq-local buffer-read-only nil)))

(defun turn-to-read-only ()
  (interactive)
  (setq-local buffer-read-only t))

(defun save-to-file ()
  (interactive)
  (write-region (point-min) (point-max) prepare-file))

(defvar-keymap prepare-key-map
  "n" #'prepare-next-line
  "p" #'prepare-previous-line
  "w" #'turn-to-write
  "C-r" #'turn-to-read-only
  "C-s" #'save-to-file)

(defun prepare-start-mode ()
  (interactive)
  (switch-to-buffer (get-buffer-create prepare-buf-name))
  (prepare-file-mode))

;; +TODO Bugs: 打开模式的时候没有从预先的文件读取
(define-derived-mode prepare-file-mode text-mode "Prepare"
  (use-local-map prepare-key-map)
  (setq-local buffer-read-only nil)
  (setq mode-name "Prepare")
  (let ((curbuf (get-buffer-create prepare-buf-name)))
    (when (buffer-modified-p)
      (write-region (point-min) (point-max) prepare-file)
      (erase-buffer))
    (when (file-regular-p prepare-file)
      (insert-file-contents prepare-file)))
  
  (let ((outbuf (get-buffer-create prepare-buf-name)))
  (with-current-buffer outbuf
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
	    (lb (line-beginning-position))
	    (le (line-end-position)))
	(message current-line)
	(put-text-property lb le 'mouse-face 'highlight)
	(when (file-regular-p current-line)
	  (put-text-property lb le 'keymap prepare/keymap))
	(forward-line 1)))))
 
  (defun prepare/map ()
    (interactive)
    (let* ((ps (or (previous-single-property-change (point) 'keymap) (point-min)))
	   (pe (or (next-single-property-change (point) 'keymap) (point-max)))
	   (current-line (buffer-substring ps pe)))
      (find-file-other-window (string-trim current-line)))))





