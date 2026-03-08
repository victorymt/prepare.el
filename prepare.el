;; prepare file mode

(require 'cl-lib)
(setq debug-on-error t)
(defvar prepare-buf-name "*prepare*")
(defvar prepare-file "~/.emacs.d/.prepare")
(defvar prepare/keymap (make-sparse-keymap))
(defvar prepare-files-list '())
(defvar prepare-marked-files-list '())


;;; global

(defun prepare-add-current-buffer-file ()
  (interactive)
  (let  ((current-file (buffer-file-name)))
    (unless (null current-file)
      (add-to-list 'prepare-files-list current-file t))))

(defun prepare-remove-current-buffer-file ()
  (interactive)
  (let  ((current-file (buffer-file-name)))
    (unless (null current-file)
      (setq prepare-files-list (delete del-file-path prepare-files-list)))))

(defun prepare-save-current-file-lists ()
  (save-to-file))

(global-set-key (kbd "C-c b") #'prepare-add-current-buffer-file)
(global-set-key (kbd "C-c r") #'prepare-remove-current-buffer-file)
;;;


(defun prepare/map ()
  (interactive)
  (let* ((ps (or (previous-single-property-change (point) 'keymap) (point-min)))
	 (pe (or (next-single-property-change (point) 'keymap) (point-max)))
	 (current-line (buffer-substring ps pe))
	 (current-file (string-trim current-line)))
    (if (file-exists-p current-file)
	(find-file-other-window current-file)
      (message "[ %s ] not exists" current-file))))

(define-key prepare/keymap (kbd "RET") 'prepare/map)

(defun prepare-next-line ()
  (interactive)
  (next-line))

(defun prepare-previous-line ()
  (interactive)
  (previous-line))

(defun save-to-file ()
  (interactive)
  (with-temp-file prepare-file
    (dolist (file prepare-files-list)
      (insert file)
      (insert "\n")))
  (message "Saved"))

(defun prepare-add-filepath ()
  (interactive)
  (let ((filepath (read-file-name "File: "))
	(old-files-list prepare-files-list))
    (add-to-list 'prepare-files-list filepath t) 
    (unless (equal old-files-list prepare-files-list)
      (with-current-buffer (get-buffer-create prepare-buf-name)
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (insert "\n")
	  (move-beginning-of-line 1)
	  (insert "  ")
	  (insert filepath)
	  
	  (let* ((lb (line-beginning-position))
		 (le (line-end-position))
		 (file-begining (+ lb 2))
		 (file-ending le))
	    (prepare--add-property lb le)))))))

(defun prepare-delete-filepath ()
  (interactive)
  (let* ((lb (line-beginning-position))
	 (le (line-end-position))
	 (file-begining (+ lb 2))
	 (file-ending le))
    (let ((del-file-path (buffer-substring file-begining file-ending)))
      (setq prepare-files-list (delete del-file-path prepare-files-list))
      (let ((inhibit-read-only t))
	(cond ((and (= le (point-max)) (= lb (point-min))) (delete-region lb le))
	      ((= le (point-max)) (delete-region (- lb 1) le))
	      ((= lb (point-min)) (delete-region lb (+ le 1)))
	      (t
	       (delete-region (- lb 1) le)))
	(message "Delete: %s" del-file-path)))))

;; 现在有个问题，mark unmark 都只是单纯的加个标记，于是还需要额外的函数去获得被标记的列表，不过似乎不是什么大问题，列表也不可能太大。
(defun prepare-set-mark ()
  (interactive)
  (let ((lb (line-beginning-position)))
    (let ((inhibit-read-only t))
      (subst-char-in-region lb (+ lb 1) ?\s ?D)))
  (forward-line))

(defun prepare-unmark ()
  (interactive)
  (let ((lb (line-beginning-position))
	(inhibit-read-only t))
    (subst-char-in-region lb (+ lb 1) ?D ?\s)))

;; 这里 dired 使用了 正则表达式，我就先暴力遍历所有吧
(defun prepare-get-marked-files ()
  (interactive)
  (with-current-buffer (get-buffer-create prepare-buf-name)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((lb (line-beginning-position))
	     (le (line-end-position))
	     (current-line (buffer-substring lb le)))
	(when (eql ?D (char-after lb))
	  (let ((file (prepare--get-filename current-line)))
	    (add-to-list 'prepare-marked-files-list file))))
      (forward-line 1))))

;; 根据 prepare-files-list 更新 prepare-buf
(defun prepare-refresh ()
  (interactive)
  (with-current-buffer (get-buffer-create prepare-buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (prepare--insert-filepath-rec)
      (prepare--add-whole-property))))

(defun prepare-delete-marked-filepath ()
  (interactive)
  (prepare-get-marked-files)
  (let ((rest-files (cl-set-difference prepare-files-list prepare-marked-files-list :test 'equal)))
    (setq prepare-files-list rest-files)
    (setq prepare-marked-files-list '()))
  (prepare-refresh))

;; 就是将 .prepare 又读入全局变量并且 refresh，目前会导致顺序改变
(defun prepare-reset ()
  (interactive)
  (prepare--file->buffer->global_var)
  (prepare-refresh))

(defvar-keymap prepare-key-map
  "n" #'prepare-next-line
  "p" #'prepare-previous-line
  "A" #'prepare-add-filepath
  "D" #'prepare-delete-filepath
  "d" #'prepare-set-mark
  "u" #'prepare-unmark
  "x" #'prepare-delete-marked-filepath
  "C-s" #'save-to-file
  "g" #'prepare-refresh
  "r" #'prepare-reset)

(defun prepare-start-mode ()
  (interactive)
  (switch-to-buffer (get-buffer-create prepare-buf-name))
  (prepare-file-mode))

;; curent-line [D /filename]
(defun prepare--get-filename (current-line)
  (if (> (length current-line) 2)
      (substring-no-properties current-line 2)
    nil))

;; 或者读入一个全局变量，保存这个文件的列表
(defun prepare--insert-filepath ()
  (let ((ln (length prepare-files-list)))
    (dolist (filepath prepare-files-list)
      (insert "  ")
      (insert filepath)
      (insert "\n"))))		; 这个版本最后一行是 \n，rec 不是

(defun prepare--insert-filepath-rec ()
  (prepare--insert-filepath* prepare-files-list))
(defun prepare--insert-filepath* (list)
  (cond ((null list) nil)
	((null (cdr list))
	 (insert "  ")
	 (insert (car list)))
	(t
	 (insert "  ")
	 (insert (car list))
	 (insert "\n")
	 (prepare--insert-filepath* (cdr list)))))

(defun prepare--add-property (line-start line-end)
  (put-text-property line-start line-end 'mouse-face 'highlight)
  (let ((file-beginning (+ line-start 2))
	(file-ending line-end))
    (put-text-property file-beginning file-ending 'keymap prepare/keymap)
    (put-text-property file-beginning file-ending 'face '(:foreground "#96a6c8"))))

(defun prepare--file->buffer->global_var ()
  (when (file-regular-p prepare-file)
    (with-temp-buffer
      (insert-file-contents prepare-file)
      (while (not (eobp))
	(let* ((lb (line-beginning-position))
	       (le (line-end-position))
	       (current-line (buffer-substring lb le)))
	  (when (> (length current-line) 0)
	    (add-to-list 'prepare-files-list current-line t)) ; 加在末尾才是正确的
	  (forward-line))))))

(defun prepare--add-whole-property ()
  (goto-char (point-min))	       
  (while (not (eobp))		
    (let* ((lb (line-beginning-position))
	   (le (line-end-position)))
      (let ((inhibit-read-only t))
	(prepare--add-property lb le))
      (forward-line 1))))

(define-derived-mode prepare-file-mode text-mode "Prepare"
  (use-local-map prepare-key-map)
  (setq-local buffer-read-only t)
  (setq mode-name "Prepare")
  (prepare--file->buffer->global_var)
  (with-current-buffer (get-buffer-create prepare-buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (prepare--insert-filepath-rec)
      (prepare--add-whole-property))))
