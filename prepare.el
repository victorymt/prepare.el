;; prepare file mode
;; 一行 {mark, filepath}
;; 有时候我们需要的是 position
;;   此时我们需要知道 line 的 position
;; 有时候我们需要的是 filepath :: string
;;   这里可以通过两种方式 1. 先获得 line 的 string
;;                     2. 先获得 line 的 position
;; 结论：我们更需要 position,再通过 line 的 position 获得 filepath 的 position 和 string

;; 现在我想给它加上 note

(require 'cl-lib)
(setq debug-on-error t)
(defvar prepare-buf-name "*prepare*")
(defvar prepare-file "~/.emacs.d/.prepare")
(defvar prepare/keymap (make-sparse-keymap))
(defvar prepare-files-list '())
(defvar prepare-marked-files-list '())

(defmacro with-prepare-buffer (&rest body)
  `(with-current-buffer (get-buffer-create prepare-buf-name)
     ,@body))

;;; global

;; 加入即时的保存
(defun prepare-extra-open-all-file ()
  (interactive)
  (prepare--file->buffer->global_var)
  (dolist (file prepare-files-list)
    (find-file-noselect file)))

(defun prepare-add-current-buffer-file ()
  (interactive)
  (let  ((current-file (buffer-file-name)))
    (unless (null current-file)
      (add-to-list 'prepare-files-list current-file t) ; 这里会自动去重
      (save-to-file)			; Q: 我是否需要更新 *prepare* buffer 的显示
      (message "Added %s" current-file))))

(defun prepare-remove-current-buffer-file ()
  (interactive)
  (let  ((current-file (buffer-file-name)))
    (unless (null current-file)
      (setq prepare-files-list (delete current-file prepare-files-list)) ; 如果当前文件不存在 delete 会直接返回原列表
      (save-to-file)
      (message "Removed %s" current-file))))

;; TODO prepare-save-current-file-lists 和 save-to-file 有什么不同吗，为什么要包装一下
(defun prepare-save-current-file-lists ()
  (save-to-file))

(global-set-key (kbd "C-c b") #'prepare-add-current-buffer-file) ; backup
(global-set-key (kbd "C-c r") #'prepare-remove-current-buffer-file) ; remove
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
      (with-prepare-buffer
       (let ((inhibit-read-only t))
	 (goto-char (point-max))
	 (unless (= (point-max) (point-min))
	   (insert "\n"))		      
	 (move-beginning-of-line 1)
	 (insert "  ")
	 (insert filepath)
	 
	 (let* ((lb (line-beginning-position))
		(le (line-end-position)))
	   (prepare--add-property lb le)))))))

;; (defmacro with-current-line (func)
;;   (let* ((lb (line-beginning-position))
;; 	 (le (line-end-position)))
;;     (,func lb le)))

;; 没有设置 prepare buffer
(defun prepare-delete-filepath ()
  (interactive)
  (with-prepare-buffer
   (let* ((lb (line-beginning-position))
	  (le (line-end-position))
	  (file-begining (+ lb 2))
	  (file-ending le))
     (if (not (= lb le))
	 (let ((del-file-path (buffer-substring file-begining file-ending)))
	   (setq prepare-files-list (delete del-file-path prepare-files-list))
	   (let ((inhibit-read-only t))
	     (cond ((and (= le (point-max)) (= lb (point-min))) (delete-region lb le))
		   ((= le (point-max)) (delete-region (- lb 1) le))
		   ((= lb (point-min)) (delete-region lb (+ le 1)))
		   (t
		    (delete-region (- lb 1) le)))
	     (message "Delete: %s" del-file-path)))
       (message "Empty!")))))

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

(defun prepare-get-marked-files ()
  (interactive)
  (with-prepare-buffer
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
  (with-prepare-buffer
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

(defun prepare-reset ()
  (interactive)
  (prepare--file->buffer->global_var)
  (prepare-refresh))

(defun prepare-rename-filepath ()
  (interactive)
  (let* ((lb (line-beginning-position))
	 (le (line-end-position))
	 (file-begining (+ lb 2))
	 (file-ending le))
    (if (not (= lb le))
	(let ((chosen-file-path (buffer-substring file-begining file-ending)))
	  (let ((renamed-file-path (read-file-name "Rename: " chosen-file-path)))
	    (setq prepare-files-list (cl-subst renamed-file-path chosen-file-path prepare-files-list :test #'equal))
	    (with-prepare-buffer
	     (let ((inhibit-read-only t))
	       (delete-region lb le)
	       (insert "  ")
	       (insert renamed-file-path)
	       (let ((nle (line-end-position)))
		 (prepare--add-property lb nle))))))
      (message "Empty!"))))


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
  "r" #'prepare-reset
  "R" #'prepare-rename-filepath)


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

;; 重新构建一下 global_var 的结构
;; ((var . val) (var . val)) 以这种形式
;; 就是： (("/usr/bin/" . "note1") ("/user/local/" . "note2"))

;; 这样，就没有 prepare--file->buffer->global_var 这个奇葩的东西了
;; 在正式的修改前，我需要先重构一下，保持原来的逻辑，让这个 global_var 相关的函数变成原子的。
;; struct of global_var:
;; (item1 item2 item3...)
;; (preapre--build-item filepath note)
;; (prepare--item-filepath item)
;; (prepare--item-note item)
;; 还有就是添加属性是根据 point 来决定的
;; 我需要函数去返回一行中 item 中的各项的位置
(define-derived-mode prepare-file-mode text-mode "Prepare"
  (use-local-map prepare-key-map)
  (setq-local buffer-read-only t)
  (setq mode-name "Prepare")
  (prepare--file->buffer->global_var)
  (with-prepare-buffer
   (let ((inhibit-read-only t))
     (erase-buffer)
     (prepare--insert-filepath-rec)
     (prepare--add-whole-property))))

(defun prepare-start-mode ()
  (interactive)
  (switch-to-buffer (get-buffer-create prepare-buf-name)) 
  (prepare-file-mode))

(provide 'prepare)
