;; prepare file mode

;; Bugs or Todo:
;; 有个问题，有些函数我只想让它在 prepare mode 存在
;; 添加一个函数，在其它 buffer 中调用，如果 visit 了一个 file, 就把这个 file 的 path 加入 .prepare 中
;; 对称的，有一个删除当前 buffer visit 的 file 在 prepare 中的 path
;; 我想再加入一个 note 的功能，并且是会保存到 .prepare 中的
;; 这样的话，要修改的部分有 prepare-file->buffer->global_var，获得文件名，保存，全局变量列表里的类型从 string 变成了 cons
;; 也就是说，除了 prepare-file->buffer->global_var 要大改
;; 从全局变量获得文件名需要一个低级接口
;; 从一行内获得文件名，获得 mark，获得 ... 都需要一个低级的接口

;; Think:
;; 还有就是许多函数暴露在外边，还有 define-derived-mode 中的函数有点多了，考虑利用 hook 挂载
;; 还有就是当前的在 buffer 中插入格式处理的太粗糙了，需要一个更低层的接口
;; 最后是变量命名，看看有没有约定俗称的方案
;; 这样子：预留两个字符的大小
;; D /home/file1.txt
;;   /home/file2.txt

;; 思路：
;; 全部围绕着全局变量 prepare-files-list 和 prepare-marked-files-list 展开
;; 删除，添加都是先修改这些全局变量，再更新界面
;; 保存到文件就是保存全局变量的内容
;; 读取文件就是读取到全局变量

;; Learn Or Research
;; 模式加 hook

;; Idea:
;; 话说，我能不能直接 将 .prepare 设置为一个目录，要准备的文件直接链接在里面，然后我就可以用 dired 了
;; md 我觉得这真是个好主意，但我决定先实现当前的项目，就当熟悉 elisp 编程了。这个主意以后再实现吧。
;; 我又想了想，用 dired 操作的删除的是文件，而我要的是删除这个文件在当前缓冲区的字符路径，需求不一样

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
  (goto-char (point-min))		; 回到开头
  (while (not (eobp))			; 这里就是遍历整个 buffer，添加属性的部分只能在这里处理
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
