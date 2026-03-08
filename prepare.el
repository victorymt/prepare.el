;; prepare file list
;; prepare file mode

;; n/p
;; RET : 跳转
;; R : 类似 dired 的 rename
;; A : 添加一个文件路径
;; d : 标记要删除的文件路径
;; D : 删除当前的文件
;; x : 删除标记的文件路径
;; u : 删除标记

;; Bugs or Todo:
;; 添加一个函数，在其它 buffer 中调用，如果 visit 了一个 file, 就把这个 file 的 path 加入 .prepare 中
;; 对称的，有一个删除当前 buffer visit 的 file 在 prepare 中的 path

;; 要研究 dired 的 dired-get-marked-files 是怎么实现的

;; Think:
;; 现在最基础的部分已经完成
;; 接下来需要完成的部分是 mark 和 unmark 以及根据 mark 删除 filepath.
;; mark 部分没什么好说的，就到开头 replace 一个字符就可以了
;; unmark 也差不多
;; 根据 mark 删除这个，首先要获得被标记的 filepath,之后删除就可以了
;; 要么遍历整个 buffer，要么使用 dired 的方式，用正则表达式匹配一遍。 
;; 还有就是许多函数暴露在外边，还有 define-derived-mode 中的函数有点多了，考虑利用 hook 挂载
;; 还有就是当前的在 buffer 中插入格式处理的太粗糙了，需要一个更低层的接口
;; 另外就是给可以访问的链接加上 dired 那样的蓝色
;; 最后是变量命名，看看有没有约定俗称的方案
;; 这样子：预留两个字符的大小
;; D /home/file1.txt
;;   /home/file2.txt

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
  (let ((filepath (read-file-name "File: ")))
    (add-to-list 'prepare-files-list filepath)
    (with-current-buffer (get-buffer-create prepare-buf-name)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(move-beginning-of-line 1)
	(insert "  ")
	(insert filepath)
	
	(let* ((lb (line-beginning-position))
	       (le (line-end-position))
	       (file-begining (+ lb 2))
	       (file-ending le))
	  (prepare-add-property lb le))
	(insert "\n")))))

(defun prepare-delete-filepath ()
  (interactive)
  (let* ((lb (line-beginning-position))
	 (le (line-end-position))
	 (file-begining (+ lb 2))
	 (file-ending le))
    (let ((del-file-path (buffer-substring file-begining file-ending)))
      (setq prepare-files-list (delete del-file-path prepare-files-list)))
    (let ((inhibit-read-only t))
      (delete-region lb (+ le 1)))))	; 把 \n 也删除

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
	  (let ((file (prepare-get-filename current-line)))
	    (add-to-list 'prepare-marked-files-list file))))
      (forward-line 1))))

;; 根据 prepare-files-list 更新 prepare-buf
(defun prepare-refresh ()
  (with-current-buffer (get-buffer-create prepare-buf-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (prepare-insert-filepath))))

(defun prepare-delete-marked-filepath ()
  (interactive)
  (prepare-get-marked-files)
  (let ((rest-files (cl-set-difference prepare-files-list prepare-marked-files-list :test 'equal)))
    (setq prepare-files-list rest-files))
  (prepare-refresh))

(defvar-keymap prepare-key-map
  "n" #'prepare-next-line
  "p" #'prepare-previous-line
  "A" #'prepare-add-filepath
  "D" #'prepare-delete-filepath
  "d" #'prepare-set-mark
  "u" #'prepare-unmark
  "x" #'prepare-delete-marked-filepath
  "C-s" #'save-to-file)

(defun prepare-start-mode ()
  (interactive)
  (switch-to-buffer (get-buffer-create prepare-buf-name))
  (prepare-file-mode))

(define-derived-mode prepare-file-mode text-mode "Prepare"
  (use-local-map prepare-key-map)
  (setq-local buffer-read-only t)
  (setq mode-name "Prepare")
  
  ;; curent-line [D /filename]
  (defun prepare-get-filename (current-line)
    (if (> (length current-line) 2)
	(substring-no-properties current-line 2)
      nil))
  ;; 将 .prepare 中的内容加入到 prepre-buf 中，每一行需要处理加入格式
  ;; 或者读入一个全局变量，保存这个文件的列表
  (defun prepare-insert-filepath ()
    (dolist (filepath prepare-files-list)
      (insert "  ")
      (insert filepath)
      (insert "\n")))
  
  (defun prepare-add-property (line-start line-end)
    (put-text-property line-start line-end 'mouse-face 'highlight)
    (let ((file-begining (+ line-start 2))
	  (file-ending line-end))
      (put-text-property file-begining file-ending 'keymap prepare/keymap)))
  
  (defun prepare/map ()
    (interactive)
    (let* ((ps (or (previous-single-property-change (point) 'keymap) (point-min)))
	   (pe (or (next-single-property-change (point) 'keymap) (point-max)))
	   (current-line (buffer-substring ps pe))
	   (current-file (string-trim current-line)))
      (if (file-exists-p current-file)
	  (find-file-other-window current-file)
	(message "[ %s ] not exists" current-file))))

  (let ((curbuf (get-buffer-create prepare-buf-name)))
    (when (file-regular-p prepare-file)
      (with-temp-buffer
	(insert-file-contents prepare-file)
	(while (not (eobp))
	  (let* ((lb (line-beginning-position))
		 (le (line-end-position))
		 (current-line (buffer-substring lb le)))
	    (add-to-list 'prepare-files-list current-line)
	    (forward-line))))
      (with-current-buffer curbuf
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (prepare-insert-filepath))

	(goto-char (point-min))		; 回到开头

	(while (not (eobp))			; 这里就是遍历整个 buffer，添加属性的部分只能在这里处理
	  (move-beginning-of-line 1)
	  (let* ((lb (line-beginning-position))
		 (le (line-end-position)))
	    (let ((inhibit-read-only t))
	      (prepare-add-property lb le))
	  (forward-line 1)))))))
