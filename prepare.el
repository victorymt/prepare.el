;; prepare file list
;; prepare file mode
;; w: 编辑模式
;; C-r: 进入只读模式
;; n/p
;; RET : 跳转
;; R : 类似 dired 的 rename
;; A : 添加一个文件路径
;; d : 标记要删除的文件路径
;; D : 删除当前的文件
;; x : 删除标记的文件路径
;; u : 删除标记

;; Bugs or Todo:
;; 删除 C-r, w，添加 A，D
;; 添加一个函数，在其它 buffer 中调用，如果 visit 了一个 file, 就把这个 file 的 path 加入 .prepare 中
;; 对称的，有一个删除当前 buffer visit 的 file 在 prepare 中的 path

;; 所以这是快捷键的设置不到位的缘故。
;; 我需要一个 方法获得当前光标下的文件名 / 获得被标记的文件的列表 
;; 要研究 dired 的 dired-get-marked-files 是怎么实现的，它似乎不光支持返回标记的文件的列表还有 current file

;; Think:
;; 文件添加属性的阶段只有进入 prepare mode. 于是想要实现 A 就需要获得文件路径写入 buf 的时候添加属性
;; 由于属性添加和处理的特性，更高效的方式是遍历一遍要处理的内容，同时在遍历的过程中添加修改
;; [X] 我原先想的获得当前行的起始和结束位置看来似乎没有必要，我只需要 forward-line 就足够了，由于一行只有一个文件，通过一个 move-to-file 判断是否有这个文件路径同时进行移动 point ，可是 move-to-file-start 和 move-to-file-end 是怎么实现的
;; md dired 用的 ls --dired，会自动给文件名添加标记，没有参考性
;; 我还是需要获得一行，不过我可以给 mark 预留一个空间
;; 于是我需要沿用原来的方式，通过字符串分割的方式获得标记？
;; 这样子：预留两个字符的大小
;; D /home/file1.txt
;;   /home/file2.txt

;; 关于 readonly 的时机
;; 进入 prepare mode 是 readonly
;; (let ((inhibit-read-only t))
;;   (delete-char 1)
;;   (insert dired-del-marker))

;; Idea:
;; 话说，我能不能直接 将 .prepare 设置为一个目录，要准备的文件直接链接在里面，然后我就可以用 dired 了
;; md 我觉得这真是个好主意，但我决定先实现当前的项目，就当熟悉 elisp 编程了。这个主意以后再实现吧。
;; 我又想了想，用 dired 操作的删除的是文件，而我要的是删除这个文件在当前缓冲区的字符路径，需求不一样

(setq debug-on-error t)
(defvar prepare-buf-name "*prepare*")
(defvar prepare-file "~/.emacs.d/.prepare")
(defvar prepare/keymap (make-sparse-keymap))
(defvar prepare-files-list '())
(define-key prepare/keymap (kbd "RET") 'prepare/map)

(defun prepare-next-line ()
  (interactive)
  (next-line))

(defun prepare-previous-line ()
  (interactive)
  (previous-line))

(defun save-to-file ()
  (interactive)
  (write-region (point-min) (point-max) prepare-file))

(defvar-keymap prepare-key-map
  "n" #'prepare-next-line
  "p" #'prepare-previous-line
  "A" #'prepare-add-filepath
  "D" #'prepare-delete-filepath
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
  (defun prepare/get-filename (current-line)
    (if (> (length current-line) 2)
	(substring current-line 2)
      nil))
  ;; 将 .prepare 中的内容加入到 prepre-buf 中，每一行需要处理加入格式
  ;; 或者读入一个全局变量，保存这个文件的列表
  (defun prepare-insert-filepath ()
    (dolist (filepath prepare-files-list)
      (insert "  ")
      (insert filepath)
      (insert "\n")))
  
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
		 (le (line-end-position))
		 (file-begining (+ lb 2))
		 (file-ending le))
	    (let ((inhibit-read-only t))
	      (put-text-property lb le 'mouse-face 'highlight)
	      (put-text-property file-begining file-ending 'keymap prepare/keymap)))
	  (forward-line 1))))
