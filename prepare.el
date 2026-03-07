;; prepare file list
;; prepare file mode
;; w: 编辑模式
;; C-r: 进入只读模式
;; n/p
;; RET : 跳转
;; R : 类似 dired 的 rename
;; A : 添加一个文件路径
;; d : 标记要删除的文件路径
;; x : 删除标记的文件路径
;; u : 删除标记

;; Bugs or Todo: 或许我应该像 dired 那样分出一个 wdired
;; C-c C-c : 从 wprepare mode -> prepare mode
;; w       : 从 prepare mode -> wprepare mode
;; 但似乎也不太对， wprepare 应该是想文本一样编辑， 而 prepare mode 则是必须通过命令编辑，根据实际的需求，我似乎没有 wprepare mode 的需求。
;; 所以这是快捷键的设置不到位的缘故。

;; Idea:
;; 话说，我能不能直接 将 .prepare 设置为一个目录，要准备的文件直接链接在里面，然后我就可以用 dired 了
;; md 我觉得这真是个好主意，但我决定先实现当前的项目，就当熟悉 elisp 编程了。这个主意以后再实现吧。
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





