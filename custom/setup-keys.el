(provide 'setup-keys)

(windmove-default-keybindings)

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c /") 'ac-complete-filename)
(global-set-key [(control tab)] 'company-complete)
(global-set-key (kbd "M-g") 'goto-line)

;; custom function key bindings
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)
(global-set-key (kbd "C-c i") 'indent-region-or-buffer)
(global-set-key (kbd "C-o") 'prelude-smart-open-line-above)
(global-set-key (kbd "M-o") 'prelude-smart-open-line)

;; package comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; package yasnippet
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)
(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
;; disable yasnippet's TAB keybinging
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-p") 'yas-expand)

;; package anzu
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; package iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; package duplicate thing
(global-set-key (kbd "M-c") 'duplicate-thing)

;; keybindings for custom functions
(global-set-key [f6] 'toggle-selective-display)
(global-set-key [f7] 'xah-new-empty-buffer)


;; org-mode keybindings
(global-set-key [f12] 'org-agenda)

;;========================================
;; 键绑定
;;========================================

;; C-t 设置标记 ;;
(global-set-key (kbd "C-t") 'set-mark-command)

;; C-x b => CRM bufer list
(global-set-key "\C-xb" 'electric-buffer-list)

;;---------- redo
(global-set-key ( kbd "C-.") 'redo)

;;========================================
;;关闭当前缓冲区 Alt+4  ;; C-x 0
(global-set-key (kbd "M-4") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)
;;切换到其它缓冲区 Alt+0 ;; C-x o
(global-set-key (kbd "M-0") 'other-window)


;;F10 显示/隐藏菜单栏 ;; M-x menu-bar-open
;;(global-set-key (kbd "F10") 'menu-bar-mode)
