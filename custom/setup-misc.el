(provide 'setup-misc)

(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(global-linum-mode t)
;;语法加亮
(global-font-lock-mode t)
(transient-mark-mode t)

;; my own config
(defvar my-config-directory (concat user-emacs-directory "my-config/"))
(if (not (file-exists-p my-config-directory))
    (make-directory my-config-directory t))
(setq custom-file (concat my-config-directory "gui-config-save.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))
(setq yas-snippet-dirs (list (concat my-config-directory "yasnippets/") 'yas-installed-snippets-dir))

;; save place, ~/.emacs.d/save.d
(defvar my-save-directory (concat user-emacs-directory "save.d/"))
(if (not (file-exists-p my-save-directory))
    (make-directory my-save-directory t))
(setq bookmark-default-file (concat my-save-directory "bookmark-save.el"))
(setq ido-save-directory-list-file (concat my-save-directory "io-last-save.el"))
(setq abbrev-file-name (concat my-save-directory "abbrev-defs-save.el"))
(setq diary-file (concat my-save-directory "diary-save.txt"))
(setq semanticdb-default-save-directory (concat my-save-directory "semanticdb"))
(setq ede-project-placeholder-cache-file (concat my-save-directory "ede-projects-cache.el"))
(setq projectile-cache-file (concat my-save-directory "projectile.cache.el"))
(setq projectile-known-projects-file (concat my-save-directory "projectile-bookmarks.eld"))
(setq recentf-save-file (concat my-save-directory "recentf.el"))
(setq ac-comphist-file (concat my-save-directory "ac-comphist.dat"))
(setq auto-save-list-file-prefix (concat my-save-directory "auto-save-list-"))
(setq session-save-file (concat my-save-directory "emacs.session"))
(setq tramp-persistency-file-name (concat my-save-directory "tramp-conn-history"))
(setq save-place-file (concat my-save-directory "save-places.el"))
(setq eshell-directory-name (concat my-save-directory "eshell/"))

;; custom data directory, ~/.emacs.d/my-data
(defvar my-data-directory (concat user-emacs-directory "my-data/"))
(if (not (file-exists-p my-data-directory))
    (make-directory my-data-directory t))
(setq org-agenda-files (list (concat my-data-directory "org-files/2015/")))

;; backup config(no backups)
(setq make-backup-files nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(size-indication-mode t)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))
;;(set-face-attribute 'italic nil
;;                    :family "Inconsolata-Italic")

(setq visible-bell nil)
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(delete-selection-mode)

;;设置删除纪录
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;;========================================
;; 缓冲区
;;========================================

;;设定行距
(setq default-line-spacing 0)

;;页宽
(setq default-fill-column 90)

;;缺省模式 text-mode
;;(setq default-major-mode 'text-mode)

;;页面平滑滚动， scroll-margin 5 靠近屏幕边沿3行时开始滚动，可以很好的看到上下文。
(setq scroll-margin 5
    scroll-conservatively 10000)

;高亮显示成对括号，但不来回弹跳
(show-paren-mode t)
(setq show-paren-style 'parentheses)


;;鼠标指针规避光标
;(mouse-avoidance-mode 'animate)

;;粘贴于光标处，而不是鼠标指针处
(setq mouse-yank-at-point t)

;;========================================
;; 回显区
;;========================================

;;锁定行高
(setq resize-mini-windows nil)

;;递归 minibuffer
(setq enable-recursive-minibuffers t)

;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
;;(setq suggest-key-bindings 1) ;;

;;========================================
;; 状态栏
;;========================================

;;显示时间
(display-time)
;;时间格式
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

;;显示列号
(setq column-number-mode t)

;;标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号
(setq frame-title-format "%f")

;;========================================
;; 编辑器设定
;;========================================

;;不生成临时文件
(setq-default make-backup-files nil)

;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)

(setq confirm-nonexistent-file-or-buffer nil)

;;将错误信息显示在回显区
;(condition-case err
;    (progn
;    (require 'xxx) )
;  (error
;   (message "Can't load xxx-mode %s" (cdr err))))

;;使用X剪贴板
(setq x-select-enable-clipboard t)
;;;;;;;; 使用空格缩进 ;;;;;;;;
;; indent-tabs-mode  t 使用 TAB 作格式化字符  nil 使用空格作格式化字符
(setq indent-tabs-mode nil)
(setq tab-always-indent nil)
(setq tab-width 4)

;;========================================
;; 颜色设置
;;========================================

;; 指针颜色
(set-cursor-color "black")
;; 鼠标颜色
(set-mouse-color "black")
;; 背景和字体颜色
(set-foreground-color "gainsboro")
(set-background-color "grey30")
(set-border-color "black")

;; 语法高亮显示，区域选择，二次选择 ;;前景和背景色
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")

;;========================================
;; 字体设置
;;========================================
(set-default-font "Monospace-13")

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))
