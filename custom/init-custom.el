;;禁用滚动栏，用鼠标滚轮代替
;;(scroll-bar-mode nil)

;;禁用启动画面
(setq inhibit-startup-message t)


;;粘贴于光标处，而不是鼠标指针处
(setq mouse-yank-at-point t)

;;========================================
;; 回显区
;;========================================

;;闪屏报警
(setq visible-bell nil)

;;使用 y or n 提问
(fset 'yes-or-no-p 'y-or-n-p)

;;锁定行高
(setq resize-mini-windows nil)

;;递归 minibuffer
(setq enable-recursive-minibuffers t)

;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
;;(setq suggest-key-bindings 1) ;;

;;========================================
;; 编辑器设定
;;========================================

;;不生成临时文件
(setq-default make-backup-files nil)

;;开启服务器模式
;(server-start)


(provide 'init-custom)
