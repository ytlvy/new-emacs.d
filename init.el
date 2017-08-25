;; init config

;;包管理设置
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;;安装所有packages.list中的包
(defun install-packages ()
  "Install all required packages listed in packages.list."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (let ((my-pkg-list-file (concat user-emacs-directory "packages.list")))
    (if (not (file-exists-p my-pkg-list-file))
	(error "package list file %s not found" my-pkg-list-file)
      (let ((my-pkg-list
             (with-temp-buffer
               (insert-file-contents my-pkg-list-file)
               (split-string (buffer-string) "\n" t))))
        (dolist (my-pkg my-pkg-list)
          (let ((my-pkg-sym (intern my-pkg)))
            (unless (package-installed-p my-pkg-sym)
              (package-install my-pkg-sym))))
        (message "package installation done.")))))

;; install packages listed in packages.list
(install-packages)

;; startup setting 全屏
(setq inhibit-startup-message t)

;;检测是否存在 tool-bar-mode 关闭
;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; make frame full-screen on startup
;;(custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))

;; set load path
(let* ((my-site-lisp-dir (concat user-emacs-directory "site-lisp/"))
       (default-directory my-site-lisp-dir))
  (add-to-list 'load-path my-site-lisp-dir)
  (normal-top-level-add-subdirs-to-load-path))
(defvar user-custom-dir (concat user-emacs-directory "custom/"))
(add-to-list 'load-path user-custom-dir)
(add-to-list 'load-path (concat user-custom-dir "package-config"))
(load-file (concat user-custom-dir "custom-funcs.el"))

(require 'setup-misc)
(require 'setup-packages)
(require 'setup-prog)
(require 'setup-keys)

(require 'init-utils)
(require 'init-elpa)
(require 'init-auto-complete)
;;(require 'init-tabbar)
(require 'init-editing-utils)
(require 'init-evil)
(require 'init-fonts)
(require 'init-git)
(require 'init-ido)
(require 'init-recentf)
(require 'init-themes)
(require 'init-custom)
