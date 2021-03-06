(provide 'setup-packages)

(require 'setup-org-mode)
(require 'setup-smartparens)
(require 'setup-golden-ratio)
(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-projectile)
(require 'setup-yasnippet)
(require 'setup-company)
;; (require 'setup-auto-complete)
(require 'setup-tramp)
;; (require 'setup-session)
(require 'setup-saveplace)
(require 'setup-web-mode)

(require 'go-autocomplete)

(require 'bookmark+)
(setq diredp-hide-details-initially-flag nil)
(require 'dired+)
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'dtrt-indent)
(dtrt-indent-mode 1)

(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

(require 'yasnippet)
(yas-global-mode 1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)

(require 'undo-tree)
(global-undo-tree-mode)

;; package anzu
(require 'anzu)
(global-anzu-mode)

;; package iedit
(setq iedit-toggle-key-default nil)
(require 'iedit)

;; package duplicate-thing
(require 'duplicate-thing)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

(require 'elpy)
(elpy-enable)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
