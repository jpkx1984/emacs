(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq package-list
      '(use-package
	 tree-sitter
	 company
	 lsp-mode
	 rust-mode
	 flycheck cargo
	 cargo-mode
	 projectile
	 markdown-mode
	 treemacs
	 lsp-treemacs
	 evil lsp-ui
	 helm helm-lsp
	 helm-projectile
	 vertico
	 paredit
	 magit
	 rainbow-delimiters
	 expand-region))

(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'use-package)
;;

(require 'expand-region)

;; evil
(require 'evil)
(evil-set-leader 'normal (kbd "\\"))
(evil-define-key 'normal 'global (kbd "<leader>fu") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>ss") 'helm-lsp-workspace-symbol)
(evil-define-key 'normal 'global (kbd "=") 'er/expand-region)
(evil-define-key 'normal 'global (kbd "-") 'er/shrink-region)
(evil-mode 1)

;; ido
(ido-mode 1)

;; vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; tree sitter
(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter-indent :ensure t)

;; lsp
(lsp-treemacs-sync-mode 1)
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; helm
(require 'helm)
(use-package helm)

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; global settings
(tool-bar-mode 1)
(global-set-key (kbd "<f12>") 'ido-switch-buffer)
(global-set-key (kbd "ESC <escape>") 'helm-buffers-list)
(global-set-key (kbd "<f3>")  'next-window-any-frame)

;; markdown

(use-package markdown-mode :ensure t :mode ("README\\.md\\'" . gfm-mode) :init (setq markdown-command "multimarkdown"))

;; Rust

(use-package rustic :ensure)
;; (use-package rust-mode
;;   :init
;;   (setq rust-mode-treesitter-derive t))

(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)))

(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;; csharp
(add-hook 'csharp-mode-hook #'lsp 'flymake)

;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(deeper-blue))
 '(package-selected-packages
   '(expand-region amx rainbow-delimiters shell-pop paredit vertico helm-lsp helm lsp-ui ## evil lsp-treemacs treemacs markdown-preview-mode projectile magit flycheck lsp-mode rust-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Pragmasevka" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))
