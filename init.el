(require 'package)

(add-to-list 'package-archives
             ;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	     '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives
          '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defvar my-packages '(projectile
                      clojure-mode
                      cider
		      ac-cider
		      paredit
		      ggtags
		      iedit
		      ;clj-refactor
		      el-get
		      rainbow-delimiters
		      smartparens
		      company
		      edit-server
		      icicles
		      slime
		      ;; JS tools
		      js2-mode
		      ;js3-mode
		      js2-refactor
		      skewer-mode
		      ;;ac-js2-mode
		      paredit-everywhere
		      jquery-doc
		      paradox
		      nim-mode
		      smex
		      god-mode
		      ample-theme
		      minimap
		      sublimity
		      tide
		      shell-pop
		      multi-term
		      neotree
		      emamux
		      ztree
		      web-mode
		      expand-region
		      magit
		      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;(require 'edit-server)
;(edit-server-start)

(require 'ido)
;(require 'expand-region)
(require 'smartparens-config)
(require 'rainbow-delimiters)
(require 'js2-refactor)
(require 'jquery-doc)
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)
;(require 'ac-js2-mode)

;; global modes

(projectile-global-mode)
(show-paren-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; TypeScript
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; org
(setq org-agenda-files (list "~/Dokumenty/CG/StoreCheck2"))

;; JS modes

(defun jk-js-modes ()
  (interactive)
  (skewer-mode)
  (paredit-everywhere-mode)
  (jquery-doc-setup)
  (smartparens-mode)
  (ac-js2-mode)
  ;;(ac-js2-mode)
  )

;(add-hook 'js-mode-hook 'js2-minor-mode)
;(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js2-mode-hook 'jk-js-modes)
(add-hook 'js3-mode-hook 'jk-js-modes)

(setq ac-js2-evaluate-calls t)

;(add-to-list 'slime-contribs 'slime-repl)
;(add-to-list 'slime-contribs 'slime-js) 

;;(setq ac-js2-external-libraries '("full/path/to/a-library.js"))

;; Clojure modes

(defun jk-clojure-modes ()
  (interactive)
		(cider-mode)
		(paredit-mode)
		;(clj-refactor-mode 1)
		(rainbow-delimiters-mode)
		(smartparens-mode)
)

;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'clojure-mode-hook 'jk-clojure-modes)
(add-hook 'cider-repl-mode-hook 'jk-clojure-modes)



(ido-mode t)
(put 'narrow-to-region 'disabled nil)

(tool-bar-mode -1)

;(load-theme 'wombat t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(add-to-list 'company-backends 'company-nim)

(setq backup-directory-alist `(("." . "~/.saves")))

;; el-get
;(unless (require 'el-get nil 'noerror)
;  (with-current-buffer
;      (url-retrieve-synchronously
;       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;    (goto-char (point-max))
;    (eval-print-last-sexp)))

;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;(el-get 'sync)

;; key bindings

(require 'expand-region)

(global-set-key (kbd "C-`") 'next-multiframe-window)
(global-set-key (kbd "<f1>") 'next-multiframe-window)
(global-set-key (kbd "<f2>") 'shell-pop)

(global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C") 'uncomment-region)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "<f3>") 'god-mode-all)


(add-hook 'paredit-mode-hook
	  (lambda () (local-set-key (kbd "C-0") 'paredit-forward-slurp-sexp)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(electric-indent-mode t)
 '(erc-email-userid "jkrysztofiak@pl.imshealth.com")
 '(erc-nick "jkrysztofiak")
 '(erc-user-full-name "Janusz Krysztofiak")
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (sublimity minimap ample-theme monokai-theme grandshell-theme solarized-theme god-mode smex nim-mode paradox jquery-doc paredit-everywhere skewer-mode js2-refactor js3-mode js2-mode slime icicles edit-server company smartparens rainbow-delimiters el-get iedit ggtags paredit ac-cider cider clojure-mode projectile)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
 ;; foreground color (yellow)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
