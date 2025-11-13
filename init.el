(setq inhibit-startup-message t)
(setq custom-safe-themes t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)

(set-language-environment "UTF-8")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

;; Configure package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))

(setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

;; Vertico for vertical completion for the emacs minibuffer.
(use-package vertico
  :init
  (vertico-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; A bunch of great search and navigation commands
(use-package consult
   :hook (completion-list-mode . consult-preview-at-point-mode)
   :custom
   (consult-preview-key nil)
   (consult-narrow-key nil)
   :config
   (consult-customize consult-theme consult-line consult-line-at-point :preview-key '(:debounce 0.2 any))
 )

;; Annotations in the minibuffer, i.e a description of the function next to the name in M-x
(use-package marginalia
  :init
  (marginalia-mode))

;; In buffer completions, think lsp completions
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ("C-n" . corfu-next)
        ([tab] . corfu-next)
        ("C-p" . corfu-previous)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

;; Completion style and fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt) ;; if a treesitter grammar can't be found for the language detected in the buffer, prompt me to install it
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) ;; if a treesitter grammar is found for the language detected in the buffer, use the corresponding language-ts-mode
  (global-treesit-auto-mode))

(use-package kanagawa-themes
  :init (load-theme 'kanagawa-wave))

(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd" "--background-index" "--clang-tidy")))
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd" "--background-index" "--clang-tidy"))))

(use-package which-key
    :config
    (which-key-mode))
