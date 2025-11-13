(use-package org
  :config
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.8)
  (dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
)

(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook #'org-display-inline-images)
(plist-put org-format-latex-options :scale 2)

(setq org-hide-emphasis-markers t)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "  ·")

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(use-package olivetti)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook 'org-modern-indent-mode 90))

(use-package org-bullets
  :config (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-modern
  :after org
  :custom
  (org-modern-star nil)
  (org-modern-list
   '((?+ . "•")
     (?- . "–")
     (?* . "•")))
  (org-modern-checkbox
   '((?X . "☑")
     (?- . "☐")
     (?\s . "☐")))
  (org-modern-todo t)
  (org-modern-todo-faces
   '(("TODO" :background "#444" :foreground "#eee" :weight bold)
     ("DONE" :background "#333" :foreground "#7f7" :weight normal)))
  (org-modern-tag t)
  (org-modern-timestamp t)
  (org-modern-block-name '("‣" . "‣"))
  (org-modern-block-fringe nil)
  (org-modern-table nil)
  (org-modern-priority nil)
  :config
  (global-org-modern-mode))

(provide 'org)
