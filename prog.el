(add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "M-o") 'nil)))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-list
        try-complete-file-name-partially
        try-complete-lisp-symbol-partially
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-complete-file-name
        try-expand-line
        try-complete-lisp-symbol))

(dolist (mode '(show-paren-mode
                whitespace-cleanup-mode
                corfu-mode
                ))
  (add-hook 'prog-mode-hook mode)
  (add-hook 'ledger-mode-hook mode))

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode)

(use-package whitespace
  :diminish
  :custom
  (whitespace-style '(face lines-tail)))

(use-package dumb-jump :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-aggressive t)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

(use-package python-x :ensure
  :config
  (python-x-setup)
  :bind
  (:map python-mode-map
        (("C-<return>" . python-shell-send-line-and-step)))
  :custom
  (python-section-delimiter "##")
)

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;; (use-package lsp-mode :ensure t
;;   ;; :hook (julia-mode . lsp)
;;   :bind (:map lsp-mode-map
;;               ("M-?" . lsp-find-references)
;;               ("M-." . lsp-find-definition)
;;               ("M-\"" . lsp-describe-thing-at-point))
;;   :commands lsp
;;   :config
;;   (setq lsp-eldoc-render-all nil)
;;   ;; (setq lsp-auto-configure nil)
;;   (defun ff/lsp-eldoc-advice (orig-fun &rest args)
;;     (let ((msg (car args)))
;;       (if msg
;;           (funcall orig-fun (->> msg (s-trim-left)
;;                                      (s-split "\n")
;;                                      (first))))))
;;   (advice-add 'lsp--eldoc-message :around #'ff/lsp-eldoc-advice))

;; (use-package lsp-ui :ensure t
;;   :commands lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-headerline-breadcrumb-enable nil)
;;   (lsp-ui-doc-enable nil))

;; (use-package lsp-julia
;;   :quelpa (lsp-julia :fetcher github :repo "non-Jedi/lsp-julia")
;;   :custom
;;   (lsp-julia-package-dir nil)
;;   (lsp-julia-default-environment "~/.julia/environments/v1.5"))

(use-package ess
  :ensure t
  :custom
  (ess-use-ido nil)
  (ess-eval-visibly 'nowait)
  (ess-use-flymake nil)
)


(require 're-builder)
(setq reb-re-syntax 'string)

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))

(use-package treesit
  :init
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (latex "https://github.com/latex-lsp/tree-sitter-latex")
     (r "https://github.com/r-lib/tree-sitter-r")
     (julia "https://github.com/tree-sitter/tree-sitter-julia")))
  ;; (if (and (zerop (mod (org-days-to-iso-week (org-today)) 2)) ; even weeks
  ;; 	   (= 6 (calendar-day-of-week (calendar-gregorian-from-absolute (org-today))))) ; saturdays
  ;;     (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))  
  )

