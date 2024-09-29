;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(
	("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
	;; ("GNU ELPA"     . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	;; ("MELPA Stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable")
	;; ("MELPA"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        )
      package-archive-priorities
      '(("MELPA Stable" . 0)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 10)))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; quelpa
(use-package quelpa
  :ensure t
  :init (setq quelpa-update-melpa-p nil))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defun load-user-file (file)
  (load-file (locate-user-emacs-file file)))

(load-user-file "ui.el")
(load-user-file "general.el")
(load-user-file "theme.el")
(load-user-file "org-mode.el")
(load-user-file "prog.el")
(load-user-file "non-prog.el")






