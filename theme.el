(use-package modus-themes :ensure t)
(use-package zenburn-theme :ensure t)

(defun xjc/adjust-theme (theme)
  (progn (set-face-attribute 'mode-line nil :box nil)
         (set-face-attribute 'mode-line-inactive nil :box nil)
         (set-face-attribute 'header-line nil :box nil)
         (set-face-attribute 'mode-line-highlight nil :box nil)
         (set-face-attribute 'aw-leading-char-face nil :height 1.0)
	 )
  )

(setq light-theme 'modus-operandi
      dark-theme 'zenburn)

(defun switch-to-theme (theme)
  (disable-theme (car custom-enabled-themes))
  (load-theme theme)
  (xjc/adjust-theme theme)
  )

(defun toggle-light-dark-themes ()
  (interactive)
  (if (eq (car custom-enabled-themes) light-theme)
      (switch-to-theme dark-theme)
    (switch-to-theme light-theme)
    )
  )

(keymap-global-set "<f8>" 'toggle-light-dark-themes)

(defun set-theme-based-on-time ()
  (let* ((hour (string-to-number (format-time-string "%H")))
	(theme (if (and (>= hour 6) (<= hour 22)) light-theme dark-theme)))
    (load-theme theme)
    (xjc/adjust-theme theme)))

(add-hook 'emacs-startup-hook 'set-theme-based-on-time)

