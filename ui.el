;; Minimal UI
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(blink-cursor-mode -1)
(fringe-mode 3)
(setq ring-bell-function 'ignore)
(setq scroll-preserve-screen-position t)
(set-face-attribute 'default nil
                    ;; :family "jetbrains mono"
                    :height 130
                    :weight 'normal
                    :width 'normal)
(set-face-attribute 'fringe nil)
