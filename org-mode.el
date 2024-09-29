(setq initial-buffer-choice "~/org/jot.org")

(use-package org
  :ensure t
  :demand
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c o" . org-open-at-point)
  (:map org-agenda-mode-map
                ("C-t" . org-todo-yesterday))
  ;; ("<f11>" . org-timer-start)
  ;; ("C-<f11>" . org-timer)
  ;; ("<f12>" . org-timer-stop)
  :config
  (require 'org-agenda)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (set-face-attribute 'org-checkbox nil :box nil)
  (setq org-agenda-clockreport-parameter-plist
	(plist-put org-agenda-clockreport-parameter-plist :fileskip0 t))
  (defun org-clock-toggle ()
    (if (org-clocking-p)
	(org-clock-out)
      (org-clock-in-last)))
  (require 'org-habit)
  ;; ;; redefine so org-entry-get check inheritance
  ;; (defun org-is-habit-p (&optional pom)
  ;;   "Is the task at POM or point a habit?"
  ;;   (string= "habit" (org-entry-get (or pom (point)) "STYLE" t)))
  ;; configure "sampling"
  (defun my-random-next-date (days-range) ;
    "Calculate a random date using DAYS-RANGE of the form [a, b)"
    (let* ((min-days (aref days-range 0))
	   (max-days (aref days-range 1))
	   (random-days (+ min-days (random (- max-days min-days))))
	   (next-time (time-add (current-time) (days-to-time random-days))))
      (format-time-string "%Y-%m-%d" next-time)))
  (defun update-random-event-on-done ()
    "Reschedule task with a random date when marked as DONE, using specified days range."
    (when (string-equal org-state "DONE")
      (let* ((days-range-str (org-entry-get nil "SAMPLE"))
	     (days-range (if days-range-str (read days-range-str) nil)))
	(when days-range
	  (let ((org-log-done nil))
	    (org-add-log-setup 'state "DONE" "TODO" 'time)
	    (org-todo "TODO")
	    (org-schedule nil (my-random-next-date days-range)))))))
  (add-hook 'org-after-todo-state-change-hook 'update-random-event-on-done)

  :custom
  (org-habit-graph-column 80)
  (org-agenda-start-on-weekday nil)
  (org-default-notes-file "~/org/general.org")
  (org-agenda-files '("~/org"))
  (org-log-done 'time)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-capture-templates
   '(("t" "[T]ODO" entry (file org-default-notes-file)
      "* TODO %?\n")
     ("c" "[C]LARIFY" entry (file org-default-notes-file)
      "* CLARIFY %?\n")
     ("w" "[W]ait" entry (file org-default-notes-file)
      "* WAIT %?\n")
     ("i" "D[I]SCUSS" entry (file org-default-notes-file)
      "* DISCUSS %?\n")
     ("n" "[n]ote" entry (file org-default-notes-file)
      "* %?\n")
     ("l" "Task (with link)" entry (file org-default-notes-file)
      "* TODO %?\n%a\n")
     ))
  (org-list-allow-alphabetical t)
  ;; (org-hide-leading-stars t)
  (org-extend-today-until 4)
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  ;; (org-startup-indented t)
  (org-log-into-drawer t)
  (org-catch-invisible-edits 'show-and-error)
  (org-agenda-custom-commands
   '(("n" "calendar + wait + todo + clarify"
      ((agenda "" ((org-habit-show-habits nil)))
       (todo "WIP")
       (tags "-SCHEDULED={.}/TODO")
       (todo "CLARIFY")
       (tags "-SCHEDULED={.}/WAIT")
       (todo "TBD")
       (agenda "habits" ((org-habit-show-habits t)
			 (org-habit-show-all-today t)
			 (org-agenda-span 'day)
			 (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit"))
			 ))
       ))))
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.x?html?\\'" . "google-chrome-stable %s")
     ("pdf" . "zathura \"%s\"")
     ("\\.je?pg\\'" . "imv \"%s\"")
     ("\\.png\\'" . "imv \"%s\"")
     ))
  (org-clock-mode-line-total 'today)
  (org-priority-start-cycle-with-default nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "WIP(o)" "WAIT(w)" "CLARIFY(c)" "TBD(m)" "DISCUSS(i)" "|" "DONE(d)"))
   )
  (org-use-fast-todo-selection 'expert)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-image-actual-width 500)
  (calendar-week-start-day 1)
  )

;; (use-package org-edna :ensure t
;;   :diminish
;;   :init
;;   (org-edna-mode))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "" :target
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d %a>"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         ("c" . org-roam-dailies-capture-date)
         ("v" . org-roam-dailies-goto-date) 
         )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (add-hook 'org-roam-dailies-find-file-hook #'flyspell-mode)
  (defun repeatize (keymap)
    "Add `repeat-mode' support to a KEYMAP."
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
	 (put cmd 'repeat-map keymap)))
     (symbol-value keymap)))
  (repeatize 'org-roam-dailies-map)
  )

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "jingchengx/org-ql")
  :bind (("C-c q f" . org-ql-find-in-agenda)
	 ("C-c q i" . xjc/org-ql-clock-in))
  :custom (org-ql-warn-no-heading nil)
  :config
  (defun xjc/org-ql-clock-in ()
    (interactive)
    (let ((marker (org-ql-completing-read (org-agenda-files)
                    :prompt "Clock into: ")))
      (set-buffer (marker-buffer marker))
      (goto-char marker)
      (org-clock-in)))
  )

(use-package hammy
  :quelpa (hammy :fetcher github :repo "jingchengx/hammy.el" :branch "in-use")
  :custom
  (hammy-mode-lighter-prefix "")
  (hammy-mode-always-show-lighter nil)
  :bind
  ("M-<f5>" . hammy-start-org-clock-in)
  ("C-<f5>" . hammy-start)
  ("C-<f6>" . hammy-stop)
  ("C-<f7>" . hammy-next)
  ("C-<f8>" . hammy-toggle)
  :init
  (hammy-mode)
  )

