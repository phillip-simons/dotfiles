;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq doom-localleader-key ",")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; accept completion from copilot and fallback to company
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "Inter" :size 18)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 30))
;; Enable key-chord mode
(use-package! key-chord
              :config
              (key-chord-mode 1)
              ;; Bind "fd" to exit insert mode
              (key-chord-define evil-insert-state-map "fd" 'evil-normal-state))
(use-package! evil
              :config
              (evil-ex-define-cmd "q" 'kill-this-buffer)
              (evil-ex-define-cmd "wq" 'doom/save-and-kill-buffer)
              )

(use-package! copilot
              :hook (prog-mode . copilot-mode)
              :bind (:map copilot-completion-map
                          ("<tab>" . 'copilot-accept-completion)
                          ("TAB" . 'copilot-accept-completion)
                          ("C-TAB" . 'copilot-accept-completion-by-word)
                          ("C-<tab>" . 'copilot-accept-completion-by-word)))
(after! (evil copilot)
        ;; Define the custom function that either accepts the completion or does the default behavior
        (defun my/copilot-tab-or-default ()
          (interactive)
          (if (and (bound-and-true-p copilot-mode)
                   ;; Add any other conditions to check for active copilot suggestions if necessary
                   )
              (copilot-accept-completion)
            (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

        ;; Bind the custom function to <tab> in Evil's insert state
        (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))
;; Ensure org-roam is loaded before configuring keybindings


(after! org

        (setq org-confirm-babel-evaluate nil)
        (setq org-hide-emphasis-markers t)
        (setq org-agenda-window-setup (quote current-window))
        (setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))
        (setq org-log-done 'time)
        (setq org-src-tab-acts-natively t)
        (setq org-return-follows-link  t)
        (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
        (setq org-agenda-start-on-weekday nil)
        (setq org-agenda-skip-deadline-if-done t)
        (setq org-agenda-skip-scheduled-if-done t)
        (setq org-agenda-skip-timestamp-if-done t)
        (setq org-agenda-skip-unavailable-files t)
        (setq org-capture-templates
              '(

                ("n" "Notes")
                ("np" "Personal Note"
                 entry (file+headline "~/org/10-19_Personal/12_Notes/12.11_notes.org" "General Notes")
                 "** %?"
                 :empty-lines 0)

                ("nw" "Work Note"
                 entry (file+headline "~/org/20-29_Work/27_Notes/27.11_notes.org" "General Notes")
                 "** %?"
                 :empty-lines 0)

                ("t" "TODOs")
                ("tp" "Personal To-Do"
                 entry (file+headline "~/org/10-19_Personal/10_System/10.03_todos.org" "General Tasks")
                 "* TODO [#B] %?\n:Created: %T\n "
                 :empty-lines 0)
                ("tw" "Work To-Do"
                 entry (file+headline "~/org/20-29_Work/20_System/20.03_todos.org" "General Tasks")
                 "* TODO [#B] %?\n:Created: %T\n "
                 :empty-lines 0)
                ("tc" "Code To-Do"
                 entry (file+headline "~/org/20-29_Work/20_System/20.03_todos.org" "Code Related Tasks")
                 "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
                 :empty-lines 0)

                ("m" "Meeting"
                 entry (file+function "~/org/20-29_Work/24_Meetings/24.11_meetinglog.org" org-reverse-datetree-goto-date-in-file)
                 "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
                 :tree-type week
                 :clock-in t
                 :clock-resume t
                 :empty-lines 0)

                ("c" "Contacts" entry (file "~/org/00-09_System/01_Inbox/01.02_Contacts.org")
                 "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:")

                ("f" "Follow Up")
                ("fp" "Personal Follow Up" entry (file+olp "~/org/10-19_Personal/10_System/10.03_todos.org" "Follow Up")
                 "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
                ("fw" "Work Follow Up" entry (file+olp "~/org/20-29_Work/20_System/20.03_todos.org" "Follow Up")
                 "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
                ))

        (setq org-todo-keywords
              '((sequence "TODO(t)" "WAITING(w!)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "PAUSED(a!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
                ))

        (setq org-todo-keyword-faces
              '(
                ("TODO"        . (:foreground "GoldenRod"  :weight bold))
                ("WAITING"     . (:foreground "yellow1"    :weight bold))
                ("PLANNING"    . (:foreground "DeepPink"   :weight bold))
                ("IN-PROGRESS" . (:foreground "Cyan"       :weight bold))
                ("PAUSED"      . (:foreground "gray"       :weight bold))
                ("VERIFYING"   . (:foreground "DarkOrange" :weight bold))
                ("BLOCKED"     . (:foreground "Red"        :weight bold))
                ("DONE"        . (:foreground "LimeGreen"  :weight bold))
                ("OBE"         . (:foreground "LimeGreen"  :weight bold))
                ("WONT-DO"     . (:foreground "LimeGreen"  :weight bold))
                ))


        ;; Tags
        (setq org-tag-alist '(
                              ;; Meeting types
                              (:startgroup . nil)
                              ("team_meeting" . ?t)
                              ("1on1" . ?1)
                              ("all_hands" . ?h)
                              ("sync" . ?s)
                              (:endgroup . nil)

                              ;; Code TODOs tags
                              ("QA" . ?q)
                              ("backend" . ?k)
                              ("broken_code" . ?c)
                              ("infrastructure" . ?i)

                              ;; Special tags
                              ("CRITICAL" . ?x)
                              ("obstacle" . ?o)

                              ;; Meeting tags
                              ("general" . ?g)
                              ("meeting" . ?m)
                              ("misc" . ?z)
                              ("planning" . ?n)

                              ;; Work Log Tags
                              ("accomplishment" . ?a)

                              ;; Personal tags
                              ("hobby" . ?h)
                              ("personal" . ?p)

                              ;; Organization
                              (:startgroup . nil)
                              ("directory" . ?d)
                              ("file" . ?f)
                              ("link" . ?n)
                              (:endgroup . nil)
                              ))

        ;; Tag colors
        (setq org-tag-faces
              '(
                ("planning"  . (:foreground "mediumPurple1" :weight bold))
                ("backend"   . (:foreground "royalblue1"    :weight bold))
                ("frontend"  . (:foreground "forest green"  :weight bold))
                ("QA"        . (:foreground "sienna"        :weight bold))
                ("meeting"   . (:foreground "yellow1"       :weight bold))
                ("CRITICAL"  . (:foreground "red1"          :weight bold))
                )
              )
        ;; Agenda View "d"
        (defun air-org-skip-subtree-if-priority (priority)
          "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
          (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                (pri-value (* 1000 (- org-lowest-priority priority)))
                (pri-current (org-get-priority (thing-at-point 'line t))))
            (if (= pri-value pri-current)
                subtree-end
              nil)))

        (setq org-agenda-skip-deadline-if-done t)

        (setq org-agenda-custom-commands
              '(
                ;; Daily Agenda & TODOs
                ("d" "Daily agenda and all TODOs"

                 ;; Display items with priority A
                 ((tags "PRIORITY=\"A\""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-overriding-header "High-priority unfinished tasks:")))

                  ;; View 7 days in the calendar view
                  (agenda "" ((org-agenda-span 7)))

                  ;; Display items with priority B (really it is view all items minus A & C)
                  (alltodo ""
                           ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                           (air-org-skip-subtree-if-priority ?C)
                                                           (org-agenda-skip-if nil '(scheduled deadline))))
                            (org-agenda-overriding-header "ALL normal priority tasks:")))

                  ;; Display items with pirority C
                  (tags "PRIORITY=\"C\""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
                  )

                 ;; Don't compress things (change to suite your tastes)
                 ((org-agenda-compact-blocks nil)))
                ))
        (setq org-contacts-files '("~/org/00-09_System/01_Inbox/01.02_Contacts.org" "~/org/20-29_Work/23_People/23.11_Contacts.org" "~/org/10-19_Personal/17_People/17.11_Contacts.org"))
        (setq org-startup-indented t
              org-pretty-entities t)

        (setq-default evil-kill-on-visual-paste nil)
        (setq org-refile-use-outline-path t)
        (setq org-refile-allow-creating-parent-nodes 'confirm)
        ;; Function to match second-level headings under "* Projects"
        (defun my/org-projects-refile-target ()
          (save-excursion
            (org-back-to-heading t)
            (when (looking-at "^\\* Projects")
              (org-map-entries (lambda () (point)) nil 'tree))))

        (setq org-refile-targets
              '((nil :maxlevel . 1) ;; Top-level headings globally
                (my/org-projects-refile-target :level . 2))) ;; Second-level under "* Projects"
        )




;; (use-package! org-superstar
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿"))
;;   (setq org-superstar-item-bullet-alist '((?- . ?•)
;;                                           (?+ . ?➤))))
(use-package! mixed-pitch
              :hook
              (org-mode . mixed-pitch-mode)
              :config
              (setq mixed-pitch-set-height t))
(custom-set-faces
 '(org-document-title ((t (:height 1.5 :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0))))
 '(variable-pitch ((t (:family "Inter" :height 1.0))))
 '(fixed-pitch ((t (:family "JetBrainsMono Nerd Font" :height 1.0)))))
;; (use-package! org-beautify-theme
;;   :config
;;   (load-theme 'org-beautify t))
;; (use-package! visual-fill-column
;;   :hook (org-mode . visual-fill-column-mode)
;;   :config
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t))
;; (defun my/org-prettify-symbols ()
;;   (push '("[ ]" . "☐") prettify-symbols-alist)
;;   (push '("[X]" . "☑") prettify-symbols-alist)
;;   (push '("[-]" . "❍") prettify-symbols-alist)
;;   (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
;;   (push '("#+END_SRC" . "λ") prettify-symbols-alist)
;;   (push '("#+BEGIN_QUOTE" . "❝") prettify-symbols-alist)
;;   (push '("#+END_QUOTE" . "❞") prettify-symbols-alist))

;; (add-hook 'org-mode-hook 'my/org-prettify-symbols)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (use-package! mu4e
;;   :defer 20 ; Wait 20 seconds after startup
;;   :config
;;   (setq
;;    mu4e-maildir (expand-file-name "~/Maildir")
;;    mu4e-context-policy 'ask
;;    ;; Refresh mail
;;    mu4e-get-mail-command "mbsync -a"
;;    mu4e-update-interval 300 ; Update every 5 minutes
;;    mu4e-headers-auto-update t

;;    ;; HTML emails
;;    mu4e-view-show-images t
;;    mu4e-view-show-addresses t

;;    ;; Send mail
;;    message-send-mail-function 'smtpmail-send-it
;;    smtpmail-stream-type 'starttls
;;    smtpmail-smtp-server "smtp.gmail.com"
;;    smtpmail-smtp-service 587
;;    smtpmail-auth-credentials "~/.authinfo.gpg"
;;    smtpmail-debug-info t)

;;   (set-email-account! "Personal"
;;                       '((mu4e-sent-folder       . "/GmailPersonal/Sent")
;;                         (mu4e-drafts-folder     . "/GmailPersonal/Drafts")
;;                         (mu4e-trash-folder      . "/GmailPersonal/Trash")
;;                         (mu4e-refile-folder     . "/GmailPersonal/Archive")
;;                         (smtpmail-smtp-user     . "phillip@simons.gg")
;;                         (user-mail-address      . "phillip@simons.gg")    ;; only needed for mu < 1.4
;;                         (mu4e-compose-signature . "

;; Best,

;; #+begin_signature
;; --
;; Phillip Simons
;; 📧 [[mailto:phillip@simons.gg][phillip@simons.gg]]
;; 📞 +1 (214) 842-0054
;; #+end_signature"))
;;                       t)
;;   (set-email-account! "Work"
;;                       '((mu4e-sent-folder       . "/GmailWork/Sent")
;;                         (mu4e-drafts-folder     . "/GmailWork/Drafts")
;;                         (mu4e-trash-folder      . "/GmailWork/Trash")
;;                         (mu4e-refile-folder     . "/GmailWork/Archive")
;;                         (smtpmail-smtp-user     . "phillip@book.io")
;;                         (user-mail-address      . "phillip@book.io")    ;; only needed for mu < 1.4
;;                         (mu4e-compose-signature . "

;; Best,

;; #+begin_signature
;; --
;; Phillip Simons
;; 📧 [[mailto:phillip@book.io][phillip@book.io]]
;; 📞 +1 (214) 842-0054
;; #+end_signature"))
;;                       t)

;;   ;; Make sure mu4e uses the correct sendmail program
;;   (setq sendmail-program "/usr/bin/msmtp")
;;   (setq message-sendmail-f-is-evil t
;;         message-sendmail-extra-arguments '("--read-envelope-from")
;;         message-send-mail-function 'message-send-mail-with-sendmail)

;;   ;; Load mu4e
;;   (mu4e t))

;; (after! mu4e
;;   (setq sendmail-program (executable-find "msmtp")
;;  send-mail-function #'smtpmail-send-it
;;  message-sendmail-f-is-evil t
;;  message-sendmail-extra-arguments '("--read-envelope-from")
;;  message-send-mail-function #'message-send-mail-with-sendmail))
;; (setq +mu4e-gmail-accounts '(("phillip@simons.gg" . "/GmailPersonal")
;;           ("phillip@book.io" . "/GmailWork")))
;; (setq mu4e-index-cleanup nil
;;       mu4e-index-lazy-check t)
(setq org-crypt-key "1DB05176280B6F70 ")
