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
      doom-variable-pitch-font (font-spec :family "IBMPlexSans" :size 18)
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
(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-directory "~/org/roam")
  :config
  (org-roam-db-autosync-mode))

(map!
 :leader
 :prefix ("n" . "notes")
 :desc "Toggle org-roam buffer" "l" #'org-roam-buffer-toggle
 :desc "Find org-roam node" "f" #'org-roam-node-find
 :desc "Insert org-roam node" "i" #'org-roam-node-insert
 :desc "Capture to roam" "c" #'org-roam-capture)


(after! org
  (setq org-capture-templates
        '(("m" "Meeting Notes" entry
           (file+olp+datetree "~/org/meetings.org")
           "* %^{Person} Meeting :meeting:\n:PROPERTIES:\n:Person: %^{Person}\n:Date: %T\n:END:\n** Notes\n%?\n** Actions\n- [ ] \n"
           :prepend t
           :empty-lines 1))))

(after! org
  ;; Set the file where your contacts will be stored
  (setq org-contacts-files '("~/org/contacts.org"))

  (add-to-list 'org-capture-templates
               '("c" "Contacts" entry (file "~/org/contacts.org")
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
:END:")))

(use-package! mu4e
  :defer 20 ; Wait 20 seconds after startup
  :config
  (setq
   ;; General settings
   mu4e-maildir (expand-file-name "~/Maildir")
   mu4e-sent-folder "/gmail/Sent"
   mu4e-drafts-folder "/gmail/Drafts"
   mu4e-trash-folder "/gmail/Trash"
   mu4e-refile-folder "/gmail/All Mail"

   ;; Contexts (if you have multiple email accounts)
   mu4e-contexts
   `( ,(make-mu4e-context
        :name "Gmail"
        :match-func (lambda (msg) (when msg
                                    (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "phillip@book.io")
                (user-full-name    . "Phillip Simons")
                (mu4e-compose-signature . "Phillip Simons"))))

   ;; Refresh mail
   mu4e-get-mail-command "mbsync -a"
   mu4e-update-interval 300 ; Update every 5 minutes
   mu4e-headers-auto-update t

   ;; HTML emails
   mu4e-view-show-images t
   mu4e-view-show-addresses t

   ;; Send mail
   message-send-mail-function 'smtpmail-send-it
   smtpmail-stream-type 'starttls
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   smtpmail-auth-credentials "~/.authinfo.gpg"
   smtpmail-debug-info t)

  ;; Make sure mu4e uses the correct sendmail program
  (setq sendmail-program "/usr/bin/msmtp")
  (setq message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; Load mu4e
  (mu4e t))

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
 '(variable-pitch ((t (:family "IBMPlexSans" :height 1.0))))
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
(use-package! org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-type 'weekly
        org-journal-enable-agenda-integration t))

(setq org-agenda-files (append org-agenda-files (directory-files-recursively "~/org/journal/" "\\.org$")))
(setq org-crypt-key "1DB05176280B6F70 ")
