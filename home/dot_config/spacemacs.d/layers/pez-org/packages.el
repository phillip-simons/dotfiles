;;; packages.el --- pez-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author:  <phillip@archlinux>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `pez-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `pez-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `pez-org/pre-init-PACKAGE' and/or
;;   `pez-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst pez-org-packages
  '(
    dash-functional
    mixed-pitch
    org
    org-auto-tangle
    org-fancy-priorities
    org-reverse-datetree
    org-starter
    ox-ssh)
  "The list of Lisp packages required by the pez-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun pez-org/init-org-auto-tangle ()
  (use-package org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :diminish org-auto-tangle-mode)
  )

(defun pez-org/init-mixed-pitch ()
  (use-package mixted-pitch
    :defer t
    :hook (org-mode . mixed-pitch-mode)
    :diminish mixed-pitch-mode)
  )

(defun pez-org/init-org-fancy-priorities ()
  (use-package org-fancy-priorities
    :defer t
    :hook (org-mode        . org-fancy-priorities-mode)
    :hook (org-agenda-mode . org-fancy-priorities-mode)
    :diminish org-fancy-priorities-mode
    :config
    (setq org-fancy-priorities-list `(,(all-the-icons-faicon "flag"     :height 1.1 :v-adjust 0.0)
                                      ,(all-the-icons-faicon "arrow-up" :height 1.1 :v-adjust 0.0)
                                      ,(all-the-icons-faicon "square"   :height 1.1 :v-adjust 0.0)
                                      ,(all-the-icons-faicon "bed" :height 1.1 :v-adjust 0.0)
                                      ))
    (setq org-priority-faces
          '((?A :foreground "#ff6c6b" :weight bold)
            (?B :foreground "#ecbe78" :weight bold)
            (?C :foreground "#98be65" :weight bold)
            (?D :foreground "#51afef" :weight bold)
            )))
  )

(defun pez-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAITING(w!)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "PAUSED(a!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" ))
          )
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
    :config
    (setq org-hide-emphasis-markers t)
    (setq org-use-sub-superscripts "{}")
    (setq org-agenda-window-setup (quote current-window))
    (setq org-agenda-files
          (append (directory-files-recursively "~/org/" "\.org$")
                  (directory-files-recursively "~/.config/spacemacs.d/cache/" "\.org$")))
    (setq org-log-done 'time)
    (setq org-highest-priority ?A
          org-default-priority ?B
          org-lowest-priority ?D)
    (setq org-src-tab-acts-natively t)
    (setq org-return-follows-link  t)
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-timestamp-if-done t)
    (setq org-agenda-skip-unavailable-files t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-contacts-files '("~/org/00-09_System/01_Inbox/01.02_Contacts.org" "~/org/20-29_Work/23_People/23.11_Contacts.org" "~/org/10-19_Personal/17_People/17.11_Contacts.org"))
    (setq org-startup-indented t
          org-ellipsis " ▼ "
          org-superstar-prettify-item-bullets t
          org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))
          org-pretty-entities t)
    (setq org-export-with-toc nil)
    (setq org-export-with-sub-superscripts '{} )
    (setq org-refile-use-outline-path t)
    (setq org-refile-allow-creating-parent-nodes 'confirm)

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

    (defun pez-org/skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (pri-value (* 1000 (- org-lowest-priority priority)))
            (pri-current (org-get-priority (thing-at-point 'line t))))
        (if (= pri-value pri-current)
            subtree-end
          nil)))

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
                       ((org-agenda-skip-function '(or (pez-org/skip-subtree-if-priority ?A)
                                                       (pez-org/skip-subtree-if-priority ?C)
                                                       (pez-org/skip-subtree-if-priority ?D)
                                                       (org-agenda-skip-if nil '(scheduled deadline))))
                        (org-agenda-overriding-header "ALL normal priority tasks:")))

              ;; Display items with pirority C
              (tags "PRIORITY=\"C\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "Low-priority Unfinished tasks:")))

              (tags "PRIORITY=\"D\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "Backlog:")))
              )

             ;; Don't compress things (change to suite your tastes)
             ((org-agenda-compact-blocks nil)))
            ))
    )
  )

(defun pez-org/pre-init-ox-ssh ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-ssh)))
(defun pez-org/init-ox-ssh ())

(defun pez-org/pre-init-dash-functional ()
  (spacemacs|use-package-add-hook org :pre-config (require 'dash-functional)))
(defun pez-org/init-dash-functional ())

(defun pez-org/pre-init-org-reverse-datetree ()
  (spacemacs|use-package-add-hook org :pre-config (require 'org-reverse-datetree)))
(defun pez-org/init-org-reverse-datetree ())

(defun pez-org/pre-init-org-starter ()
  (spacemacs|use-package-add-hook org :pre-config (require 'org-starter)))
(defun pez-org/init-org-starter ())
