;;; packages.el --- pez-email layer packages file for Spacemacs.
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
;; added to `pez-email-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `pez-email/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `pez-email/pre-init-PACKAGE' and/or
;;   `pez-email/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst pez-email-packages
  '(
    mu4easy
    persp-mode
    )
  "The list of Lisp packages required by the pez-email layer.

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

(defun pez-email/post-init-persp-mode ()
  (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
    :binding mu4e-spacemacs-layout-binding
    :body
    (progn
      (defun spacemacs-layouts/add-mu4e-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           mu4e-spacemacs-layout-name)))
      (spacemacs/add-to-hooks 'spacemacs-layouts/add-mu4e-buffer-to-persp
                              '(mu4e-main-mode-hook
                                mu4e-headers-mode-hook
                                mu4e-view-mode-hook
                                org-msg-edit-mode
                                mu4e-compose-mode-hook))
      (call-interactively 'mu4e)
      (call-interactively 'mu4e-update-index)

      (define-advice mu4e~stop (:after nil kill-mu4e-layout-after-mu4e~stop)
        (when mu4e-spacemacs-kill-layout-on-exit
          (persp-kill mu4e-spacemacs-layout-name)))))
  )
(defun pez-email/init-mu4easy ()
  (use-package mu4easy
    :ensure t
    :config
    (setq mail-user-agent 'mu4e-user-agent )
    (spacemacs/set-leader-keys-for-major-mode 'org-msg-edit-mode
      dotspacemacs-major-mode-leader-key 'org-ctrl-c-ctrl-c
      "c" 'org-ctrl-c-ctrl-c
      "k" 'message-kill-buffer
      "a" 'org-msg-attach
      "s" 'message-goto-subject
      "b" 'org-msg-goto-body
      "e" 'org-msg-preview
      "d" 'message-dont-send)
    (setq mu4easy-greeting "Howdy%s,\n\n")
    (setq mu4easy-contexts '((mu4easy-context
                              :c-name  "Personal"
                              :name    "Phillip Simons"
                              :maildir "GmailPersonal"
                              :mail    "phillip@simons.gg"
                              :smtp    "smtp.gmail.com"
                              :sent-action delete
                              :sig "

Best,

#+begin_signature
--
Phillip Simons
📧 [[mailto:phillip@simons.gg][phillip@simons.gg]]
📞 +1 (214) 842-0054
#+end_signature")
                             (mu4easy-context
                              :c-name  "Work"
                              :name    "Phillip Simons"
                              :maildir "GmailWork"
                              :mail    "phillip@book.io"
                              :smtp    "smtp.gmail.com"
                              :sent-action delete
                              :sig "

Best,

#+begin_signature
--
*Phillip Simons*
Book.io | Stuff.io
📧 [[mailto:phillip@book.io][phillip@book.io]]
📞 +1 (214) 842-0054
#+end_signature")))
    (mu4easy-mode)
    )
  )
