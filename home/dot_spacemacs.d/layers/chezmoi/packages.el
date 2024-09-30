(defconst chezmoi-packages
  '(
    (chezmoi :location (recipe
                        :fetcher github
                        :repo "tuh8888/chezmoi.el"
                        :files ("*.el" "extensions/*.el"))))
  "The list of Lisp packages required by the chezmoi layer.")


(defun chezmoi/init-chezmoi ()
  (use-package chezmoi
    :init
    (spacemacs/declare-prefix "f z" "chezmoi")

    (spacemacs/set-leader-keys
      "f z s" #'chezmoi-write
      "f z g" #'chezmoi-magit-status
      "f z d" #'chezmoi-diff
      "f z e" #'chezmoi-ediff
      "f z f" #'chezmoi-find
      "f z i" #'chezmoi-write-files
      "f z o" #'chezmoi-open-other
      "f z t" #'chezmoi-template-buffer-display
      "f z c" #'chezmoi-mode)

    (when (equalp dotspacemacs-editing-style 'vim)
      (defun chezmoi--evil-insert-state-enter ()
        "Run after evil-insert-state-entry."
        (chezmoi-template-buffer-display nil (point))
        (remove-hook 'after-change-functions #'chezmoi-template--after-change 1))

      (defun chezmoi--evil-insert-state-exit ()
        "Run after evil-insert-state-exit."
        (chezmoi-template-buffer-display nil)
        (chezmoi-template-buffer-display t)
        (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1))

      (defun chezmoi-evil ()
        (if chezmoi-mode
            (progn
              (add-hook 'evil-insert-state-entry-hook #'chezmoi--evil-insert-state-enter nil 1)
              (add-hook 'evil-insert-state-exit-hook #'chezmoi--evil-insert-state-exit nil 1))
          (progn
            (remove-hook 'evil-insert-state-entry-hook #'chezmoi--evil-insert-state-enter 1)
            (remove-hook 'evil-insert-state-exit-hook #'chezmoi--evil-insert-state-exit 1))))
      (add-hook 'chezmoi-mode-hook #'chezmoi-evil))


    (setq chezmoi-template-display-p t) ;; Display template values in all source buffers.

    (require 'chezmoi-company)
    (add-hook 'chezmoi-mode-hook #'(lambda () (if chezmoi-mode
                                                  (add-to-list 'company-backends 'chezmoi-company-backend)
                                                (setq company-backends (delete 'chezmoi-company-backend company-backends)))))

    ;; Turn off ligatures cuz they look bad.
    (add-hook 'chezmoi-mode-hook #'(lambda () (ligature-mode (if chezmoi-mode 0 1))))

    ;; I find this hook useful for my emacs config files generated through org-tangle.
    (defun chezmoi-org-babel-tangle ()
      (when-let ((fle (chezmoi-target-file (buffer-file-name))))
        (chezmoi-write file)))
    (add-hook 'org-babel-post-tangle-hook #'chezmoi-org-babel-tangle)))
