;;; funcs.el --- pez-elfeed Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Philip Simons <phillip@simons.gg>
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

(defun helm-elfeed-search-for-unread-tag ()
  "Helm interface to search available tags in Elfeed and filter unread entries by the selected tag."
  (interactive)
  (let* ((tags (elfeed-db-get-all-tags))
         (tag (helm :sources (helm-build-sync-source "Elfeed Tags"
                               :candidates tags
                               :action (lambda (selected-tag)
                                         (elfeed-search-set-filter (format "+unread +%s" selected-tag))
                                         (elfeed-search-update :force))))))
    (when tag
      (elfeed-search-set-filter (format "+unread +%s" tag))
      (elfeed-search-update :force))))
