;;; tada-list.el --- Track your accomplishments  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Holland

;; Author: Jeff Holland <jeff.holland@gmail.com>
;; Maintainer: Jeff Holland <jeff.holland@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1") (org "9.6") (transient "0.4"))
;; Keywords: convenience, productivity
;; URL: https://github.com/jeff-holland/tada-list
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; tada-list is the inverse of a to-do list: a place to record what you
;; have accomplished.  Entries have a title, optional description,
;; optional tags, and a date, and are stored as Org headings in a single
;; file (`tada-list-file').
;;
;; v0.4 adds the browse buffer:
;;   `tada-list'              — open the *Tada List* buffer.
;;   `tada-list-visit-entry'  — jump from a row to the heading in the org file.
;;
;; Capture commands:
;;   `tada-list-add-quick' — title only, dated today.
;;   `tada-list-add'       — full prompts (title, tags, date, description).
;;
;; See PLAN.org for the v1.0 roadmap.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)

(defgroup tada-list nil
  "Record and review personal accomplishments."
  :group 'convenience
  :prefix "tada-list-")

(defcustom tada-list-file "~/tada-list.org"
  "File where accomplishments are stored.
Each accomplishment is a level-1 Org heading."
  :type 'file
  :group 'tada-list)


;;;; File and entry helpers

(defun tada-list--file ()
  "Return `tada-list-file' as an expanded absolute path."
  (expand-file-name tada-list-file))

(defun tada-list--ensure-file ()
  "Create the tada-list file (and its parent directory) if missing."
  (let* ((path (tada-list--file))
         (dir (file-name-directory path)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-exists-p path)
      (with-temp-file path
        (insert "#+TITLE: Tada List\n\n")))))

(defun tada-list--existing-tags ()
  "Return the list of tags currently used in `tada-list-file'."
  (tada-list--ensure-file)
  (with-current-buffer (find-file-noselect (tada-list--file))
    (mapcar #'car (org-get-buffer-tags))))

(defun tada-list--append-entry (title tags date description)
  "Append an accomplishment to `tada-list-file'.
TITLE is a non-empty string.  TAGS is a list of strings (or nil).
DATE is a YYYY-MM-DD string.  DESCRIPTION is a string (may be empty
or nil)."
  (tada-list--ensure-file)
  (with-current-buffer (find-file-noselect (tada-list--file))
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " title)
      (when tags
        (insert " " (org-make-tag-string tags)))
      (insert "\n[" date "]\n")
      (when (and description (not (string-empty-p description)))
        (insert description)
        (unless (string-suffix-p "\n" description)
          (insert "\n"))))
    (save-buffer)))


;;;; Description popup buffer

;; The description editor uses `recursive-edit' so the calling capture
;; command blocks until the user finishes or aborts.  These two dynamic
;; vars carry state across the recursive edit; they are only valid while
;; `tada-list--read-description' is on the stack.
(defvar tada-list--description-result nil)
(defvar tada-list--description-window-config nil)

(defvar tada-list-description-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'tada-list-description-finish)
    (define-key map (kbd "C-c C-k") #'tada-list-description-abort)
    map)
  "Keymap for `tada-list-description-mode'.")

(define-derived-mode tada-list-description-mode org-mode "Tada-Description"
  "Major mode for editing a tada-list entry's description."
  (setq header-line-format
        (substitute-command-keys
         "Finish `\\[tada-list-description-finish]', abort `\\[tada-list-description-abort]'.  Empty buffer = no description.")))

(defun tada-list-description-finish ()
  "Finish editing the description and return to the capture flow."
  (interactive)
  (setq tada-list--description-result (string-trim (buffer-string)))
  (let ((buf (current-buffer)))
    (when tada-list--description-window-config
      (set-window-configuration tada-list--description-window-config))
    (kill-buffer buf))
  (exit-recursive-edit))

(defun tada-list-description-abort ()
  "Abort description editing, cancelling the whole capture."
  (interactive)
  (let ((buf (current-buffer)))
    (when tada-list--description-window-config
      (set-window-configuration tada-list--description-window-config))
    (kill-buffer buf))
  (abort-recursive-edit))

(defun tada-list--read-description ()
  "Prompt for an optional description in a popup buffer.
Return the description string (possibly empty).  Signals quit if the
user aborts."
  (setq tada-list--description-result nil
        tada-list--description-window-config (current-window-configuration))
  (let ((buf (get-buffer-create "*tada-list description*")))
    (with-current-buffer buf
      (erase-buffer)
      (tada-list-description-mode))
    (pop-to-buffer buf)
    (recursive-edit))
  tada-list--description-result)


;;;; Parser

(cl-defstruct tada-list-entry
  "A single accomplishment record parsed from `tada-list-file'."
  title        ; string, heading text without tags
  tags         ; list of strings, may be nil
  date         ; "YYYY-MM-DD" string, or nil if missing/malformed
  description  ; string, may be empty
  marker)      ; marker at the heading position, for jump-to-entry

(defun tada-list--parse-entries ()
  "Return all entries in `tada-list-file' as `tada-list-entry' structs.
Entries are returned in file order."
  (tada-list--ensure-file)
  (with-current-buffer (find-file-noselect (tada-list--file))
    (org-with-wide-buffer
     (let (entries)
       (org-map-entries
        (lambda ()
          (when (= 1 (org-current-level))
            (push (tada-list--parse-entry-at-point) entries))))
       (nreverse entries)))))

(defun tada-list--parse-entry-at-point ()
  "Build a `tada-list-entry' for the heading at point."
  (let* ((title (org-get-heading t t t t))
         (tags (org-get-tags nil t))
         (marker (point-marker))
         (body (tada-list--entry-body))
         (split (tada-list--split-body body)))
    (make-tada-list-entry
     :title title
     :tags tags
     :date (car split)
     :description (cdr split)
     :marker marker)))

(defun tada-list--entry-body ()
  "Return the body text under the heading at point.
Excludes the heading line; stops at the next heading or end of buffer."
  (save-excursion
    (let ((begin (progn (end-of-line) (forward-line 1) (point)))
          (end (save-excursion
                 (outline-next-heading)
                 (point))))
      (buffer-substring-no-properties begin end))))

(defconst tada-list--date-line-regexp
  "\\`\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?: [^]]+\\)?\\]\\'"
  "Match an inactive Org timestamp on a line by itself; group 1 is the date.")

(defun tada-list--split-body (body)
  "Split BODY into (DATE . DESCRIPTION).
DATE is a YYYY-MM-DD string parsed from a leading inactive timestamp,
or nil if the first non-blank line isn't one.  DESCRIPTION is whatever
follows, trimmed."
  (let* ((lines (split-string body "\n"))
         (first (string-trim (or (car lines) ""))))
    (if (string-match tada-list--date-line-regexp first)
        (cons (match-string 1 first)
              (string-trim (string-join (cdr lines) "\n")))
      (cons nil (string-trim body)))))


;;;; Capture commands

;;;###autoload
(defun tada-list-add-quick (title)
  "Append accomplishment TITLE to `tada-list-file', dated today."
  (interactive "sAccomplishment: ")
  (let ((trimmed (string-trim title)))
    (when (string-empty-p trimmed)
      (user-error "Title cannot be empty"))
    (tada-list--append-entry trimmed nil
                             (format-time-string "%Y-%m-%d")
                             nil)
    (message "tada! recorded: %s" trimmed)))

;;;###autoload
(defun tada-list-add ()
  "Capture a new accomplishment with prompts for tags, date, and description."
  (interactive)
  (let* ((title (string-trim (read-string "Accomplishment: ")))
         (_ (when (string-empty-p title)
              (user-error "Title cannot be empty")))
         (raw-tags (completing-read-multiple
                    "Tags (comma-separated, RET for none): "
                    (tada-list--existing-tags) nil nil))
         (tags (seq-filter (lambda (s) (not (string-empty-p s)))
                           (mapcar #'string-trim raw-tags)))
         (date (org-read-date))
         (description (tada-list--read-description)))
    (tada-list--append-entry title tags date description)
    (message "tada! recorded: %s%s"
             title
             (if tags (format " (%s)" (string-join tags ", ")) ""))))


;;;; Browse buffer

(defconst tada-list--buffer-name "*Tada List*")

(defun tada-list--row (entry)
  "Build a tabulated-list row for ENTRY.
The row's ID is the entry struct itself, so commands can access it
via `tabulated-list-get-id'."
  (list entry
        (vector (or (tada-list-entry-date entry) "")
                (tada-list-entry-title entry)
                (string-join (tada-list-entry-tags entry) ", "))))

(defun tada-list--refresh ()
  "Repopulate `tabulated-list-entries' from `tada-list-file'."
  (setq tabulated-list-entries
        (mapcar #'tada-list--row (tada-list--parse-entries))))

(define-derived-mode tada-list-mode tabulated-list-mode "Tada-List"
  "Major mode for browsing accomplishments."
  (setq tabulated-list-format
        [("Date"  12 t)
         ("Title" 50 t)
         ("Tags"  30 t)])
  ;; Newest first.
  (setq tabulated-list-sort-key '("Date" . t))
  (add-hook 'tabulated-list-revert-hook #'tada-list--refresh nil t)
  (tabulated-list-init-header))

(define-key tada-list-mode-map (kbd "RET") #'tada-list-visit-entry)

(defun tada-list-visit-entry ()
  "Open the tada-list file at the entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (marker (and entry (tada-list-entry-marker entry))))
    (unless (and marker (marker-buffer marker))
      (user-error "Entry has no valid location; press `g' to refresh"))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-show-entry)))

;;;###autoload
(defun tada-list ()
  "Open the tada-list browse buffer."
  (interactive)
  (let ((buf (get-buffer-create tada-list--buffer-name)))
    (with-current-buffer buf
      (tada-list-mode)
      (tada-list--refresh)
      (tabulated-list-print))
    (pop-to-buffer-same-window buf)))

(provide 'tada-list)
;;; tada-list.el ends here
