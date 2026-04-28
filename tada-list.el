;;; tada-list.el --- Track your accomplishments  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Holland

;; Author: Jeff Holland <jeff.holland@gmail.com>
;; Maintainer: Jeff Holland <jeff.holland@gmail.com>
;; Version: 0.7.0
;; Package-Requires: ((emacs "28.1") (org "9.6") (transient "0.4"))
;; Keywords: convenience, productivity
;; URL: https://github.com/jholland82/tada-list
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
;; v0.7 adds the discoverability layer:
;;   `tada-list-menu'  (`?' in the browse buffer) — a transient menu
;;   exposing every command, plus per-window filter shortcuts.
;;
;; Full command set:
;;   `tada-list-add-quick'           — title only, dated today.
;;   `tada-list-add'                 — full prompts (title, tags, date, description).
;;   `tada-list'                     — open the *Tada List* buffer.
;;   `tada-list-visit-entry'         — jump from a row to the heading.
;;   `tada-list-filter-by-tag'       (`t')
;;   `tada-list-filter-by-range'     (`r')  — today / week / month / year / custom
;;   `tada-list-filter-today'        — same-day entries.
;;   `tada-list-filter-this-week'    — current week (respects `calendar-week-start-day').
;;   `tada-list-filter-this-month'   — current calendar month.
;;   `tada-list-filter-this-year'    — current calendar year.
;;   `tada-list-clear-filters'       (`c')
;;   `tada-list-edit-entry'          (`e')
;;   `tada-list-delete-entry'        (`d')
;;   `tada-list-menu'                (`?')
;;
;; See PLAN.org for the v1.0 roadmap.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'transient)

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

(defun tada-list--insert-entry (title tags date description)
  "Insert an entry's serialized form at point.
TITLE is the heading text.  TAGS is a list of strings (or nil).
DATE is a YYYY-MM-DD string.  DESCRIPTION is the body text (may be
empty or nil)."
  (insert "* " title)
  (when tags
    (insert " " (org-make-tag-string tags)))
  (insert "\n[" date "]\n")
  (when (and description (not (string-empty-p description)))
    (insert description)
    (unless (string-suffix-p "\n" description)
      (insert "\n"))))

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
      (tada-list--insert-entry title tags date description))
    (save-buffer)))

(defun tada-list--replace-entry (marker title tags date description)
  "Replace the entry at MARKER with TITLE, TAGS, DATE, and DESCRIPTION.
Preserves the entry's position in the file."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((begin (point))
            (end (save-excursion
                   (outline-next-heading)
                   (point))))
        (delete-region begin end)
        (goto-char begin)
        (tada-list--insert-entry title tags date description)))
    (save-buffer)))

(defun tada-list--delete-entry-at-marker (marker)
  "Delete the entry at MARKER from its file."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((begin (point))
            (end (save-excursion
                   (outline-next-heading)
                   (point))))
        (delete-region begin end)))
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

(defun tada-list--read-description (&optional initial)
  "Prompt for an optional description in a popup buffer.
If INITIAL is non-nil, pre-fill the buffer with it.
Return the description string (possibly empty).  Signals quit if the
user aborts."
  (setq tada-list--description-result nil
        tada-list--description-window-config (current-window-configuration))
  (let ((buf (get-buffer-create "*tada-list description*")))
    (with-current-buffer buf
      (erase-buffer)
      (when initial (insert initial))
      (tada-list-description-mode)
      (goto-char (point-min)))
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
  "Capture a new accomplishment.
Prompt for title, tags, date, and an optional description."
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

(defvar-local tada-list--filter-tag nil
  "When non-nil, show only entries tagged with this string.")

(defvar-local tada-list--filter-range nil
  "When non-nil, a cons (START . END) of YYYY-MM-DD strings.
Either bound may be nil for open-ended.")

(defun tada-list--date<= (a b)
  "Non-nil if A is lexically less than or equal to B."
  (not (string< b a)))

(defun tada-list--filter (entries)
  "Apply active buffer-local filters to ENTRIES."
  (let ((result entries))
    (when tada-list--filter-tag
      (setq result (seq-filter
                    (lambda (e)
                      (member tada-list--filter-tag
                              (tada-list-entry-tags e)))
                    result)))
    (when tada-list--filter-range
      (let ((start (car tada-list--filter-range))
            (end (cdr tada-list--filter-range)))
        (setq result (seq-filter
                      (lambda (e)
                        (let ((d (tada-list-entry-date e)))
                          (and d
                               (or (null start) (tada-list--date<= start d))
                               (or (null end)   (tada-list--date<= d end)))))
                      result))))
    result))

(defun tada-list--refresh ()
  "Repopulate `tabulated-list-entries' from `tada-list-file', applying filters."
  (setq tabulated-list-entries
        (mapcar #'tada-list--row
                (tada-list--filter (tada-list--parse-entries)))))

(defun tada-list--update-mode-name ()
  "Reflect active filters in the mode-line."
  (let (parts)
    (when tada-list--filter-tag
      (push (format "tag:%s" tada-list--filter-tag) parts))
    (when tada-list--filter-range
      (push (format "%s..%s"
                    (or (car tada-list--filter-range) "")
                    (or (cdr tada-list--filter-range) ""))
            parts))
    (setq mode-name
          (if parts
              (format "Tada-List[%s]" (string-join (nreverse parts) " "))
            "Tada-List"))
    (force-mode-line-update)))

(defun tada-list--reapply ()
  "Re-parse, re-filter, redraw, and update the mode-line indicator."
  (tada-list--refresh)
  (tabulated-list-print t)
  (tada-list--update-mode-name))

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
(define-key tada-list-mode-map (kbd "t")   #'tada-list-filter-by-tag)
(define-key tada-list-mode-map (kbd "r")   #'tada-list-filter-by-range)
(define-key tada-list-mode-map (kbd "c")   #'tada-list-clear-filters)
(define-key tada-list-mode-map (kbd "e")   #'tada-list-edit-entry)
(define-key tada-list-mode-map (kbd "d")   #'tada-list-delete-entry)
(define-key tada-list-mode-map (kbd "?")   #'tada-list-menu)


;;;; Filter commands

(defun tada-list--all-tags ()
  "Return all tags currently used by parsed entries."
  (delete-dups
   (apply #'append
          (mapcar #'tada-list-entry-tags (tada-list--parse-entries)))))

(defun tada-list--week-range ()
  "Return (START . END) date strings for the current week.
Respects `calendar-week-start-day'."
  (let* ((today (current-time))
         (dow (nth 6 (decode-time today)))
         (offset (mod (- dow calendar-week-start-day) 7))
         (start (time-subtract today (days-to-time offset)))
         (end (time-add start (days-to-time 6))))
    (cons (format-time-string "%Y-%m-%d" start)
          (format-time-string "%Y-%m-%d" end))))

(defun tada-list--month-range ()
  "Return (START . END) date strings for the current calendar month."
  (let* ((now (decode-time))
         (month (nth 4 now))
         (year (nth 5 now))
         (last (calendar-last-day-of-month month year)))
    (cons (format "%04d-%02d-01" year month)
          (format "%04d-%02d-%02d" year month last))))

(defun tada-list--year-range ()
  "Return (START . END) date strings for the current calendar year."
  (let ((year (nth 5 (decode-time))))
    (cons (format "%04d-01-01" year)
          (format "%04d-12-31" year))))

(defun tada-list-filter-by-tag (tag)
  "Filter the browse buffer to entries tagged TAG."
  (interactive
   (let ((tags (tada-list--all-tags)))
     (unless tags (user-error "No tags in use yet"))
     (list (completing-read "Filter by tag: " tags nil t))))
  (setq tada-list--filter-tag tag)
  (tada-list--reapply))

(defun tada-list-filter-by-range ()
  "Filter the browse buffer to a date window or custom range."
  (interactive)
  (let ((choice (completing-read
                 "Range: "
                 '("today" "this-week" "this-month" "this-year" "custom")
                 nil t)))
    (setq tada-list--filter-range
          (pcase choice
            ("today" (let ((d (format-time-string "%Y-%m-%d")))
                       (cons d d)))
            ("this-week"  (tada-list--week-range))
            ("this-month" (tada-list--month-range))
            ("this-year"  (tada-list--year-range))
            ("custom"     (cons (org-read-date) (org-read-date))))))
  (tada-list--reapply))

(defun tada-list-filter-today ()
  "Filter the browse buffer to entries dated today."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (setq tada-list--filter-range (cons today today)))
  (tada-list--reapply))

(defun tada-list-filter-this-week ()
  "Filter the browse buffer to entries from the current week."
  (interactive)
  (setq tada-list--filter-range (tada-list--week-range))
  (tada-list--reapply))

(defun tada-list-filter-this-month ()
  "Filter the browse buffer to entries from the current calendar month."
  (interactive)
  (setq tada-list--filter-range (tada-list--month-range))
  (tada-list--reapply))

(defun tada-list-filter-this-year ()
  "Filter the browse buffer to entries from the current calendar year."
  (interactive)
  (setq tada-list--filter-range (tada-list--year-range))
  (tada-list--reapply))

(defun tada-list-clear-filters ()
  "Remove all active filters in the browse buffer."
  (interactive)
  (setq tada-list--filter-tag nil
        tada-list--filter-range nil)
  (tada-list--reapply))


;;;; Edit and delete

(defun tada-list--entry-at-point ()
  "Return the entry under point, or signal a `user-error'."
  (let ((entry (tabulated-list-get-id)))
    (unless entry
      (user-error "No entry at point"))
    (let ((marker (tada-list-entry-marker entry)))
      (unless (and marker (marker-buffer marker))
        (user-error "Entry has no valid location; press `g' to refresh")))
    entry))

(defun tada-list-edit-entry ()
  "Edit the entry under point.  Existing values are pre-filled in each prompt."
  (interactive)
  (let* ((entry (tada-list--entry-at-point))
         (marker (tada-list-entry-marker entry))
         (title (string-trim
                 (read-string "Title: " (tada-list-entry-title entry)))))
    (when (string-empty-p title)
      (user-error "Title cannot be empty"))
    (let* ((raw-tags (completing-read-multiple
                      "Tags (comma-separated, RET to keep): "
                      (tada-list--existing-tags)
                      nil nil
                      (string-join (tada-list-entry-tags entry) ",")))
           (tags (seq-filter (lambda (s) (not (string-empty-p s)))
                             (mapcar #'string-trim raw-tags)))
           (date (org-read-date nil nil nil nil
                                (tada-list-entry-date entry)))
           (description (tada-list--read-description
                         (tada-list-entry-description entry))))
      (tada-list--replace-entry marker title tags date description)
      (tada-list--reapply)
      (message "Updated: %s" title))))

(defun tada-list-delete-entry ()
  "Delete the entry under point after confirmation."
  (interactive)
  (let* ((entry (tada-list--entry-at-point))
         (title (tada-list-entry-title entry))
         (marker (tada-list-entry-marker entry)))
    (when (yes-or-no-p (format "Delete \"%s\"? " title))
      (tada-list--delete-entry-at-marker marker)
      (tada-list--reapply)
      (message "Deleted: %s" title))))

(defun tada-list-visit-entry ()
  "Open the tada-list file at the entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (marker (and entry (tada-list-entry-marker entry))))
    (unless (and marker (marker-buffer marker))
      (user-error "Entry has no valid location; press `g' to refresh"))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-fold-show-entry)))

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


;;;; Transient menu

;;;###autoload (autoload 'tada-list-menu "tada-list" nil t)
(transient-define-prefix tada-list-menu ()
  "Tada-list main menu."
  ["Capture"
   ("a" "Add (full prompts)"     tada-list-add)
   ("A" "Quick add (title only)" tada-list-add-quick)]
  ["Browse"
   ("l" "Open list"          tada-list)
   ("v" "Visit entry"        tada-list-visit-entry)]
  ["Filter"
   ("t" "By tag"             tada-list-filter-by-tag)
   ("r" "By range…"          tada-list-filter-by-range)
   ("T" "Today"              tada-list-filter-today)
   ("W" "This week"          tada-list-filter-this-week)
   ("M" "This month"         tada-list-filter-this-month)
   ("Y" "This year"          tada-list-filter-this-year)
   ("c" "Clear filters"      tada-list-clear-filters)]
  ["Edit"
   ("e" "Edit entry"         tada-list-edit-entry)
   ("d" "Delete entry"       tada-list-delete-entry)])

(provide 'tada-list)
;;; tada-list.el ends here
