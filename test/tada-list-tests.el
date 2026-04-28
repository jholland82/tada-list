;;; tada-list-tests.el --- Tests for tada-list  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for tada-list.  Run with: make test

;;; Code:

(require 'ert)
(require 'tada-list)


;;;; Body splitting

(ert-deftest tada-list-tests/split-body-parses-date-and-description ()
  (pcase-let ((`(,date . ,desc)
               (tada-list--split-body "[2026-04-26]\nGot it done.\n")))
    (should (equal date "2026-04-26"))
    (should (equal desc "Got it done."))))

(ert-deftest tada-list-tests/split-body-no-date ()
  (pcase-let ((`(,date . ,desc)
               (tada-list--split-body "just notes here\n")))
    (should (null date))
    (should (equal desc "just notes here"))))

(ert-deftest tada-list-tests/split-body-multiline-description ()
  (pcase-let ((`(,date . ,desc)
               (tada-list--split-body "[2026-04-26]\nLine one.\nLine two.\n")))
    (should (equal date "2026-04-26"))
    (should (equal desc "Line one.\nLine two."))))

(ert-deftest tada-list-tests/split-body-empty ()
  (pcase-let ((`(,date . ,desc) (tada-list--split-body "")))
    (should (null date))
    (should (equal desc ""))))

(ert-deftest tada-list-tests/split-body-date-only ()
  (pcase-let ((`(,date . ,desc) (tada-list--split-body "[2026-04-26]\n")))
    (should (equal date "2026-04-26"))
    (should (equal desc ""))))


;;;; Date comparison

(ert-deftest tada-list-tests/date<= ()
  (should (tada-list--date<= "2026-04-25" "2026-04-26"))
  (should (tada-list--date<= "2026-04-26" "2026-04-26"))
  (should-not (tada-list--date<= "2026-04-27" "2026-04-26")))


;;;; Date ranges

(ert-deftest tada-list-tests/month-range-format ()
  (let ((range (tada-list--month-range)))
    (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-01\\'" (car range)))
    (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                            (cdr range)))
    (should (tada-list--date<= (car range) (cdr range)))))

(ert-deftest tada-list-tests/year-range-format ()
  (let ((range (tada-list--year-range)))
    (should (string-match-p "\\`[0-9]\\{4\\}-01-01\\'" (car range)))
    (should (string-match-p "\\`[0-9]\\{4\\}-12-31\\'" (cdr range)))))

(ert-deftest tada-list-tests/week-range-format-and-order ()
  (let ((range (tada-list--week-range)))
    (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                            (car range)))
    (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                            (cdr range)))
    (should (tada-list--date<= (car range) (cdr range)))))


;;;; Filtering

(defun tada-list-tests--make (date tags)
  "Build a test entry with DATE and TAGS."
  (make-tada-list-entry :title "test" :date date :tags tags
                        :description "" :marker nil))

(ert-deftest tada-list-tests/filter-by-tag ()
  (let ((tada-list--filter-tag "work")
        (tada-list--filter-range nil)
        (entries (list (tada-list-tests--make "2026-04-26" '("work"))
                       (tada-list-tests--make "2026-04-26" '("home"))
                       (tada-list-tests--make "2026-04-26" '("work" "deep")))))
    (should (= 2 (length (tada-list--filter entries))))))

(ert-deftest tada-list-tests/filter-by-range ()
  (let ((tada-list--filter-tag nil)
        (tada-list--filter-range '("2026-04-01" . "2026-04-30"))
        (entries (list (tada-list-tests--make "2026-03-15" nil)
                       (tada-list-tests--make "2026-04-15" nil)
                       (tada-list-tests--make "2026-05-15" nil))))
    (should (equal '("2026-04-15")
                   (mapcar #'tada-list-entry-date
                           (tada-list--filter entries))))))

(ert-deftest tada-list-tests/filter-tag-and-range-together ()
  (let ((tada-list--filter-tag "work")
        (tada-list--filter-range '("2026-04-01" . "2026-04-30"))
        (entries (list (tada-list-tests--make "2026-04-10" '("work"))
                       (tada-list-tests--make "2026-04-10" '("home"))
                       (tada-list-tests--make "2026-05-10" '("work")))))
    (should (= 1 (length (tada-list--filter entries))))))

(ert-deftest tada-list-tests/filter-passthrough-when-empty ()
  (let ((tada-list--filter-tag nil)
        (tada-list--filter-range nil)
        (entries (list (tada-list-tests--make "2026-04-26" nil))))
    (should (equal entries (tada-list--filter entries)))))

(ert-deftest tada-list-tests/filter-drops-undated-from-range ()
  (let ((tada-list--filter-tag nil)
        (tada-list--filter-range '("2026-04-01" . "2026-04-30"))
        (entries (list (tada-list-tests--make nil nil)
                       (tada-list-tests--make "2026-04-15" nil))))
    (should (= 1 (length (tada-list--filter entries))))))


;;;; Round-trip: insert then parse

(ert-deftest tada-list-tests/roundtrip-full-entry ()
  (with-temp-buffer
    (org-mode)
    (tada-list--insert-entry "Built tests" '("work" "emacs")
                             "2026-04-28" "Description body.")
    (goto-char (point-min))
    (let ((entry (tada-list--parse-entry-at-point)))
      (should (equal "Built tests" (tada-list-entry-title entry)))
      (should (equal '("work" "emacs") (tada-list-entry-tags entry)))
      (should (equal "2026-04-28" (tada-list-entry-date entry)))
      (should (equal "Description body."
                     (tada-list-entry-description entry))))))

(ert-deftest tada-list-tests/roundtrip-no-tags-no-description ()
  (with-temp-buffer
    (org-mode)
    (tada-list--insert-entry "Just a title" nil "2026-04-28" nil)
    (goto-char (point-min))
    (let ((entry (tada-list--parse-entry-at-point)))
      (should (equal "Just a title" (tada-list-entry-title entry)))
      (should (null (tada-list-entry-tags entry)))
      (should (equal "2026-04-28" (tada-list-entry-date entry)))
      (should (equal "" (tada-list-entry-description entry))))))

(ert-deftest tada-list-tests/roundtrip-multiline-description ()
  (with-temp-buffer
    (org-mode)
    (tada-list--insert-entry "Multi" '("emacs")
                             "2026-04-28" "Line one.\nLine two.")
    (goto-char (point-min))
    (let ((entry (tada-list--parse-entry-at-point)))
      (should (equal "Line one.\nLine two."
                     (tada-list-entry-description entry))))))

(provide 'tada-list-tests)
;;; tada-list-tests.el ends here
