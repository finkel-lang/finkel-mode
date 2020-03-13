;;; finkel-mode-test.el --- Test for finkel-mode -*-lexical-binding:t-*-

;; CopyRight (C) 2020 8c6794b6

;; Author: 8c6794b6
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))
;; Created: Mar 2020
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Buttercup tests for `finkel-mode'

;;; Code:

(require 'buttercup)
(require 'dash)
(require 's)

(when (require 'undercover nil t)
  (undercover "*.el"
              (:report-file "codecov.json")
              (:report-format 'simplecov)
              (:send-report nil)))

(require 'finkel-mode)

(defun finkel-test-trim-indent (text)
  "Remove indentation from TEXT."
  (->> text s-lines (-map #'s-trim-left) (s-join "\n")))

(defun finkel-test-buffer-string ()
  "Return buffer as text with beginning and ending empty space trimmed."
  (s-trim (buffer-substring-no-properties (point-min) (point-max))))

(buttercup-define-matcher :indented (get-text)
  (let* ((text (s-trim (funcall get-text)))
         (text-no-indent (finkel-test-trim-indent text)))
    (insert text-no-indent)
    (indent-region-line-by-line (point-min) (point-max))
    (let ((text-with-indent (finkel-test-buffer-string)))
      (delete-region (point-min) (point-max))
      (if (string= text text-with-indent)
          t
        `(nil
          .
          ,(format "\nGiven text:\n%s\nwas instead indented to:\n%s\n"
                   text text-with-indent))))))

(describe "Syntax"
  (before-all (set-syntax-table finkel-mode-syntax-table))
  (after-each (delete-region (point-min) (point-max)))

  (it "sets comments"
    (insert ";; foo")
    (expect (nth 4 (syntax-ppss))))

  (it "sets strings"
    (insert "\"foo\"")
    (backward-char)
    (expect (nth 3 (syntax-ppss)))))

(describe "Indenting"
  (before-all
    (set-syntax-table finkel-mode-syntax-table)
    (finkel--put-indentation-properties)
    (finkel--mode-variables))

  (describe "standard cases"
    (it "doesn't carry - opening line has one sexp"
      (expect "
(foo
 bar)
" :indented))
    (it "carries indentation - opening line has two sexps"
      (expect "
(foo bar
     buzz)
" :indented)))

  (describe "case"
    (it "doesn't carry indentation"
      (expect "
(case expr
  (Just n) (+ n 1)
  Nothing 0)
" :indented)))

  (describe "do"
    (it "does carry aligned indentation"
      (expect "
(do (print 1)
    (print 2)
    (print 3)
    (print 4))
" :indented)))

  (describe "[]"
    (it "does carry aligned indentation"
      (expect "
(print [1 2 3
        4 5 6
        7 8 9])
" :indented)))

  (describe "{}"
    (it "does carry aligned indentation"
      (expect "
(Foo {field-one 1
      field-two \"two\"
      field-three #'3})
" :indented)))

  (describe "="
    (it "does not carry indentation of argument"
      (expect "
(= foo a b c
  (do (print a)
      (print b)
      (print c)))
:" :indented)))

  (describe "\\"
    (it "does not carry indentation of argument"
      (expect "
(\\ x y z
  (>> (print x) (print y) (print z)))
" :indented)))

  (describe "defn"
    (it "has a value in plist"
      (expect (finkel-get-indent-method 'defn) :not :to-be nil))

    (it "does carry aligned indentation"
      (expect "
(defn foo
  [n]
  (print n))
" :indented))

    (it "does carry aligned indentation with doccomment"
      (expect "
(defn foo
  \"Documentation comment for `foo'\"
  [n]
  (print n))
" :indented))

    (it "does not carry argument indentation, argument in first line"
      (expect "
(defn foo [x]
  (print n)))
" :indented))))

(provide 'finkel-mode-test)

;;; Local Variables:
;;; eval: (buttercup-minor-mode)
;;; End:

;;; finkel-mode-test.el ends here
