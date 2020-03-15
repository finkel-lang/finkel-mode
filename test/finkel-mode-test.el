;;; finkel-mode-test.el --- Test for finkel-mode -*-lexical-binding:t-*-

;; Copyright 2017 The finkel-mode Authors.  All rights reserved.
;;
;; Use of this source code is governed by a BSD-style license that can be found
;; in the LICENSE file.

;; Author: 8c6794b6
;; Created: Mar 2020
;; Keywords: test
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

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
" :indented)))

  (describe "defn'"
    (it "does carry aligned indentation"
      (expect "
(defn' foo
  [n]
  (print n))
" :indented))

    (it "does carry aligned indentation with doccomment"
      (expect "
(defn' foo
  \"Documentation comment for `foo'\"
  [n]
  (print n))
" :indented))

    (it "does not carry argument indentation, argument in first line"
      (expect "
(defn' foo [x]
  (print n)))
" :indented)))

  (describe "macrolet"
    (it "does carry aligned indentation"
      (expect "
(macrolet ((macro-one [a b]
             `(do (print ,a)
                  (print ,b)))
           (macro-two args
             `[,@args])
           (macro-three []
             `(putStrLn \"m3\")))
  (do (macro-one True False)
      (print (macro-two 1 2 3))
      (macro-three)))
" :indented)))

  (describe "macrolet-m"
    (it "should indent as done in `macrolet'"
      (expect "
(macrolet-m ((macro-one [a b]
               (return `(do (print ,a)
                            (print ,b))))
             (macro-two args
               (return `[,@args]))
             (macro-three []
               (return `(putStrLn \"m3\"))))
  (do (macro-one True False)
      (print (macro-two 1 2 3))
      (macro-three)))
" :indented))))

(provide 'finkel-mode-test)

;;; Local Variables:
;;; coding: utf-8
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; eval: (buttercup-minor-mode)
;;; End:

;;; finkel-mode-test.el ends here
