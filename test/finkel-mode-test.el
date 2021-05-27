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
              (:report-file "simplecov.json")
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

(buttercup-define-matcher :imenu (get-text get-section get-name)
  (let ((text (funcall get-text))
        (section (funcall get-section))
        (name (funcall get-name)))
    (setq imenu--index-alist nil)
    (insert text)
    (imenu--make-index-alist)
    (delete-region (point-min) (point-max))
    (let* ((entries (cdr (assoc section imenu--index-alist)))
           (entry (assoc name entries)))
      (if entry
          t
        `(nil
          .
          ,(format "Entry named `%s' in section `%s' was not found
imenu--index-alist: %s"
                   name section imenu--index-alist))))))

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
    (finkel-put-indentation-properties)
    (finkel-mode-variables))

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
:" :indented))

    (it "does carry inner form"
      (expect "
(let ((= foo (foldl bar buzz quux
                    more args1 args2))
      (= bar a b c d
        (blah a (c d) b)))
  (foo bar))
" :indented)))

  (describe "\\"
    (it "does not carry indentation of argument"
      (expect "
(\\ x y z
  (>> (print x) (print y) (print z)))
" :indented)))

  (describe "case"
    (it "doesn't carry indentation"
      (expect "
(case expr
  (Just n) (+ n 1)
  Nothing 0)
" :indented)))

  (describe "case-do"
    (it "doesn't carry indentation"
      (expect "
(case-do expr
  (Just n) (print n)
  Nothing (putStrLn \"nothing\"))
" :indented)))

  (describe "catch"
    (it "does not carry aligned indentation"
      (expect "
(catch (>> (putStr prompt) (fmap Just getLine))
  (\\ e (if (isEOFError e)
           (return Nothing)
           (throwIO e))))
" :indented)))

  (describe "cond"
    (it "does carry aligned indentation"
      (expect "
(cond
  ((< n 50)
   \"was less than 50\")
  ((< 100 n)
   \"was greater than 100\")
  (otherwise
   \"was inbetween\"))
" :indented)))

  (describe "class"
    (it "doesn't carry indentation"
      (expect "
(class (Foo a)
  (:: foo1 (-> a Int))
  (:: foo2 (-> a Int Int))
  (:: foo3 (-> a Int Int Int)))
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

  (describe "do"
    (it "does carry aligned indentation"
      (expect "
(do (print 1)
    (print 2)
    (print 3)
    (print 4))
" :indented))

    (describe "<-"
      (it "does not carry indentation"
        (expect "
(do (<- variable-with-long-name
      (function-with-long-name arg1 arg2 arg3 arg4 arg5))
    (use-the variable-with-long-name))
" :indented))))

  (describe "eval-and-compile"
    (it "does not carry indentation"
      (expect "
(eval-and-compile
  (defn foo [a b]
    (+ a b))
  (defn bar [x]
    (print x)))
" :indented))

    (it "does not carry indentation, contains pragma"
      (expect "
(eval-and-compile
  (defn foo []
    (+ a b))
  %p(INLINABLE foo)
  (defn bar [x]
    (print x)))
" :indented)))

  (describe "eval-when"
    (it "does not carry indentation"
      (expect "
(eval-when [compile]
  (defn foo [a b]
    (+ a b))
  (defn bar [x]
    (print x)))
" :indented)))

  (describe "forall"
    (it "does not carry indentation"
      (expect "
(forall k1 k2 (:: a k1) (:: b k2)
  (-> (t a) (t b) (Maybe (:~~: a b))))
" :indented)))

  (describe "foreign"
    (it "does not carry indentation"
      (expect "
(foreign import ccall unsafe \"math.h tan\"
  (:: ctan (-> Double Double)))
" :indented)))

  (describe "instance"
    (it "does not carry indentation"
      (expect "
(instance (=> (C1 a) (C1 (Foo a)))
  (= c1a (Foo a)
    expr)
  (= c1b arg1 (Foo a) body))
" :indented)))

  (describe "let"
    (it "does carry local definitions, does not carry body"
      (expect "
(let ((:: foo (-> Int Int Int))
      (= foo a b
        (+ a b))
      (= bar 30)
      (= buzz 12))
  (foo bar buzz))
" :indented)))

  (describe "lefn"
    (it "does not carry local definitions"
      (expect "
(lefn [(a 1)
       (f [x y]
         (+ x (* y 2)))]
  (f a a)")))

  (describe "lept"
    (it "does not cally local definitions"
      (expect "
(lept [a 1
       b 2]
  (+ a b))")))

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

  (describe "macroletM"
    (it "should indent as done in `macrolet'"
      (expect "
(macroletM ((macro-one [a b]
              (return `(do (print ,a)
                           (print ,b))))
            (macro-two args
              (return `[,@args]))
            (macro-three []
              (return `(putStrLn \"m3\"))))
  (do (macro-one True False)
      (print (macro-two 1 2 3))
      (macro-three)))
" :indented)))

  (describe "return"
    (it "does indent as ordinal function"
      (expect "
(return
 `(:begin
    (:eval-when-compile
      ,@body)
    ,@body))
" :indented)))

  (describe "type"
    (it "does not carry indentation"
      (expect "
(type (MyMaybe a)
  (Either () a))
" :indented)))

  (describe "type family"
    (it "does not carry indentation"
      (expect "
(type family (TF3 a)
  (= (TF3 Int) Double)
  (= (TF3 Bool) Char)
  (= (TF3 a) String))
" :indented)))

  (describe "unless"
    (it "does not carry indentation"
      (expect "
(unless test
  (do-something-with arg))
" :indented)))

  (describe "where"
    (it "does not carry indentation"
      (expect "
(where foo
  (defn foo
    (quux bar buzz))
  (defn bar 10)
  (defn buzz 20)
  (defn quux [a b]
    (+ a b)))
" :indented))))

(describe "Imenu"
  (before-all (finkel-mode-variables))

  (describe "Functions"
    (it "should contain a function entry, without type signature"
      (expect "
(defn f1 [a b]
  (do (print a)
      (print b)))
" :imenu "Functions" "f1"))

    (it "should contain a function entry, with type signature"
      (expect "
(defn (:: f2 (-> Int Int (IO ())))
  [a b]
  (do (print a)
      (print b)))
" :imenu "Functions" "f2")))

  (describe "Macros"
    (it "should show a macro entry"
      (expect "
(defmacro m1 [a b]
  `(>> (print ,a) (print ,b)))
" :imenu "Macros" "m1")))

  (describe "Datatypes"
    (it "should show data type name with `data'"
      (expect "
(data (Foo a)
  (Foo1 a)
  (Foo2 a a)
  (deriving (Eq Show)))
" :imenu "Datatypes" "Foo"))

    (it "should show data type name with `newtype'"
      (expect "
(newtype (Bar a) (Bar {unBar a}))
" :imenu "Datatypes" "Bar"))

    (it "shoould show data type name with `type'"
      (expect "
(type (MyMaybe a)
  (Either () a))
" :imenu "Datatypes" "MyMaybe")))

  (describe "Classes"
    (it "should show type-class with `class', no constraint"
      (expect "
(class (C1 a)
  (:: c1 (-> a Int)))
" :imenu "Classes" "(C1 a)"))

    (it "should show type-class with `class', with constraint"
      (expect "
(class (=> (Eq a) (Show a) (C2 a))
  (:: c2 (-> a Int))
" :imenu "Classes" "(=> (Eq a) (Show a) (C2 a))")))

  (describe "Instances"
    (it "should show instances with `instance', no constraint"
      (expect "
(instance (Show Foo)
  (= show _ \"Foo\"))
" :imenu "Instances" "(Show Foo)"))

    (it "should show instances with `instance', with constraint"
      (expect "
(instance (=> (Show a) (Show (Foo a)))
  (= show (Foo a) (show a)))
" :imenu "Instances" "(=> (Show a) (Show (Foo a)))"))))

(provide 'finkel-mode-test)

;;; Local Variables:
;;; coding: utf-8
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; eval: (buttercup-minor-mode)
;;; End:

;;; finkel-mode-test.el ends here
