;;; finkel-mode.el --- Major mode for Finkel -*-lexical-binding:t-*-

;; Copyright 2017 The finkel-mode Authors.  All rights reserved.
;;
;; Use of this source code is governed by a BSD-style license that can be found
;; in the LICENSE file.

;; Author: 8c6794b6
;; Created: Fri Dec 15 2017
;; Keywords: lisp, languages
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))
;; Homepage: https://github.com/finkel-lang/finkel-mode

;;; Commentary:

;; This file contains `finkel-mode', a major mode to edit Finkel programming
;; language source codes with basic REPL interaction support.

;;; Code:

(require 'cl-lib)
(require 'cl-indent)
(require 'imenu)
(require 'inf-lisp)
(require 'prog-mode)


;;; Groups and variables

(defgroup finkel nil
  "Major mode for editing Finkel code."
  :prefix "finkel-"
  :group 'languages
  :link '(emacs-commentary-link :tag "Commentary" "finkel-mode"))

(defcustom finkel-mode-inferior-lisp-program "finkel"
  "The command used by `inferior-lisp-program'."
  :type 'string
  :group 'finkel)

(defcustom finkel-mode-hook nil
  "Hook run when entering Finkel mode."
  :type 'hook
  :group 'finkel)

(defcustom finkel-repl-default-port 50321
  "Default port of Finkel repl server to connect."
  :group 'finkel
  :type 'integer)

(defcustom finkel-repl-default-rts-option
  ""
  "Default RTS option of Finkel REPL."
  :group 'finkel
  :type 'string)

;;; XXX: Seems like the CAR elements of the indentation properties need to be in
;;; lower case. Using "defmacrom" instead of "defmacroM".

(defcustom finkel-indentation-properties
  `(;; Finkel special forms
    (:begin 0)
    (:eval-when-compile 0)
    (:with-macro (4 &body))

    ;; Haskell keywords
    (= finkel-indent-multiargs)
    (| 0)
    (:: 1)
    (<- (2 2))
    (case (2 &body))
    (case-do . case)
    (class (2 &body))
    (data . =)
    (do 0)
    (foreign . =)
    (instance 1)
    (if (4 2 2))
    (let (4 &body))
    (lefn ((&whole 4 &rest (&whole 1 &rest &body)) &body))
    (lept (4 &body))
    (module 1)
    (newtype . =)
    (type . =)
    (where (4 &body))

    ;; Haskell Language extension keywords
    (forall . =)

    ;; Finkel Core keywords
    (cond 0)
    (cond-expand . cond)
    (defn . =)
    (defn\' . defn)
    (defn- . defn)
    ;; Avoiding quoted `defmacro' to make `package-lint' happy ...
    (,(intern "defmacro") . defn)
    (defmacro\' . defn)
    (defmacro- . defn)
    (defmacrom . defn)
    (defmacrom\' . defn)
    (defmacrom- . defn)
    (defmodule 1)
    (eval-and-compile (2 &body))
    (eval-when (4 &body))
    (import-when . eval-when)
    (macrolet ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
    (macroletm . macrolet)

    ;; Non-keywords
    (catch 1)
    (unless 1)
    (when 1))
  "Default indentation properties for Finkel source codes."
  :group 'finkel
  :type '(alist :key-type symbol :value-type sexp))

(defvar finkel-repl-con nil
  "Connection to finkel REPL.")

(defvar finkel-repl-con-port nil
  "Port number string of REPL server.")

(defvar finkel-repl-port-history nil
  "History of port used for connection.")

(defvar finkel-repl-yaml-history nil
  "History of YAML file used for stack.")


;;; Font lock

(eval-and-compile
  (defconst finkel-mode-symbol-regexp
    ;; Using single quote is fine if it's not at the head position.
    (let ((re "\\sw\\|\\s_\\|\\\\."))
      (concat "\\(?:" re "\\)\\(?:" re "\\|'\\)*"))))

(defconst finkel-mode-font-lock-keywords-1
  (eval-when-compile
    `(
      ;; Discard prefix.
      ("%_"
       (0 font-lock-preprocessor-face)
       ("[^ \n]+"
        (save-excursion (forward-sexp) (point))
        (re-search-backward "%_")
        (0 font-lock-comment-face t)))

      ;; Pragmas
      ("%p("
       (0 font-lock-preprocessor-face)
       ("[^ \n]+"
        (save-excursion (up-list) (point))
        (re-search-backward "%p(")
        (0 font-lock-preprocessor-face)))

      ;; defmodule and its internal
      ("^(\\(defmodule\\)\\s-+"
       (1 font-lock-keyword-face)
       (,(concat
          "\\<"
          (regexp-opt
           '("export" "import-when" "import" "require-and-import" "require")
           nil)
          "\\>")
        (save-excursion (up-list) (point))
        (re-search-backward "defmodule")
        (0 font-lock-keyword-face)))

      ("\\s-*(\\(import\\)"
       ("\\<\\(qualified\\|as\\|hiding\\)\\>"
        (save-excursion (up-list) (point))
        (re-search-backward "import")
        (0 font-lock-keyword-face)))

      ;; FFI import and export
      ("^(\\(foreign\\)\\s-+"
       (1 font-lock-keyword-face)
       (,(regexp-opt '("import" "export" "ccall" "safe" "unsafe") t)
        (save-excursion (up-list) (point))
        (re-search-backward "foreign")
        (0 font-lock-keyword-face)))

      ;; Documentation
      ("\\_<\\(:doc[$\\^]?\\|:dh[1234]\\)\\_>"
       (0 font-lock-variable-name-face))

      ;; Wildcard
      ("\\_<\\(_\\)\\_>"
       (1 font-lock-keyword-face))

      ;; Reserved operators.
      (,(concat "\\_<"
                (regexp-opt
                 '(".." ":" "::" "=" "\\\\" "|" "<-" "->" "~" "@" "=>")
                 t)
                "\\_>")
       (1 font-lock-variable-name-face))

      ;; Characters
      ("#'\\([^\n ]+\\)?"
       (1 font-lock-string-face prepend))

      ;; Operator functions.
      ("\\_<[~!#$%&*+-./<=>?@^|\\\\]+\\_>"
       (0 font-lock-variable-name-face))

      ;; Type or data constructor.
      ("\\_<\\([A-Z][A-za-z0-9_-]*\\.?\\)+"
       (0 font-lock-type-face))

      ;; Lambda, bang pattern, and type arg could be concatenated to varid
      ("\\(\\\\\\|!\\|@\\)"
       (1 font-lock-variable-name-face))

      ;; Function binding and function type signature.
      (,(concat
         "(\\(" (regexp-opt
                 '("defmacro" "defmacro'" "defmacro-"
                   "defmacroM" "defmacroM'" "defmacroM-"
                   "defn" "defn'" "defn-")
                 t)
         "\\)\\s-+(?\\("
         finkel-mode-symbol-regexp
         "\\)")
       (1 font-lock-keyword-face)
       (3 font-lock-function-name-face))

      ;; Function definition (defn style)
      (,(concat "(\\(defn-?\\)\\s-+(::\\s-+\\("
                finkel-mode-symbol-regexp
                "\\)")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face))

      ;; defXXX style macro
      (,(concat "(\\(def[^ \r\n\t]*\\)"
                "\\>"
                "[ \r\n\t]*"
                (concat "\\(" finkel-mode-symbol-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      ;; Top level type signature.
      (,(concat "^(\\(=\\|::\\)\\ +(?\\(" finkel-mode-symbol-regexp "\\)")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face prepend))

      ;; Word surrounded with `', treated as constant.
      ("`\\([^ ]+\\)'"
       (1 font-lock-constant-face prepend))

      ;; Keywords.
      (,(concat
         ;;"("
         ;; "[( ]"
         "\\_<"
         (regexp-opt
          '(;; Finkel special forms.
            ":begin" ":eval-when-compile"
            ":quote" ":quasiquote" ":require"
            ":unquote" ":unquote-splice"
            ":with-macro"

            ;; Finkel core macros.
            "case-do" "cond" "cond-expand"
            "defmacro" "defmacro'" "defmacro-"
            "defmacroM" "defmacroM'" "defmacroM-"
            "defn" "defn'" "defn-"
            "defmodule" "eval-and-compile"
            "eval-when" "lcase" "lefn" "lept"
            "macroexpand" "macroexpand-1" "macrolet" "macroletM"

            ;; Keyword used in Finkel core macros.
            "import-when"

            ;; Haskell keywords, without `else', `in', and `then',
            ;; since those are implicitly expressed with
            ;; S-expressions and won't appear in Finkel codes.
            "case" "class" "data" "default" "deriving" "do"
            "foreign" "if" "import" "infix" "infixl" "infixr"
            "instance" "let" "module" "newtype" "type" "where"

            ;; GHC specific
            "anyclass" "forall" "stock" "via"
            "data family" "data instance" "newtype instance"
            "type family" "type instance")
          t)
         "\\_>")
       . 1)

      ;; Common lisp style keyword
      ("\\(:[a-z0-9:_-]+\\)"
       (1 font-lock-builtin-face))

      ;; Errors.
      ("\\_<\\(error\\|undefined\\)\\_>"
       (1 font-lock-warning-face))))
  "Expressions to highlight in Finkel mode.")

(defvar finkel-mode-font-lock-keywords finkel-mode-font-lock-keywords-1
  "Default expressions to highlight in Finkel mode.")

(defun finkel-font-lock-syntactic-face-function (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
Lisp font lock syntactic face function."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (let ((listbeg (nth 1 state)))
          (if (or (lisp-string-in-doc-position-p listbeg startpos)
                  (lisp-string-after-doc-keyword-p listbeg startpos))
              font-lock-doc-face
            font-lock-string-face)))
    font-lock-comment-face))


;;; Syntax table

(defvar finkel-mode-syntax-table
  ;; The character `|' in copied syntax table behaves differently than
  ;; typical Lisp, which is used for block comments in Lisp but guards
  ;; in Finkel.
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?|  "_   " table)
    (modify-syntax-entry ?~  "'   " table)
    (modify-syntax-entry ?\\ "\\   " table)
    (modify-syntax-entry ?!  "'   " table) ; for strict field and bang pattern
    (modify-syntax-entry ?@  "'   " table) ; for type application
    (modify-syntax-entry ?#  "_ 14nb" table)
    (modify-syntax-entry ?\; "< 23" table)
    table))


;;; Indent

(defun finkel-get-indent-method (symbol)
  "Get indent method for SYMBOL from property list."
  (get symbol 'finkel-indent-function))

(defun finkel-put-indent-method (symbol prop)
  "Put indentation property PROP to SYMBOL."
  (put symbol 'finkel-indent-function prop))

(defun finkel-indent-multiargs
    (path state indent-point sexp-column normal-indent)
  "Function to indent form taking multiple arguments.
If PATH shows nested element, returns SEXP-COLUMN + 2, otherwise
NORMAL-INDENT.  STATE and INDENT-POINT are ignored,"
  (ignore state indent-point)
  ;; Use normal indent when the multi-arg form has inner lists.
  (if (cadr path)
      normal-indent
    (+ sexp-column 2)))

(defun finkel-indent-function (indent-point state)
  "Simple wrapper function over `common-lisp-indent-function'.
Could be done with advice.  See the function
`lisp-indent-function' for description of INDENT-POINT and
STATE."
  (let* ((last-open (elt state 1))
         (last-open-column
          (lambda ()
            (save-excursion (goto-char last-open)
                            (current-column)))))
    (cond
     ;; Using same column number for '['.
     ((eq (char-after last-open) ?\[)
      (1+ (funcall last-open-column)))
     ;; For lambda, indenting escape character '\\'.
     ((eq (char-after (+ last-open 1)) ?\\)
      (let ((last-open-column (save-excursion
                                (goto-char (elt state 1))
                                (current-column))))
        (+ last-open-column 2)))
     ;; Delegating to `common-lisp-indent-function'.
     (t
      (cl-letf (((symbol-function 'lisp-indent-find-method)
                 (lambda (symbol &optional no-compat)
                   (ignore no-compat)
                   (finkel-get-indent-method symbol))))
        (common-lisp-indent-function indent-point state))))))

(defun finkel-put-indentation-properties ()
  "Set properties for indentation."
  (dolist (e finkel-indentation-properties)
    (cl-destructuring-bind (name . meth0) e
      (let ((meth1 (if (symbolp meth0)
                       (finkel-get-indent-method meth0)
                     (car meth0))))
        (finkel-put-indent-method name meth1)))))


;;; Documentation string element

(defun finkel-put-doc-string-elt (sym pos)
  "Put doc-string-elt for SYM at POS."
  (put sym 'finkel-doc-string-elt pos))

(defun finkel-put-doc-string-properties ()
  "Set properties for doc string elt."
  (let ((es `((:doc . 1)
              (:doc^ . 1)
              (:doc$ . 2)
              (:dh1 . 1)
              (:dh2 . 1)
              (:dh3 . 1)
              (:dh4 . 1)
              (,(intern "defmacro") . 2)
              (defmacro\' . 2)
              (defmacroM . 2)
              (defmacroM\' . 2)
              (defn . 2)
              (defn\' . 2))))
    (dolist (e es)
      (cl-destructuring-bind (sym . pos) e
        (finkel-put-doc-string-elt sym pos)))))


;;; Imenu

(defun finkel-imenu-search-sexp (re)
  "Make a function for imenu with regexp RE."
  (when (re-search-backward re nil t)
    (save-excursion
      (down-list)
      (forward-sexp)
      (down-list)
      (up-list -1)
      (let* ((start (point))
             (end (progn (forward-list) (point))))
        (set-match-data (list start end))
        (goto-char start)))))

(defun finkel-imenu-search-class ()
  "Function to search class definition."
  (finkel-imenu-search-sexp "^\\s-*(class"))

(defun finkel-imenu-search-instance ()
  "Function to search instance definition."
  (finkel-imenu-search-sexp "^\\s-*(instance"))

;;; XXX: Won't work with top-level functions inside `eval-when's and `:begin'.
(defvar finkel-imenu-generic-expression
  (list
   (list
    (purecopy "Functions")
    (purecopy
     (concat "^("
             (eval-when-compile
               (regexp-opt '("defn" "defn'") t))
             "\\s-+\\(?:(\\s-*::\\s-+\\)?\\("
             finkel-mode-symbol-regexp
             "\\)\\s-+"))
    2)
   (list
    (purecopy "Macros")
    (purecopy
     (concat "^("
             (eval-when-compile
               (regexp-opt
                '("defmacro" "defmacro'" "defmacroM" "defmacroM'")
                t))
             "\\s-+(?\\(" finkel-mode-symbol-regexp "\\)"))
    2)
   (list
    (purecopy "Datatypes")
    (purecopy
     (concat "^("
             (eval-when-compile
               (regexp-opt
                '("data" "newtype" "type")
                t))
             "\\s-+(?\\([A-Z]+" finkel-mode-symbol-regexp "\\)"))
    2)
   (list (purecopy "Classes") #'finkel-imenu-search-class 0)
   (list (purecopy "Instances") #'finkel-imenu-search-instance 0))
  "Expression for `imenu-generic-expression'.")


;;; Inferior Finkel

(defun finkel-search-file-upward (file dir)
  "Find configuration file.
Recursively search for file FILE in DIR, with going one directory
above at each time until root directory."
  (let ((config-file (concat (file-name-as-directory dir)
                             file)))
    (cond
     ((file-exists-p config-file) config-file)
     ((equal dir (expand-file-name "/")) nil)
     (t (finkel-search-file-upward
         file
         (expand-file-name
          (concat (file-name-as-directory dir) "..")))))))

(defun finkel-find-config-file (file)
  "Find configuration file named FILE."
  (finkel-search-file-upward file
                             (file-name-directory
                              (or (buffer-file-name) "/"))))

(defun finkel-read-port-number ()
  "Prompt for port and read a number."
  (read-string
   "Port: "
   (number-to-string finkel-repl-default-port)
   'finkel-repl-port-history
   (number-to-string finkel-repl-default-port)))

(defun finkel-prompt-for-stack-exec ()
  "Prompt and construct command string to run finkel with stack."
  (let* ((yaml-file
          (read-file-name "Yaml file: " nil nil t
                          (finkel-find-config-file "stack.yaml")))
         (yaml-option
          (if yaml-file
              (concat "--stack-yaml=" (expand-file-name yaml-file))
            ""))
         (port-number (finkel-read-port-number)))
    (setq finkel-repl-con-port port-number)
    (concat "stack exec " yaml-option " -- "
            finkel-mode-inferior-lisp-program " repl --listen=" port-number
            " +RTS " finkel-repl-default-rts-option)))

(defun finkel-prompt-for-cabal-v2-exec ()
  "Prompt and construct command string to run finkel with cabal v2-exec."
  (let* ((project-file
          (read-file-name "cabal.project file: " nil nil t
                          (finkel-find-config-file "cabal.project")))
         (project-option
          (if project-file
              (concat "--project-file=" (expand-file-name project-file))
            ""))
         (port-number (finkel-read-port-number)))
    (setq finkel-repl-con-port port-number)
    (concat "cabal " project-option " v2-exec -- "
            finkel-mode-inferior-lisp-program " repl --listen=" port-number
            " +RTS " finkel-repl-default-rts-option)))

(defun finkel--more-than-n-lines (n str)
  "Return t if STR is more than N lines."
  (if (<= n 0)
      t
    (let ((index-found (string-match "\n" str)))
      (and index-found
           (let ((next-str (substring str (+ index-found 1))))
             (finkel--more-than-n-lines (- n 1) next-str))))))

(defun finkel-connection-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (ignore process)
  (if (finkel--more-than-n-lines 1 msg)
      (finkel--show-message-multilines msg)
    (finkel--show-message-oneline msg)))

(defun finkel--show-message-multilines (msg)
  "Show MSG with temporary buffer."
  (let ((bufname "*finkel-out*"))
    (with-output-to-temp-buffer bufname
      (princ msg))
    (switch-to-buffer-other-window bufname)))

(defun finkel--show-message-oneline (msg)
  "Show MSG in minibuffer."
  (message "=> %s" msg))

(defun finkel-connection-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond
   ((string= "open\n" msg)
    (message "finkel: connected to server."))
   ((string-prefix-p "failed" msg)
    (message "finkel %s: failed: %s" process msg))
   (t
    (message "finkel: %s" (replace-regexp-in-string "\n" " " msg)))))

(defun finkel-make-connection (port)
  "Make and set network connection to REPL server with PORT."
  (setq finkel-repl-con-port port)
  (setq finkel-repl-con
        (make-network-process
         :name "finkel"
         :buffer nil
         :host 'local
         :service port
         :nowait nil
         :filter 'finkel-connection-filter
         :filter-multibyte t
         :sentinel 'finkel-connection-sentinel)))

(defun finkel-send-string (str)
  "Send STR to REPL server."
  (process-send-string finkel-repl-con str))

(defun finkel-defun-at-point ()
  "Get list of start point and end point of current defun."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (cons (point) end)))))


;;; Interactive functions

(defun finkel-repl-connect ()
  "Show prompt for connecting to server."
  (interactive)
  (if (and (not (equal nil finkel-repl-con))
           (process-live-p finkel-repl-con))
      (message "Connection exists.")
    (let ((port (read-string
                 (concat "Port (default="
                         (number-to-string finkel-repl-default-port)
                         "): ")
                 nil 'finkel-repl-port-history
                 (number-to-string finkel-repl-default-port)
                 nil)))
      (finkel-make-connection port))))

(defun finkel-repl-disconnect ()
  "Disconnect current Finkel REPL server connection."
  (interactive)
  (delete-process finkel-repl-con))

(defun finkel-inferior (cmd)
  "Run CMD to start inferior finkel.
Set `inferior-lisp-buffer' with comint on successful start, and pop
to the newly created inferior finkel buffer."
  (interactive
   (list (if current-prefix-arg
             (read-string "Run finkel: " inferior-lisp-program)
           inferior-lisp-program)))
  (when (not (comint-check-proc "*finkel*"))
    (let ((cmdlist (split-string cmd)))
      (set-buffer (apply #'make-comint
                         "finkel"
                         (car cmdlist)
                         nil
                         (cdr cmdlist)))
      (finkel-inferior-mode)))
  (setq inferior-lisp-buffer "*finkel*")
  (pop-to-buffer "*finkel*"))

(defun finkel-run-repl ()
  "Run finkel REPL."
  (interactive)
  (let* ((cmd (cond
               ((y-or-n-p "Use stack exec? ")
                (finkel-prompt-for-stack-exec))
               ((y-or-n-p "Use cabal v2-exec? ")
                (finkel-prompt-for-cabal-v2-exec))
               (t
                (concat inferior-lisp-program " repl")))))
    (finkel-inferior cmd)
    ;; Wait for 1 second, until finkel REPL process been launched. Better to
    ;; detect process startup with comint.
    (sleep-for 1)
    (finkel-make-connection finkel-repl-con-port)))

(defun finkel-switch-to-repl ()
  "Switch to finkel REPL buffer, or start one if not exist."
  (interactive)
  (if (get-buffer-process inferior-lisp-buffer)
      (let ((pop-up-frames
             ;; Using `symbol-value' to make `elint-current-buffer' happy ...
             (or (symbol-value 'pop-up-frames)
                 (get-buffer-window inferior-lisp-buffer t))))
        (pop-to-buffer inferior-lisp-buffer))
    (finkel-run-repl)))

(defun finkel-send-input ()
  "Prompt for input and send to REPL connection."
  (interactive)
  (finkel-send-string
   (read-string "Eval: " nil 'finkel-input-history "" nil)))

(defun finkel-send-form-at-point ()
  "Send outermost form at current point to finkel connection."
  (interactive)
  (cl-destructuring-bind (start . end) (finkel-defun-at-point)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face 'secondary-selection)
      (run-with-timer 0.2 nil 'delete-overlay overlay))
    (finkel-send-string (buffer-substring-no-properties start end))))

(defun finkel-check-source (file-name)
  "Check if FILE-NAME needs to be saved."
  (comint-check-source file-name))

(defun finkel-reload ()
  "Send reload command to REPL."
  (interactive)
  (finkel-check-source (buffer-file-name))
  (finkel-send-string "(repl-macro reload)"))

(defun finkel-load-current-buffer ()
  "Load current buffer to REPL."
  (interactive)
  (lisp-load-file (buffer-file-name)))


;;; Mode definition

(defun finkel-mode-variables ()
  "Initialize `finkel-mode' variables."
  (setq-local comment-start ";")
  (setq-local comment-start-skip "\\(;+\\|#;+\\)\\s *")
  (setq-local comment-end-skip "")
  (setq-local comment-add 1)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)
  (setq-local imenu-generic-expression finkel-imenu-generic-expression)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local inferior-lisp-program finkel-mode-inferior-lisp-program)
  (setq-local inferior-lisp-load-command ",load \"%s\"\n")
  (setq-local lisp-describe-sym-command ",info %s\n")
  (setq-local lisp-indent-function #'finkel-indent-function)
  (setq-local lisp-doc-string-elt-property 'finkel-doc-string-elt)
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults
              '(finkel-mode-font-lock-keywords
                nil nil nil nil
                (font-lock-mark-block-function . mark-defun)
                (font-lock-extra-managed-props help-echo)
                (font-lock-syntactic-face-function
                 . finkel-font-lock-syntactic-face-function))))

(defvar finkel-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-flet ((bind (lambda (km fnsym)
                      (define-key map (kbd km) fnsym))))
      (bind "C-M-x" 'lisp-eval-defun)
      (bind "C-c C-c" 'finkel-send-form-at-point)
      (bind "C-c C-d" 'lisp-describe-sym)
      (bind "C-c C-e" 'finkel-send-input)
      (bind "C-c C-k" 'finkel-load-current-buffer)
      (bind "C-c C-l" 'lisp-load-file)
      (bind "C-c C-;" 'finkel-reload)
      (bind "C-c C-z" 'finkel-switch-to-repl)
      (bind "C-c M-j" 'finkel-repl-connect)
      (bind "C-x C-e" 'lisp-eval-last-sexp)
      map)))


;;; Properties for indentation and documentation

(finkel-put-indentation-properties)
(finkel-put-doc-string-properties)

;;;###autoload
(define-derived-mode finkel-mode prog-mode "Finkel"
  "Major mode for ediging Finkel code.

\\{finkel-mode-map}"
  (finkel-mode-variables))

;;;###autoload
(define-derived-mode finkel-inferior-mode inferior-lisp-mode "Inferior Finkel"
  "Major mode for Finkel inferior process.

\\{finkel-inferior-mode-map}"
  (setq-local indent-tabs-mode nil))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.fnk\\'" . finkel-mode))
  (add-to-list 'interpreter-mode-alist '("fnk" . finkel-mode)))

(provide 'finkel-mode)

;;; Local Variables:
;;; coding: utf-8
;;; indent-tabs-mode: nil
;;; fill-column: 80
;;; End:

;;; finkel-mode.el ends here
