;;; finkel-mode.el --- Major mode for Finkel -*-lexical-binding:t-*-

;; CopyRight (C) 2017-2020 8c6794b6

;; Author: 8c6794b6
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))
;; Created: Dec 2017
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

;; This file contains `finkel-mode', a major mode to edit Finkel programming
;; language source codes with basic REPL interaction support.

;;; Code:

(require 'cl-lib)
(require 'cl-indent)
(require 'imenu)
(require 'inf-lisp)


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
  "-A16m -c -N -I0"
  "Default RTS option of Finkel REPL."
  :group 'finkel
  :type 'string)

(defcustom finkel-indentation-properties
  `((| . 0)
    (:: . 1)
    (<- . 1)
    (:begin . 0)
    (case . (2 2 &body))
    (class . 1)
    (data . (1 &body))
    (defn . (2 2 &body))
    (,(intern "defn'") . defn)
    (defdo . defn)
    (defmacro . (0 &body))
    (defmacro-m . defmacro)
    (,(intern "defmacro'") . defmacro)
    (,(intern "defmacro-m'") . defmacro)
    (defmodule . 1)
    (do . 0)
    (eval-and-compile . 0)
    (eval-when . 1)
    (:eval-when-compile . 0)
    (forall . (0 &body))
    (foreign . 3)
    (instance . 1)
    (lefn . ((&whole 4 &rest (&whole 1 2 &lambda &body)) &body))
    (macrolet . ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
    (macrolet-m . macrolet)
    (module . 1)
    (newtype . (1 &body))
    (type . (1 &body))
    (where . (1 &body)))
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
      ;; Keywords.
      (,(concat
         "(" (regexp-opt
              '(;; Finkel special forms.
                ":begin" ":eval-when-compile"
                ":quote" ":quasiquote" ":require"
                ":unquote" ":unquote-splice"
                ":with-macro"

                ;; Finkel core macros.
                "cond" "defmacro" "defmacro'" "defmacro-m"
                "eval-when" "eval-and-compile"
                "macrolet" "macrolet-m"

                ;; Haskell keywords, without `else', `in', and `then',
                ;; since those are implicitly expressed with
                ;; S-expressions and won't appear in Finkel codes.
                "case" "class" "data" "default" "deriving" "do"
                "foreign" "if" "import" "infix" "infixl" "infixr"
                "instance" "let" "module" "newtype" "type" "where"

                ;; GHC specific
                "data family" "data instance" "forall" "newtype instance"
                "type family" "type instance")
              t)
         "\\>")
       . 1)

      ;; defmodule and its internal

      ("^(\\(defmodule\\)\\s-+"
       (1 font-lock-keyword-face)
       (,(regexp-opt '("export" "require-and-import" "require") nil)
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

      ;; Pragmas.
      ("#p("
       (0 font-lock-preprocessor-face)
       ("[^ \n]+"
        (save-excursion (up-list) (point))
        (re-search-backward "#p(")
        (0 font-lock-preprocessor-face)))

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

      ;; Operator functions.
      ("\\_<[~!#$%&*+-./<=>?@^|\\\\]+\\_>"
       (0 font-lock-variable-name-face))

      ;; Type or data constructor.
      ("\\_<!?\\([A-Z][A-Za-z0-9_-]*\\.?\\)+"
       (0 font-lock-type-face))

      ;; Function binding and function type signature.
      (,(concat
         "(\\(" (regexp-opt
                 '("define-macro" "define-macro'"
                   "defmacro" "defmacro'" "defmacro-m" "defmacro-m'"
                   "defn" "defn'" "defdo")
                 t)
         "\\)\\s-+(?\\("
         finkel-mode-symbol-regexp
         "\\)")
       (1 font-lock-keyword-face)
       (3 font-lock-function-name-face))

      ;; Function definition (defn style)
      (,(concat "(\\(defn\\|defdo\\)\\s-+(::\\s-+\\(\\sw+\\)")
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
    (modify-syntax-entry ?!  "'   " table)
    (modify-syntax-entry ?\\ "/   " table)
    (modify-syntax-entry ?#  ". 14nb" table)
    (modify-syntax-entry ?\; "< 23" table)
    table))


;;; Indent

(defun finkel-get-indent-method (symbol)
  "Get indent method for SYMBOL from property list."
  (get symbol 'finkel-indent-function))

(defun finkel-put-indent-method (symbol prop)
  "Put indentation property PROP to SYMBOL."
  (put symbol 'finkel-indent-function prop))

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
     ;; Using same column number for '{' and '['.
     ((member (char-after last-open) '(?\{ ?\[))
      (1+ (funcall last-open-column)))
     ;; For indenting lambda and raw function declaration.
     ((member (char-after (+ last-open 1)) '(?\\ ?\=))
      (+ (funcall last-open-column) 2))
     ;; Delegating to `common-lisp-indent-function'.
     (t
      (cl-letf (((symbol-function 'lisp-indent-find-method)
                 (lambda (symbol &optional no-compat)
                   (ignore no-compat)
                   (finkel-get-indent-method symbol))))
        (common-lisp-indent-function indent-point state))))))

(defun finkel--put-indentation-properties ()
  "Set properties for indentation."
  (dolist (e finkel-indentation-properties)
    (cl-destructuring-bind (name . meth0) e
      (let ((meth1 (if (symbolp meth0)
                       (finkel-get-indent-method meth0)
                     meth0)))
        (finkel-put-indent-method name meth1)))))


;;; Imenu

;;; XXX: Rewrite to work with `imenu-create-index-function' to create
;;; index for top-level `eval-when's and `:begin'.
(defvar finkel-imenu-generic-expression
  (list
   (list
    (purecopy "Functions")
    (purecopy
     (concat "^("
             (eval-when-compile
               (regexp-opt '("defn" "defn'" "defdo")
                           t))
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
                '("defmacro" "defmacro'" "defmacro-m" "defmacro-m'")
                t))
             "\\s-+(?\\(" finkel-mode-symbol-regexp "\\)"))
    2)
   (list
    (purecopy "Types")
    (purecopy
     (concat "^("
             (eval-when-compile
               (regexp-opt
                '("data" "newtype" "type")
                t))
             "\\s-+(?\\([A-Z]+" finkel-mode-symbol-regexp "\\)"))
    2)
   (list
    (purecopy "Classes")
    (purecopy (concat "^(class\\s-+(\\([A-Z]+"
                      finkel-mode-symbol-regexp
                      "\\)"))
    1))

  "Expression for `imenu-generic-expression'.")


;;; Inferior Finkel

(defun finkel--search-file-upward (file dir)
  "Find configuration file.
Recursively search for file FILE in DIR, with going one directory
above at each time until root directory."
  (let ((config-file (concat (file-name-as-directory dir)
                             file)))
    (cond
     ((file-exists-p config-file) config-file)
     ((equal dir (expand-file-name "/")) nil)
     (t (finkel--search-file-upward
         file
         (expand-file-name
          (concat (file-name-as-directory dir) "..")))))))

(defun finkel--find-config-file (file)
  "Find configuration file named FILE."
  (finkel--search-file-upward file
                              (file-name-directory
                               (or (buffer-file-name) "/"))))

(defun finkel--read-port-number ()
  "Prompt for port and read a number."
  (read-string
   "Port: "
   (number-to-string finkel-repl-default-port)
   'finkel-repl-port-history
   (number-to-string finkel-repl-default-port)))

(defun finkel--prompt-for-stack-exec ()
  "Prompt and construct command string to run finkel with stack."
  (let* ((yaml-file
          (read-file-name "Yaml file: " nil nil t
                          (finkel--find-config-file "stack.yaml")))
         (yaml-option
          (if yaml-file
              (concat "--stack-yaml=" yaml-file)
            ""))
         (port-number (finkel--read-port-number)))
    (setq finkel-repl-con-port port-number)
    (concat "stack exec " yaml-option " -- "
            finkel-mode-inferior-lisp-program " repl --listen=" port-number
            " +RTS " finkel-repl-default-rts-option)))

(defun finkel--prompt-for-cabal-v2-exec ()
  "Prompt and construct command string to run finkel with cabal v2-exec."
  (let* ((project-file
          (read-file-name "cabal.project file: " nil nil t
                          (finkel--find-config-file "cabal.project")))
         (project-option
          (if project-file
              (concat "--project-file=" project-file)
            ""))
         (port-number (finkel--read-port-number)))
    (setq finkel-repl-con-port port-number)
    (concat "cabal " project-option " v2-exec -- "
            finkel-mode-inferior-lisp-program " repl --listen=" port-number
            " +RTS " finkel-repl-default-rts-option)))

(defun finkel--connection-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (ignore process)
  (message "=> %s" msg))

(defun finkel--connection-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond
   ((string= "open\n" msg)
    (message "finkel: connected to server."))
   ((string-prefix-p "failed" msg)
    (message "finkel %s: failed: %s" process msg))
   (t
    (message "finkel: %s" (replace-regexp-in-string "\n" " " msg)))))

(defun finkel--make-connection (port)
  "Make and set network connection to REPL server with PORT."
  (setq finkel-repl-con-port port)
  (setq finkel-repl-con
        (make-network-process
         :name "finkel"
         :buffer nil
         :host 'local
         :service port
         :nowait nil
         :filter 'finkel--connection-filter
         :filter-multibyte t
         :sentinel 'finkel--connection-sentinel)))

(defun finkel--send-string (str)
  "Send STR to REPL server."
  (process-send-string finkel-repl-con str))

(defun finkel--defun-at-point ()
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
      (finkel--make-connection port))))

(defun finkel-repl-disconnect ()
  "Disconnect current Finkel REPL server connection."
  (interactive)
  (delete-process finkel-repl-con))

(defun inferior-finkel (cmd)
  "Run CMD to start finkel REPL.
Set `inferior-lisp-buffer' with comint on successful start, and pop
to the newly created inferior finkel buffer."
  (interactive
   (list (if current-prefix-arg
             (read-string "Run finkel: " inferior-lisp-program)
           inferior-lisp-program)))
  (if (not (comint-check-proc "*finkel*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply #'make-comint
                           "finkel"
                           (car cmdlist)
                           nil
                           (cdr cmdlist)))
        (inferior-finkel-mode)))
  (setq inferior-lisp-buffer "*finkel*")
  (pop-to-buffer "*finkel*"))

(defun run-finkel ()
  "Run finkel REPL."
  (interactive)
  (let* ((cmd (cond
               ((y-or-n-p "Use stack exec? ")
                (finkel--prompt-for-stack-exec))
               ((y-or-n-p "Use cabal v2-exec? ")
                (finkel--prompt-for-cabal-v2-exec))
               (t
                (concat inferior-lisp-program " repl")))))
    (inferior-finkel cmd)
    ;; Wait for 1 second, until finkel REPL process been launched. Better to
    ;; detect process startup with comint.
    (sleep-for 1)
    (finkel--make-connection finkel-repl-con-port)))

(defun switch-to-finkel ()
  "Switch to finkel REPL buffer, or start one if not exist."
  (interactive)
  (if (get-buffer-process inferior-lisp-buffer)
      (let ((pop-up-frames
             (or pop-up-frames
                 (get-buffer-window inferior-lisp-buffer t))))
        (pop-to-buffer inferior-lisp-buffer))
    (run-finkel)))

(defun finkel-send-input ()
  "Prompt for input and send to REPL connection."
  (interactive)
  (finkel--send-string
   (read-string "Eval: " nil 'finkel-input-history "" nil)))

(defun finkel-send-form-at-point ()
  "Send outermost form at point to finkel connection."
  (interactive)
  (cl-destructuring-bind (start . end) (finkel--defun-at-point)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face 'secondary-selection)
      (run-with-timer 0.2 nil 'delete-overlay overlay))
    (finkel--send-string (buffer-substring-no-properties start end))))

(defun finkel-load-current-buffer ()
  "Load current buffer to REPL."
  (interactive)
  (lisp-load-file (buffer-file-name)))


;;; Mode definition

(defun finkel--mode-variables ()
  "Initialize `finkel-mode' variables."
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
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
                nil nil
                (("+-*/.<>=!?$%_&~^:@" . "w"))
                nil
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
      (bind "C-c C-z" 'switch-to-finkel)
      (bind "C-c M-j" 'finkel-repl-connect)
      (bind "C-x C-e" 'lisp-eval-last-sexp)
      map)))


;;; Properties for indentation and documentation

(finkel--put-indentation-properties)

(put :doc 'finkel-doc-string-elt 1)
(put :doc^ 'finkel-doc-string-elt 1)
(put :doc$ 'finkel-doc-string-elt 2)
(put :dh1 'finkel-doc-string-elt 1)
(put :dh2 'finkel-doc-string-elt 1)
(put :dh3 'finkel-doc-string-elt 1)
(put :dh4 'finkel-doc-string-elt 1)

(put 'define-macro 'finkel-doc-string-elt 2)
(put (intern "define-macro'") 'finkel-doc-string-elt 2)
(put 'defmacro 'finkel-doc-string-elt 2)
(put 'defmacro-m 'finkel-doc-string-elt 2)
(put (intern "defmacro-m'") 'finkel-doc-string-elt 2)
(put 'defn 'finkel-doc-string-elt 2)

;;;###autoload
(define-derived-mode finkel-mode prog-mode "Finkel"
  "Major mode for ediging Finkel code.

\\{finkel-mode-map}"
  (finkel--mode-variables))

;;;###autoload
(define-derived-mode inferior-finkel-mode inferior-lisp-mode "Inferior Finkel"
  "Major mode for Finkel inferior process.

\\{inferior-finkel-mode-map}"
  (setq-local indent-tabs-mode nil))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.fnk\\'" . finkel-mode))
  (add-to-list 'interpreter-mode-alist '("fnk" . finkel-mode)))

(provide 'finkel-mode)

;;; Local Variables:
;;; coding: utf-8
;;; End:

;;; finkel-mode.el ends here
