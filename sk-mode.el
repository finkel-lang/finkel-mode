;;; sk-mode.el --- Major mode to edit SK-lang source.

;; CopyRight (C) 2017 8c6794b6

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

;; This file contains `sk-mode', a major mode to edit SK-lang source
;; code with basic REPL interaction support.

;;; Code:

(require 'cl-lib)
(require 'cl-indent)
(require 'lisp-mode)
(require 'inf-lisp)


;;; Groups and variables

(defgroup sk nil
  "Editing SK code."
  :prefix "sk-"
  :group 'lisp)

(defcustom sk-mode-inferior-lisp-command "sk"
  "The command used by `inferior-lisp-program'."
  :type 'string
  :group 'sk)

(defcustom sk-mode-hook nil
  "Hook run when entering SK mode."
  :type 'hook
  :group 'sk)

(defcustom sk-repl-default-port 12305
  "Default port of SK repl server to connect."
  :group 'sk
  :type 'integer)

(defcustom sk-repl-default-rts-option
  "-A16m -c -N -I0"
  "Default RTS option of SK REPL."
  :group 'sk
  :type 'string)

(defvar sk-repl-con nil
  "Connection to sk REPL.")

(defvar sk-repl-con-port nil
  "Port number string of REPL server.")

(defvar sk-repl-port-history nil
  "History of port used for connection.")

(defvar sk-repl-yaml-history nil
  "History of YAML file used for stack.")


;;; Font lock keywords

(eval-and-compile
  (defconst sk-mode-symbol-regexp
    "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"))

(defconst sk-mode-font-lock-keywords-1
  (eval-when-compile
    `(
      ;; Keywords.
      (,(concat
         "(" (regexp-opt
              '( ;; SK special forms.
                "begin" "eval-when-compile" "define-macro" "let-macro"
                "quote" "require"

                ;; SK core macros.
                "cond" "defmacro" "defmacro-m" "eval-when" "macrolet"
                "macrolet*" "match"

                ;; Haskell keywords, without `else', `in', and `then',
                ;; since those are implicitly expressed with
                ;; S-expressions and won't appear in SK codes.
                "case" "class" "data" "default" "deriving" "do"
                "foreign" "if" "import" "infix" "infixl" "infixr"
                "instance" "let" "module" "newtype" "type" "where")
              t)
         "\\>")
       . 1)

      ;; ``import' keyword. ... seems inefficient.
      ;; ("(\\(import\\W+\\(qualified\\W+\\)?\\)\\(\\(\\w+\\|\\.\\)+\\(\\(\\W+as\\W+\\)?\\)\\(\\(\\w+\\)+\\)?\\(\\(\\W+hiding\\)?\\)\\).*)"
      ;;  (1 font-lock-keyword-face)
      ;;  (4 font-lock-type-face)
      ;;  (5 font-lock-keyword-face)
      ;;  (9 font-lock-keyword-face))

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

      ;; Pragmas.
      (;; "\\(##\\(([^)]+)\\|\\w+\\)\\)"
       "\\(##(?\\(\\w+\\)\\)"
       (2 font-lock-preprocessor-face))

      ;; Type or data constructor.
      ;; (;; "\\((\\|\\[\\|{\\|\\s-\\)\\([A-Z][a-zA-Z_0-9.]*\\|:\\)"
      ;;  "\\(\\s-\\|\\-\\|(\\|\\[\\)\\(\\([A-Z][A-Za-z_0-9]*\\.?\\)+\\)"
      ;;  (2 font-lock-type-face))
      ("\\_<\\([A-Z][A-Za-z0-9_]*\\.?\\)+"
       (0 font-lock-type-face))

      ;; Function binding and function type signature.
      ( ;; "(\\(defn\\|defmacro\\|defmacro*\\)\\ +(?\\(\\sw+\\)"
       ,(concat
         ;; "(\\(def\\(n\\|n:\\|macro\\*?\\|ine-macro\\)\\)\\s-+(?\\("
         "(\\(" (regexp-opt
                 '("defdo" "define-macro" "defmacro" "defmacro!"
                   "defmacro*" "defmacro*!" "defmodule"
                   "defn" "defn!")
                 t)
         "\\)\\s-+(?\\("
         sk-mode-symbol-regexp
         "\\)")
       (1 font-lock-keyword-face)
       (3 font-lock-function-name-face))

      ;; Top level type signature.
      (,(concat "^(\\(=\\|::\\)\\ +(?\\(" sk-mode-symbol-regexp "\\)")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face prepend))

      ;; Word surrounded with `', treated as constant.
      ("`\\([^ ]+\\)'"
       (1 font-lock-constant-face prepend))

      ("\\(#|.*|#\\)"
       (1 font-lock-comment-face))

      ;; Errors.
      ("\\_<\\(error\\|undefined\\)\\_>"
       (1 font-lock-warning-face))))
  "Expressions to highlight in SK mode.")

(defvar sk-mode-font-lock-keywords sk-mode-font-lock-keywords-1
  "Default expressions to highlight in SK mode.")

(defvar sk-mode-syntax-table
  ;; The character `|' in copied syntax table behaves differently than
  ;; typical Lisp, which is used for block comments and guards in SK.
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?| "_ 23bn" table)
    (modify-syntax-entry ?\\ "/" table)
    table))

(defvar sk-imenu-generic-expression
  (list
   (list
    nil
    (purecopy
     (concat "^\\s-*("
             (eval-when-compile
               (regexp-opt
                '("::" "defn" "defn!" "defdo" "defmacro" "defmacro!"
                  "defmacro*" "defmacro*!" "define-macro")
                t))
             "\\s-+(?\\(" sk-mode-symbol-regexp "\\)"))
    2)
   (list
    (purecopy "Types")
    (purecopy
     (concat "^\\s-*("
             (eval-when-compile
               (regexp-opt
                '("data" "newtype" "type")
                t))
             "\\s-+(?\\([A-Z]+" sk-mode-symbol-regexp "\\)"))
    2)
   (list
    (purecopy "Classes")
    (purecopy (concat "^\\s-*(class\\s-+(\\([A-Z]+"
                      sk-mode-symbol-regexp
                      "\\)"))
    1)))

(defun sk-indent-function (indent-point state)
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
     ;; For indenting lambda.
     ((eq (char-after (+ last-open 1)) ?\\)
      (+ (funcall last-open-column) 2))
     ;; Delegating to `common-lisp-indent-function'.
     (t (common-lisp-indent-function indent-point state)))))

;;; XXX: Overriding settings for Common Lisp. Indentation for `do' and
;;; `case' conflicts with Common Lisp. Move the indentation rule to
;;; `sk-indent-function'.
(defun sk--put-indentation-properties ()
  "Set properties for indentation."
  ;;; May worth moving the association to customizable variable.
  (let ((l '((= . (0 &body))
             (| . 0)
             (:: . 1)
             (<- . 1)
             (begin . 0)
             (case . 1)
             (class . 1)
             (data . 1)
             (defn . (0 &body))
             (defn! . defn)
             (defdo . defn)
             (defmacro* . defmacro)
             (defmodule . 1)
             (do . 0)
             (foreign . 3)
             (instance . 1)
             (let-macro . macrolet)
             (letv . let)
             (macrolet .
               ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
             (macrolet* . macrolet)
             (match . 1)
             (module . 1)
             (newtype . 1)
             (type . 1)
             (where . 1))))
    (dolist (e l)
      (put (car e) 'common-lisp-indent-function
           (let ((v (cdr e)))
             (if (symbolp v)
                 (get v 'common-lisp-indent-function)
               v))))))

(defun sk-font-lock-syntactic-face-function (state)
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

(defun sk--mode-variables ()
  "Initialize sk-mode variables."
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)
  (setq-local comment-use-syntax t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local imenu-generic-expression sk-imenu-generic-expression)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local inferior-lisp-program sk-mode-inferior-lisp-command)
  (setq-local inferior-lisp-load-command "(load \"%s\")\n")
  (setq-local lisp-describe-sym-command "(info '%s)\n")
  (setq-local lisp-indent-function 'sk-indent-function)
  (setq font-lock-defaults
        '(sk-mode-font-lock-keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-extra-managed-props help-echo)
          (font-lock-syntactic-face-function
           . sk-font-lock-syntactic-face-function)))
  (sk--put-indentation-properties))

(defun sk--find-stack-yaml (dir)
  "Find stack YAML file.
Recursively search for stack YAML file in DIR, with going one
directory above at each time until root directory."
  (let ((yaml-file (concat (file-name-as-directory dir)
                           "stack.yaml")))
    (cond
     ((file-exists-p yaml-file) yaml-file)
     ((equal dir (expand-file-name "/")) nil)
     (t (sk--find-stack-yaml
         (expand-file-name
          (concat (file-name-as-directory dir) "..")))))))

(defun sk--find-default-stack-yaml ()
  "Find default stack yaml file, or return nil when not found."
  (sk--find-stack-yaml
   (file-name-directory (or (buffer-file-name) "/"))))

(defun sk--prompt-for-stack-exec-sk ()
  "Prompt and construct command string to run sk with stack."
  (let* ((default-yaml-file (sk--find-default-stack-yaml))
         (yaml-file (read-string
                     "Yaml file: "
                     (or default-yaml-file nil)
                     'sk-repl-yaml-history
                     nil))
         (yaml-option
          (if yaml-file
              (concat "--stack-yaml=" yaml-file)
            ""))
         (port-number
          (read-string
           "Port: "
           (number-to-string sk-repl-default-port)
           'sk-repl-port-history
           (number-to-string sk-repl-default-port))))
    (setq sk-repl-con-port port-number)
    (concat "stack exec " yaml-option
            " sk -- --listen=" port-number
            " +RTS " sk-repl-default-rts-option)))

(defun sk--connection-filter (process msg)
  "Filter to read from PROCESS and display the MSG."
  (message "=> %s" msg))

(defun sk--connection-sentinel (process msg)
  "Sentinel function for PROCESS with MSG."
  (cond
   ((string= "open\n" msg)
    (message "ski: connected to server."))
   ((string-prefix-p "failed" msg)
    (message "ski %s: failed: %s" process msg))
   (t
    (message "ski: %s" (replace-regexp-in-string "\n" " " msg)))))

(defun sk--make-connection (port)
  "Make and set network connection to REPL server with PORT."
  (setq sk-repl-con-port port)
  (setq sk-repl-con
        (make-network-process
         :name "sk"
         :buffer nil ;; "*ski*"
         :host 'local
         :service port
         :nowait nil
         :filter 'sk--connection-filter
         :filter-multibyte t
         :sentinel 'sk--connection-sentinel)))

(defun sk--send-string (str)
  "Send STR to REPL server."
  (process-send-string sk-repl-con str))

(defun sk--defun-at-point ()
  "Get list of start point and end point of current defun."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))


;;; Interactive functions

(defun sk-repl-connect ()
  "Show prompt for connecting to server."
  (interactive)
  (if (and (not (equal nil sk-repl-con))
           (process-live-p sk-repl-con))
      (message "Connection exists.")
    (let ((port (read-string
                 (concat "Port (default="
                         (number-to-string sk-repl-default-port)
                         "): ")
                 nil 'sk-repl-port-history
                 (number-to-string sk-repl-default-port)
                 nil)))
      (sk--make-connection port))))

(defun sk-repl-disconnect ()
  "Disconnect current SK REPL server connection."
  (interactive)
  (delete-process sk-repl-con))

(defun inferior-sk (cmd)
  "Run CMD to start sk REPL.
Set `inferior-lisp-buffer' with comint on successful start, and pop
to the newly created inferior sk buffer."
  (interactive
   (list (if current-prefix-arg
             (read-string "Run sk: " inferior-lisp-program)
           inferior-lisp-program)))
  (if (not (comint-check-proc "*inferior-lisp*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply #'make-comint
                           "inferior-lisp"
                           (car cmdlist)
                           nil
                           (cdr cmdlist)))
        (inferior-lisp-mode)))
  (setq inferior-lisp-buffer "*inferior-lisp*")
  (pop-to-buffer "*inferior-lisp*"))

(defun run-sk ()
  "Run sk REPL."
  (interactive)
  (let* ((use-stack (y-or-n-p "Use stack? "))
         (cmd (if use-stack
                  (sk--prompt-for-stack-exec-sk)
                inferior-lisp-program)))
    (inferior-sk cmd)
    ;; Wait for 1 second, until sk REPL process been launched. Better to
    ;; detect process startup with comint.
    (sleep-for 1)
    (sk--make-connection sk-repl-con-port)))

(defun switch-to-sk ()
  "Switch to sk REPL buffer, or start one if not exist."
  (interactive)
  (if (get-buffer-process inferior-lisp-buffer)
      (let ((pop-up-frames
             (or pop-up-frames
                 (get-buffer-window inferior-lisp-buffer t))))
        (pop-to-buffer inferior-lisp-buffer))
    (run-sk)))

(defun sk-send-input ()
  "Prompt for input and send to REPL connection."
  (interactive)
  (sk--send-string
   (read-string "Eval: " nil 'ski-input-history "" nil)))

(defun sk-send-form-at-point ()
  "Send outermost form at point to sk connection."
  (interactive)
  (let* ((points (sk--defun-at-point))
         (start (car points))
         (end (cadr points)))
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face 'secondary-selection)
      (run-with-timer 0.2 nil 'delete-overlay overlay))
    (sk--send-string
     (buffer-substring-no-properties start end))))

(defun sk-load-current-buffer ()
  "Load current buffer to REPL."
  (interactive)
  (lisp-load-file (buffer-file-name)))


;;; Mode definition

(defvar sk-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-flet ((bind (lambda (km fnsym)
                      (define-key map (kbd km) fnsym))))
      (bind "C-M-x" 'lisp-eval-defun)
      (bind "C-c C-c" 'sk-send-form-at-point)
      (bind "C-c C-d" 'lisp-describe-sym)
      (bind "C-c C-e" 'sk-send-input)
      (bind "C-c C-k" 'sk-load-current-buffer)
      (bind "C-c C-l" 'lisp-load-file)
      (bind "C-c C-z" 'switch-to-sk)
      (bind "C-c M-j" 'sk-repl-connect)
      (bind "C-x C-e" 'lisp-eval-last-sexp)
      map)))

;;;###autoload
(define-derived-mode sk-mode prog-mode "SK"
  "Major mode for ediging SK code.
\\{sk-mode-map}"
  (sk--mode-variables))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.sk\\'" . sk-mode))
  (add-to-list 'interpreter-mode-alist '("sk" . sk-mode)))

(provide 'sk-mode)

;;; sk-mode.el ends here
