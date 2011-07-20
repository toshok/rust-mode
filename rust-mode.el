;(require 'cm-mode)

;; FIXME this doesn't do what I hoped it would wrt to paren-matching in comments
;(defvar rust-mode-syntax-table (funcall (c-lang-const c-make-mode-syntax-table rust))
;  "Syntax table used in rust-mode buffers.")
(defvar rust-mode-map (make-keymap))
(defvar rust-indent-unit 4)

(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.rc$" . rust-mode))

(defun rust-mode ()
  (interactive)
  (kill-all-local-variables)
;  (set-syntax-table rust-mode-syntax-table)
  (use-local-map rust-mode-map)
  (setq major-mode 'rust-mode mode-name "Rust")
  (run-hooks 'rust-mode-hook)
  (cm-mode (make-cm-mode :token 'rust-token
                         :start-state 'make-rust-state
                         :copy-state 'copy-sequence
                         :compare-state 'equal
                         :indent 'rust-indent)))

(defun make-rust-state ()
  (vector 'rust-token-base
          (list (vector 'top (- rust-indent-unit) nil nil))
          0))
(defmacro rust-state-tokenize (x) `(aref ,x 0))
(defmacro rust-state-context (x) `(aref ,x 1))
(defmacro rust-state-indent (x) `(aref ,x 2))

(defmacro rust-context-type (x) `(aref ,x 0))
(defmacro rust-context-indent (x) `(aref ,x 1))
(defmacro rust-context-column (x) `(aref ,x 2))
(defmacro rust-context-align (x) `(aref ,x 3))

(defun rust-push-context (st type &optional align-column)
  (push (vector type (rust-state-indent st) align-column (if align-column 'unset nil))
        (rust-state-context st)))
(defun rust-pop-context (st)
  (setf (rust-state-indent st) (rust-context-indent (pop (rust-state-context st)))))

(defvar rust-operator-chars "+-/%=<>!*&|@~")
(defvar rust-punc-chars "()[].{}:;")
(defvar rust-value-keywords
  (let ((table (make-hash-table :test 'equal)))
    (dolist (word '("mod" "if" "else" "while" "do" "alt" "for" "break" "cont" "put" "ret" "be" "fail"
                    "type" "resource" "check" "assert" "claim" "prove" "state" "gc" "native" "auto"
                    "fn" "pred" "iter" "import" "export" "let" "const" "log" "log_err" "tag" "obj"))
      (puthash word t table))
    table))
;; FIXME type-context keywords

(defvar rust-tcat nil "Kludge for multiple returns without consing")

(defvar rust-char-table
  (let ((table (make-char-table 'rust)))
    (macrolet ((def (range &rest body)
                    `(let ((--b (lambda (st) ,@body)))
                       ,@(mapcar (lambda (elt) `(set-char-table-range table ',elt --b))
                                 (if (consp range) range (list range))))))
      (def t (forward-char 1) nil)
      (def (32 ?\t) (skip-chars-forward " \t") nil)
      (def ?\" (forward-char 1)
           (rust-push-context st 'string)
           (setf (rust-state-tokenize st) 'rust-token-string)
           (rust-token-string st))
      (def ?\' (forward-char 1)
           (setf rust-tcat 'atom)
           (let ((is-escape (eq (char-after) ?\\))
                 (start (point)))
             (if (not (rust-eat-until-unescaped ?\'))
                 'font-lock-warning-face
               (forward-char 1)
               (if (or is-escape (= (point) (+ start 2)))
                   'font-lock-string-face 'font-lock-warning-face))))
      (def ?/
           (cond ((cm-eat-string "//")
                  (end-of-line) 'font-lock-comment-face)
                 ((cm-eat-string "/*")
                  (rust-push-context st 'comment)
                  (setf (rust-state-tokenize st) 'rust-token-comment)
                  (rust-token-comment st))
                 (t (while (cm-eat-set rust-operator-chars)) (setf rust-tcat 'op) nil)))
      (def ?# (forward-char 1)
           (cm-eat-re "[a-z_]+") (setf rust-tcat 'macro)
           'font-lock-preprocessor-face)
      (def ((?a . ?z) (?A . ?Z) ?_)
           (cm-eat-re "[a-zA-Z_][a-zA-Z0-9_]*")
           (setf rust-tcat 'ident)
           (let ((word (match-string 0)))
             (if (cm-eat-string "::")
                 (if (not (eq (char-after) ?<))
                     'font-lock-constant-face
                   (backward-char 2) word)
               word)))
      (def ((?0 . ?9)) ; FIXME .5
           (cm-eat-re "0x[0-9a-fA-F]+\\|\\([0-9]+\\(\\.[0-9]+\\)?\\|\\.[0-9]+\\)\\(e[+\\-]?[0-9]+\\)?")
           (setf rust-tcat 'atom)
           (when (cm-eat-re "[iuf]") (cm-eat-re "[0-9]+"))
           'font-lock-constant-face)
      (def (?\( ?\) ?\[ ?\] ?. ?\{ ?\} ?: ?\;)
           (forward-char 1)
           (setf rust-tcat (char-before)) nil)
      (def (?+ ?- ?% ?= ?< ?> ?! ?* ?& ?| ?@ ?~)
           (forward-char 1)
           (setf rust-tcat 'op)
           (while (cm-eat-set rust-operator-chars)) nil)
      table)))

(defun rust-token-base (st)
  (funcall (char-table-range rust-char-table (char-after)) st))

(defun rust-eat-until-unescaped (ch)
  (let (escaped)
    (loop
     (let ((cur (char-after)))
       (when (or (eq cur ?\n) (not cur)) (return nil))
       (when (and (eq cur ch) (not escaped)) (return t))
       (forward-char 1)
       (setf escaped (and (not escaped) (eq cur ?\\)))))))

(defun rust-token-string (st)
  (setf rust-tcat 'atom)
  (when (rust-eat-until-unescaped ?\")
    (setf (rust-state-tokenize st) 'rust-token-base)
    (rust-pop-context st)
    (forward-char 1))
  'font-lock-string-face)

(defun rust-token-comment (st)
  (let ((eol (point-at-eol)))
    (loop
     (unless (re-search-forward "\\(/\\*\\)\\|\\(\\*/\\)" eol t)
       (goto-char eol)
       (return))
     (if (match-beginning 1)
         (rust-push-context st 'comment)
       (rust-pop-context st)
       (unless (eq (rust-context-type (car (rust-state-context st))) 'comment)
         (setf (rust-state-tokenize st) 'rust-token-base)
         (return))))
    'font-lock-comment-face))

(defun rust-token (st)
  (let ((cx (car (rust-state-context st))))
    (when (bolp)
      (setf (rust-state-indent st) (current-indentation))
      (when (eq (rust-context-align cx) 'unset)
        (setf (rust-context-align cx) nil)))
    (setf rust-tcat nil)
    (let ((tok (funcall (rust-state-tokenize st) st))
          (cur-cx (rust-context-type cx)))
      (when (stringp tok)
        (setf tok (and (gethash tok rust-value-keywords nil)
                       'font-lock-keyword-face)))
      (when rust-tcat
        (when (eq (rust-context-align cx) 'unset)
          (setf (rust-context-align cx) t))
        (case rust-tcat
          ((?\; ?: ?\{)
           (when (eq cur-cx 'statement) (rust-pop-context st))
           (when (eq rust-tcat ?\{)
             (rust-push-context st ?\} (- (current-column) 1))))
          (?\[ (rust-push-context st ?\] (- (current-column) 1)))
          (?\( (rust-push-context st ?\) (- (current-column) 1)))
          (?\} (dolist (close '(statement ?\} statement))
                 (when (eq close cur-cx)
                   (rust-pop-context st)
                   (setf cur-cx (rust-context-type (car (rust-state-context st)))))))
          (t (cond ((eq cur-cx rust-tcat) (rust-pop-context st))
                   ((or (eq cur-cx ?\}) (eq cur-cx 'top))
                    (rust-push-context st 'statement))))))
      tok)))

;; FIXME alt half-indent
(defun rust-indent (st)
  (let* ((cx (car (rust-state-context st)))
         (closing (eq (rust-context-type cx) (char-after))))
    (cond ((eq (rust-state-tokenize st) 'rust-token-string) 0)
          ((eq (rust-context-type cx) 'statement)
           (+ (rust-context-indent cx) (if (eq (char-after) ?\}) 0 rust-indent-unit)))
          ((eq (rust-context-align cx) t) (+ (rust-context-column cx) (if closing 0 1)))
          (t (+ (rust-context-indent cx) (if closing 0 rust-indent-unit))))))

(provide 'rust-mode)

