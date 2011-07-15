;; font-lock-*-face: keyword comment constant function-name preprocessor string variable-name warning

;; FIXME this doesn't do what I hoped it would wrt to paren-matching in comments
(defvar rust-mode-syntax-table (funcall (c-lang-const c-make-mode-syntax-table rust))
  "Syntax table used in rust-mode buffers.")
(defvar rust-mode-map (make-keymap))

(defun rust-mode-2 ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table rust-mode-syntax-table)
  (use-local-map rust-mode-map)
  (setq major-mode 'rust-mode mode-name "Rust")
  (run-hooks 'rust-mode-hook)
  (cm-mode (make-cm-mode :token 'rust-token
                         :start-state 'make-rust-state
                         :copy-state 'copy-rust-state
                         :compare-state 'rust-compare-state
                         :indent 'rust-indent)))

(defstruct rust-state
  (tokenize 'rust-token-base)
  (context (list (make-rust-context :type 'top :indent (- rust-indent-unit) :align nil))))

(defstruct rust-context
  type
  indent
  column
  (align 'unset))

(defun rust-push-context (st type)
  (push (make-rust-context :type type :indent (current-indentation) :column (current-column))
        (rust-state-context st)))
(defun rust-pop-context (st)
  (pop (rust-state-context st)))

(defun rust-compare-state (a b)
  (and (eq (rust-state-tokenize a) (rust-state-tokenize b))
       (equal (rust-state-context a) (rust-state-context b))))

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

(defun rust-token-base (st)
  (case (char-after)
    (?\" (forward-char 1)
         (rust-push-context st 'string)
         (setf (rust-state-tokenize st) 'rust-token-string)
         (rust-token-string st))
    (?\' (forward-char 1)
         (setf rust-tcat 'atom)
         (let ((escaped (eq (char-after) ?\\))
               (start (point)))
           (while (case (char-after) ((?\' ?\n nil) nil) (t t)) (forward-char 1))
           (if (and (cm-eat-char ?\') (or escaped (= (point) (+ start 2))))
               'font-lock-string-face 'font-lock-warning-face)))
    (?/ (cond ((cm-eat-string "//")
               (end-of-line) 'font-lock-comment-face)
              ((cm-eat-string "/*")
               (rust-push-context st 'comment)
               (setf (rust-state-tokenize st) 'rust-token-comment)
               (rust-token-comment st))
              (t (while (cm-eat-set rust-operator-chars)) (setf rust-tcat 'op) nil)))
    (?# (forward-char 1)
        (cm-eat-re "[a-z_]+") (setf rust-tcat 'macro) 'font-lock-preprocessor-face)
    (t (cond ((cm-eat-re "[a-z_]+")
              (setf rust-tcat 'ident)
              (let ((word (match-string 0)))
                (if (cm-eat-string "::")
                    (if (not (eq (char-after) ?<))
                        'font-lock-constant-face
                      (backward-char 2) word)
                  word)))
             ((cm-eat-re "0x[0-9a-fA-F]+\\|\\([0-9]+\\(\\.[0-9]+\\)?\\|\\.[0-9]+\\)\\(e[+\\-]?[0-9]+\\)?")
              (setf rust-tcat 'atom)
              (when (cm-eat-re "[iuf]") (cm-eat-re "[0-9]+"))
              'font-lock-constant-face)
             ((cm-eat-set rust-punc-chars) (setf rust-tcat (char-before)) nil)
             ((cm-eat-set rust-operator-chars)
              (setf rust-tcat 'op)
              (while (cm-eat-set rust-operator-chars)) nil)
             (t (forward-char 1) nil)))))

(defun rust-token-string (st)
  (setf rust-tcat 'atom)
  (let (escaped)
    (loop
     (let ((ch (char-after)))
       (case ch
         ((?\n nil) (return))
         (?\" (unless escaped
                (setf (rust-state-tokenize st) 'rust-token-base)
                (rust-pop-context st)
                (forward-char 1)
                (return))))
       (forward-char 1)
       (setf escaped (and (not escaped) (eq ch ?\\))))))
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
      (when (eq (rust-context-align cx) 'unset)
        (setf (rust-context-align cx) nil)))
    (unless (cm-eat-whitespace)
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
            ((?\; ?:) (when (eq cur-cx 'statement) (rust-pop-context st)))
            (?\{ (rust-push-context st ?\}))
            (?\[ (rust-push-context st ?\]))
            (?\( (rust-push-context st ?\)))
            (?\} (dolist (close '(statement ?\} statement))
                   (when (eq close cur-cx)
                     (setf cur-cx (rust-context-type (rust-pop-context st))))))
            (t (cond ((eq cur-cx rust-tcat) (rust-pop-context st))
                     ((or (eq cur-cx ?\}) (eq cur-cx 'top))
                      (rust-push-context st 'statement))))))
        tok))))

(defvar rust-indent-unit 4)

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
