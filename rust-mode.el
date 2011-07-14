;; FIXME keymap, syntax-table

;; font-lock-*-face: keyword comment constant function-name preprocessor string variable-name warning

(defun rust-mode-2 ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rust-mode
    mode-name "Rust")
  (run-hooks 'rust-mode-hook)
  (cm-mode (make-cm-mode :token 'rust-token
                         :start-state 'make-rust-state
                         :copy-state 'copy-rust-state
                         :compare-state 'rust-compare-state)))

(defstruct rust-state
  (tokenize 'rust-token-base)
  (context (list (make-rust-context :type 'top :indent 0))))

(defstruct rust-context
  type
  indent)

(defun rust-push-context (st type)
  (push (make-rust-context :type type :indent (current-indentation))
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
           (while (case (char-after) ((?\' ?\n) nil) (t t)) (forward-char 1))
           (if (and (cm-eat-char ?\') (or escaped (= (point) (+ start 2))))
               'font-lock-string-face 'font-lock-warning-face)))
    (?/ (cond ((cm-eat-string "//")
               (end-of-line) 'font-lock-comment-face)
              ((cm-eat-string "/*")
               (rust-push-context st 'comment)
               (setf (rust-state-tokenize st) 'rust-token-comment)
               (rust-token-comment st))
              (t (while (cm-eat-set rust-operator-chars)) (setf rust-tcat 'op) nil)))
    (?# (cm-eat-re "[a-z_]+") (setf rust-tcat 'macro) 'font-lock-preprocessor-face)
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
       (forward-char 1)
       (case ch
         (?\n (return))
         (?\" (unless escaped
                (setf (rust-state-tokenize st) 'rust-token-base)
                (rust-pop-context st)
                (return))))
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
  (unless (cm-eat-whitespace)
    (setf rust-tcat nil)
    (let ((tok (funcall (rust-state-tokenize st) st)))
      (when (stringp tok)
        (setf tok (and (gethash tok rust-value-keywords nil)
                       'font-lock-keyword-face)))
      tok)))

(provide 'rust-mode)
