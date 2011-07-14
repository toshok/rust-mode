; keyword comment constant function-name preprocessor string variable-name warning

(defun rust-mode-2 ()
  (interactive)
  (cm-mode (make-cm-mode :token 'rust-token
                         :start-state 'make-rust-state
                         :copy-state 'copy-rust-state
                         :compare-state 'rust-compare-state)))

(defstruct rust-state
  (tokenize 'rust-token-base)
  (token-arg 0))

; FIXME
(defun rust-compare-state (a b)
  (and (eq (rust-state-tokenize a) (rust-state-tokenize b))
       (eq (rust-state-token-arg a) (rust-state-token-arg b))))

(defvar rust-operator-chars "+-/%=<>!*&|@~")
(defvar rust-punc-chars "()[].{}:;")

(defvar rust-tcat nil "For multiple returns without consing")

(defun rust-token-base (st)
  (case (char-after)
    (?\" (forward-char 1)
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
               (setf (rust-state-tokenize st) 'rust-token-comment
                     (rust-state-token-arg st) 1)
               (rust-token-comment st))
              (t (while (cm-eat-set rust-operator-chars)) (setf rust-tcat 'op) nil)))
    (?# (cm-eat-re "[a-z_]+") (setf rust-tcat 'macro) 'font-lock-preprocessor-face)
    (t (cond ((cm-eat-re "[a-z_]+")
              (setf rust-tcat 'ident)
              (let ((word (match-string 0)))
                (if (cm-eat-string "::")
                    (if (not (eq (char-after) ?<))
                        'font-lock-constant-face
                      (progn (backward-char 2) word))
                  word)))
             ((cm-eat-set rust-punc-chars) (setf rust-tcat (char-before)) nil)
             ((cm-eat-re "[0-9]+")
              (setf rust-tcat 'atom)
              (when (cm-eat-re "[a-z]") (cm-eat-re "[0-9]+"))
              'font-lock-constant-face)
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
                (return))))
       (setf escaped (and (not escaped) (eq ch ?\\))))))
  'font-lock-string-face)

(defun rust-token-comment (st)
  (let ((eol (point-at-eol)) (n 0))
    (loop
     (incf n)
     (when (> n 10) (print "OUT") (return))
     (unless (re-search-forward "\\(/\\*\\)\\|\\(\\*/\\)" eol t)
       (goto-char eol)
       (return))
     (if (match-beginning 1)
         (incf (rust-state-token-arg st))
       (when (= (decf (rust-state-token-arg st)) 0)
         (setf (rust-state-tokenize st) 'rust-token-base)
         (return))))
    'font-lock-comment-face))

(defun rust-token (st)
  (unless (cm-eat-whitespace)
    (setf rust-tcat nil)
    (let ((tok (funcall (rust-state-tokenize st) st)))
      (if (stringp tok)
          nil
        tok))))

(provide 'rust-mode)
