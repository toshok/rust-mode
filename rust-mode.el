; keyword comment constant function-name preprocessor string variable-name warning

(defun rust-mode-2 ()
  (interactive)
  (cm-mode (make-cm-mode :token 'rust-token
                         :start-state 'make-rust-state
                         :copy-state 'copy-rust-state
                         :compare-state 'rust-compare-state)))

(defstruct rust-state
  (tokenize 'rust-token-base)
  (token-arg nil))

; FIXME
(defun rust-compare-state (a b)
  (eq (rust-state-tokenize a) (rust-state-tokenize b)))

(defvar rust-operator-chars "+-/%=<>!*&|")

(defun rust-token-base (st)
  (case (char-after)
    (?\" (setf (rust-state-tokenize st) 'rust-token-string)
         (forward-char 1)
         (rust-token-string st))
    (?/ (cond ((cm-eat-string "//")
               (end-of-line) 'font-lock-comment-face)
              ((cm-eat-string "/*")
               (setf (rust-state-tokenize st) 'rust-token-comment
                     (rust-state-token-arg st) 1)
               (rust-token-comment st))
              (t (while (cm-eat-set rust-operator-chars)) nil)))
    (?# (cm-eat-re "[a-z_]+") 'font-lock-preprocessor-face)
    (t (cond ((cm-eat-re "[a-z_]+") (match-string 0))
             ((cm-eat-re "[0-9]+") 'font-lock-constant-face)
             (t (forward-char 1) nil)))))

(defun rust-token-string (st)
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
    (let ((tok (funcall (rust-state-tokenize st) st)))
      (if (stringp tok)
          nil
        tok))))

(provide 'rust-mode)
