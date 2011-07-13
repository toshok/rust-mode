; keyword comment constant function-name preprocessor string variable-name warning

(defun cm-test ()
  (interactive)
  (cm-mode (make-cm-mode :token 'demo-token)))

;; Test stuff

(defun demo-token (st)
  (cond ((cm-eat-whitespace) nil)
        ((cm-eat-re "[a-z]") 'font-lock-keyword-face)
        ((cm-eat-re "[0-9]") 'font-lock-constant-face)
        ((forward-char 1) nil)))

(provide 'rust-mode)