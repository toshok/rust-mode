; keyword comment constant function-name preprocessor string variable-name warning

(defstruct cm-mode
  (token (error "Must provide a token function."))
  (start-state (lambda () 'null))
  (copy-state 'cm-default-copy-state)
  (indent nil))

(defun cm-default-copy-state (state)
  (if (consp state) (copy-sequence state) state))

(defun cm-clear-work-items (from1 to1)
  (setq cm-worklist (delete-if (lambda (i) (and (>= i from1) (< i to1))) cm-worklist)))

(defun cm-eat-set (set)
  (> (skip-chars-forward set) 0))
(defun cm-eat-re (re)
  (when (looking-at re)
    (goto-char (match-end 0))
    t))
(defun cm-eat-whitespace ()
  (cm-eat-set " 	"))

(defun cm-highlight-line (state)
  (let ((eol (point-at-eol)))
    (remove-text-properties (point) eol '(face))
    (loop
     (let ((p (point)))
       (when (= p eol) (return))
       (let ((style (funcall (cm-mode-token cm-cur-mode) state)))
         (when style
           (put-text-property p (point) 'face style)))))))

(defun cm-find-state-before-point ()
  (loop
   (beginning-of-line)
   (when (= (point) 1)
     (return (funcall (cm-mode-start-state cm-cur-mode))))
   (let ((cur (get-text-property (- (point) 1) 'cm-parse-state)   ))
     (when cur (return cur)))
   (backward-char 1)))

(defun cm-schedule-work ()
  (run-with-idle-timer 0.2 nil 'cm-do-some-work (current-buffer)))

(defun cm-do-some-work (buffer)
  (with-current-buffer buffer
    (save-excursion
      (while cm-worklist
        (when (input-pending-p) (cm-schedule-work) (return))
        (goto-char (pop cm-worklist))
        (let ((state (cm-find-state-before-point))
              (startpos (point))
              (inhibit-modification-hooks t))
          (loop
           (cm-highlight-line state)
           (when (= (point) (point-max)) (return))
           (let ((done (get-text-property (point) 'cm-parse-state)))
             (when done (return))
             (put-text-property (point) (+ (point) 1) 'cm-parse-state
                                (funcall (cm-mode-copy-state cm-cur-mode) state)))
           (when (input-pending-p) (return))
           (forward-char 1))
          (cm-clear-work-items startpos (point)))))))

(defun cm-after-change-function (from to oldlen)
  (remove-text-properties from to '(cm-parse-state))
  (push from cm-worklist)
  (cm-schedule-work))

(defun cm-mode (mode)
  (set (make-local-variable 'cm-cur-mode) mode)
  (set (make-local-variable 'cm-worklist) '(1))
  (add-hook 'after-change-functions 'cm-after-change-function t t)
  (cm-do-some-work (current-buffer)))

(defun cm-test ()
  (interactive)
  (cm-mode (make-cm-mode :token 'demo-token)))

;; Test stuff

(defun demo-token (st)
  (cond ((cm-skip-whitespace) nil)
        ((cm-eat-re "[a-z]") 'font-lock-keyword-face)
        ((cm-eat-re "[0-9]") 'font-lock-constant-face)
        ((forward-char 1) nil)))
