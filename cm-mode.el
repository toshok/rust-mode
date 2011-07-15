;; Wrapper for CodeMirror-style emacs modes. Highlighting is done by
;; running a stateful parser (with first-class state object) over the
;; buffer, line by line, using the output to add 'face properties, and
;; storing the parser state at the end of each line. Indentation is
;; done based on the parser state at the start of the line.

(require 'cl)

;; Mode data structure

(defstruct cm-mode
  (token (error "Must provide a token function."))
  (start-state (lambda () 'null))
  (copy-state 'cm-default-copy-state)
  (compare-state 'eq)
  (indent nil))

(defun cm-default-copy-state (state)
  (if (consp state) (copy-sequence state) state))

(defun cm-clear-work-items (from1 to1)
  (setf cm-worklist (delete-if (lambda (i) (and (>= i from1) (<= i to1))) cm-worklist)))

;; Parsing utilities

(defun cm-eat-set (set)
  (> (skip-chars-forward set (+ (point) 1)) 0))
(defun cm-eat-re (re)
  (when (looking-at re)
    (goto-char (match-end 0))
    t))
(defun cm-eat-char (c)
  (when (eq (char-after) c) (forward-char 1) t))
(defun cm-eat-string (str)
  (let* ((p (point)) (e (+ p (length str))))
    (when (and (<= e (point-max))
               (equal (buffer-substring-no-properties p e) str))
      (goto-char e) t)))
(defun cm-eat-whitespace ()
  (cm-eat-set " \t"))

;; Indentation

(defun cm-indent ()
  (let ((indent-pos))
    (save-excursion
      (beginning-of-line)
      (let ((state (cm-state-for-point))
            (old-indent (current-indentation)))
        (back-to-indentation)
        (let ((new-indent (funcall (cm-mode-indent cm-cur-mode) state)))
          (indent-line-to new-indent)
          (setf indent-pos (point))
          (unless (= old-indent new-indent)
            (beginning-of-line)
            (cm-highlight-line state)
            (when (< (point) (point-max))
              (put-text-property (point) (+ (point) 1) 'cm-parse-state state))))))
    (when (< (point) indent-pos)
      (goto-char indent-pos))))

(defun cm-backtrack-to-state ()
  (let ((backtracked 0)
        (min-indent most-positive-fixnum)
        min-indented)
    (loop
     (when (= (point) (point-min))
       (return (funcall (cm-mode-start-state cm-cur-mode))))
     (let ((st (get-text-property (- (point) 1) 'cm-parse-state)))
       (when st (return (funcall (cm-mode-copy-state cm-cur-mode) st))))
     (let ((i (current-indentation)))
       (when (< i min-indent)
         (setf min-indent i min-indented (point))))
     (when (> (incf backtracked) 30)
       (goto-char min-indented)
       (return (funcall (cm-mode-start-state cm-cur-mode))))
     (forward-line -1))))

(defun cm-state-for-point ()
  (let ((pos (point))
        (state (cm-backtrack-to-state)))
    (while (< (point) pos)
      (cm-highlight-line state)
      (put-text-property (point) (+ (point) 1) 'cm-parse-state
                         (funcall (cm-mode-copy-state cm-cur-mode) state))
      (forward-char 1))
    state))

;; Highlighting

(defun cm-highlight-line (state)
  (let ((eol (point-at-eol)))
    (remove-text-properties (point) eol '(face))
    (loop
     (let ((p (point)))
       (when (= p eol) (return))
       (let ((style (funcall (cm-mode-token cm-cur-mode) state)))
         (when (= p (point)) (error "Nothing consumed."))
         (when (> p eol) (error "Parser moved past EOL"))
         (when style
           (put-text-property p (point) 'face style)))))))

(defun cm-find-state-before-point ()
  (loop
   (beginning-of-line)
   (when (= (point) 1)
     (return (funcall (cm-mode-start-state cm-cur-mode))))
   (let ((cur (get-text-property (- (point) 1) 'cm-parse-state)))
     (when cur (return (funcall (cm-mode-copy-state cm-cur-mode) cur))))
   (backward-char 1)))

(defun cm-schedule-work ()
  (run-with-idle-timer 0.2 nil 'call/preserved-state 'cm-do-some-work (current-buffer)))

(defun call/preserved-state (f &rest args)
  (let ((modified (buffer-modified-p))
        (buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        deactivate-mark
        buffer-file-name
        buffer-file-truename)
    (unwind-protect (apply f args)
      (unless modified
        (restore-buffer-modified-p nil)))))

(defun cm-do-some-work (buffer)
  (condition-case err
  (with-current-buffer buffer
    (save-excursion
      (while cm-worklist
        (goto-char (apply 'min cm-worklist))
        ;; FIXME test whether this actually allows us to interrupt a parse
        (when (let ((timer-idle-list nil)) (input-pending-p))
          (cm-schedule-work) (return))
        (let ((state (cm-find-state-before-point))
              (startpos (point)))
          (loop
           (cm-highlight-line state)
           (when (= (point) (point-max)) (return))
           (let ((old (get-text-property (point) 'cm-parse-state)))
             (when (and old (funcall (cm-mode-compare-state cm-cur-mode) state old))
               (return))
             (put-text-property (point) (+ (point) 1) 'cm-parse-state
                                (funcall (cm-mode-copy-state cm-cur-mode) state)))
           (when (let ((timer-idle-list nil)) (input-pending-p)) (return))
           (forward-char 1))
          (cm-clear-work-items startpos (point))))))
  (error (print (error-message-string err)))))

(defun cm-after-change-function (from to oldlen)
  (remove-text-properties from to '(cm-parse-state))
  (push from cm-worklist)
  (cm-schedule-work))

;; Entry function

(defun cm-mode (mode)
  (set (make-local-variable 'cm-cur-mode) mode)
  (set (make-local-variable 'cm-worklist) '(1))
  (when (cm-mode-indent mode)
    (set (make-local-variable 'indent-line-function)
         (lambda () (call/preserved-state 'cm-indent))))
  (add-hook 'after-change-functions 'cm-after-change-function t t)
  (call/preserved-state 'cm-do-some-work (current-buffer)))

(provide 'cm-mode)
