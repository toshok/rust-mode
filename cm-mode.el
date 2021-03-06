;; Wrapper for CodeMirror-style emacs modes. Highlighting is done by
;; running a stateful parser (with first-class state object) over the
;; buffer, line by line, using the output to add 'face properties, and
;; storing the parser state at the end of each line. Indentation is
;; done based on the parser state at the start of the line.

(eval-when-compile (require 'cl))

;; Mode data structure

(defun make-cm-mode (token &optional start-state copy-state
                           compare-state indent)
  (vector token
          (or start-state (lambda () 'null))
          (or copy-state 'cm-default-copy-state)
          (or compare-state 'eq)
          indent))
(defmacro cm-mode-token (x) `(aref ,x 0))
(defmacro cm-mode-start-state (x) `(aref ,x 1))
(defmacro cm-mode-copy-state (x) `(aref ,x 2))
(defmacro cm-mode-compare-state (x) `(aref ,x 3))
(defmacro cm-mode-indent (x) `(aref ,x 4))

(defvar cm-cur-mode nil)
(defvar cm-worklist nil)

(defun cm-default-copy-state (state)
  (if (consp state) (copy-sequence state) state))

(defun cm-clear-work-items (from to)
  (let ((prev-cons nil)
        (rem cm-worklist))
    (while rem
      (let ((pos (marker-position (car rem))))
        (cond ((or (< pos from) (> pos to)) (setf prev-cons rem))
              (prev-cons (setf (cdr prev-cons) (cdr rem)))
              (t (setf cm-worklist (cdr rem))))
        (setf rem (cdr rem))))))

(defun cm-min-worklist-item ()
  (let ((rest cm-worklist) (min most-positive-fixnum))
    (while rest
      (let ((pos (marker-position (car rest))))
        (when (< pos min) (setf min pos)))
      (setf rest (cdr rest)))
    min))

;; Indentation

(defun cm-indent ()
  (let (indent-pos)
    (save-excursion
      (beginning-of-line)
      (let ((state (cm-preserve-state 'cm-state-for-point))
            (old-indent (current-indentation)))
        (back-to-indentation)
        (setf indent-pos (point))
        (let ((new-indent (funcall (cm-mode-indent cm-cur-mode) state)))
          (unless (= old-indent new-indent)
            (indent-line-to new-indent)
            (setf indent-pos (point))
            (beginning-of-line)
            (cm-preserve-state
             (lambda ()
               (cm-highlight-line state)
               (when (< (point) (point-max))
                 (put-text-property (point) (+ (point) 1) 'cm-parse-state state))))))))
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
       (when (and st (save-excursion
                       (backward-char)
                       (beginning-of-line)
                       (not (looking-at "[	 ]*$"))))
         (return (funcall (cm-mode-copy-state cm-cur-mode) st))))
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
      (forward-char))
    state))

;; Highlighting

(defun cm-highlight-line (state)
  (let ((eol (point-at-eol)))
    (remove-text-properties (point) eol '(face))
    (loop
     (let ((p (point)))
       (when (= p eol) (return))
       (let ((style (funcall (cm-mode-token cm-cur-mode) state)))
         (when (= p (point)) (print (point)) (error "Nothing consumed."))
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
   (backward-char)))

(defvar cm-log-list ())
(defmacro cm-log (x) ())

(defun cm-schedule-work (delay)
  (cm-log "scheduling")
  (run-with-idle-timer delay nil 'cm-preserve-state 'cm-do-some-work (current-buffer)))

(defun cm-preserve-state (f &rest args)
  (cm-log "entering preserve")
  (let ((modified (buffer-modified-p))
        (buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t))
    (unwind-protect (apply f args)
      (cm-log "leaving preserve")
      (unless modified
        (restore-buffer-modified-p nil)))))

(defun cm-do-some-work (buffer)
  (condition-case err
  (with-current-buffer buffer
    (cm-log "doing work")
    (let ((end-time (time-add (current-time) (list 0 0 500)))
          (quitting nil))
    (save-excursion
      (while (and (not quitting) cm-worklist)
        (goto-char (cm-min-worklist-item))
        (cm-log (list "found work" (point)))
        (let ((state (cm-find-state-before-point))
              (startpos (point))
              (timer-idle-list nil))
          (loop
           (cm-highlight-line state)
           (when (= (point) (point-max)) (return))
           (let ((old (get-text-property (point) 'cm-parse-state)))
             (when (and old (funcall (cm-mode-compare-state cm-cur-mode) state old))
               (return))
             (put-text-property (point) (+ (point) 1) 'cm-parse-state
                                (funcall (cm-mode-copy-state cm-cur-mode) state)))
           (when (or (let ((timer-idle-list nil)) (input-pending-p))
                     (time-less-p end-time (current-time)))
             (setf quitting t) (return))
           (forward-char))
          (cm-log (list "clearing work" startpos (point)))
          (cm-clear-work-items startpos (point)))
        (when quitting
          (cm-log (list "quitting, storing" (+ (point) 1)))
          (push (copy-marker (+ (point) 1)) cm-worklist)
          (cm-schedule-work 0.05)))))
    (cm-log "while loop finished"))
  (error (cm-log (list "err" err)) (print (error-message-string err)))))

(defun cm-after-change-function (from to oldlen)
  (cm-log (list "after-change" from to))
  (cm-preserve-state 'remove-text-properties from to '(cm-parse-state))
  (push (copy-marker from) cm-worklist)
  (cm-schedule-work 0.2))

;; Entry function

(defun cm-mode (mode)
  (set (make-local-variable 'cm-cur-mode) mode)
  (set (make-local-variable 'cm-worklist) (list (copy-marker 1)))
  (when (cm-mode-indent mode)
    (set (make-local-variable 'indent-line-function) 'cm-indent))
  (add-hook 'after-change-functions 'cm-after-change-function t t)
  (add-hook 'after-revert-hook (lambda () (cm-after-change-function 1 (point-max) nil)) t t)
  (cm-schedule-work 0.05))

(provide 'cm-mode)
