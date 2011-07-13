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
  (setq cm-worklist (delete-if (lambda (i) (and (>= i from1) (< i to1))) cm-worklist)))

;; Parsing utilities FIXME find built-in equivalents

(defun cm-eat-set (set)
  (> (skip-chars-forward set) 0))
(defun cm-eat-re (re)
  (when (looking-at re)
    (goto-char (match-end 0))
    t))
(defun cm-eat-whitespace ()
  (cm-eat-set " 	"))

;; Indentation

(defun cm-indent ()
  (let ((indent-f (cm-mode-indent cm-cur-mode)))
    (if indent-f
        (cm-indent-intelligently indent-f)
      (cm-indent-simply))))

(defun cm-indent-intelligently (func)
  (beginning-of-line)
  (let ((state (or (and (> (point) 1) (get-text-property (- (point) 1) 'cm-parse-state))
                   (cm-mode-start-state cm-cur-mode))))
    (indent-line-to (funcall func state))))

(defun cm-indent-simply ()
  (beginning-of-line)
  (if (= (point) 1)
      (indent-line-to 0)
    (backward-char 1)
    (let ((prev (current-indentation)))
      (forward-char 1)
      (indent-line-to prev))))

;; Highlighting

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
   (let ((cur (get-text-property (- (point) 1) 'cm-parse-state)))
     (when cur (return cur)))
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
  (with-current-buffer buffer
    (save-excursion
      (while cm-worklist
        (when (input-pending-p) (cm-schedule-work) (return))
        (goto-char (apply 'min cm-worklist))
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
           (when (input-pending-p) (return))
           (forward-char 1))
          (cm-clear-work-items startpos (point)))))))

(defun cm-after-change-function (from to oldlen)
  (remove-text-properties from to '(cm-parse-state))
  (push from cm-worklist)
  (cm-schedule-work))

;; Entry function

(defun cm-mode (mode)
  (set (make-local-variable 'cm-cur-mode) mode)
  (set (make-local-variable 'cm-worklist) '(1))
  (set (make-local-variable 'indent-line-function) 'cm-indent)
  (add-hook 'after-change-functions 'cm-after-change-function t t)
  (call/preserved-state 'cm-do-some-work (current-buffer)))

(provide 'cm-mode)