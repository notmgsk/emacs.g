(defun my/get-defun-sexp ()
  (save-excursion
    (prog2
        (mark-defun)
        (read (buffer-substring (region-beginning) (region-end)))
      (deactivate-mark))))

;;  

(defun my/get-defun-arglist ()
  (interactive)
  (save-excursion
    (let ((sexp (my/get-defun-sexp)))
      ;; 
      (caddr sexp))))

(defun my/cl-symbol-to-package-and-name (cl-name alt-package)
  ;; TODO Allow for all valid symbols (e.g. non-exported symbols
  ;; package::symbol-name)
  (let ((split (split-string cl-name ":")))
    (if (= 2 (length split))
        split
      (list alt-package (car split)))))

(defun my/fake-cl-object (class package)
  (destructuring-bind (package class)
      (my/cl-symbol-to-package-and-name (upcase (format "%s" class))
                                        (upcase (format "%s" package)))
    `(cl:allocate-instance
      (cl:find-class
       (cl:find-symbol ,class
                       (cl:intern ,package 'cl:keyword))))))

(defun my/slime-repl-applicable-functions ()
  (interactive)
  (let* ((symbol (slime-symbol-at-point))
         (package (substring (slime-current-package) 2)))
    (slime-eval
     `(cl:compute-applicable-methods
       (cl:symbol-function
        (cl:find-symbol ,(upcase symbol) ,(upcase package)))
       (cl:list
        ,@(loop for (name class) in (my/get-defun-arglist)
                collect
                (my/fake-cl-object class package)))))))

