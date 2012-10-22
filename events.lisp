(in-package :gtk-utils)

;;; utilities to make event handling in cl-gtk more convenient

(defmacro define-signals (&rest signal-names)
  `(progn
     ,@(mapcar
        (lambda (signal-name)
          (multiple-value-bind (signal-symbol signal-string)
              (let ((signal-string (and (symbolp signal-name)
                                        (symbol-name signal-name))))
                (cond
                  ;; signal name and string explicitly given:
                  ((listp signal-name) (values-list signal-name))
                  ;; signal symbol starts with on-, strip that in 
                  ((begins-with signal-string "on-" :test #'string-equal)
                   (values signal-name (subseq signal-string 3)))
                  ;; otherwise add ON- to the signal-name
                  (t (values (symb 'on- signal-name) signal-string))))
            `(defmacro ,signal-symbol (object &body body)
               `(connect-signal ,object ,',(format nil "~(~A~)" signal-string)
                                (ilambda (&rest args) ,@body)))))
        signal-names)))

(define-signals on-clicked on-toggled on-changed on-destroy)

(defun default-destroy (window)
  (on-destroy window (leave-gtk-main)))

(defun group-radio-buttons (&rest radio-buttons)
  (mapc (lambda (first second)
          (setf (radio-button-group first)
                (radio-button-group second)))
        radio-buttons (rest radio-buttons)))
