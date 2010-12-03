;;;; brainfuck compiler
;;;; $Id: bf-compiler.lisp,v 1.1 2010/12/03 12:41:13 toshi Exp $

(in-package :cl)

(defpackage :bf-compiler
  (:nickname :bf)
  (:use :cl)
  (:export :bf-compile :bf-convert :brainfuck))

(in-package :bf-compiler)

(define-condition bf-parser-error (error)
  ((character :initarg :character :initform nil))
  (:report
   (lambda (condition stream)
	 (format stream "Unrecognized Character: ~C" (bf-parser-error-character character)))))
  
(defun string->list (str)
  (map 'list #'identity str))

(defvar *stack* nil)  ;; tag

(defun bf-convert (char)
  (case char
	((#\[) (let ((next (gensym)) (break (gensym)))
			 (push next *stack*)
			 (push break *stack*)
			 `(,next (when (= *ptr 0) (go ,break)))))
	((#\]) (let ((break (pop *stack*)) (next (pop *stack*)))
			 `((unless (= *ptr 0) (go ,next)) ,break)))
	((#\+) '((incf *ptr)))
	((#\-) '((decf *ptr)))
	((#\>) '((incf idx)))
	((#\<) '((decf idx)))
	((#\,) '((let ((char (read-char)))
			   (setf *ptr (char-code char)))))
	((#\.) '((princ (code-char *ptr))))
	(otherwise (error "Unrecognized Character: ~C" char))))

(defun whitespace-p (char)
 (member char '(#\Space #\Tab #\Newline)))

(defun bf-compile (program)
  (loop for char in (string->list (remove-if #'whitespace-p program))
	 append (bf-convert char)))

(defun brainfuck (program)
  `(symbol-macrolet ((*ptr (aref memory idx)))
	 (prog
		 ((memory (make-array 30000 :element-type '(unsigned-byte 8) :initial-element 0))
		  (idx 0))
		,@(bf-compile program)
        (return))))

;; example -- say hello
; (eval
;    (brainfuck "+++++++++
;                [>++++++++>+++++++++++>+++++<<<-]
;                >.>++.+++++++..+++.>-.------------.<
;                ++++++++.--------.+++.------.--------.>+.")