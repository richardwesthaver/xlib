(in-package #:xlib/tests)

;;; Custom assertions
(defmacro test-required-params (function-form &rest required-params)
  "Asserts that the function throws missing-parameter errors when any required parameter is missing."
  (labels ((make-pairs (lst)
	     (assert (evenp (length lst)))
	     (and lst
		  (cons (list (first lst) (second lst))
			(make-pairs (cddr lst)))))
	   (get-all-combinations-without-one-elem (keyword-list)
	     (labels ((get-combinations-rec (pre el post)
			(if post
			    (cons (concatenate 'list pre post)
				  (get-combinations-rec (cons el pre)
							(car post)
							(cdr post)))
			    (list pre))))
	       (get-combinations-rec nil (car keyword-list) (cdr keyword-list)))))
    `(progn
       ,@(loop for param-list in (mapcar #'std:flatten
					 (get-all-combinations-without-one-elem
					  (make-pairs required-params)))
	    collecting
              `(rt:signals xlib:missing-parameter
                 (,@(std:ensure-list function-form) ,@param-list))))))

;;; Macros
(defmacro with-default-display (display &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (progn ,@body)
       (xlib:close-display ,display))))

(declaim (special *display* *screen* *root* *black* *white* *window* *font* *gcontext*))
(defmacro with-test-window (&body body)
  `(let* ((*display* (xlib:open-default-display))
          (*screen* (xlib:display-default-screen *display*))
          (*root* (xlib:screen-root *screen*))
          (*black* (xlib:screen-black-pixel *screen*))
          (*white* (xlib:screen-white-pixel *screen*))
          (*window*
            (xlib:create-window :parent *root* :x 0 :y 0 :width 640 :height 480 
                                :class :input-output
                                :background *black*
                                :event-mask '(:key-press :key-release :exposure :button-press
                                              :structure-notify)))
          (*gcontext* (xlib:create-gcontext
                    :drawable *window*
                    :foreground *black*
                    :background *white*))
          (*font* (make-instance 'font :family "Adwaita Mono" :subfamily "Regular"
                                 :size 36 :antialias t)))
     (unwind-protect (progn ,@body)
       (xlib:free-gcontext *gcontext*)
       (xlib:destroy-window *window*)
       (xlib:close-display *display*))))
