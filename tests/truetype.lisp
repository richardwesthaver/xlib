;;; truetype.lisp
(in-package :xlib/tests)

(defparameter *display* nil)

(defparameter *screen* nil)
(defparameter *root* nil)

(defun font-window ()
  (cache-fonts)
  (with-test-window
    (xlib:map-window *window*)
    (setf (xlib:gcontext-foreground *gcontext*) *black*)
    (xlib:event-case (*display* :force-output-p t
                                :discard-p t)
      (:exposure ()
                 (xlib:clear-area *window* :width (xlib:drawable-width *window*)
                                         :height (xlib:drawable-height *window*))
                 (draw-text *window* *gcontext* *font* "The quick brown fox jumps over the lazy dog." 100 100 :draw-background-p t)
                 (rotatef (xlib:gcontext-foreground *gcontext*) (xlib:gcontext-background *gcontext*))
                 (draw-text-line *window* *gcontext* *font* "Съешь же ещё этих мягких французских булок, да выпей чаю." 100 (+ 100 (baseline-to-baseline *window* *font*)) :draw-background-p t)
                 (setf (font-antialias *font*) t)
                 (setf (font-subfamily *font*) "Italic")
                 (draw-text *window* *gcontext* *font* "Жебракують філософи при ґанку церкви в Гадячі, ще й шатро їхнє п’яне знаємо." 100 (+ 100 (* 2 (baseline-to-baseline *window* *font*))) :draw-background-p t)
                 (setf (font-overline *font*) t)
                 (draw-text *window* *gcontext* *font* "Press space to exit. Нажмите пробел для выхода." 100 (+ 100 (* 3 (baseline-to-baseline *window* *font*))) :draw-background-p t)
                 nil)
      (:button-press () t)
      (:key-press (code state) (char= #\Space (xlib:keycode->character *display* code state))))))
