;;; translate.lisp --- X Key Translations

;;; Code:
(in-package :xlib)

(setq *default-keysym-translate-mask*
  (the (or (member :modifiers) mask16 list)
       (logand #xff (lognot (make-state-mask :lock)))))

(defun xkeysyms-from-character (character &optional display)
  ;; Given a character, return a list of all matching keysyms.
  ;; If DISPLAY is given, translations specific to DISPLAY are used,
  ;; otherwise only global translations are used.
  ;; Implementation dependent function.
  ;; May be slow [i.e. do a linear search over all known keysyms]
  (declare (type t character)
	   (type (or null display) display))
  (let ((result (keysyms-from-character character)))
    (when display
      (dolist (mapping (display-keysym-translation display))
	(when (eql character (second mapping))
	  (push (first mapping) result))))
    result))

;; Keysym mapping functions
(defun display-keyboard-mapping (display)
  (declare (type display display))
  (or (display-keysym-mapping display)
      (setf (display-keysym-mapping display) (keyboard-mapping display))))

(defun keysym-from-keycode (display keycode keysym-index)
  (declare (type display display)
	   (type card8 keycode)
	   (type card8 keysym-index))
  (let* ((mapping (display-keyboard-mapping display))
	 (keysym (aref mapping keycode keysym-index)))
    (declare (type (simple-array keysym (* *)) mapping)
	     (type keysym keysym))
    ;; The keysym-mapping is brain dammaged.
    ;; Mappings for both-case alphabetic characters have the
    ;; entry for keysym-index zero set to the uppercase keysym
    ;; (this is normally where the lowercase keysym goes), and the
    ;; entry for keysym-index one is zero.
    (cond ((zerop keysym-index)			; Lowercase alphabetic keysyms
	   (keysym-downcase keysym))
	  ((and (zerop keysym) (plusp keysym-index)) ; Get the uppercase keysym
	   (aref mapping keycode 0))
	  (t keysym))))

(defun character-from-keysym (display keysym &optional (state 0))
  ;; Find the character associated with a keysym.
  ;; STATE can be used to set character attributes.
  ;; Implementation dependent function.
  (declare (type display display)
	   (type keysym keysym)
	   (type card16 state))
  (declare (values (or null character)))
  (let* ((display-mappings (cdr (assoc keysym (display-keysym-translation display))))
	 (mapping (or ;; Find the matching display mapping
		   (dolist (mapping display-mappings)
		     (when (mapping-matches-p display state mapping)
		       (return mapping)))
		   ;; Find the matching static mapping
		   (when-let ((mapping (gethash keysym *keysym-character-table*)))
		     (when (mapping-matches-p display state mapping)
		       mapping)))))
    (when mapping
      ;; (or (keysym-mapping-translate mapping) 'default-keysym-translate)
      (funcall 'default-keysym-translate
	       display state (char-map-char mapping)))))

(defun mapping-matches-p (display state mapping)
  ;; Returns T when the modifiers and mask in MAPPING satisfies STATE for DISPLAY
  (declare (type display display)
	   (type mask16 state)
	   (type list mapping))
  (flet
      ((mask-from-modifiers (display-mapping modifiers errorp &aux (mask 0))
         ;; Convert MODIFIERS, which is a modifier mask, or a list of state-mask-keys into a mask.
         ;; If ERRORP is non-nil, return NIL when an unknown modifier is specified,
         ;; otherwise ignore unknown modifiers.
         (declare (type list display-mapping)	; Alist of (keysym . mask)
		  (type (or mask16 list) modifiers)
		  (type mask16 mask))
         (declare (values (or null mask16)))
         (if (numberp modifiers)
	     modifiers
	     (dolist (modifier modifiers mask)
	       (declare (type symbol modifier))
	       (let ((bit (position modifier (the simple-vector +state-mask-vector+) :test #'eq)))
	         (setq mask
		       (logior mask
			       (if bit
			           (ash 1 bit)
			           (or (cdr (assoc modifier display-mapping))
				       ;; bad modifier
				       (if errorp
				           (return-from mask-from-modifiers nil)
				           0))))))))))

    (let* ((display-mapping (get-display-modifier-mapping display))
	   (mapping-modifiers (third mapping))
	   (modifiers (or (mask-from-modifiers display-mapping (or mapping-modifiers 0) t)
			  (return-from mapping-matches-p nil)))
	   (mapping-mask (or (fourth mapping)	; If no mask, use the default.
			     (if mapping-modifiers	        ; If no modifiers, match anything.
				 *default-keysym-translate-mask*
			         0)))
	   (mask (if (eq mapping-mask :modifiers)
		     modifiers
		     (mask-from-modifiers display-mapping mapping-mask nil))))
      (declare (type mask16 modifiers mask))
      (= (logand state mask) modifiers))))

(defun default-keysym-index (display keycode state)
  ;; Returns a keysym-index for use with character-from-keycode
  (declare (values card8))
  (macrolet ((keystate-p (state keyword)
	       `(logbitp ,(position keyword +state-mask-vector+) ,state)))
    (let* ((mapping (display-keyboard-mapping display))
	   (keysyms-per-keycode (array-dimension mapping 1))
	   (symbolp (and (> keysyms-per-keycode 2)
			 (state-keysymp display state character-set-switch-keysym)))
	   (result (if symbolp 2 0)))
      (declare (type (simple-array keysym (* *)) mapping)
	       (type generalized-boolean symbolp)
	       (type card8 keysyms-per-keycode result))
      (when (and (< result keysyms-per-keycode)
		 (keysym-shift-p display state (keysym-cased-p
						(aref mapping keycode 0))))
	(incf result))
      result)))

(defun keysym-shift-p (display state uppercase-alphabetic-p &key
		                                            shift-lock-xors
		                                            (control-modifiers
			                                     '#.(list left-meta-keysym left-super-keysym left-hyper-keysym)))
  (declare (type display display)
	   (type card16 state)
	   (type generalized-boolean uppercase-alphabetic-p)
	   (type generalized-boolean shift-lock-xors));;; If T, both SHIFT-LOCK and SHIFT is the same
	                                  ;;; as neither if the character is alphabetic.
  (macrolet ((keystate-p (state keyword)
	       `(logbitp ,(position keyword +state-mask-vector+) ,state)))
    (let* ((controlp (or (keystate-p state :control)
			 (dolist (modifier control-modifiers)
			   (when (state-keysymp display state modifier)
			     (return t)))))
	   (shiftp (keystate-p state :shift))
	   (lockp  (keystate-p state :lock))
	   (alphap (or uppercase-alphabetic-p
		       (not (state-keysymp display #.(make-state-mask :lock)
					   caps-lock-keysym)))))
      (declare (type generalized-boolean controlp shiftp lockp alphap))
      ;; Control keys aren't affected by lock
      (unless controlp
	;; Not a control character - check state of lock modifier
	(when (and lockp
		   alphap
		   (or (not shiftp) shift-lock-xors))	; Lock doesn't unshift unless shift-lock-xors
	  (setq shiftp (not shiftp))))
      shiftp)))

;;; default-keysym-index implements the following tables:

;;; control shift caps-lock character               character
;;;   0       0       0       #\a                      #\8
;;;   0       0       1       #\A                      #\8
;;;   0       1       0       #\A                      #\*
;;;   0       1       1       #\A                      #\*
;;;   1       0       0       #\control-A              #\control-8
;;;   1       0       1       #\control-A              #\control-8
;;;   1       1       0       #\control-shift-a        #\control-*
;;;   1       1       1       #\control-shift-a        #\control-*
;;;
;;; control shift shift-lock character               character
;;;   0       0       0       #\a                      #\8
;;;   0       0       1       #\A                      #\*
;;;   0       1       0       #\A                      #\*
;;;   0       1       1       #\A                      #\8
;;;   1       0       0       #\control-A              #\control-8
;;;   1       0       1       #\control-A              #\control-*
;;;   1       1       0       #\control-shift-a        #\control-*
;;;   1       1       1       #\control-shift-a        #\control-8
(defun character-from-keycode (display keycode state &key keysym-index
	                                              (keysym-index-function #'default-keysym-index))
  ;; keysym-index defaults to the result of keysym-index-function which
  ;; is called with the following parameters:
  ;; (char0 state caps-lock-p keysyms-per-keycode)
  ;; where char0 is the "character" object associated with keysym-index 0 and
  ;; caps-lock-p is non-nil when the keysym associated with the lock
  ;; modifier is for caps-lock.
  ;; STATE can also used for setting character attributes.
  ;; Implementation dependent function.
  (declare (type display display)
	   (type card8 keycode)
	   (type card16 state)
	   (type (or null card8) keysym-index)
	   (type (or null (function (base-char card16 generalized-boolean card8) card8))
		 keysym-index-function))
  (declare (values (or null character)))
  (let* ((index (or keysym-index
		    (funcall keysym-index-function display keycode state)))
	 (keysym (if index (keysym-from-keycode display keycode index) 0)))
    (declare (type (or null card8) index)
	     (type keysym keysym))
    (when (plusp keysym)
      (character-from-keysym display keysym state))))

(defun get-display-modifier-mapping (display)
  (labels ((keysym-replace (display modifiers mask &aux result)
	     (dolist (modifier modifiers result)
	       (push (cons (keysym-from-keycode display modifier 0) mask) result))))
    (or (display-modifier-mapping display)
	(multiple-value-bind (shift lock control mod1 mod2 mod3 mod4 mod5)
	    (modifier-mapping display)
	  (setf (display-modifier-mapping display)
		(nconc (keysym-replace display shift #.(make-state-mask :shift))
		       (keysym-replace display lock #.(make-state-mask :lock))
		       (keysym-replace display control #.(make-state-mask :control))
		       (keysym-replace display mod1 #.(make-state-mask :mod-1))
		       (keysym-replace display mod2 #.(make-state-mask :mod-2))
		       (keysym-replace display mod3 #.(make-state-mask :mod-3))
		       (keysym-replace display mod4 #.(make-state-mask :mod-4))
		       (keysym-replace display mod5 #.(make-state-mask :mod-5))))))))

(defun state-keysymp (display state keysym)
  ;; Returns T when a modifier key associated with KEYSYM is on in STATE
  (declare (type display display)
	   (type card16 state)
	   (type keysym keysym))
  (let* ((mapping (get-display-modifier-mapping display))
	 (mask (assoc keysym mapping)))
    (and mask (plusp (logand state (cdr mask))))))

(defun mapping-notify (display request start count)
  ;; Called on a mapping-notify event to update
  ;; the keyboard-mapping cache in DISPLAY
  (declare (type display display)
	   (type (member :modifier :keyboard :pointer) request)
	   (type card8 start count)
	   (ignore count start))
  ;; Invalidate the keyboard mapping to force the next key translation to get it
  (case request
    (:modifier 
     (setf (display-modifier-mapping display) nil))
    (:keyboard
     (setf (display-keysym-mapping display) nil))))

(defun keysym-in-map-p (display keysym keymap)
  ;; Returns T if keysym is found in keymap
  (declare (type display display)
	   (type keysym keysym)
	   (type (bit-vector 256) keymap))
  ;; The keysym may appear in the keymap more than once,
  ;; So we have to search the entire keysym map.
  (do* ((min (display-min-keycode display))
	(max (display-max-keycode display))
	(map (display-keyboard-mapping display))
	(jmax (min 2 (array-dimension map 1)))
	(i min (1+ i)))
       ((> i max))
    (declare (type card8 min max jmax)
	     (type (simple-array keysym (* *)) map))
    (when (and (plusp (aref keymap i))
	       (dotimes (j jmax)
		 (when (= keysym (aref map i j)) (return t))))
      (return t))))

(defun character-in-map-p (display character keymap)
  ;; Implementation dependent function.
  ;; Returns T if character is found in keymap
  (declare (type display display)
	   (type character character)
	   (type (bit-vector 256) keymap))
  ;; Check all one bits in keymap
  (do* ((min (display-min-keycode display))
	(max (display-max-keycode display))
	(jmax (array-dimension (display-keyboard-mapping display) 1))
	(i min (1+ i)))
       ((> i max))
    (declare (type card8 min max jmax))
    (when (and (plusp (aref keymap i))
	       ;; Match when character is in mapping for this keycode
	       (dotimes (j jmax)
		 (when (eql character (character-from-keycode display i 0 :keysym-index j))
		   (return t))))
      (return t))))

(defun keycodes-from-keysym (display keysym)
  ;; Return keycodes for keysym, as multiple values
  (declare (type display display)
	   (type keysym keysym))
  ;; The keysym may appear in the keymap more than once,
  ;; So we have to search the entire keysym map.
  (do* ((min (display-min-keycode display))
	(max (display-max-keycode display))
	(map (display-keyboard-mapping display))
	(jmax (min 2 (array-dimension map 1)))
	(i min (1+ i))
	(result nil))
       ((> i max) (values-list result))
    (declare (type card8 min max jmax)
	     (type (simple-array keysym (* *)) map))
    (dotimes (j jmax)
      (when (= keysym (aref map i j))
	(push i result)))))
