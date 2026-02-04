;;; dri3.lisp --- DRI3 Extension

;; 

;;; Code:
(in-package :xlib)

(define-extension "DRI3")

(defun dri3-opcode (display)
  (extension-opcode display "DRI3"))

(defconstant +dri-major+ 1)
(defconstant +dri-minor+ 4)

(defconstant +dri3-query-version+ 0)
(defconstant +dri3-open+ 1)
(defconstant +dri3-pixmap-from-buffer+ 2)
(defconstant +dri3-buffer-from-pixmap+ 3)
(defconstant +dri3-fence-from-fd+ 4)
(defconstant +dri3-fd-from-fence+ 5)
;; 1.2
(defconstant +dri3-get-supported-modifiers+ 6)
(defconstant +dri3-pixmap-from-buffers+ 7)
(defconstant +dri3-buffers-from-pixmap+ 8)
;; 1.3
(defconstant +dri3-set-drm-device-in-use+ 9)
;; 1.4
(defconstant +dri3-import-syncobj+ 10)
(defconstant +dri3-free-syncobj+ 11)
