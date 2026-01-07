;;; big-requests.lisp

;;; Commentary:

;; No new events or errors are defined by this extension.  (Big
;; Requests Extension, section 3)

;; The name of this extension is "BIG-REQUESTS" (Big Requests
;; Extension, section 4)

;;; Code:
(in-package "XLIB")
(define-extension "BIG-REQUESTS")
(defun enable-big-requests (display)
  (declare (type display display))
  (let ((opcode (extension-opcode display "BIG-REQUESTS")))
    (with-buffer-request-and-reply (display opcode nil)
	((data 0))
      (let ((maximum-request-length (card32-get 8)))
	(setf (display-extended-max-request-length display) 
	      maximum-request-length)))))
