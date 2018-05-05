;;; -*- Mode: Common-lisp; Package: Natsoft -*-

(defun json-from-file (pathname)
  (setq pathname (pathname pathname))
  (json:decode-json-from-source pathname))