;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*-

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel)
  (require :aserve)
  (require :sax))

(defpackage natsoft
  (:use common-lisp joshua net.aserve.client net.xml.sax)
  (:shadow type-of speed time type)
  (:export ))



