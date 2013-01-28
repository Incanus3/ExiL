;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; ID:       $Id$
;;;; Purpose:  Listener functions for XLUnit
;;;;
;;;; *************************************************************************

(in-package #:xlunit)

(defclass test-listener ()
  ())

(defmethod start-test ((obj test-listener) tcase)
  (declare (ignore tcase)))

(defmethod end-test ((obj test-listener) tcase)
  (declare (ignore tcase)))


