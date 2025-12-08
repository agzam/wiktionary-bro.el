;;; wiktionary-bro-tests.el --- tests -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: December 24, 2022
;; Modified: December 24, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/agzam/wiktionary-bro.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; Tests for wiktionary-bro
;;
;;; Code:

(require 'buttercup)
(require 'wiktionary-bro)

(defvar wiktionary-bro-test--simple-table
  "<table><tbody><tr><th>H1</th><th>H2</th></tr><tr><td>C1</td><td>C2</td></tr></tbody></table>")

(defvar wiktionary-bro-test--colspan-table
  "<table><tbody><tr><th colspan=\"2\">Span</th></tr><tr><td>A</td><td>B</td></tr></tbody></table>")

(defvar wiktionary-bro-test--rowspan-table
  "<table><tbody><tr><th rowspan=\"2\">Label</th><td>V1</td></tr><tr><td>V2</td></tr></tbody></table>")

(defvar wiktionary-bro-test--audio-table
  "<table class=\"audiotable\"><tbody><tr><td>Audio</td></tr></tbody></table>")

(defvar wiktionary-bro-test--layout-table
  "<table><tbody><tr><td><ul><li>X</li></ul></td></tr></tbody></table>")

(defun wiktionary-bro-test--parse (html)
  (with-temp-buffer (insert html) (libxml-parse-html-region (point-min) (point-max))))

(defun wiktionary-bro-test--table (html)
  (car (dom-by-tag (wiktionary-bro-test--parse html) 'table)))

(describe "wiktionary-bro--superscript-number"
  (it "converts digits"
    (expect (wiktionary-bro--superscript-number 1) :to-equal "¹")
    (expect (wiktionary-bro--superscript-number 12) :to-equal "¹²")))

(describe "wiktionary-bro--table-has-audio-p"
  (it "detects audio tables"
    (expect (wiktionary-bro--table-has-audio-p (wiktionary-bro-test--table wiktionary-bro-test--audio-table)) :to-be-truthy))
  (it "returns nil for regular"
    (expect (wiktionary-bro--table-has-audio-p (wiktionary-bro-test--table wiktionary-bro-test--simple-table)) :not :to-be-truthy)))

(describe "wiktionary-bro--table-is-layout-p"
  (it "identifies layout tables"
    (expect (wiktionary-bro--table-is-layout-p (wiktionary-bro-test--table wiktionary-bro-test--layout-table)) :to-be-truthy))
  (it "identifies data tables"
    (expect (wiktionary-bro--table-is-layout-p (wiktionary-bro-test--table wiktionary-bro-test--simple-table)) :not :to-be-truthy)))

(describe "wiktionary-bro--parse-table-to-grid"
  (it "parses dimensions"
    (let ((r (wiktionary-bro--parse-table-to-grid (wiktionary-bro-test--table wiktionary-bro-test--simple-table))))
      (expect (nth 3 r) :to-equal 2)
      (expect (nth 4 r) :to-equal 2)))
  (it "extracts content"
    (let ((grid (nth 0 (wiktionary-bro--parse-table-to-grid (wiktionary-bro-test--table wiktionary-bro-test--simple-table)))))
      (expect (gethash '(0 . 0) grid) :to-equal "H1")))
  (it "handles colspan"
    (let ((spans (nth 1 (wiktionary-bro--parse-table-to-grid (wiktionary-bro-test--table wiktionary-bro-test--colspan-table)))))
      (expect (cdr (gethash '(0 . 0) spans)) :to-equal 2)))
  (it "handles rowspan"
    (let ((spans (nth 1 (wiktionary-bro--parse-table-to-grid (wiktionary-bro-test--table wiktionary-bro-test--rowspan-table)))))
      (expect (car (gethash '(0 . 0) spans)) :to-equal 2))))

(describe "wiktionary-bro--table-to-ascii"
  (it "renders borders"
    (let ((r (wiktionary-bro--table-to-ascii (wiktionary-bro-test--table wiktionary-bro-test--simple-table))))
      (expect r :to-match "\\+")
      (expect r :to-match "|")))
  (it "contains content"
    (let ((r (wiktionary-bro--table-to-ascii (wiktionary-bro-test--table wiktionary-bro-test--simple-table))))
      (expect r :to-match "H1")
      (expect r :to-match "C1")))
  (it "handles colspan"
    (expect (wiktionary-bro--table-to-ascii (wiktionary-bro-test--table wiktionary-bro-test--colspan-table)) :to-match "Span"))
  (it "handles rowspan"
    (let ((r (wiktionary-bro--table-to-ascii (wiktionary-bro-test--table wiktionary-bro-test--rowspan-table))))
      (expect r :to-match "Label")
      (expect r :to-match "V1"))))

;;; wiktionary-bro-tests.el ends here
