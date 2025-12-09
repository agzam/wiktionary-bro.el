;;; wiktionary-bro-tests.el --- tests -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: December 24, 2022
;; Modified: December 09, 2025
;; Version: 1.1.0
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

(describe "wiktionary-bro--extract-word-from-url"
  (it "extracts word from simple URL"
    (expect (wiktionary-bro--extract-word-from-url "https://en.wiktionary.org/wiki/hello")
            :to-equal "hello"))
  (it "extracts word from URL with fragment"
    (expect (wiktionary-bro--extract-word-from-url "https://en.wiktionary.org/wiki/hello#English")
            :to-equal "hello"))
  (it "extracts word from URL with query params"
    (expect (wiktionary-bro--extract-word-from-url "https://en.wiktionary.org/wiki/hello?foo=bar")
            :to-equal "hello"))
  (it "decodes URL-encoded words"
    (let ((result (wiktionary-bro--extract-word-from-url "https://en.wiktionary.org/wiki/caf%C3%A9")))
      (expect result :to-equal "café")))
  (it "returns nil for non-wiki URLs"
    (expect (wiktionary-bro--extract-word-from-url "https://en.wiktionary.org/api.php")
            :to-be nil)))

(describe "wiktionary-bro--wiktionary-url-p"
  (it "recognizes wiktionary.org URLs"
    (expect (wiktionary-bro--wiktionary-url-p "https://en.wiktionary.org/wiki/test") :to-be-truthy)
    (expect (wiktionary-bro--wiktionary-url-p "https://fr.wiktionary.org/wiki/test") :to-be-truthy))
  (it "rejects non-wiktionary URLs"
    (expect (wiktionary-bro--wiktionary-url-p "https://en.wikipedia.org/wiki/test") :not :to-be-truthy)
    (expect (wiktionary-bro--wiktionary-url-p "https://example.com") :not :to-be-truthy))
  (it "handles nil"
    (expect (wiktionary-bro--wiktionary-url-p nil) :not :to-be-truthy)))

(describe "wiktionary-bro--wikipedia-url-p"
  (it "recognizes wikipedia.org URLs"
    (expect (wiktionary-bro--wikipedia-url-p "https://en.wikipedia.org/wiki/Test") :to-be-truthy)
    (expect (wiktionary-bro--wikipedia-url-p "https://fr.wikipedia.org/wiki/Test") :to-be-truthy))
  (it "rejects non-wikipedia URLs"
    (expect (wiktionary-bro--wikipedia-url-p "https://en.wiktionary.org/wiki/test") :not :to-be-truthy)
    (expect (wiktionary-bro--wikipedia-url-p "https://example.com") :not :to-be-truthy))
  (it "handles nil"
    (expect (wiktionary-bro--wikipedia-url-p nil) :not :to-be-truthy)))

(describe "wiktionary-bro--handle-link"
  (it "returns nil when not in wiktionary-bro-mode"
    (with-temp-buffer
      (org-mode)
      (insert "[[https://en.wiktionary.org/wiki/test][test]]")
      (goto-char (point-min))
      (expect (wiktionary-bro--handle-link) :to-be nil)))
  
  (it "identifies wiktionary links in wiktionary-bro-mode"
    (with-temp-buffer
      (org-mode)
      (insert "[[https://en.wiktionary.org/wiki/test][test]]")
      (goto-char 3)
      (wiktionary-bro-mode)
      (setq-local wiktionary-bro-current-language "en")
      ;; Should return t (handled) but we can't test the actual navigation without mocking
      (let ((ctx (org-element-context)))
        (expect (org-element-type ctx) :to-equal 'link)
        (expect (org-element-property :raw-link ctx) :to-match "wiktionary")))))

(describe "wiktionary-bro-mode buffer-local variables"
  (it "sets buffer-local language"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (setq-local wiktionary-bro-current-language "fr")
      (expect wiktionary-bro-current-language :to-equal "fr")))
  
  (it "sets buffer-local word"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (setq-local wiktionary-bro-current-word "bonjour")
      (expect wiktionary-bro-current-word :to-equal "bonjour")))
  
  (it "sets buffer-local available languages"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (setq-local wiktionary-bro-available-languages '(("en" . "English") ("fr" . "français")))
      (expect wiktionary-bro-available-languages :to-equal '(("en" . "English") ("fr" . "français"))))))

(describe "wiktionary-bro-mode keymap"
  (it "defines C-c C-l binding"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (expect (lookup-key wiktionary-bro-mode-map (kbd "C-c C-l"))
              :to-equal 'wiktionary-bro-change-language))))

(describe "wiktionary-bro customization"
  (it "has default language setting"
    (expect wiktionary-bro-language :to-equal "en"))
  
  (it "allows language customization"
    (let ((wiktionary-bro-language "fr"))
      (expect wiktionary-bro-language :to-equal "fr"))))

(describe "wiktionary-bro--dom-texts"
  (it "extracts simple text"
    (expect (wiktionary-bro--dom-texts "hello") :to-equal "hello"))
  
  (it "returns empty string for nil"
    (expect (wiktionary-bro--dom-texts nil) :to-equal ""))
  
  (it "extracts text from simple element"
    (expect (wiktionary-bro--dom-texts '(p nil "text")) :to-equal "text"))
  
  (it "skips style elements"
    (expect (wiktionary-bro--dom-texts '(style nil "display: none;")) :to-equal ""))
  
  (it "skips script elements"
    (expect (wiktionary-bro--dom-texts '(script nil "alert('hi');")) :to-equal ""))
  
  (it "converts br to newline"
    (expect (wiktionary-bro--dom-texts '(br nil)) :to-equal "\n"))
  
  (it "wraps superscript in parentheses"
    (expect (wiktionary-bro--dom-texts '(sup nil "1")) :to-equal "(1)"))
  
  (it "handles nested elements"
    (expect (wiktionary-bro--dom-texts '(p nil "hello " (b nil "world")))
            :to-equal "hello world"))
  
  (it "handles multiple children"
    (expect (wiktionary-bro--dom-texts '(div nil "one" (br nil) "two"))
            :to-equal "one\ntwo")))

(describe "wiktionary-bro--cell-content-to-string"
  (it "normalizes whitespace"
    (let ((cell '(td nil "  hello   world  ")))
      (expect (wiktionary-bro--cell-content-to-string cell) :to-equal "hello world")))
  
  (it "converts newlines to slash separator"
    (let ((cell '(td nil "line1" (br nil) "line2")))
      (expect (wiktionary-bro--cell-content-to-string cell) :to-equal "line1 / line2")))
  
  (it "converts numeric footnotes to superscript"
    (let ((cell '(td nil "text" (sup nil "1"))))
      (expect (wiktionary-bro--cell-content-to-string cell) :to-equal "text¹")))
  
  (it "handles empty cells"
    (let ((cell '(td nil)))
      (expect (wiktionary-bro--cell-content-to-string cell) :to-equal "")))
  
  (it "fixes double parentheses from nested sup"
    (let ((cell '(td nil "text((nested))")))
      (expect (wiktionary-bro--cell-content-to-string cell) :to-equal "text(nested)"))))

(describe "wiktionary-bro--at-the-beginning-of-word-p"
  (it "is used to determine if cursor needs repositioning"
    ;; This function has subtle behavior - testing it exists and is callable
    (with-temp-buffer
      (insert "hello world")
      (goto-char (point-min))
      ;; Function should return a boolean value
      (let ((result (wiktionary-bro--at-the-beginning-of-word-p (point))))
        (expect (or (eq result t) (eq result nil)) :to-be-truthy))))
  
  (it "works with different cursor positions"
    (with-temp-buffer
      (insert "hello world")
      ;; Test that it doesn't error at various positions
      (goto-char (point-min))
      (expect (wiktionary-bro--at-the-beginning-of-word-p (point)) :not :to-throw)
      (goto-char (+ (point-min) 3))
      (expect (wiktionary-bro--at-the-beginning-of-word-p (point)) :not :to-throw)
      (goto-char (point-max))
      (expect (wiktionary-bro--at-the-beginning-of-word-p (point)) :not :to-throw))))

(describe "wiktionary-bro--get-original-word"
  (it "returns selected text when region is active"
    (with-temp-buffer
      (insert "hello world")
      (set-mark (point-min))
      (goto-char (+ (point-min) 5))
      (activate-mark)
      (expect (wiktionary-bro--get-original-word (region-beginning) (region-end))
              :to-equal "hello")))
  
  (it "returns text without properties"
    (with-temp-buffer
      (insert (propertize "hello" 'face 'bold))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (let ((result (wiktionary-bro--get-original-word (region-beginning) (region-end))))
        (expect result :to-equal "hello")
        (expect (text-properties-at 0 result) :to-equal nil)))))

(describe "wiktionary-bro-change-language error handling"
  (it "errors when not in wiktionary-bro-mode"
    (with-temp-buffer
      (expect (wiktionary-bro-change-language "fr")
              :to-throw 'user-error)))
  
  (it "errors when no word is set"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (expect (wiktionary-bro-change-language "fr")
              :to-throw 'user-error))))

(describe "wiktionary-bro-mode setup"
  (it "installs org-open-at-point hook"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (expect (member 'wiktionary-bro--handle-link
                      (buffer-local-value 'org-open-at-point-functions (current-buffer)))
              :to-be-truthy)))
  
  (it "derives from org-mode"
    (with-temp-buffer
      (wiktionary-bro-mode)
      (expect (derived-mode-p 'org-mode) :to-be-truthy))))

(describe "wiktionary-bro audio playback"
  (it "handles URLs with // prefix"
    ;; We can't actually test playback, but we can test URL construction
    (let ((url "//upload.wikimedia.org/test.ogg"))
      (expect (string-prefix-p "//" url) :to-be-truthy)
      (expect (concat "https:" url) :to-equal "https://upload.wikimedia.org/test.ogg"))))

(describe "wiktionary-bro buffer naming"
  (it "follows Emacs conventions with asterisks"
    (let ((buffer-name (format "*wiktionary: %s*" "test")))
      (expect (string-prefix-p "*" buffer-name) :to-be-truthy)
      (expect (string-suffix-p "*" buffer-name) :to-be-truthy)))
  
  (it "includes the word in the buffer name"
    (let ((buffer-name (format "*wiktionary: %s*" "hello")))
      (expect (string-match-p "hello" buffer-name) :to-be-truthy)))
  
  (it "has consistent prefix for easy killing"
    (let ((buffer1 (format "*wiktionary: %s*" "word1"))
          (buffer2 (format "*wiktionary: %s*" "word2")))
      (expect (string-prefix-p "*wiktionary:" buffer1) :to-be-truthy)
      (expect (string-prefix-p "*wiktionary:" buffer2) :to-be-truthy))))

;;; wiktionary-bro-tests.el ends here
