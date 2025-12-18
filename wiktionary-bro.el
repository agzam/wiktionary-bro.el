;;; wiktionary-bro.el --- Lookup Wiktionary entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: December 24, 2022
;; Modified: December 18, 2025
;; Version: 1.1.2
;; Keywords: convenience multimedia
;; Homepage: https://github.com/agzam/wiktionary-bro.el
;; Package-Requires: ((emacs "30.1") (request "0.3.3"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Lookup Wiktionary entries a bit more conveniently
;; It renders entries in Org-mode outline
;; and tables in ASCII
;;
;; Features:
;; - Look up words in Wiktionary with `wiktionary-bro-dwim'
;; - Navigate Wiktionary links by pressing RET (opens in wiktionary-bro buffer)
;; - External links (Wikipedia, etc.) open in browser
;; - Multi-language support: customize `wiktionary-bro-language' or use C-c C-l
;; - Audio pronunciation playback
;; - Clean org-mode outline rendering with collapsible sections

;;; Code:

(require 'shr)
(require 'dom)
(require 'org)
(require 'org-element)
(require 'let-alist)
(require 'request)

(defgroup wiktionary-bro nil
  "Lookup Wiktionary entries."
  :prefix "wiktionary-bro-"
  :group 'applications)

(defcustom wiktionary-bro-audio-player "ffplay -nodisp -autoexit"
  "Command to play audio files. The URL will be appended."
  :type 'string
  :group 'wiktionary-bro)

(defcustom wiktionary-bro-language "en"
  "Default language code for Wiktionary (e.g., \"en\", \"fr\", \"de\").
This determines which language version of Wiktionary to use."
  :type 'string
  :group 'wiktionary-bro)

(defface wiktionary-bro-table-header
  '((t :inherit bold))
  "Face for table header cells."
  :group 'wiktionary-bro)

(defface wiktionary-bro-footnote-ref
  '((t :inherit font-lock-warning-face :height 0.8))
  "Face for footnote reference numbers in tables."
  :group 'wiktionary-bro)

(defface wiktionary-bro-audio-button
  '((t :inherit link :box (:line-width -1 :style released-button)))
  "Face for audio play buttons."
  :group 'wiktionary-bro)

(defun wiktionary-bro-play-audio (url)
  "Play audio from URL using `wiktionary-bro-audio-player'.'."
  (let ((full-url (if (string-prefix-p "//" url)
                      (concat "https:" url)
                    url)))
    (message "Playing audio: %s" full-url)
    (start-process-shell-command
     "wiktionary-audio" nil
     (format "%s %s" wiktionary-bro-audio-player (shell-quote-argument full-url)))))

(defvar-local wiktionary-bro-current-language nil
  "The language code for the current `wiktionary-bro' buffer.")

(defvar-local wiktionary-bro-current-word nil
  "The word being displayed in the current `wiktionary-bro' buffer.")

(defvar-local wiktionary-bro-available-languages nil
  "Alist of available languages for the current entry.
Each element is (LANG-CODE . LANG-NAME).")

(defvar wiktionary-bro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'wiktionary-bro-change-language)
    map)
  "Keymap for `wiktionary-bro-mode'.")

(define-derived-mode wiktionary-bro-mode
  org-mode "Wiktionary"
  "Major mode for browsing Wiktionary entries."
  :group 'wiktionary-bro
  (add-hook 'org-open-at-point-functions #'wiktionary-bro--handle-link nil t)
  (setq-local browse-url-browser-function #'eww-browse-url))

(defun wiktionary-bro--at-the-beginning-of-word-p (word-point)
  "Predicate to check whether WORD-POINT points to the beginning of the word."
  (save-excursion
    (backward-word)
    (forward-word)
    (< (point) word-point)))

(defun wiktionary-bro--get-original-word (beginning end)
  "Get a word to look for from the user.
BEGINNING and END correspond to the selected text (if selected).
If presented, the selected text will be used.
Otherwise, user must provide additional information."
  (if (use-region-p)
      (buffer-substring-no-properties beginning end)
    (read-string "Wiktionary look up: ")))

(defun wiktionary-bro--dom-texts (dom)
  "Recursively extract all text from DOM node, skipping style/script tags.
Handles <br> tags by inserting newlines and <sup> with parentheses."
  (cond
   ((null dom) "")
   ((stringp dom) dom)
   ((consp dom)
    (let ((tag (car-safe dom)))
      (cond
       ((memq tag '(style script)) "")
       ((eq tag 'br) "\n")
       ((eq tag 'sup)
        (let ((inner (mapconcat #'wiktionary-bro--dom-texts (cddr dom) "")))
          (if (string-empty-p inner) ""
            (concat "(" inner ")"))))
       (t (mapconcat #'wiktionary-bro--dom-texts (cddr dom) "")))))
   (t "")))

(defun wiktionary-bro--superscript-number (n)
  "Convert number N to Unicode superscript string."
  (let ((superscripts ["⁰" "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹"]))
    (mapconcat (lambda (c)
                 (aref superscripts (- c ?0)))
               (number-to-string n) "")))

(defun wiktionary-bro--cell-content-to-string (cell)
  "Extract text content from a table CELL dom element."
  (let ((text (wiktionary-bro--dom-texts cell)))
    ;; Normalize whitespace but preserve intentional line breaks as " / "
    (thread-last text
      (replace-regexp-in-string "[ \t]+" " ")
      (replace-regexp-in-string " *\n+ *" " / ")
      (replace-regexp-in-string "^ */ *" "")
      (replace-regexp-in-string " */ *$" "")
      ;; Fix double parentheses from nested <sup> tags
      (replace-regexp-in-string "((\\([^)]+\\)))" "(\\1)")
      ;; Convert numeric footnotes to superscript: (1) → ¹
      (replace-regexp-in-string
       "(\\([0-9]+\\))"
       (lambda (m)
         (wiktionary-bro--superscript-number
          (string-to-number (match-string 1 m)))))
      (string-trim))))

(defun wiktionary-bro--table-has-audio-p (table)
  "Check if TABLE contain audio elements."
  (or
   ;; Check class attribute for audiotable
   (let ((class (alist-get 'class (cadr table))))
     (and class (string-match-p "audiotable" class)))
   ;; Or check for audio elements inside
   (dom-by-tag table 'audio)))

(defun wiktionary-bro--table-is-layout-p (table)
  "Check if TABLE is a layout table (not a real data table).
Layout tables typically have a single cell containing lists or other
block elements."
  (let* ((tbody (or (alist-get 'tbody (cdr table))
                    (cdr table)))
         (rows (seq-filter (lambda (x) (eq (car-safe x) 'tr)) tbody))
         (cell-p (lambda (cell)
                   (or (eq (car-safe cell) 'th)
                       (eq (car-safe cell) 'td)))))
    (or
     ;; Audio tables should be rendered by shr (to get audio buttons)
     (wiktionary-bro--table-has-audio-p table)
     ;; Single row with lists
     (and
      (= (length rows) 1)
      (let ((cells (seq-filter cell-p (car rows))))
        (seq-some (lambda (cell)
                    (seq-some (lambda (el)
                                (memq (car-safe el) '(ul ol style)))
                              (cddr cell)))
                  cells))))))

(defun wiktionary-bro--parse-table-to-grid (table)
  "Parse HTML TABLE dom into a 2D grid handling colspan/rowspan.
Returns (grid spans headers num-rows num-cols) where:
- grid is a hash-table with (row . col) keys containing cell content
- spans is a hash-table with (row . col) keys containing (rowspan . colspan)
- headers is a hash-table marking which cells are th (headers)"
  (let* ((tbody (or (alist-get 'tbody (cdr table))
                    (cdr table)))
         (rows (seq-filter (lambda (x) (eq (car-safe x) 'tr)) tbody))
         (cell-p (lambda (cell)
                   (or (eq (car-safe cell) 'th)
                       (eq (car-safe cell) 'td))))
         (grid (make-hash-table :test 'equal))
         (spans (make-hash-table :test 'equal))
         (headers (make-hash-table :test 'equal))
         (occupied (make-hash-table :test 'equal))
         (num-rows (length rows))
         (max-col 0)
         (row-idx 0))

    (dolist (row rows)
      (let ((col-idx 0)
            (cells (seq-filter cell-p row)))
        (dolist (cell cells)
          ;; Skip cells occupied by rowspan from above
          (while (gethash (cons row-idx col-idx) occupied)
            (cl-incf col-idx))
          (let* ((tag (car cell))
                 (attrs (nth 1 cell))
                 (colspan (max 1 (string-to-number (or (alist-get 'colspan attrs) "1"))))
                 (rowspan (max 1 (string-to-number (or (alist-get 'rowspan attrs) "1"))))
                 (content (wiktionary-bro--cell-content-to-string cell)))
            ;; Store content and span info at the origin cell
            (puthash (cons row-idx col-idx) content grid)
            (puthash (cons row-idx col-idx) (cons rowspan colspan) spans)
            ;; Mark if this is a header cell
            (when (eq tag 'th)
              (puthash (cons row-idx col-idx) t headers))
            ;; Mark all spanned cells as occupied
            (dotimes (dr rowspan)
              (dotimes (dc colspan)
                (puthash (cons (+ row-idx dr) (+ col-idx dc)) t occupied)))
            (setq max-col (max max-col (+ col-idx colspan)))
            (cl-incf col-idx colspan))))
      (cl-incf row-idx))

    (list grid spans headers num-rows max-col)))

(defun wiktionary-bro--table-to-ascii (table)
  "Renders shr-dom representation of a TABLE in ascii.
Creates a text representation with faces for headers and footnotes."
  (unless (and (consp table)
               (or (alist-get 'tbody (cdr table))
                   (seq-find (lambda (x) (eq (car-safe x) 'tr)) (cdr table))))
    (user-error "Parameter must be a list representing a table"))

  (let* ((parsed (wiktionary-bro--parse-table-to-grid table))
         (grid (nth 0 parsed))
         (spans (nth 1 parsed))
         (headers (nth 2 parsed))
         (num-rows (nth 3 parsed))
         (num-cols (nth 4 parsed))
         (col-widths (make-vector num-cols 5)))

    ;; Calculate column widths based on content
    (maphash (lambda (pos content)
               (when (stringp content)
                 (let* ((col (cdr pos))
                        (span-info (gethash pos spans))
                        (colspan (if span-info (cdr span-info) 1)))
                   (when (= colspan 1)
                     (aset col-widths col
                           (max (aref col-widths col)
                                (min 32 (+ 2 (length content)))))))))
             grid)

    ;; Build ASCII table
    (with-temp-buffer
      (let ((skip-cells (make-hash-table :test 'equal))
            (rowspan-at (make-hash-table :test 'equal))  ; (row . col) -> (origin-row . rowspan)
            (separator-rows (make-hash-table :test 'equal)))

        ;; Pre-calculate skip cells, rowspan info, and separator rows
        (maphash (lambda (pos span-info)
                   (let ((row (car pos))
                         (col (cdr pos))
                         (rowspan (car span-info))
                         (colspan (cdr span-info))
                         (content (gethash pos grid)))
                     ;; Check if this is a separator row (spans all cols, empty content)
                     (when (and (= col 0)
                                (= colspan num-cols)
                                (string-empty-p (or content "")))
                       (puthash row t separator-rows))
                     ;; Track rowspan info and mark spanned cells
                     (dotimes (dr rowspan)
                       (dotimes (dc colspan)
                         (puthash (cons (+ row dr) (+ col dc))
                                  (cons row rowspan) rowspan-at)
                         (unless (and (= dr 0) (= dc 0))
                           (puthash (cons (+ row dr) (+ col dc)) t skip-cells))))))
                 spans)

        ;; Helper to create horizontal line respecting rowspans
        (let ((make-hline
               (lambda (after-row)
                 (let ((parts (list "+")))
                   (dotimes (col num-cols)
                     (let* ((rs-info (gethash (cons after-row col) rowspan-at))
                            (origin-row (car rs-info))
                            (rowspan (cdr rs-info))
                            ;; Does a rowspan continue past this row?
                            (continues (and rs-info
                                            (< after-row (+ origin-row rowspan -1)))))
                       (push (if continues
                                 (make-string (aref col-widths col) ? )
                               (make-string (aref col-widths col) ?-))
                             parts)
                       (push "+" parts)))
                   (apply #'concat (nreverse parts))))))

          ;; Top border - use proper format
          (insert "+" (mapconcat (lambda (w) (make-string w ?-)) col-widths "+") "+\n")

          ;; Render rows
          (dotimes (row num-rows)
            ;; Skip separator rows entirely
            (unless (gethash row separator-rows)
              ;; Render row content
              (insert "|")
              (let ((col 0))
                (while (< col num-cols)
                  ;; Get rowspan info for this position
                  (let* ((rs-info (gethash (cons row col) rowspan-at))
                         (origin-row (if rs-info (car rs-info) row))
                         (rowspan (if rs-info (cdr rs-info) 1))
                         ;; Get span info from the origin cell
                         (origin-span (gethash (cons origin-row col) spans))
                         (colspan (if origin-span (cdr origin-span) 1))
                         ;; Calculate which row within the span this is
                         (row-within-span (- row origin-row))
                         ;; Middle row of the span (for vertical centering)
                         (middle-offset (/ (1- rowspan) 2))
                         (is-middle-row (= row-within-span middle-offset))
                         ;; Get content from origin cell, show only on middle row
                         (content (if is-middle-row
                                      (or (gethash (cons origin-row col) grid) "")
                                    ""))
                         (is-header (gethash (cons origin-row col) headers))
                         ;; Total width for colspan
                         (total-width (+ (cl-loop for c from col below (+ col colspan)
                                                  sum (aref col-widths c))
                                         (1- colspan)))
                         (inner-width (- total-width 2))
                         (truncated (truncate-string-to-width content
                                                              (max 1 inner-width)))
                         ;; Apply faces to content
                         (styled-content
                          (if (string-empty-p truncated)
                              truncated
                            (if is-header
                                (propertize truncated 'face 'wiktionary-bro-table-header)
                              truncated)))
                         ;; Center the text horizontally
                         (padding-left (/ (- inner-width (length truncated)) 2))
                         (padding-right (- inner-width (length truncated) padding-left)))
                    (insert " "
                            (make-string padding-left ? )
                            styled-content
                            (make-string padding-right ? )
                            " |")
                    (cl-incf col colspan))))
              (insert "\n"))

            ;; Draw horizontal line after this row
            (let* ((next-is-sep (gethash (1+ row) separator-rows))
                   (curr-is-sep (gethash row separator-rows))
                   (is-last-row (= row (1- num-rows)))
                   ;; Check if any rowspan continues past this row
                   (any-continues nil))
              (dotimes (col num-cols)
                (let* ((rs-info (gethash (cons row col) rowspan-at))
                       (origin-row (car rs-info))
                       (rowspan (cdr rs-info)))
                  (when (and rs-info (< row (+ origin-row rowspan -1)))
                    (setq any-continues t))))
              ;; Draw line unless we're inside a rowspan for all columns
              (when (or (not any-continues)
                        curr-is-sep
                        next-is-sep
                        is-last-row)
                ;; For last row, always draw full border
                (if is-last-row
                    (insert "+" (mapconcat (lambda (w) (make-string w ?-)) col-widths "+") "+\n")
                  (insert (funcall make-hline row) "\n"))))))

      (buffer-substring (point-min) (point-max))))))

(defun wiktionary-bro--shr-insert-doc (dom)
  "Overrides shr-insert-doc of DOM for cleaner content."
  (cl-letf*
      ((;; remove images
        (symbol-function 'shr-tag-img) #'ignore)

       ;; remove edit buttons
       (shr-tag-span* (symbol-function 'shr-tag-span))
       ((symbol-function 'shr-tag-span)
        (lambda (dom)
          (let ((href (alist-get
                       'href (car (alist-get 'a (cdr dom))))))
            (unless (and href (string-match-p "action=edit" href))
              (funcall shr-tag-span* dom)))))

       ;; transform urls to org-mode format
       ((symbol-function 'shr-tag-a)
        (lambda (dom)
          (let-alist (cadr dom)
            (when-let ((href .href))
              (let ((desc (cond
                           ;; Direct string content
                           ((stringp (caddr dom))
                            (string-trim (caddr dom)))
                           ;; Nested element with string content
                           ((and (consp (caddr dom))
                                 (stringp (caddr (caddr dom))))
                            (string-trim (caddr (caddr dom))))
                           ;; No description, use all text content or href as fallback
                           (t (let ((text (wiktionary-bro--dom-texts dom)))
                                (if (string-empty-p text) href (string-trim text)))))))
                (when (and desc (not (string-empty-p desc)))
                  (insert (format "[[%s][%s]]" href desc))))))))

       (shr-tag-div* (symbol-function 'shr-tag-div))
       ((symbol-function 'shr-tag-div)
        (lambda (dom)
          ;; remove TOC
          (let ((id (alist-get 'id (cadr dom))))
            (unless (and id (string= id "toc") )
              (funcall shr-tag-div* dom)))))

       ;; list items should be prefixed with dash (not asterisk)
       (shr-tag-li* (symbol-function 'shr-tag-li))
       ((symbol-function 'shr-tag-li)
        (lambda (dom)
          (cl-letf (((symbol-function 'shr-mark-fill)
                     #'ignore))
            (let ((shr-internal-bullet '("- " . 2)))
              (funcall shr-tag-li* dom)))))

       ((symbol-function 'shr-tag-table)
        (lambda (dom)
          (if (wiktionary-bro--table-is-layout-p dom)
              ;; Layout table - let shr render contents normally
              (shr-generic dom)
            ;; Real data table - render as ASCII
            (ignore-errors
              (insert "\n" (wiktionary-bro--table-to-ascii dom) "\n")))))

       ;; Handle audio elements - create playable button
       ((symbol-function 'shr-tag-audio)
        (lambda (dom)
          ;; Find the first source URL (prefer ogg/mp3)
          (let* ((sources (dom-by-tag dom 'source))
                 (url (cl-loop for src in sources
                               for src-url = (dom-attr src 'src)
                               when src-url return src-url)))
            (when url
              (insert " ")
              (insert-text-button
               "▶ Play"
               'face 'wiktionary-bro-audio-button
               'audio-url url
               'action (lambda (btn)
                         (wiktionary-bro-play-audio (button-get btn 'audio-url)))
               'follow-link t
               'help-echo (format "Play audio: %s" url))
              (insert " ")))))

       (shr-tag-a* (symbol-function 'shr-tag-a))
       ((symbol-function 'shr-tag-a)
        (lambda (dom)
          ;; fix internal links
          ;; otherwise they won't be navigable
          (let* ((href (alist-get 'href (cadr dom)))
                 (lang (or wiktionary-bro-current-language wiktionary-bro-language "en")))
            (when href
              (unless (string-match-p "https://" href)
                (setf
                 (alist-get 'href (cadr dom))
                 (concat "https://" lang ".wiktionary.org" href))))
            (funcall shr-tag-a* dom))))

       ((symbol-function 'shr-tag-h1)
        (lambda (dom)
          (insert (propertize "* " 'invisible t))
          (shr-fontize-dom dom 'shr-h1)
          (insert "\n")))
       ((symbol-function 'shr-tag-h2)
        (lambda (dom)
          (insert (propertize "* " 'invisible t))
          (shr-fontize-dom dom 'shr-h2)
          (insert "\n")))
       ((symbol-function 'shr-tag-h3)
        (lambda (dom)
          (insert (propertize "** " 'invisible t))
          (shr-fontize-dom dom 'shr-h3)
          (insert "\n")))
       ((symbol-function 'shr-tag-h4) (lambda (dom)
                                        (insert (propertize "*** " 'invisible t))
                                        (shr-fontize-dom dom 'shr-h4)
                                        (insert "\n")))
       ((symbol-function 'shr-tag-h5)
        (lambda (dom)
          (insert (propertize "**** " 'invisible t))
          (shr-fontize-dom dom 'shr-h5)
          (insert "\n")))
       ((symbol-function 'shr-tag-h6)
        (lambda (dom)
          (insert (propertize "***** " 'invisible t))
          (shr-fontize-dom dom 'shr-h6)
          (insert "\n")))

       ((symbol-function 'shr-tag-p)
        (lambda (dom)
          (cl-letf (((symbol-function 'shr-mark-fill)
                     (lambda (_) nil)))
            (shr-generic dom)
            (insert "\n")))))
    (shr-insert-document dom)))

(defun wiktionary-bro--extract-word-from-url (url)
  "Extract the word/page name from a Wiktionary URL."
  (when (string-match "/wiki/\\([^#?]+\\)" url)
    (decode-coding-string (url-unhex-string (match-string 1 url)) 'utf-8)))

(defun wiktionary-bro--wiktionary-url-p (url)
  "Check if URL is a Wiktionary URL."
  (and url (string-match-p "wiktionary\\.org" url)))

(defun wiktionary-bro--wikipedia-url-p (url)
  "Check if URL is a Wikipedia URL."
  (and url (string-match-p "wikipedia\\.org" url)))

(defun wiktionary-bro--handle-link ()
  "Handle link at point in `wiktionary-bro' buffers.
Returns t if handled, nil otherwise."
  (when (derived-mode-p 'wiktionary-bro-mode)
    (let* ((ctx (org-element-context))
           (type (org-element-type ctx)))
      (when (eq type 'link)
        (let ((url (org-element-property :raw-link ctx)))
          (cond
           ;; Handle wiktionary links
           ((wiktionary-bro--wiktionary-url-p url)
            (when-let* ((word (wiktionary-bro--extract-word-from-url url)))
              (wiktionary-bro-lookup word wiktionary-bro-current-language)
              t))
           ;; Handle wikipedia and other external links
           ((or (wiktionary-bro--wikipedia-url-p url)
                (string-prefix-p "http" url))
            (browse-url url)
            t)
           ;; Let org-mode handle other links
           (t nil)))))))

(defun wiktionary-bro-change-language (lang)
  "Change the language of the current `wiktionary-bro' buffer and re-render.
LANG is the language code (e.g., \"en\", \"fr\", \"de\")."
  (interactive
   (list
    (if (and (derived-mode-p 'wiktionary-bro-mode)
             wiktionary-bro-available-languages)
        ;; Use completing-read with available languages
        (let* ((current (or wiktionary-bro-current-language wiktionary-bro-language "en"))
               (choices (mapcar (lambda (pair)
                                  (cons (format "%s (%s)" (cdr pair) (car pair))
                                        (car pair)))
                                wiktionary-bro-available-languages))
               (selection (completing-read
                          (format "Language (current: %s): " current)
                          choices
                          nil nil nil nil
                          ;; Default to current language
                          (car (rassoc current choices)))))
          (or (cdr (assoc selection choices)) selection))
      ;; Fallback to simple string input
      (read-string
       (format "Language code (current: %s): "
               (or wiktionary-bro-current-language wiktionary-bro-language "en"))
       nil nil
       (or wiktionary-bro-current-language wiktionary-bro-language "en")))))
  (unless (derived-mode-p 'wiktionary-bro-mode)
    (user-error "Not in a wiktionary-bro buffer"))
  (unless wiktionary-bro-current-word
    (user-error "No word associated with this buffer"))
  (wiktionary-bro-lookup wiktionary-bro-current-word lang))

(defun wiktionary-bro--render (url title html-text word lang available-langs)
  "Render HTML-TEXT of a Wiktionary entry.
URL is the entry URL, TITLE is the page title, WORD is the lookup word,
LANG is the language code, and AVAILABLE-LANGS is an alist of (code . name)."
  (let* ((parsed (with-temp-buffer
                   (insert html-text)
                   (libxml-parse-html-region
                    (point-min)
                    (point-max))))
         (buffer-name (format "*wiktionary: %s*" title))
         (buffer (generate-new-buffer buffer-name))
         (same-win-p (derived-mode-p 'wiktionary-bro-mode)))
    (with-current-buffer buffer
      (insert url)
      (insert "\n\n")
      (wiktionary-bro--shr-insert-doc parsed)
      (wiktionary-bro-mode)
      (setq-local wiktionary-bro-current-word word)
      (setq-local wiktionary-bro-current-language lang)
      (setq-local wiktionary-bro-available-languages available-langs)
      (setq-local truncate-lines t)  ; prevent line wrapping for tables
      (setq-local org-hide-emphasis-markers t)
      (read-only-mode)
      (goto-char (point-min))
      (if same-win-p
          (pop-to-buffer-same-window buffer)
        (pop-to-buffer buffer)))))

(defun wiktionary-bro-lookup (word &optional lang)
  "Look up WORD in Wiktionary using language LANG.
Defaults to `wiktionary-bro-language'."
  (let* ((lang (or lang wiktionary-bro-current-language wiktionary-bro-language "en"))
         (encoded-word (if (string-match-p "%" word)
                           word  ; Already encoded
                         (url-hexify-string word)))
         (url (format
               "https://%s.wiktionary.org/w/api.php?action=parse&format=json&page=%s"
               lang encoded-word)))
    (request url
      :parser #'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let-alist data
           (if .error
               (message .error.info)
             (let* ((wiki-url (format "https://%s.wiktionary.org/wiki/%s"
                                      lang encoded-word))
                    ;; Extract available languages from langlinks
                    (available-langs
                     (when .parse.langlinks
                       (cons (cons lang (upcase lang))  ; Include current language
                             (mapcar (lambda (link)
                                       (let-alist link
                                         (cons .lang .*)))
                                     (append .parse.langlinks nil))))))
               (wiktionary-bro--render
                wiki-url
                .parse.title
                .parse.text.*
                word
                lang
                available-langs)))))))))

(defun wiktionary-bro (&optional beginning end)
  "Look up a Wiktionary entry.
BEGINNING and END correspond to the selected text with a word
to look up. If there is no selection provided, additional input
will be required."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (let ((word (wiktionary-bro--get-original-word beginning end)))
    (wiktionary-bro-lookup word)))

(defun wiktionary-bro-at-point (word-point)
  "Look up a Wiktionary entry for WORD-POINT."
  (interactive (list (point)))
  (save-mark-and-excursion
    (unless (wiktionary-bro--at-the-beginning-of-word-p word-point)
      (backward-word))
    (set-mark (point))
    (forward-word)
    (activate-mark)
    (wiktionary-bro (region-beginning) (region-end))))

(defun wiktionary-bro-dwim ()
  "Look up a Wiktionary entry.
Dispatches proper fn depending of region selection or
`thing-at-point', or prompts for a word."
  (interactive)
  (if (use-region-p)
      (wiktionary-bro
       (region-beginning)
       (region-end))
    (if (thing-at-point 'word)
        (wiktionary-bro-at-point (point))
      (wiktionary-bro))))

(provide 'wiktionary-bro)
;;; wiktionary-bro.el ends here
