;;; wiktionary-bro.el --- Lookup Wiktionary entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: December 24, 2022
;; Modified: December 24, 2022
;; Version: 0.0.1
;; Keywords: convenience multimedia
;; Homepage: https://github.com/agzam/wiktionary-bro.el
;; Package-Requires: ((emacs "28.1") (request "0.3.0") (org "9"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Description
;;
;; Lookup Wiktionary entries a bit more conveniently
;;
;;; Commentary:
;;
;;; Code:

(require 'request)
(require 'shr)
(require 'let-alist)
(require 'outline)
(require 'org-indent)

(defgroup wiktionary-bro nil
  "Lookup Wiktionary entries."
  :prefix "wiktionary-bro-"
  :group 'applications)

(defvar wiktionary-bro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "RET") #'wiktionary-bro-dwim)
    map)
  "Keymap for `wiktionary-bro-mode'.")

(define-derived-mode wiktionary-bro-mode
  org-mode "Wiktionary"
  "Major mode for browsing Wiktionary entries.")

(defun wiktionary-bro--at-the-beginning-of-word-p (word-point)
  "Predicate to check whether `WORD-POINT' points to the beginning of the word."
  (save-excursion
    ;; If we are at the beginning of a word
    ;; this will take us to the beginning of the previous word.
    ;; Otherwise, this will take us to the beginning of the current word.
    (backward-word)
    ;; This will take us to the end of the previous word or to the end
    ;; of the current word depending on whether we were at the beginning
    ;; of a word.
    (forward-word)
    ;; Compare our original position with wherever we're now to
    ;; separate those two cases
    (< (point) word-point)))

(defun wiktionary-bro--get-original-word (beginning end)
  "Get a word to look for from the user.
`BEGINNING' and `END' correspond to the selected text (if selected).
If presented, the selected text will be used.
Otherwise, user must provide additional information."
  (if (use-region-p)
      (buffer-substring-no-properties beginning end)
    (read-string "Wiktionary look up: ")))

(defun wiktionary-bro--table-to-ascii (table)
  "Renders shr-dom representation of a TABLE in ascii."
  (unless (and (consp table) (alist-get 'tbody (cdr table)))
    (user-error "Parameter must be a list representing a table"))

  (let* ((cell-p (lambda (cell)
                   (or (eq (car-safe cell) 'th)
                       (eq (car-safe cell) 'td))))
         (tbody (alist-get 'tbody (cdr table)))
         (rows (seq-filter (lambda (x) (eq (car-safe x) 'tr)) tbody))
         (cols-num (thread-last
                     tbody
                     (seq-filter (lambda (x) (eq (car-safe x) 'tr)))
                     (seq-map (lambda (row)
                                (length
                                 (seq-filter cell-p row))))
                     (seq-max)))
         (cells (thread-last
                  tbody
                  (seq-filter (lambda (x) (eq (car-safe x) 'tr)))
                  (seq-mapcat (lambda (row)
                                (seq-filter cell-p row))))))
    (with-temp-buffer
      (table-insert cols-num (length rows))
      (dolist (cell cells)
        (let* ((attrs (nth 1 cell))
               (content (seq-drop cell 2))
               (content (if (eq 1 (length content)) (car content)
                          (with-temp-buffer
                            (shr-tag-span
                             (append '(span nil) content))
                            (buffer-string))))
               (colspan (string-to-number (or (alist-get 'colspan attrs) "")))
               (rowspan (string-to-number (or (alist-get 'rowspan attrs) ""))))
          (dotimes (_ (1- colspan))
            (ignore-errors
              (table-span-cell 'right)))
          (dotimes (_ (1- rowspan))
            (ignore-errors
              (table-span-cell 'below)))
          (ignore-errors
            (table-insert-sequence
             content 1 1 1 'center)
            (table-forward-cell))))
      (buffer-substring-no-properties
       (point-min)
       (point-max)))))

(defun wiktionary-bro--shr-insert-doc (dom)
  "Overrides shr-insert-doc for cleaner content."
  (cl-letf*
      (((symbol-function 'shr-tag-img)
        ;; remove images
        #'ignore)

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
            (when-let ((desc (if (stringp (caddr dom))
                                 (string-trim (caddr dom))
                               (caddr (caddr dom))))
                       (href .href))
              (insert (format "[[%s][%s]]" href desc))))))

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

       (shr-tag-table* (symbol-function 'shr-tag-table))
       ((symbol-function 'shr-tag-table)
        (lambda (dom)
          nil
          ;; (insert (wiktionary-bro--table-to-ascii dom))
          ))

       (shr-tag-a* (symbol-function 'shr-tag-a))
       ((symbol-function 'shr-tag-a)
        (lambda (dom)
          ;; fix internal links
          ;; otherwise they won't be navigable
          (let ((href (alist-get 'href (cadr dom))))
            (unless (string-match-p "https://" href)
              (setf
               (alist-get 'href (cadr dom))
               (concat "https://wiktionary.org" href)))
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

(defun wiktionary-bro--render (url title html-text)
  "Render HTML-TEXT of a Wiktionary entry with URL and TITLE."
  (let* ((parsed (with-temp-buffer
                   (insert html-text)
                   (libxml-parse-html-region
                    (point-min)
                    (point-max))))
         (buffer (generate-new-buffer title))
         (same-win-p (eq major-mode 'wiktionary-bro-mode)))
    (with-current-buffer buffer
      (insert url)
      (insert "\n\n")
      (wiktionary-bro--shr-insert-doc parsed)
      (wiktionary-bro-mode)
      (read-only-mode)
      (goto-char (point-min))
      (if same-win-p
          (pop-to-buffer-same-window buffer)
        (pop-to-buffer buffer)))))

(defun wiktionary-bro (&optional beginning end)
  "Look up a Wiktionary entry.
`BEGINNING' and `END' correspond to the selected text with a word
to look up. If there is no selection provided, additional input
will be required."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (let* ((word (url-hexify-string (wiktionary-bro--get-original-word beginning end)))
         (url (format
               "https://en.wiktionary.org/w/api.php?action=parse&format=json&page=%s"
               word)))
    (request url
      :parser #'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         ;; (let ((b (generate-new-buffer "wiktionary")))
         ;;   (with-current-buffer b
         ;;     (insert (pp data))
         ;;     (switch-to-buffer b)))
         (let-alist data
           (if .error
               (message .error.info)
             (let ((wiki-url (ignore-errors
                               (thread-last
                                 (elt .parse.langlinks 0)
                                 (alist-get 'url)
                                 (replace-regexp-in-string
                                  "//[[:alpha:]]+." "//")))))
               (wiktionary-bro--render
                wiki-url
                .parse.title
                .parse.text.*)))))))))

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
thing-at-point, or prompts for a word."
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
