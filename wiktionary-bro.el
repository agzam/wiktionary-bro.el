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
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Description
;;
;; Browse Wiktionary entries a bit more conveniently
;;
;;; Commentary:
;;
;;; Code:

(require 'request)
(require 'shr)
(require 'let-alist)

(defgroup wiktionary-bro nil
  "Lookup Wiktionary entries"
  :prefix "wiktionary-bro-"
  :group 'applications)

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

(defun wiktionary-bro--render (html-text)
  "Renders HTML-TEXT of a wiktioary entry"
  (with-temp-buffer
    (insert html-text)
    (cl-letf* ((shr-tag-span* (symbol-function 'shr-tag-span))
               ((symbol-function 'shr-tag-span)
                (lambda (dom)
                  (let ((href (alist-get
                               'href (car (alist-get 'a (cdr dom))))))
                    (unless (and href
                                 (string-match-p "action=edit" href))
                      (funcall shr-tag-span* dom)))))

               (shr-tag-div* (symbol-function 'shr-tag-div))
               ((symbol-function 'shr-tag-div)
                (lambda (dom)
                  (let ((id (alist-get 'id (cadr dom))))
                    (unless (and id (string= id "toc") )
                      (funcall shr-tag-div* dom))))))
      (shr-render-buffer
       (current-buffer)))))

(defun wiktionary-bro (&optional beginning end)
  "Look up a Wiktionary entry
`BEGINNING' and `END' correspond to the selected text with a word
to look up. If there is no selection provided, additional input
will be required."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (let* ((word (wiktionary-bro--get-original-word beginning end))
         (url (format
               "https://en.wiktionary.org/w/api.php?action=parse&format=json&page=%s"
               word)))
    (request url
      :parser #'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let-alist data
           (if .error
               (message .error.info)
             (wiktionary-bro--render .parse.text.*))))))))

(defun wiktionary-bro--at-point (word-point)
  "Look up a Wiktionary entry for word at point."
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
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning)
                end (region-end))
          (wiktionary-bro beg end))
      (if (thing-at-point 'word)
          (wiktionary-bro--at-point (point))
        (wiktionary-bro)))))

(provide 'wiktionary-bro)

;;; wiktionary-bro.el ends here
