;;; tailwind-minor-mode.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shadr

;; Author: shadr <shadr.nn@gmail.com>
;; Created: 2025-09-16
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar tailwind-minor-mode--class-list)

(defvar tailwind-minor-mode--class-kind-map)

(defun tailwind-minor-mode--class-file-name ()
  (expand-file-name "tailwind_keywords.txt" (file-name-directory (or load-file-name ""))))

(defun tailwind-minor-mode-reload-classes()
  (interactive)
  (let ((data (tailwind-minor-mode--read-class-data (tailwind-minor-mode--class-file-name))))
    (setq tailwind-minor-mode--class-list (car data))
    (setq tailwind-minor-mode--class-kind-map (car (cdr data)))))

(defvar tailwind-minor-mode--kind-map
  '(text text method function constructor field variable
    class interface module property unit value enum keyword
    snippet color file reference folder enummember constant
    struct event operator typeparameter))

(defun tailwind-minor-mode--read-class-data (filename)
  "Read FILENAME and return a list (class-list class-kind-map).
CLASS-LIST is a list of class names in order of appearance.
CLASS-KIND-MAP is a hash table mapping class names to their kinds."
  (let ((class-list '())
        (class-kind-map (make-hash-table :test 'equal :size 20000)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (string-trim (buffer-substring (line-beginning-position) (line-end-position))))
               (tokens (split-string line)))
          (when (>= (length tokens) 2)
            (let ((class-name (car tokens))
                  (kind (mapconcat 'identity (cdr tokens) " ")))
              (push class-name class-list)
              (puthash class-name kind class-kind-map))))
        (forward-line)))
    (list (nreverse class-list) class-kind-map)))

(defun tailwind-minor-mode--point-in-class-p ()
  "Return non-nil if point is inside quotes of a class attribute."
  (let ((state (syntax-ppss)))
    (when (nth 3 state)  ; Inside a string
      (let ((string-start (nth 8 state)))
        (save-excursion
          (goto-char string-start)
          (backward-char)  ; Move before the opening quote
          (skip-chars-backward " \t\n\r")  ; Skip whitespace
          (when (eq (char-after) ?=)  ; Check for equals sign
            (skip-chars-backward " \t\n\r")  ; Skip whitespace
            (let ((end (point)))
              (skip-chars-backward "a-zA-Z")  ; Move to start of attribute name
              (string= (downcase (buffer-substring-no-properties (point) end)) "class"))))))))

(defun tailwind-minor-mode--class-kind (class)
  "Get completion kind for a give class name"
  (let ((kind (gethash class tailwind-minor-mode--class-kind-map)))
    (if kind (nth (string-to-number kind) tailwind-minor-mode--kind-map) 'text)))

(defun tailwind-minor-mode-completion-function ()
  "Capf for tailwind classes when point inside class attribute"
  (when (tailwind-minor-mode--point-in-class-p)
    (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                      (cons (point) (point)))))
      (list (car bounds)
            (cdr bounds)
            tailwind-minor-mode--class-list
            :annotation-function (lambda (_) " tailwind class")
            :category 'text
            :company-kind #'tailwind-minor-mode--class-kind))))

(define-minor-mode tailwind-minor-mode
  "Minor mode that provides tailwind classes completion"
  :lighter "yes"
  (if tailwind-minor-mode
      (add-to-list 'completion-at-point-functions #'tailwind-minor-mode-completion-function)
    (setq completion-at-point-functions (remove #'tailwind-minor-mode-completion-function completion-at-point-functions))))

(tailwind-minor-mode-reload-classes)

(provide 'tailwind-minor-mode)
