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

(require 'tailwind-cache)

(defvar tailwind-minor-mode--kind-map
  '(text text method function constructor field variable
    class interface module property unit value enum keyword
    snippet color file reference folder enummember constant
    struct event operator typeparameter))

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
  (benchmark-progn
    (let* ((completion-item (gethash class (tailwind-minor-mode--get-project-cached-completions)))
           (kind (plist-get completion-item :kind)))
      (if kind (nth kind tailwind-minor-mode--kind-map) 'text))))

(defun tailwind-minor-mode-completion-function ()
  "Capf for tailwind classes when point is inside class attribute"
  (when (tailwind-minor-mode--point-in-class-p)
    (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                      (cons (point) (point)))))
      (list (car bounds)
            (cdr bounds)
            (tailwind-minor-mode--get-project-cached-completions)
            :annotation-function (lambda (_) " tailwind class")
            :category 'text
            :company-kind #'tailwind-minor-mode--class-kind))))

(define-minor-mode tailwind-minor-mode
  "Minor mode that provides tailwind classes completion"
  :lighter "yes"
  (if tailwind-minor-mode
      (progn
        (add-to-list 'completion-at-point-functions #'tailwind-minor-mode-completion-function)
        (unless (plist-get  tailwind-minor-mode--cached-completions (project-root (project-current)) #'string-equal)
          (tailwind-minor-mode-cache-completions)))
    (setq completion-at-point-functions (remove #'tailwind-minor-mode-completion-function completion-at-point-functions))))

(provide 'tailwind-minor-mode)
