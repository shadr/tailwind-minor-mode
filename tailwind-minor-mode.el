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

(require 'treesit)

(defvar tailwind-minor-mode--class-list
  (with-temp-buffer
    (insert-file-contents (expand-file-name "tailwind_keywords.txt" (file-name-directory (or load-file-name ""))))
    (split-string (buffer-string) "\n" t)))

(defun tailwind-minor-mode--point-in-class-p ()
  "Return non-nil if point is inside quotes of a class attribute."
  (and (nth 3 (syntax-ppss))
       (save-excursion
           (re-search-backward "class\\s-*=\\s-*[\"']" nil t))))

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
            :company-kind (lambda (_) 'text)))))

(define-minor-mode tailwind-minor-mode
  "Minor mode that provides tailwind classes completion"
  :lighter "yes"
  (if tailwind-minor-mode
      (add-to-list 'completion-at-point-functions #'tailwind-minor-mode-completion-function)
    (setq completion-at-point-functions (remove #'tailwind-minor-mode-completion-function completion-at-point-functions))))


(provide 'tailwind-minor-mode)
