;;; copy-candidates.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 shadr
;;
;; Author: shadr
;; Version: 0.0.1
;; Homepage: https://github.com/shadr/tailwind-minor-mode
;;
;; This file is not part of GNU Emacs.
;;
;;  Description
;;
;; This file is not meant to be used by everyone, it is for
;; internal use by maintainers of the tailwind-minor-mode
;;
;;  This is a function that copies candidates from a corfu to dedicated buffer.
;;  To get tailwind classes:
;;  1) enable lsp-tailwindcss to connect to the tailwindcss-lsp
;;  2) place point inside empty double quotes: class=""
;;  3) trigger `completion-at-point'
;;  4) press `M-k' to extract candidates into a dedicated buffer
;;
;;; Code:

(defun tailwind-minor-mode--corfu-to-candidates-buffer ()
  "Copy all current Corfu candidates to the kill ring and a buffer."
  (interactive)
  (when (and (boundp 'corfu--candidates) corfu--candidates)
    (let ((candidates (mapconcat #'identity corfu--candidates "\n")))
      (kill-new candidates)
      (with-current-buffer (get-buffer-create "*Corfu Candidates*")
        (erase-buffer)
        (insert candidates)
        (pop-to-buffer (current-buffer)))
      (message "Copied %d candidates to *Corfu Candidates* buffer" (length corfu--candidates)))))

(map! :map corfu-map :i "M-k" #'tailwind-minor-mode--corfu-to-candidates-buffer)
