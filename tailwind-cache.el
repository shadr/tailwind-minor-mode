;;; tailwind-cache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shadr

;; Author: shadr <shadr.nn@gmail.com>
;; Created: 2025-11-09
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;;
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

(require 'jsonrpc)

(defvar tailwind-minor-mode--cached-completions nil
  "A map that stores cached completion items for each project.")

(defun tailwind-minor-mode--cache-make-connection ()
  (jsonrpc-process-connection
   :name "Tailwindcss Caching Client"
   :request-dispatcher #'tailwind-minor-mode--cache-on-request
   :process (make-process
             :name "tailwindcss-language-server"
             :command '("/home/shadr/.bun/bin/tailwindcss-language-server" "--stdio")
             :connection-type 'pipe
             :noquery t
             :stderr (get-buffer-create "*tailwind-lsp-stderr*"))))

(defun tailwind-minor-mode--cache-on-request (_ method params)
  (when (string-equal "workspace/configuration" method)
    (if-let* ((items (plist-get params :items))
              (section (plist-get (elt items 0) :section)))
        (progn
          (when (string-equal "editor" section)
            '([nil]))
          (when (string-equal "tailwindCSS" section)
            '(:emmetCompletions t
              :showPixelEquivalents t
              :rootFontSize 16
              :validate t
              :hovers nil
              :suggestions t
              :codeActions nil
              :lint (:invalidScreen "error"
                     :invalidVariant "error"
                     :invalidTailwindDirective "error"
                     :invalidApply "error"
                     :invalidConfigPath "error"
                     :cssConflict "warning"
                     :recommendedVariantOrder "warning")
              :experimental (:classRegex ["class: \"(.*)\""])
              :classAttributes ["class" "className" "ngClass" "class:list"]))))))

(defun tailwind-minor-mode--get-completions (root)
  (let ((tailwind-cache--connection (tailwind-minor-mode--cache-make-connection)))
    (jsonrpc-request tailwind-cache--connection 'initialize `((processId) (rootPath . ,root) (rootUri . ,root) (capabilities (general (positionEncodings . ["utf-32" "utf-16"])) (workspace (workspaceEdit (documentChanges . t) (resourceOperations . ["create" "rename" "delete"])) (applyEdit . t) (symbol (symbolKind (valueSet . [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26]))) (executeCommand (dynamicRegistration . :json-false)) (didChangeWatchedFiles (dynamicRegistration . t)) (workspaceFolders . t) (configuration . t) (inlayHint (refreshSupport . :json-false)) (diagnostics (refreshSupport . :json-false)) (fileOperations (didCreate . :json-false) (willCreate . :json-false) (didRename . t) (willRename . t) (didDelete . :json-false) (willDelete . :json-false))) (textDocument (declaration (dynamicRegistration . t) (linkSupport . t)) (definition (dynamicRegistration . t) (linkSupport . t)) (references (dynamicRegistration . t)) (implementation (dynamicRegistration . t) (linkSupport . t)) (typeDefinition (dynamicRegistration . t) (linkSupport . t)) (synchronization (willSave . t) (didSave . t) (willSaveWaitUntil . t)) (documentSymbol (symbolKind (valueSet . [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26])) (hierarchicalDocumentSymbolSupport . t)) (formatting (dynamicRegistration . t)) (rangeFormatting (dynamicRegistration . t)) (onTypeFormatting (dynamicRegistration . t)) (rename (dynamicRegistration . t) (prepareSupport . t)) (codeAction (dynamicRegistration . t) (isPreferredSupport . t) (codeActionLiteralSupport (codeActionKind (valueSet . ["" "quickfix" "refactor" "refactor.extract" "refactor.inline" "refactor.rewrite" "source" "source.organizeImports"]))) (resolveSupport (properties . ["edit" "command"])) (dataSupport . t)) (completion (completionItem (snippetSupport . :json-false) (documentationFormat . ["markdown" "plaintext"]) (resolveAdditionalTextEditsSupport . t) (insertReplaceSupport . t) (deprecatedSupport . t) (resolveSupport (properties . ["documentation" "detail" "additionalTextEdits" "command"])) (insertTextModeSupport (valueSet . [1 2])) (labelDetailsSupport . t)) (contextSupport . t) (dynamicRegistration . t)) (signatureHelp (signatureInformation (parameterInformation (labelOffsetSupport . t)) (activeParameterSupport . t)) (dynamicRegistration . t)) (documentLink (dynamicRegistration . t) (tooltipSupport . t)) (hover (contentFormat . ["markdown" "plaintext"]) (dynamicRegistration . t)) (selectionRange (dynamicRegistration . t)) (callHierarchy (dynamicRegistration . :json-false)) (typeHierarchy (dynamicRegistration . t)) (publishDiagnostics (relatedInformation . t) (tagSupport (valueSet . [1 2])) (versionSupport . t)) (diagnostic (dynamicRegistration . :json-false) (relatedDocumentSupport . :json-false)) (linkedEditingRange (dynamicRegistration . t)) (inlineCompletion) (inlayHint (dynamicRegistration . :json-false))) (window (workDoneProgress . t) (showDocument (support . t)))) (initializationOptions (configuration (tailwindCSS (emmetCompletions . t) (showPixelEquivalents . t) (rootFontSize . 16) (validate . t) (hovers . :json-false) (suggestions . t) (codeActions . :json-false) (lint (invalidScreen . "error") (invalidVariant . "error") (invalidTailwindDirective . "error") (invalidApply . "error") (invalidConfigPath . "error") (cssConflict . "warning") (recommendedVariantOrder . "warning")) (experimental (classRegex . ["class: \"(.*)\""])) (classAttributes . ["class" "className" "ngClass" "class:list"])))) (workDoneToken . "1")))
    (jsonrpc-notify tailwind-cache--connection 'initialized '())
    (jsonrpc-notify tailwind-cache--connection 'textDocument/didOpen
                    `(:textDocument
                      (:uri ,(concat "file://" root "/.tailwind-cache.html")
                       :languageId "html"
                       :version 1
                       :text "<div class=\"\"></div>\n")))

    (let ((result (jsonrpc-request tailwind-cache--connection 'textDocument/completion
                                   `(:textDocument (:uri ,(concat "file://" root "/.tailwind-cache.html"))
                                     :position (:line 0 :character 12)
                                     :context (:triggerKind 1)))))
      (jsonrpc-shutdown tailwind-cache--connection)
      (plist-get result :items))))

(defun tailwind-minor-mode--completions-to-hash-table (sequence)
  (let ((hash-table (make-hash-table :test 'equal :size (length sequence))))
    (dotimes (i (length sequence) hash-table)
      (let ((plist (aref sequence i))
            (label (plist-get (aref sequence i) :label)))
        (when label
          (puthash label plist hash-table))))))

(defun tailwind-minor-mode--get-project-cached-completions ()
  (let ((root (project-root (project-current))))
    (plist-get tailwind-minor-mode--cached-completions root #'string-equal)))

(defun tailwind-minor-mode-cache-completions ()
  (interactive)
  (let ((root (project-root (project-current))))
    (setq tailwind-minor-mode--cached-completions
          (plist-put tailwind-minor-mode--cached-completions root
                     (tailwind-minor-mode--completions-to-hash-table (tailwind-minor-mode--get-completions root))))))

(provide 'tailwind-cache)
;;; tailwind-cache.el ends here
