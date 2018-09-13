;;; lsp-java-classpath.el --- LSP Java classpath function  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;; LSP Java classpath displaying functions.

;;; Code:

(require 'lsp-common)
(require 'f)
(require 'tree-widget)

(defcustom lsp-java-themes-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons")
  "Directory containing themes."
  :type 'directory
  :group 'lsp-java)

(defcustom lsp-java-theme "vscode"
  "Theme to use."
  :type 'string
  :group 'lsp-java)

(define-minor-mode lsp-java-classpath-mode
  "Minor mode for browsing classpath."
  :init-value nil
  :group lsp-java
  (setq buffer-read-only t))

(define-widget 'lsp-java-widget-guide 'item
  "Vertical guide line."
  :tag       " "
  :format    "%t")

(define-widget 'lsp-java-widget-end-guide 'item
  "End of a vertical guide line."
  :tag       " "
  :format    "%t")

(define-widget 'lsp-java-widget-handle 'item
  "Horizontal guide line that joins a vertical guide line to a node."
  :tag       " "
  :format    "%t")

(defmacro lsp-java-define-widget (name &optional image-open image-closed image-empty)
  "Helper for defining widget icons."
  (let ((open-icon (make-symbol (format "lsp-java-%s-open" name)))
        (close-icon (make-symbol (format "lsp-java-%s-close" name)))
        (empty-icon (make-symbol (format "lsp-java-%s-empty" name)))
        (leaf-icon (make-symbol (format "lsp-java-%s-leaf" name))))
    `(progn
       (define-widget (quote ,open-icon) 'tree-widget-icon
         "Icon for a open tree-widget node."
         :tag        "[+]"
         :glyph-name ,(or image-open name))
       (define-widget (quote ,close-icon) 'tree-widget-icon
         "Icon for a closed tree-widget node."
         :tag        "[-]"
         :glyph-name ,(or image-closed image-open name))
       (define-widget (quote ,empty-icon) 'tree-widget-icon
         "Icon for a closed tree-widget node."
         :tag        "[.]"
         :glyph-name ,(or image-empty image-open name))
       (list :open-icon (quote ,open-icon)
             :close-icon (quote ,close-icon)
             :empty-icon (quote ,empty-icon)
             :leaf-icon (quote ,leaf-icon)
             :handle 'lsp-java-widget-handle
             :end-guide 'lsp-java-widget-end-guide
             :guide 'lsp-java-widget-guide))))

(defvar lsp-java-icons-file-type-jar (dap-ui-define-widget "file_type_jar"))
(defvar lsp-java-icons-file-type_class (dap-ui-define-widget "file_type_class"))
(defvar lsp-java-icons-folder-type-component (dap-ui-define-widget "folder_type_component"
                                                                   "folder_type_component_opened"
                                                                   "folder_type_component"))
(defvar lsp-java-icons-default-folder (dap-ui-define-widget "default_folder"
                                                            "default_folder_opened"
                                                            "default_folder"))
(defvar lsp-java-icons-folder-type-library (dap-ui-define-widget "folder_type_library"
                                                                 "folder_type_library_opened"
                                                                 "folder_type_library"))
(defvar lsp-java-icons-default-root-folder (dap-ui-define-widget "default_root_folder"
                                                                 "default_root_folder_opened"
                                                                 "default_root_folder"))
(defvar lsp-java-icons-folder-type-maven (dap-ui-define-widget "folder_type_maven"
                                                               "folder_type_maven_opened"
                                                               "folder_type_maven"))

(defun lsp-java-classpath--get-icon (kind)
  "Get the icon corresponding to KIND."
  (pcase kind
    (1 lsp-java-icons-file-type-jar)
    (2 lsp-java-icons-default-root-folder)
    (3 lsp-java-icons-default-folder)
    (5 lsp-java-icons-folder-type-library)))

(defun lsp-java-classpath--get-node (path kind project-uri)
  "Get the icon corresponding to KIND."
  (pcase kind
    (1 `(push-button :format "%[%t%]\n"
                     :tag ,(-> path lsp--path-to-uri f-filename)))
    (2 `(push-button :format "%[%t%]\n"
                     :tag ,(-> path lsp--path-to-uri f-filename)))
    (3 `(push-button :format "%[%t%]\n"
                     :tag ,(f-relative (lsp--uri-to-path path) (lsp--uri-to-path project-uri))))
    (5 `(push-button :format "%[%t%]\n"
                     :tag ,path))))

(defun lsp-java-classpath--render-classpath (classpath-entry project-uri)
  (-let* (((&hash "path" "children" "entryKind" kind) classpath-entry)
          (icons (lsp-java-classpath--get-icon kind))
          (node (lsp-java-classpath--get-node path kind project-uri)))
    `(tree-widget
      :node ,node
      :open t
      ,@icons
      ,@(--map (lsp-java-classpath--render-classpath it project-uri) children))))

(defun lsp-java-classpath-browse ()
  "Show currently active sessions."
  (interactive)
  (lsp--cur-workspace-check)
  (let ((inhibit-read-only t)
        (workspace lsp--cur-workspace)
        (project-uri (cadddr  (cdr project-uris)))
        (buf (get-buffer-create "*classpath*")))
    (with-current-buffer buf
      (erase-buffer)
      (setq-local lsp--cur-workspace workspace)
      (setq-local tree-widget-themes-directory lsp-java-themes-directory)
      (tree-widget-set-theme lsp-java-theme)

      (widget-create
       `(tree-widget
         :node (push-button :format "%[%t%]\n"
                            :tag ,(f-filename (lsp--uri-to-path project-uri)))
         :open t
         ,@lsp-java-icons-default-root-folder
         ,@(--map (lsp-java-classpath--render-classpath it project-uri)
                  (lsp-send-execute-command "che.jdt.ls.extension.classpathTree" project-uri))))
      (lsp-java-classpath-mode t))
    (dap-ui--show-buffer buf)))

(provide 'lsp-java-classpath)
;;; lsp-java-classpath.el ends here
