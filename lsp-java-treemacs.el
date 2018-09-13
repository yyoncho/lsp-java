;;; lsp-java-treemacs.el --- LSP Java treemacs integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: lsp

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

;; Provides integration between `lsp-java' and `treemacs' which aims to provide
;; ide like project explorer.

;;; Code:

(require 'ht)
(require 'lsp-mode)
(require 'treemacs-extensions)
(require 'dash)
(require 'dash-functional)

(defface lsp-java-treemacs-directory-face
  '((t :inherit  treemacs-directory-face))
  "Face used by treemacs for lsp-java directories and packages."
  :group 'lsp-java-treemacs)

(defface lsp-java-treemacs-file-face
  '((t :inherit  treemacs-file-face))
  "Face used by treemacs for files."
  :group 'lsp-java-treemacs)

(defun lsp-java-treemacs--set-project-uris (workspace project-uris)
  "Set WORKSPACE project uri list to PROJECT-URIS."
  (puthash "project-uris" project-uris (lsp--workspace-metadata workspace)))

(defun lsp-java-treemacs--update-project-uris (workspace)
  "Update WORKSPACE project uris."
  (with-lsp-workspace workspace
    (->> workspace
         lsp--workspace-workspace-folders
         (--map (lsp-send-execute-command "che.jdt.ls.extension.mavenProjects"
                                          (lsp--path-to-uri it)))
         -flatten
         (lsp-java-treemacs--set-project-uris workspace) )))

(defun lsp-java-treemacs--get-project-uris (workspace)
  "Get WORKSPACE maven projects."
  (gethash "project-uris" (lsp--workspace-metadata workspace)))

(defun lsp-java-treemacs--find-root (folder)
  "Return the workspace corresponding FOLDER."
  (->> lsp--workspaces
       ht-values
       -uniq
       (--first (-contains? (lsp-java-treemacs--get-project-uris it) folder))))

(defun lsp-java-treemacs--get-libraries (project-uri)
  "Get the list of buffers, grouped by their major mode.
PROJECT-URI is the project id of the active project."
  (when-lsp-workspace (lsp-java-treemacs--find-root project-uri)
    (lsp-send-execute-command "che.jdt.ls.extension.externalLibraries"
                              (list :projectUri project-uri))))

(defun lsp-java-treemacs--library-children (project-uri node-id)
  "Get the list of library children.
PROJECT-URI and NODE-ID are the details for the current node."
  (when-lsp-workspace (lsp-java-treemacs--find-root  project-uri)
    (lsp-send-execute-command "che.jdt.ls.extension.libraryChildren"
                              (list :projectUri project-uri
                                    :nodeId node-id))))

(defun lsp-java-treemacs--external-library-children (project-uri node-id path)
  "Get the list of external library children.
PROJECT-URI, NODE-ID and PATH are the details for the current node."
  (when-lsp-workspace (lsp-java-treemacs--find-root project-uri)
    (lsp-send-execute-command "che.jdt.ls.extension.externalLibrariesChildren"
                              (list :projectUri project-uri
                                    :nodeId node-id
                                    :nodePath path))))

(defun lsp-java-treemacs--open-file (_arg)
  "Open resource file."
  (when-lsp-workspace (lsp-java-treemacs--find-root (treemacs--prop-at-point :project-uri))
    (->> (treemacs--prop-at-point :uri)
         lsp--uri-to-path
         find-file)))

(treemacs-define-leaf-node lsp-file treemacs-icon-java
                           :ret-action 'lsp-java-treemacs--open-file)

(defun lsp-java-treemacs--icon-path (rel-path)
  "Get fullpath to `lsp-java-treemacs' icon.
REL-PATH rel path to the icon."
  (f-join (f-dirname (or load-file-name buffer-file-name)) rel-path))

(treemacs--setup-icon lsp-java-treemacs-jar
                      (lsp-java-treemacs--icon-path "icons/vscode/file_type_jar.png"))
(treemacs--setup-icon treemacs-lsp-java-package
                      (lsp-java-treemacs--icon-path "icons/vscode/folder_type_package.png"))
(treemacs--setup-icon lsp-java-treemacs-package-opened
                      (lsp-java-treemacs--icon-path "icons/vscode/folder_type_package_opened.png"))
(treemacs--setup-icon treemacs-lsp-java-jar-folder
                      (lsp-java-treemacs--icon-path "icons/vscode/folder_type_component.png"))
(treemacs--setup-icon treemacs-lsp-java-jar-folder-opened
                      (lsp-java-treemacs--icon-path "icons/vscode/folder_type_component_opened.png"))
(treemacs--setup-icon treemacs-lsp-java-library-folder
                      (lsp-java-treemacs--icon-path "icons/vscode/folder_type_library.png"))
(treemacs--setup-icon treemacs-lsp-java-library-folder-opened
                      (lsp-java-treemacs--icon-path "icons/vscode/folder_type_library_opened.png"))
(treemacs--setup-icon treemacs-lsp-java-class
                      (lsp-java-treemacs--icon-path "icons/vscode/file_type_class.png")
                      "class")

(defmacro treemacs--lsp-node-or-folder ()
  "Extract common code from nodes."
  (let ((more-properties '(:project-uri (button-get btn :project-uri)
                                        :path path
                                        :node-id (button-get btn :node-id)
                                        :uri uri)))
    `(-let (((&hash "name" "path" "entryType" "uri") item))
       (pcase entryType
         ("PACKAGE"
          (treemacs-render-node :icon treemacs-icon-package-closed
                                :label-form name
                                :state treemacs-package-closed-state
                                :key-form path
                                :face 'lsp-java-treemacs-directory-face
                                :more-properties ,more-properties))
         ("FOLDER"
          (treemacs-render-node :icon treemacs-icon-folder-closed
                                :label-form name
                                :state treemacs-folder-closed-state
                                :key-form path
                                :face 'lsp-java-treemacs-directory-face
                                :more-properties ,more-properties))
         ((or "CLASS_FILE" "FILE")
          (treemacs-render-node :icon (ht-get treemacs-icons-hash
                                              (-> name treemacs--file-extension downcase)
                                              treemacs-icon-fallback)
                                :label-form name
                                :state treemacs-lsp-file-state
                                :key-form uri
                                :face 'lsp-java-treemacs-file-face
                                :more-properties ,more-properties))))))

(treemacs-define-expandable-node package
  :icon-open lsp-java-treemacs-package-opened
  :icon-closed treemacs-lsp-java-package
  :query-function (lsp-java-treemacs--external-library-children
                   (button-get btn :project-uri)
                   (button-get btn :node-id)
                   (button-get btn :path))
  :render-action (treemacs--lsp-node-or-folder))

(treemacs-define-expandable-node folder
  :icon-open treemacs-lsp-java-jar-folder-opened
  :icon-closed treemacs-lsp-java-jar-folder
  :query-function (lsp-java-treemacs--external-library-children
                   (button-get btn :project-uri)
                   (button-get btn :node-id)
                   (button-get btn :path))
  :render-action (treemacs--lsp-node-or-folder))

(treemacs-define-expandable-node jar-file
  :icon-open lsp-java-treemacs-jar
  :icon-closed lsp-java-treemacs-jar
  :query-function (lsp-java-treemacs--library-children (button-get btn :project-uri)
                                                       (button-get btn :key))
  :render-action (treemacs--lsp-node-or-folder))

(treemacs-define-expandable-node external-library
  :icon-open treemacs-lsp-java-library-folder
  :icon-closed treemacs-lsp-java-library-folder-opened
  :query-function (-> btn
                      (button-get :parent)
                      (button-get :path)
                      lsp--path-to-uri
                      lsp-java-treemacs--get-libraries)
  :root-label "External Libraries"
  :root-marker t
  :root-face font-lock-type-face
  :root-key-form (lsp--path-to-uri (button-get parent :path))
  :render-action (-let ((project-uri (-> btn
                                         (button-get :parent)
                                         (button-get :path)
                                         lsp--path-to-uri))
                        ((&hash "name" "id") item))
                   (treemacs-render-node
                    :icon treemacs-icon-jar-file-closed
                    :label-form name
                    :state treemacs-jar-file-closed-state
                    :face 'lsp-java-treemacs-directory-face
                    :key-form id
                    :more-properties (:node-id id :project-uri project-uri
                                               :workspace (lsp-java-treemacs--find-root project-uri)))))

(defun lsp-java-treemacs-register ()
  "Register `lsp-java' extension."
  (interactive)
  (->> lsp--workspaces
       ht-values
       -uniq
       (-map 'lsp-java-treemacs--update-project-uris))
  (treemacs-define-extension 'treemacs-EXTERNAL-LIBRARY-extension
                             'project-start))

(defun lsp-java-treemacs-unregister ()
  "Unregister extension."
  (interactive)
  (treemacs-remove-extension 'treemacs-EXTERNAL-LIBRARY-extension
                             'project-start))

(provide 'lsp-java-treemacs )
;;; lsp-java-treemacs.el ends here
