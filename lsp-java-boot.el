;;; lsp-java-boot.el --- spring boot lsp client      -*- lexical-binding: t; -*-

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

;; lsp java spring boot integration.

;;; Code:


(defun lsp-java-boot--ls-command (port)
  (list "java"
        "-Dloader.path=/usr/lib/jvm/java-8-oracle/lib/tools.jar"
        (format "-Dspring.lsp.client-port=%s" port)
        (format "-Dserver.port=%s" port)
        "-Dsts.lsp.client=vscode"
        "-Dsts.log.file=/tmp/vscode-spring-boot-1537211835.log"
        "-jar"
        "/home/kyoncho/.vscode/extensions/pivotal.vscode-spring-boot-0.6.0/jars/spring-boot-language-server-0.6.0-SNAPSHOT.jar"))

(lsp-define-tcp-client lsp-java-boot "java"
                       (lambda () "/home/kyoncho/workspace1")
                       (lsp-java-boot--ls-command (setq  srping-boot-port (1+ srping-boot-port)))
                       "localhost"
                       5000
                       ;; :extra-init-params (list :workspaceFolders (mapcar
                       ;;                                             'lsp--path-to-uri
                       ;;                                             lsp-java--workspace-folders)
                       ;;                          :settings (lsp-java--settings)
                       ;;                          :extendedClientCapabilities (list :progressReportProvider t
                       ;;                                                            :classFileContentsSupport t)
                       ;;                          :bundles (lsp-java--bundles))
                       )

(provide 'lsp-java-boot)
;;; lsp-java-boot.el ends here
