;;; lsp-java-error-list.el --- Error list for java   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: abbrev, abbrev

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

;;

;;; Code:

(require 'lsp-common)
(require 'lsp-java-classpath)

(defvar lsp-java-error-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") #'lsp-java-error-list-fix)
    (define-key map (kbd "RET") 'lsp-java-error-list-select)
    map))

(defun lsp-java-error-list-select ()
  "Goto to the selected element in the list."
  (interactive)
  (if-let (goto-fn (-some-> (dap-ui-sessions--tree-under-cursor)
                            (widget-get :goto-fn)))
      (funcall goto-fn)
    (message "Nothing to act on")))

(defun lsp-java-error-list-fix ()
  "Select the element under cursor."
  (interactive)
  (let ((workspace lsp--cur-workspace ))
    (if-let* ((diag (widget-get (dap-ui-sessions--tree-under-cursor) :diag))
              (file-name (widget-get (dap-ui-sessions--tree-under-cursor) :file-name)))
        (with-current-buffer (find-file-noselect file-name)
          (with-lsp-workspace workspace
            (save-excursion
              (goto-char (point-min))
              (goto-line (1+ (lsp-diagnostic-line diag)))
              (goto-char (+ (point-at-bol) (lsp-diagnostic-column diag)))
              (call-interactively 'lsp-execute-code-action))))
      (message "Nothing to act on"))))

(defun lsp-java-error-list--schedule-update ()
  (when dap-ui--sessions-refresh-timer
    (cancel-timer dap-ui--sessions-refresh-timer)
    (setq lsp-java-error-list--refresh-timer
          (run-at-time dap-ui-session-refresh-delay nil 'lsp-java-error-list--update-diagnostics))))

(define-minor-mode lsp-java-error-list-mode
  "Minor mode for browsing errors."
  :init-value nil
  :group lsp-java
  :keymap lsp-java-error-list-map
  (setq buffer-read-only t)
  (if lsp-java-error-list-mode
      (add-hook 'lsp-after-diagnostics-hook 'lsp-java-error-list--schedule-update)
    (remove-hook 'lsp-after-diagnostics-hook 'lsp-java-error-list--schedule-update)))

(defun lsp-java-error-list-error-icon (severity)
  (case severity
    (1 lsp-java-icons-error)
    (2 lsp-java-icons-warning)
    (3 lsp-java-icons-info)
    (4 lsp-java-icons-info)))

(defvar lsp-java-error-list--refresh-timer nil)

(defun lsp-java-error-list--update-diagnostics ()
  (setq lsp-java-error-list--refresh-timer nil)
  (let ((inhibit-read-only t)
        (workspace lsp--cur-workspace)
        (buf (get-buffer-create "*lsp-java-errors*")))
    (with-current-buffer buf
      (erase-buffer)
      (setq-local lsp--cur-workspace workspace)
      (setq-local tree-widget-themes-directory lsp-java-themes-directory)

      (tree-widget-set-theme lsp-java-theme)

      (-map
       (-lambda ((project-dir . rest))
         (when project-dir
           (widget-create
            `(tree-widget
              :value ,(f-filename project-dir)
              :open t
              :goto-fn (lambda () (find-file ,project-dir))
              ,@lsp-java-icons-default-root-folder
              ,@(-map
                 (-lambda ((_ file-name . errors))
                   `(tree-widget
                     :value ,(f-filename file-name)
                     :open t
                     :goto-fn (lambda () (find-file ,file-name))
                     ,@lsp-java-icons-file-type-class
                     ,@(-map
                        (-lambda (diag)
                          `(tree-widget
                            :diag ,diag
                            :file-name ,file-name
                            :goto-fn (lambda ()
                                       (find-file ,file-name)
                                       (goto-char (point-min))
                                       (goto-line (1+ ,(lsp-diagnostic-line diag)))
                                       (goto-char (+ (point-at-bol) ,(lsp-diagnostic-column diag))))
                            :value ,(substring (lsp-diagnostic-message diag) 0 (if (< (length (lsp-diagnostic-message diag)) 100)
                                                                                   (length (lsp-diagnostic-message diag))
                                                                                 100))
                            ,@(lsp-java-error-list-error-icon (lsp-diagnostic-severity diag))))
                        errors)))
                 rest)))))
       (let ((maven-project-roots (->> lsp--cur-workspace
                                       lsp-java-treemacs--get-project-uris
                                       (-map 'lsp--uri-to-path))))
         (->> lsp--diagnostics
              (ht-map
               (lambda (file-name errors)
                 (list* (-max-by (lambda (project-a project-b)
                                   (> (length project-a)
                                      (length project-b)))
                                 (--filter (s-starts-with? it file-name) maven-project-roots))
                        file-name
                        errors)))
              (-group-by 'first))))
      buf)))

(defun lsp-java-error-list ()
  "Show currently active sessions."
  (interactive)
  (lsp--cur-workspace-check)
  (dap-ui--show-buffer (lsp-java-error-list--update-diagnostics))
  (lsp-java-error-list-mode 1))

(provide 'lsp-java-error-list)

(with-current-buffer "AppTest.java"
  (lsp-java-error-list))

;; (-flatten (ht-keys lsp--diagnostics))

(with-current-buffer "AppTest.java"
  (length
   (let ((maven-project-roots (->> lsp--cur-workspace
                                   lsp-java-treemacs--get-project-uris
                                   (-map 'lsp--uri-to-path))))
     (->> lsp--diagnostics
          (ht-map
           (lambda (file-name errors)
             (list* (-max-by (lambda (project-a project-b)
                               (> (length project-a)
                                  (length project-b)))
                             (--filter (s-starts-with? it file-name) maven-project-roots ))
                    file-name
                    errors)))
          (-group-by 'first)))))

;; (remove-hook 'lsp-java-error-list-mode-hook 'evil-evilified-state)

;; (gethash (last (map-keys lsp--diagnostics)) lsp--diagnostics)
;; (widget-create
;;  `(tree-widget
;;    :value "sss"))
