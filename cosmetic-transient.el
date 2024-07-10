;;; cosmetic-transient.el --- A transient for cosmetics on source code -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience text tools files
;; URL: https://github.com/akirak/cosmetic-transient.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'transient)

(defgroup cosmetic-transient nil
  "A transient for cosmetics on source code"
  :group 'transient)

(defcustom cosmetic-transient-language-formatters
  '((nix-ts-mode
     :minor-mode alejandra-on-save-mode
     :buffer-formatter alejandra-buffer
     :region-formatter alejandra-region)
    (elixir-ts-mode
     :minor-mode mix-format-on-save-mode
     :buffer-formatter mix-format-buffer
     :region-formatter mix-format-region)
    (sql-mode
     :minor-mode sqlformat-on-save-mode
     :buffer-formatter sqlformat-buffer
     :region-formatter sqlformat-region))
  "Alist of formatter settings for specific major modes."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type plist))

(defvar-local cosmetic-transient-language-formatter nil)

(defvar-local cosmetic-transient-docco-status nil)

;;;###autoload (autoload 'cosmetic-transient "cosmetic-transient" nil 'interactive)
(transient-define-prefix cosmetic-transient ()
  ["General status"
   :class transient-subgroups
   [("e"
     eglot-list-connections
     :description (lambda () (format "LSP: %s (eglot)" (eglot--languageId)))
     :if cosmetic-transient--eglot-p)
    ("e" "Enable eglot" eglot :if-not cosmetic-transient--eglot-p)]]
  ["Formatter"
   :class transient-subgroups
   [:if
    cosmetic-transient--eglot-p
    ("fe" "Format with LSP" eglot-format-buffer)]
   ;; Language-specific formatters
   [:description
    (lambda ()
      (format "%s" (car cosmetic-transient-language-formatter)))
    :if-non-nil cosmetic-transient-language-formatter
    :setup-children cosmetic-transient--language-formatter-children]]
  ["Linter"]
  ["Doc comments"
   :class transient-row
   :if (lambda ()
         (and (require 'docco nil t)
              ;; Use a side-effect to memoize the result
              (setq cosmetic-transient-docco-status (docco-comment-statuses))))
   ("df"
    (lambda ()
      (cosmetic-transient--doc-comment-description "Function" 'function))
    docco-edit-function-comment)
   ("dm"
    (lambda ()
      (cosmetic-transient--doc-comment-description "Module" 'module))
    docco-edit-module-comment)]
  (interactive)
  (setq cosmetic-transient-language-formatter
        (cosmetic-transient--language-formatter-settings))
  (transient-setup 'cosmetic-transient))

;;;; Eglot integration

(defun cosmetic-transient--eglot-p ()
  (bound-and-true-p eglot--managed-mode))

;;;; Documentation comments

(defun cosmetic-transient--doc-comment-description (name symbol)
  (format "%s: %s"
          name
          (if (alist-get symbol cosmetic-transient-docco-status)
              "Exists"
            "Not exists")))

;;;; Language-specific formatters

(defun cosmetic-transient--language-formatter-settings ()
  (when-let (mode (apply #'derived-mode-p
                         (mapcar #'car cosmetic-transient-language-formatters)))
    (assq mode cosmetic-transient-language-formatters)))

(defun cosmetic-transient--language-formatter-children (_)
  (let ((mode (plist-get (cdr cosmetic-transient-language-formatter) :minor-mode))
        (bfmt (plist-get (cdr cosmetic-transient-language-formatter) :buffer-formatter))
        (rfmt (plist-get (cdr cosmetic-transient-language-formatter) :region-formatter)))
    (thread-last
      (list (when mode
              (list :key "fm"
                    :description (format "Mode: %s (%s)"
                                         (if (and (boundp mode)
                                                  (symbol-value mode))
                                             "Enabled"
                                           "Disabled")
                                         mode)
                    :command mode))
            (when bfmt
              (list :key "fb"
                    :description (format "%s (on buffer)" bfmt)
                    :command bfmt))
            (when (and rfmt (use-region-p))
              (list :key "fr"
                    :description (format "%s (on region)" rfmt)
                    :command rfmt)))
      (delq nil)
      (mapcar (lambda (plist)
                (list transient--default-child-level
                      'transient-suffix
                      plist))))))

(provide 'cosmetic-transient)
;;; cosmetic-transient.el ends here
