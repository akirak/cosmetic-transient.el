;;; cosmetic-transient.el --- A transient for cosmetics on source code -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Akira Komamura

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
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)

(declare-function eglot--languageId "eglot")
(declare-function docco-edit-comment-of-type "ext:docco")
(defvar apheleia-formatter)
(defvar apheleia-formatters)

(defgroup cosmetic-transient nil
  "A transient for cosmetics on source code"
  :group 'transient)

(defcustom cosmetic-transient-language-formatters
  '((nix
     :minor-mode nixfmt-on-save-mode
     :buffer-formatter nixfmt-buffer
     :region-formatter nixfmt-region)
    (elixir
     :minor-mode mix-format-on-save-mode
     :buffer-formatter mix-format-buffer
     :region-formatter mix-format-region)
    (ocaml
     :minor-mode ocamlformat-on-save-mode
     :buffer-formatter ocamlformat-buffer
     :region-formatter ocamlformat-region)
    (sql
     :minor-mode sqlformat-on-save-mode
     :buffer-formatter sqlformat-buffer
     :region-formatter sqlformat-region))
  ;; There is no well-accepted standard formatter for SQL, so it is not
  ;; supported out of the box.
  "Alist of formatter settings for specific major modes."
  :type '(alist :key-type (symbol :tag "Language name")
                :value-type plist))

(defcustom cosmetic-transient-string-ts-nodes
  '((rust "string_content")
    (typescript "string_fragment")
    (elixir "quoted_content"))
  "Alist of tree-sitter node types for strings."
  :type '(alist :key-type (symbol :tag "Treesit major mode")
                :value-type (repeat :tag "List of node types" string)))

(defcustom cosmetic-transient-syntax-options
  '((elixir :end-with-newline t))
  ""
  :type '(alist :key-type (symbol :tag "Language name based on the treesit mode")
                :value-type (plist :tag "Options")))

(defcustom cosmetic-transient-ts-language-detectors
  '((typescript . cosmetic-transient-ts-js-language))
  "Alist of functions that returns the local language of the node."
  :type '(alist :key-type (symbol :tag "Treesit major mode")
                :value-type (function :tag "Function that takes a node as an argument")))

(defvar-local cosmetic-transient-language-formatter nil)

(defvar-local cosmetic-transient-docco-status nil)

(defvar-local cosmetic-transient-string-node nil)

(defvar cosmetic-transient-alternative-language nil)

;;;; Infixes

(defclass cosmetic-transient-choice-variable (transient-variable)
  ((variable :initarg :variable)
   (completion-table :initarg :completion-table)))

(cl-defmethod transient-init-value ((obj cosmetic-transient-choice-variable))
  (let ((value (symbol-value (oref obj variable))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj cosmetic-transient-choice-variable))
  (intern (completing-read (oref obj prompt)
                           (oref obj completion-table))))

(cl-defmethod transient-infix-set ((obj cosmetic-transient-choice-variable) value)
  (set (oref obj variable) (oset obj value value)))

(cl-defmethod transient-format-value ((obj cosmetic-transient-choice-variable))
  (if-let* ((value (oref obj value)))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (if value
           (propertize (format "%s" value) 'face 'transient-value)
         (propertize "nil" 'face 'transient-inactive-value))
       (propertize ")" 'face 'transient-inactive-value))
    ""))

(transient-define-infix cosmetic-transient-set-apheleia-formatter ()
  :class 'cosmetic-transient-choice-variable
  :variable 'apheleia-formatter
  :completion-table
  (lambda (string pred action)
    (complete-with-action action (mapcar #'car apheleia-formatters)
                          string pred))
  :description "Formatter"
  :prompt "apheleia-formatter: ")

;;;; Prefix

;;;###autoload (autoload 'cosmetic-transient "cosmetic-transient" nil 'interactive)
(transient-define-prefix cosmetic-transient ()
  :refresh-suffixes t
  [:class
   transient-columns
   ["Apheleia"
    :if cosmetic-transient--apheleia-installed-p
    :class transient-row
    ("-a" cosmetic-transient-set-apheleia-formatter)
    ("fa" "Format with Apheleia" apheleia-format-buffer
     :if cosmetic-transient--apheleia-configured-p)]
   ["Eglot"
    :if cosmetic-transient--eglot-p
    :class transient-row
    ("e" eglot-list-connections
     :description (lambda () (format "LSP: %s (eglot)" (eglot--languageId))))
    ("fe" "Format with eglot" eglot-format-buffer)]
   ["Eglot: Not enabled"
    :if-not cosmetic-transient--eglot-p
    ("e" "Enable eglot" eglot)]
   ;; Experimental; Is there a better way to integrate this feature?
   ["Misc"
    :class transient-row
    ("s" "Cleanup whitespace" whitespace-cleanup)]]
  ;; Language-specific formatters
  [:description
   (lambda ()
     (format "%s" (car cosmetic-transient-language-formatter)))
   :if-non-nil cosmetic-transient-language-formatter
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'cosmetic-transient
                               (cosmetic-transient--language-formatter-children)))]
  ["Format the string at point"
   :if-non-nil cosmetic-transient-string-node
   :class transient-row
   ("-l" cosmetic-transient-other-language)
   ("fo" "Format" cosmetic-transient-run-other-formatter)]
  ["Linter"]
  ["Documentation comments"
   :if (lambda ()
         (require 'docco nil t))
   ("c" "Insert/edit a comment" docco-edit-comment-of-type)]
  (interactive)
  (setq cosmetic-transient-language-formatter
        (cosmetic-transient--language-formatter-settings))
  (let ((node-or-region (cosmetic-transient--string-region-at (point))))
    (setq cosmetic-transient-string-node node-or-region)
    (setq cosmetic-transient-alternative-language
          (or cosmetic-transient-alternative-language
              (when node-or-region
                (cosmetic-transient--region-language node-or-region)))))
  (transient-setup 'cosmetic-transient))

;;;; Eglot integration

(defun cosmetic-transient--eglot-p ()
  (bound-and-true-p eglot--managed-mode))

;;;; Apheleia integration

(defun cosmetic-transient--apheleia-installed-p ()
  (require 'apheleia nil t))

(defun cosmetic-transient--apheleia-configured-p ()
  (or (bound-and-true-p apheleia-mode)
      (bound-and-true-p apheleia-formatter)))

;;;; Language-specific formatters

(defun cosmetic-transient--mode-language (mode)
  (cl-case mode
    ((neocaml-mode neocamli-mode tuareg-mode)
     'ocaml)
    (otherwise (thread-last
                 (symbol-name mode)
                 (string-remove-suffix "-ts-mode")
                 (string-remove-suffix "-mode")
                 (intern)))))

(defun cosmetic-transient--language-formatter-settings ()
  (assq (cosmetic-transient--mode-language major-mode)
        cosmetic-transient-language-formatters))

(defun cosmetic-transient--language-formatter-children ()
  (let ((mode (plist-get (cdr cosmetic-transient-language-formatter) :minor-mode))
        (bfmt (plist-get (cdr cosmetic-transient-language-formatter) :buffer-formatter))
        (rfmt (plist-get (cdr cosmetic-transient-language-formatter) :region-formatter)))
    (thread-last
      (list (when mode
              (list "fm"
                    (format "Mode: %s (%s)"
                            (if (and (boundp mode)
                                     (symbol-value mode))
                                "Enabled"
                              "Disabled")
                            mode)
                    mode))
            (when bfmt
              (list "fb"
                    (format "%s (on buffer)" bfmt)
                    bfmt))
            (when (and rfmt (use-region-p))
              (list "fr"
                    (format "%s (on region)" rfmt)
                    rfmt)))
      (delq nil))))

;;;; Formatter for another language

(defclass cosmetic-transient-language (transient-variable)
  ((variable :initarg :variable)
   (prompt :initarg :prompt)))

(cl-defmethod transient-init-value ((obj cosmetic-transient-language))
  (let ((value (symbol-value (oref obj variable))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj cosmetic-transient-language))
  (completing-read "Language: " cosmetic-transient-language-formatters
                   nil 'require-match
                   nil nil nil
                   (symbol-value (oref obj variable))))

(cl-defmethod transient-infix-set ((obj cosmetic-transient-language) value)
  (let ((value (if (stringp value)
                   (intern value)
                 value)))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-format-value ((obj cosmetic-transient-language))
  (if-let* ((value (oref obj value)))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (propertize (format "%s" value)
                   'face 'transient-value)
       (propertize ")" 'face 'transient-inactive-value))
    ""))

(transient-define-infix cosmetic-transient-other-language ()
  :class 'cosmetic-transient-language
  :variable 'cosmetic-transient-alternative-language
  :prompt "Language: "
  :description "Language")

(defun cosmetic-transient-run-other-formatter ()
  (interactive)
  (if cosmetic-transient-alternative-language
      (if-let* ((plist (cdr (assq cosmetic-transient-alternative-language
                                  cosmetic-transient-language-formatters)))
                (formatter (plist-get plist :region-formatter)))
          (pcase (cosmetic-transient--bounds cosmetic-transient-string-node)
            (`(,beg . ,end)
             (funcall formatter beg end)
             (cosmetic-transient--apply-offset beg
               :end-with-newline
               (thread-first
                 (cosmetic-transient--mode-language major-mode)
                 (assq cosmetic-transient-syntax-options)
                 (cdr)
                 (plist-get :end-with-newline)))))
        (user-error "No formatter is set"))
    (user-error "No language is set")))

(defun cosmetic-transient--region-language (node-or-region)
  (if (treesit-node-p node-or-region)
      (when-let* ((func (alist-get (treesit-node-language node-or-region)
                                 cosmetic-transient-ts-language-detectors)))
        (funcall func node-or-region))))

(defun cosmetic-transient--string-region-at (pos)
  (if-let* ((treesit-node-types (alist-get (cosmetic-transient--mode-language major-mode)
                                           cosmetic-transient-string-ts-nodes))
            (ts-node (and (treesit-available-p)
                          (treesit-language-at pos)
                          (treesit-node-at pos))))
      (when (member (treesit-node-type ts-node)
                    treesit-node-types)
        ts-node)
    (when-let* ((start (ppss-comment-or-string-start (syntax-ppss))))
      (save-excursion
        (goto-char start)
        (forward-sexp)
        ;; It is assumed that string quotations are single characters.
        ;; Otherwise, this function malfunctions.
        (cons (1+ start) (1- (point)))))))

(defun cosmetic-transient--bounds (node-or-region)
  (pcase-exhaustive (if (treesit-node-p node-or-region)
                        (cons (treesit-node-start node-or-region)
                              (treesit-node-end node-or-region))
                      node-or-region)
    (`(,beg . ,end)
     (cons (save-excursion
             (goto-char beg)
             (if (eolp)
                 (re-search-forward (rx word-start) end)
               beg))
           end))))

(cl-defun cosmetic-transient--apply-offset (beg &key end-with-newline)
  (declare (indent 1))
  (save-excursion
    (goto-char beg)
    (let ((offset (current-column))
          (bound (cdr (cosmetic-transient--bounds (cosmetic-transient--string-region-at beg)))))
      (end-of-line)
      (when (> offset 0)
        (replace-regexp-in-region (rx bol) (make-string offset ?\s) nil bound))
      (goto-char (cdr (cosmetic-transient--bounds
                       (cosmetic-transient--string-region-at beg))))
      (delete-all-space)
      (whitespace-cleanup-region beg (point))
      (when end-with-newline
        (newline-and-indent)))))

(defun cosmetic-transient-ts-js-language (node)
  "Detect the language of \"string_fragment\" node."
  (when-let* ((parent (treesit-node-parent node))
              (node1 (treesit-node-prev-sibling parent))
              (tag (when (equal (treesit-node-type node1) "identifier")
                     (treesit-node-text node1))))
    (cdr (assoc tag '(("gql" . graphql))))))

(provide 'cosmetic-transient)
;;; cosmetic-transient.el ends here
