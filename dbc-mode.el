;;; dbc-mode.el --- major mode for CAN dbc files         -*- lexical-binding: t; -*-

;; Copyright (c) 2017 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/dbc-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages provides syntax highlighting and imenu support for CAN dbc
;; files

;;;; Setup

;; (require 'dbc-mode)

;;; Code:

(defvar dbc-mode-keywords '("VERSION" ; version
                            "NS_" ; net symbol
                            "BS_" ; bit timings section
                            "BO_" ; message / frame definition
                            "BU_" ; network node / board unit
                            "BO_TX_BU_"  ; message transmitter
                            "SG_" ; signal
                            "CM_" ; comment
                            "CM_ BO_" ; message / frame comments
                            "CM_ BU_" ; board unit comments
                            "CM_ EV_"
                            "CM_ SG_" ; signal comment
                            "BA_DEF_" ; definitions
                            "BA_DEF_DEF_" ; defaults
                            "BA_DEF_REL_"
                            "BA_DEF_DEF_REL_"
                            "BA_" ; attribute
                            "EV_" ; environment variables
                            "VAL_" ; value description
                            "BA_REL_"
                            "BU_SG_REL_"
                            "NS_DESC_"
                            "CAT_DEF_"
                            "CAT_"
                            "FILTER"
                            "EV_DATA_"
                            "ENVVAR_DATA_"
                            "SGTYPE_"
                            "SGTYPE_VAL_"
                            "BA_DEF_SGTYPE_"
                            "BA_SGTYPE_"
                            "SIG_TYPE_REF_"
                            "SIG_GROUP_"
                            "SIG_VALTYPE_"
                            "SIGTYPE_VALTYPE_"
                            "BU_EV_REL_"
                            "BU_BO_REL_"
                            "SG_MUL_VAL_"
                            "VAL_TABLE_"
                            "SIG_VALTYPE_"))


(defvar dbc-mode-font-lock-defaults
  `(((,(regexp-opt dbc-mode-keywords 'words) . font-lock-keyword-face)
     ("[=:@]" . 'font-lock-operator)
     (";" . 'font-lock-builtin)
     ;; message id and name
     ("BO_ \\([0-9]+ [A-Za-z0-9_]+\\)" 1 font-lock-function-name-face t)
     ;; signal name
     ("SG_ \\([A-Za-z0-9_]+\\) \\(\\|M  \\|m[0-9]+  \\): " 1 font-lock-variable-name-face t)
     ;; number decimal
     ("\\<-?[0-9.]+\\>" . 'font-lock-constant-face)
     ;; junk
     ("\\<[A-Za-z_,]\\>" . 'default)
     )))

(defvar dbc-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\; ">" table)
    table))

(defun dbc-mode-eldoc-function ()
  "Get documentation for option `eldoc-mode'."
  (save-excursion
    ;; for now do documentation for the entire line
    (let ((line (thing-at-point 'line t))
          (identifier nil)
          (id nil)
          (type nil)
          (doc nil))
      (cond
       ;; signal definition
       ((string-match "^\\s-*SG_ \\([A-Za-z0-9_]+\\)" line)
        (let ((name (match-string 1 line)))
          ;; find signal id
          (when (re-search-backward "^\\s-*BO_ \\([0-9]+\\)" nil t)
            (setq id (match-string 1))
            (when (re-search-forward (format "^\\s-*CM_ SG_ %s %s \""  id name) nil t)
              (setq identifier (propertize name 'face 'font-lock-variable-name-face))
              (setq type 'signal)))))
       ;; frame definition
       ((string-match "^\\s-*BO_ \\([0-9]+\\) \\([A-Za-z0-9_]+\\)" line)
        (let ((name (match-string 2 line)))
          (setq id (match-string 1 line))
          (when (re-search-forward (format "^\\s-*CM_ BO_ %s \""  id) nil t)
            (setq identifier (propertize name
                                         'face 'font-lock-function-name-face))
            (setq type 'frame))))
       ;; unit definition
       ((string-match "^\\s-*BU_:" line)
        (let ((symbol (thing-at-point 'symbol t)))
          (when (and symbol
                     (re-search-forward (format "^\\s-*CM_ BU_ %s \""  symbol) nil t))
            (setq identifier symbol)
            (setq type 'unit)))))
      (let ((start (point)))
        (when (and identifier
                   (search-forward "\";" nil t)) ; find end of doc
          (setq doc (concat identifier ": "
                            ;; strip off quote and semicolon
                            (propertize (buffer-substring start (- (point) 2)) 'face nil))))
        ;; if is a signal then try find value definitions for it as well
        (when (and (eq type 'signal)
                   (re-search-forward (format "^\\s-*VAL_ %s %s "  id (substring-no-properties identifier)) nil t))
          (setq start (point))
          (when (search-forward ";" nil t) ; find end of doc
            (setq doc (concat doc "\n\n"
                              ;; strip off quote and semicolon
                              (propertize (buffer-substring start (- (point) 2)) 'face nil))))))
      doc)))

(define-derived-mode dbc-mode fundamental-mode "dbc"
  "dbc-mode is a major mode for editing CAN DBC files."
  :syntax-table dbc-mode-syntax-table
  (setq font-lock-defaults dbc-mode-font-lock-defaults)
  (setq imenu-generic-expression '(("Messages" "^\\s-*BO_ \\([0-9]+ [A-Za-z0-9_]+\\)" 1)
                                   ("Signals" "^\\s-*SG_ \\([A-Za-z0-9_]+\\)" 1)))
  (setq comment-start "//")
  (setq comment-end "")
  (add-function :before-until (local 'eldoc-documentation-function)
                #'dbc-mode-eldoc-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dbc\\'" . dbc-mode))

(provide 'dbc-mode)
;;; dbc-mode.el ends here
