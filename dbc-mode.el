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
                            "BO_" ; message definition
                            "BU_" ; network node
                            "BO_TX_BU_"  ; message transmitter
                            "CM_" ; comment
                            "CM_ BO_"
                            "CM_ EV_"
                            "CM_ SG_" ; signal comment
                            "SG_"
                            "BA_DEF_"
                            "BA_DEF_DEF_"
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
     ("SG_ \\([A-Za-z0-9_]+\\) : " 1 font-lock-variable-name-face t)
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

(define-derived-mode dbc-mode fundamental-mode "dbc"
  "dbc-mode is a major mode for editing CAN DBC files."
  :syntax-table dbc-mode-syntax-table
  (setq font-lock-defaults dbc-mode-font-lock-defaults)
  (setq imenu-generic-expression '(("Messages" "^\\s-*BO_ \\([0-9]+ [A-Za-z0-9_]+\\)" 1)
                                   ("Signals" "^\\s-*SG_ \\([A-Za-z0-9_]+\\)" 1)))
  (setq comment-start "//")
  (setq comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dbc\\'" . dbc-mode))

(provide 'dbc-mode)
;;; dbc-mode.el ends here
