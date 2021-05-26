;;; const-aliases --- define alias for C defconsts to allow let-bind

;; Copyright (C) 2015 Nelson Ferreira

;; Author: Nelson Ferreira <nelson.ferreira@ieee.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: internal

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; The following code is required at startup to define all the
;; necessary defconst alias for C DEFVAR_CONST that we wish to
;; allow let-bind to succeed.


;;; Code:

(when (featurep 'bigfr)
  (defconst pi bigfr-pi "The value of Pi (3.1415926...)")
  (defconst e euler "The value of e (2.7182818...)"))

(provide 'const-aliases)

