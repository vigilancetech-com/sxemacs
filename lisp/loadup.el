;; loadup.el --- load up standardly loaded Lisp files for SXEmacs.

;; Copyright (C) 1985, 1986, 1992, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Richard Mlynarik.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: Last synched with FSF 19.30, with wild divergence since.

;;; Commentary:

;; Please do not edit this file.  Use site-init.el or site-load.el instead.

;; ***Note the docstrings for the variables in this file. They follow the
;; conventions described in lib-src/make-docfile.c, and any new variables or
;; functions added to this file should follow those conventions too, since
;; this file is always loaded uncompiled, and the byte-compiler never gets a
;; chance to format the docstrings in the way make-docfile.c understands.

;; This is loaded into a bare SXEmacs to make a dumpable one.

;;; Code:

(when (fboundp 'error)
  (error "loadup.el already loaded!"))

(defvar running-xemacs t
  "Non-nil when the current emacs is XEmacs or SXEmacs.")
(defvar running-sxemacs t
  "Non-nil when the current emacs is SXEmacs.")

;; Can't make this constant for now because it causes an error in
;; update-elc.el.
(defvar source-lisp (file-name-directory (expand-file-name (nth 2 command-line-args))) "\
Root of tree containing the Lisp source code for the current build.
Differs from `lisp-directory' if this SXEmacs has been installed. ")

(defconst build-directory (expand-file-name ".." (expand-file-name ".." invocation-directory)) "\
Root of tree containing object files and executables produced by build.
Differs from `source-directory' if configured with --srcdir option, a practice
recommended for developers.")

(defconst source-directory (expand-file-name ".." source-lisp)  "\
Root of tree containing source code for the current build.
Used during loadup and for documenting source of symbols defined in C.")

(defvar preloaded-file-list nil "\
List of Lisp files preloaded into the XEmacs binary image,
with the exception of `loadup.el'.")

(defvar Installation-string nil
  "Description of SXEmacs installation.")

;(start-profiling)

(defun compute-build-root (dir)
  "Given DIR as basis, traverse parent-wards until the cookie
file .sxemacs.source.tree is found."
  (when (stringp dir)
    (while (and (file-readable-p dir)
		(not (string-equal "/" dir))
		(not (file-exists-p
		      (expand-file-name ".sxemacs.source.tree" dir))))
      (setq dir (expand-file-name ".." dir)))
    dir))


(let ((gc-cons-threshold
       ;; setting it low makes loadup incredibly fucking slow.
       ;; no need to do it when not dumping.
       (if (and purify-flag
		(not (memq 'quick-build internal-error-checking)))
	   30000 3000000)))


;; This is awfully damn early to be getting an error, right?
(call-with-condition-handler 'really-early-error-handler
    #'(lambda ()

	;; Initialize Installation-string.  We do it before loading
	;; anything so that dumped code can make use of its value.
	(setq Installation-string
	      (save-current-buffer
		(set-buffer (get-buffer-create (generate-new-buffer-name
						" *temp*")))
		;; insert-file-contents-internal bogusly calls
		;; format-decode without checking if it's defined.
		(fset 'format-decode #'(lambda (f l &optional v) l))
		(insert-file-contents-internal "../Installation")
		(fmakunbound 'format-decode)
		(prog1 (buffer-substring)
		  (kill-buffer (current-buffer)))))

	(let ((build-root (compute-build-root invocation-directory))
	      (source-tree-root (getenv "SOURCE_TREE_ROOT"))
	      (build-tree-root (getenv "BUILD_TREE_ROOT")))
	  (setq load-path
		(list (expand-file-name "lisp" build-root)
		      (expand-file-name "lisp" build-tree-root)
		      (expand-file-name "lisp" source-tree-root)))
	  (setq module-load-path
		(list (expand-file-name "modules" build-root)
		      (expand-file-name "modules" build-tree-root)
		      (expand-file-name "modules" source-tree-root)))
	  (unless (file-exists-p (car load-path))
	    (setq load-path (cdr load-path)))
	  (unless (file-exists-p (car module-load-path))
	    (setq module-load-path (cdr module-load-path))))

	;; message not defined yet ...
	(external-debugging-output (format "\nUsing load-path %s" load-path))
	(external-debugging-output (format "\nUsing module-load-path %s"
					   module-load-path))

	;; We don't want to have any undo records in the dumped SXEmacs.
	(buffer-disable-undo (get-buffer "*scratch*"))

	;; Load our first bootstrap support
	(load "very-early-lisp" nil t)

	;; lread.c (or src/Makefile.in.in) has prepended
	;; "${srcdir}/../lisp/" to load-path, which is how this file
	;; has been found.  At this point, enough of SXEmacs has been
	;; initialized that we can start dumping "standard" lisp.
	;; Dumped lisp from external packages is added when we search
	;; the package path.
	;; #### This code is duplicated in two other places.
	(let ((temp-path (expand-file-name "." (car load-path))))
	  (setq load-path
		(nconc
		 (mapcar
		  #'(lambda (i) (concat i "/"))
		  (directory-files temp-path t "^[^-.]"
				   nil 'subdir))
		 (cons (file-name-as-directory temp-path)
		       load-path))))

	(setq load-warn-when-source-newer t ; Used to be set to nil at the end
	      load-warn-when-source-only  t) ; Set to nil at the end

	;; garbage collect after loading every file in an attempt to
	;; minimize the size of the dumped image (if we don't do this,
	;; there will be lots of extra space in the data segment filled
	;; with garbage-collected junk)
	(defun pureload (file)
	  (let ((full-path
		 (locate-file file load-path
			      (if load-ignore-elc-files
				  '(".el" "") '(".elc" ".el" "")))))
	    (if full-path
		(prog1
		    (load full-path)
		  ;; but garbage collection really slows down loading.
		  (unless (memq 'quick-build internal-error-checking)
		    (garbage-collect)))
	      (external-debugging-output (format "\nLoad file %s: not found\n"
						 file))
	      ;; Uncomment in case of trouble
	      ;;(print (format "late-packages: %S" late-packages))
	      ;;(print (format "guessed-roots: %S" (paths-find-emacs-roots invocation-directory invocation-name)))
	      nil)))

	(let ((f (locate-file "dumped-lisp.el" load-path)))
	  (load f))

	(let ((files preloaded-file-list)
	      file)
	  (while (setq file (car files))
	    (unless (pureload file)
	      (external-debugging-output "Fatal error during load, aborting")
	      (kill-emacs 1))
	    (setq files (cdr files)))
	  (when (not (featurep 'toolbar))
	    ;; else still define a few functions.
	    (defun toolbar-button-p    (obj) "No toolbar support." nil)
	    (defun toolbar-specifier-p (obj) "No toolbar support." nil))
	  (fmakunbound 'pureload))

	(packages-load-package-dumped-lisps late-package-load-path)

	)) ;; end of call-with-condition-handler

;; Fix up the preloaded file list
(setq preloaded-file-list (mapcar #'file-name-sans-extension
				  preloaded-file-list))

(setq load-warn-when-source-newer t ; set to t at top of file
      load-warn-when-source-only nil)

(setq debugger 'debug)

(when (member "no-site-file" command-line-args)
  (setq site-start-file nil))

;; If you want additional libraries to be preloaded and their
;; doc strings kept in the DOC file rather than in core,
;; you may load them with a "site-load.el" file.
;; But you must also cause them to be scanned when the DOC file
;; is generated.  For VMS, you must edit ../../vms/makedoc.com.
;; For other systems, you must edit ../../src/Makefile.in.in.
(when (load "site-load" t)
  (garbage-collect)
)

;;FSFmacs randomness
;;(if (fboundp 'x-popup-menu)
;;    (precompute-menubar-bindings))
;;; Turn on recording of which commands get rebound,
;;; for the sake of the next call to precompute-menubar-bindings.
;(setq define-key-rebound-commands nil)

;; Note: all compiled Lisp files loaded above this point
;; must be among the ones parsed by make-docfile
;; to construct DOC.  Any that are not processed
;; for DOC will not have doc strings in the dumped SXEmacs.

;; Don't bother with these if we're running temacs, i.e. if we're
;; just debugging don't waste time finding doc strings.

;; purify-flag is nil if called from loadup-el.el.
(when purify-flag
  (message "Finding pointers to doc strings...")
  (Snarf-documentation "DOC")
  (message "Finding pointers to doc strings...done")
  (Verify-documentation))

;; Note: You can cause additional libraries to be preloaded
;; by writing a site-init.el that loads them.
;; See also "site-load" above.
(when (stringp site-start-file)
  (load "site-init" t))
;; Add information from this file to the load history:
(setq load-history (cons (nreverse current-load-list) load-history)
      ;; Clear current-load-list; this (and adding information to
      ;; load-history) is normally done in lread.c after reading the
      ;; entirety of a file, something which never happens for loadup.el.
      current-load-list nil)
;; Make the path to this file look a little nicer:
(setcar (car load-history) (file-truename (caar load-history)))

(garbage-collect)

;;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

) ;; frequent garbage collection

;(stop-profiling)

;; yuck!  need to insert the function def here, and rewrite the dolist
;; loop below.

;(defun loadup-profile-results (&optional info stream)
;  "Print profiling info INFO to STREAM in a pretty format.
;If INFO is omitted, the current profiling info is retrieved using
; `get-profiling-info'.
;If STREAM is omitted, either a *Profiling Results* buffer or standard
; output are used, depending on whether the function was called
; interactively or not."
;  (interactive)
;  (setq info (if info
;		 (copy-alist info)
;	       (get-profiling-info)))
;  (when (and (not stream)
;	     (interactive-p))
;    (pop-to-buffer (get-buffer-create "*Profiling Results*"))
;    (erase-buffer))
;  (let ((standard-output (or stream (if (interactive-p)
;					(current-buffer)
;				      standard-output)))
;	;; Calculate the longest function
;	(maxfunlen (apply #'max
;			  (length "Function Name")
;			  (mapcar
;			   (lambda (el)
;			     ;; Functions longer than 50 characters (usually
;			     ;; anonymous functions) don't qualify
;			     (let ((l (length (format "%s" (car el)))))
;			       (if (< l 50)
;				   l 0)))
;			   info))))
;    (princ (format "%-*s    Ticks    %%/Total   Call Count\n"
;		   maxfunlen "Function Name"))
;    (princ (make-string maxfunlen ?=))
;    (princ "    =====    =======   ==========\n")
;    (let ((sum (float (apply #'+ (mapcar #'cdr info)))))
;      (let (entry
;	    (entry-list (nreverse (sort info #'cdr-less-than-cdr))))
;	(while entry-list
;	  (setq entry (car entry-list))
;	  (princ (format "%-*s    %-5d    %-6.3f    %s\n"
;			 maxfunlen (car entry) (cdr entry)
;			 (* 100 (/ (cdr entry) sum))
;			 (or (gethash (car entry) call-count-profile-table)
;			     "")))
;	  (setq entry-list (cdr entry-list))))
;      (princ (make-string maxfunlen ?-))
;      (princ "---------------------------------\n")
;      (princ (format "%-*s    %-5d    %-6.2f\n" maxfunlen "Total" sum 100.0))
;      (princ (format "\n\nOne tick = %g ms\n"
;		     (/ default-profiling-interval 1000.0)))
;      (and (boundp 'internal-error-checking)
;	   internal-error-checking
;	   (princ "
;WARNING: Error checking is turned on in this SXEmacs.  This might make
;         the measurements very unreliable.\n"))))
;  (when (and (not stream)
;	     (interactive-p))
;    (goto-char (point-min))))

;(loadup-profile-results nil 'external-debugging-output)

;; Dump into the name `sxemacs' (only)
(let ((cmds (member "--dump" command-line-args)))
  (when cmds
    (let* ((dmpf (and (cdr cmds) (stringp (cadr cmds)) (cadr cmds)))
	   (invf (expand-file-name invocation-name invocation-directory)))
      (message "Dumping under the name %s" dmpf)
      ;; This is handled earlier in the build process.
      ;; (condition-case () (delete-file "sxemacs") (file-error nil))
      (when-fboundp 'really-free
	(really-free))
      (dump-emacs invf dmpf)
      (kill-emacs))))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

(when (member "run-temacs" command-line-args)
  (message "\nBootstrapping from temacs...")
  ;; Remove all args up to and including "run-temacs"
  (apply #'run-emacs-from-temacs (cdr (member "run-temacs" command-line-args)))
  ;; run-emacs-from-temacs doesn't actually return anyway.
  (kill-emacs))

;; XEmacs change
;; If you are using 'recompile', then you should have used -l loadup-el.el
;; so that the .el files always get loaded (the .elc files may be out-of-
;; date or bad).
(when (member "recompile" command-line-args)
  (setq command-line-args-left (cdr (member "recompile" command-line-args)))
  (batch-byte-recompile-directory)
  (kill-emacs))

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(when (not (fboundp 'dump-emacs))
  ;; Avoid loading loadup.el a second time!
  (setq command-line-args (cdr (cdr command-line-args)))
  (eval top-level))

;;; loadup.el ends here
