;;; startup.el --- process SXEmacs shell arguments

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 2004 - 2015 Steve Youngs

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; -batch, -t, and -nw are processed by main() in emacs.c and are
;; never seen by lisp code.

;; -version and -help are special-cased as well: they imply -batch,
;; but are left on the list for lisp code to process.

;;; Code:

(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst startup-message-timeout 12000) ; More or less disable the timeout
(defconst splash-frame-timeout 10) ; interval between splash frame elements

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup message.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

(defvar command-line-args-left nil
  "List of command-line args not yet processed.") ; bound by `command-line'

(defvar command-line-default-directory nil
  "Default directory to use for command line arguments.
This is normally copied from `default-directory' when SXEmacs starts.")

(defvar before-init-hook nil
  "Functions to call after handling urgent options but before init files.
The frame system uses this to open frames to display messages while
SXEmacs loads the user's initialization file.")

(defvar after-init-hook nil
  "*Functions to call after loading the init file.
The call is not protected by a condition-case, so you can set `debug-on-error'
in the init file, and put all the actual code on `after-init-hook'.")

(defvar term-setup-hook nil
  "*Functions to be called after loading terminal-specific Lisp code.
See `run-hooks'.  This variable exists for users to set, so as to
override the definitions made by the terminal-specific file.  SXEmacs
never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.
This variable is used to define the proper function and keypad keys
for use under X.  It is used in a fashion analogous to the environment
value TERM.")

(defvar window-setup-hook nil
  "Normal hook run to initialize window system display.
SXEmacs runs this hook after processing the command line arguments and loading
the user's init file.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defvar emacs-roots nil
  "List of plausible roots of the SXEmacs hierarchy.")

(defun find-user-init-directory ()
  "Find and set the user's init directory.

If no init directory currently exists, this will return:
\"$XDG_CONFIG_HOME/sxemacs\", which falls back to
\"~/.config/sxemacs\" if $XDG_CONFIG_HOME is not set in the user's
environment.

If the legacy init directory, \"~/.sxemacs\" exists, return that.

If both the legacy directory and the XDG-based directory exist, return
the XDG-based directory unless $SXE_USE_LEGACY is set in the user's
environment."
  (let* ((legacy (getenv "SXE_USE_LEGACY"))
	 (xdg (getenv "XDG_CONFIG_HOME"))
	 (xdgdir (or (and xdg
			  (paths-construct-path
			   (list xdg "sxemacs")))
		     (paths-construct-path
		      (list (user-home-directory) ".config" "sxemacs"))))
	 (legacydir (paths-construct-path
		     (list (user-home-directory) ".sxemacs")))
	 (locations (list xdgdir legacydir))
	 (multi (count-if #'file-directory-p locations)))
    (if legacy
	(setq user-init-directory (file-name-as-directory legacydir))
      (catch 'found
	(dolist (dir locations)
	  (and (paths-file-readable-directory-p dir)
	       (throw 'found (setq user-init-directory
				   (file-name-as-directory dir)))))
	(setq user-init-directory (file-name-as-directory xdgdir))))
    ;; Warn if multiple init directories exist
    (when (> multi 1)
      (lwarn 'multi-initd nil
	"Multiple init directories found:
%S

Currently using: %s

See `display-warning-suppressed-classes' to suppress this warning"
	locations user-init-directory))))

(defvar user-init-directory ""
  "Directory where user-installed init files may go.

See: `find-user-init-directory'.")

(defvar user-init-file-base-list '("init.elc" "init.el")
  "List of allowed init files in the user's init directory.
The first one found takes precedence.")

(defvar load-user-init-file-p t
  "Non-nil if SXEmacs should load the user's init file.")

;; #### called `site-run-file' in FSFmacs

(defvar site-start-file "site-start"
  "File containing site-wide run-time initializations.
This file is loaded at run-time before `user-init-file'.  It
contains inits that need to be in place for the entire site, but
which, due to their higher incidence of change, don't make sense to
load into SXEmacs' dumped image.  Thus, the run-time load order is:

  1. file described in this variable, if non-nil;
  2. `user-init-file';
  3. `/path/to/sxemacs/lisp/default.el'.

Don't use the `site-start.el' file for things some users may not like.
Put them in `default.el' instead, so that users can more easily
override them.  Users can prevent loading `default.el' with the `-q'
option or by setting `inhibit-default-init' in their own init files,
but inhibiting `site-start.el' requires `--no-site-file', which
is less convenient.")

;;(defconst iso-8859-1-locale-regexp "8859[-_]?1"
;;  "Regexp that specifies when to enable the ISO 8859-1 character set.
;;We do that if this regexp matches the locale name
;;specified by the LC_ALL, LC_CTYPE and LANG environment variables.")

(defcustom mail-host-address nil
  "*Name of this machine, for purposes of naming users."
  :type 'string
  :group 'mail)

(defcustom user-mail-address nil
  "*Full mailing address of this user.
This is initialized based on `mail-host-address',
after your init file is read, in case it sets `mail-host-address'."
  :type 'string
  :group 'mail)

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defvar init-file-loaded nil
  "True after the user's init file has been loaded (or suppressed with -q).
This will be true when `after-init-hook' is run and at all times
after, and will not be true at any time before.")

(defvar initial-frame-unmapped-p nil)



(defvar command-switch-alist
  '(("-help"	. command-line-do-help)
    ("-h"       . command-line-do-help)
    ("-version" . command-line-do-version)
    ("-V"	. command-line-do-version)
    ("-funcall" . command-line-do-funcall)
    ("-f"	. command-line-do-funcall)
    ("-e"	. command-line-do-funcall-1)
    ("-eval"	. command-line-do-eval)
    ("-load"	. command-line-do-load)
    ("-l"	. command-line-do-load)
    ("-insert"	. command-line-do-insert)
    ("-i"	. command-line-do-insert)
    ("-kill"	. command-line-do-kill)
    ;; Options like +35 are handled specially.
    ;; Window-system, site, or package-specific code might add to this.
    ;; X11 handles its options by letting Xt remove args from this list.
    )
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

;;; default switches
;;; Note: these doc strings are semi-magical.

(defun command-line-do-help (arg)
  "Print the SXEmacs usage message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (concat "\n" (emacs-version) "\n\n"))
    (princ
     (if (featurep 'x)
	 (concat "When creating a window on an X display, "
		 (emacs-name)
		 " accepts all standard X Toolkit
command line options plus the following:
  -iconname <title>     Use title as the icon name.
  -mc <color>           Use color as the mouse color.
  -cr <color>           Use color as the text-cursor foregound color.
  -private              Install a private colormap.

In addition, the")
       "The"))
    (princ " following options are accepted:
  -sd                   Show dump ID.  Ignored when configured without --pdump.
  -nd                   Don't load the dump file.  Roughly like old temacs.
			Ignored when configured without --pdump.
  -t <device>           Use TTY <device> instead of the terminal for input
			and output.  This implies the -nw option.
  -nw                   Inhibit the use of any window-system-specific
			display code: use the current tty.
  -batch                Execute noninteractively (messages go to stderr).
  -debug-init           Enter the debugger if an error in the init file occurs.
  -unmapped             Do not map the initial frame.
  -no-site-file         Do not load the site-specific init file (site-start.el).
  -no-init-file         Do not load the user-specific init file.
  -no-early-packages	Do not process early packages.
  -no-autoloads		Do not load global symbol files (auto-autoloads) at
			startup.  Also implies `-vanilla'.
  -vanilla		Equivalent to -q -no-site-file -no-early-packages.
  -q                    Same as -no-init-file.
  -user-init-file <file> Use <file> as init file.
  -user-init-directory <directory> Use <directory> as init directory.
  -user-pkgs-directory <directory> Use <directory> as the top of the local (early)
                        packages tree.
  -user <user>          Load user's init file instead of your own.
                        Probably not a wise thing to do.
  -u <user>             Same as -user.\n")
   (let ((l command-switch-alist)
	  (insert (lambda (&rest x)
		    (princ "  ")
		    (let ((len 2))
		      (while x
			(princ (car x))
			(incf len (length (car x)))
			(setq x (cdr x)))
		      (when (>= len 24)
			(terpri) (setq len 0))
		      (while (< len 24)
			(princ " ")
			(incf len))))))
      (while l
	(let ((name (car (car l)))
	      (fn (cdr (car l)))
	      doc arg cons)
	  (cond
	   ((and (symbolp fn) (get fn 'undocumented)) nil)
	   (t
	    (setq doc (documentation fn))
	    (if (member doc '(nil "")) (setq doc "(undocumented)"))
	    (cond ((string-match "\n\\(<.*>\\)\n?\\'" doc)
		   ;; Doc of the form "The frobber switch\n<arg1> <arg2>"
		   (setq arg (substring doc (match-beginning 1) (match-end 1))
			 doc (substring doc 0 (match-beginning 0))))
		  ((string-match "\n+\\'" doc)
		   (setq doc (substring doc 0 (match-beginning 0)))))
	    (if (and (setq cons (rassq fn command-switch-alist))
		     (not (eq cons (car l))))
		(setq doc (format "Same as %s." (car cons))))
	    (if arg
		(funcall insert name " " arg)
	      (funcall insert name))
	    (princ doc)
	    (terpri))))
	(setq l (cdr l))))
    (princ (concat "+N <file>             Start displaying <file> at line N.

Anything else is considered a file name, and is placed into a buffer for
editing.

" (emacs-name) " has an online tutorial and manuals.  Type ^Ht (Control-h t) after
starting SXEmacs to run the tutorial.  Type ^Hi to enter the manual browser.
Type ^H^H^H (Control-h Control-h Control-h) to get more help options.\n")

    (kill-emacs 0))))

(defun command-line-do-funcall (arg)
  "Invoke the named lisp function with no arguments.
<function>"
  (funcall (intern (pop command-line-args-left))))
(fset 'command-line-do-funcall-1 'command-line-do-funcall)
(put 'command-line-do-funcall-1 'undocumented t)

(defun command-line-do-eval (arg)
  "Evaluate the lisp form.  Quote it carefully.
<form>"
  (eval (read (pop command-line-args-left))))

(defun command-line-do-load (arg)
  "Load the named file of Lisp code into SXEmacs.
<file>"
  (let ((file (pop command-line-args-left)))
    ;; Take file from default dir if it exists there;
    ;; otherwise let `load' search for it.
    (if (file-exists-p (expand-file-name file))
	(setq file (expand-file-name file)))
    (load file nil t)))

(defun command-line-do-insert (arg)
  "Insert file into the current buffer.
<file>"
  (insert-file-contents (pop command-line-args-left)))

(defun command-line-do-kill (arg)
  "Exit SXEmacs."
  (kill-emacs t))

(defun command-line-do-version (arg)
  "Print version info and exit."
  (princ (concat (emacs-version) "\n"))
  (kill-emacs 0))


;;; Processing the command line and loading various init files

(defun early-error-handler (&rest debugger-args)
  "You should probably not be using this."
  ;; Used as the debugger during SXEmacs initialization; if an error occurs,
  ;; print some diagnostics, and kill SXEmacs.

  ;; output the contents of the warning buffer, since it won't be seen
  ;; otherwise.
  ;; #### kludge!  The call to Feval forces the pending warnings to
  ;; get output.  There definitely needs to be a better way.
  (let ((buffer (eval (get-buffer-create "*Warnings*"))))
    (princ (buffer-substring (point-min buffer) (point-max buffer) buffer)
	   'external-debugging-output))

  (let ((string "Initialization error")
	(error (nth 1 debugger-args))
	(debug-on-error nil)
	(stream 'external-debugging-output))
    (if (null error)
	(princ string stream)
      (princ (concat "\n" string ": ") stream)
      (condition-case ()
	  (display-error error stream)
	(error (princ "<<< error printing error message >>>" stream)))
      (princ "\n" stream)
      (if (memq (car-safe error) '(void-function void-variable))
	  (princ "
	This probably means that SXEmacs is picking up an old version of
	the lisp library, or that some .elc files are not up-to-date.\n"
		 stream)))
    (when (not suppress-early-error-handler-backtrace)
      (let ((print-length 1000)
	    (print-level 1000)
	    (print-escape-newlines t)
	    (print-readably nil))
	(when (getenv "EMACSLOADPATH")
	  (princ (format "\n$EMACSLOADPATH is %s" (getenv "EMACSLOADPATH"))
		 stream))
	(princ (format "\nexec-directory is %S" exec-directory) stream)
	(princ (format "\ndata-directory is %S" data-directory) stream)
	(princ (format "\ndata-directory-list is %S" data-directory-list) stream)
	(princ (format "\ndoc-directory is %S" doc-directory) stream)
	(princ (format "\nload-path is %S" load-path) stream)
	(princ "\n\n" stream)))
    (when (not suppress-early-error-handler-backtrace)
      (backtrace stream t)))
  (kill-emacs -1))

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; Do this first for maximum likelihood of catching errors. The main
    ;; purpose of this is so that debug-on-error can be set to catch errors
    ;; during normal noninteractive running.
    (when (getenv "SXEMACSDEBUG")
      (eval (read (getenv "SXEMACSDEBUG"))))
    ;; Canonicalize HOME (PWD is canonicalized by init_buffer in buffer.c)
    (let ((value (user-home-directory)))
      (if (and value
	       (< (length value) (length default-directory))
	       (equal (file-attributes default-directory)
		      (file-attributes value)))
	  (setq default-directory (file-name-as-directory value))))
    (setq default-directory (abbreviate-file-name default-directory))
    (initialize-xemacs-paths)

    (startup-set-invocation-environment)

    (let ((debug-paths (or debug-paths
			   (and (getenv "EMACSDEBUGPATHS")
				t))))

      (setq emacs-roots (paths-find-emacs-roots invocation-directory
						invocation-name))

      (when debug-paths
	(princ (format "invocation: p:%S n:%S\n"
		       invocation-directory invocation-name)
	       'external-debugging-output)
	(princ (format "emacs-roots:\n%S\n" emacs-roots)
	       'external-debugging-output))

      (if (null emacs-roots)
	  (startup-find-roots-warning))
      (startup-setup-paths emacs-roots
			   user-init-directory
			   inhibit-early-packages
			   debug-paths)
      (startup-setup-paths-warning))

    ;; we set up a separate function, so that a user can later decide
    ;; to load the autoloads
    (startup-load-autoloads)

    (let (error-data)
      (condition-case data
	  (command-line)
	;; catch non-error signals, especially quit
	(t (setq error-data data)))
      ;; Do this again, in case the init file defined more abbreviations.
      (setq default-directory (abbreviate-file-name default-directory))
      ;; Specify the file for recording all the auto save files of
      ;; this session.  This is used by recover-session.
      (if auto-save-list-file-prefix
	  (setq auto-save-list-file-name
		(expand-file-name
		 (format "%s%d-%s"
			 auto-save-list-file-prefix
			 (emacs-pid)
			 (system-name)))))
      (run-hooks 'emacs-startup-hook)
      (and term-setup-hook
	   (run-hooks 'term-setup-hook))
      (setq term-setup-hook nil)
      ;;      ;; Modify the initial frame based on what the init file puts into
      ;;      ;; ...-frame-alist.
      (frame-notice-user-settings)
      ;;      ;;####FSFmacs junk
      ;;      ;; Now we know the user's default font, so add it to the menu.
      ;;      (if (fboundp 'font-menu-add-default)
      ;;	  (font-menu-add-default))
      (when window-setup-hook
	(run-hooks 'window-setup-hook))
      (setq window-setup-hook nil)
      (if error-data
	  ;; re-signal, and don't allow continuation as that will probably
	  ;; wipe out the user's .emacs if she hasn't migrated yet!
	  ;; Not applicable to SXEmacs --SY.
	  (signal-error (car error-data) (cdr error-data))))

    ;; FSF calls precompute-menubar-bindings.  We don't mix menubars
    ;; and keymaps.
    ))

(defvar forced-user-init-directory nil
  "Non-nil when `user-init-directory' is set on cmd line.

Internal variable, DO NOT USE.")

(defun command-line-early (args)
  ;; This processes those switches which need to be processed before
  ;; starting up the window system.

  (setq command-line-default-directory default-directory)

  ;; See if we should import version-control from the environment variable.
  (let ((vc (getenv "VERSION_CONTROL")))
    (cond ((eq vc nil))			;don't do anything if not set
	  ((or (string= vc "t")
	       (string= vc "numbered"))
	   (setq version-control t))
	  ((or (string= vc "nil")
	       (string= vc "existing"))
	   (setq version-control nil))
	  ((or (string= vc "never")
	       (string= vc "simple"))
	   (setq version-control 'never))))

  (setq load-user-init-file-p (not (noninteractive)))

  ;; Allow (at least) these arguments anywhere in the command line
  (let ((new-args nil)
	(arg      nil))
    (while args
      (setq arg (pop args))
      (cond
       ((or (string= arg "-q")
	    (string= arg "-no-init-file")
	    (string= arg "--no-init-file"))
	(setq load-user-init-file-p nil))
       ((or (string= arg "-no-site-file")
	    (string= arg "--no-site-file"))
	(setq site-start-file nil))
       ((or (string= arg "-no-early-packages")
	    (string= arg "--no-early-packages"))
	(setq inhibit-early-packages t))
       ((or (string= arg "-warn-early-package-shadows")
	    (string= arg "--warn-early-package-shadows"))
	(setq warn-early-package-shadows t))
       ((or (string= arg "-vanilla")
	    (string= arg "--vanilla")
	    ;; Some work on this one already done in emacs.c.
	    (string= arg "-no-autoloads")
	    (string= arg "--no-autoloads"))
	(setq load-user-init-file-p nil
	      site-start-file nil))
       ((or (string= arg "-user-init-file")
	    (string= arg "--user-init-file"))
	(setq user-init-file (pop args)))
       ((or (string= arg "-user-init-directory")
	    (string= arg "--user-init-directory"))
	(setq forced-user-init-directory t)
	(setq user-init-directory (file-name-as-directory (pop args)))
	(setq user-init-file (find-user-init-file user-init-directory))
	(setq custom-file (make-custom-file-name user-init-file))
	(startup-setup-paths emacs-roots
			     user-init-directory
			     inhibit-early-packages
			     debug-paths)
	(unless inhibit-early-packages
	  (unless inhibit-autoloads
	    (packages-load-package-auto-autoloads
	     early-package-load-path)))
	(setq lisp-initd-dir
	      (file-name-as-directory
	       (paths-construct-path (list user-init-directory
					   lisp-initd-basename)))))
       ((or (string= arg "-u")
	    (string= arg "-user")
	    (string= arg "--user"))
	(let* ((user (pop args))
	       (home-user (concat "~" user))
	       (xdgdir (paths-construct-path
			(list home-user ".config" "sxemacs")))
	       (xdgpdir (paths-construct-path
			 (list home-user ".local" "share" "sxemacs")))
	       (legacydir (paths-construct-path
			   (list home-user ".sxemacs")))
	       (dir-user (or (and (file-directory-p xdgdir)
				  (file-name-as-directory xdgdir))
			     (file-name-as-directory legacydir)))
	       (pdir-user (or (and (file-directory-p xdgpdir)
				   (file-name-as-directory xdgpdir))
			      (file-name-as-directory legacydir))))
	  (setq forced-user-init-directory t)
	  (setq user-init-directory dir-user)
	  (setq user-packages-topdir pdir-user)
	  (setq user-init-file
		(find-user-init-file user-init-directory))
	  (setq custom-file (make-custom-file-name user-init-file))
	  (startup-setup-paths emacs-roots
			       user-init-directory
			       inhibit-early-packages
			       debug-paths)
	  (unless inhibit-early-packages
	    (unless inhibit-autoloads
	      (packages-load-package-auto-autoloads
	       early-package-load-path)))
	  (setq lisp-initd-dir
		(file-name-as-directory
		 (paths-construct-path (list user-init-directory
					     lisp-initd-basename))))))
       ((or (string= arg "-debug-init")
	    (string= arg "--debug-init"))
	(setq init-file-debug t))
       ((or (string= arg "-unmapped")
	    (string= arg "--unmapped"))
	(setq initial-frame-unmapped-p t))
       ((or (string= arg "-debug-paths")
	    (string= arg "--debug-paths"))
	t)
       ((or (string= arg "--") (string= arg "-"))
	(while args
	  (push (pop args) new-args)))
       (t (push arg new-args))))

    ;; obsolete, initialize for backward compatibility
    (with-obsolete-variable 'init-file-user
      (setq init-file-user (and load-user-init-file-p "")))

    (if (and debug-paths forced-user-init-directory)
	(progn
	  (princ (format "user-init-directory:\n%S\n"
			 user-init-directory)
		 'external-debugging-output)
	  (princ (format "lisp-initd-dir:\n\%S\n" lisp-initd-dir)
		 'external-debugging-output)))

    (nreverse new-args)))

(defconst initial-scratch-message
  ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, first visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;;
;; In \"SXEmacs-speak\", `C-char' and `M-char' are abbreviations that mean
;; `Control+char' and `Meta+char' (hold down the Control or Meta key while
;; pressing `char').
;;
;; For Lisp evaluation, type an expression, move to the end and hit C-j.


"
  "Initial message displayed in *scratch* buffer at startup.
If this is nil, no message will be displayed.")

(defun command-line ()
  (let ((command-line-args-left (cdr command-line-args)))

    (let ((debugger 'early-error-handler)
	  (debug-on-error t))

      ;; Process magic command-line switches like -q and -u.  Do this
      ;; before creating the first frame because some of these switches
      ;; may affect that.  I think it's ok to do this before establishing
      ;; the X connection, and maybe someday things like -nw can be
      ;; handled here instead of down in C.
      (setq command-line-args-left (command-line-early command-line-args-left))

      ;; Setup the toolbar icon directory
      (when (featurep 'toolbar)
	(init-toolbar-location))

      ;; Run the window system's init function.  tty is considered to be
      ;; a type of window system for this purpose.  This creates the
      ;; initial (non stdio) device.
      (when (and initial-window-system (not noninteractive))
	(funcall (intern (concat "init-"
				 (symbol-name initial-window-system)
				 "-win"))))

      ;; When not in batch mode, this creates the first visible frame,
      ;; and deletes the stdio device.
      (frame-initialize))

    ;; Reinitialize faces if necessary.  This function changes face if
    ;; it is created during auto-autoloads loading.  Otherwise, it
    ;; does nothing.
    (startup-initialize-custom-faces)

    ;;
    ;; We have normality, I repeat, we have normality.  Anything you still
    ;; can't cope with is therefore your own problem.  (And we don't need
    ;; to kill SXEmacs for it.)
    ;;

    ;;; Load init files.
    (load-init-file)

    (with-current-buffer (get-buffer "*scratch*")
      (erase-buffer)
      ;; (insert initial-scratch-message)
      (set-buffer-modified-p nil)
      (when (eq major-mode 'fundamental-mode)
	(funcall initial-major-mode))
      (font-lock-set-defaults))

    ;; Load library for our terminal type.
    ;; User init file can set term-file-prefix to nil to prevent this.
    ;; Note that for any TTY's opened subsequently, the TTY init
    ;; code will run this.
    (when (and (eq 'tty (console-type))
	       (not (noninteractive)))
      (load-terminal-library))

    ;; Process the remaining args.
    (command-line-1)

    ;; it was turned on by default so that the warnings don't get displayed
    ;; until after the splash screen.
    (setq inhibit-warning-display nil)
    ;; If -batch, terminate after processing the command options.
    (when (noninteractive) (kill-emacs t))))

(defun load-terminal-library ()
  "Load the terminal initialization library"
  (when term-file-prefix
    (let ((term (getenv "TERM"))
	  hyphend colornum)
      (while (and term
		  (not (load (concat term-file-prefix term) t t)))
	;; Strip off last hyphen and what follows, then try again
	(if (setq colornum (string-match #r"[-+_][0-9]+\'" term))
	    (setq term (substring term 0 colornum))
	  (if (setq colornum (string-match #r"[-+_]\([0-9]+\)[^-_]+\'" term))
	      (setq term (replace-in-string term (match-string 1 term) ""))
	    (if (setq hyphend (string-match "[-+_][^-_]+\\'" term))
		(setq term (substring term 0 hyphend))
	      (setq term nil))))))))

(defun find-user-init-directory-init-file (&optional init-directory)
  "Determine the user's init file if in the init directory."
  (let ((init-directory (or init-directory user-init-directory)))
    (catch 'found
      (dolist (file user-init-file-base-list)
	(let ((expanded (expand-file-name file init-directory)))
	  (when (file-readable-p expanded)
	    (throw 'found expanded)))))))

(defun find-user-init-file (&optional init-directory)
  "Determine the user's init file."
  (find-user-init-directory-init-file init-directory))

(defun load-user-init-file ()
  "This function actually reads the init file."
  (unless (paths-file-readable-directory-p user-init-directory)
    (make-directory-path user-init-directory))
  (if (not user-init-file)
      (setq user-init-file
	    (find-user-init-file user-init-directory)))
  (if (not custom-file)
      (setq custom-file (make-custom-file-name user-init-file)))
  ;; #### should test load-user-init-file-p here, not in load-init-file
  ;; see comment there
  (if (and user-init-file
	   (file-readable-p user-init-file))
      (load user-init-file t t t))
  (if (and custom-file
	   (or (not user-init-file)
	       (not (string= custom-file user-init-file)))
	   (file-readable-p custom-file))
      (load custom-file t t t))
  (unless inhibit-default-init
    (let ((inhibit-startup-message nil))
      ;; Users are supposed to be told their rights.
      ;; (Plus how to get help and how to undo.)
      ;; Don't you dare turn this off for anyone except yourself.
      (load "default" t t))))

;;; Load user's init file and default ones.
(defun load-init-file ()

  (require 'const-aliases)

  (run-hooks 'before-init-hook)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before .emacs.  There is no point in doing this after
  ;; .emacs; that is useless.
  (when site-start-file
    (load site-start-file t t))

  ;; Sites should not disable this.  Only individuals should disable
  ;; the startup message.
  (setq inhibit-startup-message nil)

  (let (debug-on-error-from-init-file
	debug-on-error-should-be-set
	(debug-on-error-initial
	 (if (eq init-file-debug t) 'startup init-file-debug)))
    (let ((debug-on-error debug-on-error-initial))
      ;; #### I believe this test is incorrect, it leads to custom-file
      ;; (at least) being undefined
      (if (and load-user-init-file-p init-file-debug)
	  (progn
	    ;; Do this without a condition-case if the user wants to debug.
	    (load-user-init-file))
	(condition-case error
	    (progn
	      ;; #### probably incorrect, see comment above
	      (if load-user-init-file-p
		  (load-user-init-file))
	      (setq init-file-had-error nil))
	  (error
	   (message "Error in init file: %s" (error-message-string error))
	   (display-warning 'initialization
	     (format "An error has occurred while loading %s:

%s

To ensure normal operation, you should investigate the cause of the error
in your initialization file and remove it.  Use the `-debug-init' option
to SXEmacs to view a complete error backtrace."
		     user-init-file (error-message-string error))
	     'error)
	   (setq init-file-had-error t))))
      ;; If we can tell that the init file altered debug-on-error,
      ;; arrange to preserve the value that it set up.
      (or (eq debug-on-error debug-on-error-initial)
	  (setq debug-on-error-should-be-set t
		debug-on-error-from-init-file debug-on-error)))
    (when debug-on-error-should-be-set
      (setq debug-on-error debug-on-error-from-init-file)))

  (setq init-file-loaded t)

  ;; Do this here in case the init file sets mail-host-address.
  ;; Don't do this here unless noninteractive, it is frequently wrong. -sb
  ;; (or user-mail-address
  (when noninteractive
    (setq user-mail-address (concat (user-login-name) "@"
				    (or mail-host-address
					(system-name)))))

  (run-hooks 'after-init-hook)
  nil)

(defun load-options-file (filename)
  "Load the file of saved options (from the Options menu) called FILENAME.
Currently this does nothing but call `load', but it might be redefined
in the future to support automatically converting older options files to
a new format, when variables have changed, etc."
  (load filename))

(defun command-line-1 ()
  (cond
   ((null command-line-args-left)
    (unless noninteractive
      ;; If there are no switches to process, run the term-setup-hook
      ;; before displaying the copyright notice; there may be some need
      ;; to do it before doing any output.  If we're not going to
      ;; display a copyright notice (because other options are present)
      ;; then this is run after those options are processed.
      (run-hooks 'term-setup-hook)
      ;; Don't let the hook be run twice.
      (setq term-setup-hook nil)

      ;; Don't clobber a non-scratch buffer if init file
      ;; has selected it.
      (when (string= (buffer-name) "*scratch*")
	(unless (or inhibit-startup-message
		    (input-pending-p))
	  (let (tmout circ-tmout)
	    (unwind-protect
		;; Guts of with-timeout
		(catch 'tmout
		  (setq tmout (add-timeout startup-message-timeout
					   (lambda (ignore)
					     (condition-case nil
						 (throw 'tmout t)
					       (error nil)))
					   nil))
		  (setq circ-tmout (display-splash-frame))
		  (or nil;; (pos-visible-in-window-p (point-min))
		      (goto-char (point-min)))
		  (sit-for 0)
		  (setq unread-command-event (next-command-event)))
	      (when tmout (disable-timeout tmout))
	      (when circ-tmout (disable-timeout circ-tmout)))))
	(with-current-buffer (get-buffer "*scratch*")
	  ;; In case the SXEmacs server has already selected
	  ;; another buffer, erase the one our message is in.
	  (erase-buffer)
	  (when (stringp initial-scratch-message)
	    (insert initial-scratch-message))
	  (set-buffer-modified-p nil)))))

   (t
    ;; Command-line-options exist
    (let ((dir command-line-default-directory)
	  (file-count 0)
	  (line nil)
	  (end-of-options nil)
	  file-p arg tem)
      (while command-line-args-left
	(setq arg (pop command-line-args-left))
	(cond
	 (end-of-options
	  (setq file-p t))
	 ((setq tem (when (eq (aref arg 0) ?-)
		      (or (assoc arg command-switch-alist)
			  (assoc (substring arg 1)
				 command-switch-alist))))
	  (funcall (cdr tem) arg))
	 ((string-match #r"\`\+[0-9]+\'" arg)
	  (setq line (string-to-int arg)))
	 ;; "- file" means don't treat "file" as a switch
	 ;;  ("+0 file" has the same effect; "-" added
	 ;;   for unixoidiality).
	 ;; This is worthless; the `unixoid' way is "./file". -jwz
	 ((or (string= arg "-") (string= arg "--"))
	  (setq end-of-options t))
	 (t
	  (setq file-p t)))

	(when file-p
	  (setq file-p nil)
	  (incf file-count)
	  (setq arg (expand-file-name arg dir))
	  (cond
	   ((= file-count 1)
	    (find-file arg))
	   (noninteractive (find-file arg))
	   (t (find-file-other-window arg)))
	  (when line
	    (goto-line line)
	    (setq line nil))))))))

(defvar startup-presentation-hack-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'startup-presentation-hack-keymap)
    (define-key map '[button1] 'startup-presentation-hack)
    (define-key map '[button2] 'startup-presentation-hack)
    map)
  "Putting yesterday in the future tomorrow.")

(defun startup-presentation-hack ()
  (interactive)
  (let ((e last-command-event))
    (and (button-press-event-p e)
	 (setq e (extent-at (event-point e)
			    (event-buffer e)
			    'startup-presentation-hack))
	 (setq e (extent-property e 'startup-presentation-hack))
	 (if (consp e)
	     (apply (car e) (cdr e))
	   (while (keymapp (indirect-function e))
	     (let ((map e)
		   (overriding-local-map (indirect-function e)))
	       (setq e (read-key-sequence
			(let ((p (keymap-prompt map t)))
			  (cond ((symbolp map)
				 (if p
				     (format "%s %s " map p)
				   (format "%s " map)))
				(p)
				(t
				 (prin1-to-string map))))))
	       (if (and (button-release-event-p (elt e 0))
			(null (key-binding e)))
		   (setq e map)		; try again
		 (setq e (key-binding e)))))
	   (call-interactively e)))))

(defun startup-presentation-hack-help (e)
  (setq e (extent-property e 'startup-presentation-hack))
  (if (consp e)
      (format "Evaluate %S" e)
    (symbol-name e)))

(defun splash-frame-present-hack (e v)
  ;;   (set-extent-property e 'mouse-face 'highlight)
  ;;   (set-extent-property e 'keymap
  ;;		       startup-presentation-hack-keymap)
  ;;   (set-extent-property e 'startup-presentation-hack v)
  ;;   (set-extent-property e 'help-echo
  ;;		       'startup-presentation-hack-help)
  )

(defun splash-frame-present (l)
  (cond ((stringp l)
	 (insert l))
	((eq (car-safe l) 'face)
	 ;; (face name string)
	 (let ((p (point)))
	   (splash-frame-present (elt l 2))
	   (if (fboundp 'set-extent-face)
	       (set-extent-face (make-extent p (point))
				(elt l 1)))))
	((eq (car-safe l) 'key)
	 (let* ((c (elt l 1))
		(p (point))
		(k (where-is-internal c nil t)))
	   (insert (if k (key-description k)
		     (format "M-x %s" c)))
	   (if (fboundp 'set-extent-face)
	       (let ((e (make-extent p (point))))
		 (set-extent-face e 'bold)
		 (splash-frame-present-hack e c)))))
	((eq (car-safe l) 'funcall)
	 ;; (funcall (fun . args) string)
	 (let ((p (point)))
	   (splash-frame-present (elt l 2))
	   (if (fboundp 'set-extent-face)
	       (splash-frame-present-hack (make-extent p (point))
					  (elt l 1)))))
	((consp l)
	 (mapcar 'splash-frame-present l))
	(t
	 (backtrace 'external-debugging-output)
	 (error "WTF!?"))))

(defun startup-center-spaces (glyph)
  ;; Return the number of spaces to insert in order to center
  ;; the given glyph (may be a string or a pixmap).
  ;; Assume spaces are as wide as avg-pixwidth.
  ;; Won't be quite right for proportional fonts, but it's the best we can do.
  ;; Maybe the new redisplay will export something a glyph-width function.
  ;;; #### Yes, there is a glyph-width function but it isn't quite what
  ;;; #### this was expecting.  Or is it?
  ;; (An alternate way to get avg-pixwidth would be to use x-font-properties
  ;; and calculate RESOLUTION_X * AVERAGE_WIDTH / 722.7, but it's no better.)

  ;; This function is used in about.el too.
  (let* ((avg-pixwidth     (round (/ (frame-pixel-width) (frame-width))))
	 (fill-area-width  (* avg-pixwidth (- fill-column left-margin)))
	 (glyph-pixwidth   (cond ((stringp glyph)
				  (* avg-pixwidth (length glyph)))
				 ;; #### the pixmap option should be removed
				 ;;((pixmapp glyph)
				 ;; (pixmap-width glyph))
				 ((glyphp glyph)
				  (glyph-width glyph))
				 (t
				  (error "startup-center-spaces: bad arg")))))
    (+ left-margin
       (round (/ (/ (- fill-area-width glyph-pixwidth) 2) avg-pixwidth)))))

(defun splash-frame-body ()
  `[((face (blue bold underline)
	   "\nDistribution, copying license, warranty:\n\n")
     "Please visit the SXEmacs website at https://www.sxemacs.org !\n\n"
     ((key describe-no-warranty)
      ": "(face (red bold) "SXEmacs comes with ABSOLUTELY NO WARRANTY\n"))
     ((key describe-copying)
      ": conditions to give out copies of SXEmacs\n")
     ((key describe-distribution)
      ": how to get the latest version\n")
     ((face (blue bold underline) "\nOur Mission is:\n\n"))
     "
  - To provide the Open Source community with a text editing and
    development environment that is based on XEmacs and is 2nd to
    none in regards to stability, features, and innovation.

  - To foster a user and developer friendly project environment.

  - And, above all, to have fun doing it.\n"
     "\n--\n"
     (face italic "
Copyright (C) 2004 - 2015 Steve Youngs\n"))
; Copyright (C) 1985-2001 Free Software Foundation, Inc.
; Copyright (C) 1990-1994 Lucid, Inc.
; Copyright (C) 1993-1997 Sun Microsystems, Inc. All Rights Reserved.
; Copyright (C) 1994-1996 Board of Trustees, University of Illinois
; Copyright (C) 1995-2001 Ben Wing\n"))

    ((face (blue bold underline) "\nInformation, on-line help:\n\n")
     "SXEmacs comes with plenty of documentation...\n\n"
     ,@(if emacs-beta-version
	   `((key describe-beta)
	     ": " (face (red bold)
			"This is an Experimental version of SXEmacs.\n"))
	 `( "\n"))
     ((key sxemacs-local-faq)
      ": read the SXEmacs FAQ (a " (face underline "capital") " F!)\n")
     ((key help-with-tutorial)
      ": read the SXEmacs tutorial (also available through the "
      (face bold "Help") " menu)\n")
     ((key help-command)
      ": get help on using SXEmacs (also available through the "
      (face bold "Help") " menu)\n")
     ((key info) ": read the on-line documentation\n\n")
     ((key about-sxemacs) ": see who's developing SXEmacs\n"))

    ((face (blue bold underline) "\nUseful stuff:\n\n")
     "Things that you should learn rather quickly...\n\n"
     ((key find-file) ": visit a file\n")
     ((key save-buffer) ": save changes\n")
     ((key advertised-undo) ": undo changes\n")
     ((key save-buffers-kill-emacs) ": exit SXEmacs\n"))
    ])

;; I really hate global variables, oh well.
;(defvar xemacs-startup-logo-function nil
;  "If non-nil, function called to provide the startup logo.
;This function should return an initialized glyph if it is used.")

;; This will hopefully go away when gettext is functional.
(defconst splash-frame-static-body
  `(,(emacs-version) "\n\n"
    (face italic "`C-' means the control key,`M-' means the meta key\n\n")))


(defun circulate-splash-frame-elements (client-data)
  (with-current-buffer (aref client-data 2)
    (let ((buffer-read-only nil)
	  (elements (aref client-data 3))
	  (indice (aref client-data 0)))
      (goto-char (aref client-data 1))
      (delete-region (point) (point-max))
      (splash-frame-present (aref elements indice))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (aset client-data 0
	    (if (= indice (1- (length elements)))
		0
	      (1+ indice)))
      )))

;; #### This function now returns the (possibly nil) timeout circulating the
;; splash-frame elements
(defun display-splash-frame ()
  (let ((logo sxemacs-logo)
	(buffer-read-only nil)
	(cramped-p (eq 'tty (console-type))))
    (unless cramped-p (insert "\n"))
    (indent-to (startup-center-spaces logo))
    (set-extent-begin-glyph (make-extent (point) (point)) logo)
    ;;(splash-frame-present-hack (make-extent p (point)) 'about-sxemacs))
    (insert "\n\n")
    (splash-frame-present splash-frame-static-body)
    (goto-char (point-max))
    (let* ((after-change-functions nil) ; no font-lock, thank you
	   (elements (splash-frame-body))
	   (client-data `[ 1 ,(point) ,(current-buffer) ,elements ])
	   tmout)
      (if (listp  elements) ;; A single element to display
	  (splash-frame-present (splash-frame-body))
	;; several elements to rotate
	(splash-frame-present (aref elements 0))
	(setq tmout (add-timeout splash-frame-timeout
				 'circulate-splash-frame-elements
				 client-data splash-frame-timeout)))
      (set-buffer-modified-p nil)
      tmout)))

;;  (let ((present-file
;;         #'(lambda (f)
;;             (splash-frame-present
;;	      (list 'funcall
;;		    (list 'find-file-other-window
;;			  (expand-file-name f data-directory))
;;		    f)))))
;;    (insert "For customization examples, see the files ")
;;    (funcall present-file "sample.emacs")
;;    (insert " and ")
;;    (funcall present-file "sample.Xdefaults")
;;    (insert (format "\nin the directory %s." data-directory)))

(defun startup-set-invocation-environment ()
  ;; XEmacs -- Steven Baur says invocation directory is nil if you
  ;; try to use XEmacs as a login shell.
  (or invocation-directory (setq invocation-directory default-directory))
  (setq invocation-directory
	;; don't let /tmp_mnt/... get into the load-path or exec-path.
	(abbreviate-file-name invocation-directory)))

(defvar Info-directory-list)

(defun startup-setup-paths (roots userdir
				  &optional
				  inhibit-early-packages debug-paths)
  "Setup all the various paths.
ROOTS is a list of plausible roots of the SXEmacs directory hierarchy.
USERDIR is the user's init directory, possibly computed.
If INHIBIT-EARLY-PACKAGES is non-NIL, don't do user packages.
If DEBUG-PATHS is non-NIL, print paths as they are detected.  It's
idempotent, so call this as often as you like!"
  ;; Maybe find a good candidate for user-init-directory, but only if
  ;; SXEmacs was started without any command line arg that would set
  ;; or change it.
  (let ((allargs command-line-args)
	(initdargs '("-u" "-user" "--user" "-user-init-directory"
		     "--user-init-directory")))
    (unless
	(or (member t (mapfam #'string= initdargs allargs :mode 'comb))
	    forced-user-init-directory)
      (find-user-init-directory)
      (setq userdir user-init-directory)
      (setq lisp-initd-dir (file-name-as-directory
			    (paths-construct-path
			     (list userdir lisp-initd-basename))))
      (if debug-paths
	  (progn
	    (princ (format "user-init-directory:\n%S\n"
			   user-init-directory)
		   'external-debugging-output)
	    (princ (format "lisp-initd-dir:\n\%S\n" lisp-initd-dir)
		   'external-debugging-output)))))
  ;; Packages
  (setq user-packages-topdir (packages-find-user-topdir))
  (when debug-paths
    (princ (format "user-packages-topdir: \n%S\n"
		   user-packages-topdir)
	   'external-debugging-output))
  (apply #'(lambda (early late last)
	     (setq early-packages (and (not inhibit-early-packages)
				       early))
	     (setq late-packages late)
	     (setq last-packages last))
	 (packages-find-packages
	  roots
	  (packages-compute-package-locations user-packages-topdir)))

  (setq early-package-load-path
	(packages-find-package-load-path early-packages)
	late-package-load-path
	(packages-find-package-load-path late-packages)
	last-package-load-path
	(packages-find-package-load-path last-packages))

  (if debug-paths
      (progn
	(princ (format "configure-package-path:\n%S\n" configure-package-path)
	       'external-debugging-output)
	(princ (format "early-packages and early-package-load-path:\n%S\n%S\n"
		       early-packages early-package-load-path)
	       'external-debugging-output)
	(princ (format "late-packages and late-package-load-path:\n%S\n%S\n"
		       late-packages late-package-load-path)
	       'external-debugging-output)
	(princ (format "last-packages and last-package-load-path:\n%S\n%S\n"
		       last-packages last-package-load-path)
	       'external-debugging-output)))
  ;; Core lisp
  (setq lisp-directory (paths-find-lisp-directory roots))

  (if debug-paths
      (princ (format "lisp-directory:\n%S\n" lisp-directory)
	     'external-debugging-output))
  ;; mule (core)
  (if (featurep 'mule)
      (progn
	(setq mule-lisp-directory
	      (paths-find-mule-lisp-directory roots
					      lisp-directory))
	(if debug-paths
	    (princ (format "mule-lisp-directory:\n%S\n"
			   mule-lisp-directory)
		   'external-debugging-output)))
    (setq mule-lisp-directory '()))
  ;; FFI
  (setq ffi-lisp-directory
	(when (fboundp #'ffi-defun)
	  (paths-find-ffi-lisp-directory roots
					 lisp-directory)))

  ;; Modules
  (setq module-directory (paths-find-module-directory roots))
  (if debug-paths
      (princ (format "module-directory:\n%S\n" module-directory)
	     'external-debugging-output))
  (setq site-module-directory (and (null inhibit-site-modules)
				   (paths-find-site-module-directory roots)))
  (if (and debug-paths (null inhibit-site-modules))
      (princ (format "site-module-directory:\n%S\n" site-module-directory)
	     'external-debugging-output))

  (setq load-path (paths-construct-load-path roots
					     early-package-load-path
					     late-package-load-path
					     last-package-load-path
					     lisp-directory
					     nil
					     mule-lisp-directory
					     ffi-lisp-directory))
  ;; Info
  (setq Info-directory-list
	(paths-construct-info-path roots
				   early-packages late-packages last-packages))


  (if debug-paths
      (princ (format "Info-directory-list:\n%S\n" Info-directory-list)
	     'external-debugging-output))

  (setq exec-directory (paths-find-exec-directory roots))

  (if debug-paths
      (princ (format "exec-directory:\n%s\n" exec-directory)
	     'external-debugging-output))
  ;; Exec
  (setq exec-path
	(paths-construct-exec-path roots exec-directory
				   early-packages late-packages last-packages))

  (if debug-paths
      (princ (format "exec-path:\n%S\n" exec-path)
	     'external-debugging-output))
  ;; Doc
  (setq doc-directory (paths-find-doc-directory roots))

  (if debug-paths
      (princ (format "doc-directory:\n%S\n" doc-directory)
	     'external-debugging-output))
  ;; Data
  (setq data-directory (paths-find-data-directory roots))

  (if debug-paths
      (princ (format "data-directory:\n%S\n" data-directory)
	     'external-debugging-output))

  (setq data-directory-list
	(paths-construct-data-directory-list data-directory
					     early-packages
					     late-packages
					     last-packages))
  (if debug-paths
      (princ (format "data-directory-list:\n%S\n" data-directory-list)
	     'external-debugging-output)))

(defun startup-find-roots-warning ()
  (save-excursion
    (set-buffer (get-buffer-create " *warning-tmp*"))
    (erase-buffer)
    (buffer-disable-undo (current-buffer))

    (insert "Couldn't find an obvious default for the root of the\n"
	    "SXEmacs hierarchy.")

    (princ "\nWARNING:\n" 'external-debugging-output)
    (princ (buffer-string) 'external-debugging-output)))

(defun startup-setup-paths-warning ()
  (let ((warnings '()))
    (cond
     ((null (and lisp-directory exec-directory data-directory doc-directory
		 load-path))
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(if (null lisp-directory) (push "lisp-directory" warnings))
	(if (and (featurep 'mule)
		 (null mule-lisp-directory))
	    (push "mule-lisp-directory" warnings))
	(if (null exec-directory) (push "exec-directory" warnings))
	(if (null data-directory) (push "data-directory" warnings))
	(if (null doc-directory)  (push "doc-directory"  warnings))
	(if (null load-path)      (push "load-path"      warnings))

	(insert "Couldn't find obvious defaults for:\n")
	(while warnings
	  (insert (car warnings) "\n")
	  (setq warnings (cdr warnings)))
	(insert "Perhaps some directories don't exist, "
		"or the SXEmacs executable,\n"
		(concat invocation-directory invocation-name)
		"\nis in a strange place?")

	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)
	(erase-buffer)
	t)))))


(defun startup-load-autoloads ()
  "Load autoloads from known locations."

  (when (and (not inhibit-autoloads)
	     (or lisp-directory module-directory))
    ;; ordinary auto-autoloads in lisp/
    (let ((aalfile (file-name-sans-extension autoload-file-name)))

      (condition-case nil
	  (load (expand-file-name aalfile lisp-directory) nil t)
	(error . nil))

      ;; just load them all
      (mapc-internal
       #'(lambda (root)
	   (condition-case nil
	       (load (expand-file-name (concat "lisp/" aalfile) root) nil t)
	     (error . nil)))
       emacs-roots)

      (when (featurep 'mule)
	(load (expand-file-name aalfile
				(expand-file-name "mule" lisp-directory))
	      t t))
      (when (featurep 'modules)
	(load (expand-file-name aalfile module-directory) t t))

      (when (fboundp #'ffi-defun)
	(load (expand-file-name aalfile
				(expand-file-name "ffi" lisp-directory))
	      t t))))

  (unless inhibit-autoloads
    (unless inhibit-early-packages
      (packages-load-package-auto-autoloads early-package-load-path))
    (if (and (not inhibit-early-packages) (not warn-early-package-shadows))
	(let ((early-path (mapcar 'file-basename early-package-load-path))
	      late-load-path)
	  (mapc (lambda (path)
		  (unless (member (file-basename path) early-path)
		    (setq late-load-path (append late-load-path (list path)))))
		late-package-load-path)
	  (packages-load-package-auto-autoloads late-load-path))
    (packages-load-package-auto-autoloads late-package-load-path))
    (packages-load-package-auto-autoloads last-package-load-path)))

;;; startup.el ends here
