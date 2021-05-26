;; ffi-magic.el --- SXEmacs interface to libmagic   -*- mode: emacs-lisp -*-

;; Copyright (C) 2008 - 2020 Steve Youngs

;; Author:     Steve Youngs <steve@sxemacs.org>
;; Maintainer: Steve Youngs <steve@sxemacs.org>
;; Created:    <2008-04-02>
;; Homepage:   https://www.sxemacs.org
;; Keywords:   ffi, file, magic, extension

;; This file is part of SXEmacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;;    (magic:file (expand-file-name "about.el" lisp-directory))
;;     => "Lisp/Scheme program, ISO-8859 text"
;;
;;    That's the vanilla use, however there's more!
;;    See `C-h f magic:file RET'.
;;
;;    If you'd like to use magic for coding system detection for
;;    #'find-file, put...
;;
;;        (magic:find-file-magic-alist-enable)
;;
;;    in your init.el.  You _might_ not want that though because you'd
;;    be surprised at how little it takes for libmagic to believe a
;;    file is binary.  A single character can trigger it.  To see for
;;    yourself, visit a file in SXEmacs that you know file(1) reports
;;    as normal text and do: `C-u M-: (format "%c" #o177)', save
;;    it, and go see what file(1) reports now.
;;    ---
;;    Steps are now taken to guard against this sort of thing so it
;;    should be quite safe to enable this.
;;
;;
;;    Other useful bits:
;;
;;        #'magic:file-audio-p
;;        #'magic:file-video-p
;;        #'magic:file-image-p
;;        #'magic:file-text-p
;;

;;; Todo:  <<< ffi-magic notes
;;
;;    o Optionally output MIME type strings like "text/plain",
;;      "applicaton/octet-stream"
;;      Ah, yep, can do that now, as of 2020-02-23
;;
;;    o  magic_getparam(), magic_setparam(), magic_list(), magic_compile(),
;;       magic_check() are all to be added.
;;

;;; Code:
(require 'ffi)
(require 'ffi-libc)

;; Can't do anything without this
(ffi-load "libmagic")

(defvar ffi-magic-shared nil
  "Shared context with preloaded magic file, to speed up things.")

;;;###autoload(put 'ffi-magic-persistent-flags 'risky-local-variable t)
(defvar ffi-magic-persistent-flags '(:error)
  "A list of libmagic option flags to always set.

A good thing to keep on this list is ':error', the default. It will
ensure that any errors, from either libmagic or the shell, will be
captured in, and thus reportable via, `magic:error'.

If you were to get an error from libmagic \(or the shell\) and you
_didn't_ have the ':error' flag set, one of two things would happen
depending on the version of libmagic you have. Either it'd be lost to
stderr, or it'd hit stdout but wouldn't be an error from our POV.  In
both cases `magic:error' would return nil.

See `ffi-magic-options-list' for what can be included here.")


(unless (ffi-find-named-type 'magic_t)
  (define-ffi-type magic_t pointer))

(unless (ffi-find-named-type 'magic-options)
  (define-ffi-enum magic-options
    (:none              #x0000000)   ; No flags
    (:debug             #x0000001)   ; Turn on debugging
    (:symlink           #x0000002)   ; Follow symlinks
    (:compress          #x0000004)   ; Check inside compressed files
    (:devices           #x0000008)   ; Look at contents of devices
    (:mime-type         #x0000010)   ; Return the MIME type
    (:continue          #x0000020)   ; Return all matches
    (:check             #x0000040)   ; Print warnings to stderr
    (:preserve-atime    #x0000080)   ; Restore access time on exit
    (:raw               #x0000100)   ; Don't translate unprintable chars
    (:error             #x0000200)   ; Handle ENOENT etc as real errors
    (:mime-encoding     #x0000400)   ; Return MIME encoding
    (:mime              #x0000410)   ; :mime-type + :mime-encoding
    (:apple             #x0000800)   ; Return Apple creator and type
    (:extension         #x1000000)   ; Return a / separated list of
				     ; extensions
    (:compress-transp   #x2000000)   ; Check inside compressed files but
				     ; not report compression
    (:nodesc            #x1001210)   ; :extension + :mime + :apple
    (:no-check-compress #x0001000)   ; Don't check for compressed files
    (:no-check-tar      #x0002000)   ; Don't check for tar files
    (:no-check-soft     #x0004000)   ; Don't check magic entries
    (:no-check-apptype  #x0008000)   ; Don't check application type
    (:no-check-elf      #x0010000)   ; Don't check for elf details
    (:no-check-text     #x0020000)   ; Don't check for text files
    (:no-check-cdf      #x0040000)   ; Don't check for cdf files
    (:no-check-csv      #x0080000)   ; Don't check for CSV files
    (:no-check-tokens   #x0100000)   ; Don't check tokens
    (:no-check-encoding #x0200000)   ; Don't check for text encodings
    (:no-check-json     #x0400000))) ; Don't check for JSON files

(defvar ffi-magic-options-list
  (mapfam #'car :mode 'keyw (ffi-enum-values 'magic-options))
  "List of possible option flags.

These flags are used to influence the results of querying the magic
db.  See below for a few little \"quirks\".

Allowable flags are:

   :none               No flags
   :debug              Turn on debugging
   :symlink            Follow symlinks
   :compress           Check inside compressed files
   :devices            Look at contents of devices
   :mime-type          Return the MIME type
   :continue           Return all matches
   :check              Print warnings to stderr
   :preserve-atime     Restore access time on exit
   :raw                Don't translate unprintable chars
   :error              Handle ENOENT etc as real errors
   :mime-encoding      Return MIME encoding
   :mime               Alias for :mime-type + :mime-encoding
   :apple              Return Apple creator and type
   :extension          Return a / separated list of
                       extensions
   :compress-transp    Check inside compressed files but
                       not report compression
   :nodesc             :extension + :mime + :apple
   :no-check-compress  Don't check for compressed files
   :no-check-tar       Don't check for tar files
   :no-check-soft      Don't check magic entries
   :no-check-apptype   Don't check application type
   :no-check-elf       Don't check for elf details
   :no-check-text      Don't check for text files
   :no-check-cdf       Don't check for cdf files
   :no-check-csv       Don't check for CSV files
   :no-check-tokens    Don't check tokens
   :no-check-encoding  Don't check for text encodings
   :no-check-json      Don't check for JSON files

A couple of points to note:

  If you want to pretend that you didn't access the files you just
  accessed you can use ':preserve-atime'.  Be aware that it only works
  on systems that support utime(3) or utimes(2).

  Using ':none' effectively turns off all flags so setting this along
  with any other flag is pointless.  One possible good reason to use
  ':none' is if you want to override `ffi-magic-persistent-flags'.  Be
  aware though, that in our eyes, none means none.  If you use ':none'
  that is what you'll get... no flags.

  The ':mime' flag being an alias for ':mime-type :mime-encoding'
  means that if you use it, you don't need either of the other two.

  When using ':debug' it is also good to add ':error' and ':check' as
  well.  Oh, and you've just lost all of your output if you didn't
  remember to redirect stderr.

  Trying to do things like use ':no-check-encoding' with ':mime-encoding'
  at the same time is... well... I'm sure you can see the problem.

  Using ':apple' only makes sense for apple mac created files.  If
  you're not using a Mac, you probably don't have any.  Sure, you can
  still use this flag on any file, arch, or iron, but non-mac files will
  return a ever-so-enlightening \"UNKNUNKN\".  Have fun with that.

  ':continue' will output literal \\012 instead of linefeed so you need
  to add ':raw' to get actual linefeeds.

  I have absolutely no idea what ':nodesc' might return \(I don't
  have a Mac, and haven't ever encountered any of these \"extensions\"
  libmagic talks of\), so... YMMV and all that.")

(define-ffi-function magic-open (flag)
  "Call libmagic's magic_open()."
  '(function magic_t int)
  "magic_open")

(define-ffi-function magic-close (magic)
  "Call libmagic's magic_close()."
  '(function void magic_t)
  "magic_close")

(define-ffi-function magic-load (magic magicfile)
  "Call libmagic's magic_load()."
  '(function int magic_t c-string)
  "magic_load")

(define-ffi-function magic-list (magic magicfile)
  "Call libmagic's magic_list()."
  '(function int magic_t c-string)
  "magic_list")

(define-ffi-function magic-file (magic file)
  "Call libmagic's magic_file()."
  '(function safe-string magic_t c-string)
  "magic_file")

(define-ffi-function magic-error (magic)
  "Call libmagic's magic_error()."
  '(function safe-string magic_t)
  "magic_error")

(define-ffi-function magic-errno (magic)
  "Call libmagic's magic_errno()."
  '(function int magic_t)
  "magic_errno")

(define-ffi-function magic-getflags (magic)
  "Call libmagic's magic_setflags()."
  '(function int magic_t)
  "magic_getflags")

(define-ffi-function magic-setflags (magic flag)
  "Call libmagic's magic_setflags()."
  '(function int magic_t int)
  "magic_setflags")

(define-ffi-function magic-version ()
  "Call libmagic's magic_version()."
  '(function int)
  "magic_version")

(defconst magic:version (magic-version)
  "libmagic version")

(defun magic:version (&optional test ver)
  "Return libmagic version number \(integer\).

With optional argument TEST return t if libmagic version is
\(=, <, <=, >, >=\) VER, depending on what TEST.

TEST can be one of the following symbols:

    'e'   Equal to
    'lt'  Less than
    'lte' Less than or equal to
    'gt'  Greater than
    'gte' Greater than or equal to

VER can be either an int or a float.  The reason is that file\(1\)
reports its version as \"5.38\", for example, but the underlying
libmagic of the same version reports as \"538\".  And because I'm
such a nice guy and always thinking of you, I cater for both."
  (if test
      (case test
	(e (if (floatp ver)
	       (= (/ magic:version 100.0) ver)
	     (= magic:version ver)))
	(lt (if (floatp ver)
		(< (/ magic:version 100.0) ver)
	      (< magic:version ver)))
	(lte (if (floatp ver)
		 (<= (/ magic:version 100.0) ver)
	       (<= magic:version ver)))
	(gt (if (floatp ver)
		(> (/ magic:version 100.0) ver)
	      (> magic:version ver)))
	(gte (if (floatp ver)
		 (>= (/ magic:version 100.0) ver)
	       (>= magic:version ver)))
	(otherwise (error 'invalid-argument test)))
    magic:version))

(define-ffi-function magic-descriptor (magic fd)
  "Call libmagic's magic_descriptor()."
  '(function safe-string magic_t int)
  "magic_descriptor")

(defun magic:getflags (magic)
  (magic-getflags magic))

;;;###autoload(put 'ffi-magic-no-safety 'risky-local-variable t)
(defvar ffi-magic-no-safety nil
  "Set to non-nil if you DO NOT want your option flags sanitised.")

(defun ffi-magic-sanitise-flags (flags)
  "Do our best to not let the user shoot themselves in the foot.

Argument FLAGS is the list of options given to `magic:file' to influence
its output.

There are a few cases where using some of those flags is bad or
downright crazy.  This function tries to help you by taking a few
rudimentary precautions:

  Don't have _any_ flags if you want :none
  Don't have any duplicates
  Don't have any superfluous flags \(if you have :mime, you don't need
    :mime-type or :mime-encoding\)
  Don't include :mime-encoding if :no-check-encoding is present
  Don't have any unknown flags.
  Don't let you :debug without giving you :error and :check.
  Don't over cook :continue, serve it :raw.

However, if you really really want to, you can bypass this sanitation
completely by setting `ffi-magic-no-safety' non-nil.  "
  (let ((cleanflags (copy-sequence flags)))
    (unless ffi-magic-no-safety
      ;; Did you say ":none"? OK, no flags for you!
      (and (memq ':none cleanflags) (setq cleanflags nil))
      (when cleanflags
	;; Dups
	(setq cleanflags (remove-duplicates cleanflags))
	;; Too much MIME on our hands
	(when (memq ':mime cleanflags)
	  (delq ':mime-type cleanflags)
	  (delq ':mime-encoding cleanflags))
	;; Want encoding, don't wanna check for encoding. How's that
	;; supposed to work, Sparky?
	(when (memq ':no-check-encoding cleanflags)
	  (delq ':mime-encoding cleanflags))
	;; I've never heard of that flag
	(mapc
	 #'(lambda (elt)
	     (and (not (memq elt ffi-magic-options-list))
		  (delq elt cleanflags)))
	 cleanflags)
	;; The :debug tastes better when served with :error and :check
	(when (memq ':debug cleanflags)
	  (add-to-list 'cleanflags ':error)
	  (add-to-list 'cleanflags ':check))
	;; :continue is best served :raw
	(when (memq ':continue cleanflags)
	  (add-to-list 'cleanflags ':raw)))
      ;; Warn about any changes
      (unless (eq (length cleanflags) (length flags))
	(lwarn 'magic-flags 'warning
	  "*** Your libmagic option flags have been altered! ***

Flags you asked for: %s
      Flags you got: %s

See `display-warning-suppressed-classes' to suppress this warning.
See `ffi-magic-sanitise-flags' and `ffi-magic-options-list' to see why
you got it in the first place."
	  flags cleanflags)))
    ;; Return the new and improved squeaky-clean flags
    cleanflags))

(defun ffi-magic-flag-value (flag)
  "Return numeric value of FLAG."
  (cdr (assq flag (ffi-enum-values 'magic-options))))

(defun magic:setflags (magic &rest flags)
  (let ((f 0)
	(flags (ffi-magic-sanitise-flags (car flags))))
    ;; Remove any previously set flags
    (magic-setflags magic 0)
    ;; Set flags if any survived sanitation
    (when flags
      (mapc
       #'(lambda (flg)
	   (setq f (+ f (ffi-magic-flag-value flg))))
       flags)
      (magic-setflags magic f))))

(globally-declare-fboundp 'completing-read-multiple)
(globally-declare-boundp 'crm-separator)
;;;###autoload
(defun magic:file (file &rest flags)
  "Return as a string information about FILE using libmagic.

FLAGS are optional keys for determining the type of output required.
Interactively, they can be set via prefix arg.

A second prefix arg will insert the result into the current buffer at
point.

The supported flags are:

   :none               No flags
   :debug              Turn on debugging
   :symlink            Follow symlinks
   :compress           Check inside compressed files
   :devices            Look at contents of devices
   :mime-type          Return the MIME type
   :continue           Return all matches
   :check              Print warnings to stderr
   :preserve-atime     Restore access time on exit
   :raw                Don't translate unprintable chars
   :error              Handle ENOENT etc as real errors
   :mime-encoding      Return MIME encoding
   :mime               Alias for :mime-type + :mime-encoding
   :apple              Return Apple creator and type
   :extension          Return a / separated list of
                       extensions
   :compress-transp    Check inside compressed files but
                       not report compression
   :nodesc             :extension + :mime + :apple
   :no-check-compress  Don't check for compressed files
   :no-check-tar       Don't check for tar files
   :no-check-soft      Don't check magic entries
   :no-check-apptype   Don't check application type
   :no-check-elf       Don't check for elf details
   :no-check-text      Don't check for text files
   :no-check-cdf       Don't check for cdf files
   :no-check-csv       Don't check for CSV files
   :no-check-tokens    Don't check tokens
   :no-check-encoding  Don't check for text encodings
   :no-check-json      Don't check for JSON files

Example usage:

    \(magic:file \(expand-file-name \"about.el\" lisp-directory\)\)
     => \"Lisp/Scheme program, ISO-8859 text\"

    \(magic:file \(expand-file-name \"about.el\" lisp-directory\)
                 :mime-type\)
     => \"text/x-lisp\"

    \(magic:file \(expand-file-name \"about.el\" lisp-directory\)
                 :mime-encoding\)
     => \"iso-8859-1\"

    \(magic:file \(expand-file-name \"about.el\" lisp-directory\)
                 :mime-encoding
                 :mime-type\)
     => \"text/x-lisp; charset=iso-8859-1\"

    \(magic:file \"~/tmp/foo\"\)
     => \"symbolic link to `a-dummy-file.txt'\"

    \(magic:file \"~/tmp/foo\" :symlink\)
     => \"UTF-8 Unicode text\""
  (interactive
   (list
    (read-file-name "File name: " default-directory nil t)
    (when current-prefix-arg
      (if (featurep 'crm)
	  (progn
	    (setq flags
		  (completing-read-multiple
		   (format "Magic options ('%s' separated): " crm-separator)
		   (mapfam #'list
			   (mapfam #'symbol-name ffi-magic-options-list))))
	    (if (string-equal (car flags) "")
		(setq flags nil)
	      (setq flags (car (mapfam #'intern flags)))))
	(error 'unimplemented 'crm
	       ;; Bogus, yes, but crm isn't currently listed as a
	       ;; provide in the edit-utils pkg
	       (package-get-package-provider 'avoid))))))
  (unless ffi-magic-shared
    (setq ffi-magic-shared (magic-open 0))
    (magic-load ffi-magic-shared (ffi-null-pointer)))
  (when ffi-magic-persistent-flags
    (setq flags (append ffi-magic-persistent-flags flags))
    (delq nil flags))
  (and flags (magic:setflags ffi-magic-shared flags))
  (unwind-protect
      (let ((ftype (magic-file ffi-magic-shared
			       (expand-file-name file))))
	(if (interactive-p)
	    (if (eq (car current-prefix-arg) 16)
		(insert ftype)
	      (message ftype))
	  ftype))
    (let ((emsg (magic:error ffi-magic-shared)))
      (and emsg (error 'ffi-magic-error emsg)))))

(define-obsolete-function-alias #'magic:file-type #'magic:file)

(defun magic:cleanup ()
  "Ensure that the magic file is closed on SXEmacs exit.
Called from `kill-emacs-hook'."
  (when ffi-magic-shared
    (magic-close ffi-magic-shared)
    (setq ffi-magic-shared nil)))

(add-hook 'kill-emacs-hook #'magic:cleanup)

(define-error 'ffi-magic-error "%S" 'file-error)

(defun magic:error (magic)
  "Return string of errors/warnings from libmagic.

Argument MAGIC is the magic db, normally `ffi-magic-shared'.

This is designed to be used as an argument to `error', `warn', and the
like.  There is a corresponding error datum symbol, `ffi-magic-error'.

Example:

    \(or \(magic:file \"/path/to/file\" :error\)
        \(error 'ffi-magic-error \(magic:error ffi-magic-shared\)\)\)

Note: For `magic:error' to return anything other than nil the libmagic
option flag, `:error' must be set.  See `ffi-magic-persistent-flags'."
  (let ((estr (magic-error magic))
	(enum (magic-errno magic))
	(sh shell-file-name))
    (if (not (zerop enum))
	(format "[%s RC:%d]: %s" sh enum estr)
      (and estr (format "[libmagic]: %s" estr)))))

;;;###autoload
(defun magic:file-audio-p (file)
  "Return non-nil if FILE is an audio file.

As reported by libmagic. If so, the audio type is returned."
  (let ((type (magic:file file :mime-type)))
    (when (string-match #r"^audio/\(.*$\)" type)
      (substring type (match-beginning 1)))))

;;;###autoload
(defun magic:file-video-p (file)
  "Return non-nil if FILE is a video file.

As reported by libmagic. If so, the video type is returned."
  (let ((type (magic:file file :mime-type)))
    (when (string-match #r"^video/\(.*$\)" type)
      (substring type (match-beginning 1)))))

;;;###autoload
(defun magic:file-image-p (file)
  "Return non-nil if FILE is an image file.

As reported by libmagic. If so, the image type is returned."
  (let ((type (magic:file file :mime-type)))
    (when (string-match #r"^image/\(.*$\)" type)
      (substring type (match-beginning 1)))))

;;;###autoload
(defun magic:file-text-p (file)
  "Return non-nil if FILE is a text file.

As reported by libmagic. If so, the text type is returned."
  (let ((type (magic:file file :mime-type)))
    (when (string-match #r"^text/\(.*$\)" type)
      (substring type (match-beginning 1)))))

(defun magic:file-coding-system-p (file)
  "Return non-nil if FILE is encoded in a known coding system.

This will return nil if `coding-system-for-read' had been explicitly
set by something else prior.  For example, if the user had called
`find-file' with a prefix arg."
  (unless coding-system-for-read
    (let ((cs (magic:file file :mime-encoding)))
      (and cs
	   (magic:file-text-p file)
	   (find-coding-system (intern cs))))))

(defun magic:find-file-noselect (file)
  (let* ((codesys (intern (magic:file file :mime-encoding)))
	 (coding-system-for-read codesys)
	 (buf (create-file-buffer file))
	 (coding nil))
    (with-current-buffer buf
      (insert-file-contents file t)
      (hack-local-variables)
      (when (and coding (not (eq coding codesys)))
	(let ((coding-system-for-read coding))
	  (set-buffer-file-coding-system coding)
	  (insert-file-contents file t nil nil t)))
      (after-find-file nil t))
    buf))

;;;###autoload
(defun magic:find-file-magic-alist-enable ()
  "Enables libmagic backed coding system detection."
  (add-to-list 'find-file-magic-files-alist
	       (cons 'magic:file-coding-system-p
		     'magic:find-file-noselect) t))

(provide 'ffi-magic)
;;; ffi-magic.el ends here
