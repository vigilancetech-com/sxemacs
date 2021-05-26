;;; sound.el --- Loading sound files in SXEmacs

;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 2006 Sebastian Freundt

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Code:
(defgroup sound nil
  "Configure SXEmacs sounds and properties"
  :group 'environment)

(defcustom sound-default-alist
      '((default		:sound bass)
	(undefined-key	:sound drum)
	(undefined-click	:sound drum)
	;; beginning-of-buffer or end-of-buffer errors.
	(buffer-bound	:sound drum)
	;; buffer-read-only error
	(read-only	        :sound drum)
	;; non-interactive function or lambda called
	(command-error	:sound bass)
	(y-or-n-p		:sound quiet)
	(yes-or-no-p		:sound quiet)
	(auto-save-error	:sound whip :volume 100)
	(no-completion	:sound whip)
	(isearch-failed	:sound quiet)
	(isearch-quit	:sound bass)
	;; QUIT: sound generated by ^G and its variants.
	(quit		:sound quiet :volume 75)
	;; READY: time-consuming task has completed...  compile,
	;; cvs-update, etc.
	(ready		:sound cuckoo)
	;; WARP: XEmacs has changed the selected-window or frame
	;; asynchronously...  Especially when it's done by an
	;; asynchronous process filter.  Perhaps by a debugger breakpoint
	;; has been hit?
	(warp		:sound yeep :volume 75)
	;; ALARM: used for reminders...
	(alarm		:sound cuckoo :volume 100)
	)
      "The alist of sounds and associated error symbols.

 Used to set sound-alist in load-default-sounds."
      :group 'sound
      :type '(repeat
	      (group (symbol :tag "Name")
		     (checklist :inline t
				:greedy t
				(group :inline t
				       (const :format "" :value :sound)
				       (symbol :tag "Sound"))
				(group :inline t
				       (const :format "" :value :volume)
				       (integer :tag "Volume"))
				(group :inline t
				       (const :format "" :value :pitch)
				       (integer :tag "Pitch"))
				(group :inline t
				       (const :format "" :value :duration)
				       (integer :tag "Duration"))))))

(defcustom sound-load-list
  '((load-sound-file "drum-beep"	'drum)
    (load-sound-file "quiet-beep"	'quiet)
    (load-sound-file "bass-snap"	'bass 80)
    (load-sound-file "whip"		'whip 70)
    (load-sound-file "cuckoo"		'cuckoo)
    (load-sound-file "yeep"		'yeep)
    (load-sound-file "hype"		'hype 100)
    )
  "A list of calls to load-sound-file to be processed by load-default-sounds.

  Reference load-sound-file for more information."

  :group 'sound
  :type '(repeat  (sexp :tag "Sound")
		  ))

(defcustom default-sound-directory (locate-data-directory "sounds")
  "Default directory to load a sound file from."
  :group 'sound
  :type 'directory
  )

;; #### This should really be a list.  --hniksic
;; #### It is now :) -hroptatyr
(defcustom sound-extension-list (cond ((eq system-type 'linux)
				       '(".wav" ".au" ".mp3" ".mka" ".ogg"
					 ".aac" ".ac3" ".mp4" ".flac"
					 ".mpc" ".mpa"))
				      (t
				       '(".au")))
  "List of filename extensions to complete sound file name with."
  :group 'sound
  :type 'string)

(defcustom default-sound-directory-list (locate-data-directory-list "sounds")
  "List of directories which to search for sound files"
  :group 'sound
  :type '(repeat directory )
  )

(defvar default-media-stream-volume nil
  "*The default volume to assign to media streams
when an explicit volume parameter is omitted.")

;;;###autoload
(or sound-alist
    ;; these should be silent until sounds are loaded
    (setq sound-alist '((ready nil) (warp nil))))

(eval-when-compile (defvar file))

(defun locate-sound-file (filename)
  "Search for FILENAME in `default-sound-directory-list'
with respect to the extensions given by `sound-extension-list'."
  (let ((exts (cond ((listp sound-extension-list)
		     sound-extension-list)
		    ((stringp sound-extension-list)
		     (split-string sound-extension-list ":"))
		    (t nil))))

    (cond ((file-exists-p filename)
	   (expand-file-name filename))
	  ((file-name-absolute-p filename)
	   ;; For absolute file names, we don't have on choice on the
	   ;; location, but sound extensions however can still be tried
	   (setq file (locate-file filename
				   (list (file-name-directory filename))
				   exts)))
	  (t
	   (setq file (locate-file filename
				   default-sound-directory-list
				   exts))))))

(defun make-sound-alist-item (filename sound-name &optional volume)
  "Return an item suitable for `sound-alist'."
  (let* ((file (locate-sound-file filename))
	 ;; let's create media-streams
	 (stream (make-media-stream :file file))
	 (data))

    (unless file
      (error "Couldn't load sound file %s" filename))

;; DEPRECATED as of merge of hrop-feat-MM series
;;     (unwind-protect
;;         (save-excursion
;;           (set-buffer (setq buf (get-buffer-create " *sound-tmp*")))
;;           (buffer-disable-undo (current-buffer))
;;           (erase-buffer)
;;           (let ((coding-system-for-read 'binary))
;;             (insert-file-contents  file))
;;           (setq data (buffer-string))
;;           (erase-buffer))
;;       (and buf (kill-buffer buf)))

    (nconc (list sound-name)
	   (if (and volume (not (eq 0 volume)))
	       (list ':volume volume))
	   (if data
	       (list ':sound data))
	   (list ':stream stream))))

;;;###autoload
(defun load-sound-file (filename sound-name &optional volume)
  "Read in an audio-file and add it to the sound-alist.
The cached sound can be referenced later by SOUND-NAME.

FILENAME can either be absolute or relative, in which case the file will
be searched in the directories given by `default-sound-directory-list'.
When looking for the file, the extensions given by `sound-extension-list' are
also tried in the given order."
  (interactive "fSound file name: \n\
SSymbol to name this sound: \n\
nVolume (0 for default): ")
  (unless (symbolp sound-name)
    (error "sound-name not a symbol"))
  (unless (or (null volume) (integerp volume))
    (error "volume not an integer or nil"))

  (let ((item (make-sound-alist-item filename sound-name volume))
	(old (assq sound-name sound-alist)))

      ;; some conses in sound-alist might have been dumped with emacs.
      (when old
	(setq sound-alist (delq old (copy-sequence sound-alist))))

      (setq sound-alist (cons item sound-alist)))
  sound-name)

;;;###autoload
(defun load-default-sounds ()
  "Load and install some sound files as beep-types, using
`load-sound-file'.  This only works if you specify `default-audio-device'."
  (interactive)
  ;; #### - this should do NOTHING if the sounds can't be played.
  (message "Loading sounds...")
  (setq sound-alist nil)
  ;; this is where the calls to load-sound-file get done
  (mapc 'eval sound-load-list)
  (setq sound-alist
	(append sound-default-alist
		sound-alist))
  (message "Loading sounds...done")
  ;; (beep nil 'quiet)
  )

;;;###autoload
(defun play-media-stream (stream &optional device sentinel volume)
  "Play the media stream STREAM on an audio device.
STREAM must be a valid media-stream object as created by
`make-media-stream'.
Return a media-thread object which can be used to interact with
the worker thread which handles STREAM, which of course is only
useful in asynchronous mode.

This function uses the value of `synchronous-sounds' to decide
if a stream is played synchronously or asynchronously.
See `play-media-stream-synchronously' and
`play-media-stream&'.

Optional second argument DEVICE must be an audio device created
by `make-audio-device'.
If omitted DEVICE defaults to the value of `default-audio-device'.

Optional third argument SENTINEL specifies a lisp function to be
called after the stream playback finishes.  The function should
take one argument (STREAM) where STREAM is bound to the
media stream which finished.  See `set-media-thread-sentinel'.

Optional fourth argument VOLUME specifies an initial value for
the playback volume.


Depending on the value of `synchronous-sounds' this function will
decide whether to play either asynchronously or synchronously.

If `synchronous-sounds' is `nil' AND threads are supported,
streams will be passed to the `play-media-stream&'
function.
In that case return a media-thread object which can be used to
interact with the worker thread which handles STREAM.

If `synchronous-sounds' is non-`nil' OR threads are not supported,
streams will be passed to the `play-media-stream-synchronously'
function.
In that case return non-`nil' if STREAM was played successfully,
and `nil' otherwise.

See `play-media-stream-synchronously' and
`play-media-stream&'."
  (let* ((vol (or volume default-media-stream-volume)))
    (if (and (fboundp #'play-media-stream&)
	     (not synchronous-sounds))
	(play-media-stream& stream device sentinel vol)
      (play-media-stream-synchronously stream device sentinel vol))))

;;;###autoload
(defun play-sound (sound &optional volume device sentinel)
  "Play the sound SOUND from `sound-alist'.

The sound is played at the specified VOLUME \(0-100, default
specified by the `bell-volume' variable\).

With no further media drivers, the sound file must be in the
Sun/NeXT U-LAW format. Under Linux WAV files are also supported.

DEVICE can be any device created by `make-audio-device' and
defaults to `default-audio-device', or, if that is `nil',
to the selected device.

Optional argument SENTINEL specifies a lisp function to be
called after the stream playback finishes.  The function should
take two arguments (STREAM STATE) where STREAM is bound to the
media stream which finished and STATE is a symbol \(currently the
only valid symbol is 'finished\).  See `set-media-thread-sentinel'."
  (let ((data (cdr-safe (assq sound sound-alist))))
    (when (and data default-audio-device)
      (let ((ms (or (plist-get data :stream)
		    (let ((s (plist-get data :sound)))
		      (and (stringp s)
			   (make-media-stream :data s)))))
	    (vol
	     (plist-get data :volume
			(or volume default-media-stream-volume bell-volume))))
	(when ms
	  (play-media-stream ms device sentinel vol))))))

;;; sound.el ends here.