/* Interface from Emacs to terminfo.
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: FSF 19.30. */

#include <config.h>

#include <string.h>



/* Every little bit of this God-damned file has caused all manner
   of headaches due to inconsistent and incorrect header files
   on one system or other, and we don't currently need anything here,
   so just comment the whole damn lot out!!! */

#ifndef HAVE_TERMIOS
#ifdef AIX
#include <termio.h>
#endif				/* AIX */
#endif

/* Interface to curses/terminfo library.
   Turns out that all of the terminfo-level routines look
   like their termcap counterparts except for tparm, which replaces
   tgoto.  Not only is the calling sequence different, but the string
   format is different too.
*/

#ifdef HAVE_CURSES_H
#ifdef CURSES_H_FILE
#include CURSES_H_FILE
#endif
#endif

#ifdef HAVE_TERMCAP_H
#ifdef TERMCAP_H_FILE
#include TERMCAP_H_FILE
#endif
#endif

/* Sun, in their infinite lameness, supplies (possibly) broken headers
   even under Solaris.  GCC feels it necessary to correct things by
   supplying its own headers.  Unfortunately, if you build GCC under
   one version of Solaris and then upgrade your Solaris, you may get
   screwed because Sun in their continuing lameness changes curses.h
   in such a way that the "fixed" GCC headers are now broken. (GCC
   is equally lame in that it supplies "fixed" headers for curses.h
   but not term.h.) However, it seems to work to just not include
   term.h under Solaris, so we try that.  KLUDGE! */
#ifdef HAVE_TERM_H
#ifdef TERM_H_FILE
#if !(defined (__GNUC__) && defined (SOLARIS2))
#include TERM_H_FILE
#endif
#endif
#endif

extern void *xmalloc_atomic(int size);

#ifndef HAVE_TPARM_PROTOTYPE
/* Canonical for one, in their infinite wisdom ships the tinfo library without
   any headers of which we'd need for proper tparm prototype.
   If it is not defined, let's define it here, since if we are linking this
   configure has determined we have a linkable tparm
*/
extern char *tparm(const char *string, int arg1, int arg2, int arg3,
		   int arg4, int arg5, int arg6, int arg7, int arg8, int arg9);
#endif

/* XEmacs: renamed this function because just tparam() conflicts with
   ncurses (We don't use this function anyway!) */
char *emacs_tparam(const char *string, char *outstring, int len, int arg1,
		   int arg2, int arg3, int arg4, int arg5, int arg6, int arg7,
		   int arg8, int arg9);
char *emacs_tparam(const char *string, char *outstring, int len, int arg1,
		   int arg2, int arg3, int arg4, int arg5, int arg6, int arg7,
		   int arg8, int arg9)
{
	size_t slen = len;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
	char *paramstring = (char *)string;
	char *temp = (char *)
	        tparm(paramstring, arg1, arg2, arg3, arg4, arg5, arg6, arg7,
		      arg8, arg9);
#pragma GCC diagnostic pop


	if (outstring == 0) {
		slen = strlen(temp)+1;
		outstring = (char *)xmalloc_atomic( slen );
	}
	strncpy(outstring, temp, slen);
	return outstring;
}
