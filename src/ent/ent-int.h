/*
  ent-int.c -- Ordinary Integers for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt

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


#ifndef INCLUDED_ent_int_h_
#define INCLUDED_ent_int_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

extern EMACS_INT Vmost_negative_int, Vmost_positive_int;
extern Lisp_Object Qzero, Qone;

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
extern Lisp_Object make_bigz(long);
#define make_integer(x)							\
	(NUMBER_FITS_IN_AN_EMACS_INT(x) ? make_int(x) : make_bigz(x))
#else
#define make_integer(x) make_int(x)
#endif

extern Bufpos marker_position(Lisp_Object);
extern_inline EMACS_INT ent_int(Lisp_Object);


extern_inline EMACS_INT
ent_int(Lisp_Object number)
{
	if (!CHARP(number) && !MARKERP(number))
		return XINT(number);
	else if (CHARP(number))
		return XCHAR(number);
	else if (MARKERP(number))
		return marker_position(number);
	else
		return 0;
}

extern void init_optables_INT_T(void);
extern void init_ent_int(void);
extern void syms_of_ent_int(void);
extern void vars_of_ent_int(void);

#endif /* INCLUDED_ent_int_h_ */
