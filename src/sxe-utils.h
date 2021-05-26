/* Utility definitions for C code of SXEmacs

   Copyright (C) 1985-1987, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1996 Richard Mlynarik.
   Copyright (C) 1995, 1996, 2000 Ben Wing.
   Copyright (C) 2004 Steve Youngs.
   Copyright (C) 2011 Nelson Ferreira.

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

/* NOT synched with FSF */
#ifndef INCLUDED_sxe_utils_h_
#define INCLUDED_sxe_utils_h_

/* ------------------------ include files ------------------- */

/* We include the following generally useful header files so that you
   don't have to worry about prototypes when using the standard C
   library functions and macros.  These files shouldn't be excessively
   large so they shouldn't cause that much of a slowdown. */

#include <stdlib.h>
#include <string.h>		/* primarily for memcpy, etc. */
#include <stdio.h>		/* NULL, etc. */
#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>		/* offsetof */
#include <sys/types.h>
#include <limits.h>
#if defined HAVE_STDBOOL_H
# include <stdbool.h>
#endif


/* goodies */
#ifdef SXE_UNUSED
#elif defined(__GNUC__)
# define SXE_UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define SXE_UNUSED(x) /*@unused@*/ x
#else
# define SXE_UNUSED(x) x
#endif

#ifdef UNUSED
#undef UNUSED
#define UNUSED(x) SXE_UNUSED(x)
#endif

#ifdef SXE_SET_UNUSED
#else
#  define SXE_SET_UNUSED(x) ((void)(x))
#endif

#ifdef WEAK_EXTERN
#elif defined(__GNUC__)
# define WEAK_EXTERN	extern __attribute__((weak))
#else
# error "Grrr, bloody 'ell, can't figure out how to create weak symbols"
#endif

#ifdef WEAK
#elif defined(__GNUC__)
# define WEAK		__attribute__((weak))
#else
# error "Grrr, bloody 'ell, can't figure out how to create weak symbols"
#endif


#ifdef LIKELY
#else
#define LIKELY(_x)	__builtin_expect(!!(_x), 1)
#endif

#ifdef UNLIKELY
#else
#define UNLIKELY(_x)	__builtin_expect(!!(_x), 0)
#endif


#define extern_inline	static inline

#ifdef ALL_DEBUG_FLAGS
#undef SXE_DEBUG_FLAG
#define SXE_DEBUG_FLAG
#endif

#define __SXE_DEBUG__(args...)		fprintf(stderr, "SXE " args)
#ifndef SXE_DEBUG_FLAG
#define SXE_DEBUG(args...)
#else
#define SXE_DEBUG(args...)		__SXE_DEBUG__(args)
#endif
#define SXE_DEBUG_PT(args...)		SXE_DEBUG("[pthread]: " args)
#define SXE_CRITICAL(args...)		__SXE_DEBUG__("CRITICAL: " args)


/* We define assert iff USE_ASSERTIONS or DEBUG_SXEMACS is defined.
   Otherwise we define it to be empty.  Quantify has shown that the
   time the assert checks take is measurable so let's not include them
   in production binaries. */

#if defined(USE_ASSERTIONS) && defined(emacs)
/* Highly dubious kludge */
/*   (thanks, Jamie, I feel better now -- ben) */
void assert_failed(const char *, int, const char *);
# define abort() (assert_failed (__FILE__, __LINE__, "abort()"))
# define assert(x) ((x) ? (void) 0 : assert_failed (__FILE__, __LINE__, #x))
#else
# ifdef DEBUG_SXEMACS
#  define assert(x) ((x) ? (void) 0 : (void) abort ())
# else
#  define assert(x) ((void)0)
# endif
#endif

/* from c.ac */
#ifndef BITS_PER_CHAR
#define BITS_PER_CHAR 8
#endif
#define SXE_SHORTBITS (SIZEOF_SHORT * BITS_PER_CHAR)
#define SXE_INTBITS (SIZEOF_INT * BITS_PER_CHAR)
#define SXE_LONGBITS (SIZEOF_LONG * BITS_PER_CHAR)
#define SXE_LONG_LONG_BITS (SIZEOF_LONG_LONG_INT * BITS_PER_CHAR)
#define SXE_VOID_P_BITS (SIZEOF_VOID_P * BITS_PER_CHAR)

/* Also define min() and max(). (Some compilers put them in strange
   places that won't be referenced by the above include files, such
   as 'macros.h' under Solaris.) */

#ifndef min
#define min(a,b) (((a) <= (b)) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif


#define countof(x) ((int) (sizeof(x)/sizeof((x)[0])))

#if !defined HAVE_DECL_STRDUP
extern char *strdup(const char *s);
#endif	/* HAVE_DECL_STRDUP */


#ifndef PRINTF_ARGS
# if defined (__GNUC__) && (__GNUC__ >= 2)
#  define PRINTF_ARGS(string_index,first_to_check) \
	  __attribute__ ((format (printf, string_index, first_to_check)))
# else
#  define PRINTF_ARGS(string_index,first_to_check)
# endif				/* GNUC */
#endif

#ifndef DOESNT_RETURN
# if defined __GNUC__
#  if ((__GNUC__ > 2) || (__GNUC__ == 2) && (__GNUC_MINOR__ >= 5))
#   define DOESNT_RETURN void
#   define DECLARE_DOESNT_RETURN(decl) \
	   extern void decl __attribute__ ((noreturn))
#   define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
     /* Should be able to state multiple independent __attribute__s, but  \
	the losing syntax doesn't work that way, and screws losing cpp */ \
	   extern void decl \
		  __attribute__ ((noreturn, format (printf, str, idx)))
#  else
#   define DOESNT_RETURN void volatile
#   define DECLARE_DOESNT_RETURN(decl) extern void volatile decl
#   define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
	   extern void volatile decl PRINTF_ARGS(str,idx)
#  endif			/* GNUC 2.5 */
# else
#  define DOESNT_RETURN void
#  define DECLARE_DOESNT_RETURN(decl) extern void decl
#  define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
	  extern void decl PRINTF_ARGS(str,idx)
# endif				/* GNUC */
#endif


/* No type has a greater alignment requirement than sxe_max_align_t.
   (except perhaps for types we don't use, like long double) */
typedef union {
	struct {
		long l;
	} l;
	struct {
		void *p;
	} p;
	struct {
		void (*f) (void);
	} f;
	struct {
		double d;
	} d;
} sxe_max_align_t;

#ifndef ALIGNOF
# if defined (__GNUC__) && (__GNUC__ >= 2)
/* gcc has an extension that gives us exactly what we want. */
#  define ALIGNOF(type) __alignof__ (type)
# elif ! defined (__cplusplus)
/* The following is mostly portable, except that:
   - it doesn't work for inside out declarations like void (*) (void).
     (so just call ALIGNOF with a typedef'ed name)
   - it doesn't work with C++.  The C++ committee has decided,
     in its infinite wisdom, that:
     "Types must be declared in declarations, not in expressions." */
#  define ALIGNOF(type) offsetof (struct { char c; type member; }, member)
# else
/* C++ is annoying, but it has a big bag of tricks.
   The following doesn't have the "inside out" declaration bug C does. */
template < typename T > struct alignment_trick {
	char c;
	T member;
};
#  define ALIGNOF(type) offsetof (alignment_trick<type>, member)
# endif
#endif				/* ALIGNOF */

#define ALIGN_SIZE(len, unit) \
  ((((len) + (unit) - 1) / (unit)) * (unit))

/* #### Yuck, this is kind of evil */
#define ALIGN_PTR(ptr, unit) \
  ((void *) ALIGN_SIZE ((size_t) (ptr), unit))

#ifndef DO_NOTHING
#define DO_NOTHING do {} while (0)
#endif

#ifndef DECLARE_NOTHING
#define DECLARE_NOTHING struct nosuchstruct
#endif


/* str funs */
#define xstrlen		strlen
#define xstrcmp		strcmp
#define xstrncmp	strncmp
#define xstrncat	strncat

extern_inline char*
xstrncpy(char* target, const char*source, size_t len)
	__attribute__((always_inline));
extern_inline char*
xstrncpy(char* target, const char*source, size_t len)
{
	*target ='\0';
	return strncat(target,source,len-1);
}

#if !defined(FORBID_STRCPY)
#  define xstrcat		strcat
#  define xstrcpy		strcpy
#  if defined(HAVE_STPCPY)
#     define xstpcpy	stpcpy
#   else
extern_inline char*
xstpcpy(char *target, const char *source)
	__attribute__((always_inline));
extern_inline char*
xstpcpy(char *target, const char *source)
{
	char *p = target;
	size_t len = xstrlen(source);

	strcpy(target, source);
	p += len;
	return p;
}
#   endif
#else
#  if defined(strcpy)
#    undef strcpy
#  endif
#  define strcpy  no_strcpy
extern_inline char*
no_strcpy(char*,const char*)
	__attribute__((always_inline));
extern_inline char*
no_strcpy(char * SXE_UNUSED(target),const char * SXE_UNUSED(source))
{
	assert(0);
	return NULL;
}
#  if defined(strcat)
#    undef strcat
#  endif
#  define strcat  no_strcat
extern_inline char*
no_strcat(char*,const char*)
	__attribute__((always_inline));
extern_inline char*
no_strcat(char * SXE_UNUSED(target), const char* SXE_UNUSED(source))
{
	assert(0);
	return NULL;
}
#  if defined(stpcpy)
#    undef stpcpy
#  endif
#  define stpcpy	no_stpcpy
#  define xstpcpy	no_stpcpy
extern_inline char*
no_stpcpy(char*,const char*)
	__attribute__((always_inline));
extern_inline char*
no_stpcpy(char* SXE_UNUSED(target),const char* SXE_UNUSED(source))
{
	assert(0);
	return NULL;
}
#endif


#if defined HAVE_STPNCPY
# define xstpncpy	stpncpy
#else
extern_inline char*
xstpncpy(char *target, const char *source, size_t len)
	__attribute__((always_inline));
extern_inline char*
xstpncpy(char *target, const char *source, size_t len)
{
	char *p = target;
	xstrncpy(target, source, len);
	p += len;
	return p;
}
#endif	/* !HAVE_STPNCPY */

#define xmemcmp		memcmp
#define xmemcpy		memcpy



extern_inline size_t
xmin_size_t(size_t a, size_t b)
	__attribute__((always_inline));
extern_inline size_t
xmin_size_t(size_t a, size_t b)
{
	if (a < b) {
		return a;
	} else {
		return b;
	}
}

#endif
