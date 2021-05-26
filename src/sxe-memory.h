/* Utility definitions for C code of SXEmacs for handling memory

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
#ifndef INCLUDED_sxe_memory_h_
#define INCLUDED_sxe_memory_h_


/* generally useful */
#define xnew(type) ((type *) xmalloc (sizeof (type)))
#define xnew_atomic(type) ((type *) xmalloc_atomic (sizeof (type)))
#define xnew_array(type, len) ((type *) xmalloc ((len) * sizeof (type)))
#define xnew_atomic_array(type, len)			\
	((type*)xmalloc_atomic((len) * sizeof(type)))
#define xnew_and_zero(type) ((type *) xmalloc_and_zero (sizeof (type)))
#define xzero(lvalue) ((void) memset (&(lvalue), '\0', sizeof (lvalue)))
#define xnew_array_and_zero(type, len)				\
	((type*)xmalloc_and_zero ((len) * sizeof (type)))
#define xrealloc_array(ptr, type, len)				\
	((void) (ptr = (type *) xrealloc (ptr, (len) * sizeof (type))))
#define XREALLOC_ARRAY		xrealloc_array
#define alloca_array(type, len) ((type *) alloca ((len) * sizeof (type)))

/************************************************************************/
/*				  Memory				*/
/************************************************************************/

#ifdef ALL_DEBUG_FLAGS
#undef GC_DEBUG_FLAG
#define GC_DEBUG_FLAG
#endif

#if !defined GC_DEBUG_FLAG
# define SXE_DEBUG_GC(args...)
#else
# define SXE_DEBUG_GC(args...)		__SXE_DEBUG__("[gc] " args)
#endif
#define SXE_DEBUG_GC_PT(args...)	SXE_DEBUG_GC("[pthread]: " args)
#define SXE_CRITICAL_GC(args...)	__SXE_DEBUG__("[gc] CRITICAL: " args)

void malloc_warning(const char *);

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
# if defined HAVE_THREADS
#  if !defined GC_PTHREADS
#   define GC_PTHREADS	1
#  endif  /* !GC_PTHREADS */
#  if !defined GC_THREADS
#   define GC_THREADS	1
#  endif  /* !GC_THREADS */
# endif	 /* HAVE_THREADS */

# undef GC_DEBUG
# if defined GC_DEBUG_FLAG
#  define GC_DEBUG	1
# endif	 /* GC_DEBUG_FLAG */
# if defined HAVE_GC_GC_H
#  include <gc/gc.h>
# elif defined HAVE_GC_H
#  include <gc.h>
# else
#  error "Take less of those pills!"
# endif

# if defined GC_DEBUG_FLAG
#  define zmalloc		GC_MALLOC_IGNORE_OFF_PAGE
#  define zcalloc(n, m)	GC_MALLOC((n)*(m))
#  define zmalloc_atomic	GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE
#  define zmalloc_and_zero	GC_MALLOC
#  define zrealloc		GC_REALLOC
#  define zstrdup		GC_STRDUP
#  undef zfree
#  define zfree(x)		GC_FREE(x)
# else	/* !GC_DEBUG_FLAG */
#  define zmalloc		GC_malloc_ignore_off_page
#  define zcalloc(n, m)		GC_malloc((n)*(m))
#  define zmalloc_atomic	GC_malloc_atomic_ignore_off_page
#  define zmalloc_and_zero	GC_malloc
#  define zrealloc		GC_realloc
#  define zstrdup		GC_strdup
#  undef zfree
#  define zfree(x)
# endif	/* GC_DEBUG_FLAG */

#else  /* !BDWGC */
#define zmalloc		F&^!
#define zcalloc		F&^!
#define zmalloc_atomic	F&^!
#define zmalloc_and_zero	F&^!
#define zrealloc	F&^!
#define zstrdrup	F&^!
#endif	/* BDWGC */

/* also define libc mem funs */
#define ymalloc		malloc
#define ycalloc(n, m)	calloc(n, m)
#define ymalloc_atomic(n)	ycalloc(1, n)
#define ymalloc_and_zero(x)	ycalloc(1, x)
#define yrealloc	realloc
#define ystrdup		strdup
#define yfree(x)	free(x)
/* and their convenience companions */
#define ynew(type)		((type*)ymalloc(sizeof(type)))
#define ynew_array(type, len)	((type*)ymalloc((len) * sizeof(type)))
#define ynew_and_zero(type)	((type*)ymalloc_and_zero(sizeof(type)))
#define ynew_array_and_zero(type, len)			\
	((type*)ymalloc_and_zero((len) * sizeof(type)))
#define YREALLOC_ARRAY(ptr, type, len)					\
	((void)(ptr = (type *)yrealloc(ptr, (len) * sizeof (type))))

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
/* and now the x* series */
# define xmalloc		zmalloc
# define xcalloc		zcalloc
# define xmalloc_atomic		zmalloc_atomic
# define xmalloc_and_zero	zmalloc_and_zero
# define xrealloc		zrealloc
# define xstrdup		zstrdup
# if defined ERROR_CHECK_MALLOC
#  define xfree(args...)	zfree(args)
# else	/* !ERROR_CHECK_MALLOC */
#  define xfree(args...)
# endif	 /* ERROR_CHECK_MALLOC */

#else  /* !BDWGC */
void *xmalloc(size_t size);
void *xmalloc_atomic(size_t size);
void *xmalloc_and_zero(size_t size);
void *xrealloc(void *, size_t size);
char *xstrdup(const char *);
# if defined ERROR_CHECK_MALLOC
#  if SIZEOF_VOID_P == 4
#   define xfree(lvalue)					\
	do {							\
		void *volatile *xfree_ptr =			\
			(void *volatile*)			\
			((volatile void*)&(lvalue));		\
		assert(*xfree_ptr != (void*)0xB00BB4BE);	\
		yfree(*xfree_ptr);				\
		*xfree_ptr = (void*)0xB00BB4BE;			\
	} while (0)
#  elif SIZEOF_VOID_P == 8
#   define xfree(lvalue)							\
	do {								\
		void *volatile *xfree_ptr =				\
			(void *volatile*)				\
			((volatile void*)&(lvalue));			\
		assert(*xfree_ptr != (void*)0xCAFEBABEDEADBEEF);	\
		yfree(*xfree_ptr);					\
		*xfree_ptr = (void*)0xCAFEBABEDEADBEEF;			\
	} while (0)
#  else  /* huh? */
#   error "Strange-arse system detected.  Watch a movie, it\'s more fun!"
#  endif
# else	/* !ERROR_CHECK_MALLOC */
#  define xfree(args...)	yfree(args)
# endif	 /* ERROR_CHECK_MALLOC */
#endif	/* BDWGC */


#endif
