/* Opaque Lisp objects.
   Copyright (C) 1993, 1994, 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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


/* Synched up with: Not in FSF. */

/* Written by Ben Wing, October 1993. */

/* "Opaque" is used internally to hold keep track of allocated memory
   so it gets GC'd properly, and to store arbitrary data in places
   where a Lisp_Object is required and which may get GC'd. (e.g.  as
   the argument to record_unwind_protect()).  Once created in C,
   opaque objects cannot be resized.

   OPAQUE OBJECTS SHOULD NEVER ESCAPE TO THE LISP LEVEL.  Some code
   depends on this.  As such, opaque objects are a generalization
   of the Qunbound marker.
 */

#include <config.h>
#include "lisp.h"
#include "opaque.h"

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
Lisp_Object Vopaque_ptr_free_list;
#endif	/* !BDWGC */

/* Should never, ever be called. (except by an external debugger) */
static void
print_opaque(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	const Lisp_Opaque *p = XOPAQUE(obj);

	write_fmt_str(printcharfun,
		      "#<INTERNAL OBJECT (SXEmacs bug?) (opaque, size=%lu) 0x%lx>",
		      (long)(p->size), (unsigned long)p);
}

static inline size_t
aligned_sizeof_opaque(size_t opaque_size)
	__attribute__((always_inline));
static inline size_t
aligned_sizeof_opaque(size_t opaque_size)
{
	return ALIGN_SIZE(offsetof(Lisp_Opaque, data) + opaque_size,
			  ALIGNOF(sxe_max_align_t));
}

static size_t sizeof_opaque(const void *header)
{
	return aligned_sizeof_opaque(((const Lisp_Opaque *)header)->size);
}

/* Return an opaque object of size SIZE.
   If DATA is OPAQUE_CLEAR, the object's data is memset to '\0' bytes.
   If DATA is OPAQUE_UNINIT, the object's data is uninitialized.
   Else the object's data is initialized by copying from DATA. */
Lisp_Object
make_opaque(const void *data, size_t size)
{
	Lisp_Opaque *p = (Lisp_Opaque *)
		alloc_lcrecord(aligned_sizeof_opaque(size), &lrecord_opaque);

	assert(p!=NULL);
	if(p != NULL) {
		p->size = size;

		if (data == OPAQUE_CLEAR)
			memset(p->data, '\0', size);
		else if (data == OPAQUE_UNINIT)
			DO_NOTHING;
		else
			memcpy(p->data, data, size);

		{
			Lisp_Object val;
			XSETOPAQUE(val, p);
			return val;
		}
	}
	return Qnil;
}

/* This will not work correctly for opaques with subobjects! */

static int equal_opaque(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	size_t size;
	return ((size = XOPAQUE_SIZE(obj1)) == XOPAQUE_SIZE(obj2) &&
		!memcmp(XOPAQUE_DATA(obj1), XOPAQUE_DATA(obj2), size));
}

/* This will not work correctly for opaques with subobjects! */

static unsigned long hash_opaque(Lisp_Object obj, int depth)
{
	if (XOPAQUE_SIZE(obj) == sizeof(unsigned long))
		return *((unsigned long *)XOPAQUE_DATA(obj));
	else
		return memory_hash(XOPAQUE_DATA(obj), XOPAQUE_SIZE(obj));
}

static const struct lrecord_description opaque_description[] = {
	{XD_END}
};

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION("opaque", opaque,
				       0, print_opaque, 0,
				       equal_opaque, hash_opaque,
				       opaque_description,
				       sizeof_opaque, Lisp_Opaque);

/* stuff to handle opaque pointers */

/* Should never, ever be called. (except by an external debugger) */
static void
print_opaque_ptr(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	const Lisp_Opaque_Ptr *p = XOPAQUE_PTR(obj);

	write_fmt_string(printcharfun,
			 "#<INTERNAL OBJECT (SXEmacs bug?) "
			 "(opaque-ptr, adr=%p) %p>", p->ptr, p);
}

static int equal_opaque_ptr(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return (XOPAQUE_PTR(obj1)->ptr == XOPAQUE_PTR(obj2)->ptr);
}

static unsigned long hash_opaque_ptr(Lisp_Object obj, int depth)
{
	return (unsigned long)XOPAQUE_PTR(obj)->ptr;
}

DEFINE_LRECORD_IMPLEMENTATION("opaque-ptr", opaque_ptr,
			      0, print_opaque_ptr, 0,
			      equal_opaque_ptr, hash_opaque_ptr, 0,
			      Lisp_Opaque_Ptr);

Lisp_Object make_opaque_ptr(void *val)
{
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	Lisp_Object res = wrap_object(
		alloc_lcrecord(sizeof(Lisp_Opaque_Ptr), &lrecord_opaque_ptr));
#else  /* !BDWGC */
	Lisp_Object res = allocate_managed_lcrecord(Vopaque_ptr_free_list);
#endif	/* BDWGC */
	/* escrow val */
	set_opaque_ptr(res, val);
	return res;
}

/* Be very very careful with this.  Same admonitions as with
   free_cons() apply. */

void free_opaque_ptr(Lisp_Object ptr)
{
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	xfree(ptr);
#else  /* !BDWGC */
	free_managed_lcrecord(Vopaque_ptr_free_list, ptr);
#endif	/* BDWGC */
	return;
}

void
reinit_opaque_once_early(void)
{
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	Vopaque_ptr_free_list =
		make_lcrecord_list(sizeof(Lisp_Opaque_Ptr),
				   &lrecord_opaque_ptr);
	staticpro_nodump(&Vopaque_ptr_free_list);
#endif	/* !BDWGC */
	return;
}

void init_opaque_once_early(void)
{
	INIT_LRECORD_IMPLEMENTATION(opaque);
	INIT_LRECORD_IMPLEMENTATION(opaque_ptr);

	reinit_opaque_once_early();
}
