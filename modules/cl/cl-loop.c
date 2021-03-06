/*
  cl-loop.c -- Common Lisp Goodness, the fast version
  Copyright (C) 2006, 2007 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

  * This file is part of SXEmacs.
  *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  *
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  *
  * 2. Redistributions in binary form must reproduce the above copyright
  *    notice, this list of conditions and the following disclaimer in the
  *    documentation and/or other materials provided with the distribution.
  *
  * 3. Neither the name of the author nor the names of any contributors
  *    may be used to endorse or promote products derived from this
  *    software without specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
  * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
  * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
  * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  */

/* Synched up with: Not in FSF. */

#include "config.h"
#include <sxemacs.h>
#include "cl-loop.h"
#include "elhash.h"
#include "ent/ent.h"

#if !defined EMOD_CL_MONOMOD
PROVIDE(cl_loop);
REQUIRE(cl_loop, "cl");
#endif
#define INIT	cl_loop_LTX_init
#define REINIT	cl_loop_LTX_reinit
#define DEINIT	cl_loop_LTX_deinit

static inline int
emodcl_initialise_vars(Lisp_Object varform)
	__attribute__((always_inline));
static inline void
emodcl_step_vars(Lisp_Object varform, int varcount)
	__attribute__((always_inline));

Lisp_Object Qcl_loop_sentence, Qcl_loop_sentence_p;
Lisp_Object Qcl_loop_for_clause, Qcl_loop_for_clause_p;
Lisp_Object Qcl_loop_do_clause, Qcl_loop_do_clause_p;
Lisp_Object Qcl_loop_with_clause, Qcl_loop_with_clause_p;
Lisp_Object Qcl_loop_repeat_clause, Qcl_loop_repeat_clause_p;
Lisp_Object Qcl_loop_append_clause, Qcl_loop_append_clause_p;
Lisp_Object Qcl_loop_collect_clause, Qcl_loop_collect_clause_p;
Lisp_Object Qcl_loop_nconc_clause, Qcl_loop_nconc_clause_p;
Lisp_Object Qcl_loop_return_clause, Qcl_loop_return_clause_p;
Lisp_Object Qcl_loop_initially_clause, Qcl_loop_initially_clause_p;
Lisp_Object Qcl_loop_finally_clause, Qcl_loop_finally_clause_p;
Lisp_Object Qcl_loop_count_clause, Qcl_loop_count_clause_p;
Lisp_Object Qcl_loop_sum_clause, Qcl_loop_sum_clause_p;
Lisp_Object Qcl_loop_maximise_clause, Qcl_loop_maximise_clause_p;
Lisp_Object Qcl_loop_minimise_clause, Qcl_loop_minimise_clause_p;

Lisp_Object Qfor, Qas;
Lisp_Object Qfrom, Qdownfrom, Qupfrom, Qto, Qdownto, Qupto, Qabove, Qbelow, Qby;
Lisp_Object Qin, Qon, Qthen, Qacross, Qeach, Qthe, Qbeing, Qof;
Lisp_Object Qhash_key, Qhash_keys, Qhash_value, Qhash_values, Qusing;
Lisp_Object Qdo, Qdoing;
Lisp_Object Qtoken;
Lisp_Object Qwith, Qequals, Qand;
Lisp_Object Qrepeat;
Lisp_Object Qappend, Qappending, Qcollect, Qcollecting, Qnconc, Qnconcing;
Lisp_Object Qinto;
Lisp_Object Qcount, Qcounting, Qsum, Qsumming;
Lisp_Object Qmaximise, Qmaximising, Qmaximize, Qmaximizing;
Lisp_Object Qminimise, Qminimising, Qminimize, Qminimizing;
Lisp_Object Qinitially, Qfinally;

static Lisp_Object Qanon_acn;


static int
emodcl_initialise_vars(Lisp_Object varform)
{
	/* basically a let */
	int idx = 0;
	int varcount = 0;

	/* Make space to hold the values to give the bound variables. */
	GET_EXTERNAL_LIST_LENGTH(varform, varcount);

	{
		Lisp_Object temps[varcount];
		struct gcpro ngcpro1;

		/* wipe temps first */
		memset(temps, 0, sizeof(Lisp_Object)*varcount);

		/* Compute the values and store them in `temps' */
		NGCPROn(temps, varcount);

		LIST_LOOP_2(var, varform) {
			Lisp_Object *value = &temps[idx++];
			if (SYMBOLP(var))
				*value = Qnil;
			else {
				Lisp_Object tem;
				CHECK_CONS(var);
				tem = XCDR(var);
				if (NILP(tem))
					*value = Qnil;
				else {
					CHECK_CONS(tem);
					*value = Feval(XCAR(tem));
				}
			}
		}

		idx = 0;

		LIST_LOOP_2(var, varform) {
			specbind(SYMBOLP(var) ? var : XCAR(var), temps[idx++]);
		}

		NUNGCPRO;
	}

	return varcount;
}

static void
emodcl_step_vars(Lisp_Object varform, int varcount)
{
	/* basically a let */
	Lisp_Object temps[varcount];
	struct gcpro ngcpro1;
	int idx = 0;

	/* wipe temps first */
	memset(temps, 0, sizeof(Lisp_Object)*varcount);

	/* Compute the values and store them in `temps' */
	NGCPROn(temps, varcount);

	LIST_LOOP_2(var, varform) {
		Lisp_Object *value = &temps[idx++];
		Lisp_Object tmp;
		if (CONSP(var) && CONSP((tmp = XCDR(var))) &&
		    !NILP(XCDR(tmp))) {
			/* only if there is a step form of course */
			*value = Feval(XCAR(XCDR(tmp)));
		}
	}

	idx = 0;
	LIST_LOOP_2(var, varform) {
		Fset(XCAR(var), temps[idx++]);
	}

	NUNGCPRO;
	return;
}

static int
emodcl_initialise_vars_star(Lisp_Object varform)
{
	/* basically a let* */
	EXTERNAL_LIST_LOOP_3(var, varform, tail) {
		Lisp_Object symbol, value, tem;
		if (SYMBOLP(var))
			symbol = var, value = Qnil;
		else {
			CHECK_CONS(var);
			symbol = XCAR(var);
			tem = XCDR(var);
			if (NILP(tem))
				value = Qnil;
			else {
				CHECK_CONS(tem);
				value = Feval(XCAR(tem));
			}
		}
		specbind(symbol, value);
	}
	return 0;
}

static void
emodcl_step_vars_star(Lisp_Object varform, int unused)
{
	EXTERNAL_LIST_LOOP_3(var, varform, tail) {
		Lisp_Object symbol, value, tmp;
		if (CONSP(var) && CONSP((tmp = XCDR(var))) &&
		    !NILP(XCDR(tmp))) {
			/* only if there is a step form of course */
			symbol = XCAR(var);
			value = Feval(XCAR(XCDR(tmp)));
			Fset(symbol, value);
		}
	}
	return;
}

static inline Lisp_Object
emodcl_do_obtain_result(Lisp_Object resultform)
{
	/* assumes that resultform is gc-protected already */
	Lisp_Object result = Qnil;

	LIST_LOOP_2(form, resultform) {
		result = Feval(form);
	}

	return result;
}

static Lisp_Object
emodcl_do(
	Lisp_Object varform, Lisp_Object endtest, Lisp_Object resultform,
	Lisp_Object body,
	int(*initialiser)(Lisp_Object), void(*stepper)(Lisp_Object, int))
{
	Lisp_Object result = Qnil;
	int numbervars = 0;

	/* initial assignment */
	numbervars = initialiser(varform);

	/* now loop */
	while (NILP(Feval(endtest))) {
#if 1
		LIST_LOOP_2(form, body) {
			Feval(form);
		}
#else
		internal_catch(tag, Fprogn, body, 0);
#endif
		/* evaluate step forms */
		stepper(varform, numbervars);
	}

	/* obtain a result */
	result = emodcl_do_obtain_result(resultform);
	return result;
}


/* dynacat magic */
static void
cl_loop_sentence_mark(Lisp_Object obj)
{
	cl_loop_sentence_t *lsen = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("sentence:0x%x@0x%x shall be marked...\n",
			   (unsigned int)(lsen), (unsigned int)obj);

	mark_object(lsen->prologue);
	mark_object(lsen->epilogue);
	mark_object(lsen->iteration);

	mark_object(lsen->result);
	return;
}

static void
cl_loop_for_clause_mark(Lisp_Object obj)
{
	cl_loop_for_clause_t *fc = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("FOR:0x%x@0x%x shall be marked...\n",
			   (unsigned int)(fc), (unsigned int)obj);

	mark_object(fc->form1);
	mark_object(fc->from);
	mark_object(fc->to);
	mark_object(fc->by);

	mark_object(fc->inonacross);

	mark_object(fc->equals);
	mark_object(fc->then);

	mark_object(fc->hash_keyvar);
	mark_object(fc->hash_valvar);

	mark_object(fc->curval);
	mark_object(fc->curbound);
	mark_object(fc->curstep);
	return;
}

static void
cl_loop_do_clause_mark(Lisp_Object obj)
{
	cl_loop_do_clause_t *doc = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("DO:0x%x@0x%x shall be marked...\n",
			   (unsigned int)(doc), (unsigned int)obj);

	mark_object(doc->form);
	return;
}

static void
cl_loop_with_clause_mark(Lisp_Object obj)
{
	cl_loop_with_clause_t *wc = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("WITH:0x%x@0x%x shall be marked...\n",
			   (unsigned int)(wc), (unsigned int)obj);

	mark_object(wc->varform);
	mark_object(wc->valform);
	mark_object(wc->next);
	return;
}

static void
cl_loop_repeat_clause_mark(Lisp_Object obj)
{
	cl_loop_repeat_clause_t *rc = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("REPEAT:0x%x@0x%x shall be marked...\n",
			   (unsigned int)(rc), (unsigned int)obj);

	mark_object(rc->form);
	return;
}

static void
cl_loop_inifinret_clause_mark(Lisp_Object obj)
{
	cl_loop_inifinret_clause_t *rc = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("RETURN|INITIALLY|FINALLY:"
			   "0x%x@0x%x shall be marked...\n",
			   (unsigned int)(rc), (unsigned int)obj);

	mark_object(rc->form);
	return;
}

static void
cl_loop_accu_clause_mark(Lisp_Object obj)
{
	cl_loop_accu_clause_t *ac = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("ACCU(=COLLECT|APPEND|NCONC|etc.):"
			   "0x%x@0x%x shall be marked...\n",
			   (unsigned int)(ac), (unsigned int)obj);

	mark_object(ac->form);
	mark_object(ac->into);
	mark_object(ac->cur);
	return;
}

static void
cl_loop_generic_finaliser(Lisp_Object obj, int SXE_UNUSED(for_disksave))
{
	void *free_me = get_dynacat(obj);

	EMOD_CL_DEBUG_LOOP("generic:%p@%p shall be freed\n",
			   free_me, (void*)obj);

	xfree(free_me);
	set_dynacat(obj, NULL);
	return;
}


/* auxiliary stuff */
typedef void(*cl_loop_binder_f)(Lisp_Object, Lisp_Object);

static int
cl_loop_destructuring_bind(
	cl_loop_binder_f bindfun, Lisp_Object form, Lisp_Object value)
{
	Lisp_Object tmpf, tmpv;
	while (!NILP(form)) {
		if (SYMBOLP(form)) {
			bindfun(form, value);
			return 1;
		}
		CHECK_CONS(form);
		CHECK_CONS(value);
		tmpf = XCAR(form);
		tmpv = XCAR(value);
		/* recursive approach? :| */
		cl_loop_destructuring_bind(bindfun, tmpf, tmpv);
		form = XCDR(form);
		value = XCDR(value);
	}
	return 1;
}


/* constructors */
static inline Lisp_Object
cl_loop_make_sentence(void)
{
	cl_loop_sentence_t *lsen = xnew_and_zero(cl_loop_sentence_t);
	Lisp_Object result = make_dynacat(lsen);

	set_dynacat_type(result, Qcl_loop_sentence);

	XSETDLLIST(lsen->prologue, make_dllist());
	XSETDLLIST(lsen->epilogue, make_dllist());
	XSETDLLIST(lsen->iteration, make_dllist());
	lsen->state = 0;
	lsen->result = Qnil;

	set_dynacat_marker(result, cl_loop_sentence_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("sentence:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)lsen, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_for_clause(Lisp_Object form1)
{
	cl_loop_for_clause_t *fc = xnew_and_zero(cl_loop_for_clause_t);
	Lisp_Object result = make_dynacat(fc);

	set_dynacat_type(result, Qcl_loop_for_clause);

	fc->form1 = form1;
	fc->for_subclause = FOR_INVALID_CLAUSE;

	/* arith subclause */
	fc->from = Qzero;
	fc->to = Qzero;
	fc->by = Qone;
	/* by default we increment and compare with equalp */
	fc->byop = ASE_BINARY_OP_SUM;
	fc->torel = ASE_BINARY_REL_LESSP;
	fc->torel_strictp = 0;

	/* in/on subclauses */
	fc->inonacross = Qnil;

	/* =-then */
	fc->equals = Qnil;
	fc->then = Qnil;

	/* hash values */
	fc->hash_keyvar = Qnil;
	fc->hash_valvar = Qnil;

	/* for parallel bind */
	fc->next = Qnil;
	fc->depth = 1;

	/* for runtime */
	fc->curval = Qnil;
	fc->curbound = Qzero;
	fc->curstep = Qone;
	fc->counter = 0;

	set_dynacat_marker(result, cl_loop_for_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("FOR:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)fc, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_do_clause(Lisp_Object form1)
{
	cl_loop_do_clause_t *doc = xnew_and_zero(cl_loop_do_clause_t);
	Lisp_Object result = make_dynacat(doc);

	set_dynacat_type(result, Qcl_loop_do_clause);

	doc->form = form1;

	set_dynacat_marker(result, cl_loop_do_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("DO:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)doc, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_repeat_clause(Lisp_Object form)
{
	cl_loop_repeat_clause_t *rc = xnew_and_zero(cl_loop_repeat_clause_t);
	Lisp_Object result = make_dynacat(rc);

	set_dynacat_type(result, Qcl_loop_repeat_clause);

	rc->form = form;
	rc->counter = 0;

	set_dynacat_marker(result, cl_loop_repeat_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("REPEAT:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)rc, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_return_clause(Lisp_Object form)
{
	cl_loop_inifinret_clause_t *rc =
		xnew_and_zero(cl_loop_inifinret_clause_t);
	Lisp_Object result = make_dynacat(rc);

	set_dynacat_type(result, Qcl_loop_return_clause);

	rc->form = form;

	set_dynacat_marker(result, cl_loop_inifinret_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("RETURN:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)rc, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_initially_clause(Lisp_Object form)
{
	cl_loop_inifinret_clause_t *rc =
		xnew_and_zero(cl_loop_inifinret_clause_t);
	Lisp_Object result = make_dynacat(rc);

	set_dynacat_type(result, Qcl_loop_initially_clause);

	rc->form = form;

	set_dynacat_marker(result, cl_loop_inifinret_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("INITIALLY:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)rc, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_finally_clause(Lisp_Object form)
{
	cl_loop_inifinret_clause_t *rc =
		xnew_and_zero(cl_loop_inifinret_clause_t);
	Lisp_Object result = make_dynacat(rc);

	set_dynacat_type(result, Qcl_loop_finally_clause);

	rc->form = form;

	set_dynacat_marker(result, cl_loop_inifinret_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("FINALLY:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)rc, (unsigned int)result);

	return result;
}

/* maybe a generic cl_loop_make_accu_clause? */
Lisp_Object
cl_loop_make_append_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_append_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("APPEND:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_collect_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_collect_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("COLLECT:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_nconc_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_nconc_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("NCONC:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_count_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_count_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("COUNT:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_sum_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_sum_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("SUM:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_maximise_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_maximise_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("MAXIMISE:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_minimise_clause(Lisp_Object form)
{
	cl_loop_accu_clause_t *ac = xnew_and_zero(cl_loop_accu_clause_t);
	Lisp_Object result = make_dynacat(ac);

	set_dynacat_type(result, Qcl_loop_minimise_clause);

	ac->form = form;
	ac->into = 0;
	ac->cur = 0;

	set_dynacat_marker(result, cl_loop_accu_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("MINIMISE:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)ac, (unsigned int)result);

	return result;
}

Lisp_Object
cl_loop_make_with_clause(Lisp_Object form)
{
	cl_loop_with_clause_t *wc = xnew_and_zero(cl_loop_with_clause_t);
	Lisp_Object result = make_dynacat(wc);

	set_dynacat_type(result, Qcl_loop_with_clause);

	wc->varform = form;
	wc->valform = Qnil;
	wc->next = Qnil;
	wc->depth = 1;

	set_dynacat_marker(result, cl_loop_with_clause_mark);
	set_dynacat_finaliser(result, cl_loop_generic_finaliser);

	EMOD_CL_DEBUG_LOOP("WITH:0x%x shall be wrapped to 0x%x...\n",
			   (unsigned int)wc, (unsigned int)result);

	return result;
}


int
cl_loop_yylex(YYSTYPE *yys, Lisp_Object *scanner,
	      cl_loop_sentence_t *lsen, Lisp_Object *ctx, Lisp_Object *token)
{
	Lisp_Object tok;

	if (NILP(*scanner))
		return *yys = 0;

	tok = *token = XCAR(*scanner);
	*scanner = XCDR(*scanner);

	if (EQ(tok, Qrepeat)) {
		return *yys = REPEAT;
	}
	if (EQ(tok, Qfor) || EQ(tok, Qas)) {
		return *yys = FOR;
	}
	if (EQ(tok, Qdo) || EQ(tok, Qdoing)) {
		return *yys = DO;
	}
	if (EQ(tok, Qwith)) {
		return *yys = WITH;
	}
	if (EQ(tok, Qand)) {
		return *yys = AND;
	}

	if (EQ(tok, Qfrom) ||
	    EQ(tok, Qdownfrom) ||
	    EQ(tok, Qupfrom)) {
		return *yys = FROM;
	}
	if (EQ(tok, Qto) ||
	    EQ(tok, Qdownto) ||
	    EQ(tok, Qupto)) {
		return *yys = TO;
	}
	if (EQ(tok, Qbelow)) {
		return *yys = BELOW;
	}
	if (EQ(tok, Qabove)) {
		return *yys = ABOVE;
	}
	if (EQ(tok, Qby)) {
		return *yys = BY;
	}
	if (EQ(tok, Qin)) {
		return *yys = IN;
	}
	if (EQ(tok, Qon)) {
		return *yys = ON;
	}
	if (EQ(tok, Qequals)) {
		return *yys = EQUALS;
	}
	if (EQ(tok, Qthen)) {
		return *yys = THEN;
	}
	if (EQ(tok, Qacross)) {
		return *yys = ACROSS;
	}
	if (EQ(tok, Qbeing)) {
		return *yys = BEING;
	}
	if (EQ(tok, Qthe) || EQ(tok, Qeach)) {
		return *yys = EACH;
	}
	if (EQ(tok, Qof) || EQ(tok, Qin)) {
		return *yys = IN;
	}
	if (EQ(tok, Qhash_key) || EQ(tok, Qhash_keys)) {
		return *yys = HASH_KEY;
	}
	if (EQ(tok, Qhash_value) || EQ(tok, Qhash_values)) {
		return *yys = HASH_VALUE;
	}
	if (EQ(tok, Qusing)) {
		return *yys = USING;
	}
	if (EQ(tok, Qcollect) || EQ(tok, Qcollecting)) {
		return *yys = COLLECT;
	}
	if (EQ(tok, Qappend) || EQ(tok, Qappending)) {
		return *yys = APPEND;
	}
	if (EQ(tok, Qnconc) || EQ(tok, Qnconcing)) {
		return *yys = NCONC;
	}
	if (EQ(tok, Qcount) || EQ(tok, Qcount)) {
		return *yys = COUNT;
	}
	if (EQ(tok, Qsum) || EQ(tok, Qsumming)) {
		return *yys = SUM;
	}
	if (EQ(tok, Qminimise) || EQ(tok, Qminimising) ||
	    EQ(tok, Qminimize) || EQ(tok, Qminimizing)) {
		return *yys = MINIMISE;
	}
	if (EQ(tok, Qmaximise) || EQ(tok, Qmaximising) ||
	    EQ(tok, Qmaximize) || EQ(tok, Qmaximizing)) {
		return *yys = MAXIMISE;
	}
	if (EQ(tok, Qinto)) {
		return *yys = INTO;
	}
	if (EQ(tok, Qinitially)) {
		return *yys = INITIALLY;
	}
	if (EQ(tok, Qfinally)) {
		return *yys = FINALLY;
	}
	if (EQ(tok, Qreturn)) {
		return *yys = RETURN;
	}

	return *yys = FORM;
}

void
cl_loop_yyerror(Lisp_Object *scanner, cl_loop_sentence_t *lsen,
		Lisp_Object *ctx, Lisp_Object *token, char *msg)
{
	Fsignal(Qinvalid_read_syntax, *scanner);
	return;
}


static void
cl_loop_perform_with_pro(cl_loop_with_clause_t *wc)
{
	Lisp_Object val = Feval(wc->valform);
	if (wc->depth == 1) {
		/* optimise for the trivial case */
		cl_loop_destructuring_bind(specbind, wc->varform, val);
	} else {
		Lisp_Object *tmp = alloca_array(Lisp_Object, wc->depth);
		size_t i;
		Lisp_Object tra;

		tmp[0] = val;
		tra = wc->next;
		for (i = 1; !NILP(tra); i++) {
			cl_loop_with_clause_t *wct = get_dynacat(tra);
			tmp[i] = Feval(wct->valform);
			tra = wct->next;
		}

		/* now specbind them */
		cl_loop_destructuring_bind(specbind, wc->varform, tmp[0]);
		tra = wc->next;
		for (i = 1; !NILP(tra); i++) {
			cl_loop_with_clause_t *wct = get_dynacat(tra);
			cl_loop_destructuring_bind(
				specbind, wct->varform, tmp[i]);
			tra = wct->next;
		}
	}
}

static inline void
cl_loop_perform_colappnco_pro(cl_loop_accu_clause_t *ac)
	__attribute__((always_inline));
static inline void
cl_loop_perform_colappnco_pro(cl_loop_accu_clause_t *ac)
{
	if (ac->into == Qnull_pointer) {
		/* generate a random symbol */
		ac->into = Qanon_acn;
	}
	specbind(ac->into, ac->cur = Qnil);
}

static inline void
cl_loop_perform_countsum_pro(cl_loop_accu_clause_t *ac)
	__attribute__((always_inline));
static inline void
cl_loop_perform_countsum_pro(cl_loop_accu_clause_t *ac)
{
	if (ac->into == Qnull_pointer) {
		/* generate a random symbol */
		ac->into = Qanon_acn;
	}
	specbind(ac->into, ac->cur = Qzero);
}

static inline void
cl_loop_perform_maximise_pro(cl_loop_accu_clause_t *ac)
	__attribute__((always_inline));
static inline void
cl_loop_perform_maximise_pro(cl_loop_accu_clause_t *ac)
{
	if (ac->into == Qnull_pointer) {
		/* generate a random symbol */
		ac->into = Qanon_acn;
	}
	specbind(ac->into, ac->cur = Vninfinity);
}

static inline void
cl_loop_perform_minimise_pro(cl_loop_accu_clause_t *ac)
	__attribute__((always_inline));
static inline void
cl_loop_perform_minimise_pro(cl_loop_accu_clause_t *ac)
{
	if (ac->into == Qnull_pointer) {
		/* generate a random symbol */
		ac->into = Qanon_acn;
	}
	specbind(ac->into, ac->cur = Vpinfinity);
}

static inline void
cl_loop_perform_repeat_pro(cl_loop_repeat_clause_t *rc)
	__attribute__((always_inline));
static inline void
cl_loop_perform_repeat_pro(cl_loop_repeat_clause_t *rc)
{
	Lisp_Object lctr = Feval(rc->form);
	CHECK_INT(lctr);
	rc->counter = XINT(lctr);
	return;
}

static inline void
cl_loop_perform_initially_pro(cl_loop_inifinret_clause_t *rc)
	__attribute__((always_inline));
static inline void
cl_loop_perform_initially_pro(cl_loop_inifinret_clause_t *rc)
{
	Feval(rc->form);
	return;
}

static hentry_t
cl_loop_next_hentry(hentry_t e, const hash_table_t ht)
{
	const hentry_t term = ht->hentries + ht->size;
	if (e == NULL) {
		e = ht->hentries;
		e--;
	}

	while (e < term && HENTRY_CLEAR_P(++e));

	if (e < term)
		return e;
	else
		return NULL;
}

static void
cl_loop_perform_for_pro_i(cl_loop_for_clause_t *fc)
{
	switch (fc->for_subclause) {
	case FOR_ARITHMETIC_CLAUSE:
		fc->curval = Feval(fc->from);
		fc->curbound = Feval(fc->to);
		fc->curstep = Feval(fc->by);
		break;
	case FOR_IN_SUBLIST_CLAUSE:
		fc->curbound = Feval(fc->inonacross);
		/* error handling here, make sure curbound is a cons */
		CHECK_CONS(fc->curbound);
		fc->curval = XCAR(fc->curbound);
		break;
	case FOR_ON_SUBLIST_CLAUSE:
		fc->curbound = Feval(fc->inonacross);
		CHECK_CONS(fc->curbound);
		fc->curval = fc->curbound;
		break;
	case FOR_ACROSS_ARRAY_CLAUSE:
		fc->curbound = Feval(fc->inonacross);
		fc->bound = XINT(Flength(fc->curbound));
		/* CHECK_ARRAY(fc->curbound); */
		fc->counter = 0;
		fc->curval = Faref(fc->curbound, Qzero);
		break;
	case FOR_EQUALS_THEN_CLAUSE:
		fc->curval = Feval(fc->equals);
		fc->counter = 0;
		break;
	case FOR_OF_HASHTABLE_CLAUSE: {
		hentry_t e;
		fc->curbound = Feval(fc->inonacross);
		e = cl_loop_next_hentry(
			NULL, XHASH_TABLE(fc->curbound));
		if ((fc->ptr1 = e) == NULL) {
			return;
		}
		fc->curval = Qnil;
		return;
	}
	case FOR_INVALID_CLAUSE:
	default:
		/* there are `for' subclauses without stuff in the prologue */
		break;
	}
	return;
}

static void
cl_loop_perform_for_pro_b(cl_loop_for_clause_t *fc)
{
	switch (fc->for_subclause) {
	case FOR_ARITHMETIC_CLAUSE:
	case FOR_IN_SUBLIST_CLAUSE:
	case FOR_ON_SUBLIST_CLAUSE:
	case FOR_ACROSS_ARRAY_CLAUSE:
	case FOR_EQUALS_THEN_CLAUSE:
		cl_loop_destructuring_bind(specbind, fc->form1, fc->curval);
		break;

	case FOR_OF_HASHTABLE_CLAUSE: {
		hentry_t e = fc->ptr1;
		if (e == NULL) {
			return;
		}
		if (!NILP(fc->hash_keyvar)) {
			cl_loop_destructuring_bind(
				specbind, fc->hash_keyvar, e->key);
		}
		if (!NILP(fc->hash_valvar)) {
			cl_loop_destructuring_bind(
				specbind, fc->hash_valvar, e->value);
		}
		return;
	}
	case FOR_INVALID_CLAUSE:
	default:
		/* there are `for' subclauses without stuff in the prologue */
		break;
	}
	return;
}

static void
cl_loop_perform_for_pro(cl_loop_for_clause_t *fc)
{
	if (fc->depth == 1) {
		/* optimise for the trivial case */
		cl_loop_perform_for_pro_i(fc);
		cl_loop_perform_for_pro_b(fc);
		return;
	} else {
		Lisp_Object tra;

		cl_loop_perform_for_pro_i(fc);
		tra = fc->next;
		while (!NILP(tra)) {
			cl_loop_for_clause_t *fct = get_dynacat(tra);
			cl_loop_perform_for_pro_i(fct);
			tra = fct->next;
		}

		/* now specbind them */
		cl_loop_perform_for_pro_b(fc);
		tra = fc->next;
		while (!NILP(tra)) {
			cl_loop_for_clause_t *fct = get_dynacat(tra);
			cl_loop_perform_for_pro_b(fct);
			tra = fct->next;
		}
	}
}

static void
cl_loop_perform_for_i(cl_loop_for_clause_t *fc)
{
	/* non stepping stuff */
	switch (fc->for_subclause) {
	case FOR_EQUALS_THEN_CLAUSE:
		if (fc->counter++) {
			cl_loop_destructuring_bind(
				(cl_loop_binder_f)Fset, fc->form1,
				fc->curval = Feval(fc->then));
		}
		return;
	case FOR_INVALID_CLAUSE:
	case FOR_ARITHMETIC_CLAUSE:
	case FOR_IN_SUBLIST_CLAUSE:
	case FOR_ON_SUBLIST_CLAUSE:
	case FOR_ACROSS_ARRAY_CLAUSE:
	case FOR_OF_HASHTABLE_CLAUSE:
	default:
		break;
	}
	return;
}

static int
cl_loop_perform_for_b(cl_loop_for_clause_t *fc)
{
	switch (fc->for_subclause) {
	case FOR_ARITHMETIC_CLAUSE:
	case FOR_IN_SUBLIST_CLAUSE:
	case FOR_ON_SUBLIST_CLAUSE:
	case FOR_ACROSS_ARRAY_CLAUSE:
		/* bind to the value computed during the last iteration */
		cl_loop_destructuring_bind(
			(cl_loop_binder_f)Fset, fc->form1, fc->curval);
	case FOR_INVALID_CLAUSE:
	case FOR_OF_HASHTABLE_CLAUSE:
	case FOR_EQUALS_THEN_CLAUSE:
	default:
		break;
	}

	/* most clauses step in this fun */
	switch (fc->for_subclause) {
	case FOR_EQUALS_THEN_CLAUSE:
		return 1;
	case FOR_ARITHMETIC_CLAUSE:
		fc->curval = ent_binop(fc->byop, fc->curval, fc->curstep);
		if (!fc->torel_strictp) {
			return ent_binrel2(fc->torel, ASE_BINARY_REL_EQUALP,
					   fc->curval, fc->curbound);
		} else {
			return ent_binrel(fc->torel, fc->curval, fc->curbound);
		}
		break;
	case FOR_IN_SUBLIST_CLAUSE:
		/* error handling here, make sure curbound is a cons */
		fc->curbound = XCDR(fc->curbound);
		if (NILP(fc->curbound))
			return 0;
		fc->curval = XCAR(fc->curbound);
		return 1;
		break;
	case FOR_ON_SUBLIST_CLAUSE:
		/* error handling here, make sure curbound is a cons */
		if (NILP(fc->curval = XCDR(fc->curval)))
			return 0;
		return 1;
		break;
	case FOR_ACROSS_ARRAY_CLAUSE:
		fc->counter++;
		if (fc->counter >= fc->bound)
			return 0;
		fc->curval = Faref(fc->curbound, make_int(fc->counter));
		return 1;
		break;
	case FOR_OF_HASHTABLE_CLAUSE: {
		hentry_t e = fc->ptr1;
		if (e == NULL) {
			return 0;
		}
		if (!NILP(fc->hash_keyvar)) {
			cl_loop_destructuring_bind(
				(cl_loop_binder_f)Fset,
				fc->hash_keyvar, e->key);
		}
		if (!NILP(fc->hash_valvar)) {
			cl_loop_destructuring_bind(
				(cl_loop_binder_f)Fset,
				fc->hash_valvar, e->value);
		}
		fc->ptr1 = cl_loop_next_hentry(e, XHASH_TABLE(fc->curbound));
		return 1;
	}
	case FOR_INVALID_CLAUSE:
	default:
		break;
	}
	return 1;
}

static inline int
cl_loop_perform_for(cl_loop_for_clause_t *fc)
{
	if (fc->depth == 1) {
		/* optimise for the trivial case */
		cl_loop_perform_for_i(fc);
		return cl_loop_perform_for_b(fc);
	} else {
		Lisp_Object tra;
		int state;

		cl_loop_perform_for_i(fc);
		tra = fc->next;
		while (!NILP(tra)) {
			cl_loop_for_clause_t *fct = get_dynacat(tra);
			cl_loop_perform_for_i(fct);
			tra = fct->next;
		}

		/* now specbind them */
		state = cl_loop_perform_for_b(fc);
		tra = fc->next;
		while (!NILP(tra)) {
			cl_loop_for_clause_t *fct = get_dynacat(tra);
			state &= cl_loop_perform_for_b(fct);
			tra = fct->next;
		}
		return state;
	}
}

static inline int
cl_loop_perform_do(cl_loop_do_clause_t *dc)
{
	Feval(dc->form);
	return 1;
}

static inline int
cl_loop_perform_repeat(cl_loop_repeat_clause_t *rc)
{
	if (--rc->counter > 0) {
		return 1;
	}
	return 0;
}

static inline int
cl_loop_perform_collect(cl_loop_accu_clause_t *ac)
{
	if (!NILP(ac->cur))
		ac->cur = XCDR(ac->cur) = Fcons(Feval(ac->form), Qnil);
	else {
		Fset(ac->into, ac->cur = Fcons(Feval(ac->form), Qnil));
	}
	return 1;
}

static inline int
cl_loop_perform_append(cl_loop_accu_clause_t *ac)
{
	Lisp_Object form = Feval(ac->form);
	CHECK_CONS(form);
	if (!NILP(ac->cur))
		XCDR(ac->cur) = form;
	else {
		Fset(ac->into, ac->cur = form);
	}
	while (!NILP(XCDR(ac->cur)) && CONSP(XCDR(ac->cur)))
		ac->cur = XCDR(ac->cur);
	if (CONSP(ac->cur) && NILP(XCDR(ac->cur)))
		return 1;
	else
		return wrong_type_argument(Qlistp, form);
}

static inline int
cl_loop_perform_nconc(cl_loop_accu_clause_t *ac)
{
	Lisp_Object form = Feval(ac->form);
	if (!NILP(ac->cur) && CONSP(ac->cur)) {
		XCDR(ac->cur) = form;
	} else {
		Fset(ac->into, ac->cur = form);
	}
	while (CONSP(ac->cur) &&
	       !NILP(XCDR(ac->cur)) &&
	       CONSP(XCDR(ac->cur)))
		ac->cur = XCDR(ac->cur);
	return 1;
}

static inline int
cl_loop_perform_count(cl_loop_accu_clause_t *ac)
{
	if (!NILP(Feval(ac->form))) {
		Fset(ac->into, ac->cur = make_int(XINT(ac->cur)+1));
	}
	return 1;
}

static inline int
cl_loop_perform_sum(cl_loop_accu_clause_t *ac)
{
	Lisp_Object form = Feval(ac->form);
	CHECK_NUMBER(form);
	Fset(ac->into,
	     ac->cur = ent_binop(ASE_BINARY_OP_SUM, ac->cur, form));
	return 1;
}

static inline int
cl_loop_perform_maximise(cl_loop_accu_clause_t *ac)
{
	Lisp_Object form = Feval(ac->form);
	CHECK_NUMBER(form);
	if (ent_binrel(ASE_BINARY_REL_GREATERP, form, ac->cur))
		Fset(ac->into, ac->cur = form);
	return 1;
}

static inline int
cl_loop_perform_minimise(cl_loop_accu_clause_t *ac)
{
	Lisp_Object form = Feval(ac->form);
	CHECK_NUMBER(form);
	if (ent_binrel(ASE_BINARY_REL_LESSP, form, ac->cur))
		Fset(ac->into, ac->cur = form);
	return 1;
}

static inline Lisp_Object
cl_loop_perform_accu_epi()
	__attribute__((always_inline));
static inline Lisp_Object
cl_loop_perform_accu_epi(
	Lisp_Object *SXE_UNUSED(result), cl_loop_accu_clause_t *ac)
{
	return symbol_value(XSYMBOL(ac->into));
}

static inline Lisp_Object
cl_loop_perform_finally_epi()
	__attribute__((always_inline));
static inline Lisp_Object
cl_loop_perform_finally_epi(
	Lisp_Object *SXE_UNUSED(result), cl_loop_inifinret_clause_t *rc)
{
	return Feval(rc->form);
}

static inline Lisp_Object
cl_loop_perform_return_epi()
	__attribute__((always_inline));
static inline Lisp_Object
cl_loop_perform_return_epi(
	Lisp_Object *result, cl_loop_inifinret_clause_t *rc)
{
	return *result = Feval(rc->form);
}


static int
cl_loop_prologue(Lisp_Object clause)
{
	void *emp = NULL;

	emp = get_dynacat(clause);
	if (EQ(get_dynacat_type(clause), Qcl_loop_repeat_clause)) {
		cl_loop_perform_repeat_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_for_clause)) {
		cl_loop_perform_for_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_with_clause)) {
		cl_loop_perform_with_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_collect_clause)) {
		cl_loop_perform_colappnco_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_append_clause)) {
		cl_loop_perform_colappnco_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_nconc_clause)) {
		cl_loop_perform_colappnco_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_count_clause)) {
		cl_loop_perform_countsum_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_sum_clause)) {
		cl_loop_perform_countsum_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_maximise_clause)) {
		cl_loop_perform_maximise_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_minimise_clause)) {
		cl_loop_perform_minimise_pro(emp);
		return 1;
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_initially_clause)) {
		cl_loop_perform_initially_pro(emp);
		return 1;
	}

	return 1;
}

static Lisp_Object
cl_loop_epilogue(Lisp_Object *result, Lisp_Object clause)
{
	void *emp = NULL;

	emp = get_dynacat(clause);
	if (EQ(get_dynacat_type(clause), Qcl_loop_collect_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_append_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_nconc_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_count_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_sum_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_maximise_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_minimise_clause)) {
		return cl_loop_perform_accu_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_return_clause)) {
		return cl_loop_perform_return_epi(result, emp);
	}
	if (EQ(get_dynacat_type(clause), Qcl_loop_finally_clause)) {
		return cl_loop_perform_finally_epi(result, emp);
	}
	return Qnull_pointer;
}

static int
cl_loop_iteration(Lisp_Object clause)
{
	void *emp = NULL;

	emp = get_dynacat(clause);
	if (EQ(get_dynacat_type(clause), Qcl_loop_repeat_clause))
		return cl_loop_perform_repeat(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_for_clause))
		return cl_loop_perform_for(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_do_clause))
		return cl_loop_perform_do(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_collect_clause))
		return cl_loop_perform_collect(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_append_clause))
		return cl_loop_perform_append(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_nconc_clause))
		return cl_loop_perform_nconc(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_count_clause))
		return cl_loop_perform_count(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_sum_clause))
		return cl_loop_perform_sum(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_maximise_clause))
		return cl_loop_perform_maximise(emp);
	if (EQ(get_dynacat_type(clause), Qcl_loop_minimise_clause))
		return cl_loop_perform_minimise(emp);

	return 0;
}

static int
cl_loop_dllist_map(int(*fun)(Lisp_Object), dllist_t dll)
{
	int state;
	dllist_item_t item = dllist_first(dll);

	if (item == NULL) {
		return 0;
	}

	state = 1;
	while (item) {
		state &= fun((Lisp_Object)item->item);
		item = item->next;
	}
	return state;
}

static Lisp_Object
cl_loop_dllist_map_return(
	Lisp_Object *result,
	Lisp_Object(*fun)(Lisp_Object*, Lisp_Object), dllist_t dll)
{
	dllist_item_t item = dllist_first(dll);
	Lisp_Object ret = Qnil;

	if (item == NULL) {
		return Qnil;
	}

	while (item) {
		ret = fun(result, (Lisp_Object)item->item);
		item = item->next;
	}
	if (!EQ(ret, Qnull_pointer))
		return ret;
	else
		return Qnil;
}

static Lisp_Object
cl_loop_perform(cl_loop_sentence_t *lsen)
{
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t epi = XDLLIST(lsen->epilogue);
	dllist_t iter = XDLLIST(lsen->iteration);
	int speccount = specpdl_depth();
	Lisp_Object res;

	lsen->state = 1;

	/* traverse the prologue */
	cl_loop_dllist_map(cl_loop_prologue, pro);
	/* traverse the iteration */
	while (lsen->state) {
		QUIT;
		lsen->state = cl_loop_dllist_map(cl_loop_iteration, iter);
	}
	/* traverse the epilogue */
	lsen->result = Qnull_pointer;
	res = cl_loop_dllist_map_return(&lsen->result, cl_loop_epilogue, epi);

	unbind_to(speccount, Qnil);
	if (lsen->result)
		return lsen->result;
	else
		return res;
}


/* ###autoload */
DEFUN("cl:loop-sentence", Fcl_loop_sentence, 0, UNEVALLED, 0, /*
Part of The Common Lisp loop macro.
See: `cl:loop'
							      */
      (args))
{
	Lisp_Object loop_sentence = cl_loop_make_sentence();
	Lisp_Object context = Qnil, token = Qnil;
	cl_loop_sentence_t *lsen = get_dynacat(loop_sentence);
	int parse_result;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	GCPRO4(args, loop_sentence, context, token);

	/* now parse the stuff */
	parse_result = cl_loop_yyparse(&args, lsen, &context, &token);
	(void)parse_result;

	UNGCPRO;
	return loop_sentence;
}

DEFUN("cl:loop*", Fcl_loopX, 1, 1, 0, /*
Execute LOOP-SENTENCE.
*/
      (loop_sentence))
{
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;

	CHECK_CL_LOOP_SENTENCE(loop_sentence);

	GCPRO2(result, loop_sentence);

	result = cl_loop_perform(XCL_LOOP_SENTENCE(loop_sentence));

	UNGCPRO;
	return result;
}

DEFUN("cl:loop", Fcl_loop, 0, UNEVALLED, 0, /*
(loop CLAUSE...): The Common Lisp loop macro.

Overview of valid clauses:
  for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM,
  for VAR in LIST by FUNC, for VAR on LIST by FUNC, for VAR = INIT then EXPR,
  for VAR across ARRAY, repeat NUM, with VAR = INIT, while COND, until COND,
  always COND, never COND, thereis COND, collect EXPR into VAR,
  append EXPR into VAR, nconc EXPR into VAR, sum EXPR into VAR,
  count EXPR into VAR, maximize EXPR into VAR, minimize EXPR into VAR,
  if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  do EXPRS..., initially EXPRS..., finally EXPRS..., return EXPR,
  finally return EXPR, named NAME.

The loop macro consists of a series of clauses, which do things like
iterate variables, set conditions for exiting the loop, accumulating values
to be returned as the return value of the loop, and executing arbitrary
blocks of code.  Each clause is proceed in turn, and the loop executes its
body repeatedly until an exit condition is hit.

It's important to understand that loop clauses such as `for' and `while',
which look like loop-establishing constructs, don't actually *establish* a
loop; the looping is established by the `loop' clause itself, which will
repeatedly process its body until told to stop.  `while' merely establishes
a condition which, when true, causes the loop to finish, and `for' sets a
variable to different values on each iteration (e.g. successive elements of
a list) and sets an exit condition when there are no more values.  This
means, for example, that if two `for' clauses appear, you don't get two
nested loops, but instead two variables that are stepped in parallel, and
two exit conditions, either of which, if triggered, will cause the loop to
end.  Similarly for a loop with a `for' and a `while' clause.  For example:

(loop
  for x in list
  while x
  do ...)

In each successive iteration, X is set to the next element of the list.  If
there are no more elements, or if any element is nil (the `while' clause),
the loop exits.  Otherwise, the block of code following `do' is executed.)

This example also shows that some clauses establish variable bindings --
essentially like a `let' binding -- and that following clauses can
reference these variables.  Furthermore, the entire loop is surrounded by a
block named nil (unless the `named' clause is given), so you can return
from the loop using the macro `return'. (The other way to exit the loop is
through the macro `loop-finish'.  The difference is that some loop clauses
establish or accumulate a value to be returned, and `loop-finish' returns
this. `return', however, can only return an explicitly-specified value.
NOTE CAREFULLY: There is a loop clause called `return' as well as a
standard Lisp macro called `return'.  Normally they work similarly; but if
you give the loop a name with `named', you will need to use the macro
`return-from'.)

Another extremely useful feature of loops is called "destructuring".  If,
in place of VAR, a list (possibly dotted, possibly a tree of arbitary
complexity) is given, the value to be assigned is assumed to have a similar
structure to the list given, and variables in the list will be matched up
with corresponding elements in the structure.  For example:

(loop
  for (x y) in '((foo 1) (bar 2) (baz 3))
  do (puthash x y some-hash-table))

will add three elements to a hash table, mapping foo -> 1, bar -> 2, and
baz -> 3.  As other examples, you can conveniently process alists using

(loop for (x . y) in alist do ...)

and plists using

(loop for (x y) on plist by #'cddr do ...)

Destructuring is forgiving in that mismatches in the number of elements on
either size will be handled gracefully, either by ignoring or initializing
to nil.

If you don't understand how a particular loop clause works, create an
example and use `macroexpand-sexp' to expand the macro.

In greater detail, valid clauses are:

(NOTE: Keywords in lowercase; slashes separate different possibilities
for keywords, some of which are synonymous; brackets indicate optional
parts of the clause.  In all of the clauses with `being', the word `being',
the words `each' or `the', and the difference between singular and plural
keywords are all just syntactic sugar.  Stylistically, you should write
either `being each foo' or `being the foos'.)

  for VAR from/upfrom/downfrom NUM1 to/upto/downto/above/below NUM2 [by NUMSTEP]
    Step VAR across numbers.  `upfrom', `upto', and `below' explicitly
    indicate upward stepping; `downfrom', `downto', and `above' explicitly
    indicate downward stepping. (If none of these is given, the default is
    upward.) `to', `upto', and `downto' cause stepping to include NUM2 as
    the last iteration, while `above' and `below' stop just before reaching
    NUM2.  `by' can be given to indicate a stepping increment other than 1.

  for VAR in LIST [by FUNC]
    Step VAR over elements of a LIST.  FUNC specifies how to get successive
    sublists and defaults to `cdr'.

  for VAR on LIST [by FUNC]
    Step VAR over tails of a LIST.  FUNC specifies how to get successive
    sublists and defaults to `cdr'.

  for VAR in-ref LIST [by FUNC]
    Step VAR over elements of a LIST, like `for ... in', except the VAR is
    bound using `symbol-macrolet' instead of `let'.  In essence, VAR is set
    to a "reference" to the list element instead of the element itself;
    this us, you can destructively modify the list using `setf' on VAR, and
    any changes to the list will "magically" reflect themselves in
    subsequent uses of VAR.

  for VAR = INIT [then EXPR]
    Set VAR on each iteration of the loop.  If only INIT is given, use it
    on each iteration.  Otherwise, use INIT on the first iteration and EXPR
    on subsequent ones.

  for VAR across/across-ref ARRAY
    Step VAR across a sequence other than a list (string, vector, bit
    vector).  If `across-ref' is given, VAR is bound using
    `symbol-macrolet' instead of `let' -- see above.

  for VAR being each/the element/elements in/of/in-ref/of-ref SEQUENCE [using (index INDEX-VAR)]
    Step VAR across any sequence.  A variable can be specified with a
    `using' phrase to receive the index, starting at 0.  If `in-ref' or
    `of-ref' is given, VAR is bound using `symbol-macrolet' instead of
    `let' -- see above.

  for VAR being each/the hash-key/hash-keys/hash-value/hash-values in/of HASH-TABLE [using (hash-value/hash-key OTHER-VAR)]

  for VAR being each/the hash-key/hash-keys/hash-value/hash-values in/of HASH-TABLE [using (hash-value/hash-key OTHER-VAR)]
    Map VAR over a hash table.  The various keywords are synonymous except
    those that distinguish between keys and values.  The `using' phrase is
    optional and allows both key and value to be bound.

  for VAR being each/the symbol/present-symbol/external-symbol/symbols/present-symbols/external-symbols in/of OBARRAY
    Map VAR over the symbols in an obarray.  All symbol keywords are
    currently synonymous.

  for VAR being each/the extent/extents [in/of BUFFER-OR-STRING] [from POS] [to POS]
    Map VAR over the extents in a buffer or string, defaulting to the
    current buffer, the beginning and the end, respectively.

  for VAR being each/the interval/intervals [in/of BUFFER-OR-STRING] [property PROPERTY] [from POS] [to POS]
    Map VAR over the intervals without property change in a buffer or
    string, defaulting to the current buffer, the beginning and the end,
    respectively.  If PROPERTY is given, iteration occurs using
    `next-single-property-change'; otherwise, using
    `next-property-change'.

  for VAR being each/the window/windows [in/of FRAME]
    Step VAR over the windows in FRAME, defaulting to the selected frame.

  for VAR being each/the frame/frames
    Step VAR over all frames.

  for VAR being each/the buffer/buffers [by FUNC]
    Step VAR over all buffers.  This is actually equivalent to
    `for VAR in (buffer-list) [by FUNC]'.

  for VAR being each/the key-code/key-codes/key-seq/key-seqs/key-binding/key-bindings in KEYMAP [using (key-code/key-codes/key-seq/key-seqs/key-binding/key-bindings OTHER-VAR)]
    Map VAR over the entries in a keymap.  Keyword `key-seq' causes
    recursive mapping over prefix keymaps occurring in the keymap, with VAR
    getting the built-up sequence (a vector).  Otherwise, mapping does not
    occur recursively.  `key-code' and `key-seq' refer to what is bound
    (second argument of `define-key'), and `key-binding' what it's bound to
    (third argument of `define-key').

  as VAR ...
    `as' is a synonym for `for'.

  and VAR ...
    `and' clauses have the same syntax as `for' clauses except that the
    variables in the clause are bound in parallel with a preceding
    `and'/`for' clause instead of in series.

  with VAR = INIT
    Set VAR to INIT once, before doing any iterations.

  repeat NUM
    Exit the loop if more than NUM iterations have occurred.

  while COND
    Exit the loop if COND isn't true.

  until COND
    Exit the loop if COND is true.

  collect EXPR [into VAR]
    Push EXPR onto the end of a list of values -- stored either in VAR or a
    temporary variable that will be returned as the return value of the
    loop if it terminates through an exit condition or a call to
    `loop-finish'.

  append EXPR [into VAR]
    Append EXPR (a list) onto the end of a list of values, like `collect'.

  nconc EXPR [into VAR]
    Nconc EXPR (a list) onto the end of a list of values, like `collect'.

  concat EXPR [into VAR]
    Concatenate EXPR (a string) onto the end of a string of values, like
    `collect'.

  vconcat EXPR [into VAR]
    Concatenate EXPR (a vector) onto the end of a vector of values, like
    `collect'.

  bvconcat EXPR [into VAR]
    Concatenate EXPR (a bit vector) onto the end of a bit vector of values,
    like `collect'.

  sum EXPR [into VAR]
    Add EXPR to a value, like `collect'.

  count EXPR [into VAR]
    If EXPR is true, increment a value by 1, like `collect'.

  maximize EXPR [into VAR]
    IF EXPR is greater than a value, replace the value with EXPR, like
    `collect'.

  minimize EXPR [into VAR]
    IF EXPR is less than a value, replace the value with EXPR, like
    `collect'.

  always COND
    If COND is true, continue the loop and set the loop return value (the
    same value that's manipulated by `collect' and friends and is returned
    by a normal loop exit or an exit using `loop-finish') to t; otherwise,
    exit the loop and return nil.  The effect is to determine and return
    whether a condition is true "always" (all iterations of the loop).

  never COND
    If COND is false, continue the loop and set the loop return value (like
    `always') to t; otherwise, exit the loop and return nil.  The effect
    is to determine and return whether a condition is "never" true (all
    iterations of the loop).

  thereis COND
    If COND is true, exit the loop and return COND.

  if/when COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    If COND is true, execute the directly following clause(s); otherwise,
    execute the clauses following `else'.

  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    If COND is false, execute the directly following clause(s); otherwise, execute the clauses following `else'.

  do EXPRS...
    Execute the expressions (any Lisp forms).

  initially EXPRS...
    Execute EXPR once, before doing any iterations, and after values have
    been set using `with'.

  finally EXPRS...
    Execute EXPR once, directly before the loop terminates.  This will not
    be executed if the loop terminates prematurely as a result of `always',
    `never', `thereis', or `return'.

  return EXPR
    Exit from the loop and return EXPR.

  finally return EXPR
    Specify the value to be returned when the loop exits. (Unlike `return',
    this doesn't cause the loop to immediately exit; it will exit whenever
    it normally would have.) This takes precedence over a return value
    specified with `collect' and friends or `always' and friends.

  named NAME
    Specify the name for block surrounding the loop, in place of nil.
    (See `block'.)
					    */
      (args))
{
	Lisp_Object loop_sentence = Qnil;
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;
	cl_loop_sentence_t *lsen;

	/* bullshit case */
	if (NILP(args)) {
		while (1) {
			QUIT;
		}
		return Qnil;
	}

	GCPRO2(result, loop_sentence);

	loop_sentence = Fcl_loop_sentence(args);
	lsen = get_dynacat(loop_sentence);
	result = cl_loop_perform(lsen);

	UNGCPRO;
	return result;
}

/* ###autoload */
DEFUN("cl:do", Fcl_do, 2, UNEVALLED, 0, /*
The Common Lisp `do' loop.
Format is: (do ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)
*/
      (args))
{
	/* This function can GC */
	Lisp_Object varform = XCAR(args);
	Lisp_Object endform = XCAR(XCDR(args));
	Lisp_Object body = XCDR(XCDR(args));
	Lisp_Object result = Qnil;
	Lisp_Object endtest = Qnil, resultform = Qnil;
	struct gcpro gcpro1, gcpro2;
	int speccount = specpdl_depth();

	CHECK_CONS(varform);
	CHECK_CONS(endform);

	GCPRO2(endtest, resultform);

	endtest = XCAR(endform);
	resultform = XCDR(endform);

	result = emodcl_do(
		varform, endtest, resultform, body,
		emodcl_initialise_vars, emodcl_step_vars);

	unbind_to(speccount, Qnil);
	UNGCPRO;
	return result;
}

/* ###autoload */
DEFUN("cl:do*", Fcl_doX, 2, UNEVALLED, 0, /*
The Common Lisp `do*' loop.
Format is: (do* ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)
*/
      (args))
{
	/* This function can GC */
	Lisp_Object varform = XCAR(args);
	Lisp_Object endform = XCAR(XCDR(args));
	Lisp_Object body = XCDR(XCDR(args));
	Lisp_Object result = Qnil;
	Lisp_Object endtest = Qnil, resultform = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
	int speccount = specpdl_depth();

	CHECK_CONS(varform);
	CHECK_CONS(endform);

	GCPRO4(result, endtest, resultform, body);

	endtest = XCAR(endform);
	resultform = XCDR(endform);

	result = emodcl_do(
		varform, endtest, resultform, body,
		emodcl_initialise_vars_star, emodcl_step_vars_star);

	unbind_to(speccount, Qnil);
	UNGCPRO;
	return result;
}


/* ###autoload */

DEFUN("cl:dotimes", Fcl_dotimes, 1, UNEVALLED, 0, /*
The Common Lisp `dotimes' loop.
Format is: (dotimes (VAR COUNT [RESULT]) BODY...)

Loop a certain number of times. Evaluate BODY with VAR bound to
successive integers from 0, inclusive,to COUNT, exclusive.  Then
evaluate RESULT to get return value, default nil.
						  */
      (args))
{
	/* This function can GC */
	Lisp_Object varform = XCAR(args);
	Lisp_Object body = XCDR(args);
	Lisp_Object result = Qnil;
	Lisp_Object varsym = Qnil, varcnt = Qnil, resultform = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	int speccount = specpdl_depth();
	size_t j;

	CHECK_CONS(varform);
	CHECK_CONS(XCDR(varform));
	CHECK_SYMBOL(varsym = XCAR(varform));

	GCPRO3(result, varform, body);
	CHECK_NATNUM(varcnt = Feval(XCAR(XCDR(varform))));

	specbind(varsym, Qzero);
	for (j = 0; j < XUINT(varcnt); j++) {
		Fset(varsym, make_int(j));
		LIST_LOOP_2(form, body) {
			Feval(form);
		}
	}

	if (!NILP(resultform = XCDR(XCDR(varform)))) {
		LIST_LOOP_2(form, resultform) {
			result = Feval(form);
		}
	}

	unbind_to(speccount, Qnil);
	UNGCPRO;
	return result;
}

/* ###autoload */
DEFUN("cl:dolist", Fcl_dolist, 1, UNEVALLED, 0, /*
The Common Lisp `dolist' loop.
Format is: (dolist (VAR LIST [RESULT]) BODY...)
loop over a list.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Then evaluate RESULT to get return value, default nil.
						*/
      (args))
{
	/* This function can GC */
	Lisp_Object varform = XCAR(args);
	Lisp_Object body = XCDR(args);
	Lisp_Object result = Qnil;
	Lisp_Object varsym = Qnil, list = Qnil, resultform = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	int speccount = specpdl_depth();

	CHECK_CONS(varform);
	CHECK_CONS(XCDR(varform));
	CHECK_SYMBOL(varsym = XCAR(varform));

	GCPRO3(result, varform, body);
	list = Feval(XCAR(XCDR(varform)));
	if (!NILP(list)) {
		CHECK_CONS(list);
	} else {
		/* nothing to do */
		goto get_result;
	}

	specbind(varsym, Qnil);
	while (!NILP(list)) {
		Fset(varsym, XCAR(list));
		LIST_LOOP_2(form, body) {
			Feval(form);
		}
		list = XCDR(list);
	}

get_result:
	if (!NILP(resultform = XCDR(XCDR(varform)))) {
		LIST_LOOP_2(form, resultform) {
			result = Feval(form);
		}
	}

	unbind_to(speccount, Qnil);
	UNGCPRO;
	return result;
}

extern Lisp_Object check_obarray(Lisp_Object obarray);
/* ###autoload */
DEFUN("cl:do-symbols", Fcl_do_symbols, 1, UNEVALLED, 0, /*
The Common Lisp `dolist' loop.
Format is: (do-symbols (VAR [OBARRAY [RESULT]]) BODY...)
loop over all symbols.
Evaluate BODY with VAR bound to each interned symbol, or to each symbol
from OBARRAY.
							*/
      (args))
{
	/* This function can GC */
	Lisp_Object varform = XCAR(args);
	Lisp_Object body = XCDR(args);
	Lisp_Object result = Qnil;
	Lisp_Object varsym = Qnil, obarr = Qnil, resultform = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	int speccount = specpdl_depth();
	REGISTER int j;

	CHECK_CONS(varform);
	CHECK_SYMBOL(varsym = XCAR(varform));

	GCPRO3(result, varform, body);

	if (NILP(XCDR(varform))) {
		obarr = Vobarray;
	} else {
		CHECK_CONS(XCDR(varform));
		obarr = Feval(XCAR(XCDR(varform)));
	}
	obarr = check_obarray(obarr);

	specbind(varsym, Qnil);
	for (j = XVECTOR_LENGTH(obarr)-1; j >= 0; j--) {
		Lisp_Object tail = XVECTOR_DATA(obarr)[j];
		if (SYMBOLP(tail))
			while (1) {
				Lisp_Symbol *next;
				Fset(varsym, tail);
				LIST_LOOP_2(form, body) {
					Feval(form);
				}
				next = symbol_next(XSYMBOL(tail));
				if (!next)
					break;
				XSETSYMBOL(tail, next);
			}
	}

	if (!NILP(XCDR(varform)) &&
	    !NILP(resultform = XCDR(XCDR(varform)))) {
		LIST_LOOP_2(form, resultform) {
			result = Feval(form);
		}
	}

	unbind_to(speccount, Qnil);
	UNGCPRO;
	return result;
}

/* ###autoload */
DEFUN("cl:do-all-symbols", Fcl_do_all_symbols, 1, UNEVALLED, 0, /*
The Common Lisp `dolist' loop.
Format is: (do-all-symbols (VAR [RESULT]) BODY...)
*/
      (args))
{
	/* This function can GC */
	Lisp_Object varform = XCAR(args);
	Lisp_Object body = XCDR(args);
	Lisp_Object result = Qnil;
	Lisp_Object varsym = Qnil, obarr = Qnil, resultform = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;
	int speccount = specpdl_depth();
	REGISTER int j;

	CHECK_CONS(varform);
	CHECK_SYMBOL(varsym = XCAR(varform));

	GCPRO3(result, varform, body);

	obarr = Vobarray;

	specbind(varsym, Qnil);
	for (j = XVECTOR_LENGTH(obarr)-1; j >= 0; j--) {
		Lisp_Object tail = XVECTOR_DATA(obarr)[j];
		if (SYMBOLP(tail))
			while (1) {
				Lisp_Symbol *next;
				Fset(varsym, tail);
				LIST_LOOP_2(form, body) {
					Feval(form);
				}
				next = symbol_next(XSYMBOL(tail));
				if (!next)
					break;
				XSETSYMBOL(tail, next);
			}
	}

	if (!NILP(resultform = XCDR(varform))) {
		LIST_LOOP_2(form, resultform) {
			result = Feval(form);
		}
	}

	unbind_to(speccount, Qnil);
	UNGCPRO;
	return result;
}


/* simplified initialisation */
void
INIT(void)
{
	DEFSUBR(Fcl_loop_sentence);
	DEFSUBR(Fcl_loop);
	DEFSUBR(Fcl_loopX);
	DEFSUBR(Fcl_do);
	DEFSUBR(Fcl_doX);
	DEFSUBR(Fcl_dotimes);
	DEFSUBR(Fcl_dolist);
	DEFSUBR(Fcl_do_symbols);
	DEFSUBR(Fcl_do_all_symbols);

	DEFSYMBOL(Qcl_loop_sentence);
	DEFSYMBOL(Qcl_loop_sentence_p);
	DEFSYMBOL(Qcl_loop_for_clause);
	DEFSYMBOL(Qcl_loop_for_clause_p);
	DEFSYMBOL(Qcl_loop_do_clause);
	DEFSYMBOL(Qcl_loop_do_clause_p);
	DEFSYMBOL(Qcl_loop_with_clause);
	DEFSYMBOL(Qcl_loop_with_clause_p);
	DEFSYMBOL(Qcl_loop_repeat_clause);
	DEFSYMBOL(Qcl_loop_repeat_clause_p);
	DEFSYMBOL(Qcl_loop_append_clause);
	DEFSYMBOL(Qcl_loop_append_clause_p);
	DEFSYMBOL(Qcl_loop_collect_clause);
	DEFSYMBOL(Qcl_loop_collect_clause_p);
	DEFSYMBOL(Qcl_loop_nconc_clause);
	DEFSYMBOL(Qcl_loop_nconc_clause_p);
	DEFSYMBOL(Qcl_loop_return_clause);
	DEFSYMBOL(Qcl_loop_return_clause_p);
	DEFSYMBOL(Qcl_loop_finally_clause);
	DEFSYMBOL(Qcl_loop_finally_clause_p);
	DEFSYMBOL(Qcl_loop_initially_clause);
	DEFSYMBOL(Qcl_loop_initially_clause_p);
	DEFSYMBOL(Qcl_loop_count_clause);
	DEFSYMBOL(Qcl_loop_count_clause_p);
	DEFSYMBOL(Qcl_loop_sum_clause);
	DEFSYMBOL(Qcl_loop_sum_clause_p);
	DEFSYMBOL(Qcl_loop_minimise_clause);
	DEFSYMBOL(Qcl_loop_minimise_clause_p);
	DEFSYMBOL(Qcl_loop_maximise_clause);
	DEFSYMBOL(Qcl_loop_maximise_clause_p);

	DEFSYMBOL(Qfor);
	DEFSYMBOL(Qas);
	DEFSYMBOL(Qfrom);
	DEFSYMBOL(Qdownfrom);
	DEFSYMBOL(Qupfrom);
	DEFSYMBOL(Qto);
	DEFSYMBOL(Qdownto);
	DEFSYMBOL(Qupto);
	DEFSYMBOL(Qabove);
	DEFSYMBOL(Qbelow);
	DEFSYMBOL(Qby);
	DEFSYMBOL(Qin);
	DEFSYMBOL(Qon);
	DEFSYMBOL(Qthen);
	DEFSYMBOL(Qacross);
	DEFSYMBOL(Qeach);
	DEFSYMBOL(Qthe);
	DEFSYMBOL(Qbeing);
	DEFSYMBOL(Qhash_key);
	DEFSYMBOL(Qhash_keys);
	DEFSYMBOL(Qhash_value);
	DEFSYMBOL(Qhash_values);
	DEFSYMBOL(Qof);
	DEFSYMBOL(Qusing);

	DEFSYMBOL(Qand);
	DEFSYMBOL(Qwith);
	defsymbol(&Qequals, "=");

	DEFSYMBOL(Qappend);
	DEFSYMBOL(Qappending);
	DEFSYMBOL(Qcollect);
	DEFSYMBOL(Qcollecting);
	DEFSYMBOL(Qnconc);
	DEFSYMBOL(Qnconcing);
	DEFSYMBOL(Qinto);
	DEFSYMBOL(Qcount);
	DEFSYMBOL(Qcount);
	DEFSYMBOL(Qsum);
	DEFSYMBOL(Qsumming);
	DEFSYMBOL(Qmaximise);
	DEFSYMBOL(Qmaximising);
	DEFSYMBOL(Qmaximize);
	DEFSYMBOL(Qmaximizing);
	DEFSYMBOL(Qminimise);
	DEFSYMBOL(Qminimising);
	DEFSYMBOL(Qminimize);
	DEFSYMBOL(Qminimizing);

	DEFSYMBOL(Qrepeat);

	DEFSYMBOL(Qdo);
	DEFSYMBOL(Qdoing);

	DEFSYMBOL(Qinitially);
	DEFSYMBOL(Qfinally);

	DEFSYMBOL(Qanon_acn);

	Fprovide(intern("cl-loop"));
}

void
DEINIT(void)
{
	Frevoke(intern("cl-loop"));
}

/* cl-loop.c ends here */
