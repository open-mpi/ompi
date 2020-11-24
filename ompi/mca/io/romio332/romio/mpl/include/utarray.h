/* MPICH notes:
 * - The file name has been changed to avoid conflicts with any system-installed
 *   "utlist.h" header files.
 * - malloc/free/realloc usages have been substituted with utarray_malloc_
 *   (etc.) indirection macros and then changed to use MPL_malloc and friends
 * - add a ut_ptr_icd for the common case of dynamic tables of pointers
 * - intentionally omitted from "mpiimpl.h" in order to require using code to
 *   opt-in
 * [goodell@ 2011-10-04] */
/*
Copyright (c) 2008-2011, Troy D. Hanson   http://uthash.sourceforge.net
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/* a dynamic array implementation using macros
 * see http://uthash.sourceforge.net/utarray
 */
#ifndef UTARRAY_H_INCLUDED
#define UTARRAY_H_INCLUDED

#define UTARRAY_VERSION 1.9.4

#ifdef __GNUC__
#define _UNUSED_ __attribute__ ((__unused__))
#else
#define _UNUSED_
#endif

#include <stddef.h>     /* size_t */
#include <string.h>     /* memset, etc */
#include <stdlib.h>     /* exit */

/* MPICH modification: override these to change what malloc/free/realloc
 * routines are used by the utarray code */
#define utarray_malloc_(x_,class_)     MPL_malloc(x_,class_)
#define utarray_free_(x_)       MPL_free(x_)
#define utarray_realloc_(x_,y_,class_) MPL_realloc(x_,y_,class_)
#define utarray_strdup_(x_)     MPL_strdup(x_)

#ifndef utarray_oom
#define utarray_oom() exit(-1)
#endif

typedef void (ctor_f) (void *dst, const void *src);
typedef void (dtor_f) (void *elt);
typedef void (init_f) (void *elt);
typedef struct {
    size_t sz;
    init_f *init;
    ctor_f *copy;
    dtor_f *dtor;
} UT_icd;

typedef struct {
    unsigned i, n;              /* i: index of next available slot, n: num slots */
    const UT_icd *icd;          /* initializer, copy and destructor functions */
    char *d;                    /* n slots of size icd->sz */
} UT_array;

#define utarray_init(a,_icd) do {                                             \
  memset(a,0,sizeof(UT_array));                                               \
  (a)->icd=_icd;                                                              \
} while (0)

#define utarray_done(a) do {                                                  \
  if ((a)->n) {                                                               \
    if ((a)->icd->dtor) {                                                     \
      size_t _ut_i;                                                           \
      for(_ut_i=0; _ut_i < (a)->i; _ut_i++) {                                 \
        (a)->icd->dtor(utarray_eltptr(a,_ut_i));                              \
      }                                                                       \
    }                                                                         \
    utarray_free_((a)->d);                                                    \
  }                                                                           \
  (a)->n=0;                                                                   \
} while (0)

#define utarray_new(a,_icd, class) do {                                       \
  a=(UT_array*)utarray_malloc_(sizeof(UT_array), class);                      \
  if (a == NULL) utarray_oom();                                               \
  utarray_init(a,_icd);                                                       \
} while (0)

#define utarray_free(a) do {                                                  \
  utarray_done(a);                                                            \
  utarray_free_(a);                                                           \
} while (0)

#define utarray_reserve(a,by,class) do {                                      \
  if (((a)->i+by) > ((a)->n)) {                                               \
    void * d_;                                                                \
    while (((a)->i+by) > ((a)->n)) { (a)->n = ((a)->n ? (2*(a)->n) : 8); }     \
    d_=(char*)utarray_realloc_((a)->d, (a)->n*(a)->icd->sz, class);           \
    if (d_ == NULL) utarray_oom();                                            \
    (a)->d = d_;                                                              \
  }                                                                           \
} while (0)

#define utarray_push_back(a,p,class) do {                                     \
  utarray_reserve(a,1,class);                                                 \
  if ((a)->icd->copy) { (a)->icd->copy(_utarray_eltptr(a,(a)->i++), p); }    \
  else { memcpy(_utarray_eltptr(a,(a)->i++), p, (a)->icd->sz); };             \
} while (0)

#define utarray_pop_back(a) do {                                              \
  if ((a)->icd->dtor) { (a)->icd->dtor(_utarray_eltptr(a,--((a)->i))); }     \
  else { (a)->i--; }                                                          \
} while (0)

#define utarray_extend_back(a,class) do {                                     \
  utarray_reserve(a,1,class);                                                 \
  if ((a)->icd->init) { (a)->icd->init(_utarray_eltptr(a,(a)->i)); }          \
  else { memset(_utarray_eltptr(a,(a)->i),0,(a)->icd->sz); }                  \
  (a)->i++;                                                                   \
} while (0)

#define utarray_len(a) ((a)->i)

#define utarray_eltptr(a,j) ((((unsigned) j) < (a)->i) ? _utarray_eltptr(a,j) : NULL)
#define _utarray_eltptr(a,j) ((char*)((a)->d + ((a)->icd->sz*(j))))

#define utarray_insert(a,p,j,class) do {                                      \
  utarray_reserve(a,1,class);                                                 \
  if (j > (a)->i) break;                                                      \
  if ((j) < (a)->i) {                                                         \
    memmove(_utarray_eltptr(a,(j)+1), _utarray_eltptr(a,j),                  \
             ((a)->i - (j))*((a)->icd->sz));                                  \
  }                                                                           \
  if ((a)->icd->copy) { (a)->icd->copy(_utarray_eltptr(a,j), p); }           \
  else { memcpy(_utarray_eltptr(a,j), p, (a)->icd->sz); };                    \
  (a)->i++;                                                                   \
} while (0)

#define utarray_inserta(a,w,j,class) do {                                     \
  if (utarray_len(w) == 0) break;                                             \
  if (j > (a)->i) break;                                                      \
  utarray_reserve(a,utarray_len(w),class);                                    \
  if ((j) < (a)->i) {                                                         \
    memmove(_utarray_eltptr(a,(j)+utarray_len(w)),                            \
            _utarray_eltptr(a,j),                                             \
            ((a)->i - (j))*((a)->icd->sz));                                   \
  }                                                                           \
  if ((a)->icd->copy) {                                                         \
    size_t _ut_i;                                                             \
    for(_ut_i=0;_ut_i<(w)->i;_ut_i++) {                                       \
      (a)->icd->copy(_utarray_eltptr(a,j+_ut_i), _utarray_eltptr(w,_ut_i));   \
    }                                                                         \
  } else {                                                                    \
    memcpy(_utarray_eltptr(a,j), _utarray_eltptr(w,0),                        \
           utarray_len(w)*((a)->icd->sz));                                    \
  }                                                                           \
  (a)->i += utarray_len(w);                                                   \
} while (0)

#define utarray_resize(dst,num,class) do {                                    \
  size_t _ut_i;                                                               \
  if (dst->i > (size_t)(num)) {                                               \
    if ((dst)->icd->dtor) {                                                   \
      for(_ut_i=num; _ut_i < dst->i; _ut_i++) {                               \
        (dst)->icd->dtor(utarray_eltptr(dst,_ut_i));                          \
      }                                                                       \
    }                                                                         \
  } else if (dst->i < (size_t)(num)) {                                        \
    utarray_reserve(dst,num-dst->i,class);                                    \
    if ((dst)->icd->init) {                                                   \
      for(_ut_i=dst->i; _ut_i < num; _ut_i++) {                               \
        (dst)->icd->init(utarray_eltptr(dst,_ut_i));                          \
      }                                                                       \
    } else {                                                                  \
      memset(_utarray_eltptr(dst,dst->i),0,(dst)->icd->sz*(num-dst->i));      \
    }                                                                         \
  }                                                                           \
  dst->i = num;                                                               \
} while (0)

#define utarray_concat(dst,src,class) do {                                    \
  utarray_inserta((dst),(src),utarray_len(dst),class);                        \
} while (0)

#define utarray_erase(a,pos,len) do {                                         \
  if ((a)->icd->dtor) {                                                       \
    size_t _ut_i;                                                             \
    for(_ut_i=0; _ut_i < len; _ut_i++) {                                      \
      (a)->icd->dtor(utarray_eltptr(a,pos+_ut_i));                            \
    }                                                                         \
  }                                                                           \
  if ((a)->i > (pos+len)) {                                                   \
    memmove(_utarray_eltptr(a,pos), _utarray_eltptr(a,pos+len),              \
            ((a->i)-(pos+len))*((a)->icd->sz));                               \
  }                                                                           \
  (a)->i -= (len);                                                            \
} while (0)

#define utarray_clear(a) do {                                                 \
  if ((a)->i > 0) {                                                           \
    if ((a)->icd->dtor) {                                                     \
      size_t _ut_i;                                                           \
      for(_ut_i=0; _ut_i < (a)->i; _ut_i++) {                                 \
        (a)->icd->dtor(utarray_eltptr(a,_ut_i));                              \
      }                                                                       \
    }                                                                         \
    (a)->i = 0;                                                               \
  }                                                                           \
} while (0)

#define utarray_sort(a,cmp) do {                                              \
  qsort((a)->d, (a)->i, (a)->icd->sz, cmp);                                   \
} while (0)

#define utarray_find(a,v,cmp) bsearch((v),(a)->d,(a)->i,(a)->icd->sz,cmp)

#define utarray_front(a) (((a)->i) ? (_utarray_eltptr(a,0)) : NULL)
#define utarray_next(a,e) (((e)==NULL) ? utarray_front(a) : ((((a)->i) > (utarray_eltidx(a,e)+1)) ? _utarray_eltptr(a,utarray_eltidx(a,e)+1) : NULL))
#define utarray_back(a) (((a)->i) ? (_utarray_eltptr(a,(a)->i-1)) : NULL)
#define utarray_eltidx(a,e) (((char*)(e) >= (char*)((a)->d)) ? (((char*)(e) - (char*)((a)->d))/(a)->icd->sz) : -1)

/* last we pre-define a few icd for common utarrays of ints and strings */
static void utarray_str_cpy(void *dst, const void *src)
{
    char **_src = (char **) src, **_dst = (char **) dst;
    *_dst = (*_src == NULL) ? NULL : (char *) utarray_strdup_(*_src);
}

static void utarray_str_dtor(void *elt)
{
    char **eltc = (char **) elt;
    if (*eltc)
        utarray_free_(*eltc);
}
static const UT_icd ut_str_icd _UNUSED_ =
    { sizeof(char *), NULL, utarray_str_cpy, utarray_str_dtor };
static const UT_icd ut_int_icd _UNUSED_ = { sizeof(int), NULL, NULL, NULL };

/* MPICH additions: */
static const UT_icd ut_ptr_icd _UNUSED_ = { sizeof(void *), NULL, NULL, NULL };

/* These are convenience macros for directly accessing the array.
   Care should be taken when using the returned pointer as the pointer
   can be invalidated by other utarray operations. */
#define ut_int_array(a) ((int*)(a)->d)
#define ut_str_array(a) ((char**)(a)->d)
#define ut_ptr_array(a) ((void**)(a)->d)


#endif /* UTARRAY_H_INCLUDED */
