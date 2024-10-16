/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/*
Copyright (c) 2007-2011, Troy D. Hanson   http://uthash.sourceforge.net
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

/* Argonne changes:

 - some configure-time checking for __typeof() support was added
 - intentionally omitted from "mpl.h" in order to require using code to opt-in
 [goodell@ 2010-12-20]

 - Added _N (for LL_ macros) and _NP (for DL_ and CDL_ macros)
   variants to each macro.  Thease take additional parameters to specify the
   next or next and prev fields, respectively.  The field name should be used in
   those parameters.  E.g., say the struct looks like this:

       struct my_struct {
           struct my_struct *my_next;
           struct my_struct *my_prev;
           int some_data;
       };

   Then, to append an element "my_element" of type my_struct:

       DL_APPEND_NP(my_head, my_element, my_next, my_prev);

   For convenience one can define a macro to eliminate the need to specify the
   field names every time:

       #define MY_STRUCT_DL_APPEND(head, add) \
           DL_APPEND_NP(head, add, my_next, my_prev)

 [buntinas 12/2/2011]
*/

#ifndef UTLIST_H_INCLUDED
#define UTLIST_H_INCLUDED

#define UTLIST_VERSION 1.9.5

#include "mplconfig.h"

/*
 * This file contains macros to manipulate singly and doubly-linked lists.
 *
 * 1. LL_ macros:  singly-linked lists.
 * 2. DL_ macros:  doubly-linked lists.
 * 3. CDL_ macros: circular doubly-linked lists.
 *
 * To use singly-linked lists, your structure must have a "next" pointer.
 * To use doubly-linked lists, your structure must "prev" and "next" pointers.
 * Either way, the pointer to the head of the list must be initialized to NULL.
 *
 * ----------------.EXAMPLE -------------------------
 * struct item {
 *      int id;
 *      struct item *prev, *next;
 * }
 *
 * struct item *list = NULL:
 *
 * int main() {
 *      struct item *item;
 *      ... allocate and populate item ...
 *      DL_APPEND(list, item);
 * }
 * --------------------------------------------------
 *
 * For doubly-linked lists, the append and delete macros are O(1)
 * For singly-linked lists, append and delete are O(n) but prepend is O(1)
 * The sort macro is O(n log(n)) for all types of single/double/circular lists.
 */

#ifdef MPL_HAVE___TYPEOF        /* MPICH modification */
/* These macros use decltype or the earlier __typeof GNU extension.
   As decltype is only available in newer compilers (VS2010 or gcc 4.3+
   when compiling c++ code), this code uses whatever method is needed
   or, for VS2008 where neither is available, uses casting workarounds. */
#ifdef _MSC_VER /* MS compiler */
#if _MSC_VER >= 1600 && defined(__cplusplus)    /* VS2010 or newer in C++ mode */
#define LDECLTYPE(x) decltype(x)
#else /* VS2008 or older (or VS2010 in C mode) */
#define NO_DECLTYPE
#define LDECLTYPE(x) char*
#endif
#else /* GNU, Sun and other compilers */
#define LDECLTYPE(x) __typeof(x)
#endif
#else /* !MPL_HAVE___TYPEOF */
#define NO_DECLTYPE
#define LDECLTYPE(x) char*
#endif /* !MPL_HAVE___TYPEOF */

/* for VS2008 we use some workarounds to get around the lack of decltype,
 * namely, we always reassign our tmp variable to the list head if we need
 * to dereference its prev/next pointers, and save/restore the real head.*/
#ifdef NO_DECLTYPE
#define _SV(elt,list) _tmp = (char*)(list); {char **_alias = (char**)&(list); *_alias = (elt); }
#define _NEXT(elt,list,_next) ((char*)((list)->_next))
#define _NEXTASGN(elt,list,to,_next) { char **_alias = (char**)&((list)->_next); *_alias=(char*)(to); }
#define _PREV(elt,list,_prev) ((char*)((list)->_prev))
#define _PREVASGN(elt,list,to,_prev) { char **_alias = (char**)&((list)->_prev); *_alias=(char*)(to); }
#define _RS(list) { char **_alias = (char**)&(list); *_alias=_tmp; }
#define _CASTASGN(a,b) { char **_alias = (char**)&(a); *_alias=(char*)(b); }
#else
#define _SV(elt,list)
#define _NEXT(elt,list,_next) ((elt)->_next)
#define _NEXTASGN(elt,list,to,_next) ((elt)->_next)=(to)
#define _PREV(elt,list,_prev) ((elt)->_prev)
#define _PREVASGN(elt,list,to,_prev) ((elt)->_prev)=(to)
#define _RS(list)
#define _CASTASGN(a,b) (a)=(b)
#endif

/******************************************************************************
 * The sort macro is an adaptation of Simon Tatham's O(n log(n)) mergesort    *
 * Unwieldy variable names used here to avoid shadowing passed-in variables.  *
 *****************************************************************************/
#define LL_SORT(list, cmp) LL_SORT_N(list, cmp, next)
#define LL_SORT_N(list, cmp, _next)                                                        \
do {                                                                                           \
  LDECLTYPE(list) _ls_p;                                                                   \
  LDECLTYPE(list) _ls_q;                                                                   \
  LDECLTYPE(list) _ls_e;                                                                   \
  LDECLTYPE(list) _ls_tail;                                                                \
  LDECLTYPE(list) _ls_oldhead;                                                             \
  LDECLTYPE(list) _tmp;                                                                    \
  int _ls_insize, _ls_nmerges, _ls_psize, _ls_qsize, _ls_i, _ls_looping;                       \
  if (list) {                                                                                  \
    _ls_insize = 1;                                                                            \
    _ls_looping = 1;                                                                           \
    while (_ls_looping) {                                                                      \
      _CASTASGN(_ls_p,list);                                                               \
      _CASTASGN(_ls_oldhead,list);                                                         \
      list = NULL;                                                                             \
      _ls_tail = NULL;                                                                         \
      _ls_nmerges = 0;                                                                         \
      while (_ls_p) {                                                                          \
        _ls_nmerges++;                                                                         \
        _ls_q = _ls_p;                                                                         \
        _ls_psize = 0;                                                                         \
        for (_ls_i = 0; _ls_i < _ls_insize; _ls_i++) {                                         \
          _ls_psize++;                                                                         \
          _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list);             \
          if (!_ls_q) break;                                                                   \
        }                                                                                      \
        _ls_qsize = _ls_insize;                                                                \
        while (_ls_psize > 0 || (_ls_qsize > 0 && _ls_q)) {                                    \
          if (_ls_psize == 0) {                                                                \
            _ls_e = _ls_q; _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list); _ls_qsize--; \
          } else if (_ls_qsize == 0 || !_ls_q) {                                               \
            _ls_e = _ls_p; _SV(_ls_p,list); _ls_p = _NEXT(_ls_p,list,_next); _RS(list); _ls_psize--; \
          } else if (cmp(_ls_p,_ls_q) <= 0) {                                                  \
            _ls_e = _ls_p; _SV(_ls_p,list); _ls_p = _NEXT(_ls_p,list,_next); _RS(list); _ls_psize--; \
          } else {                                                                             \
            _ls_e = _ls_q; _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list); _ls_qsize--; \
          }                                                                                    \
          if (_ls_tail) {                                                                      \
            _SV(_ls_tail,list); _NEXTASGN(_ls_tail,list,_ls_e,_next); _RS(list);   \
          } else {                                                                             \
            _CASTASGN(list,_ls_e);                                                         \
          }                                                                                    \
          _ls_tail = _ls_e;                                                                    \
        }                                                                                      \
        _ls_p = _ls_q;                                                                         \
      }                                                                                        \
      _SV(_ls_tail,list); _NEXTASGN(_ls_tail,list,NULL,_next); _RS(list);          \
      if (_ls_nmerges <= 1) {                                                                  \
        _ls_looping=0;                                                                         \
      }                                                                                        \
      _ls_insize *= 2;                                                                         \
    }                                                                                          \
  } else _tmp=NULL; /* quiet gcc unused variable warning */                                    \
} while (0)

#define DL_SORT(list, cmp) DL_SORT_NP(list, cmp, next, prev)
#define DL_SORT_NP(list, cmp, _next, _prev)                                                \
do {                                                                                           \
  LDECLTYPE(list) _ls_p;                                                                   \
  LDECLTYPE(list) _ls_q;                                                                   \
  LDECLTYPE(list) _ls_e;                                                                   \
  LDECLTYPE(list) _ls_tail;                                                                \
  LDECLTYPE(list) _ls_oldhead;                                                             \
  LDECLTYPE(list) _tmp;                                                                    \
  int _ls_insize, _ls_nmerges, _ls_psize, _ls_qsize, _ls_i, _ls_looping;                       \
  if (list) {                                                                                  \
    _ls_insize = 1;                                                                            \
    _ls_looping = 1;                                                                           \
    while (_ls_looping) {                                                                      \
      _CASTASGN(_ls_p,list);                                                               \
      _CASTASGN(_ls_oldhead,list);                                                         \
      list = NULL;                                                                             \
      _ls_tail = NULL;                                                                         \
      _ls_nmerges = 0;                                                                         \
      while (_ls_p) {                                                                          \
        _ls_nmerges++;                                                                         \
        _ls_q = _ls_p;                                                                         \
        _ls_psize = 0;                                                                         \
        for (_ls_i = 0; _ls_i < _ls_insize; _ls_i++) {                                         \
          _ls_psize++;                                                                         \
          _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list);             \
          if (!_ls_q) break;                                                                   \
        }                                                                                      \
        _ls_qsize = _ls_insize;                                                                \
        while (_ls_psize > 0 || (_ls_qsize > 0 && _ls_q)) {                                    \
          if (_ls_psize == 0) {                                                                \
            _ls_e = _ls_q; _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list); _ls_qsize--; \
          } else if (_ls_qsize == 0 || !_ls_q) {                                               \
            _ls_e = _ls_p; _SV(_ls_p,list); _ls_p = _NEXT(_ls_p,list,_next); _RS(list); _ls_psize--; \
          } else if (cmp(_ls_p,_ls_q) <= 0) {                                                  \
            _ls_e = _ls_p; _SV(_ls_p,list); _ls_p = _NEXT(_ls_p,list,_next); _RS(list); _ls_psize--; \
          } else {                                                                             \
            _ls_e = _ls_q; _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list); _ls_qsize--; \
          }                                                                                    \
          if (_ls_tail) {                                                                      \
            _SV(_ls_tail,list); _NEXTASGN(_ls_tail,list,_ls_e,_next); _RS(list);   \
          } else {                                                                             \
            _CASTASGN(list,_ls_e);                                                         \
          }                                                                                    \
          _SV(_ls_e,list); _PREVASGN(_ls_e,list,_ls_tail,_prev); _RS(list);        \
          _ls_tail = _ls_e;                                                                    \
        }                                                                                      \
        _ls_p = _ls_q;                                                                         \
      }                                                                                        \
      _CASTASGN(list->_prev, _ls_tail);                                                    \
      _SV(_ls_tail,list); _NEXTASGN(_ls_tail,list,NULL,_next); _RS(list);          \
      if (_ls_nmerges <= 1) {                                                                  \
        _ls_looping=0;                                                                         \
      }                                                                                        \
      _ls_insize *= 2;                                                                         \
    }                                                                                          \
  } else _tmp=NULL; /* quiet gcc unused variable warning */                                    \
} while (0)

#define CDL_SORT(list, cmp) CDL_SORT_NP(list, cmp, next, prev)
#define CDL_SORT_NP(list, cmp, _next, _prev)                                               \
do {                                                                                           \
  LDECLTYPE(list) _ls_p;                                                                   \
  LDECLTYPE(list) _ls_q;                                                                   \
  LDECLTYPE(list) _ls_e;                                                                   \
  LDECLTYPE(list) _ls_tail;                                                                \
  LDECLTYPE(list) _ls_oldhead;                                                             \
  LDECLTYPE(list) _tmp;                                                                    \
  LDECLTYPE(list) _tmp2;                                                                   \
  int _ls_insize, _ls_nmerges, _ls_psize, _ls_qsize, _ls_i, _ls_looping;                       \
  if (list) {                                                                                  \
    _ls_insize = 1;                                                                            \
    _ls_looping = 1;                                                                           \
    while (_ls_looping) {                                                                      \
      _CASTASGN(_ls_p,list);                                                               \
      _CASTASGN(_ls_oldhead,list);                                                         \
      list = NULL;                                                                             \
      _ls_tail = NULL;                                                                         \
      _ls_nmerges = 0;                                                                         \
      while (_ls_p) {                                                                          \
        _ls_nmerges++;                                                                         \
        _ls_q = _ls_p;                                                                         \
        _ls_psize = 0;                                                                         \
        for (_ls_i = 0; _ls_i < _ls_insize; _ls_i++) {                                         \
          _ls_psize++;                                                                         \
          _SV(_ls_q,list);                                                                 \
          if (_NEXT(_ls_q,list,_next) == _ls_oldhead) {                                    \
            _ls_q = NULL;                                                                      \
          } else {                                                                             \
            _ls_q = _NEXT(_ls_q,list,_next);                                               \
          }                                                                                    \
          _RS(list);                                                                       \
          if (!_ls_q) break;                                                                   \
        }                                                                                      \
        _ls_qsize = _ls_insize;                                                                \
        while (_ls_psize > 0 || (_ls_qsize > 0 && _ls_q)) {                                    \
          if (_ls_psize == 0) {                                                                \
            _ls_e = _ls_q; _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list); _ls_qsize--; \
            if (_ls_q == _ls_oldhead) { _ls_q = NULL; }                                        \
          } else if (_ls_qsize == 0 || !_ls_q) {                                               \
            _ls_e = _ls_p; _SV(_ls_p,list); _ls_p = _NEXT(_ls_p,list,_next); _RS(list); _ls_psize--; \
            if (_ls_p == _ls_oldhead) { _ls_p = NULL; }                                        \
          } else if (cmp(_ls_p,_ls_q) <= 0) {                                                  \
            _ls_e = _ls_p; _SV(_ls_p,list); _ls_p = _NEXT(_ls_p,list,_next); _RS(list); _ls_psize--; \
            if (_ls_p == _ls_oldhead) { _ls_p = NULL; }                                        \
          } else {                                                                             \
            _ls_e = _ls_q; _SV(_ls_q,list); _ls_q = _NEXT(_ls_q,list,_next); _RS(list); _ls_qsize--; \
            if (_ls_q == _ls_oldhead) { _ls_q = NULL; }                                        \
          }                                                                                    \
          if (_ls_tail) {                                                                      \
            _SV(_ls_tail,list); _NEXTASGN(_ls_tail,list,_ls_e,_next); _RS(list);   \
          } else {                                                                             \
            _CASTASGN(list,_ls_e);                                                         \
          }                                                                                    \
          _SV(_ls_e,list); _PREVASGN(_ls_e,list,_ls_tail,_prev); _RS(list);        \
          _ls_tail = _ls_e;                                                                    \
        }                                                                                      \
        _ls_p = _ls_q;                                                                         \
      }                                                                                        \
      _CASTASGN(list->_prev,_ls_tail);                                                     \
      _CASTASGN(_tmp2,list);                                                               \
      _SV(_ls_tail,list); _NEXTASGN(_ls_tail,list,_tmp2,_next); _RS(list);         \
      if (_ls_nmerges <= 1) {                                                                  \
        _ls_looping=0;                                                                         \
      }                                                                                        \
      _ls_insize *= 2;                                                                         \
    }                                                                                          \
  } else _tmp=NULL; /* quiet gcc unused variable warning */                                    \
} while (0)

/******************************************************************************
 * singly linked list macros (non-circular)                                   *
 *****************************************************************************/
#define LL_PREPEND(head,tail,add) LL_PREPEND_N(head,tail,add,next)
#define LL_PREPEND_N(head,tail,add,_next)                                                  \
do {                                                                                           \
  (add)->_next = head;                                                                         \
  head = add;                                                                                  \
  if ((add)->_next == NULL)                                                                    \
    (tail) = (add);                                                                            \
} while (0)

#define LL_CONCAT(head1,head2,tail1,tail2) LL_CONCAT_N(head1,head2,tail1,tail2,next)
#define LL_CONCAT_N(head1,head2,tail1,tail2,_next)                                         \
do {                                                                                           \
  if (tail1) {                                                                                 \
    (tail1)->_next=(head2);                                                                    \
  } else {                                                                                     \
    (head1)=(head2);                                                                           \
  }                                                                                            \
  if (tail2) {                                                                                 \
    (tail1)=(tail2);                                                                           \
  }                                                                                            \
} while (0)

#define LL_APPEND(head,tail,add) LL_APPEND_N(head,tail,add,next)
#define LL_APPEND_N(head,tail,add,_next)                                                   \
do {                                                                                           \
  (add)->_next=NULL;                                                                           \
  if (tail) {                                                                                  \
    (tail)->_next=(add);                                                                       \
  } else {                                                                                     \
    (head)=(add);                                                                              \
  }                                                                                            \
  (tail)=(add);                                                                                \
} while (0)

#define LL_DELETE(head,tail,del) LL_DELETE_N(head,tail,del,next)
#define LL_DELETE_N(head,tail,del,_next)                                                   \
do {                                                                                           \
  LDECLTYPE(head) _tmp;                                                                    \
  if ((head) == (del)) {                                                                       \
    (head)=(head)->_next;                                                                      \
    if ((tail) == (del))                                                                       \
      (tail)=(head);                                                                           \
  } else {                                                                                     \
    _tmp = head;                                                                               \
    while (_tmp->_next && (_tmp->_next != (del))) {                                            \
      _tmp = _tmp->_next;                                                                      \
    }                                                                                          \
    if (_tmp->_next) {                                                                         \
      _tmp->_next = ((del)->_next);                                                            \
      if ((tail) == (del))                                                                     \
        (tail)=_tmp;                                                                           \
    }                                                                                          \
  }                                                                                            \
} while (0)

/* Here are VS2008 replacements for LL_DELETE */
#define LL_DELETE_VS2008(head,tail,del) LL_DELETE_N_VS2008(head,tail,del,next)
#define LL_DELETE_N_VS2008(head,tail,del,_next)                                            \
do {                                                                                           \
  if ((head) == (del)) {                                                                       \
    (head)=(head)->_next;                                                                      \
    if ((tail) == (del))                                                                       \
      (tail)=(head);                                                                           \
  } else {                                                                                     \
    char *_tmp = (char*)(head);                                                                \
    while ((head)->_next && ((head)->_next != (del))) {                                        \
      (head) = (head)->_next;                                                                  \
    }                                                                                          \
    if ((head)->_next) {                                                                       \
      (head)->_next = ((del)->_next);                                                          \
      if ((tail) == (del))                                                                     \
        (tail)=(head);                                                                         \
    }                                                                                          \
    {                                                                                          \
      char **_head_alias = (char**)&(head);                                                    \
      *_head_alias = _tmp;                                                                     \
    }                                                                                          \
  }                                                                                            \
} while (0)
#ifdef NO_DECLTYPE
#undef LL_DELETE
#define LL_DELETE LL_DELETE_VS2008
#undef DL_CONCAT        /* no DL_CONCAT_VS2008 */
#endif
/* end VS2008 replacements */

#define LL_FOREACH(head,el) LL_FOREACH_N(head,el,next)
#define LL_FOREACH_N(head,el,_next)                                                        \
    for(el=head;el;el=el->_next)

#define LL_FOREACH_SAFE(head,el,tmp) LL_FOREACH_SAFE_N(head,el,tmp,next)
#define LL_FOREACH_SAFE_N(head,el,tmp,_next)                                               \
    for((el)=(head);(el) && (tmp = (el)->_next, 1); (el) = tmp)

#define LL_SEARCH_SCALAR(head,out,field,val) LL_SEARCH_SCALAR_N(head,out,field,val,next)
#define LL_SEARCH_SCALAR_N(head,out,field,val,_next)                                       \
do {                                                                                           \
    LL_FOREACH_N(head,out,_next) {                                                         \
      if ((out)->field == (val)) break;                                                        \
    }                                                                                          \
} while (0)

#define LL_SEARCH(head,out,elt,cmp) LL_SEARCH_N(head,out,elt,cmp,next)
#define LL_SEARCH_N(head,out,elt,cmp,_next)                                                \
do {                                                                                           \
    LL_FOREACH_N(head,out,_next) {                                                         \
      if ((cmp(out,elt))==0) break;                                                            \
    }                                                                                          \
} while (0)

/******************************************************************************
 * doubly linked list macros (non-circular)                                   *
 *****************************************************************************/
#define DL_PREPEND(head,add) DL_PREPEND_NP(head,add,next,prev)
#define DL_PREPEND_NP(head,add,_next,_prev)                                                \
do {                                                                                           \
 (add)->_next = head;                                                                          \
 if (head) {                                                                                   \
   (add)->_prev = (head)->_prev;                                                               \
   (head)->_prev = (add);                                                                      \
 } else {                                                                                      \
   (add)->_prev = (add);                                                                       \
 }                                                                                             \
 (head) = (add);                                                                               \
} while (0)

#define DL_APPEND(head,add) DL_APPEND_NP(head,add,next,prev)
#define DL_APPEND_NP(head,add,_next,_prev)                                                 \
do {                                                                                           \
  if (head) {                                                                                  \
      (add)->_prev = (head)->_prev;                                                            \
      (head)->_prev->_next = (add);                                                            \
      (head)->_prev = (add);                                                                   \
      (add)->_next = NULL;                                                                     \
  } else {                                                                                     \
      (head)=(add);                                                                            \
      (head)->_prev = (head);                                                                  \
      (head)->_next = NULL;                                                                    \
  }                                                                                            \
} while (0)

#define DL_CONCAT(head1,head2) DL_CONCAT_NP(head1,head2,next,prev)
#define DL_CONCAT_NP(head1,head2,_next,_prev)                                              \
do {                                                                                           \
  LDECLTYPE(head1) _tmp;                                                                   \
  if (head2) {                                                                                 \
    if (head1) {                                                                               \
        _tmp = (head2)->_prev;                                                                 \
        (head2)->_prev = (head1)->_prev;                                                       \
        (head1)->_prev->_next = (head2);                                                       \
        (head1)->_prev = _tmp;                                                                 \
    } else {                                                                                   \
        (head1)=(head2);                                                                       \
    }                                                                                          \
  }                                                                                            \
} while (0)

#define DL_DELETE(head,del) DL_DELETE_NP(head,del,next,prev)
#define DL_DELETE_NP(head,del,_next,_prev)                                                 \
do {                                                                                           \
  if ((del)->_prev == (del)) {                                                                 \
      (head)=NULL;                                                                             \
  } else if ((del)==(head)) {                                                                  \
      (del)->_next->_prev = (del)->_prev;                                                      \
      (head) = (del)->_next;                                                                   \
  } else {                                                                                     \
      (del)->_prev->_next = (del)->_next;                                                      \
      if ((del)->_next) {                                                                      \
          (del)->_next->_prev = (del)->_prev;                                                  \
      } else {                                                                                 \
          (head)->_prev = (del)->_prev;                                                        \
      }                                                                                        \
  }                                                                                            \
} while (0)


#define DL_FOREACH(head,el) DL_FOREACH_NP(head,el,next,prev)
#define DL_FOREACH_NP(head,el,_next,_prev)                                                 \
    for(el=head;el;el=el->_next)

/* this version is safe for deleting the elements during iteration */
#define DL_FOREACH_SAFE(head,el,tmp) DL_FOREACH_SAFE_NP(head,el,tmp,next,prev)
#define DL_FOREACH_SAFE_NP(head,el,tmp,_next,_prev)                                        \
  for((el)=(head);(el) && (tmp = (el)->_next, 1); (el) = tmp)

/* these are identical to their singly-linked list counterparts */
#define DL_SEARCH_SCALAR LL_SEARCH_SCALAR
#define DL_SEARCH_SCALAR_NP(head,out,field,val,_next,_prev) LL_SEARCH_SCALAR_N(head,out,field,val,_next)
#define DL_SEARCH LL_SEARCH
#define DL_SEARCH_NP(head,out,elt,cmp,_next,_prev) LL_SEARCH_N(head,out,elt,cmp,_next)

/******************************************************************************
 * circular doubly linked list macros                                         *
 *****************************************************************************/
#define CDL_PREPEND(head,add) CDL_PREPEND_NP(head,add,next,prev)
#define CDL_PREPEND_NP(head,add,_next,_prev)                                               \
do {                                                                                           \
 if (head) {                                                                                   \
   (add)->_prev = (head)->_prev;                                                               \
   (add)->_next = (head);                                                                      \
   (head)->_prev = (add);                                                                      \
   (add)->_prev->_next = (add);                                                                \
 } else {                                                                                      \
   (add)->_prev = (add);                                                                       \
   (add)->_next = (add);                                                                       \
 }                                                                                             \
(head)=(add);                                                                                  \
} while (0)

#define CDL_DELETE(head,del) CDL_DELETE_NP(head,del,next,prev)
#define CDL_DELETE_NP(head,del,_next,_prev)                                                \
do {                                                                                           \
  if (((head)==(del)) && ((head)->_next == (head))) {                                         \
      (head) = 0L;                                                                             \
  } else {                                                                                     \
     (del)->_next->_prev = (del)->_prev;                                                       \
     (del)->_prev->_next = (del)->_next;                                                       \
     if ((del) == (head)) (head)=(del)->_next;                                                 \
  }                                                                                            \
} while (0)

#define CDL_FOREACH(head,el) CDL_FOREACH_NP(head,el,next,prev)
#define CDL_FOREACH_NP(head,el,_next,_prev)                                                \
    for(el=head;el;el=(el->_next==head ? 0L : el->_next))

#define CDL_FOREACH_SAFE(head,el,tmp1,tmp2) CDL_FOREACH_SAFE_NP(head,el,tmp1,tmp2,next,prev)
#define CDL_FOREACH_SAFE_NP(head,el,tmp1,tmp2,_next,_prev)                                 \
  for((el)=(head), ((tmp1)=(head)?((head)->_prev):NULL);                                       \
      (el) && ((tmp2)=(el)->_next, 1);                                                         \
      ((el) = (((el)==(tmp1)) ? 0L : (tmp2))))

#define CDL_SEARCH_SCALAR(head,out,field,val) CDL_SEARCH_SCALAR_NP(head,out,field,val,next,prev)
#define CDL_SEARCH_SCALAR_NP(head,out,field,val,_next,_prev)                               \
do {                                                                                           \
    CDL_FOREACH_NP(head,out,_next,_prev) {                                                 \
      if ((out)->field == (val)) break;                                                        \
    }                                                                                          \
} while (0)

#define CDL_SEARCH(head,out,elt,cmp) CDL_SEARCH_NP(head,out,elt,cmp,next,prev)
#define CDL_SEARCH_NP(head,out,elt,cmp,_next,_prev)                                        \
do {                                                                                           \
    CDL_FOREACH_NP(head,out,_next,_prev) {                                                 \
      if ((cmp(out,elt))==0) break;                                                            \
    }                                                                                          \
} while (0)

#endif /* UTLIST_H_INCLUDED */
