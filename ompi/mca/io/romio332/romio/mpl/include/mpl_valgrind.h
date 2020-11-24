/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2010 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* IMPORTANT!!!: you must define MPL_VG_ENABLED before including mpl.h if you
   want the the actual valgrind macros to be expanded when an MPL_VG_ macro is
   used */

/* this file should not be included directly, it expects to be included from
 * mpl.h with the results of various configure tests and several system
 * libraries already included */
#if !defined(MPL_H_INCLUDED)
#error do not include mpl_valgrind.h directly, use mpl.h instead
#endif

/* valgrind interaction macros and header include logic

   These macros are intended to simplify client request interactions with
   valgrind.  A client source file that needs valgrind client requests if they
   are available can include this file instead and use MPL_VG_ macros without
   worrying about whether or not valgrind is actually present, which headers to
   include, and how to include them.

   For more complicated logic this header will also define/undefine the
   preprocessor token "MPL_VG_AVAILABLE".

 */
#if !defined(MPL_VALGRIND_H_INCLUDED)
#define MPL_VALGRIND_H_INCLUDED

#undef MPL_VG_AVAILABLE

#if !defined(HAVE_BROKEN_VALGRIND) && defined(MPL_VG_ENABLED)
#if !defined(NVALGRIND)
#if defined(MPL_HAVE_VALGRIND_H) && defined(MPL_HAVE_MEMCHECK_H)
#include <valgrind.h>
#include <memcheck.h>
#define MPL_VG_AVAILABLE 1
#elif defined(MPL_HAVE_VALGRIND_VALGRIND_H) && defined(MPL_HAVE_VALGRIND_MEMCHECK_H)
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#define MPL_VG_AVAILABLE 1
#endif
#endif
#endif


/* Valgrind-based thread checking tools:
 *
 * There are three main tools right now, Helgrind and DRD come in the standard
 * Valgrind distribution, while ThreadSanitizer (tsan) requires a separate
 * download.  All three tools are fairly similar, with a few variations.  They
 * primarily check for data races by tracing the happens-before relation between
 * memory accesses and synchronization primitives.  Helgrind also offers good
 * pthread API usage checking and lock-order checking, which usually makes it
 * the most useful tool of the group.  Unfortunately, Helgrind has the most
 * limited support for client requests.
 *
 * All three tools are source-level compatible with the ANNOTATE_* set of macros
 * defined by tsan.  However, they are not totally binary compatible, so a
 * particular tool must usually be selected at compile time.  One exception to
 * this is that modern-ish DRDs will usually understand Helgrind requests (but
 * Helgrind won't understand DRD requests).
 *
 * To further complicate matters, Helgrind will issue warnings when a client
 * request is encountered that it does not implement.  In particular, Helgrind
 * does not support ANNOTATE_BENIGN_RACE or
 * ANNOTATE_IGNORE_{READS,WRITES}_{BEGIN,END}, which makes it difficult to avoid
 * emitting warning/error messages w.r.t. some kinds of lockfree
 * synchronization.
 *
 * So for the moment, we only provide a minimal set of annotations that seems to
 * be both common between the tools and useful in MPICH.
 */

#define MPL_VG_THREAD_INVALID  0
#define MPL_VG_THREAD_HELGRIND 1
#define MPL_VG_THREAD_DRD      2
#define MPL_VG_THREAD_TSAN     3

/* TODO make this selectable by configure */
/* default to helgrind for now, since DRD understands helgrind annotations, but
 * not the other way around */
#define MPL_VG_THREAD_TOOL MPL_VG_THREAD_HELGRIND


#if defined(MPL_VG_AVAILABLE)
#if (MPL_VG_THREAD_TOOL == MPL_VG_THREAD_HELGRIND)
#if defined(MPL_HAVE_HELGRIND_H)
#include <helgrind.h>
#define MPL_VG_THREAD_TOOL_SUPPORTED_ 1
#elif defined(MPL_HAVE_VALGRIND_HELGRIND_H)
#include <valgrind/helgrind.h>
#define MPL_VG_THREAD_TOOL_SUPPORTED_ 1
#endif
#elif (MPL_VG_THREAD_TOOL == MPL_VG_THREAD_DRD)
#if defined(MPL_HAVE_DRD_H)
#include <drd.h>
#define MPL_VG_THREAD_TOOL_SUPPORTED_ 1
#elif defined(MPL_HAVE_VALGRIND_DRD_H)
#include <valgrind/drd.h>
#define MPL_VG_THREAD_TOOL_SUPPORTED_ 1
#endif
#elif (MPL_VG_THREAD_TOOL == MPL_VG_THREAD_TSAN)
     /* support wouldn't be hard to add, but it does require additional build
      * system work */
#error 'ThreadSanitizer not currently supported'
#else
#error 'unknown or unsupported tool'
#endif
#else
#undef MPL_VG_THREAD_TOOL_SUPPORTED_    /*defensive */
#endif

/* This is only a modest subset of all of the available client requests defined
   in the valgrind headers.  As MPICH is modified to use more of them, this
   list should be expanded appropriately. */
#if defined(MPL_VG_AVAILABLE)
#if defined(VALGRIND_MAKE_MEM_DEFINED)
/* this valgrind is version 3.2.0 or later */
/* tt#1784: do not add extra parens around the VALGRIND_... macros, since
 * valgrind-3.6.0 incorrectly includes a ";" at the end of the macro */
#define MPL_VG_MAKE_MEM_DEFINED(addr_,len_)         do { (void) VALGRIND_MAKE_MEM_DEFINED((addr_),(len_)); } while (0)
#define MPL_VG_MAKE_MEM_NOACCESS(addr_,len_)        do { (void) VALGRIND_MAKE_MEM_NOACCESS((addr_),(len_)); } while (0)
#define MPL_VG_MAKE_MEM_UNDEFINED(addr_,len_)       do { (void) VALGRIND_MAKE_MEM_UNDEFINED((addr_),(len_)); } while (0)
#define MPL_VG_CHECK_MEM_IS_DEFINED(addr_,len_)     do { (void) VALGRIND_CHECK_MEM_IS_DEFINED((addr_),(len_)); } while (0)
#define MPL_VG_CHECK_MEM_IS_ADDRESSABLE(addr_,len_) do { (void) VALGRIND_CHECK_MEM_IS_ADDRESSABLE((addr_),(len_)); } while (0)
#else
/* this is an older version of valgrind.  Use the older (subtly misleading) names */
#define MPL_VG_MAKE_MEM_DEFINED(addr_,len_)         VALGRIND_MAKE_READABLE((addr_),(len_))
#define MPL_VG_MAKE_MEM_NOACCESS(addr_,len_)        VALGRIND_MAKE_NOACCESS((addr_),(len_))
#define MPL_VG_MAKE_MEM_UNDEFINED(addr_,len_)       VALGRIND_MAKE_WRITABLE((addr_),(len_))
#define MPL_VG_CHECK_MEM_IS_DEFINED(addr_,len_)     VALGRIND_CHECK_READABLE((addr_),(len_))
#define MPL_VG_CHECK_MEM_IS_ADDRESSABLE(addr_,len_) VALGRIND_CHECK_WRITABLE((addr_),(len_))
#endif
#define MPL_VG_CREATE_BLOCK(addr_,len_,desc_)       do { (void) VALGRIND_CREATE_BLOCK((addr_),(len_),(desc_)); } while (0)
#define MPL_VG_RUNNING_ON_VALGRIND()                RUNNING_ON_VALGRIND
#define MPL_VG_PRINTF_BACKTRACE                     VALGRIND_PRINTF_BACKTRACE
/* Valgrind has a bug
 * (https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=524488) that
 * causes it to report a warning when the compiler adds padding to
 * structures.  Even when we initialize all the fields in the
 * structure, the padding bytes are not initialized.  The idea here is
 * to detect when we are in "valgrind mode" and in such cases
 * initialize all bytes of the structure. */
#define MPL_VG_MEM_INIT(addr_,len_)                 do { memset(addr_, 0, len_); } while (0)

/* custom allocator client requests, you probably shouldn't use these unless you
 * really know what you are doing */
#define MPL_VG_CREATE_MEMPOOL(pool, rzB, is_zeroed) VALGRIND_CREATE_MEMPOOL((pool), (rzB), (is_zeroed))
#define MPL_VG_DESTROY_MEMPOOL(pool)                VALGRIND_DESTROY_MEMPOOL((pool))
#define MPL_VG_MEMPOOL_ALLOC(pool, addr, size)      VALGRIND_MEMPOOL_ALLOC((pool), (addr), (size))
#define MPL_VG_MEMPOOL_FREE(pool, addr)             VALGRIND_MEMPOOL_FREE((pool), (addr))

#else /* !defined(MPL_VG_AVAILABLE) */
#define MPL_VG_MAKE_MEM_DEFINED(addr_,len_)         do {} while (0)
#define MPL_VG_MAKE_MEM_NOACCESS(addr_,len_)        do {} while (0)
#define MPL_VG_MAKE_MEM_UNDEFINED(addr_,len_)       do {} while (0)
#define MPL_VG_CHECK_MEM_IS_DEFINED(addr_,len_)     do {} while (0)
#define MPL_VG_CHECK_MEM_IS_ADDRESSABLE(addr_,len_) do {} while (0)
#define MPL_VG_CREATE_BLOCK(addr_,len_,desc_)       do {} while (0)
#define MPL_VG_RUNNING_ON_VALGRIND()                (0) /*always false */
#define MPL_VG_MEM_INIT(addr_,len_)                 do {} while (0)
#if defined(MPL_HAVE_MACRO_VA_ARGS)
#define MPL_VG_PRINTF_BACKTRACE(...)              do {} while (0)
#else
#define MPL_VG_PRINTF_BACKTRACE MPL_VG_printf_do_nothing_func
static mpl_inline void MPL_VG_printf_do_nothing_func(char *fmt, ...)
{
    /* do nothing */
}
#endif /* defined(MPL_HAVE_MACRO_VA_ARGS) */
#define MPL_VG_CREATE_MEMPOOL(pool, rzB, is_zeroed) do {} while (0)
#define MPL_VG_DESTROY_MEMPOOL(pool)                do {} while (0)
#define MPL_VG_MEMPOOL_ALLOC(pool, addr, size)      do {} while (0)
#define MPL_VG_MEMPOOL_FREE(pool, addr)             do {} while (0)

#endif /* defined(MPL_VG_AVAILABLE) */

#if defined(MPL_VG_THREAD_TOOL_SUPPORTED_)
   /* could switch on tool type, but all three tools know about these annotations */
#define MPL_VG_ANNOTATE_HAPPENS_BEFORE(obj_)        ANNOTATE_HAPPENS_BEFORE(obj_)
#define MPL_VG_ANNOTATE_HAPPENS_AFTER(obj_)         ANNOTATE_HAPPENS_AFTER(obj_)
   /* older versions of some of Helgrind & DRD don't support this annotation */
#if defined(ANNOTATE_NEW_MEMORY)
#define MPL_VG_ANNOTATE_NEW_MEMORY(obj_,size_)    ANNOTATE_NEW_MEMORY(obj_,size_)
#else
#define MPL_VG_ANNOTATE_NEW_MEMORY(obj_,size_)    do {} while (0)
#endif
#else
#define MPL_VG_ANNOTATE_HAPPENS_BEFORE(obj_)        do {} while (0)
#define MPL_VG_ANNOTATE_HAPPENS_AFTER(obj_)         do {} while (0)
#define MPL_VG_ANNOTATE_NEW_MEMORY(obj_,size_)      do {} while (0)
#endif


#endif /* MPL_VALGRIND_H_INCLUDED */
