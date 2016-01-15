/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2010 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef PMIX_CONFIG_BOTTOM_H
#define PMIX_CONFIG_BOTTOM_H

/*
 * If we build a static library, Visual C define the _LIB symbol. In the
 * case of a shared library _USERDLL get defined.
 *
 */
#ifndef PMIX_BUILDING
#define PMIX_BUILDING 1
#endif

/*
 * Flex is trying to include the unistd.h file. As there is no configure
 * option or this, the flex generated files will try to include the file
 * even on platforms without unistd.h. Therefore, if we
 * know this file is not available, we can prevent flex from including it.
 */
#ifndef HAVE_UNISTD_H
#define YY_NO_UNISTD_H
#endif

/***********************************************************************
 *
 * code that should be in ompi_config_bottom.h regardless of build
 * status
 *
 **********************************************************************/

/* Do we have posix or solaris thread lib */
#define PMIX_HAVE_THREADS (PMIX_HAVE_POSIX_THREADS || PMIX_HAVE_SOLARIS_THREADS)

/*
 * BEGIN_C_DECLS should be used at the beginning of your declarations,
 * so that C++ compilers don't mangle their names.  Use END_C_DECLS at
 * the end of C declarations.
 */
#undef BEGIN_C_DECLS
#undef END_C_DECLS
#if defined(c_plusplus) || defined(__cplusplus)
# define BEGIN_C_DECLS extern "C" {
# define END_C_DECLS }
#else
#define BEGIN_C_DECLS          /* empty */
#define END_C_DECLS            /* empty */
#endif

/**
 * The attribute definition should be included before any potential
 * usage.
 */
#if PMIX_HAVE_ATTRIBUTE_ALIGNED
#    define __pmix_attribute_aligned__(a)    __attribute__((__aligned__(a)))
#    define __pmix_attribute_aligned_max__   __attribute__((__aligned__))
#else
#    define __pmix_attribute_aligned__(a)
#    define __pmix_attribute_aligned_max__
#endif

#if PMIX_HAVE_ATTRIBUTE_ALWAYS_INLINE
#    define __pmix_attribute_always_inline__ __attribute__((__always_inline__))
#else
#    define __pmix_attribute_always_inline__
#endif

#if PMIX_HAVE_ATTRIBUTE_COLD
#    define __pmix_attribute_cold__          __attribute__((__cold__))
#else
#    define __pmix_attribute_cold__
#endif

#if PMIX_HAVE_ATTRIBUTE_CONST
#    define __pmix_attribute_const__         __attribute__((__const__))
#else
#    define __pmix_attribute_const__
#endif

#if PMIX_HAVE_ATTRIBUTE_DEPRECATED
#    define __pmix_attribute_deprecated__    __attribute__((__deprecated__))
#else
#    define __pmix_attribute_deprecated__
#endif

#if PMIX_HAVE_ATTRIBUTE_FORMAT
#    define __pmix_attribute_format__(a,b,c) __attribute__((__format__(a, b, c)))
#else
#    define __pmix_attribute_format__(a,b,c)
#endif

/* Use this __atribute__ on function-ptr declarations, only */
#if PMIX_HAVE_ATTRIBUTE_FORMAT_FUNCPTR
#    define __pmix_attribute_format_funcptr__(a,b,c) __attribute__((__format__(a, b, c)))
#else
#    define __pmix_attribute_format_funcptr__(a,b,c)
#endif

#if PMIX_HAVE_ATTRIBUTE_HOT
#    define __pmix_attribute_hot__           __attribute__((__hot__))
#else
#    define __pmix_attribute_hot__
#endif

#if PMIX_HAVE_ATTRIBUTE_MALLOC
#    define __pmix_attribute_malloc__        __attribute__((__malloc__))
#else
#    define __pmix_attribute_malloc__
#endif

#if PMIX_HAVE_ATTRIBUTE_MAY_ALIAS
#    define __pmix_attribute_may_alias__     __attribute__((__may_alias__))
#else
#    define __pmix_attribute_may_alias__
#endif

#if PMIX_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION
#    define __pmix_attribute_no_instrument_function__  __attribute__((__no_instrument_function__))
#else
#    define __pmix_attribute_no_instrument_function__
#endif

#if PMIX_HAVE_ATTRIBUTE_NONNULL
#    define __pmix_attribute_nonnull__(a)    __attribute__((__nonnull__(a)))
#    define __pmix_attribute_nonnull_all__   __attribute__((__nonnull__))
#else
#    define __pmix_attribute_nonnull__(a)
#    define __pmix_attribute_nonnull_all__
#endif

#if PMIX_HAVE_ATTRIBUTE_NORETURN
#    define __pmix_attribute_noreturn__      __attribute__((__noreturn__))
#else
#    define __pmix_attribute_noreturn__
#endif

/* Use this __atribute__ on function-ptr declarations, only */
#if PMIX_HAVE_ATTRIBUTE_NORETURN_FUNCPTR
#    define __pmix_attribute_noreturn_funcptr__  __attribute__((__noreturn__))
#else
#    define __pmix_attribute_noreturn_funcptr__
#endif

#if PMIX_HAVE_ATTRIBUTE_PACKED
#    define __pmix_attribute_packed__        __attribute__((__packed__))
#else
#    define __pmix_attribute_packed__
#endif

#if PMIX_HAVE_ATTRIBUTE_PURE
#    define __pmix_attribute_pure__          __attribute__((__pure__))
#else
#    define __pmix_attribute_pure__
#endif

#if PMIX_HAVE_ATTRIBUTE_SENTINEL
#    define __pmix_attribute_sentinel__      __attribute__((__sentinel__))
#else
#    define __pmix_attribute_sentinel__
#endif

#if PMIX_HAVE_ATTRIBUTE_UNUSED
#    define __pmix_attribute_unused__        __attribute__((__unused__))
#else
#    define __pmix_attribute_unused__
#endif

#if PMIX_HAVE_ATTRIBUTE_VISIBILITY
#    define __pmix_attribute_visibility__(a) __attribute__((__visibility__(a)))
#else
#    define __pmix_attribute_visibility__(a)
#endif

#if PMIX_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT
#    define __pmix_attribute_warn_unused_result__ __attribute__((__warn_unused_result__))
#else
#    define __pmix_attribute_warn_unused_result__
#endif

#if PMIX_HAVE_ATTRIBUTE_DESTRUCTOR
#    define __pmix_attribute_destructor__    __attribute__((__destructor__))
#else
#    define __pmix_attribute_destructor__
#endif

#ifndef PMIX_DECLSPEC
#  if PMIX_C_HAVE_VISIBILITY
#    define PMIX_DECLSPEC           __pmix_attribute_visibility__("default")
#    define PMIX_MODULE_DECLSPEC    __pmix_attribute_visibility__("default")
#  else
#    define PMIX_DECLSPEC
#    define PMIX_MODULE_DECLSPEC
#  endif
#endif

/*
 * Do we have <stdint.h>?
 */
#ifdef HAVE_STDINT_H
#if !defined(__STDC_LIMIT_MACROS) && (defined(c_plusplus) || defined (__cplusplus))
/* When using a C++ compiler, the max / min value #defines for std
   types are only included if __STDC_LIMIT_MACROS is set before
   including stdint.h */
#define __STDC_LIMIT_MACROS
#endif
#include <pmix/autogen/config.h>
#include <stdint.h>
#else
#include <pmix/autogen/pmix_stdint.h>
#endif

/***********************************************************************
 *
 * Code that is only for when building PMIx or utilities that are
 * using the internals of PMIx.  It should not be included when
 * building MPI applications
 *
 **********************************************************************/
#if PMIX_BUILDING

#ifndef HAVE_PTRDIFF_T
typedef PMIX_PTRDIFF_TYPE ptrdiff_t;
#endif

/*
 * Maximum size of a filename path.
 */
#include <limits.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if defined(PATH_MAX)
#define PMIX_PATH_MAX	(PATH_MAX + 1)
#elif defined(_POSIX_PATH_MAX)
#define PMIX_PATH_MAX	(_POSIX_PATH_MAX + 1)
#else
#define PMIX_PATH_MAX	256
#endif

/*
 * Set the compile-time path-separator on this system and variable separator
 */
#define PMIX_PATH_SEP "/"
#define PMIX_ENV_SEP  ':'

/*
 * printf functions for portability (only when building PMIx)
 */
#if !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include <stdarg.h>
#include <stdlib.h>
#endif

#if !defined(HAVE_ASPRINTF) || !defined(HAVE_SNPRINTF) || !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include "util/printf.h"
#endif

#ifndef HAVE_ASPRINTF
# define asprintf pmix_asprintf
#endif

#ifndef HAVE_SNPRINTF
# define snprintf pmix_snprintf
#endif

#ifndef HAVE_VASPRINTF
# define vasprintf pmix_vasprintf
#endif

#ifndef HAVE_VSNPRINTF
# define vsnprintf pmix_vsnprintf
#endif

/*
 * Some platforms (Solaris) have a broken qsort implementation.  Work
 * around by using our own.
 */
#if PMIX_HAVE_BROKEN_QSORT
#ifdef qsort
#undef qsort
#endif

#include "util/qsort.h"
#define qsort pmix_qsort
#endif

/*
 * Define __func__-preprocessor directive if the compiler does not
 * already define it.  Define it to __FILE__ so that we at least have
 * a clue where the developer is trying to indicate where the error is
 * coming from (assuming that __func__ is typically used for
 * printf-style debugging).
 */
#if defined(HAVE_DECL___FUNC__) && !HAVE_DECL___FUNC__
#define __func__ __FILE__
#endif

#define IOVBASE_TYPE  void

#include <stdbool.h>
/**
 * If we generate our own bool type, we need a special way to cast the result
 * in such a way to keep the compilers silent.
 */
#  define PMIX_INT_TO_BOOL(VALUE)  (bool)(VALUE)

#if !defined(HAVE_STRUCT_SOCKADDR_STORAGE) && defined(HAVE_STRUCT_SOCKADDR_IN)
#define sockaddr_storage sockaddr
#define ss_family sa_family
#endif

/* Compatibility structure so that we don't have to have as many
   #if checks in the code base */
#if !defined(HAVE_STRUCT_SOCKADDR_IN6) && defined(HAVE_STRUCT_SOCKADDR_IN)
#define sockaddr_in6 sockaddr_in
#define sin6_len sin_len
#define sin6_family sin_family
#define sin6_port sin_port
#define sin6_addr sin_addr
#endif

#if !HAVE_DECL_AF_UNSPEC
#define AF_UNSPEC 0
#endif
#if !HAVE_DECL_PF_UNSPEC
#define PF_UNSPEC 0
#endif
#if !HAVE_DECL_AF_INET6
#define AF_INET6 AF_UNSPEC
#endif
#if !HAVE_DECL_PF_INET6
#define PF_INET6 PF_UNSPEC
#endif

#if defined(__APPLE__) && defined(HAVE_INTTYPES_H)
/* Prior to Mac OS X 10.3, the length modifier "ll" wasn't
   supported, but "q" was for long long.  This isn't ANSI
   C and causes a warning when using PRI?64 macros.  We
   don't support versions prior to OS X 10.3, so we dont'
   need such backward compatibility.  Instead, redefine
   the macros to be "ll", which is ANSI C and doesn't
   cause a compiler warning. */
#include <inttypes.h>
#if defined(__PRI_64_LENGTH_MODIFIER__)
#undef __PRI_64_LENGTH_MODIFIER__
#define __PRI_64_LENGTH_MODIFIER__ "ll"
#endif
#if defined(__SCN_64_LENGTH_MODIFIER__)
#undef __SCN_64_LENGTH_MODIFIER__
#define __SCN_64_LENGTH_MODIFIER__ "ll"
#endif
#endif

#ifdef MCS_VXWORKS
/* VXWorks puts some common functions in oddly named headers.  Rather
   than update all the places the functions are used, which would be a
   maintenance disatster, just update here... */
#ifdef HAVE_IOLIB_H
/* pipe(), ioctl() */
#include <ioLib.h>
#endif
#ifdef HAVE_SOCKLIB_H
/* socket() */
#include <sockLib.h>
#endif
#ifdef HAVE_HOSTLIB_H
/* gethostname() */
#include <hostLib.h>

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif
#endif

#endif

/* If we're in C++, then just undefine restrict and then define it to
   nothing.  "restrict" is not part of the C++ language, and we don't
   have a corresponding AC_CXX_RESTRICT to figure out what the C++
   compiler supports. */
#if defined(c_plusplus) || defined(__cplusplus)
#undef restrict
#define restrict
#endif

#else

/* For a similar reason to what is listed in pmix_config_top.h, we
   want to protect others from the autoconf/automake-generated
   PACKAGE_<foo> macros in pmix_config.h.  We can't put these undef's
   directly in pmix_config.h because they'll be turned into #defines'
   via autoconf.

   So put them here in case any one else includes PMIX's
   config.h files. */

#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#undef HAVE_CONFIG_H

#endif /* PMIX_BUILDING */
#endif /* PMIX_CONFIG_BOTTOM_H */

