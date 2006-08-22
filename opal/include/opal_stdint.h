/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * This file includes the C99 stdint.h file if available, and otherwise
 * defines fixed-width types according to the SIZEOF information
 * gathered by configure.
 */

#ifndef OMPI_STDINT_H
#define OMPI_STDINT_H 1

/*
 * Include what we can and define what is missing.
 */
#include <limits.h>

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/* 8-bit */

#if SIZEOF_CHAR == 1

#ifndef HAVE_INT8_T
typedef signed char int8_t;
#endif

#ifndef HAVE_UINT8_T
typedef unsigned char uint8_t;
#endif

#else

#error Failed to define 8-bit types

#endif

/* 16-bit */

#if SIZEOF_SHORT == 2

#ifndef HAVE_INT16_T
typedef signed short int16_t;
#endif

#ifndef HAVE_UINT16_T
typedef unsigned short uint16_t;
#endif

#else

#error Failed to define 16-bit types

#endif

/* 32-bit */

#if SIZEOF_INT == 4

#ifndef HAVE_INT32_T
typedef signed int int32_t;
#endif

#ifndef HAVE_UINT32_T
typedef unsigned int uint32_t;
#endif

#elif SIZEOF_LONG == 4

#ifndef HAVE_INT32_T
typedef signed long int32_t;
#endif

#ifndef HAVE_UINT32_T
typedef unsigned long uint32_t;
#endif

#else

#error Failed to define 32-bit types

#endif

/* 64-bit */

#if SIZEOF_INT == 8

#ifndef HAVE_INT64_T
typedef signed int int64_t;
#endif

#ifndef HAVE_UINT64_T
typedef unsigned int uint64_t;
#endif

#elif SIZEOF_LONG == 8

#ifndef HAVE_INT64_T
typedef signed long int64_t;
#endif

#ifndef HAVE_UINT64_T
typedef unsigned long uint64_t;
#endif

#elif HAVE_LONG_LONG && SIZEOF_LONG_LONG == 8

#ifndef HAVE_INT64_T
typedef signed long long int64_t;
#endif

#ifndef HAVE_UINT64_T
typedef unsigned long long uint64_t;
#endif

#else

#error Failed to define 64-bit types

#endif

/* Pointers */

#if SIZEOF_VOID_P == SIZEOF_INT

#ifndef HAVE_INTPTR_T
typedef signed int intptr_t;
#endif

#ifndef HAVE_UINTPTR_T
typedef unsigned int uintptr_t;
#endif

#elif SIZEOF_VOID_P == SIZEOF_LONG

#ifndef HAVE_INTPTR_T
typedef signed long intptr_t;
#endif

#ifndef HAVE_UINTPTR_T
typedef unsigned long uintptr_t;
#endif

#elif HAVE_LONG_LONG && SIZEOF_VOID_P == SIZEOF_LONG_LONG

#ifndef HAVE_INTPTR_T
typedef signed long long intptr_t;
#endif
#ifndef HAVE_UINTPTR_T
typedef unsigned long long uintptr_t;
#endif

#else

#error Failed to define pointer-sized integer types

#endif

/* fix up some constants that may be missing */
#ifndef SIZE_MAX
#if SIZEOF_VOID_P == SIZEOF_INT
#define SIZE_MAX UINT_MAX
#elif SIZEOF_VOID_P == SIZEOF_LONG
#define SIZE_MAX ULONG_MAX
#else
#error Failed to find value for SIZE_MAX
#endif

#endif /* ifndef SIZE_MAX */

#endif /* OMPI_STDINT_H */

