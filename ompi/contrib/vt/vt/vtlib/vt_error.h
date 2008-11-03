/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_ERROR_H
#define _VT_ERROR_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include <stdarg.h>


/* set process id/rank for messages */
EXTERN void vt_error_pid(const int pid);

/* abort and system error message */
#define vt_error() vt_error_impl(__FILE__, __LINE__)
EXTERN void vt_error_impl(const char* f, int l);                          

/* abort and user error message */
EXTERN void vt_error_msg(const char* fmt, ...);

/* user warning message without abort */
EXTERN void vt_warning(const char* fmt, ...);

/* user control message without abort (printed only if VT_VERBOSE is set) */
EXTERN void vt_cntl_msg(const char* fmt, ...);

/* Debug messages if compiled with -DVT_DEBUG=level, the higher the level,
 * the more output you get ;-)
 */
EXTERN void vt_debug_msg(int level, const char* fmt, ...);

#endif






