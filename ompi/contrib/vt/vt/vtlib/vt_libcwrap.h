/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_LIBCWRAP_H
#define _VT_LIBCWRAP_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#if (defined(VT_LIBCWRAP))

#define VT_ENABLE_LIBC_TRACING() vt_libc_tracing_enabled = 1;
#define VT_DISABLE_LIBC_TRACING() vt_libc_tracing_enabled = 0;
#define VT_SUSPEND_LIBC_TRACING() \
  vt_libc_tracing_state = vt_libc_tracing_enabled; \
  vt_libc_tracing_enabled = 0;
#define VT_RESUME_LIBC_TRACING() \
  vt_libc_tracing_enabled = vt_libc_tracing_state;

/* libc wrapper initialization */
EXTERN void vt_libcwrap_init(void);

/* libc wrapper finalization */
EXTERN void vt_libcwrap_finalize(void);

EXTERN int vt_libc_tracing_enabled;
EXTERN int vt_libc_tracing_state;

#else /* VT_LIBCWRAP */

#define VT_ENABLE_LIBC_TRACING()
#define VT_DISABLE_LIBC_TRACING()
#define VT_SUSPEND_LIBC_TRACING()
#define VT_RESUME_LIBC_TRACING()

#endif

#endif /* _VT_LIBCWRAP_H */

