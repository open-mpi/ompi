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

#ifndef _VT_MEMHOOK_H
#define _VT_MEMHOOK_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_inttypes.h"
#include <stdlib.h>

#if (defined(VT_MEMHOOK))
# include <malloc.h>
# define VT_MEMHOOKS_OFF() \
    if ( memhook_is_initialized && memhook_is_enabled ) { \
      __malloc_hook = org_malloc_hook; \
      __realloc_hook = org_realloc_hook; \
      __free_hook = org_free_hook; \
      memhook_is_enabled = 0; }
# define VT_MEMHOOKS_ON() \
    if ( memhook_is_initialized && !memhook_is_enabled ) { \
      __malloc_hook = vt_malloc_hook; \
      __realloc_hook = vt_realloc_hook; \
      __free_hook = vt_free_hook; \
      memhook_is_enabled = 1; }
# define VT_MEMHOOKS_ENABLED() memhook_is_enabled

/* memory hooks initialization */
EXTERN void vt_memhook_init(void);

/* memory hooks finalization */
EXTERN void vt_memhook_finalize(void);

/* Prototypes for our hooks */
EXTERN void* vt_malloc_hook(size_t size, const void* caller);
EXTERN void* vt_realloc_hook(void* ptr, size_t size, const void* caller);
EXTERN void  vt_free_hook(void* ptr, const void* caller);

/* Variables to save original hooks */
EXTERN void* (*org_malloc_hook)(size_t, const void *);
EXTERN void* (*org_realloc_hook)(void* ptr, size_t size, const void* caller);
EXTERN void  (*org_free_hook)(void* ptr, const void* caller);

EXTERN uint8_t memhook_is_initialized;
EXTERN uint8_t memhook_is_enabled;
#else
# define VT_MEMHOOKS_OFF()
# define VT_MEMHOOKS_ON()
# define VT_MEMHOOKS_ENABLED() 0
#endif

#endif /* _VT_MEMHOOK_H */

