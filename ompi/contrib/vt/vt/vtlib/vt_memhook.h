/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
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

#if (defined(VT_MEMHOOK))

#include "vt_inttypes.h"
#include <stdlib.h>
#include <malloc.h>

#define VT_MEMHOOKS_OFF() \
  if ( vt_memhook_is_initialized && vt_memhook_is_enabled ) { \
    __malloc_hook = vt_malloc_hook_org; \
    __realloc_hook = vt_realloc_hook_org; \
    __free_hook = vt_free_hook_org; \
    vt_memhook_is_enabled = 0; }
#define VT_MEMHOOKS_ON() \
  if ( vt_memhook_is_initialized && !vt_memhook_is_enabled ) { \
    __malloc_hook = vt_malloc_hook; \
    __realloc_hook = vt_realloc_hook; \
    __free_hook = vt_free_hook; \
     vt_memhook_is_enabled = 1; }
#define VT_MEMHOOKS_ENABLED() vt_memhook_is_enabled

/* memory hooks initialization */
EXTERN void vt_memhook_init(void);

/* memory hooks finalization */
EXTERN void vt_memhook_finalize(void);

/* Prototypes for our hooks */
EXTERN void* vt_malloc_hook(size_t size, const void* caller);
EXTERN void* vt_realloc_hook(void* ptr, size_t size, const void* caller);
EXTERN void  vt_free_hook(void* ptr, const void* caller);

/* Variables to save original hooks */
EXTERN void* (*vt_malloc_hook_org)(size_t, const void *);
EXTERN void* (*vt_realloc_hook_org)(void* ptr, size_t size, const void* caller);
EXTERN void  (*vt_free_hook_org)(void* ptr, const void* caller);

EXTERN uint8_t vt_memhook_is_initialized;
EXTERN uint8_t vt_memhook_is_enabled;

#else /* VT_MEMHOOK */

#define VT_MEMHOOKS_OFF()
#define VT_MEMHOOKS_ON()
#define VT_MEMHOOKS_ENABLED() 0

#endif /* VT_MEMHOOK */

#endif /* _VT_MEMHOOK_H */

