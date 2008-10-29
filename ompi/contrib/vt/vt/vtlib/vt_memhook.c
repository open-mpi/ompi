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

#include "config.h"

#include "vt_pform.h"
#include "vt_trc.h"
#include "vt_memhook.h"
#include "vt_memreg.h"
#include "vt_inttypes.h"
#include <malloc.h>
#include <stdlib.h>

/* Variables to save original hooks */
void *(*org_malloc_hook)(size_t size, const void* caller);
void *(*org_realloc_hook)(void* ptr, size_t size, const void* caller);
void  (*org_free_hook)(void* ptr, const void* caller);

uint8_t memhook_is_initialized = 0;
uint8_t memhook_is_enabled = 0;

void vt_memhook_init()
{
  if( memhook_is_initialized ) return;

  org_malloc_hook = __malloc_hook;
  org_realloc_hook = __realloc_hook;
  org_free_hook = __free_hook;

  memhook_is_initialized = 1;
}

void vt_memhook_finalize()
{
  if( !memhook_is_initialized ) return;

  __malloc_hook = org_malloc_hook;
  __realloc_hook = org_realloc_hook;
  __free_hook = org_free_hook;
}

void* vt_malloc_hook(size_t size, const void* caller)
{
  void* result;
  uint64_t bytes;
  uint64_t time;

  VT_MEMHOOKS_OFF();   /* Restore original hooks */

  time = vt_pform_wtime();
  vt_enter(&time, vt_mem_regid[VT__MEM_MALLOC]);
    
  result = malloc(size);   /* Call recursively */

  /* Get total allocated memory */
  if ( result != NULL )
  {
    bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)result - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes = 0;
  }

  time = vt_pform_wtime();
  
  vt_mem_alloc(&time, bytes);
  vt_exit(&time);

  VT_MEMHOOKS_ON();   /* Restore our own hooks */

  return result;
}

void* vt_realloc_hook(void* ptr, size_t size, const void* caller)
{
  void* result;
  uint64_t bytes1;
  uint64_t bytes2;
  uint64_t time;

  VT_MEMHOOKS_OFF();   /* Restore original hooks */

  time = vt_pform_wtime();
  vt_enter(&time, vt_mem_regid[VT__MEM_REALLOC]);

  /* Get total allocated memory before realloc */
  if ( NULL != ptr )
  {
    bytes1 = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ptr - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes1 = 0;
  }

  result = realloc(ptr, size);   /* Call recursively */

  /* Get total allocated memory after realloc */
  if ( NULL != result )
  {
    bytes2 = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)result - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes2 = 0;
  }

  time = vt_pform_wtime();
  if ( bytes2 < bytes1 )
    vt_mem_free(&time, bytes1 - bytes2);
  else
    vt_mem_alloc(&time, bytes2 - bytes1);
  vt_exit(&time);

  VT_MEMHOOKS_ON();   /* Restore our own hooks */

  return result;
}

void vt_free_hook(void* ptr, const void* caller)
{
  uint64_t bytes;
  uint64_t time;

  VT_MEMHOOKS_OFF();   /* Restore original hooks */

  time = vt_pform_wtime();
  vt_enter(&time, vt_mem_regid[VT__MEM_FREE]);

  if ( NULL != ptr )
  {
    bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ptr - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes = 0;
  }

  free(ptr);   /* Call recursively */

  time = vt_pform_wtime();
  vt_mem_free(&time, bytes);
  vt_exit(&time);

  VT_MEMHOOKS_ON();   /* Restore our own hooks */
}
