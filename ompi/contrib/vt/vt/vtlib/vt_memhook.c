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

#include "config.h"

#include <malloc.h>
#include <stdlib.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"

#define MEMHOOK_REG_MALLOC  0
#define MEMHOOK_REG_REALLOC 1
#define MEMHOOK_REG_FREE    2

#define MEMHOOK_MARK_ALLOC  0
#define MEMHOOK_MARK_FREE   1

/* variables to save original hooks */
void *(*vt_malloc_hook_org)(size_t size, const void* caller);
void *(*vt_realloc_hook_org)(void* ptr, size_t size, const void* caller);
void  (*vt_free_hook_org)(void* ptr, const void* caller);

uint8_t vt_memhook_is_initialized = 0;
uint8_t vt_memhook_is_enabled = 0;

/* write marker for each alloc/free event? */
uint8_t memalloc_marker = 0;

/* array of memory allocation region IDs */
static uint32_t memhook_regid[3];

/* memory allocation marker IDs */
static uint32_t memalloc_mid[2];

/* memory allocation counter ID */
static uint32_t memalloc_cid;

/* memory allocation counter value */
static uint64_t memalloc_val = 0;

void vt_memhook_init()
{
  uint32_t fid;
  uint32_t gid;

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  vt_error_msg("Memory tracing by GNU C malloc-hooks for threaded application "
               "not yet supported");
#endif /* VT_MT || VT_HYB || VT_JAVA */

  if( vt_memhook_is_initialized ) return;

  vt_malloc_hook_org = __malloc_hook;
  vt_realloc_hook_org = __realloc_hook;
  vt_free_hook_org = __free_hook;

  /* define source */
  fid = vt_def_scl_file(VT_CURRENT_THREAD, "MEM");

  /* define regions */
  memhook_regid[MEMHOOK_REG_MALLOC] =
    vt_def_region(VT_CURRENT_THREAD, "malloc", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_MEMORY);
  memhook_regid[MEMHOOK_REG_REALLOC] =
    vt_def_region(VT_CURRENT_THREAD, "realloc", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_MEMORY);
  memhook_regid[MEMHOOK_REG_FREE] =
    vt_def_region(VT_CURRENT_THREAD, "free", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_MEMORY);

  /* define markers, if necessary */
  if( (memalloc_marker = vt_env_memtrace_marker()) )
  {
    memalloc_mid[MEMHOOK_MARK_ALLOC] =
      vt_def_marker(VT_CURRENT_THREAD, "Memory Allocation", VT_MARKER_HINT);
    memalloc_mid[MEMHOOK_MARK_FREE] =
      vt_def_marker(VT_CURRENT_THREAD, "Memory Deallocation", VT_MARKER_HINT);
  }

  /* define counter group */
  gid = vt_def_counter_group(VT_CURRENT_THREAD, "Memory");

  /* define counter */
  memalloc_cid =
    vt_def_counter(VT_CURRENT_THREAD, "MEM_ALLOC", "Bytes",
                   VT_CNTR_ABS | VT_CNTR_NEXT,
                   gid, 0);

  vt_memhook_is_initialized = 1;
}

void vt_memhook_finalize()
{
  if( !vt_memhook_is_initialized ) return;

  __malloc_hook = vt_malloc_hook_org;
  __realloc_hook = vt_realloc_hook_org;
  __free_hook = vt_free_hook_org;

  vt_memhook_is_initialized = 0;
  vt_memhook_is_enabled = 0;
}

void* vt_malloc_hook(size_t size, const void* caller)
{
  void* result;
  uint64_t bytes;
  uint64_t time;
  uint8_t was_recorded;

  VT_MEMHOOKS_OFF(); /* restore original hooks */

  time = vt_pform_wtime();
  was_recorded = vt_enter(VT_CURRENT_THREAD, &time,
                          memhook_regid[MEMHOOK_REG_MALLOC]);

  result = malloc(size); /* call recursively */

  /* get total allocated memory */
  if ( result != NULL )
  {
    bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)result - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes = 0;
  }

  /* update counter value */
  memalloc_val += bytes;

  time = vt_pform_wtime();

  if ( was_recorded && bytes > 0 )
  {
    /* write marker, if desired */
    if( memalloc_marker )
    {
      vt_marker(VT_CURRENT_THREAD, &time, memalloc_mid[MEMHOOK_MARK_ALLOC],
                "Allocated %llu Bytes", (unsigned long long)bytes);
    }

    /* write counter value */
    vt_count(VT_CURRENT_THREAD, &time, memalloc_cid, memalloc_val);
  }

  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON(); /* restore our own hooks */

  return result;
}

void* vt_realloc_hook(void* ptr, size_t size, const void* caller)
{
  void* result;
  uint64_t bytes;
  uint64_t bytes1;
  uint64_t bytes2;
  uint64_t time;
  uint8_t was_recorded;

  VT_MEMHOOKS_OFF(); /* restore original hooks */

  time = vt_pform_wtime();
  was_recorded = vt_enter(VT_CURRENT_THREAD, &time,
                          memhook_regid[MEMHOOK_REG_REALLOC]);

  /* get total allocated memory before realloc */
  if ( NULL != ptr )
  {
    bytes1 = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ptr - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes1 = bytes = 0;
  }

  result = realloc(ptr, size); /* call recursively */

  /* get total allocated memory after realloc */
  if ( NULL != result )
  {
    bytes2 = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)result - SIZEOF_VOIDP ) );
    bytes = bytes2 < bytes1 ? bytes1 - bytes2 : bytes2 - bytes1;
  }
  else
  {
    bytes2 = bytes = 0;
  }

  /* update counter value */
  if ( bytes2 < bytes1 )
  {
    if ( bytes <= memalloc_val )
      memalloc_val -= bytes;
    else
      memalloc_val = 0;
  }
  else
  {
    memalloc_val += bytes;
  }

  time = vt_pform_wtime();

  if( was_recorded && bytes > 0 )
  {
    /* write marker, if desired */
    if( memalloc_marker )
    {
      uint32_t marker_type;
      char* marker_prefix;

      if ( bytes2 < bytes1 )
      {
        marker_type = MEMHOOK_MARK_FREE;
        marker_prefix = "Freed";
      }
      else
      {
        marker_type = MEMHOOK_MARK_ALLOC;
        marker_prefix = "Allocated";
      }

      /* write marker */
      vt_marker(VT_CURRENT_THREAD, &time, memalloc_mid[marker_type],
                "%s %llu Bytes", marker_prefix, (unsigned long long)bytes);
    }

    /* write counter value */
    vt_count(VT_CURRENT_THREAD, &time, memalloc_cid, memalloc_val);
  }

  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON(); /* restore our own hooks */

  return result;
}

void vt_free_hook(void* ptr, const void* caller)
{
  uint64_t bytes;
  uint64_t time;
  uint8_t was_recorded;

  VT_MEMHOOKS_OFF(); /* restore original hooks */

  time = vt_pform_wtime();
  was_recorded = vt_enter(VT_CURRENT_THREAD, &time,
                          memhook_regid[MEMHOOK_REG_FREE]);

  if ( NULL != ptr )
  {
    bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ptr - SIZEOF_VOIDP ) );
  }
  else
  {
    bytes = 0;
  }

  free(ptr); /* call recursively */

  /* update counter value */
  if ( bytes <= memalloc_val )
    memalloc_val -= bytes;
  else
    memalloc_val = 0;

  time = vt_pform_wtime();

  if ( was_recorded && bytes > 0 )
  {
    /* write marker, if desired */
    if( memalloc_marker )
    {
      vt_marker(VT_CURRENT_THREAD, &time, memalloc_mid[MEMHOOK_MARK_FREE],
                "Freed %llu Bytes", (unsigned long long)bytes);
    }

    /* write counter value */
    vt_count(VT_CURRENT_THREAD, &time, memalloc_cid, memalloc_val);
  }

  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON(); /* restore our own hooks */
}
