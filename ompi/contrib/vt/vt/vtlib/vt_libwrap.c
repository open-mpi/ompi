/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#define _GNU_SOURCE

#include "config.h"

#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_libwrap.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* maximum number of library wrapper objects */
#define MAX_LW 16

/* maximum number of handles for shared libraries
   =VT_LIBWRAP_MAX_SHLIBS [+1 for RTLD_NEXT] */
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
# define MAX_HANDLES (VT_LIBWRAP_MAX_SHLIBS+1)
#else /* HAVE_DECL_RTLD_NEXT */
# define MAX_HANDLES VT_LIBWRAP_MAX_SHLIBS
#endif /* HAVE_DECL_RTLD_NEXT */

/* data structure for library wrapper object */
struct VTLibwrap_struct
{
  VTLibwrapAttr* attr;                 /* attributes */
  void*          handlev[MAX_HANDLES]; /* vector of handles */
  uint32_t       handlen;              /* number of handles */
};

static VTLibwrap* lwv[MAX_LW]; /* vector of library wrapper objects */
static uint32_t   lwn = 0;     /* number of library wrapper objects */

/* default library wrapper attributes */
static VTLibwrapAttr default_attr = VT_LIBWRAP_ATTR_DEFAULT;

/* mutex for locking creation of library wrapper objects */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
static VTThrdMutex* lw_create_mutex = NULL;
#endif /* VT_MT || VT_HYB || VT_JAVA */

void vt_libwrap_init()
{
}

void vt_libwrap_finalize()
{
  /* destroy mutex for locking creation of library wrapper objects,
     if necessary */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  if( lw_create_mutex != NULL )
    VTThrd_deleteMutex(&lw_create_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */
}

void VTLibwrap_create(VTLibwrap** lw, VTLibwrapAttr* lwattr)
{
  uint8_t error = 0;
  char error_msg[1024] = "";

  VT_MEMHOOKS_OFF();

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTThrd_lock(&lw_create_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */

  do
  {
    /* library wrapper object already exists ? */
    if( *lw != VT_LIBWRAP_NULL )
    {
      error = 1;
      break;
    }

    /* maximum number of library wrapper objects reached ? */
    if( lwn + 1 > MAX_LW )
    {
      error = 1;
      snprintf(error_msg, sizeof(error_msg) - 1,
               "Cannot create more than %d library wrapper objects", MAX_LW);
      break;
    }

    /* allocate new library wrapper object */
    *lw = (VTLibwrap*)calloc(1, sizeof(VTLibwrap));
    if( *lw == NULL )
    {
      error = 1;
      snprintf(error_msg, sizeof(error_msg) - 1,
               "Cannot allocate memory for library wrapper object");
      break;
    }

    /* if not attributes given, use the default attributes */
    (*lw)->attr = lwattr ? lwattr : &default_attr;

    /* call attributes initializer function, if necessary */
    if( (*lw)->attr->init_func )
      (*lw)->attr->init_func((*lw)->attr);

    /* shared libraries specified ? */
    if( (*lw)->attr->shlibs_num > 0 )
    {
      int i;

      /* number of specified shared libraries to high ? */
      if( (*lw)->attr->shlibs_num > VT_LIBWRAP_MAX_SHLIBS )
      {
        error = 1;
        snprintf(error_msg, sizeof(error_msg) - 1,
                 "Number of shared libraries for searching actual library "
                 "functions exceeds VampirTrace maximum of %d",
                 VT_LIBWRAP_MAX_SHLIBS);
        break;
      }

      /* get handles for specified shared libraries */
      for( i = 0; i < (*lw)->attr->shlibs_num; i++ )
      {
        (void)dlerror();
        (*lw)->handlev[i] = dlopen((*lw)->attr->shlibs[i],
                                   RTLD_LAZY | RTLD_LOCAL);
        if( (*lw)->handlev[i] == NULL )
        {
          error = 1;
          snprintf(error_msg, sizeof(error_msg) - 1,
                   "dlopen(\"%s\") failed: %s",
                   (*lw)->attr->shlibs[i], dlerror());
          break;
        }
        (*lw)->handlen++;
      }
      if( error ) break;
    }

    /* append 'RTLD_NEXT' to the vector of handles, if possible */
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
    (*lw)->handlev[(*lw)->handlen++] = RTLD_NEXT;
#else /* HAVE_DECL_RTLD_NEXT */
    if( (*lw)->handlen == 0 )
    {
      error = 1;
      snprintf(error_msg, sizeof(error_msg) - 1,
               "No shared library for searching actual library functions "
               "specified");
      break;
    }
#endif /* HAVE_DECL_RTLD_NEXT */

    /* store new library wrapper object */
    lwv[lwn++] = *lw;
  } while(0);

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTThrd_unlock(&lw_create_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */

  /* error occurred ? */
  if( error )
  {
    /* abort VampirTrace, if necessary */
    if( error_msg[0] ) vt_error_msg(error_msg);
  }
  else
  {
    /* initialize VampirTrace, if necessary */
    if( !(*lw)->attr->wait_for_init && !vt_is_alive )
      vt_open();
  }

  VT_MEMHOOKS_ON();
}

void VTLibwrap_delete(VTLibwrap** lw)
{
  uint32_t i;

  vt_assert(*lw);

  /* close all opened handles */
  for( i = 0; i < (*lw)->handlen; i++ )
  {
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
    if( (*lw)->handlev[i] != RTLD_NEXT )
    {
#endif /* HAVE_DECL_RTLD_NEXT */
    (void)dlerror();
    if( dlclose((*lw)->handlev[i]) != 0 )
      vt_error_msg("dlclose(\"%s\") failed: %s",
                   (*lw)->attr->shlibs[i], dlerror());
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
    }
#endif /* HAVE_DECL_RTLD_NEXT */
  }

  free(*lw);
  *lw = VT_LIBWRAP_NULL;
}

void VTLibwrap_delete_all()
{
  uint32_t i;

  /* delete all library wrapper objects */
  for( i = 0; i < lwn; i++ )
    if( lwv[i] != VT_LIBWRAP_NULL ) VTLibwrap_delete(&(lwv[i]));
}

void VTLibwrap_func_init(const VTLibwrap* lw, const char* func,
                         const char* file, int line,
                         void** funcptr, int* funcid)
{
  uint32_t i;

  vt_assert(lw);

  VT_MEMHOOKS_OFF();

  if( !(*funcptr) )
  {
    /* array for dlsym error messages */
    char dlsym_errors[MAX_HANDLES][256];

    /* search all handles for function */
    for( i = 0; i < lw->handlen && !(*funcptr); i++ )
    {
      (void)dlerror();
      *funcptr = dlsym(lw->handlev[i], func);

      /* function not found ? */
      if( !(*funcptr) )
      {
        char* dlsym_error_msg = dlerror();

        /* store dlsym error message, if available */
        if( dlsym_error_msg )
        {
          strncpy(dlsym_errors[i], dlsym_error_msg, sizeof(dlsym_errors[i])-1);
        }
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
        /* usually the dlsym error message for RTLD_NEXT may empty */
        else if( i == lw->handlen - 1 )
        {
          snprintf(dlsym_errors[i], sizeof(dlsym_errors[i])-1,
                   "RTLD_NEXT: symbol not found: %s", func);
        }
#endif /* HAVE_DECL_RTLD_NEXT */
        else
        {
          strncpy(dlsym_errors[i], "unknown error", sizeof(dlsym_errors[i])-1);
        }
      }
    }

    /* merge all dlsym error messages to one message, if
       function not found */
    if( !(*funcptr) )
    {
      char* dlsym_errors_merged;

      dlsym_errors_merged =
        (char*)calloc(lw->handlen * sizeof(dlsym_errors[0]), sizeof(char));
      if( dlsym_errors_merged == NULL )
        vt_error();

      for( i = 0; i < lw->handlen; i++ )
      {
        if( i > 0 )
          strncat(dlsym_errors_merged, "\n", 255 - strlen(dlsym_errors_merged));
        strncat(dlsym_errors_merged, dlsym_errors[i],
                255 - strlen(dlsym_errors_merged));
      }
      vt_error_msg("dlsym(\"%s\") failed:\n%s", func, dlsym_errors_merged);
    }
  }

  /* get function identifier, if necessary */
  if( vt_is_alive )
  {
    if( *funcid == VT_LIBWRAP_NOID )
    {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
      VTTHRD_LOCK_IDS();
      if( *funcid == VT_LIBWRAP_NOID )
      {
#endif /* VT_MT || VT_HYB || VT_JAVA */
      uint32_t fid = VT_NO_ID;
      uint32_t lno = VT_NO_LNO;

      /* register source file, if available */
      if( file != NULL && line > 0 )
      {
        fid = vt_def_scl_file(file);
        lno = line;
      }
      /* register function */
      *funcid = vt_def_region(func, fid, lno, VT_NO_LNO,
                              lw->attr->func_group, VT_FUNCTION);
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
      }
      VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB || VT_JAVA */
    }
  }

  VT_MEMHOOKS_ON();
}

void VTLibwrap_func_start(const VTLibwrap* lw, const int funcid)
{
  uint64_t time;

  vt_assert(lw);

  if( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  vt_assert(funcid != VT_LIBWRAP_NOID);

  time = vt_pform_wtime();

  (void)vt_enter(&time, funcid);

  VT_MEMHOOKS_ON();
}

void VTLibwrap_func_end(const VTLibwrap* lw, const int funcid)
{
  uint64_t time;

  vt_assert(lw);

  if( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  vt_assert(funcid != VT_LIBWRAP_NOID);

  time = vt_pform_wtime();

  vt_exit(&time);

  VT_MEMHOOKS_ON();
}
