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

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_fbindings.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#define VTRACE
#undef VTRACE_NO_REGION
#include "vt_user.h"

#include "util/hash.h"

#include <stdlib.h>
#include <string.h>

static int vt_init = 1;        /* is initialization needed? */

#define VT_INIT \
  if ( vt_init ) { \
    VT_MEMHOOKS_OFF(); \
    vt_init = 0; \
    vt_open(); \
    VT_MEMHOOKS_ON(); \
  }

#define HASH_MAX 1021

typedef struct HN_RegionS
{
  const char* name;   /* region name (hash-key) */
  const char* file;   /* source file (hash-key) */
  int lno;            /* line number within source file (hash-key) */
  uint32_t rid;       /* associated region identifier  */
  struct HN_RegionS* next;
} HN_RegionT;

typedef struct HN_AddrS
{
  unsigned long addr; /* region address (hash-key) */
  uint32_t rid;       /* associated region identifier */
  struct HN_AddrS* next;
} HN_AddrT;

static HN_RegionT* htab_region[HASH_MAX];
static HN_AddrT* htab_addr[HASH_MAX];

static uint32_t hash_get_region(const char* name, const char* file,
                                int lno)
{
  uint32_t idx;
  HN_RegionT* curr;

  /* -- get hash index -- */
  idx = (uint32_t)vt_hash((unsigned char*)name, strlen(name), 0);
  if ( file )
  {
    idx = (uint32_t)vt_hash((unsigned char*)file, strlen(file), idx);
    idx = (uint32_t)vt_hash((unsigned char*)&lno, sizeof(uint32_t), idx);
  }
  idx %= HASH_MAX;

  /* -- search for matching entry at calculated hash index -- */
  curr = htab_region[idx];
  while ( curr )
  {
    if ( strcmp( curr->name, name ) == 0 )
    {
      if ( ( !curr->file && !file ) ||
           ( curr->file && file && strcmp( curr->file, file ) == 0 &&
             curr->lno == lno ) )
      {
        return curr->rid;
      }
    }
    curr = curr->next;
  }

  return VT_NO_ID;
}

static void hash_put_region(const char* name, const char* file, int lno,
                            uint32_t rid)
{
  uint32_t idx;
  HN_RegionT* add;

  /* -- get hash index -- */
  idx = (uint32_t)vt_hash((unsigned char*)name, strlen(name), 0);
  if ( file )
  {
    idx = (uint32_t)vt_hash((unsigned char*)file, strlen(file), idx);
    idx = (uint32_t)vt_hash((unsigned char*)&lno, sizeof(uint32_t), idx);
  }
  idx %= HASH_MAX;

  /* -- allocate/initialize new hash entry -- */
  add = (HN_RegionT*)calloc(1, sizeof(HN_RegionT));
  if ( add == NULL )
    vt_error();

  add->name = strdup(name);
  if ( file )
  {
    add->file = strdup(file);
    add->lno = lno;
  }
  add->rid = rid;

  /* -- insert new hash entry at calculated hash index -- */
  add->next = htab_region[idx];
  htab_region[idx] = add;
}

static uint32_t hash_get_addr(unsigned long addr)
{
  /* -- get hash index */
  unsigned long idx = addr % HASH_MAX;

  /* -- search for matching entry at calculated hash index -- */
  HN_AddrT* curr = htab_addr[idx];
  while ( curr )
  {
    if ( curr->addr == addr )
      return curr->rid;
    curr = curr->next;
  }

  return VT_NO_ID;
}

static void hash_put_addr(unsigned long addr, uint32_t rid)
{
  /* -- get hash index */
  unsigned long idx = addr % HASH_MAX;

  /* -- allocate/initialize new hash entry -- */
  HN_AddrT* add = (HN_AddrT*)malloc(sizeof(HN_AddrT));
  if ( add == NULL )
    vt_error();

  add->addr = addr;
  add->rid = rid;

  /* -- insert new hash entry at calculated hash index -- */
  add->next = htab_addr[idx];
  htab_addr[idx] = add;
}

static uint32_t register_region(unsigned long addr, const char* name,
                                const char* file, int lno)
{
  uint32_t rid;
  uint32_t fid;

  /* -- register file if available -- */
  if ( file && file[0] != '\0' )
  {
    fid = vt_def_scl_file(VT_CURRENT_THREAD, file);
  }
  else
  {
    fid = VT_NO_ID;
    lno = VT_NO_LNO;
  }

  /* -- register region and store region identifier -- */
  rid = vt_def_region(VT_CURRENT_THREAD, name, fid, lno, VT_NO_LNO, NULL,
                      VT_FUNCTION);
  if ( addr )
    hash_put_addr( addr, rid );
  else
    hash_put_region( name, (fid != VT_NO_ID) ? file : NULL, lno, rid );

  return rid;
}

void VT_User_start__(const char* name, const char* file, int lno)
{
  uint32_t rid;
  uint64_t time;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* -- get region identifier by address -- */
  if ( (rid = hash_get_addr((unsigned long)name)) == VT_NO_ID )
  {
    /* -- region entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( (rid = hash_get_addr((unsigned long)name)) == VT_NO_ID )
      rid = register_region((unsigned long)name, name, file, lno);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    rid = register_region((unsigned long)name, name, file, lno);
#endif /* VT_MT || VT_HYB */
  }

  /* -- write enter record -- */
  vt_enter(VT_CURRENT_THREAD, &time, rid);

  VT_MEMHOOKS_ON();
}

void VT_User_end__(const char* name)
{
  uint64_t time;

  (void)name;

  VT_MEMHOOKS_OFF();

  /* -- write exit record -- */
  time = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON();
}

unsigned int VT_User_def__(const char* name, const char* file, int lno)
{
  uint32_t rid;

  VT_INIT;

  VT_MEMHOOKS_OFF();

  /* -- get region identifier by name, file, and line number -- */
  if ( (rid = hash_get_region(name, file, lno)) == VT_NO_ID )
  {
    /* -- register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( (rid = hash_get_region(name, file, lno)) == VT_NO_ID )
      rid = register_region(0, name, file, lno);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    rid = register_region(0, name, file, lno);
#endif /* VT_MT || VT_HYB */
  }

  VT_MEMHOOKS_ON();

  return rid;
}

void VT_User_start_id__(unsigned int rid)
{
  uint64_t time;

  VT_MEMHOOKS_OFF();

  /* -- write enter record -- */
  time = vt_pform_wtime();
  vt_enter(VT_CURRENT_THREAD, &time, rid);

  VT_MEMHOOKS_ON();
}

void VT_User_end_id__(unsigned int rid)
{
  uint64_t time;

  (void)rid;

  VT_MEMHOOKS_OFF();

  /* -- write exit record -- */
  time = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON();
}

/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_start___f(const char* name, const char* file, int* lno,
                                  int nl, int fl))
{
  uint32_t rid;
  uint64_t time;

  int namlen;
  int fillen;
  char fnambuf[128];
  char ffilbuf[1024];

  VT_INIT;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, name, namlen);
  fnambuf[namlen] = '\0';
  fillen = ( fl < 1024 ) ? fl : 1023;
  strncpy(ffilbuf, file, fillen);
  ffilbuf[fillen] = '\0';

  /* -- get region identifier by address -- */
  if ( (rid = hash_get_addr((unsigned long)name)) == VT_NO_ID )
  {
    /* -- region entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( (rid = hash_get_addr((unsigned long)name)) == VT_NO_ID )
      rid = register_region((unsigned long)name, fnambuf, ffilbuf, *lno);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    rid = register_region((unsigned long)name, fnambuf, ffilbuf, *lno);
#endif /* VT_MT || VT_HYB */
  }

  /* -- write enter record -- */
  vt_enter(VT_CURRENT_THREAD, &time, rid);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_start__, VT_USER_START__,
                           VT_User_start___f,
                           (const char* name, const char* file, int* lno,
                            int nl, int fl),
                           (name, file, lno, nl, fl))


VT_DECLDEF(void VT_User_end___f(const char* name, int nl))
{
  uint64_t time;

  VT_MEMHOOKS_OFF();

  /* -- write exit record -- */
  time = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_end__, VT_USER_END__,
                           VT_User_end___f,
                           (const char *name, int nl),
                           (name, nl))

VT_DECLDEF(void VT_User_def___f(const char* name, const char* file,
                                int* lno, unsigned int* rid, int nl, int fl))
{
  int namlen;
  int fillen;
  char fnambuf[128];
  char ffilbuf[1024];

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, name, namlen);
  fnambuf[namlen] = '\0';
  fillen = ( fl < 1024 ) ? fl : 1023;
  strncpy(ffilbuf, file, fillen);
  ffilbuf[fillen] = '\0';

  /* -- get region identifier from C version */
  *rid = VT_User_def__(fnambuf, ffilbuf, *lno);
} VT_GENERATE_F77_BINDINGS(vt_user_def__, VT_USER_DEF__,
                           VT_User_def___f,
                           (const char* name, const char* file,
                            int* lno, unsigned int* rid, int nl, int fl),
                           (name, file, lno, rid, nl, fl))

VT_DECLDEF(void VT_User_start_id___f(unsigned int* rid))
{
  uint64_t time;

  VT_MEMHOOKS_OFF();

  /* -- write enter record -- */
  time = vt_pform_wtime();
  vt_enter(VT_CURRENT_THREAD, &time, *rid);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_start_id__, VT_USER_START_ID__,
                           VT_User_start_id___f,
                           (unsigned int* rid),
                           (rid))

VT_DECLDEF(void VT_User_end_id___f(unsigned int* rid))
{
  uint64_t time;

  (void)rid;

  VT_MEMHOOKS_OFF();

  /* -- write exit record -- */
  time = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &time);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_end_id__, VT_USER_END_ID__,
                           VT_User_end_id___f,
                           (unsigned int* rid),
                           (rid))
