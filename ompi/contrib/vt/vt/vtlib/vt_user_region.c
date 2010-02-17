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

#include <stdlib.h>
#include <string.h>

#include "vt_fbindings.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#define VTRACE
#undef VTRACE_NO_REGION
#include "vt_user.h"

/*
 *-----------------------------------------------------------------------------
 * Simple hash table to map function names to region identifier
 *-----------------------------------------------------------------------------
 */

typedef struct HN {
  unsigned long id;   /* hash code (address of function name) */
  uint32_t vtid;      /* associated region identifier  */
  struct HN* next;
} HashNode;

#define HASH_MAX 1021

static int vt_init = 1;       /* is initialization needed? */

static HashNode* htab[HASH_MAX];

/*
 * Stores region identifier `e' under hash code `h'
 */

static void hash_put(unsigned long h, uint32_t e) {
  unsigned long id = h % HASH_MAX;
  HashNode *add = (HashNode*)malloc(sizeof(HashNode));
  add->id = h;
  add->vtid = e;
  add->next = htab[id];
  htab[id] = add;
}

/*
 * Lookup hash code `h'
 * Returns region identifier if already stored, otherwise VT_NO_ID
 */

static uint32_t hash_get(unsigned long h) {
  unsigned long id = h % HASH_MAX;
  HashNode *curr = htab[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr->vtid;
    }
    curr = curr->next;
  }
  return VT_NO_ID;
}

/*
 * Register new region
 */

static uint32_t register_region(const char *name, unsigned long addr, const char* file, int lno) {
  uint32_t rid;
  uint32_t fid;

  /* -- register file and region and store region identifier -- */
  fid = vt_def_scl_file(file);
  rid = vt_def_region(name, fid, lno, VT_NO_LNO, NULL, VT_FUNCTION);
  hash_put(addr == 0 ? (unsigned long) name : addr, rid);
  return rid;
}

/*
 * This function is called at the entry of each function
 * C/C++ version
 */

void VT_User_start__(const char* name, const char *file, int lno) {
  uint32_t rid;
  uint64_t time;

  /* -- if not yet initialized, initialize VampirTrace -- */
  if ( vt_init ) {
    VT_MEMHOOKS_OFF();
    vt_init = 0;
    vt_open();
    VT_MEMHOOKS_ON();
  }

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( (rid = hash_get((unsigned long) name)) == VT_NO_ID ) {
    /* -- region entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( (rid = hash_get((unsigned long) name)) == VT_NO_ID )
      rid = register_region(name, 0, file, lno);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    rid = register_region(name, 0, file, lno);
#endif /* VT_MT || VT_HYB */
  }

  /* -- write enter record -- */
  vt_enter(&time, rid);

  VT_MEMHOOKS_ON();
}

/*
 * This function is called at the exit of each function
 * C/C++ version
 */

void VT_User_end__(const char *name) {
  uint64_t time;

  VT_MEMHOOKS_OFF();

  /* -- write exit record -- */
  time = vt_pform_wtime();
  vt_exit(&time);

  VT_MEMHOOKS_ON();
}

/*
 * This function is called at the entry of each function
 * Fortran version
 */

void VT_User_start___f(const char* name, const char *file, int *lno, int nl, int fl);
void VT_User_end___f(const char *name, int nl);
static char fnambuf[128];
static char ffilbuf[1024];

void VT_User_start___f(const char* name, const char *file, int *lno, int nl, int fl) {
  uint32_t rid;
  uint64_t time;
  int namlen;
  int fillen;

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  fillen = ( fl < 1024 ) ? fl : 1023;
  strncpy(fnambuf, name, namlen);
  fnambuf[namlen] = '\0';
  strncpy(ffilbuf, file, fillen);
  ffilbuf[fillen] = '\0';

  /* -- if not yet initialized, initialize VampirTrace -- */
  if ( vt_init ) {
    VT_MEMHOOKS_OFF();
    vt_init = 0;
    vt_open();
    VT_MEMHOOKS_ON();
  }

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( (rid = hash_get((unsigned long) name)) == VT_NO_ID ) {
    /* -- region entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( (rid = hash_get((unsigned long) name)) == VT_NO_ID )
      rid = register_region(fnambuf, (unsigned long) name, ffilbuf, *lno);
    VTTHRD_UNLOCK_IDS();
#else
    rid = register_region(fnambuf, (unsigned long) name, ffilbuf, *lno);
#endif
  }

  /* -- write enter record -- */
  vt_enter(&time, rid);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_start__, VT_USER_START__,
			   VT_User_start___f,
			   (const char* name, const char *file, int *lno, int nl, int fl),
			   (name, file, lno, nl, fl))

/*
 * This function is called at the exit of each function
 * Fortran version
 */

void VT_User_end___f(const char *name, int nl) {
  uint64_t time;

  VT_MEMHOOKS_OFF();

  /* -- write exit record -- */
  time = vt_pform_wtime();
  vt_exit(&time);

  VT_MEMHOOKS_ON();
} VT_GENERATE_F77_BINDINGS(vt_user_end__, VT_USER_END__,
			   VT_User_end___f,
			   (const char *name, int nl),
			   (name, nl))
