/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include "vt_comp.h"
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_strdup.h"
#include "vt_trc.h"
#if defined (VT_OMPI) || defined (VT_OMP)
# include "opari_omp.h"
# define VT_MY_THREAD   omp_get_thread_num() 
# define VT_NUM_THREADS omp_get_max_threads() 
#else
# define VT_MY_THREAD   0
# define VT_NUM_THREADS 1 
#endif

static int dyn_init = 1;       /* is initialization needed? */

/*
 *-----------------------------------------------------------------------------
 * Simple hash table to map function addresses to region names/identifier
 *-----------------------------------------------------------------------------
 */

typedef struct HN {
  long id;            /* hash code (address of function) */
  uint32_t vtid;      /* associated region identifier    */
  char* func;
  char* file;
  int lno;
  struct HN* next;
} HashNode;

#define HASH_MAX 1021

static HashNode* htab[HASH_MAX];

/*
 * Stores region identifier `e' under hash code `h'
 */

static HashNode* hash_put(long h, uint32_t e) {
  long id = h % HASH_MAX;
  HashNode *add = (HashNode*)malloc(sizeof(HashNode));
  add->id = h;
  add->vtid = e;
  add->next = htab[id];
  htab[id] = add;
  return add;
}

/*
 * Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL
 */

static HashNode* hash_get(long h) {
  long id = h % HASH_MAX;
  HashNode *curr = htab[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/*
 * Register new region
 */

static HashNode *register_region(long addr, char* func, char* file, int lno) {
  uint32_t rid;
  uint32_t fid;
  HashNode* nhn;

  /* -- register file and region and store region identifier -- */
  fid = vt_def_file(file);
  rid = vt_def_region(func, fid, lno, VT_NO_LNO, VT_DEF_GROUP, VT_FUNCTION);
  nhn = hash_put(addr, rid);
  nhn->func = vt_strdup(func);
  nhn->file = vt_strdup(file);
  nhn->lno  = lno;
  return nhn;
}

static long *stk_level;
static int extra_exits = 0;

void VT_Dyn_finalize(void);
void VT_Dyn_start(void* addr, char* name, char* fname, int lno);
void VT_Dyn_end(void* addr);
void VT_Dyn_attach(void);

void VT_Dyn_finalize()
{
  int mt = VT_MY_THREAD;
  uint64_t time;

  VT_MEMHOOKS_OFF();

  extra_exits = stk_level[mt];
  while(stk_level[mt] > 0) {
    stk_level[mt]--;
    time = vt_pform_wtime();
    vt_exit(&time);
  }
  vt_close();
}

/*
 * This function is called at the entry of each function
 */

void VT_Dyn_start(void* addr, char* name, char* fname, int lno)
{
  HashNode *hn;
  int mt = VT_MY_THREAD;
  uint64_t time;

  /* -- ignore events if VT is initializing -- */
  if( !dyn_init && !VT_IS_ALIVE ) return;

  /* -- if not yet initialized, initialize VampirTrace -- */
  if ( dyn_init ) {
    VT_MEMHOOKS_OFF();
    dyn_init = 0;
    stk_level = (long*)calloc(VT_NUM_THREADS, sizeof(long));
    vt_open();
    vt_comp_finalize = &VT_Dyn_finalize;
    VT_MEMHOOKS_ON();
  }

  /* -- return, if tracing is disabled? -- */
  if ( !VT_IS_TRACE_ON() ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( (hn = hash_get((long) addr)) == 0 ) {
    /* -- region entered the first time, register region -- */
#   if defined (VT_OMPI) || defined (VT_OMP)
    if (omp_in_parallel()) {
#     pragma omp critical (vt_comp_dyninst_1)
      {
	if ( (hn = hash_get((long) addr)) == 0 ) {
	  hn = register_region((long)addr, name, fname, lno);
	}
      }
    } else {
      hn = register_region((long)addr, name, fname, lno);
    }
#   else
    hn = register_region((long)addr, name, fname, lno);
#   endif
  }

  /* -- write enter record -- */
  vt_enter(&time, hn->vtid);
  stk_level[mt]++;

  VT_MEMHOOKS_ON();
}

/*
 * This function is called at the exit of each function
 */

void VT_Dyn_end(void* addr)
{
  int mt = VT_MY_THREAD;
  uint64_t time;

  /* -- return, if tracing is disabled? -- */
  if ( !VT_IS_TRACE_ON() ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  vt_exit(&time);
  stk_level[mt]--;

  VT_MEMHOOKS_ON();
}

/*
 * Mutatee's signal handler ...
 */

void sig_usr1_handler(int signum);
void sig_usr2_handler(int signum);
int mutatee_cont = 0;
int mutator_error = 0;

/* ... for continue execution of mutatee */
void sig_usr1_handler(int signum)
{
  mutatee_cont = 1;
}

/* ... for errors, which occur during attaching mutator process */
void sig_usr2_handler(int signum)
{
  mutator_error = 1;
  mutatee_cont = 1;
}

/*
 * This function is called by the shared dyninst attach library (vt.dynatt)
 */

void VT_Dyn_attach()
{
  char* mutatee_path = vt_env_apppath();
  int mutatee_pid = getpid();
  
  vt_cntl_msg("Attaching instrumentor at pid %i ...", mutatee_pid);

  /* Install signal handler for continue execution (SIGUSR1)
     and abort execution (SIGUSR2)
  */
  if( signal(SIGUSR1, sig_usr1_handler) == SIG_ERR )
    vt_error_msg("Could not install handler for signal SIGUSR1");

  if( signal(SIGUSR2, sig_usr2_handler) == SIG_ERR )
    vt_error_msg("Could not install handler for signal SIGUSR2");

  /* Attach dyninst instrumentor on running executable
   */
  switch( fork() )
  {
    case -1:
    {
      vt_error_msg("Could not attach dyninst instrumentor");
      break;
    }
    case 0:
    {
      int rc;
      char cmd[1024];
      char* blist = vt_env_dyn_blacklist();
      char* shlibs = vt_env_dyn_shlibs();
      char* shlibs_arg = NULL;

      /* Restore original signal handler
       */
      signal(SIGUSR1, SIG_DFL);
      signal(SIGUSR2, SIG_DFL);
       
      /* Replace all colons to commas in list of shared libraries
       */
      if ( shlibs && strlen(shlibs) > 0 )
      {
	char* tk;
	shlibs_arg = (char*)calloc(strlen(shlibs)+2, sizeof(char));
	tk = strtok( shlibs, ":" );
	do
	{
	   strcat(shlibs_arg, tk);
	   strcat(shlibs_arg, ",");
	} while( (tk = strtok( 0, ":" )) );
	shlibs_arg[strlen(shlibs_arg)-1] = '\0';
      }

#ifdef BINDIR
      if(access(BINDIR "/vtdyn", X_OK) == 0)
      {
        sprintf(cmd, "%s/vtdyn %s %s %s %s %s -p %i %s", BINDIR,
		vt_env_is_verbose() ? "-v" : "",
		blist ? "-b" : "", blist ? blist : "",
		shlibs_arg ? "-s" : "", shlibs_arg ? shlibs_arg : "",
		mutatee_pid,
		mutatee_path ? mutatee_path : "");
      }
      else
#endif
      {
	sprintf(cmd, "vtdyn %s %s %s %s %s -p %i %s",
		vt_env_is_verbose() ? "-v" : "",
		blist ? "-b" : "", blist ? blist : "",
		shlibs_arg ? "-s" : "", shlibs_arg ? shlibs_arg : "",
		mutatee_pid,
		mutatee_path ? mutatee_path : "");
      }
      
      if ( shlibs_arg )
	free(shlibs_arg);

      /* Start mutator (instrumentor) */
      rc = system(cmd);

      /* Kill mutatee, if an error occurred during attaching
       */
      if(rc != 0)
	kill(mutatee_pid, SIGUSR2);

      exit(rc);

      break;
    }
    default:
    {
      /* set/overwrite environment variable VT_UNIFY to zero,
	 so VampirTrace don't unify local traces (DYNINST Bug?) */
#if defined(HAVE_SETENV) && HAVE_SETENV
      setenv("VT_UNIFY", "no", 1);
#else /* HAVE_SETENV */
      putenv("VT_UNIFY=no");
#endif
      
      /* Wait until mutator send signal to continue execution
       */
      vt_cntl_msg("[%i]: Wait until instrumentation is done ...", mutatee_pid);

      do { sleep(1); } while(mutatee_cont == 0);
      
      if ( !mutator_error )
      {
	/* Restore original signal handler
	 */
	signal(SIGUSR1, SIG_DFL);
	signal(SIGUSR2, SIG_DFL);
      }
      else
      {
	vt_error_msg("An error occurred during instrumenting");
      }

      break;
    }
  }
}
