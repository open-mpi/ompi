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
#include "vt_thrd.h"
#include "vt_trc.h"

#include "util/installdirs.h"

static int dyn_init = 1;       /* is initialization needed? */

static uint32_t* rtab = NULL;  /* region id lookup table */

/*
 * Register new region
 */

static void register_region(uint32_t* rid, char* func, char* file, int lno)
{
  uint32_t fid;

  /* Register file if available
   */
  if( file[0] )
  {
    fid = vt_def_scl_file(VT_CURRENT_THREAD, file);
  }
  else
  {
    fid = VT_NO_ID;
    lno = VT_NO_LNO;
  }

  /* Register region and store region identifier */
  *rid = vt_def_region(VT_CURRENT_THREAD, func, fid, lno, VT_NO_LNO, NULL,
                       VT_FUNCTION);
}

void VT_Dyn_start(uint32_t index, char* name, char* fname, int lno);
void VT_Dyn_end(uint32_t index);
void VT_Dyn_attach(void);
void VT_Dyn_finalize(void);

/*
 * This function is called at the entry of each function
 */

void VT_Dyn_start(uint32_t index, char* name, char* fname, int lno)
{
  uint64_t time;
  uint32_t* rid;

  vt_libassert(index < VT_MAX_DYNINST_REGIONS);

  /* Ignore events if VT is initializing */
  if( !dyn_init && !vt_is_alive ) return;

  /* If not yet initialized, initialize VampirTrace */
  if ( dyn_init )
  {
    VT_MEMHOOKS_OFF();
    dyn_init = 0;
    rtab = (uint32_t*)calloc(VT_MAX_DYNINST_REGIONS, sizeof(uint32_t));
    if ( rtab == NULL )
      vt_error();
    vt_open();
    vt_comp_finalize = VT_Dyn_finalize;
    VT_MEMHOOKS_ON();
  }

  /* If VampirTrace already finalized, return */
  if ( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* Get region identifier
   */
  rid = &(rtab[index]);
  if ( *rid == 0 )
  {
    /* If region entered the first time, register region
     */
#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
    if ( *rid == 0 )
      register_region(rid, name, fname, lno);
    VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
    register_region(rid, name, fname, lno);
#endif /* VT_MT || VT_HYB */
  }

  /* Write enter record */
  vt_enter(VT_CURRENT_THREAD, &time, *rid);

  VT_MEMHOOKS_ON();
}

/*
 * This function is called at the exit of each function
 */

void VT_Dyn_end(uint32_t index)
{
  uint64_t time;

  vt_libassert(index < VT_MAX_DYNINST_REGIONS);

  /* If VampirTrace already finalized, return */
  if ( !vt_is_alive ) return;

  /* If region id isn't present, return */
  if ( rtab[index] == 0 ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* Write exit record */
  vt_exit(VT_CURRENT_THREAD, &time);

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
 * This function is called by the shared Dyninst attach library (libvt-dynatt)
 */

void VT_Dyn_attach()
{
  int mutatee_pid = getpid();

  vt_cntl_msg(1, "[%i]: Attaching instrumentor", mutatee_pid);

  /* Install signal handler for continue execution (SIGUSR1)
     and abort execution (SIGUSR2)
  */
  if( signal(SIGUSR1, sig_usr1_handler) == SIG_ERR )
    vt_error_msg("Could not install handler for signal SIGUSR1");

  if( signal(SIGUSR2, sig_usr2_handler) == SIG_ERR )
    vt_error_msg("Could not install handler for signal SIGUSR2");

  /* The Dyninst attach library (libvt-dynatt) could be set by LD_PRELOAD.
     Unset this environment variable to avoid recursion. */
  putenv((char*)"LD_PRELOAD=");
  putenv((char*)"DYLD_INSERT_LIBRARIES="); /* equivalent on MacOS */

  /* Attach Dyninst instrumentor on running executable
   */
  switch( fork() )
  {
    case -1:
    {
      vt_error_msg("Could not attach Dyninst instrumentor");
      break;
    }
    case 0:
    {
      int rc;
      char cmd[1024];
      char* filter = vt_env_filter_spec();
      char* shlibs = vt_env_dyn_shlibs();
      char* shlibs_arg = NULL;
      char* mutatee_path = NULL;

      /* Restore original signal handler
       */
      signal(SIGUSR1, SIG_DFL);
      signal(SIGUSR2, SIG_DFL);

      /* Try to get path of mutatee
       */
      vt_pform_init();
      mutatee_path = vt_env_apppath();

      /* Replace all colons by commas in list of shared libraries
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

      snprintf(cmd, sizeof(cmd)-1, "%s/vtdyn %s %s %s %s %s %s %s %s -p %i %s",
              vt_installdirs_get(VT_INSTALLDIR_BINDIR),
              (vt_env_verbose() == 0) ? "-q" : "",
              (vt_env_verbose() >= 2) ? "-v" : "",
              filter ? "-f" : "", filter ? filter : "",
              shlibs_arg ? "-s" : "", shlibs_arg ? shlibs_arg : "",
              (vt_env_dyn_ignore_nodbg()) ? "--ignore-nodbg" : "",
              (vt_env_dyn_detach()) ? "" : "--nodetach",
              mutatee_pid,
              mutatee_path ? mutatee_path : "");

      if ( shlibs_arg )
        free(shlibs_arg);

      /* Start mutator (instrumentor) */
      vt_cntl_msg(2, "[%i]: Executing %s", mutatee_pid, cmd);
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
      /* Wait until mutator send signal to continue execution
       */
      vt_cntl_msg(1, "[%i]: Waiting until instrumentation is done",
                  mutatee_pid);

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

/*
 * Finalize instrumentation interface
 */

void VT_Dyn_finalize()
{
  if ( dyn_init ) return;

  /* Free region id table
   */
  free( rtab );
  rtab = NULL;

  dyn_init = 1;
}
