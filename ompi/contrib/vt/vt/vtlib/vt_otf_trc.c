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

#include "config.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>

#include "vt_thrd.h"
#include "vt_trc.h"
#include "vt_otf_gen.h"
#include "vt_env.h"
#include "vt_fork.h"
#include "vt_iowrap.h"
#include "vt_libcwrap.h"
#include "vt_memhook.h"
#include "vt_metric.h"
#include "vt_pform.h"
#include "vt_error.h"

#include "util/hash.h"
#include "util/installdirs.h"

#include "otf.h"

#if defined(VT_LIBWRAP)
# include "vt_libwrap.h"
#endif /* VT_LIBWRAP */

#if ((defined(VT_MT) || defined(VT_HYB)) && defined(VT_PTHREAD))
# include "vt_pthreadreg.h"
#endif /* (VT_MT || VT_HYB) && VT_PTHREAD */

#if (defined(VT_MPI) || defined(VT_HYB))
# include "vt_mpireg.h"
# include "vt_sync.h"
# include "vt_unimci.h"
# if (defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
#   include "vt_esync.h"
# endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */
# include "mpi.h"
#endif /* VT_MPI || VT_HYB */

#if defined(VT_GETCPU)
# include "vt_getcpu.h"
# define UPDATE_CPUID(time)                                                   \
  if ( vt_env_cpuidtrace() &&                                                 \
       VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON )            \
  {                                                                           \
    uint8_t changed;                                                          \
    vt_getcpu_read(&(VTTHRD_CPUID_VAL(VTThrdv[VT_MY_THREAD])), &changed);     \
    if ( changed ) {                                                          \
      VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),                  \
                          (time),                                             \
                          vt_getcpu_cid,                                      \
                          VTTHRD_CPUID_VAL(VTThrdv[VT_MY_THREAD]));           \
    }                                                                         \
  }
#else /* VT_GETCPU */
# define UPDATE_CPUID(time)
#endif /* VT_GETCPU */

#if defined(VT_RUSAGE)
# include "vt_rusage.h"
# define UPDATE_RUSAGE(time)                                                  \
  if ( num_rusage > 0 &&                                                      \
       *(time) >= VTTHRD_RU_NEXT_READ(VTThrdv[VT_MY_THREAD]) ) {              \
    int i;                                                                    \
    uint32_t changed;                                                         \
    vt_rusage_read(VTTHRD_RU_OBJ(VTThrdv[VT_MY_THREAD]),                      \
		   VTTHRD_RU_VALV(VTThrdv[VT_MY_THREAD]), &changed);          \
    for ( i = 0; i < num_rusage; i++ ) {                                      \
      if ( VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) break;  \
      if ( (changed & (1<<i)) != 0 ) {                                        \
        VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),                \
                            (time),                                           \
                            vt_rusage_cidv[i],                                \
                            VTTHRD_RU_VALV(VTThrdv[VT_MY_THREAD])[i]);        \
      }                                                                       \
    }                                                                         \
    VTTHRD_RU_NEXT_READ(VTThrdv[VT_MY_THREAD]) = *(time) + vt_rusage_intv;    \
  }
#else /* VT_RUSAGE */
# define UPDATE_RUSAGE(time)
#endif /* VT_RUSAGE */


/*
 *-----------------------------------------------------------------------------
 * Two simple hash tables 1: maps region groups to region group identifier
 *                        2: maps file name to file identifier
 *-----------------------------------------------------------------------------
 */

#define HASH_TAB__RDESC  0
#define HASH_TAB__SFILE  1
#define HASH_MAX 1021

typedef struct HN_rdesc {
  char*             rdesc;  /* region group name */
  uint32_t          rdid;   /* associated region group identifier */
  struct HN_rdesc*  next;
} HashNode_rdesc;

typedef struct HN_sfile {
  char*             fname;  /* source file name */
  uint32_t          fid;    /* associated source file identifier */
  struct HN_sfile*  next;
} HashNode_sfile;

static HashNode_rdesc* htab_rdesc[HASH_MAX];
static HashNode_sfile* htab_sfile[HASH_MAX];

static void hash_put(int t, const char* n, int i) {
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) % HASH_MAX;
  if(t==HASH_TAB__RDESC)
  {
    HashNode_rdesc *add = (HashNode_rdesc*)malloc(sizeof(HashNode_rdesc));
    add->rdesc = strdup(n);
    add->rdid = i;
    add->next = htab_rdesc[id];
    htab_rdesc[id] = add;
  }
  else if(t==HASH_TAB__SFILE)
  {
    HashNode_sfile *add = (HashNode_sfile*)malloc(sizeof(HashNode_sfile));
    add->fname = strdup(n);
    add->fid = i;
    add->next = htab_sfile[id];
    htab_sfile[id] = add;
  }
}

static void* hash_get(int t, const char* n) {
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) % HASH_MAX;
  if(t==HASH_TAB__RDESC)
  {
    HashNode_rdesc *curr = htab_rdesc[id];
    while ( curr ) {
      if ( strcmp( curr->rdesc, n ) == 0 ) {
        return curr;
      }
      curr = curr->next;
    }
  }
  else if(t==HASH_TAB__SFILE)
  {
    HashNode_sfile *curr = htab_sfile[id];
    while ( curr ) {
      if ( strcmp( curr->fname, n ) == 0 ) {
        return curr;
      }
      curr = curr->next;
    }
  }
  return NULL;
}

static void hash_clear(void)
{
  int i;
  HashNode_rdesc* tmp_rdesc;
  HashNode_sfile* tmp_sfile;

  for ( i = 0; i < HASH_MAX; i++ )
  {
    while( htab_rdesc[i] )
    {
      tmp_rdesc = htab_rdesc[i]->next;
      free( htab_rdesc[i]->rdesc );
      free( htab_rdesc[i] );
      htab_rdesc[i] = tmp_rdesc;
    }
    while( htab_sfile[i] )
    {
      tmp_sfile = htab_sfile[i]->next;
      free( htab_sfile[i]->fname );
      free( htab_sfile[i] );
      htab_sfile[i] = tmp_sfile;
    }
  }
}

/* compiler adapter finalizer */
void (*vt_comp_finalize)(void) = NULL;

int vt_my_ptrace    = -1;
int vt_num_traces   =  1;
int vt_my_trace     =  0;
int vt_my_funique   =  0;

uint32_t vt_trc_regid[VT__TRC_REGID_NUM];

uint32_t vt_misc_cgid = 0;

uint8_t  vt_is_alive  = 0;

static int       init_pid         = -1;
static uint64_t  start_time_epoch =  0;
static uint64_t  my_ltime[2]      =  { 0, 1 };
static int64_t   my_offset[2]     =  { 0, 0 };

static int       max_stack_depth  = 0;

#if defined(VT_METR)
  /* number of performance metrics */
  static int num_metrics = 0;
#endif

#if defined(VT_RUSAGE)
  /* number of resource usage counters */
  static int num_rusage = 0;
#endif

static uint8_t vt_open_called = 0;
static uint8_t vt_close_called = 0;

#if (defined(VT_MT) || defined(VT_HYB))
static VTThrdMutex* init_mutex = NULL;
#endif /* VT_MT || VT_HYB */

/* id counter starting with 1 */
static uint32_t curid = 1;

/* id for VampirTrace related markers */
static uint32_t markid = 0;

static void vt_mpi_sync(uint64_t* time, void* comm);

static uint32_t vt_def_scl(uint32_t fid, uint32_t begln, uint32_t endln);

static uint32_t vt_def_region_desc(const char* rdesc);

static uint32_t vt_get_unique_file_id(void);

static void vt_write_def_header(void);

static void vt_write_uctl_file(void);

void vt_open()
{
  /* do initialization only once */
#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_lock(&init_mutex);
#endif /* VT_MT || VT_HYB */
  if ( vt_open_called ) {
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_unlock(&init_mutex);
#endif /* VT_MT || VT_HYB */
    return;
  }

  vt_open_called = 1;

  /* initialization specific to this platform */
  vt_pform_init();

  /* make sure the hash tables are zero'd */
  memset( htab_rdesc, 0, sizeof(htab_rdesc) );
  memset( htab_sfile, 0, sizeof(htab_sfile) );

  /* get maximum stack depth */
  max_stack_depth = vt_env_max_stack_depth();
  if (max_stack_depth == 0) max_stack_depth = 0x7FFFFFFF;

#if defined(VT_RUSAGE)

  /* initialize resource usage counters */
  num_rusage = vt_rusage_open();

#endif

#if defined(VT_METR)

  /* initialize hardware counters */
  num_metrics = vt_metric_open();

#endif /* VT_METR */

  /* initialize thread object management + trace file creation */
  VTThrd_init();

  /* write start-time as comment to definitions */
  {
    struct timeval tv0, tv1;
    gettimeofday(&tv0, NULL);
    do { gettimeofday(&tv1, NULL); } while ( tv0.tv_usec == tv1.tv_usec );

    start_time_epoch = ((uint64_t)tv1.tv_sec * (uint64_t)1000000) +
       (uint64_t)tv1.tv_usec;
    vt_def_comment("__STARTTIME__ %llu", (unsigned long long)start_time_epoch);
  }

#if !(defined(VT_MPI) || defined(VT_HYB))

  /* write VT related definition comments */
  vt_write_def_header();

  /* get unique file id */
  if (vt_my_ptrace == -1 && vt_env_funique() > -1)
    vt_my_funique = (int)vt_get_unique_file_id();

#endif /* VT_MPI || VT_HYB */

#if !defined(VT_DISABLE_RFG)

  /* set file names for filter and groups specification */
  {
    char* filter_deffile = vt_env_filter_spec();
    char* groups_deffile = vt_env_groups_spec();

    if( filter_deffile )
    {
      RFG_Regions_setFilterDefFile(VTThrdv[0]->rfg_regions, filter_deffile);
      if( !RFG_Regions_readFilterDefFile(VTThrdv[0]->rfg_regions, -1) )
	vt_error_msg("Could not read region filter specification file");
    }

    if( groups_deffile )
    {
      RFG_Regions_setGroupsDefFile(VTThrdv[0]->rfg_regions, groups_deffile);
      if( !RFG_Regions_readGroupsDefFile(VTThrdv[0]->rfg_regions) )
	vt_error_msg("Could not read region group specification file");
    }
  }

#endif /* VT_DISABLE_RFG */

  /* register function "user" */
  vt_trc_regid[VT__TRC_USER] =
    vt_def_region("user", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_FUNCTION);

  /* register function "sync" */
  vt_trc_regid[VT__TRC_SYNC] =
    vt_def_region("sync", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_INTERNAL);

  /* register function "sync time" */
  vt_trc_regid[VT__TRC_SYNCTIME] =
    vt_def_region("sync time", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_INTERNAL);

  /* register function "flush" */
  vt_trc_regid[VT__TRC_FLUSH] =
    vt_def_region("flush", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_INTERNAL);

  /* register function "stat" */
  vt_trc_regid[VT__TRC_STAT] =
    vt_def_region("dump statistics", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_INTERNAL);

  /* register function "off" */
  vt_trc_regid[VT__TRC_OFF] =
    vt_def_region("tracing off", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_INTERNAL);

#if ((defined(VT_MT) || defined(VT_HYB)) && defined(VT_OMP))

  /* register function "parallel region" */
  vt_trc_regid[VT__TRC_OMPPREG] =
    vt_def_region("parallel region", VT_NO_ID, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_OMP_PARALLEL_REGION);

#endif /* (VT_MT || VT_HYB) && VT_OMP */

  /* define counter group for miscellaneous counters (e.g. cpu id) */
  vt_misc_cgid = vt_def_counter_group("Miscellaneous");

  /* define marker for VampirTrace related stuff */
  markid = vt_def_marker("VampirTrace", OTF_MARKER_TYPE_HINT);

#if defined(VT_LIBWRAP)
  vt_libwrap_init();
#endif /* VT_LIBWRAP */

#if defined(VT_LIBCWRAP)

  if (vt_env_libctrace())
  {
    vt_libcwrap_init();

#if defined(VT_FORK)
    vt_fork_init();
#endif /* VT_FORK */

    VT_ENABLE_LIBC_TRACING();
  }

#endif /* VT_LIBCWRAP */

#if defined(VT_IOWRAP)
  if( vt_env_iotrace() )
    vt_iowrap_reg();
#endif

#if defined(VT_MEMHOOK)

  if (vt_env_memtrace())
    vt_memhook_init();

#endif /* VT_MEMHOOK */

#if defined(VT_GETCPU)

  if (vt_env_cpuidtrace())
    vt_getcpu_init();

#endif /* VT_GETCPU */

#if defined(VT_RUSAGE)

  if ( num_rusage > 0 )
    vt_rusage_init();

#endif /* VT_RUSAGE */

  /* initialize MPI related stuff */

#if (defined(VT_MPI) || defined(VT_HYB))

  /* initialize UniMCI if necessary */
# if defined(VT_UNIMCI)

  if ( vt_env_mpicheck() )
    vt_unimci_init();

# endif /* VT_UNIMCI */

  /* initialize enhanced time sync. if necessary */
# if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)

  if ( vt_env_etimesync() )
    vt_esync_init();

# endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

  /* register MPI routines */
  vt_mpi_register();

#endif /* VT_MPI || VT_HYB */

  /* register Pthread API routines */
#if ((defined(VT_MT) || defined(VT_HYB)) && defined(VT_PTHREAD))

  vt_pthread_register();

#endif /* (VT_MT || VT_HYB) && VT_PTHREAD */

#if !defined(VT_JAVA)

# if !(defined(VT_MPI) || defined(VT_HYB))
  atexit(vt_close);
# endif /* VT_MPI || VT_HYB */

  /* install signal handlers for process termination */
# ifdef SIGINT
  if (signal(SIGINT, vt_close_by_signal) == SIG_ERR)
    vt_warning("Could not install handler for signal SIGINT");
# endif
# ifdef SIGQUIT
  if (signal(SIGQUIT, vt_close_by_signal) == SIG_ERR)
    vt_warning("Could not install handler for signal SIGQUIT");
# endif
# ifdef SIGTERM
  if (signal(SIGTERM, vt_close_by_signal) == SIG_ERR)
    vt_warning("Could not install handler for signal SIGTERM");
# endif

#endif /* VT_JAVA */

  init_pid = getpid();

#if defined(VT_METR)
  {
    uint32_t gid;
    int i;

    /* return if no counters requested */
    if ( num_metrics > 0 )
    {
      /* write counter group name */
      gid = vt_def_counter_group(VT_METR);

      /* write counter definition records */
      for ( i = 0; i < num_metrics; i++ )
      {
	VTGen_write_DEF_COUNTER(VTTHRD_GEN(VTThrdv[0]),
				i+1,
				vt_metric_name(i),
				vt_metric_props(i),
				gid,
				vt_metric_unit(i));
      }
    }
  }
#endif /* VT_METR */

  vt_is_alive = 1;

#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_unlock(&init_mutex);
#endif /* VT_MT || VT_HYB */

  if( vt_env_debug() > 0 )
  {
    uint64_t t = vt_pform_wtime();
    vt_comment( &t, "NODEID: %lx", vt_pform_node_id() );
    vt_comment( &t, "NODEID_31: %x", vt_pform_node_id() & 0x7fffffff );
  }
}

void vt_reset()
{
#if (!defined(VT_MPI) && !defined(VT_MT) && !defined(VT_HYB) && !defined(VT_JAVA))

  int i;
  int extra_enters;
  uint64_t time;

  vt_is_alive = 0;

  /* notice current call stack level */
  extra_enters = VTTHRD_STACK_LEVEL(VTThrdv[0]) - 1;

#if defined(VT_MEMHOOK)

  /* finalize memory hooks if enabled */
  if (vt_env_memtrace())
    vt_memhook_finalize();

#endif /* VT_MEMHOOK */

#if defined(VT_GETCPU)

  /* finalize cpu id tracing if enabled */
  if ( vt_env_cpuidtrace() )
    vt_getcpu_finalize();

#endif /* VT_GETCPU */

#if defined(VT_IOWRAP)

  /* finalize I/O wrapper if enabled */
  if (vt_env_iotrace())
  {
    VT_DISABLE_IO_TRACING();
    vt_iowrap_finalize();
  }

#endif /* VT_IOWRAP */

#if defined(VT_LIBCWRAP)

  /* finalize LIBC wrapper if enabled */
  if (vt_env_libctrace())
  {
    VT_DISABLE_LIBC_TRACING();

#if defined(VT_FORK)
    vt_fork_finalize();
#endif /* VT_FORK */

    vt_libcwrap_finalize();
  }

#endif /* VT_LIBCWRAP */

#if defined(VT_LIBWRAP)

  /* finalize library wrapper */
  vt_libwrap_finalize();

#endif /* VT_LIBWRAP */

  /* finalize compiler adapter */
  if (vt_comp_finalize)
    vt_comp_finalize();

  /* destroy tread object */
  VTThrd_destroy(VTThrdv[0], 0);

  /* finalize thread object management */
  VTThrd_finalize();

  /* finalize hardware counters */
#if defined(VT_METR)

  if ( num_metrics > 0 )
    vt_metric_close();

#endif /* VT_METR */

  /* finalize UniMCI if necessary */
#if defined(VT_UNIMCI)

  if ( vt_env_mpicheck() )
    vt_unimci_finalize();

#endif /* VT_UNIMCI */

  /* finalize enhanced time sync. if necessary */
#if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)

  if ( vt_env_etimesync() )
    vt_esync_finalize();

#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

  /* re-initialize some variables */

  vt_open_called = 0;
  vt_close_called = 0;
  curid = 1;

  /* clear the hash tables */
  hash_clear();

  /* re-open VampirTrace */
  vt_open();

  /* repair call-stack */
  for ( i = 0; i < extra_enters; i++ )
  {
    time = vt_pform_wtime();
    vt_enter_user(&time);
  }

#endif /* !VT_MPI && !VT_MT && !VT_HYB && !VT_JAVA */
}

void vt_close_by_signal(int signum)
{
  vt_cntl_msg(2, "Received signal %i on pid %i", signum, getpid());

  /* restore original signal handler */
  signal(signum, SIG_DFL);

  /* trigger (at)exit handler */
  exit(signum);
}

void vt_close()
{
  int tnum;
  int i;

  /* catch vt_close called from child processes through atexit */
  if ( init_pid != getpid() ) return;

  /* do finalization only once */
#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_lock(&init_mutex);
#endif /* VT_MT || VT_HYB */
  if ( vt_close_called ) {
#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_unlock(&init_mutex);
#endif /* VT_MT || VT_HYB */
    return;
  }

  if( vt_env_debug() > 0 )
  {
    uint64_t t = vt_pform_wtime();
    vt_comment( &t, "NODEID: %lx", vt_pform_node_id() );
    vt_comment( &t, "NODEID_31: %x", vt_pform_node_id() & 0x7fffffff );
  }

  vt_close_called = 1;
  vt_is_alive = 0;

  tnum = (int)VTThrdn;

#if defined(VT_MEMHOOK)

  /* finalize memory hooks if enabled */
  if (vt_env_memtrace())
    vt_memhook_finalize();

#endif /* VT_MEMHOOK */

#if defined(VT_GETCPU)

  /* finalize cpu id tracing if enabled */
  if ( vt_env_cpuidtrace() )
    vt_getcpu_finalize();

#endif /* VT_GETCPU */

#if defined(VT_IOWRAP)

  /* finalize I/O wrapper if enabled */
  if (vt_env_iotrace())
  {
    VT_DISABLE_IO_TRACING();
    vt_iowrap_finalize();
  }

#endif /* VT_IOWRAP */

#if defined(VT_LIBCWRAP)

  /* finalize LIBC wrapper if enabled */
  if (vt_env_libctrace())
  {
    VT_DISABLE_LIBC_TRACING();
    vt_libcwrap_finalize();
  }

#endif /* VT_LIBCWRAP */

#if defined(VT_LIBWRAP)

  /* finalize library wrapper */
  vt_libwrap_finalize();

#endif /* VT_LIBWRAP */

  /* finalize compiler adapter */
  if (vt_comp_finalize)
    vt_comp_finalize();

  /* write stop-time as comment to definitions */
  {
    uint64_t stop_time_epoch;
    struct timeval tv0;
    gettimeofday(&tv0, NULL);

    stop_time_epoch = ((uint64_t)tv0.tv_sec * (uint64_t)1000000) +
       (uint64_t)tv0.tv_usec;
    vt_def_comment("__STOPTIME__ %llu", (unsigned long long)stop_time_epoch);
  }

  /* close trace files */
  for (i = 0; i < tnum; i++)
    VTThrd_close(VTThrdv[i]);

#if (defined(VT_LIBCWRAP) && defined(VT_FORK))

  if (vt_env_libctrace())
  {
    /* wait until all child processes are terminated */
    vt_fork_waitchilds();

    /* get total number of child processes */
    vt_num_traces = 1 + vt_fork_get_num_childs_tot();

    /* the master process removes the temp. trace-id file */
    if (vt_my_trace == 0)
    {
      char* trcid_filename = vt_fork_get_trcid_filename();
      remove(trcid_filename);
      free(trcid_filename);
    }

    vt_fork_finalize();
  }

#endif /* VT_LIBCWRAP && VT_FORK */

  /* write unify control file */
  vt_write_uctl_file();

  /* free temporary file names */
  for (i = 0; i < tnum; i++)
    VTThrd_delete(VTThrdv[i], i);

  /* finalize thread object management */
  VTThrd_finalize();

  /* finalize UniMCI if necessary */
#if defined(VT_UNIMCI)

  if ( vt_env_mpicheck() )
    vt_unimci_finalize();

#endif /* VT_UNIMCI */

  /* finalize enhanced time sync. if necessary */
#if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)

  if ( vt_env_etimesync() )
    vt_esync_finalize();

#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

  /* clear the hash tables */
  hash_clear();

  /* finalize hardware counters */
#if defined(VT_METR)

  if ( num_metrics > 0 )
    vt_metric_close();

#endif /* VT_METR */

  /* finalize resource usage counters */
#if defined(VT_RUSAGE)

  if ( num_rusage > 0 )
    vt_rusage_close();

#endif /* VT_RUSAGE */

  /*- Rank 0: unify trace files -*/
  if (vt_my_trace == 0 && vt_env_do_unify())
    {
      char* vtunify;
      char* filename;
      char* fprefix;
      char* cmd;
      int len;
      int nf;
      int8_t exec_stat;

      vtunify = vt_installdirs_expand("${bindir}/vtunify");
      if ( vtunify == NULL )
        vt_error();

      if ( access(vtunify, X_OK) == -1 )
        vt_error_msg("Cannot execute %s: %s", vtunify, strerror(errno));

      len = strlen(vt_env_gdir()) + strlen(vt_env_fprefix()) + 32;
      filename = (char*)calloc(len, sizeof(char));
      if ( filename == NULL )
        vt_error();

      fprefix = (char*)calloc(strlen(vt_env_fprefix()) + 16, sizeof(char));
      if ( fprefix == NULL )
        vt_error();

      if ( vt_my_funique > 0 )
        sprintf(fprefix, "%s_%u",  vt_env_fprefix(), vt_my_funique);
      else
        strcpy(fprefix, vt_env_fprefix());

      /*- wait for files to be ready -*/
      for (i = 0; i < vt_num_traces; i++)
        {
          sprintf(filename, "%s/%s.%x.uctl", vt_env_gdir(),
                  fprefix, i+1);

          vt_cntl_msg(2, "Checking for %s ...", filename);
          nf = 0;
          while (access(filename, R_OK) != 0 )
          {
            ++nf;
            /*- if file not ready in 15 sec give up -*/
            if ( nf > 15 ) return;
            sleep(1);
          }
        }

      /*- do actual merge -*/
      cmd = (char*)calloc(strlen(vtunify) + 16 + len, sizeof(char));
      if ( cmd == NULL )
        vt_error();
      sprintf(cmd, "%s %d %s/%s %s %s %s %s",
              vtunify, vt_num_traces,
              vt_env_gdir(), fprefix,
              vt_env_compression() ? "" : "-c",
              vt_env_do_clean() ? "" : "-k",
              (vt_env_verbose() == 0) ? "-q" : "",
              (vt_env_verbose() >= 2) ? "-v" : "");

      vt_cntl_msg(2, "Executing %s", cmd);
      exec_stat = system( cmd ); 
      if( exec_stat == 127 || exec_stat == -1 )
        vt_error_msg("Failed to execute %s (i): %s", cmd, exec_stat, strerror(errno));

      free(vtunify);
      free(filename);
      free(fprefix);
      free(cmd);
    }

#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_unlock(&init_mutex);
  VTThrd_deleteMutex(&init_mutex);
#endif /* VT_MT || VT_HYB */
}

void vt_trace_on(uint8_t mark)
{
  VT_CHECK_THREAD;

  if ( vt_is_alive &&
       VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_OFF )
  {
    /* switch tracing on, if current call stack level is equal to the
       stored one at switching trace off */
    if ( VTTHRD_STACK_LEVEL(VTThrdv[VT_MY_THREAD]) ==
	 VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[VT_MY_THREAD]) )
    {
      VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) = VT_TRACE_ON;

      if ( mark )
      {
	uint64_t time = vt_pform_wtime();
	vt_exit(&time);
      }

      vt_cntl_msg(2, "Tracing switched on");
    }
    /* otherwise: abort */
    else
    {
      vt_error_msg("Could not switch tracing on.\n"
		   "The current call stack level (%i) isn't equal to the "
		   "stored one (%i) at switching trace off.",
		   VTTHRD_STACK_LEVEL(VTThrdv[VT_MY_THREAD]),
		   VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[VT_MY_THREAD]) );
    }
  }
}

void vt_trace_off(uint8_t mark, uint8_t permanent)
{
  VT_CHECK_THREAD;

  if ( vt_is_alive &&
       VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_OFF_PERMANENT )
  {
    if ( mark )
    {
      uint64_t time;
      time = vt_pform_wtime();
      vt_enter(&time, vt_trc_regid[VT__TRC_OFF]);
    }

    if ( permanent )
    {
      VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) = VT_TRACE_OFF_PERMANENT;

      vt_cntl_msg(1, "Tracing switched off permanently");
    }
    else if ( VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON )
    {
      /* store current call stack level */
      VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) = VT_TRACE_OFF;
      VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[VT_MY_THREAD]) =
      VTTHRD_STACK_LEVEL(VTThrdv[VT_MY_THREAD]);

      vt_cntl_msg(2, "Tracing switched off at call stack level (%i)",
      VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[VT_MY_THREAD]));
    }
  }
}

uint8_t vt_is_trace_on()
{
  VT_CHECK_THREAD;

  if ( !vt_is_alive ) return 0;

  return (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) ? 1 : 0;
}

void vt_buffer_flush()
{
  VT_CHECK_THREAD;

  if ( !vt_is_alive ) return;

  VTGen_flush(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), 0, vt_pform_wtime(), NULL);
}

void vt_update_counter()
{
  uint64_t time;

  VT_CHECK_THREAD; 

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  time = vt_pform_wtime();

  /* update counters from VT_METRICS */
#if defined(VT_METR)
  if ( num_metrics > 0 )
  {
    int i;

    vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                   VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                   VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

    for ( i = 0; i < num_metrics; i++ )
    {
      if ( VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) break;
        VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
                            &time,
                            i+1,
                            VTTHRD_VALV(VTThrdv[VT_MY_THREAD])[i]);
    }
  }
#endif /* VT_METR */

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(&time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(&time);
}

void vt_mpi_init()
{
#if (defined(VT_MPI) || defined(VT_HYB))

  VT_MPI_INT myrank, size;

  PMPI_Comm_rank(MPI_COMM_WORLD, &myrank);
  PMPI_Comm_size(MPI_COMM_WORLD, &size);
  vt_my_trace = (int)myrank;
  vt_num_traces = (int)size;

  vt_error_pid(vt_my_trace);

  /* C++ on CrayXT (sometimes?) results in forked child processes doing
     MPI_Init */
  if (init_pid != getpid())
    init_pid = getpid();

#if !defined(VT_DISABLE_RFG)

  {
    char* filter_deffile = vt_env_filter_spec();
    if (filter_deffile)
    {
      if (!RFG_Regions_readFilterDefFile( VTThrdv[0]->rfg_regions,
                                          (int)myrank))
        vt_error_msg("Could not read region filter specification file");
    }
  }

#endif /* VT_DISABLE_RFG */

  /* register remaining MPI routines */
  vt_mpi_register_remain();

  /* write VT related definition comments */
  if (vt_my_trace == 0)
    vt_write_def_header();

  /* read environment variable "VT_FILE_UNIQUE" */
  vt_my_funique = vt_env_funique();

  if (vt_my_funique == -1)     /* no file-uniqueness desired ... */
  {
    vt_my_funique = 0;
  }
  else if (vt_my_funique == 0) /* rank 0 generates a unique file id
				  and notify all ranks about this ... */
  {
    if (vt_my_trace == 0)
      vt_my_funique = (int)vt_get_unique_file_id();
    if (vt_num_traces > 1)
      PMPI_Bcast(&vt_my_funique, 1, MPI_INT, 0, MPI_COMM_WORLD);
  }

  /* first clock synchronization if necessary */
#if TIMER_IS_GLOBAL == 0
  if (vt_num_traces > 1)
  {
#ifdef VT_ETIMESYNC
    if (vt_env_etimesync())
      vt_esync(MPI_COMM_WORLD);
    else
#endif /* VT_ETIMESYNC */
      vt_sync(MPI_COMM_WORLD, &my_ltime[0], &my_offset[0]);
  }
#endif /* TIMER_IS_GLOBAL */

  atexit(vt_close);

#endif /* VT_MPI || VT_HYB */
}

void vt_mpi_finalize()
{
#if (defined(VT_MPI) || defined(VT_HYB))

  /* last clock synchronization if necessary */
#if TIMER_IS_GLOBAL == 0
  if (vt_num_traces > 1)
  {
#ifdef VT_ETIMESYNC
    if (vt_env_etimesync())
      vt_esync(MPI_COMM_WORLD);
    else
#endif /* VT_ETIMESYNC */
      vt_sync(MPI_COMM_WORLD, &my_ltime[1], &my_offset[1]);
  }
#endif /* TIMER_IS_GLOBAL */

  PMPI_Barrier(MPI_COMM_WORLD);
#endif /* VT_MPI || VT_HYB */
}

static void vt_mpi_sync(uint64_t* time, void* comm)
{
#if (defined(VT_MPI) || defined(VT_HYB))
  static const int sync_flush_flag = 1<<0;
  static const int sync_time_flag  = 1<<1;
  static int sync_flush_env = -1;
  static int sync_flush_level_env = -1;
  static int sync_time_env = -1;
  VT_MPI_INT lsync_mask = 0;
  VT_MPI_INT sync_mask = 0;

  VT_CHECK_THREAD;

  /* get environment variables, if first call */

  if (sync_flush_env == -1)
    sync_flush_env = vt_env_sync_flush();
  if (sync_flush_level_env == -1)
    sync_flush_level_env = vt_env_sync_flush_level();
#if (defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  if (sync_time_env == -1)
    sync_time_env = vt_env_etimesync();
#else /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */
  sync_time_env = 0;
#endif /* VT_ETIMESYNC && !TIME_IS_GLOBAL */

  /* return, if neither sync. buffer flush nor enhanced
     time sync. enabled */
  if (!sync_flush_env && !sync_time_env) return;

  /* return, if MPI communicator isn't MPI_COMM_WORLD or a
     copy of it */
  if (*((MPI_Comm*)comm) != MPI_COMM_WORLD)
  {
    VT_MPI_INT comm_size;
    PMPI_Comm_size(*((MPI_Comm*)comm), &comm_size);
    if ((int)comm_size != vt_num_traces) return;
  }

  /* mark begin of synchronization */
  vt_enter(time, vt_trc_regid[VT__TRC_SYNC]);

  /* checking whether buffer flush needed */

  if (sync_flush_env)
  {
    /* set bit for flushing buffer, if fill level >= sync_flush_level_env */
    if ((int)VTGen_get_buflevel(VTTHRD_GEN(VTThrdv[VT_MY_THREAD])) >=
	sync_flush_level_env)
    {
      lsync_mask |= sync_flush_flag;
      /* also set bit for time sync. */
      if (sync_time_env) lsync_mask |= sync_time_flag;
    }
  }

  /* checking whether time sync. needed */

#if (defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  if (sync_time_env && (sync_mask & sync_time_flag) == 0)
  {
    /* set bit for time sync. if necessary */
    if (vt_esync_next() <= *time)
      lsync_mask |= sync_time_flag;
  }
#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

  PMPI_Allreduce(&lsync_mask, &sync_mask, 1,
		 MPI_INT, MPI_BOR, *((MPI_Comm*)comm));

  /* flush buffer, if necessary */
  if ((sync_mask & sync_flush_flag) != 0)
    VTGen_flush(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), 0, vt_pform_wtime(), NULL);
#if (defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  /* sync. time, if necessary */
  if ((sync_mask & sync_time_flag) != 0)
    vt_esync(*((MPI_Comm*)comm));
#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

  /* barrier at exit, if only sync. buffer flush performed */
  if ((sync_mask & sync_time_flag) == 0 && (sync_mask & sync_flush_flag) != 0)
    PMPI_Barrier(*((MPI_Comm*)comm));

  /* mark end of synchronization */
  *time = vt_pform_wtime();
  vt_exit(time);
#endif /* VT_MPI || VT_HYB */
}

/*
 *-----------------------------------------------------------------------------
 * Defining source code entities
 *-----------------------------------------------------------------------------
 */

static uint32_t vt_def_scl(uint32_t fid, uint32_t begln, uint32_t endln)
{
  uint32_t sid;

  VT_CHECK_THREAD;

  if( fid == VT_NO_ID || begln == VT_NO_LNO )
    return 0;

  sid = curid++;

  VTGen_write_DEF_SCL(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), sid, fid, begln);
  return sid;
}

static uint32_t vt_def_region_desc(const char* rdesc)
{
  uint32_t rdid;

  HashNode_rdesc* hn;

  VT_CHECK_THREAD;

  hn = hash_get(HASH_TAB__RDESC, rdesc);

  if(hn == NULL)
  {
    rdid = curid++;

    VTGen_write_DEF_FUNCTION_GROUP(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
				    rdid, rdesc);

    hash_put(HASH_TAB__RDESC, rdesc, rdid);
  }
  else
  {
    rdid = hn->rdid;
  }

  return rdid;
}

static uint32_t vt_get_unique_file_id()
{
  int new_fuid;

  /* read environment variable "VT_FILE_UNIQUE" */
  new_fuid = vt_env_funique();

  if( new_fuid == -1 )     /* no file-uniqueness desired ... */
  {
    new_fuid = 0;
  }
  else if( new_fuid == 0 ) /* generate unique file id ... */
  {
    int  fd;
    int8_t tmp_len;
    struct flock fl;
    char lock_filename[300];
    char tmp[16] = "";
    uint8_t do_unlock = 1;

    VT_SUSPEND_IO_TRACING();

    /* create filename for unique id file */
    snprintf(lock_filename, sizeof(lock_filename)-1, "%s/%s.lock", vt_env_gdir(), vt_env_fprefix());

    /* open/create unique id file */
    if( (fd = open(lock_filename, (O_RDWR | O_CREAT),
		   (S_IRUSR | S_IWUSR))) == -1 )
      vt_error_msg("Cannot open file %s: %s", lock_filename, strerror(errno));

    /* lock unique id file */
    fl.l_type = F_WRLCK; fl.l_whence = SEEK_SET; fl.l_start = 0; fl.l_len = 0;
    if( fcntl(fd, F_SETLKW, &fl) == -1 )
    {
      do_unlock = 0;
      vt_warning("Cannot lock file %s: %s", lock_filename, strerror(errno));
    }

    /* read current unique id */
    if( read(fd, tmp, 15) == -1 )
      vt_error_msg("Cannot read file %s: %s", lock_filename, strerror(errno));

    /* terminate buffer to avoid issues in upcoming functions */
    tmp[15] = '\0';

    if( tmp[0] == '\0' )
      new_fuid = 0;             /* set unique id to 0, if file is empty */
    else
      new_fuid = atoi(tmp) + 1; /* increment unique id */

    /* write new unique id */
    lseek(fd, 0, SEEK_SET);
    snprintf(tmp, sizeof(tmp)-1, "%u\n", new_fuid);
    tmp_len = strlen( tmp );
    if( tmp_len > write(fd, tmp, tmp_len) )
      vt_error_msg("Failed to write to file %s: %s", lock_filename, strerror(errno));

    /* unlock unique id file */
    if( do_unlock )
    {
      fl.l_type = F_UNLCK;
      if (fcntl(fd, F_SETLK, &fl) == -1)
	vt_error_msg("Cannot unlock file %s: %s",
		     lock_filename, strerror(errno));
    }

    /* close unique id file */
    close(fd);

    VT_RESUME_IO_TRACING();
  }

  return new_fuid;
}

static void vt_write_def_header()
{
  int32_t  tmp_int32;
  uint64_t tmp_uint64;
  char     tmp_char[128];

  vt_def_comment("__VT_COMMENT__ VampirTrace Environment:");

  /* VT_MODE */
  tmp_int32 = vt_env_mode();

  tmp_char[0] = '\0';
  if( (tmp_int32 & VT_MODE_TRACE) != 0 )
  {
    strncpy(tmp_char, "TRACE", sizeof(tmp_char)-1);
    tmp_char[sizeof(tmp_char)-1] = '\0';
  }

  if( (tmp_int32 & VT_MODE_STAT) != 0 )
  {
    if( strlen(tmp_char) > 0 )
      strncat(tmp_char, ":", sizeof(tmp_char)-1-strlen(tmp_char));

    strncat(tmp_char, "STAT", sizeof(tmp_char)-1-strlen(tmp_char));
  }

  vt_def_comment("__VT_COMMENT__  VT_MODE: %s", tmp_char);

  /* VT_BUFFER_SIZE */
  tmp_uint64 = (uint64_t)vt_env_bsize();

  if( tmp_uint64 >= (1024*1024*1024) )
  {
    tmp_uint64 /= (1024*1024*1024);
    snprintf(tmp_char, sizeof(tmp_char)-1, "%lluG",
             (unsigned long long)tmp_uint64);
  }
  else if( tmp_uint64 >= (1024*1024) )
  {
    tmp_uint64 /= (1024*1024);
    snprintf(tmp_char, sizeof(tmp_char)-1, "%lluM",
             (unsigned long long)tmp_uint64);
  }
  else
  {
    snprintf(tmp_char, sizeof(tmp_char)-1, "%llu",
             (unsigned long long)tmp_uint64);
  }

  vt_def_comment("__VT_COMMENT__  VT_BUFFER_SIZE: %s", tmp_char);

  /* VT_SYNC_FLUSH */
  vt_def_comment("__VT_COMMENT__  VT_SYNC_FLUSH: %s",
                 vt_env_sync_flush() ? "yes" : "no");

  /* VT_SYNC_FLUSH_LEVEL */
  vt_def_comment("__VT_COMMENT__  VT_SYNC_FLUSH_LEVEL: %i",
                 vt_env_sync_flush_level());

  /* VT_MAX_FLUSHES */
  vt_def_comment("__VT_COMMENT__  VT_MAX_FLUSHES: %i", vt_env_max_flushes());

#if defined(VT_METR)
  /* VT_METRICS */
  vt_def_comment("__VT_COMMENT__  VT_METRICS: %s",
                 vt_env_metrics() ? vt_env_metrics() : "<not set>");
#endif /* VT_METR */

#if defined(VT_RUSAGE)
  /* VT_RUSAGE */
  vt_def_comment("__VT_COMMENT__  VT_RUSAGE: %s",
                 vt_env_rusage() ? vt_env_rusage() : "<not set>");

  /* VT_RUSAGE_INTV */
  vt_def_comment("__VT_COMMENT__  VT_RUSAGE_INTV: %i", vt_env_rusage_intv());
#endif /* VT_RUSAGE */

#if (defined(VT_MPI) || defined(VT_HYB))
  /* VT_MPITRACE */
  vt_def_comment("__VT_COMMENT__  VT_MPITRACE: %s",
                 vt_env_mpitrace() ? "yes" : "no");
#endif /* VT_MPI || VT_HYB */

#if defined(VT_UNIMCI)
  /* VT_MPICHECK */
  vt_def_comment("__VT_COMMENT__  VT_MPICHECK: %s",
                 vt_env_mpicheck() ? "yes" : "no");

  /* VT_MPICHECK_ERREXIT */
  vt_def_comment("__VT_COMMENT__  VT_MPICHECK_ERREXIT: %s",
                 vt_env_mpicheck_errexit() ? "yes" : "no");
#endif /* VT_UNIMCI */

#if defined(VT_MEMHOOK)
  /* VT_MEMTRACE */
  vt_def_comment("__VT_COMMENT__  VT_MEMTRACE: %s",
                 vt_env_memtrace() ? "yes" : "no");

  vt_def_comment("__VT_COMMENT__  VT_MEMTRACE_MARKER: %s",
                 vt_env_memtrace_marker() ? "yes" : "no");
#endif /* VT_MEMHOOK */

#if defined(VT_GETCPU)
  /* VT_CPUIDTRACE */
  vt_def_comment("__VT_COMMENT__  VT_CPUIDTRACE: %s",
                 vt_env_cpuidtrace() ? "yes" : "no");
#endif /* VT_GETCPU */

#if defined(VT_IOWRAP)
  /* VT_IOTRACE */
  vt_def_comment("__VT_COMMENT__  VT_IOTRACE: %s",
                 vt_env_iotrace() ? "yes" : "no");
#endif /* VT_IOWRAP */

#if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  /* VT_ETIMESYNC */
  vt_def_comment("__VT_COMMENT__  VT_ETIMESYNC: %s",
                 vt_env_etimesync() ? "yes" : "no");

  /* VT_ETIMESYNC_INTV */
  vt_def_comment("__VT_COMMENT__  VT_ETIMESYNC_INTV: %i",
                 vt_env_etimesync_intv());
#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

#if defined(VT_THRD_PTHREAD)
  /* VT_PTHREAD_REUSE */
  vt_def_comment("__VT_COMMENT__  VT_PTHREAD_REUSE: %s",
                 vt_env_pthread_reuse() ? "yes" : "no");
#endif /* VT_THRD_PTHREAD */

  /* VT_FILER_SPEC */
  vt_def_comment("__VT_COMMENT__  VT_FILTER_SPEC: %s",
                 vt_env_filter_spec() ? vt_env_filter_spec() : "<not set>");

  /* VT_GROUPS_SPEC */
  vt_def_comment("__VT_COMMENT__  VT_GROUPS_SPEC: %s",
                 vt_env_groups_spec() ? vt_env_groups_spec() : "<not set>");
}

static void vt_write_uctl_file()
{
  int   i;
  char  filename[300];
  FILE* uctl_file;

  if (vt_my_funique > 0)
    snprintf(filename, sizeof(filename) - 1, "%s/%s_%u.%x.uctl",
	     vt_env_gdir(), vt_env_fprefix(), vt_my_funique, vt_my_trace+1);
  else
    snprintf(filename, sizeof(filename) - 1, "%s/%s.%x.uctl",
	     vt_env_gdir(), vt_env_fprefix(), vt_my_trace+1);

  uctl_file = fopen(filename, "w");

  if (uctl_file == NULL)
    vt_error_msg("Cannot open file %s", filename);

  /* write stream ids */

  for(i=0; i<(int)VTThrdn; i++)
  {
    fprintf(uctl_file, "%s%u",
	    i>0 ? ":" : "", 65536*i+(vt_my_trace+1));
  }
  fprintf(uctl_file, ":\n");

  /* write time offset */
  fprintf(uctl_file, "%lli:%lli:%lli:%lli:\n",
	  (long long int)my_ltime[0],
	  (long long int)my_offset[0],
	  (long long int)my_ltime[1],
	  (long long int)my_offset[1]);

#if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  if ( vt_env_etimesync() )
    vt_esync_app_uctl_file(uctl_file);
#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

  fclose(uctl_file);

  vt_cntl_msg(2, "Wrote unify control file %s", filename);
}


void vt_def_comment(const char* fmt, ...)
{
  char comment[VT_MAX_COMMENT_LEN];
  va_list ap;

  VT_CHECK_THREAD;

  va_start(ap, fmt);

  vsnprintf(comment, VT_MAX_COMMENT_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_DEFINITION_COMMENT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), comment);
}

uint32_t vt_def_scl_file(const char* fname)
{
  uint32_t fid;

  HashNode_sfile* hn;

  VT_CHECK_THREAD;

  hn = hash_get(HASH_TAB__SFILE, fname);

  if( hn == NULL )
  {
    fid = curid++;

    VTGen_write_DEF_SCL_FILE(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), fid, fname);

    hash_put(HASH_TAB__SFILE, fname, fid);
  }
  else
  {
    fid = hn->fid;
  }

  return fid;
}

uint32_t vt_def_file_group(const char* gname)
{
  uint32_t gid;

  VT_CHECK_THREAD;

  gid = curid++;

  VTGen_write_DEF_FILE_GROUP(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), gid, gname);

  return gid;
}

uint32_t vt_def_file(const char* fname, uint32_t gid)
{
  uint32_t fid;

  VT_CHECK_THREAD;

  fid = curid++;

  VTGen_write_DEF_FILE(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
		       fid,
		       fname,
		       gid);

  return fid;
}

uint32_t vt_def_region(const char* rname, uint32_t fid, uint32_t begln, 
                       uint32_t endln, const char* rdesc, uint8_t rtype)
{
#if !defined(VT_DISABLE_RFG)
  RFG_RegionInfo* rinf;
#endif
  uint32_t sid;
  uint32_t rid;
  uint32_t rdid;

  VT_CHECK_THREAD;

  sid = vt_def_scl(fid, begln, endln);
  rid = curid++;

#if !defined(VT_DISABLE_RFG)
  /* get region's filter/group information */
  rinf = RFG_Regions_add(VTTHRD_RFGREGIONS(VTThrdv[0]), rname, rid);
  vt_assert(rinf != NULL);

  /* get region's group name, if specified by VT_GROUPS_SPEC */
  if ( rinf->groupName != NULL )
    rdesc = rinf->groupName;
#endif /* VT_DISABLE_RFG */

  /* get region's default group name, if necessary */
  if ( rdesc == NULL )
  {
    switch ( rtype )
    {
      case VT_INTERNAL:
        rdesc = "VT_API";
        break;
      case VT_LIBC:
        rdesc = "LIBC";
        break;
      case VT_LIBC_IO:
        rdesc = "LIBC-I/O";
        break;
      case VT_MEMORY:
        rdesc = "MEM";
        break;
      case VT_MPI_FUNCTION:
      case VT_MPI_COLL_ALL2ALL:
      case VT_MPI_COLL_ALL2ONE:
      case VT_MPI_COLL_BARRIER:
      case VT_MPI_COLL_ONE2ALL:
      case VT_MPI_COLL_OTHER:
        rdesc = "MPI";
        break;
      case VT_OMP_FUNCTION:
      case VT_OMP_ATOMIC:
      case VT_OMP_CRITICAL:
      case VT_OMP_CRITICAL_SBLOCK:
      case VT_OMP_FLUSH:
      case VT_OMP_MASTER:
      case VT_OMP_PARALLEL:
      case VT_OMP_SECTION:
      case VT_OMP_SECTIONS:
      case VT_OMP_SINGLE:
      case VT_OMP_SINGLE_SBLOCK:
      case VT_OMP_WORKSHARE:
        rdesc = "OMP";
        break;
      case VT_OMP_PARALLEL_REGION:
        rdesc = "OMP-PREG";
        break;
      case VT_OMP_BARRIER:
      case VT_OMP_IBARRIER:
        rdesc = "OMP-SYNC";
        break;
      case VT_OMP_LOOP:
        rdesc = "OMP-LOOP";
        break;
      case VT_PTHRD_FUNCTION:
        rdesc = "PTHREAD";
        break;
      case VT_LOOP:
        rdesc = "LOOP";
        break;
      default: /* e.g. VT_FUNCTION */
        rdesc = VT_DEF_GROUP;
        break;
    }

#if !defined(VT_DISABLE_RFG)
    /* add assignment for default group */
    RFG_Regions_addGroupAssign(VTTHRD_RFGREGIONS(VTThrdv[0]),
                               rdesc, 1, rname);
#endif
  }

  /* define group and store identifier */
  rdid = vt_def_region_desc(rdesc);

#if (defined(VT_MPI) || defined(VT_HYB))
  /* define MPI collective operation, if necessary */
  if ( rtype == VT_MPI_COLL_ALL2ALL ||
       rtype == VT_MPI_COLL_ALL2ONE ||
       rtype == VT_MPI_COLL_BARRIER ||
       rtype == VT_MPI_COLL_ONE2ALL ||
       rtype == VT_MPI_COLL_OTHER )
  {
    VTGen_write_DEF_COLLECTIVE_OPERATION(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
                                         rid,   /* collective id equal region id */
                                         rname, /* collective name equal region name */
      (rtype == VT_MPI_COLL_OTHER) ? OTF_COLLECTIVE_TYPE_UNKNOWN :
      (rtype == VT_MPI_COLL_BARRIER) ? OTF_COLLECTIVE_TYPE_BARRIER :
      (rtype == VT_MPI_COLL_ONE2ALL) ? OTF_COLLECTIVE_TYPE_ONE2ALL :
      (rtype == VT_MPI_COLL_ALL2ONE) ? OTF_COLLECTIVE_TYPE_ALL2ONE :
      (rtype == VT_MPI_COLL_ALL2ALL) ? OTF_COLLECTIVE_TYPE_ALL2ALL :
        OTF_COLLECTIVE_TYPE_UNKNOWN);
  }
#endif /* VT_MPI || VT_HYB */

  /* define region and return identifier */
  VTGen_write_DEF_FUNCTION(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
                           rid,
                           rname,
                           rdid,
                           sid);

  return rid;
}

uint32_t vt_def_counter_group(const char* gname)
{
  uint32_t gid;

  VT_CHECK_THREAD;

  gid = curid++;

  VTGen_write_DEF_COUNTER_GROUP(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]), gid, gname);

  return gid;
}

uint32_t vt_def_counter(const char* cname,
			uint32_t cprop,
			uint32_t gid,
			const char* cunit)
{
  uint32_t cid = 0;

  VT_CHECK_THREAD;

#if defined(VT_METR)
  cid = num_metrics;
#endif /* VT_METR */

  cid += curid++;

  VTGen_write_DEF_COUNTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			  cid,
			  cname,
			  cprop,
			  gid,
			  cunit);

  return cid;
}

uint32_t vt_def_marker(const char* mname,
		       uint32_t mtype)
{
  uint32_t mid;

  VT_CHECK_THREAD;

  mid = curid++;

  VTGen_write_DEF_MARKER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			 mid,
			 mname,
			 mtype);

  return mid;
}

void vt_def_mpi_comm(uint32_t cid,
		     uint32_t grpc,
		     uint8_t grpv[])
{
  int i;

  uint32_t  cgrpc;
  uint32_t* cgrpv;
  char      cname[20];

  VT_CHECK_THREAD;

  cgrpv = (uint32_t*)calloc(grpc * 8, sizeof(uint32_t));
  if( cgrpv == NULL )
    vt_error();

  for(cgrpc = 0, i = 0; i < (int)grpc; i++)
  {
    if(grpv[i] & 0x1)  cgrpv[cgrpc++] = (i * 8) + 1;
    if(grpv[i] & 0x2)  cgrpv[cgrpc++] = (i * 8) + 2;
    if(grpv[i] & 0x4)  cgrpv[cgrpc++] = (i * 8) + 3;
    if(grpv[i] & 0x8)  cgrpv[cgrpc++] = (i * 8) + 4;
    if(grpv[i] & 0x10) cgrpv[cgrpc++] = (i * 8) + 5;
    if(grpv[i] & 0x20) cgrpv[cgrpc++] = (i * 8) + 6;
    if(grpv[i] & 0x40) cgrpv[cgrpc++] = (i * 8) + 7;
    if(grpv[i] & 0x80) cgrpv[cgrpc++] = (i * 8) + 8;
  }

  if(cid == 0)
    strncpy(cname, "__MPI_COMM_WORLD__", sizeof(cname) - 1);
  else if(cid == 1)
    strncpy(cname, "__MPI_COMM_SELF__", sizeof(cname) - 1);
  else
    strncpy(cname, "__MPI_COMM_USER__", sizeof(cname) - 1);

  VTGen_write_DEF_PROCESS_GROUP(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
				cid+1, cname, cgrpc, cgrpv);

  free(cgrpv);
}

/*
 *-----------------------------------------------------------------------------
 * Recording events
 *-----------------------------------------------------------------------------
 */

/* -- Region -- */

uint8_t vt_enter(uint64_t* time, uint32_t rid) {
#if !defined(VT_DISABLE_RFG)
  RFG_RegionInfo* rinf;
#endif
  uint8_t do_trace;

  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) ==
      VT_TRACE_OFF_PERMANENT) return 0;

  VTTHRD_STACK_PUSH(VTThrdv[VT_MY_THREAD]);

  do_trace = ((VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) &&
	      (VTTHRD_STACK_LEVEL(VTThrdv[VT_MY_THREAD]) <= max_stack_depth));

#if !defined(VT_DISABLE_RFG)
  if( !RFG_Regions_stackPush(VTTHRD_RFGREGIONS(VTThrdv[VT_MY_THREAD]),
                             rid, do_trace, &rinf) )
  {
#   if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    RFG_RegionInfo* rinf_master;
    VTTHRD_LOCK_IDS();
    rinf_master = RFG_Regions_get(VTTHRD_RFGREGIONS(VTThrdv[0]), rid);
    VTTHRD_UNLOCK_IDS();
    vt_assert(rinf_master != NULL);

    rinf = RFG_Regions_add(VTTHRD_RFGREGIONS(VTThrdv[VT_MY_THREAD]),
                           rinf_master->regionName, rid);

    /* copy master's call limit */
    rinf->callLimit = rinf_master->callLimit;

    /* initialize call limit count down */
    rinf->callLimitCD = rinf->callLimit;

    if (!RFG_Regions_stackPush(VTTHRD_RFGREGIONS(VTThrdv[VT_MY_THREAD]),
                               rid, do_trace, &rinf))
      vt_assert(0);
#   else /* VT_MT || VT_HYB || VT_JAVA */
    vt_assert(0);
#   endif /* VT_MT || VT_HYB || VT_JAVA */
  }

  if (do_trace)
  {
    /* write marker, if the next enter will reach the call limit */
    if (rinf->callLimitCD == 1)
    {
      char marktext[1024];
      snprintf(marktext, sizeof(marktext) - 1,
               "Beginning to filter out function '%s' "
               "(call limit (=%i) reached at this point)",
               rinf->regionName,
               rinf->callLimit-1);
      vt_marker(time, markid, marktext);
    }
    /* don't record this enter, if call limit reached */
    else if (rinf->callLimitCD == 0)
    {
      do_trace = 0;
    }
  }
#endif /* VT_DISABLE_RFG */

  if (do_trace)
  {
#   if defined(VT_METR)
      if ( num_metrics > 0 )
      {
        vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                       VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                       VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

	VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			  time,
			  rid,
			  0,
			  num_metrics,
			  VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));
      }
      else
      {
	VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			  time,
			  rid,
			  0,
			  0, NULL);
      }
#   else /* VT_METR */
      VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			time,
			rid,
			0,
			0, NULL);
#   endif /* VT_METR */

    /* update cpu id counter (VT_GETCPU) */
    UPDATE_CPUID(time);
    /* update resource usage counters (VT_RUSAGE) */
    UPDATE_RUSAGE(time);
  }

  return do_trace;
}

void vt_exit(uint64_t* time) {
#if !defined(VT_DISABLE_RFG)
  RFG_RegionInfo* rinf;
  int climitbyenter;
#endif
  uint8_t do_trace;

  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) ==
      VT_TRACE_OFF_PERMANENT) return;

  VTTHRD_STACK_POP(VTThrdv[VT_MY_THREAD]);

  do_trace = ((VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) &&
	      (VTTHRD_STACK_LEVEL(VTThrdv[VT_MY_THREAD])+1 <= max_stack_depth));

#if !defined(VT_DISABLE_RFG)
  if (!RFG_Regions_stackPop(VTTHRD_RFGREGIONS(VTThrdv[VT_MY_THREAD]),
                            &rinf, &climitbyenter))
  {
    vt_assert(0);
  }

  if (climitbyenter == 0)
    do_trace = 0;
#endif /* VT_DISABLE_RFG */

  if (do_trace)
  {
    /* update cpu id counter (VT_GETCPU) */
    UPDATE_CPUID(time);
    /* update resource usage counters (VT_RUSAGE) */
    UPDATE_RUSAGE(time);

#   if defined(VT_METR)
      if ( num_metrics > 0 )
      {
        vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                       VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                       VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

	VTGen_write_LEAVE(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			  time,
			  0,
			  0,
			  num_metrics,
			  VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));
      }
      else
      {
	VTGen_write_LEAVE(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			  time,
			  0,
			  0,
			  0, NULL);
      }
#   else /* VT_METR */
      VTGen_write_LEAVE(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			time,
			0,
			0,
			0, NULL);
#   endif /* VT_METR */
  }
}

/* -- File I/O -- */

void vt_ioexit(uint64_t* time, uint64_t* etime, uint32_t fid, uint64_t hid, uint32_t op, uint64_t bytes) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) {
    VTGen_write_FILE_OPERATION(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
                               time,
                               etime,
                               fid,
                               hid,
                               op,
                               bytes,
                               0);
  }

  vt_exit(etime);
}

void vt_iobegin(uint64_t* time, uint64_t hid) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) {
    VTGen_write_BEGIN_FILE_OPERATION(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
				     time,
				     hid,
				     0);
  }
}

void vt_ioend(uint64_t* time, uint32_t fid, uint64_t hid, uint32_t op, uint64_t bytes) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) {
    VTGen_write_END_FILE_OPERATION(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
				   time,
				   fid,
				   hid,
				   op,
				   bytes,
				   0);
  }
}

/* -- Counter -- */

void vt_count(uint64_t* time, uint32_t cid, uint64_t cval) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
		      time,
		      cid,
		      cval);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

/* -- Comment -- */

void vt_comment(uint64_t* time, const char* fmt, ...) {
  char comment[VT_MAX_COMMENT_LEN];
  va_list ap;

  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  va_start(ap, fmt);

  vsnprintf(comment, VT_MAX_COMMENT_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_COMMENT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
                      time,
                      comment);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

/* -- Marker -- */

void vt_marker(uint64_t* time, uint32_t mid, const char* fmt, ...) {
  char mtext[VT_MAX_MARKER_LEN];
  va_list ap;

  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  va_start(ap, fmt);

  vsnprintf(mtext, VT_MAX_MARKER_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_MARKER(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
                     time,
                     mid,
                     mtext);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

/* -- MPI-1 -- */

void vt_mpi_send(uint64_t* time, uint32_t dpid, uint32_t cid, uint32_t tag, uint32_t sent) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_SEND_MSG(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
		       time,
		       dpid+1,
		       cid+1,
		       tag,
		       sent,
		       0);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

void vt_mpi_recv(uint64_t* time, uint32_t spid, uint32_t cid, uint32_t tag, uint32_t recvd) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_RECV_MSG(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
		       time,
		       spid+1,
		       cid+1,
		       tag,
		       recvd,
		       0);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

void vt_mpi_collexit(uint64_t* time, uint64_t* etime, uint32_t rid, uint32_t rpid, uint32_t cid,
		     void* comm, uint32_t sent, uint32_t recvd) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) == VT_TRACE_ON) {
    VTGen_write_COLLECTIVE_OPERATION(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
  				     time,
  				     etime,
  				     rid,
  				     cid+1,
  				     rpid != VT_NO_ID ? rpid+1 : 0,
  				     sent,
  				     recvd,
  				     0);
  }

  vt_exit(etime);

  /* intermediate time sync. or buffer flush, if necessary */
  if (vt_num_traces > 1)
    vt_mpi_sync(etime, comm);
}

/* -- MPI2 - 1sided -- */

void vt_mpi_rma_put(uint64_t* time, uint32_t tpid, uint32_t cid, uint32_t tag, 
        uint64_t sent)
{ 
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_RMA_PUT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
            time,
            65536 * VT_MY_THREAD + vt_my_trace + 1,
            tpid+1,
            cid+1,
            tag,
            sent,
            0);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

void vt_mpi_rma_putre(uint64_t* time, uint32_t tpid, uint32_t cid, uint32_t tag, 
        uint64_t sent)
{ 
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_RMA_PUTRE(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
            time,
            0,
            tpid+1,
            cid+1,
            tag,
            sent,
            0);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

void vt_mpi_rma_get(uint64_t* time, uint32_t tpid, uint32_t cid, uint32_t tag, 
        uint64_t recvd)
{
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_RMA_GET(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
            time,
            0,
            tpid+1,
            cid+1,
            tag,
            recvd,
            0);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

void vt_mpi_rma_end(uint64_t* time, uint32_t cid, uint32_t tag)
{
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  VTGen_write_RMA_END(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
            time,
            0,
            cid+1,
            tag,
            0);

  /* update cpu id counter (VT_GETCPU) */
  UPDATE_CPUID(time);
  /* update resource usage counters (VT_RUSAGE) */
  UPDATE_RUSAGE(time);
}

/* -- OpenMP -- */

void vt_omp_fork(uint64_t* time)
{
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  vt_enter(time, vt_trc_regid[VT__TRC_OMPPREG]);
}

void vt_omp_fork2(uint64_t* time, uint32_t* ptid)
{
  VT_CHECK_THREAD;

  *ptid = VT_MY_THREAD;

  vt_omp_fork(time);
}

void vt_omp_join(uint64_t* time)
{
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  vt_exit(time);
}

void vt_omp_parallel_begin()
{
  VT_CHECK_THREAD;
#if defined(VT_METR)
  if ( VT_MY_THREAD > 0 &&
       vt_metric_num() > 0 && !VTTHRD_METV(VTThrdv[VT_MY_THREAD]) ) {
    /* create metrics in worker threads */
    VTThrdv[VT_MY_THREAD]->metv = vt_metric_create();
    vt_cntl_msg(2, "Recreated metrics in thread #%d", VT_MY_THREAD);
  }
#endif /* VT_METR */
}

void vt_omp_parallel_begin2(uint32_t ptid)
{
#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_registerThread(ptid);
#endif /* VT_MT || VT_HYB */
  vt_omp_parallel_begin();
}

void vt_omp_parallel_end()
{
  VT_CHECK_THREAD;

#if defined(VT_METR)
  if ( VT_MY_THREAD > 0 &&
       vt_metric_num() > 0 && VTTHRD_METV(VTThrdv[VT_MY_THREAD]) ) {
    /* shut down metrics in worker threads */
    vt_metric_free(VTTHRD_METV(VTThrdv[VT_MY_THREAD]));
    VTTHRD_METV(VTThrdv[VT_MY_THREAD]) = NULL;
    vt_metric_thread_fini();
    vt_cntl_msg(2, "Shut down metrics in thread #%d", VT_MY_THREAD);

    /* store last metric values */
    if ( VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]) &&
         VTTHRD_VALV(VTThrdv[VT_MY_THREAD]) )
    {
      memcpy(VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
             VTTHRD_VALV(VTThrdv[VT_MY_THREAD]),
             vt_metric_num() * sizeof(uint64_t));
    }
  }
#endif /* VT_METR */
}

/* -- VampirTrace Internal -- */

void vt_enter_user(uint64_t* time) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  vt_enter(time, vt_trc_regid[VT__TRC_USER]);
}

void vt_exit_user(uint64_t* time) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

  vt_exit(time);
}

void vt_enter_stat(uint64_t* time) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

      VTGen_write_ENTER_STAT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			     time,
			     num_metrics,
			     VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_ENTER_STAT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			     time,
			     0, NULL);
    }
#else /* VT_METR */
  VTGen_write_ENTER_STAT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			 time,
			 0, NULL);
#endif /* VT_METR */
}

void vt_exit_stat(uint64_t* time) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

      VTGen_write_EXIT_STAT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			    time,
			    num_metrics,
			    VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_EXIT_STAT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			    time,
			    0, NULL);
    }
#else /* VT_METR */
  VTGen_write_EXIT_STAT(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			time,
			0, NULL);
#endif /* VT_METR */
}

void vt_enter_flush(uint64_t* time) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

      VTGen_write_ENTER_FLUSH(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			      time,
			      num_metrics,
			      VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_ENTER_FLUSH(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			      time,
			      0, NULL);
    }
#else /* VT_METR */
  VTGen_write_ENTER_FLUSH(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			  time,
			  0, NULL);
#endif /* VT_METR */
}

void vt_exit_flush(uint64_t* time) {
  VT_CHECK_THREAD;

  if (VTTHRD_TRACE_STATUS(VTThrdv[VT_MY_THREAD]) != VT_TRACE_ON) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_OFFV(VTThrdv[VT_MY_THREAD]),
                     VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));

      VTGen_write_EXIT_FLUSH(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			     time,
			     num_metrics,
			     VTTHRD_VALV(VTThrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_EXIT_FLUSH(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			     time,
			     0, NULL);
    }
#else /* VT_METR */
  VTGen_write_EXIT_FLUSH(VTTHRD_GEN(VTThrdv[VT_MY_THREAD]),
			 time,
			 0, NULL);
#endif /* VT_METR */
}
