/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
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
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

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

#if defined(VT_LIBWRAP)
# include "vt_libwrap.h"
#endif /* VT_LIBWRAP */

#if defined(VT_CUDARTWRAP)
# include "vt_cudartwrap.h"
#endif /* VT_CUDARTWRAP */

#if ((defined(VT_MT) || defined(VT_HYB)) && defined(VT_PTHREAD))
# include "vt_pthreadreg.h"
#endif /* (VT_MT || VT_HYB) && VT_PTHREAD */

#if (defined(VT_MPI) || defined(VT_HYB))
# include "vt_mpireg.h"
# include "vt_sync.h"
# include "vt_unimci.h"
# if defined(VT_MPIUNIFYLIB)
#   include "vt_unify_lib.h"
# endif /* VT_MPIUNIFYLIB */
# if (defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
#   include "vt_esync.h"
# endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */
# include "mpi.h"
#endif /* VT_MPI || VT_HYB */

#if defined(VT_GETCPU)
# include "vt_getcpu.h"
#endif /* VT_GETCPU */

#if defined(VT_RUSAGE)
# include "vt_rusage.h"
#endif /* VT_RUSAGE */

#if defined(VT_PLUGIN_CNTR)
# include "vt_plugin_cntr_int.h"
#endif /* VT_PLUGIN_CNTR */


#define GET_THREAD_ID(tid)            \
  if ( (tid) == VT_CURRENT_THREAD ) { \
    VT_CHECK_THREAD;                  \
    (tid) = VT_MY_THREAD;             \
  }

/*
 *-----------------------------------------------------------------------------
 * Two simple hash tables 1: maps region groups to region group identifier
 *                        2: maps file name to file identifier
 *-----------------------------------------------------------------------------
 */

#define HASH_TAB__RDESC  0
#define HASH_TAB__SFILE  1
#define HASH_MAX 1021

typedef struct HN_rdesc
{
  char*             rdesc;  /* region group name */
  uint32_t          rdid;   /* associated region group identifier */
  struct HN_rdesc*  next;
} HashNode_rdesc;

typedef struct HN_sfile
{
  char*             fname;  /* source file name */
  uint32_t          fid;    /* associated source file identifier */
  struct HN_sfile*  next;
} HashNode_sfile;

static HashNode_rdesc* htab_rdesc[HASH_MAX];
static HashNode_sfile* htab_sfile[HASH_MAX];

static void hash_put(int t, const char* n, int i)
{
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

static void* hash_get(int t, const char* n)
{
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) % HASH_MAX;
  if(t==HASH_TAB__RDESC)
  {
    HashNode_rdesc *curr = htab_rdesc[id];
    while ( curr ) {
      if ( strcmp( curr->rdesc, n ) == 0 )
        return curr;

      curr = curr->next;
    }
  }
  else if(t==HASH_TAB__SFILE)
  {
    HashNode_sfile *curr = htab_sfile[id];
    while ( curr ) {
      if ( strcmp( curr->fname, n ) == 0 )
        return curr;

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

/*
 *-----------------------------------------------------------------------------
 * Global variables
 *-----------------------------------------------------------------------------
 */

int vt_num_traces =  1; /* number of processes */
int vt_my_trace   =  0; /* current process id (i.e. MPI-rank) */
int vt_my_ptrace  = -1; /* parent process id */
uint8_t vt_my_trace_is_master = 1; /* 1st process on local node? */
uint8_t vt_my_trace_is_disabled = 0; /* process disabled? */
uint8_t vt_my_trace_is_first_avail = 0; /* 1st not disabled process? */
int vt_my_funique =  0; /* unique file id */

/* array of indices for internal regions */
uint32_t vt_trc_regid[VT__TRC_REGID_NUM];

/* array of induces for internal markers (error, warnings, hints) */
uint32_t vt_trc_mid[3];

/* counter group id for miscellaneous counters (e.g. cpu id) */
uint32_t vt_misc_cgid = 0;

/* flag: indicates whether VampirTrace is initialized and ready to trace */
uint8_t vt_is_alive = 0;

/* flag: indicates whether VampirTrace shall be closed if MPI_Finalize is
         called */
uint8_t vt_close_on_mpi_finalize = 0;

/* compiler adapter finalizer */
void (*vt_comp_finalize)(void) = NULL;

/* start time (set during vt_open() with vt_pform_wtime() */
uint64_t vt_start_time = 0;

/* start time (µs after 00:00:00 UTC 1 January 1970) */
uint64_t vt_start_time_epoch = 0;

/*
 *-----------------------------------------------------------------------------
 * Static variables
 *-----------------------------------------------------------------------------
 */

/* process id at initialization */
static int init_pid = -1;

/* chronological offsets to global time and local times
   (used for classical time synchronization) */
static uint64_t my_ltime[2] = { 0, 1 };
static int64_t  my_offset[2] = { 0, 0 };

/* maximum allowed call stack depth (VT_MAX_STACK_DEPTH) */
static int max_stack_depth = 0;

#if defined(VT_METR)
  /* number of performance metrics */
  static int num_metrics = 0;
#endif

#if defined(VT_RUSAGE)
  /* number of resource usage counters */
  static int num_rusage = 0;
#endif

#if (defined(VT_MT) || defined(VT_HYB))
  /* mutex for locking initialization/finalization */
  static VTThrdMutex* init_mutex = NULL;
#endif /* VT_MT || VT_HYB */

/* id counter starting with 1 */
static uint32_t curid = 1;

/* flags: indicate whether vt_open/vt_close called */
static uint8_t vt_open_called = 0;
static uint8_t vt_close_called = 0;

#if (defined(VT_MPI) || defined(VT_HYB))
  /* flag: indicates whether vt_mpi_finalize called */
  static uint8_t vt_mpi_finalize_called = 0;
#endif /* VT_MPI || VT_HYB */

/*
 *-----------------------------------------------------------------------------
 * Static functions
 *-----------------------------------------------------------------------------
 */

static uint32_t get_unique_file_id(void)
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

    VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

    /* create filename for unique id file */
    snprintf(lock_filename, sizeof(lock_filename)-1, "%s/%s.lock",
             vt_env_gdir(), vt_env_fprefix());

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
      vt_error_msg("Failed to write to file %s: %s", lock_filename,
                   strerror(errno));

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

    VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);
  }

  return new_fuid;
}

static void write_def_header(void)
{
  int32_t  tmp_int32;
  uint64_t tmp_uint64;
  char     tmp_char[128];

  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT"VampirTrace Environment:");

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

  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_MODE: %s", tmp_char);

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

  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_BUFFER_SIZE: %s", tmp_char);

  /* VT_SYNC_FLUSH */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_SYNC_FLUSH: %s",
                 vt_env_sync_flush() ? "yes" : "no");

  /* VT_SYNC_FLUSH_LEVEL */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_SYNC_FLUSH_LEVEL: %i",
                 vt_env_sync_flush_level());

  /* VT_MAX_FLUSHES */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_MAX_FLUSHES: %i",
                 vt_env_max_flushes());

  if( (vt_env_mode() & VT_MODE_STAT) != 0 )
  {
    /* VT_STAT_INTV */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_STAT_INTV: %i",
                   vt_env_stat_intv());

    /* VT_STAT_PROPS */
    tmp_int32 = vt_env_stat_props();

    tmp_char[0] = '\0';
    if( (tmp_int32 & VT_SUM_PROP_FUNC) != 0 )
    {
      strncpy(tmp_char, "FUNC", sizeof(tmp_char)-1);
      tmp_char[sizeof(tmp_char)-1] = '\0';
    }

    if( (tmp_int32 & VT_SUM_PROP_MSG) != 0 )
    {
      if( strlen(tmp_char) > 0 )
        strncat(tmp_char, ":", sizeof(tmp_char)-1-strlen(tmp_char));

      strncat(tmp_char, "MSG", sizeof(tmp_char)-1-strlen(tmp_char));
    }

    if( (tmp_int32 & VT_SUM_PROP_COLLOP) != 0 )
    {
      if( strlen(tmp_char) > 0 )
        strncat(tmp_char, ":", sizeof(tmp_char)-1-strlen(tmp_char));

      strncat(tmp_char, "COLLOP", sizeof(tmp_char)-1-strlen(tmp_char));
    }

/*    if( (tmp_int32 & VT_SUM_PROP_FILEOP) != 0 )
    {
      if( strlen(tmp_char) > 0 )
        strncat(tmp_char, ":", sizeof(tmp_char)-1-strlen(tmp_char));

      strncat(tmp_char, "FILEOP", sizeof(tmp_char)-1-strlen(tmp_char));
    }*/

    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_STAT_PROPS: %s", tmp_char);
  }

#if defined(VT_METR)
  /* VT_METRICS */
  vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_VT_COMMENT" VT_METRICS: %s",
                 vt_env_metrics() ? vt_env_metrics() : "<not set>");
#endif /* VT_METR */

#if defined(VT_RUSAGE)
  /* VT_RUSAGE */
  vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_VT_COMMENT" VT_RUSAGE: %s",
                 vt_env_rusage() ? vt_env_rusage() : "<not set>");

  if( vt_env_rusage() )
  {
    /* VT_RUSAGE_INTV */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_RUSAGE_INTV: %i",
                   vt_env_rusage_intv());
  }
#endif /* VT_RUSAGE */

#if (defined(VT_MPI) || defined(VT_HYB))
  /* VT_MPITRACE */
  vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_VT_COMMENT" VT_MPITRACE: %s",
                 vt_env_mpitrace() ? "yes" : "no");
#endif /* VT_MPI || VT_HYB */

#if defined(VT_UNIMCI)
  if( vt_env_mpitrace() )
  {
    /* VT_MPICHECK */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_MPICHECK: %s",
                   vt_env_mpicheck() ? "yes" : "no");

    /* VT_MPICHECK_ERREXIT */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_MPICHECK_ERREXIT: %s",
                   vt_env_mpicheck_errexit() ? "yes" : "no");
  }
#endif /* VT_UNIMCI */

#if defined(VT_MEMHOOK)
  /* VT_MEMTRACE */
  vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_VT_COMMENT" VT_MEMTRACE: %s",
                 vt_env_memtrace() ? "yes" : "no");

  if( vt_env_memtrace() )
  {
    /* VT_MEMTRACE_MARKER */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_MEMTRACE_MARKER: %s",
                   vt_env_memtrace_marker() ? "yes" : "no");
  }
#endif /* VT_MEMHOOK */

#if defined(VT_GETCPU)
  /* VT_CPUIDTRACE */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_CPUIDTRACE: %s",
                 vt_env_cpuidtrace() ? "yes" : "no");
#endif /* VT_GETCPU */

#if defined(VT_IOWRAP)
  /* VT_IOTRACE */
  vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_VT_COMMENT" VT_IOTRACE: %s",
                 vt_env_iotrace() ? "yes" : "no");
#endif /* VT_IOWRAP */

#if defined(VT_CUDARTWRAP)
  /* VT_CUDARTTRACE */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_CUDARTTRACE: %s",
                 vt_env_cudarttrace() ? "yes" : "no");

  if( vt_env_cudarttrace() )
  {
    /* VT_CUDATRACE_BUFFER_SIZE */
    tmp_uint64 = (uint64_t)vt_env_cudatrace_bsize();

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

    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_CUDATRACE_BUFFER_SIZE: %s",
                   tmp_char);

    /* VT_CUDATRACE_IDLE */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_CUDATRACE_IDLE: %s",
                   vt_env_cudatrace_idle() ? "yes" : "no");

    /* VT_CUDATRACE_KERNEL */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_CUDATRACE_KERNEL: %s",
                   vt_env_cudatrace_kernel() ? "yes" : "no");

    /* VT_CUDATRACE_MEMCPYASYNC */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_CUDATRACE_MEMCPYASYNC: %s",
                   vt_env_cudatrace_memcpyasync() ? "yes" : "no");

    /* VT_CUDATRACE_GPUMEMUSAGE */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_CUDATRACE_GPUMEMUSAGE: %s",
                   vt_env_cudatrace_gpumem() ? "yes" : "no");

    /* VT_CUDATRACE_SYNC */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_CUDATRACE_SYNC: %s",
                   vt_env_cudatrace_sync() ? "yes" : "no");
  }
#endif /* VT_CUDARTWRAP */

#if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  /* VT_ETIMESYNC */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_ETIMESYNC: %s",
                 vt_env_etimesync() ? "yes" : "no");

  if( vt_env_etimesync() )
  {
    /* VT_ETIMESYNC_INTV */
    vt_def_comment(VT_MASTER_THREAD,
                   VT_UNIFY_STRID_VT_COMMENT" VT_ETIMESYNC_INTV: %i",
                   vt_env_etimesync_intv());
  }
#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

#if defined(VT_THRD_PTHREAD)
  /* VT_PTHREAD_REUSE */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_PTHREAD_REUSE: %s",
                 vt_env_pthread_reuse() ? "yes" : "no");
#endif /* VT_THRD_PTHREAD */

  /* VT_FILER_SPEC */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_FILTER_SPEC: %s",
                 vt_env_filter_spec() ? vt_env_filter_spec() : "<not set>");

  /* VT_GROUPS_SPEC */
  vt_def_comment(VT_MASTER_THREAD,
                 VT_UNIFY_STRID_VT_COMMENT" VT_GROUPS_SPEC: %s",
                 vt_env_groups_spec() ? vt_env_groups_spec() : "<not set>");
}

static void write_uctl_file(void)
{
  char* uctl_data;
  size_t uctl_data_size;

  int i;

#if (!defined(VT_MPI) && !defined(VT_HYB))
  if (vt_my_trace != 0) return;
#endif /* !VT_MPI && VT_HYB */

  /* calculate size needed for uctl data */
  uctl_data_size =
    2 +                     /* "*:" */
    VTThrdn * (8 + 1 + 1) + 1 + /* stream ids[!]: + '\n' */
    4 * (16 + 1) + 1 + 1;   /* 2xltime + 2xoffset + '\n' + '\0' */
#if (defined(VT_LIBCWRAP) && defined(VT_FORK))
  if (vt_env_libctrace())
    uctl_data_size += vt_fork_get_num_childs_tot() * (8 + 1);
#endif /* VT_LIBCWRAP && VT_FORK */

  /* allocate memory for unify control data */
  uctl_data = (char*)malloc(uctl_data_size * sizeof(char));
  if (uctl_data == NULL)
    vt_error();

  sprintf(uctl_data, "*:");

  /* add stream ids to uctl data */
  for (i = 0; i < (int)VTThrdn; i++)
  {
    sprintf(uctl_data + strlen(uctl_data), "%x%s:",
            VT_PROCESS_ID(vt_my_trace, i), vt_my_trace_is_disabled ? "!" : "");
  }

#if (defined(VT_LIBCWRAP) && defined(VT_FORK))
  /* add stream ids of forked child processes to uctl data, if necessary */
  if (vt_env_libctrace())
  {
    for (i = 1; i <= (int)vt_fork_get_num_childs_tot(); i++)
      sprintf(uctl_data + strlen(uctl_data), "%x:", vt_my_trace+1+i);
  }
#endif /* VT_LIBCWRAP && VT_FORK */

  strcat(uctl_data, "\n");

  /* add chronological offsets to global time and local times */
  sprintf(uctl_data + strlen(uctl_data),
          "%llx:%llx:%llx:%llx:\n",
          (unsigned long long int)my_ltime[0],
          (unsigned long long int)my_offset[0],
          (unsigned long long int)my_ltime[1],
          (unsigned long long int)my_offset[1]);

#if (defined (VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  /* add enhanced time synchronization to uctl data, if necessary */
  if (vt_env_etimesync())
    vt_esync_app_uctl_data(&uctl_data);
#endif /* VT_ETIMESYNC && !TIMER_IS_GLOBAL */

#if (defined(VT_MPI) || defined(VT_HYB))
  {
    VT_MPI_INT send_count = strlen(uctl_data);

    VT_MPI_INT  recv_count = 0;
    VT_MPI_INT* recv_counts = NULL;
    VT_MPI_INT* recv_displs = NULL;

    char* recv_buffer = NULL;

    /* rank 0 allocates memory for receive counts and displacements */
    if (vt_my_trace == 0)
    {
      recv_counts = (VT_MPI_INT*)malloc(vt_num_traces * sizeof(VT_MPI_INT));
      if (recv_counts == NULL)
        vt_error();
      recv_displs = (VT_MPI_INT*)malloc(vt_num_traces * sizeof(VT_MPI_INT));
      if (recv_displs == NULL)
        vt_error();
    }

    /* gather sizes of uctl data */
    PMPI_Gather(&send_count, 1, MPI_UNSIGNED, recv_counts, 1, MPI_UNSIGNED, 0,
                MPI_COMM_WORLD);

    /* rank 0 calculates the size needed to hold all uctl data
       and sets the relative displacements in the receive buffer */
    if (vt_my_trace == 0)
    {
      VT_MPI_INT offset = 0;
      recv_count = 0;
      for(i = 0; i < vt_num_traces; i++)
      {
        recv_count += recv_counts[i];
        recv_displs[i] = offset;
        offset += recv_counts[i];
      }

      /* allocate receive buffer for all uctl data */
      recv_buffer = (char*)malloc((recv_count + 1) * sizeof(char));
      if(recv_buffer == NULL)
        vt_error();
    }

    /* gather all uctl data */
    PMPI_Gatherv(uctl_data, send_count, MPI_CHAR, recv_buffer, recv_counts,
                 recv_displs, MPI_CHAR, 0, MPI_COMM_WORLD);

    if (vt_my_trace == 0)
    {
      /* terminate received uctl data */
      recv_buffer[recv_count] = '\0';

      free(uctl_data);
      uctl_data = recv_buffer;

      /* free memory for receive counts and displacements */
      free(recv_counts);
      free(recv_displs);
    }
  }

  /* rank 0 writes the uctl file */
  if (vt_my_trace == 0)
#endif /* VT_MPI || VT_HYB */
  {
    FILE* uctl_file;
    char  uctl_filename[1024];

    /* compose uctl file name */
    if (vt_my_funique > 0)
      snprintf(uctl_filename, sizeof(uctl_filename) - 1, "%s/%s_%u.uctl",
               vt_env_gdir(), vt_env_fprefix(), vt_my_funique);
    else
      snprintf(uctl_filename, sizeof(uctl_filename) - 1, "%s/%s.uctl",
               vt_env_gdir(), vt_env_fprefix());

    /* open uctl file */
    uctl_file = fopen(uctl_filename, "w");
    if (uctl_file == NULL)
    vt_error_msg("Cannot open file %s", uctl_filename);

    /* write uctl data to file */
    fprintf(uctl_file, "%s", uctl_data);

    /* close uctl file */
    fclose(uctl_file);

    vt_cntl_msg(2, "Wrote unify control file %s", uctl_filename);
  }

  /* free uctl data */
  free(uctl_data);
}

static void unify_traces(void)
{
  int argc = 1;
  char** argv;

  int i;

  uint8_t error = 0;

#if !defined(VT_MPIUNIFYLIB)
  /* rank 0 calls the unify command */
  if (vt_my_trace != 0) return;
#endif /* VT_MPIUNIFYLIB */

  /* compose unify arguments */

  argv = (char**)calloc(10 + vt_env_verbose(), sizeof(char*));
  if (argv == NULL) vt_error();

  argv[0] = NULL;

  argv[argc] = (char*)calloc(VT_PATH_MAX, sizeof(char));
  if (argv[argc] == NULL) vt_error();

  if (vt_my_funique > 0)
  {
    snprintf(argv[argc], VT_PATH_MAX-1, "%s/%s_%u",
             vt_env_gdir(), vt_env_fprefix(), vt_my_funique);
  }
  else
  {
    snprintf(argv[argc], VT_PATH_MAX-1, "%s/%s",
             vt_env_gdir(), vt_env_fprefix());
  }
  argc++;

  if (!vt_env_compression()) argv[argc++] = strdup("--nocompress");
  if (!vt_env_do_clean())    argv[argc++] = strdup("-k");
  if (vt_env_verbose() == 0) argv[argc++] = strdup("-q");
  else if (vt_env_verbose() >= 2)
  {
    for (i=1;i<vt_env_verbose();i++)
      argv[argc++] = strdup("-v");
    argv[argc++] = strdup("-p");
  }

  /* do actual unify */
  vt_cntl_msg(2, "Starting trace unification");

#if defined(VT_MPIUNIFYLIB)
  {
    /* either by calling unify library... */
    if (VTUnify(argc, argv) != 0)
      error = 1;
  }
#else /* VT_MPIUNIFYLIB */
  {
    /* ...or by calling unify command */

    char* cmd;
    int len;
    int rc;

    cmd = vt_installdirs_expand("${bindir}/vtunify");
    if (cmd == NULL) vt_error();

    len = strlen(cmd);
    for (i=1;i<argc;i++)
      len += 1 + strlen(argv[i]);

    cmd = (char*)realloc(cmd, (len + 1) * sizeof(char));
    if (cmd == NULL) vt_error();

    for (i=1;i<argc;i++)
    {
      strcat(cmd, " ");
      strcat(cmd, argv[i]);
    }

    vt_cntl_msg(2, "Executing %s", cmd);
    rc = system(cmd);
    if (rc == -1)
      vt_error_msg("Failed to execute %s", cmd);
    else if (!WIFEXITED(rc) || WEXITSTATUS(rc) != 0)
      error = 1;

    free(cmd);
  }
#endif /* VT_MPIUNIFYLIB */

  if (error)
    vt_error_msg("Trace unification has terminated abnormally");

  for (i=1;i<argc;i++)
    free(argv[i]);
  free(argv);
}

/*
 *-----------------------------------------------------------------------------
 * Global functions
 *-----------------------------------------------------------------------------
 */

void vt_open()
{
  /* double check whether vt_open() has already been called (avoid deadlock) */
  if ( vt_open_called )
    return;

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

  /* initialize thread object management */
  VTThrd_init();

  /* get initial timestamp */
  vt_start_time = vt_pform_wtime();

  /* write start-time as comment to definitions */
  {
    struct timeval tv0, tv1;
    gettimeofday(&tv0, NULL);
    do { gettimeofday(&tv1, NULL); } while ( tv0.tv_usec == tv1.tv_usec );

    vt_start_time_epoch = ((uint64_t)tv1.tv_sec * (uint64_t)1000000) +
       (uint64_t)tv1.tv_usec;
    vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_STARTTIME_COMMENT"%llu",
                   (unsigned long long)vt_start_time_epoch);
  }

#if !(defined(VT_MPI) || defined(VT_HYB))

  /* write VT related definition comments */
  write_def_header();

  /* get unique file id */
  if (vt_my_ptrace == -1 && vt_env_funique() > -1)
    vt_my_funique = (int)get_unique_file_id();

#endif /* VT_MPI || VT_HYB */

#if !defined(VT_DISABLE_RFG)

  /* set file names for filter and groups specification */
  {
    char* filter_deffile = vt_env_filter_spec();
    char* groups_deffile = vt_env_groups_spec();

    if( filter_deffile )
    {
      RFG_Regions_setFilterDefFile(VTThrdv[0]->rfg_regions, filter_deffile);
      if( !RFG_Regions_readFilterDefFile(VTThrdv[0]->rfg_regions, -1, NULL) )
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
    vt_def_region(VT_MASTER_THREAD, "user", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
                  NULL, VT_FUNCTION);

  /* register function "sync" */
  vt_trc_regid[VT__TRC_SYNC] =
    vt_def_region(VT_MASTER_THREAD, "sync", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
                  NULL, VT_INTERNAL);

  /* register function "sync time" */
  vt_trc_regid[VT__TRC_SYNCTIME] =
    vt_def_region(VT_MASTER_THREAD, "sync time", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
                  NULL, VT_INTERNAL);

  /* register function "flush" */
  vt_trc_regid[VT__TRC_FLUSH] =
    vt_def_region(VT_MASTER_THREAD, "flush", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
                  NULL, VT_INTERNAL);

  /* register function "stat" */
  vt_trc_regid[VT__TRC_STAT] =
    vt_def_region(VT_MASTER_THREAD, "dump statistics", VT_NO_ID, VT_NO_LNO,
                  VT_NO_LNO, NULL, VT_INTERNAL);

  /* register function "off" */
  vt_trc_regid[VT__TRC_OFF] =
    vt_def_region(VT_MASTER_THREAD, "tracing off", VT_NO_ID, VT_NO_LNO,
                  VT_NO_LNO, NULL, VT_INTERNAL);

  /* register function "rewind" */
  vt_trc_regid[VT__TRC_REWIND] =
    vt_def_region(VT_MASTER_THREAD, "rewind", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
                  NULL, VT_INTERNAL);

#if ((defined(VT_MT) || defined(VT_HYB)) && defined(VT_OMP))

  /* register function "parallel region" */
  vt_trc_regid[VT__TRC_OMPPREG] =
    vt_def_region(VT_MASTER_THREAD, "parallel region", VT_NO_ID, VT_NO_LNO,
                  VT_NO_LNO, NULL, VT_OMP_PARALLEL_REGION);

#endif /* (VT_MT || VT_HYB) && VT_OMP */

  /* internal markers will be defined when they are used */
  vt_trc_mid[VT__TRC_MARKER_ERROR]   = VT_NO_ID;
  vt_trc_mid[VT__TRC_MARKER_WARNING] = VT_NO_ID;
  vt_trc_mid[VT__TRC_MARKER_HINT]    = VT_NO_ID;

  /* define counter group for miscellaneous counters (e.g. cpu id) */
  vt_misc_cgid = vt_def_counter_group(VT_MASTER_THREAD, "Miscellaneous");

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

#if !(defined(VT_JAVA) || defined(VT_MPI) || defined(VT_HYB))
  atexit(vt_close);

  /* install signal handlers for process termination */
# ifdef SIGINT
  if (signal(SIGINT, vt_close_by_signal) == SIG_ERR)
    vt_warning("Could not install handler for signal SIGINT");
# endif /* SIGINT */
# ifdef SIGQUIT
  if (signal(SIGQUIT, vt_close_by_signal) == SIG_ERR)
    vt_warning("Could not install handler for signal SIGQUIT");
# endif /* SIGQUIT */
# ifdef SIGTERM
  if (signal(SIGTERM, vt_close_by_signal) == SIG_ERR)
    vt_warning("Could not install handler for signal SIGTERM");
# endif /* SIGTERM */

#endif /* VT_JAVA || VT_MPI || VT_HYB */

  init_pid = getpid();

#if defined(VT_METR)
  {
    uint32_t gid;
    int i;

    /* return if no counters requested */
    if ( num_metrics > 0 )
    {
      /* write counter group name */
      gid = vt_def_counter_group(VT_MASTER_THREAD, VT_METR);

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

#if (defined(VT_PLUGIN_CNTR))
# if (!defined(VT_MPI) && !defined(VT_HYB))
  vt_plugin_cntr_init();
  /* if we really use plugins */
  if (vt_plugin_cntr_used)
  {
    vt_plugin_cntr_thread_init(VTThrdv[0], 0);

    /* if this thread also uses plugins */
    if (VTTHRD_PLUGIN_CNTR_DEFINES(VTThrdv[0]))
      vt_plugin_cntr_thread_enable_counters(VTThrdv[0]);
  }
#endif /* !VT_MPI && !VT_HYB */
#endif /* VT_PLUGIN_CNTR */

  vt_is_alive = 1;

#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_unlock(&init_mutex);
#endif /* VT_MT || VT_HYB */

  if( vt_env_debug() > 0 )
  {
    uint64_t t = vt_pform_wtime();
    vt_comment(VT_MASTER_THREAD, &t, "NODEID: %lx", vt_pform_node_id());
    vt_comment(VT_MASTER_THREAD, &t, "NODEID_31: %x",
               vt_pform_node_id() & 0x7fffffff);
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

#if defined(VT_PLUGIN_CNTR)

  /* finalize counter plugins */
  vt_plugin_cntr_finalize();

#endif /* VT_PLUGIN_CNTR */

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
    vt_enter_user(VT_MASTER_THREAD, &time);
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

#if (defined(VT_MPI) || defined(VT_HYB))
  /* do not close VampirTrace before MPI_Finalize is called */
  if ( !vt_mpi_finalize_called )
  {
    vt_close_on_mpi_finalize = 1;
#if defined(VT_HYB)
    VTThrd_unlock(&init_mutex);
#endif /* VT_HYB */
    return;
  }
#endif /* VT_MPI || VT_HYB */

  if( vt_env_debug() > 0 )
  {
    uint64_t t = vt_pform_wtime();
    vt_comment(VT_MASTER_THREAD, &t, "NODEID: %lx", vt_pform_node_id());
    vt_comment(VT_MASTER_THREAD, &t, "NODEID_31: %x",
               vt_pform_node_id() & 0x7fffffff);
  }

  vt_close_called = 1;
  vt_is_alive = 0;

  tnum = (int)VTThrdn;

#if defined(VT_CUDARTWRAP)

  /* finalize cuda wrapping if enabled */
  if (vt_env_cudarttrace())
    vt_cudartwrap_finalize();

#endif /* VT_CUDARTWRAP */

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
    vt_def_comment(VT_MASTER_THREAD, VT_UNIFY_STRID_STOPTIME_COMMENT"%llu",
                   (unsigned long long)stop_time_epoch);
  }

  /* close trace files */
  for (i = 0; i < tnum; i++)
    VTThrd_close(VTThrdv[i]);

#if (defined(VT_LIBCWRAP) && defined(VT_FORK))

  /* wait until all child processes are terminated */
  if (vt_env_libctrace())
    vt_fork_waitchilds();

#endif /* VT_LIBCWRAP && VT_FORK */

  /* write unify control file */
  write_uctl_file();

#if (defined(VT_LIBCWRAP) && defined(VT_FORK))

  if (vt_env_libctrace())
  {
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

#if defined(VT_PLUGIN_CNTR)

  /* finalize counter plugins */
  vt_plugin_cntr_finalize();

#endif /* VT_PLUGIN_CNTR */

  /* unify local traces, if desired */
  if ( vt_env_do_unify() )
    unify_traces();

#if (defined(VT_MPI) || defined(VT_HYB))
  PMPI_Finalize();
#endif /* VT_MPI || VT_HYB */

#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_unlock(&init_mutex);
  VTThrd_deleteMutex(&init_mutex);
#endif /* VT_MT || VT_HYB */
}

void vt_trace_on(uint32_t tid, uint8_t mark)
{
  GET_THREAD_ID(tid);

  if ( vt_is_alive &&
       VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_OFF )
  {
    /* switch tracing on, if current call stack level is equal to the
       stored one at switching trace off */
    if ( VTTHRD_STACK_LEVEL(VTThrdv[tid]) ==
         VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[tid]) )
    {
      VTTHRD_TRACE_STATUS(VTThrdv[tid]) = VT_TRACE_ON;

      if ( mark )
      {
        uint64_t time = vt_pform_wtime();
        vt_exit(tid, &time);
      }

      vt_cntl_msg(2, "Tracing switched on");
    }
    /* otherwise: abort */
    else
    {
      vt_error_msg("Could not switch tracing on.\n"
                   "The current call stack level (%i) isn't equal to the "
                   "stored one (%i) at switching trace off.",
                   VTTHRD_STACK_LEVEL(VTThrdv[tid]),
                   VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[tid]) );
    }
  }
}

void vt_trace_off(uint32_t tid, uint8_t mark, uint8_t permanent)
{
  GET_THREAD_ID(tid);

  if ( vt_is_alive &&
       VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_OFF_PERMANENT )
  {
    if ( mark )
    {
      uint64_t time;
      time = vt_pform_wtime();

      /* directly call the VTGen API to prevent calling to vt_update_counter()
         if tracing is going to switch off permanently */
      if ( permanent )
        VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[tid]), &time,
                          vt_trc_regid[VT__TRC_OFF], 0);
      else
        vt_enter(tid, &time, vt_trc_regid[VT__TRC_OFF]);
    }

    if ( permanent )
    {
      VTTHRD_TRACE_STATUS(VTThrdv[tid]) = VT_TRACE_OFF_PERMANENT;

      vt_cntl_msg(1, "Tracing switched off permanently");
    }
    else if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON )
    {
      /* store current call stack level */
      VTTHRD_TRACE_STATUS(VTThrdv[tid]) = VT_TRACE_OFF;
      VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[tid]) =
      VTTHRD_STACK_LEVEL(VTThrdv[tid]);

      vt_cntl_msg(2, "Tracing switched off at call stack level (%i)",
      VTTHRD_STACK_LEVEL_AT_OFF(VTThrdv[tid]));
    }
  }
}

uint8_t vt_is_trace_on(uint32_t tid)
{
  GET_THREAD_ID(tid);

  return (vt_is_alive && VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON);
}

void vt_guarantee_buffer(uint32_t tid, size_t size)
{
  GET_THREAD_ID(tid);

  if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON )
    VTGen_guarantee(VTTHRD_GEN(VTThrdv[tid]), size);
}

void vt_buffer_flush(uint32_t tid)
{
  GET_THREAD_ID(tid);

  if ( !vt_is_alive ) return;

  VTGen_flush(VTTHRD_GEN(VTThrdv[tid]), 0, vt_pform_wtime(), NULL);
}

void vt_update_counter(uint32_t tid, uint64_t* time)
{
#if (defined(VT_METR) || defined(VT_RUSAGE) || defined(VT_GETCPU) ||           \
     defined(VT_PLUGIN_CNTR))
  GET_THREAD_ID(tid);

#if defined(VT_METR)
  /* update hardware performance counters (VT_METRICS) */
  if ( num_metrics > 0 && VTThrdv[tid]->is_virtual_thread == 0 &&
       VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON )
  {
    int i;

    vt_metric_read(VTTHRD_METV(VTThrdv[tid]),
                   VTTHRD_OFFV(VTThrdv[tid]),
                   VTTHRD_VALV(VTThrdv[tid]));

    for ( i = 0; i < num_metrics; i++ )
    {
      if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) break;

      VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[tid]),
                          time,
                          i+1,
                          VTTHRD_VALV(VTThrdv[tid])[i]);
    }
  }
#endif /* VT_METR */

#if defined(VT_RUSAGE)
  /* update resource usage counters (VT_RUSAGE) */
  if ( num_rusage > 0 &&
       VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON &&
       *(time) >= VTTHRD_RU_NEXT_READ(VTThrdv[tid]) )
  {
    int i;
    uint32_t changed;
    vt_rusage_read(VTTHRD_RU_OBJ(VTThrdv[tid]),
                   VTTHRD_RU_VALV(VTThrdv[tid]), &changed);
    for ( i = 0; i < num_rusage; i++ )
    {
      if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) break;

      if ( (changed & (1<<i)) != 0 )
      {
        VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[tid]),
                            time,
                            vt_rusage_cidv[i],
                            VTTHRD_RU_VALV(VTThrdv[tid])[i]);
      }
    }

    VTTHRD_RU_NEXT_READ(VTThrdv[tid]) = *(time) + vt_rusage_intv;
  }
#endif /* VT_RUSAGE */

#if defined(VT_GETCPU)
  /* update cpu id counter (VT_GETCPU) */
  if ( vt_env_cpuidtrace() &&
       VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON )
  {
    uint8_t changed;
    vt_getcpu_read(&(VTTHRD_CPUID_VAL(VTThrdv[tid])), &changed);
    if ( changed )
    {
      VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[tid]),
                          time,
                          vt_getcpu_cid,
                          VTTHRD_CPUID_VAL(VTThrdv[tid]));
    }
  }
#endif /* VT_GETCPU */

#if defined(VT_PLUGIN_CNTR)
  /* update synchronous, asynchronous callback and asynchronous on event
   * plugin values. */
  /* But only if we really use plugins and this thread also uses some */
  if (vt_plugin_cntr_used && VTTHRD_PLUGIN_CNTR_DEFINES(VTThrdv[tid]))
  {
    if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON)
    {
      int plugin_metrics = vt_plugin_cntr_get_num_synch_metrics(VTThrdv[tid]);
      uint32_t counter_id;
      uint64_t value;
      int i;
      for ( i = 0; i < plugin_metrics; i++ )
      {
        if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) break;

        vt_plugin_cntr_get_synch_value(VTThrdv[tid], i, &counter_id, &value);

        VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[tid]),
                            time,
                            counter_id,
                            value);
      }

      if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON){
        vt_plugin_cntr_write_callback_data(*time,tid);
        vt_plugin_cntr_write_asynch_event_data(*time,tid);
      }
    }
  }
#endif /* VT_PLUGIN_CNTR */

#endif /* VT_METR) || VT_RUSAGE || VT_GETCPU || VT_PLUGIN_CNTR */
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

  /* read filter rules for current rank */
  {
    char* filter_deffile = vt_env_filter_spec();
    if (filter_deffile)
    {
      if (!RFG_Regions_readFilterDefFile(VTThrdv[0]->rfg_regions,
                                         vt_my_trace, &vt_my_trace_is_disabled))
      {
        vt_error_msg("Could not read region filter specification file");
      }

      /* if the current rank shall be disabled, switch tracing off */
      if( vt_my_trace_is_disabled )
        vt_trace_off(VT_MASTER_THREAD, 0, 1);
    }
  }

  /* determine first available (not disabled) rank */
  {
    VT_MPI_INT rank_avail = vt_my_trace_is_disabled ? 0x7FFFFFFF : myrank;
    VT_MPI_INT first_avail_rank;

    PMPI_Allreduce(&rank_avail, &first_avail_rank, 1, MPI_INT, MPI_MIN,
                   MPI_COMM_WORLD);

    if (myrank == first_avail_rank)
      vt_my_trace_is_first_avail = 1;
  }

#else /* VT_DISABLE_RFG */

  if (vt_my_trace == 0)
    vt_my_trace_is_first_avail = 1;

#endif /* VT_DISABLE_RFG */

  /* register remaining MPI routines */
  vt_mpi_register_remain();

  /* write VT related definition comments */
  if (vt_my_trace_is_first_avail)
    write_def_header();

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
      vt_my_funique = (int)get_unique_file_id();
    if (vt_num_traces > 1)
      PMPI_Bcast(&vt_my_funique, 1, MPI_INT, 0, MPI_COMM_WORLD);
  }

  /* determine first rank on current node */
  {
    MPI_Comm host_comm;
    VT_MPI_INT host_rank;

    PMPI_Comm_split(MPI_COMM_WORLD, (vt_pform_node_id() & 0x7FFFFFFF), 0, &host_comm);
    PMPI_Comm_rank(host_comm, &host_rank);

    vt_my_trace_is_master = (uint8_t)(host_rank == 0);
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

#if defined(VT_PLUGIN_CNTR)
  /* call it when it can be called */
  vt_plugin_cntr_init();
  /* if we really use plugins */
  if (vt_plugin_cntr_used)
  {
    vt_plugin_cntr_thread_init(VTThrdv[0], 0);

    /* if this thread uses plugins */
    if (VTTHRD_PLUGIN_CNTR_DEFINES(VTThrdv[0]))
      vt_plugin_cntr_thread_enable_counters(VTThrdv[0]);
  }
#endif /* VT_PLUGIN_CNTR */

  atexit(vt_close);

#endif /* VT_MPI || VT_HYB */
}

void vt_mpi_finalize()
{
#if (defined(VT_MPI) || defined(VT_HYB))
  vt_mpi_finalize_called = 1;

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

void vt_mpi_sync(uint32_t tid, uint64_t* time, void* comm)
{
#if (defined(VT_MPI) || defined(VT_HYB))
  static const int sync_flush_flag = 1<<0;
  static const int sync_time_flag  = 1<<1;
  static int sync_flush_env = -1;
  static int sync_flush_level_env = -1;
  static int sync_time_env = -1;
  VT_MPI_INT lsync_mask = 0;
  VT_MPI_INT sync_mask = 0;

  GET_THREAD_ID(tid);

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
  vt_enter(tid, time, vt_trc_regid[VT__TRC_SYNC]);

  /* checking whether buffer flush needed */

  if (sync_flush_env)
  {
    /* set bit for flushing buffer, if fill level >= sync_flush_level_env */
    if ((int)VTGen_get_buflevel(VTTHRD_GEN(VTThrdv[tid])) >=
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
    VTGen_flush(VTTHRD_GEN(VTThrdv[tid]), 0, vt_pform_wtime(), NULL);
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
  vt_exit(tid, time);
#endif /* VT_MPI || VT_HYB */
}

uint32_t vt_get_curid()
{
  return curid++;
}

/*
 *-----------------------------------------------------------------------------
 * Definition records
 *-----------------------------------------------------------------------------
 */

void vt_def_comment(uint32_t tid, const char* fmt, ...)
{
  char comment[VT_MAX_COMMENT_LEN];
  va_list ap;

  GET_THREAD_ID(tid);

  va_start(ap, fmt);

  vsnprintf(comment, VT_MAX_COMMENT_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_DEFINITION_COMMENT(VTTHRD_GEN(VTThrdv[tid]), comment);
}

uint32_t vt_def_scl_file(uint32_t tid, const char* fname)
{
  uint32_t fid;

  HashNode_sfile* hn;

  GET_THREAD_ID(tid);

  hn = hash_get(HASH_TAB__SFILE, fname);

  if( hn == NULL )
  {
    fid = curid++;

    VTGen_write_DEF_SCL_FILE(VTTHRD_GEN(VTThrdv[tid]), fid, fname);

    hash_put(HASH_TAB__SFILE, fname, fid);
  }
  else
  {
    fid = hn->fid;
  }

  return fid;
}

uint32_t vt_def_scl(uint32_t tid, uint32_t fid, uint32_t begln, uint32_t endln)
{
  uint32_t sid;

  GET_THREAD_ID(tid);

  if( fid == VT_NO_ID || begln == VT_NO_LNO )
    return 0;

  sid = curid++;

  VTGen_write_DEF_SCL(VTTHRD_GEN(VTThrdv[tid]), sid, fid, begln);

  return sid;
}

uint32_t vt_def_file_group(uint32_t tid, const char* gname)
{
  uint32_t gid;

  GET_THREAD_ID(tid);


  gid = curid++;

  VTGen_write_DEF_FILE_GROUP(VTTHRD_GEN(VTThrdv[tid]), gid, gname);

  return gid;
}

uint32_t vt_def_file(uint32_t tid, const char* fname, uint32_t gid)
{
  uint32_t fid;

  GET_THREAD_ID(tid);

  fid = curid++;

  VTGen_write_DEF_FILE(VTTHRD_GEN(VTThrdv[tid]),
                       fid,
                       fname,
                       gid);

  return fid;
}

uint32_t vt_def_region_group(uint32_t tid, const char* gname)
{
  uint32_t rdid;

  HashNode_rdesc* hn;

  GET_THREAD_ID(tid);

  hn = hash_get(HASH_TAB__RDESC, gname);

  if(hn == NULL)
  {
    rdid = curid++;

    VTGen_write_DEF_FUNCTION_GROUP(VTTHRD_GEN(VTThrdv[tid]),
                                    rdid, gname);

    hash_put(HASH_TAB__RDESC, gname, rdid);
  }
  else
  {
    rdid = hn->rdid;
  }

  return rdid;
}

uint32_t vt_def_region(uint32_t tid, const char* rname, uint32_t fid,
                       uint32_t begln, uint32_t endln, const char* rdesc,
                       uint8_t rtype)
{
#if !defined(VT_DISABLE_RFG)
  RFG_RegionInfo* rinf;
#endif
  uint32_t sid;
  uint32_t rid;
  uint32_t rdid;

  GET_THREAD_ID(tid);

  sid = vt_def_scl(tid, fid, begln, endln);
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
        rdesc = VT_DEFAULT_REGION_GROUP;
        break;
    }
  }

  /* define group and store identifier */
  rdid = vt_def_region_group(tid, rdesc);

#if (defined(VT_MPI) || defined(VT_HYB))
  /* define MPI collective operation, if necessary */
  if ( rtype == VT_MPI_COLL_ALL2ALL ||
       rtype == VT_MPI_COLL_ALL2ONE ||
       rtype == VT_MPI_COLL_BARRIER ||
       rtype == VT_MPI_COLL_ONE2ALL ||
       rtype == VT_MPI_COLL_OTHER )
  {
    VTGen_write_DEF_COLLECTIVE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                                         rid,   /* collective id equal region id */
                                         rname, /* collective name equal region name */
                                         rtype);
  }
#endif /* VT_MPI || VT_HYB */

  /* define region and return identifier */
  VTGen_write_DEF_FUNCTION(VTTHRD_GEN(VTThrdv[tid]),
                           rid,
                           rname,
                           rdid,
                           sid);

  return rid;
}

uint32_t vt_def_counter_group(uint32_t tid, const char* gname)
{
  uint32_t gid;

  GET_THREAD_ID(tid);

  gid = curid++;

  VTGen_write_DEF_COUNTER_GROUP(VTTHRD_GEN(VTThrdv[tid]), gid, gname);

  return gid;
}

uint32_t vt_def_counter(uint32_t tid, const char* cname, uint32_t cprop,
                        uint32_t gid, const char* cunit)
{
  uint32_t cid = 0;

  GET_THREAD_ID(tid);

#if defined(VT_METR)
  cid = num_metrics;
#endif /* VT_METR */

  cid += curid++;

  VTGen_write_DEF_COUNTER(VTTHRD_GEN(VTThrdv[tid]),
                          cid,
                          cname,
                          cprop,
                          gid,
                          cunit);

  return cid;
}

uint32_t vt_def_marker(uint32_t tid, const char* mname, uint32_t mtype)
{
  uint32_t mid;

  GET_THREAD_ID(tid);

  mid = curid++;

  VTGen_write_DEF_MARKER(VTTHRD_GEN(VTThrdv[tid]),
                         mid,
                         mname,
                         mtype);

  return mid;
}

void vt_def_gpu_comm(uint32_t grpc, uint32_t grpv[], const char *name,
                     uint32_t cid)
{
  uint32_t i;
  uint32_t tid;

  if(grpc > 0 && grpv != NULL){
    /* Process group will be written to first thread in given array*/
    tid = grpv[0];
  }else return;

  for(i = 0; i < grpc; i++){
    grpv[i] = VT_PROCESS_ID(vt_my_trace, grpv[i]);
  }

  VTGen_write_DEF_PROCESS_GROUP(VTTHRD_GEN(VTThrdv[tid]),
        cid+1, name, grpc, grpv);
}

uint32_t vt_def_mpi_comm(uint32_t tid, uint8_t ctype, uint32_t grpc,
                         uint8_t grpv[])
{
  uint32_t cid;

  uint32_t  cgrpc = 0;
  uint32_t* cgrpv = NULL;
  char      cname[20];

  GET_THREAD_ID(tid);

  cid = curid++;

  if( grpc > 0 )
  {
    uint32_t i;

    cgrpv = (uint32_t*)calloc(grpc * 8, sizeof(uint32_t));
    if( cgrpv == NULL )
      vt_error();

    for(i = 0; i < grpc; i++)
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
  }

  if(ctype == VT_MPI_COMM_WORLD)
    strncpy(cname, VT_UNIFY_STRID_MPI_COMM_WORLD_PROCGRP, sizeof(cname) - 1);
  else if(ctype == VT_MPI_COMM_SELF)
    strncpy(cname, VT_UNIFY_STRID_MPI_COMM_SELF_PROCGRP, sizeof(cname) - 1);
  else /* VT_MPI_COMM_OTHER */
    strncpy(cname, VT_UNIFY_STRID_MPI_COMM_OTHER_PROCGRP, sizeof(cname) - 1);

  VTGen_write_DEF_PROCESS_GROUP(VTTHRD_GEN(VTThrdv[tid]),
                                cid+1, cname, cgrpc, cgrpv);

  if(cgrpv)
    free(cgrpv);

  return cid;
}

uint32_t vt_def_user_comm(uint32_t tid, const char* cname)
{
   uint32_t  cid;
   char      tmp_char[128];

   GET_THREAD_ID(tid);

   cid = curid++;

   snprintf(tmp_char, sizeof(tmp_char) - 1,
            VT_UNIFY_STRID_USER_COMM_PROCGRP"%s", cname);

   VTGen_write_DEF_PROCESS_GROUP(VTTHRD_GEN(VTThrdv[tid]),
                                 cid+1, tmp_char, 0, NULL);

   return cid;
}

uint32_t vt_def_keyval(uint32_t tid, uint8_t vtype, const char* kname)
{
  uint32_t kid;

  GET_THREAD_ID(tid);

  kid = curid++;

  VTGen_write_DEF_KEYVAL(VTTHRD_GEN(VTThrdv[tid]),
                         kid,
                         vtype,
                         kname);

  return kid;
}

uint32_t vt_def_async_source(uint32_t tid, const char* sname)
{
  uint32_t kid;
  char kname[128];

  GET_THREAD_ID(tid);

  kid = curid++;

  snprintf(kname, sizeof(kname) - 1,
           VT_UNIFY_STRID_ASYNC_SOURCE_KEY"%s", sname);

  VTGen_write_DEF_KEYVAL(VTTHRD_GEN(VTThrdv[tid]),
                         kid,
                         VT_KEYVAL_TYPE_UINT64,
                         kname);

  return kid;
}

/*
 *-----------------------------------------------------------------------------
 * Event records
 *-----------------------------------------------------------------------------
 */

/* -- Region -- */

uint8_t vt_enter(uint32_t tid, uint64_t* time, uint32_t rid)
{
#if !defined(VT_DISABLE_RFG)
  RFG_RegionInfo* rinf;
#endif
  uint8_t do_trace;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_OFF_PERMANENT) return 0;

  VTTHRD_STACK_PUSH(VTThrdv[tid]);

  do_trace = ((VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON) &&
              (VTTHRD_STACK_LEVEL(VTThrdv[tid]) <= max_stack_depth));

#if !defined(VT_DISABLE_RFG)
  if( !RFG_Regions_stackPush(VTTHRD_RFGREGIONS(VTThrdv[tid]),
                             rid, do_trace, &rinf) )
  {
#   if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA) || defined(VT_CUDARTWRAP))
    RFG_RegionInfo* rinf_master;
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_LOCK_IDS();
#endif /* VT_MT || VT_HYB || V_JAVA */
    rinf_master = RFG_Regions_get(VTTHRD_RFGREGIONS(VTThrdv[0]), rid);
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB || V_JAVA */
    vt_assert(rinf_master != NULL);

    rinf = RFG_Regions_add(VTTHRD_RFGREGIONS(VTThrdv[tid]),
                           rinf_master->regionName, rid);

    /* copy master's call limit */
    rinf->callLimit = rinf_master->callLimit;

    /* initialize call limit count down */
    rinf->callLimitCD = rinf->callLimit;

    if (!RFG_Regions_stackPush(VTTHRD_RFGREGIONS(VTThrdv[tid]),
                               rid, do_trace, &rinf))
      vt_assert(0);
#   else /* VT_MT || VT_HYB || VT_JAVA || VT_CUDARTWRAP */
    vt_assert(0);
#   endif /* VT_MT || VT_HYB || VT_JAVA || VT_CUDARTWRAP */
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
      vt_marker_hint(tid, time, marktext);
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
    VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[tid]), time, rid, 0);

    vt_update_counter(tid, time);
  }

  return do_trace;
}

void vt_exit(uint32_t tid, uint64_t* time)
{
#if !defined(VT_DISABLE_RFG)
  RFG_RegionInfo* rinf;
  int climitbyenter;
#endif
  uint8_t do_trace;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) ==
      VT_TRACE_OFF_PERMANENT) return;

  VTTHRD_STACK_POP(VTThrdv[tid]);

  do_trace = ((VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON) &&
              (VTTHRD_STACK_LEVEL(VTThrdv[tid])+1 <= max_stack_depth));

#if !defined(VT_DISABLE_RFG)
  if (!RFG_Regions_stackPop(VTTHRD_RFGREGIONS(VTThrdv[tid]),
                            &rinf, &climitbyenter))
  {
    vt_assert(0);
  }

  if (climitbyenter == 0)
    do_trace = 0;
#endif /* VT_DISABLE_RFG */

  if (do_trace)
  {
    vt_update_counter(tid, time);

    VTGen_write_LEAVE(VTTHRD_GEN(VTThrdv[tid]), time, 0, 0);
  }
}

/* -- File I/O -- */

void vt_ioexit(uint32_t tid, uint64_t* time, uint64_t* etime, uint32_t fid,
               uint64_t hid, uint32_t op, uint64_t bytes)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_FILE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                             time,
                             etime,
                             fid,
                             hid,
                             op,
                             bytes,
                             0);

  vt_exit(tid, etime);
}

void vt_iobegin(uint32_t tid, uint64_t* time, uint64_t mid)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_BEGIN_FILE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                                   time,
                                   mid,
                                   0);
}

void vt_ioend(uint32_t tid, uint64_t* time, uint32_t fid, uint64_t mid,
              uint64_t hid, uint32_t op, uint64_t bytes)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_END_FILE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                                 time,
                                 fid,
                                 mid,
                                 hid,
                                 op,
                                 bytes,
                                 0);
}

/* -- Counter -- */

void vt_count(uint32_t tid, uint64_t* time, uint32_t cid, uint64_t cval)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_COUNTER(VTTHRD_GEN(VTThrdv[tid]),
                      time,
                      cid,
                      cval);
}

/* -- Comment -- */

void vt_comment(uint32_t tid, uint64_t* time, const char* fmt, ...)
{
  char comment[VT_MAX_COMMENT_LEN];
  va_list ap;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  va_start(ap, fmt);

  vsnprintf(comment, VT_MAX_COMMENT_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_COMMENT(VTTHRD_GEN(VTThrdv[tid]),
                      time,
                      comment);
}

/* -- Marker -- */

void vt_marker(uint32_t tid, uint64_t* time, uint32_t mid, const char* fmt, ...)
{
  char mtext[VT_MAX_MARKER_LEN];
  va_list ap;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  va_start(ap, fmt);

  vsnprintf(mtext, VT_MAX_MARKER_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_MARKER(VTTHRD_GEN(VTThrdv[tid]),
                     time,
                     mid,
                     mtext);
}

void vt_marker_error(uint32_t tid, uint64_t* time, const char* fmt, ...)
{
  char mtext[VT_MAX_MARKER_LEN];
  va_list ap;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  if (vt_trc_mid[VT__TRC_MARKER_ERROR] == VT_NO_ID)
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_LOCK_IDS();
    if (vt_trc_mid[VT__TRC_MARKER_ERROR] == VT_NO_ID)
    {
#endif /* VT_MT || VT_HYB || VT_JAVA */
    vt_trc_mid[VT__TRC_MARKER_ERROR] =
      vt_def_marker(VT_MASTER_THREAD, "VampirTrace", VT_MARKER_ERROR);
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    }
    VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB || VT_JAVA */
  }

  va_start(ap, fmt);

  vsnprintf(mtext, VT_MAX_MARKER_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_MARKER(VTTHRD_GEN(VTThrdv[tid]),
                     time,
                     vt_trc_mid[VT__TRC_MARKER_ERROR],
                     mtext);
}

void vt_marker_warning(uint32_t tid, uint64_t* time, const char* fmt, ...)
{
  char mtext[VT_MAX_MARKER_LEN];
  va_list ap;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  if (vt_trc_mid[VT__TRC_MARKER_WARNING] == VT_NO_ID)
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_LOCK_IDS();
    if (vt_trc_mid[VT__TRC_MARKER_WARNING] == VT_NO_ID)
    {
#endif /* VT_MT || VT_HYB || VT_JAVA */
    vt_trc_mid[VT__TRC_MARKER_WARNING] =
      vt_def_marker(VT_MASTER_THREAD, "VampirTrace", VT_MARKER_WARNING);
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    }
    VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB || VT_JAVA */
  }

  va_start(ap, fmt);

  vsnprintf(mtext, VT_MAX_MARKER_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_MARKER(VTTHRD_GEN(VTThrdv[tid]),
                     time,
                     vt_trc_mid[VT__TRC_MARKER_WARNING],
                     mtext);
}

void vt_marker_hint(uint32_t tid, uint64_t* time, const char* fmt, ...)
{
  char mtext[VT_MAX_MARKER_LEN];
  va_list ap;

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  if (vt_trc_mid[VT__TRC_MARKER_HINT] == VT_NO_ID)
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_LOCK_IDS();
    if (vt_trc_mid[VT__TRC_MARKER_HINT] == VT_NO_ID)
    {
#endif /* VT_MT || VT_HYB || VT_JAVA */
    vt_trc_mid[VT__TRC_MARKER_HINT] =
      vt_def_marker(VT_MASTER_THREAD, "VampirTrace", VT_MARKER_HINT);
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    }
    VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB || VT_JAVA */
  }

  va_start(ap, fmt);

  vsnprintf(mtext, VT_MAX_MARKER_LEN, fmt, ap);

  va_end(ap);

  VTGen_write_MARKER(VTTHRD_GEN(VTThrdv[tid]),
                     time,
                     vt_trc_mid[VT__TRC_MARKER_HINT],
                     mtext);
}

/* -- Key-Value -- */

void vt_keyval(uint32_t tid, uint32_t kid, uint8_t vtype, void* kvalue)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_KEYVAL(VTTHRD_GEN(VTThrdv[tid]),
                     kid,
                     vtype,
                     kvalue);
}

void vt_next_async_time(uint32_t tid, uint32_t kid, uint64_t atime)
{
  GET_THREAD_ID(tid)

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_KEYVAL(VTTHRD_GEN(VTThrdv[tid]),
                     kid,
                     VT_KEYVAL_TYPE_UINT64,
                     &atime);
}

/* -- MPI-1 -- */

void vt_mpi_send(uint32_t tid, uint64_t* time, uint32_t dpid, uint32_t cid,
                 uint32_t tag, uint32_t sent)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_SEND_MSG(VTTHRD_GEN(VTThrdv[tid]),
                       time,
                       dpid+1,
                       cid+1,
                       tag,
                       sent,
                       0);
}

void vt_mpi_recv(uint32_t tid, uint64_t* time, uint32_t spid, uint32_t cid,
                 uint32_t tag, uint32_t recvd)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_RECV_MSG(VTTHRD_GEN(VTThrdv[tid]),
                       time,
                       spid+1,
                       cid+1,
                       tag,
                       recvd,
                       0);
}

void vt_mpi_collexit(uint32_t tid, uint64_t* time, uint64_t* etime,
                     uint32_t rid, uint32_t rpid, uint32_t cid, void* comm,
                     uint32_t sent, uint32_t recvd)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON)
  {
    VTGen_write_COLLECTIVE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                                     time,
                                     etime,
                                     rid,
                                     cid+1,
                                     rpid != VT_NO_ID ? rpid+1 : 0,
                                     sent,
                                     recvd,
                                     0);

    vt_exit(tid, etime);
  }

  /* intermediate time sync. or buffer flush, if necessary */
  if (vt_num_traces > 1)
    vt_mpi_sync(tid, etime, comm);
}

void vt_mpi_collbegin(uint32_t tid, uint64_t* time, uint32_t rid, uint64_t mid,
                      uint32_t rpid, uint32_t cid, uint64_t sent,
                      uint64_t recvd)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_BEGIN_COLLECTIVE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                                         time,
                                         rid,
                                         mid,
                                         rpid != VT_NO_ID ? rpid+1 : 0,
                                         cid+1,
                                         sent,
                                         recvd,
                                         0);
}

void vt_mpi_collend(uint32_t tid, uint64_t* time, uint64_t mid, void* comm)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) == VT_TRACE_ON)
  {
    VTGen_write_END_COLLECTIVE_OPERATION(VTTHRD_GEN(VTThrdv[tid]),
                                         time,
                                         mid);
  }

  /* intermediate time sync. or buffer flush, if necessary */
  if (vt_num_traces > 1)
    vt_mpi_sync(tid, time, comm);
}

/* -- MPI2 - 1sided -- */

void vt_mpi_rma_put(uint32_t tid, uint64_t* time, uint32_t tpid, uint32_t cid,
                    uint32_t tag, uint64_t sent)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_RMA_PUT(VTTHRD_GEN(VTThrdv[tid]),
                      time,
                      VT_PROCESS_ID(vt_my_trace, tid),
                      tpid+1,
                      cid+1,
                      tag,
                      sent,
                      0);
}

void vt_mpi_rma_putre(uint32_t tid, uint64_t* time, uint32_t tpid, uint32_t cid,
                      uint32_t tag, uint64_t sent)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_RMA_PUTRE(VTTHRD_GEN(VTThrdv[tid]),
                        time,
                        0,
                        tpid+1,
                        cid+1,
                        tag,
                        sent,
                        0);
}

void vt_mpi_rma_get(uint32_t tid, uint64_t* time, uint32_t tpid, uint32_t cid,
                    uint32_t tag, uint64_t recvd)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_RMA_GET(VTTHRD_GEN(VTThrdv[tid]),
                      time,
                      0,
                      tpid+1,
                      cid+1,
                      tag,
                      recvd,
                      0);
}

void vt_mpi_rma_end(uint32_t tid, uint64_t* time, uint32_t cid, uint32_t tag)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_RMA_END(VTTHRD_GEN(VTThrdv[tid]),
                      time,
                      0,
                      cid+1,
                      tag,
                      0);
}

/* -- OpenMP -- */

void vt_omp_fork(uint32_t tid)
{
  (void)tid;
}

void vt_omp_fork2(uint32_t tid, uint32_t* ptid)
{
  GET_THREAD_ID(tid);

  *ptid = tid;

  vt_omp_fork(tid);
}

void vt_omp_join(uint32_t tid)
{
  (void)tid;
}

void vt_omp_parallel_begin(uint32_t tid)
{
  GET_THREAD_ID(tid);
#if defined(VT_METR)
  if ( tid > 0 && vt_metric_num() > 0 && !VTTHRD_METV(VTThrdv[tid]) )
  {
    /* create metrics in worker threads */
    VTThrdv[tid]->metv = vt_metric_create();
    vt_cntl_msg(2, "Recreated metrics in thread #%d", tid);
  }
#endif /* VT_METR */

#if defined(VT_PLUGIN_CNTR)
  /* if we really use plugins and this thread also uses some;
     re-enable plugin metrics */
  if (vt_plugin_cntr_used && VTTHRD_PLUGIN_CNTR_DEFINES(VTThrdv[tid]))
    vt_plugin_cntr_thread_enable_counters(VTThrdv[tid]);
#endif /* VT_PLUGIN_CNTR */
}

void vt_omp_parallel_begin2(uint32_t tid, uint32_t ptid)
{
  GET_THREAD_ID(tid);
#if (defined(VT_MT) || defined(VT_HYB))
  VTThrd_registerThread(ptid);
#endif /* VT_MT || VT_HYB */
  vt_omp_parallel_begin(tid);
}

void vt_omp_parallel_end(uint32_t tid)
{
  GET_THREAD_ID(tid);

#if defined(VT_METR)
  if ( tid > 0 && vt_metric_num() > 0 && VTTHRD_METV(VTThrdv[tid]) ) {
    /* shut down metrics in worker threads */
    vt_metric_free(VTTHRD_METV(VTThrdv[tid]), tid);
    VTTHRD_METV(VTThrdv[tid]) = NULL;
    vt_metric_thread_fini();
    vt_cntl_msg(2, "Shut down metrics in thread #%d", tid);

    /* store last metric values */
    if ( VTTHRD_OFFV(VTThrdv[tid]) && VTTHRD_VALV(VTThrdv[tid]) )
    {
      memcpy(VTTHRD_OFFV(VTThrdv[tid]), VTTHRD_VALV(VTThrdv[tid]),
             vt_metric_num() * sizeof(uint64_t));
    }
  }
#endif /* VT_METR */

#if defined(VT_PLUGIN_CNTR)
  /* if we really use plugins and this thread also uses some;
     disable plugin counting */
  if (vt_plugin_cntr_used && VTTHRD_PLUGIN_CNTR_DEFINES(VTThrdv[tid]))
    vt_plugin_cntr_thread_disable_counters(VTThrdv[tid]);
#endif /* VT_PLUGIN_CNTR */
}

/* -- User Point-to-Point Communication -- */

void vt_user_send(uint32_t tid, uint64_t* time, uint32_t cid, uint32_t tag,
                  uint32_t sent)
{
  char comid_comment[128];

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  snprintf(comid_comment, sizeof(comid_comment) - 1,
           VT_UNIFY_STRID_USRCOM_SEND_COMMENT"C%xT%x", cid+1, tag);

  VTGen_write_DEFINITION_COMMENT(VTTHRD_GEN(VTThrdv[tid]),
                                 comid_comment);

  VTGen_write_SEND_MSG(VTTHRD_GEN(VTThrdv[tid]),
                       time,
                       1,
                       cid+1,
                       tag,
                       sent,
                       0);
}

void vt_user_recv(uint32_t tid, uint64_t* time, uint32_t cid, uint32_t tag,
                  uint32_t recvd)
{
  char comid_comment[128];

  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  snprintf(comid_comment, sizeof(comid_comment) - 1,
           VT_UNIFY_STRID_USRCOM_RECV_COMMENT"C%xT%x", cid+1, tag);

  VTGen_write_DEFINITION_COMMENT(VTTHRD_GEN(VTThrdv[tid]),
                                 comid_comment);

  VTGen_write_RECV_MSG(VTTHRD_GEN(VTThrdv[tid]),
                       time,
                       1,
                       cid+1,
                       tag,
                       recvd,
                       0);
}

/* -- VampirTrace Internal -- */

void vt_enter_user(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  vt_enter(tid, time, vt_trc_regid[VT__TRC_USER]);
}

void vt_exit_user(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  vt_exit(tid, time);
}

void vt_enter_stat(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[tid]), time,
                    vt_trc_regid[VT__TRC_STAT], 0);

  vt_update_counter(tid, time);
}

void vt_exit_stat(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  vt_update_counter(tid, time);

  VTGen_write_LEAVE(VTTHRD_GEN(VTThrdv[tid]), time, 0, 0);
}

void vt_enter_flush(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_ENTER_FLUSH(VTTHRD_GEN(VTThrdv[tid]), time);
}

void vt_exit_flush(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_LEAVE_FLUSH(VTTHRD_GEN(VTThrdv[tid]), time);
}

void vt_enter_rewind(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTGen_write_ENTER(VTTHRD_GEN(VTThrdv[tid]), time,
                    vt_trc_regid[VT__TRC_REWIND], 0);
}

void vt_exit_rewind(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  vt_update_counter(tid, time);

  VTGen_write_LEAVE(VTTHRD_GEN(VTThrdv[tid]), time, 0, 0);
}

/* -- Rewind -- */

void vt_set_rewind_mark(uint32_t tid, uint64_t* time)
{
  GET_THREAD_ID(tid);

  if (VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON) return;

  VTTHRD_STACK_LEVEL_AT_REWIND_MARK(VTThrdv[tid]) =
    VTTHRD_STACK_LEVEL(VTThrdv[tid]);

  vt_update_counter(tid, time);
  VTGen_set_rewind_mark(VTTHRD_GEN(VTThrdv[tid]), time);

  vt_cntl_msg(2, "Rewind mark set");
}

void vt_rewind(uint32_t tid, uint64_t* time)
{
  uint64_t marktime;

  GET_THREAD_ID(tid);

  if ( VTTHRD_TRACE_STATUS(VTThrdv[tid]) != VT_TRACE_ON ) return;

  /* check if rewind mark is present */
  if ( VTGen_is_rewind_mark_present(VTTHRD_GEN(VTThrdv[tid])) )
  {
    /* check if current call stack level is equal to call level
       at rewind mark */
    if ( VTTHRD_STACK_LEVEL(VTThrdv[tid]) ==
         VTTHRD_STACK_LEVEL_AT_REWIND_MARK(VTThrdv[tid]) )
    {
      VTGen_rewind(VTTHRD_GEN(VTThrdv[tid]), &marktime);

      /* mark rewind time interval */
      vt_enter_rewind(tid, &marktime);
      vt_exit_rewind(tid, time);

      vt_cntl_msg(2, "Buffer rewind");
    }
    /* otherwise: abort */
    else
    {
      vt_error_msg("Could not rewind.\n"
                   "The current call stack level (%i) isn't equal to the "
                   "stored one (%i) at the rewind mark.",
                   VTTHRD_STACK_LEVEL(VTThrdv[tid]),
                   VTTHRD_STACK_LEVEL_AT_REWIND_MARK(VTThrdv[tid]) );
    }
  }
  /* otherwise: warning */
  else
  {
    vt_warning("Could not rewind, no mark present");
  }
}
