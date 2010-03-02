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

#include "vt_thrd.h"
#include "vt_trc.h"
#include "vt_otf_gen.h"
#include "vt_env.h"
#include "vt_iowrap.h"
#include "vt_mpireg.h"
#include "vt_omplock.h"
#include "vt_memhook.h"
#include "vt_memreg.h"
#include "vt_metric.h"
#include "vt_pform.h"
#include "vt_error.h"

#include "util/installdirs.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "otf.h"

#if (defined (VT_MPI) || defined (VT_OMPI))
#include "mpi.h"
#include "vt_sync.h"
#endif
#if (defined (VT_OMPI) || defined (VT_OMP))
#include <omp.h>
#define VT_MY_THREAD   omp_get_thread_num() 
#define VT_NUM_THREADS omp_get_num_threads() 
extern void POMP_Init(void);
#else 
#define VT_MY_THREAD   0
#define VT_NUM_THREADS 1 
#endif

/*
 *-----------------------------------------------------------------------------
 * Two simple hash tables 1: maps region groups to region group identifier
 *                        2: maps file name to file identifier 
 *-----------------------------------------------------------------------------
 */

#define HASH_TAB__RDESC  0
#define HASH_TAB__FILE   1
#define HASH_MAX 1021

typedef struct HN_rdesc {
  const char*       rdesc;  /* region group name */
  uint32_t          rdid;   /* associated region group identifier */
  struct HN_rdesc*  next;
} HashNode_rdesc;

typedef struct HN_file {
  const char*       fname;  /* file name */
  uint32_t          fid;    /* associated file identifier */
  struct HN_file*   next;
} HashNode_file;

static HashNode_rdesc*  htab_rdesc[HASH_MAX];
static HashNode_file*   htab_file[HASH_MAX];

/*
 * Generates hash code for region group name 'n'
 * Returs hash code
 */

static long hash_get_key(const char* n) {
  long h = 0;
  const char* p = n;
  while( *p ) h = h<<1 ^ *p++;
  if( h < 0 ) h = -h;
  return h;
}

static void hash_put(int t, const char* n, int i) {
  long id = hash_get_key(n) % HASH_MAX;
  if(t==HASH_TAB__RDESC)
  {
    HashNode_rdesc *add = (HashNode_rdesc*)malloc(sizeof(HashNode_rdesc));
    add->rdesc = strdup(n);
    add->rdid = i;
    add->next = htab_rdesc[id];
    htab_rdesc[id] = add;
  }
  else if(t==HASH_TAB__FILE)
  {
    HashNode_file *add = (HashNode_file*)malloc(sizeof(HashNode_file));
    add->fname = strdup(n);
    add->fid = i;
    add->next = htab_file[id];
    htab_file[id] = add; 
  }
}

static void* hash_get(int t, const char* n) {
  long id = hash_get_key(n) % HASH_MAX;
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
  else if(t==HASH_TAB__FILE)
  {
    HashNode_file *curr = htab_file[id];
    while ( curr ) {
      if ( strcmp( curr->fname, n ) == 0 ) {
        return curr;
      }
      curr = curr->next;
    }
  }
  return NULL;
}

/* compiler adapter finalizer */
void (*vt_comp_finalize)(void) = NULL;

/* vector of the thread objects */
static VTThrd** thrdv;

#if (defined (VT_MPI) || defined (VT_OMPI))
static long      my_node;
static int       num_nodes  =  1;
#endif
static int       my_trace   =  0;
static int       num_traces =  1;
static int       init_pid   = -1;
static int64_t   my_ltime[2]  = { 0, 1 };
static int64_t   my_offset[2] = { 0, 0 };

int vt_trc_regid[VT__REGID_NUM];

uint8_t vt_is_alive = 0;

#if (defined (VT_MEMHOOK))
  /* id of memory usage counter */
  static uint32_t mem_app_alloc_cid;
#endif

#if (defined (VT_METR))
  /* number of performance metrics */
  static int num_metrics = 0;
#endif

static uint8_t vt_open_called = 0;
static uint8_t vt_close_called = 0;

/* id counter starting with 1 */
static uint32_t  curid  = 1;

static uint8_t  thrdgrp_created = 0;

static uint32_t  thrdgrp_cid = 10000;

static uint32_t  ompcollop_id = 0;

static uint32_t  vt_def_file_loc(uint32_t fid,
				 uint32_t begln,
				 uint32_t endln);

static uint32_t  vt_def_region_desc(const char* rdesc);

static uint32_t  vt_def_omp_comm(void);

static void vt_write_uctl_file(void);

static void vt_cpy_to_gdir(uint32_t tid);

#if (defined (RFG))
static void vt_write_filt_file(void);
#endif

void vt_check_thrd_id(uint32_t tid);

void vt_open()
{
  int i_am_the_initer = 0;

  /* check for double initialization error */
#if (defined (VT_OMPI) || defined (VT_OMP)) 
# pragma omp critical
  {
#endif
  if ( !vt_open_called ) {
    vt_open_called = 1;
    i_am_the_initer = 1;
  }
#if (defined (VT_OMPI) || defined (VT_OMP)) 
  }
#endif

  if (vt_open_called && !i_am_the_initer)
    return;

 /* initialization specific to this platform */
  vt_pform_init();

#if (defined (VT_METR))

  /* initialize hardware counters */
  num_metrics = vt_metric_open();

#if (defined (VT_OMPI) || defined (VT_OMP)) 
  /* initialize thread support */
  vt_metric_thread_init(omp_get_thread_num);
#endif

#endif

  /* trace file creation */

  thrdv = (VTThrd**)calloc(vt_env_max_threads(), sizeof(VTThrd*));
  if ( thrdv == NULL )
    vt_error();
  
#if (defined (VT_OMPI) || defined (VT_OMP)) 
#pragma omp parallel
  {
#endif
    thrdv[VT_MY_THREAD] = VTThrd_create(VT_MY_THREAD);
    VTThrd_open(thrdv[VT_MY_THREAD], VT_MY_THREAD);
#if (defined (VT_OMPI) || defined (VT_OMP)) 
  }
#endif

#if (defined (RFG))
  {
    char* filter_deffile = vt_env_filter_spec();
    char* groups_deffile = vt_env_groups_spec();

    /* set default group name */
    RFG_Regions_setDefaultGroup( thrdv[0]->rfg_regions, VT_DEF_GROUP );

    if( filter_deffile )
    {
      RFG_Regions_setFilterDefFile( thrdv[0]->rfg_regions, filter_deffile );
      if( !RFG_Regions_readFilterDefFile( thrdv[0]->rfg_regions ) )
	vt_error_msg("Could not read region filter specification file ");
    }

    if( groups_deffile )
    {
      RFG_Regions_setGroupsDefFile( thrdv[0]->rfg_regions, groups_deffile );
      if( !RFG_Regions_readGroupsDefFile( thrdv[0]->rfg_regions ) )
	vt_error_msg("Could not read region group specification file ");
    }
  }
#endif

  /* register function "user" */
  vt_trc_regid[VT__USER] =
    vt_def_region("user", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
		  VT_DEF_GROUP, VT_FUNCTION);

  /* register function "sync" */
  vt_trc_regid[VT__SYNC] =
    vt_def_region("sync", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
		  "VT_API", VT_FUNCTION);

  /* register function "flush" */
  vt_trc_regid[VT__FLUSH] =
    vt_def_region("flush", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
		  "VT_API", VT_FUNCTION);

  /* register function "stat" */
  vt_trc_regid[VT__STAT] =
    vt_def_region("stat", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
		  "VT_API", VT_FUNCTION);

#if (defined (VT_OMPI) || defined(VT_OMP))
  /* register function "PREG" */
  vt_trc_regid[VT__PREG] =
    vt_def_region("parallel region", VT_NO_ID, VT_NO_LNO, VT_NO_LNO,
		  "PREG", VT_FUNCTION);
#endif

#if (defined (VT_IOWRAP))
  if (vt_env_iotrace())
  {
    vt_iowrap_init();
    VT_ENABLE_IO_TRACING();
  }
#endif

#if (defined (VT_MEMHOOK))

  /* write group name for memory counters */
  if (vt_env_memtrace())
  {
    uint32_t gid;

    /* GNU C MALLOC HOOKS ARE NOT THREAD-SAFE!!! */
#   if (defined (VT_OMPI) || defined (VT_OMP)) 
      vt_error_msg("Memory tracing by GNU C malloc-hooks for threaded application not yet supported");
#   endif
      
      /* write counter group name */
      gid = vt_def_counter_group("Memory");

      /* initalize memory hooks */
      vt_memhook_init();
      
      /* register memory routines */
      vt_mem_register();

      /* write counter definition record for allocated memory */
      mem_app_alloc_cid =
	vt_def_counter("MEM_APP_ALLOC",
		       OTF_COUNTER_TYPE_ABS|OTF_COUNTER_SCOPE_NEXT,
		       gid,
		       "Bytes");
  }

#endif


  /* register MPI and OpenMP API routines if necessary */
     
#if (defined (VT_OMPI) || defined (VT_MPI))
  vt_mpi_register();
#endif

#if (defined (VT_OMPI) || defined (VT_OMP))
  POMP_Init();
#endif
  atexit(vt_close);
  init_pid = getpid();


#if (defined (VT_METR))
  {
    uint32_t gid;
    int i;

    /* return if no counters requested */
    if ( num_metrics > 0 )
    {
      /* write counter group name */
      gid = vt_def_counter_group("Hardware");

      /* write counter definition records */
      for ( i = 0; i < num_metrics; i++ )
      {
	VTGen_write_DEF_COUNTER(VTTHRD_GEN(thrdv[0]),
				i+1,
				vt_metric_name(i),
				OTF_COUNTER_TYPE_ACC,
				gid,
				"#");
      }
    }
  }
#endif

  vt_is_alive = 1;

  return;
}

void vt_close()
{  
  int i, nf;
  int i_am_the_closer = 0;

  /* catch vt_close called from child processes through atexit */
  if ( init_pid != getpid() ) return;

  /* check for double finalization error */
#if (defined (VT_OMPI) || defined (VT_OMP)) 
# pragma omp critical
  {
#endif
  if ( !vt_close_called ) {
    vt_close_called = 1;
    i_am_the_closer = 1;
  }
#if (defined (VT_OMPI) || defined (VT_OMP)) 
  }
#endif

  if (vt_close_called && !i_am_the_closer)
    return;

  vt_is_alive = 0;

#if (defined (VT_MEMHOOK))

  /* finalize memory hooks if enabled */
  if (vt_env_memtrace())
    vt_memhook_finalize();

#endif

#if (defined (VT_IOWRAP))

  /* finalize I/O wrapper if enabled */
  if (vt_env_iotrace())
  {
    VT_DISABLE_IO_TRACING();
    vt_iowrap_finalize();
  }
#endif

  /* finalize compiler adapter */
  if (vt_comp_finalize)
    vt_comp_finalize();

  /* close trace files */
  for (i = 0; i < (int)VTThrd_get_num_thrds(); i++)
    VTThrd_close(thrdv[i]);

  /* call cleanup functions */

#if (defined (VT_OMPI) || defined (VT_OMP)) 
  /* OpenMP locks */
  vt_lock_close();
#endif

  /* hardware counters */
#if (defined (VT_METR))
  if ( num_metrics > 0 )
    vt_metric_close();
#endif

  /* copy per-process trace from local directory to 
     global directory */
  for(i=0; i<(int)VTThrd_get_num_thrds(); i++)
    vt_cpy_to_gdir(i);

  /* write unify control file */
  vt_write_uctl_file();

#if (defined (RFG))
  /* write list of regions whose call limit are reached */
  vt_write_filt_file();
#endif

  /*- Rank 0: unify trace files -*/
  if (my_trace == 0 && vt_env_do_unify())
    {
      char* vtunify;
      char* filename;
      char* cmd;
      int len;

      vtunify = vt_installdirs_expand("${bindir}/vtunify");
      if ( vtunify == NULL )
	vt_error();

      if ( access(vtunify, X_OK) == -1 )
	vt_error_msg("Cannot execute %s", vtunify);

      len = strlen(vt_env_gdir()) + strlen(vt_env_fprefix()) + 32;
      filename = (char*)calloc(len, sizeof(char));
      if ( filename == NULL )
	vt_error();

      /*- wait for files to be ready -*/
      for (i = 0; i < num_traces; i++)
        {
          sprintf(filename, "%s/%s.%x.uctl", vt_env_gdir(),
		  vt_env_fprefix(), i+1);
          vt_cntl_msg("Checking for %s ...", filename);
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
	          vtunify, num_traces,
	          vt_env_gdir(), vt_env_fprefix(),
		  vt_env_stat_show() ? "" : "-q",
		  vt_env_compression() ? "" : "-c",
		  vt_env_do_clean() ? "" : "-k",
		  vt_env_is_verbose() ? "-v" : "");

      vt_cntl_msg(cmd);
      system(cmd);

      free(vtunify);
      free(filename);
      free(cmd);
    }

  /* free temporary file names. This has to be done inside a parallel 
     region because vt_metric_free() needs to be called by the
     thread itself. */

#if (defined (VT_OMPI) || defined (VT_OMP)) 
  omp_set_num_threads(VTThrd_get_num_thrds());
#pragma omp parallel
  {
#endif
    VTThrd_delete(thrdv[VT_MY_THREAD], VT_MY_THREAD);
#if (defined (VT_OMPI) || defined (VT_OMP)) 
  }
#endif
  
  free(thrdv);
}

void vt_trace_on()
{
  vt_check_thrd_id(VT_MY_THREAD);

  if ( vt_is_alive && VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) == 0 )
    VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) = 1;  
}

void vt_trace_off(uint8_t permanent)
{
  vt_check_thrd_id(VT_MY_THREAD);

  if ( vt_is_alive && VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) != -1 )
  {
    if ( permanent )
    {
      uint64_t time;
      while(VTTHRD_STACK_LEVEL(thrdv[VT_MY_THREAD]) > 0) {
	time = vt_pform_wtime();
	vt_exit(&time);
      }
      VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) = -1;
    }
    else
    {
      VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) = 0;
    }
  }
}

uint8_t vt_is_trace_on()
{
  vt_check_thrd_id(VT_MY_THREAD);

  return ( vt_is_alive ) ? VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) : 0;
}

#if (defined (VT_MPI) || defined (VT_OMPI))
static int longcmp(const void *a, const void *b)
{
  long x = *(long*)a;
  long y = *(long*)b;
  return x-y;
}
#endif

void vt_mpi_init()
{
#if (defined (VT_MPI) || defined (VT_OMPI))

  int i;

  PMPI_Comm_rank(MPI_COMM_WORLD, &my_trace);
  PMPI_Comm_size(MPI_COMM_WORLD, &num_traces);
  
  vt_error_pid(my_trace);

  /* notify all threads about trace id */
  for(i=0; i<(int)VTThrd_get_num_thrds(); i++)
    VTGen_init_trc_id(VTTHRD_GEN(thrdv[i]), my_trace);

  /* 1. clock synchronization if necessary*/
  if (num_traces > 1)
  {
#if DISABLE_CLOCK_SYNC == 0
    {
      uint64_t time;

      /* mark begin of clock synchronization */
      time = vt_pform_wtime();
      vt_enter(&time, vt_trc_regid[VT__SYNC]);

      /* measure offset */
      my_offset[0] = vt_offset(&my_ltime[0], MPI_COMM_WORLD);

      /* mark end of clock synchronization */
      time = vt_pform_wtime();
      vt_exit(&time);
    }
#endif
  }

#endif

  atexit(vt_close); /* re-register to be called on exit before MPI's atexit */
}

void vt_mpi_finalize()
{
#if (defined (VT_MPI) || defined (VT_OMPI))

  long* nodeids = NULL;
  long lastid;
  int i;

  /* 2. clock synchronization if necessary */ 
  if (num_traces > 1)
  {
#if DISABLE_CLOCK_SYNC == 0
    {
      uint64_t time;

      /* mark begin of clock synchronization */
      time = vt_pform_wtime();
      vt_enter(&time, vt_trc_regid[VT__SYNC]);

      /* measure offset */
      my_offset[1] = vt_offset(&my_ltime[1], MPI_COMM_WORLD);

      /* mark end of clock synchronization */
      time = vt_pform_wtime();
      vt_exit(&time);
    }
#endif
  }

  /* determine number of nodes */
  if (my_trace == 0)
    {
      nodeids = (long*)malloc(num_traces * sizeof(long));
      if ( nodeids == NULL )
	vt_error();
    }
  PMPI_Gather(&my_node, 1, MPI_LONG, nodeids, 1, MPI_LONG, 0, MPI_COMM_WORLD);
  if (my_trace == 0)
    {
      qsort(nodeids, num_traces, sizeof(long), longcmp);
      lastid=nodeids[0];
      for (i=1; i<num_traces; ++i)
        {
          if ( nodeids[i] != lastid )
	    {
              lastid = nodeids[i];
              num_nodes++;
            }
        }
	free(nodeids);
    }
    PMPI_Barrier(MPI_COMM_WORLD);
#endif
}

void vt_init_trc_id(int my_id, int num_procs)
{
  int i;

  my_trace   = my_id;
  num_traces = num_procs;

  /* notify all threads about trace id */
  for(i=0; i<(int)VTThrd_get_num_thrds(); i++)
    VTGen_init_trc_id(VTTHRD_GEN(thrdv[i]), my_trace);
}

/* This routine checks if the thread object for the current 
 * thread ID has already been created or not. If not it creates 
 * the corresponding thread object
 */
void vt_check_thrd_id(uint32_t tid)
{
#if (defined (VT_OMPI) || defined (VT_OMP))

  if( !vt_is_alive ) return;

  if( VTThrd_get_num_thrds() < (tid+1) )
    {
      vt_cntl_msg("Dynamic thread creation. Thread #%d\n", VT_MY_THREAD);
      
      thrdv[VT_MY_THREAD] = VTThrd_create(VT_MY_THREAD);
      VTThrd_open(thrdv[VT_MY_THREAD], VT_MY_THREAD);
      VTGen_init_trc_id(VTTHRD_GEN(thrdv[VT_MY_THREAD]), my_trace);
    }

#endif
}

static void vt_cpy_to_gdir(uint32_t tid)
{
  static const size_t buffer_size = 0x2000000;
  char* buffer;

  char* global_dir = vt_env_gdir();
  char* file_prefix = vt_env_fprefix();

  char  global_name[1024];
  char* local_name;
  char* local_prefix;
  char* suffix;

  FILE* infile;
  FILE* outfile;

  uint8_t i;

  buffer = (char*)calloc(buffer_size, sizeof(char));
  if ( buffer == NULL )
    vt_error();

  local_prefix = VTGen_get_name(thrdv[tid]->gen);

  for(i = 0; i < 3; i++)
  {   
    uint64_t bytes_read;

    /* get local file name */
    if(i == 0)
      local_name = VTGen_get_defname(thrdv[tid]->gen);
    else if( i == 1 )
      local_name = VTGen_get_eventname(thrdv[tid]->gen);
    else
      local_name = VTGen_get_statname(thrdv[tid]->gen);

    if ( local_name == NULL )
      vt_error();

    /* determine file suffix */
    suffix = strchr(local_name+strlen(local_prefix)+1, '.');
    if ( suffix == NULL )
      vt_error();

    /* build global file name */
    snprintf(global_name, sizeof(global_name) - 1, "%s/%s.%x%s",
	     global_dir, file_prefix,
	     65536*tid+(my_trace+1), suffix);
      
    infile = fopen(local_name, "rb");
    if ( infile == NULL )
    {
      free(local_name);
      continue;
    }

    outfile = fopen(global_name, "wb");
    if ( outfile == NULL )
      vt_error_msg("Cannot open file %s for writing", global_name);

    /* copy file */
    while((bytes_read = fread(buffer, sizeof(char), buffer_size, infile)))
      fwrite(buffer, sizeof(char), bytes_read, outfile);

    fclose(infile);
    fclose(outfile);

    free(local_name);
  }

  free(buffer);
}

#if (defined(RFG))

static void vt_write_filt_file()
{
  int i, j;

  for(i=0; i<(int)VTThrd_get_num_thrds(); i++)
  {
    uint32_t nrinfs = 0;
    RFG_RegionInfo** rinfs = NULL;

    /* get regions, whose call limit are reached */
    RFG_Regions_getFilteredRegions(VTTHRD_RFGREGIONS(thrdv[i]),
				   &nrinfs, &rinfs);

    if(nrinfs > 0)
    {
      char  filename[300];
      FILE* filt_file;
  
      snprintf(filename, sizeof(filename) - 1, "%s/%s.%x.filt",
	       vt_env_gdir(), vt_env_fprefix(), 65536*i+(my_trace+1));

      filt_file = fopen(filename, "w");

      if(filt_file == NULL)
	vt_error_msg("Cannot open file %s", filename);

      fprintf(filt_file, "# list of regions, which are denied or whose call limit are reached\n");
      fprintf(filt_file, "# (region:limit)\n");

      /* write region names and call limits */

      for(j=0; j<(int)nrinfs; j++)
      {
	fprintf(filt_file, "%s:%i\n",
		rinfs[j]->regionName,
		rinfs[j]->callLimit == 0 ? 0 : rinfs[j]->callLimit-1);
      }

      fclose(filt_file);

      vt_cntl_msg("Wrote list of filtered regions to file %s", filename);

      free(rinfs);
    }
  }
}

#endif

/*
 *-----------------------------------------------------------------------------
 * Defining source code entities
 *-----------------------------------------------------------------------------
 */

static uint32_t vt_def_file_loc(uint32_t fid,
				uint32_t begln,
				uint32_t endln)
{
  uint32_t sid;

  vt_check_thrd_id(VT_MY_THREAD);

  if( fid == VT_NO_ID || begln == VT_NO_LNO )
    return 0;

  sid = curid++;

  VTGen_write_DEF_SCL(VTTHRD_GEN(thrdv[VT_MY_THREAD]), sid, fid, begln);
  return sid;
}

static uint32_t vt_def_region_desc(const char* rdesc)
{
  uint32_t rdid;

  HashNode_rdesc* hn;

  vt_check_thrd_id(VT_MY_THREAD);

  hn = hash_get(HASH_TAB__RDESC, rdesc);

  if(hn == NULL)
  {     
    rdid = curid++;

    VTGen_write_DEF_FUNCTION_GROUP(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
				    rdid, rdesc);

    hash_put(HASH_TAB__RDESC, rdesc, rdid);
  }
  else
  {
    rdid = hn->rdid;
  }

  return rdid;
}

static uint32_t vt_def_omp_comm()
{
  uint32_t cid;

  vt_check_thrd_id(VT_MY_THREAD);

  cid = thrdgrp_cid;

  if(VT_MY_THREAD == 0 && thrdgrp_created == 0)
  {
    uint32_t  cgrpc;
    uint32_t* cgrpv;
    int i;

    cgrpc = VT_NUM_THREADS;

    cgrpv = (uint32_t*)calloc(cgrpc * 8, sizeof(uint32_t));
    if( cgrpv == NULL )
      vt_error();

    for(i = 0; i < (int)cgrpc; i++)
      cgrpv[i] = 65536*i+(my_trace+1);

    VTGen_write_DEF_PROCESS_GROUP(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
				   cid+1,
				   "__OMP_TEAM__",
				   cgrpc, cgrpv);

    free(cgrpv);

    thrdgrp_created = 1;
  }

  return cid;
}

static void vt_write_uctl_file()
{
  int   i;
  char  filename[300];
  FILE* uctl_file;
  
  snprintf(filename, sizeof(filename) - 1, "%s/%s.%x.uctl",
	   vt_env_gdir(), vt_env_fprefix(), my_trace+1);

  uctl_file = fopen(filename, "w");

  if(uctl_file == NULL)
    vt_error_msg("Cannot open file %s", filename);

  /* write namestubs */

  for(i=0; i<(int)VTThrd_get_num_thrds(); i++)
  {
    fprintf(uctl_file, "%s%u",
	    i>0 ? ":" : "", 65536*i+(my_trace+1));
  }
  fprintf(uctl_file, "\n");

  /* write time offset */
  fprintf(uctl_file, "%lli:%lli:%lli:%lli\n", 
	  (long long int)my_ltime[0],
	  (long long int)my_offset[0],
	  (long long int)my_ltime[1],
	  (long long int)my_offset[1]);

  fclose(uctl_file);

  vt_cntl_msg("Wrote unify control file %s", filename);
}


void vt_def_comment(const char* comment)
{
  vt_check_thrd_id(VT_MY_THREAD);

  VTGen_write_DEFINITION_COMMENT(VTTHRD_GEN(thrdv[VT_MY_THREAD]), comment);
}

uint32_t vt_def_file(const char* fname)
{
  uint32_t fid;

  HashNode_file* hn;

  vt_check_thrd_id(VT_MY_THREAD);

  hn = hash_get(HASH_TAB__FILE, fname);

  if( hn == NULL )
  {
    fid = curid++;

    VTGen_write_DEF_SCL_FILE(VTTHRD_GEN(thrdv[VT_MY_THREAD]), fid, fname);

    hash_put(HASH_TAB__FILE, fname, fid);
  }
  else
  {
    fid = hn->fid;
  }

  return fid;
}

uint32_t vt_def_fileio_group(const char* gname)
{
  uint32_t gid;

  vt_check_thrd_id(VT_MY_THREAD);

  gid = curid++;

  VTGen_write_DEF_FILE_GROUP(VTTHRD_GEN(thrdv[VT_MY_THREAD]), gid, gname);

  return gid;
}

uint32_t vt_def_fileio(const char* fname, uint32_t gid)
{
  uint32_t fid;

  vt_check_thrd_id(VT_MY_THREAD);

  fid = curid++;

  VTGen_write_DEF_FILE(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		       fid,
		       fname,
		       gid);

  return fid;
}

uint32_t vt_def_region(const char* rname,
		       uint32_t fid,
		       uint32_t begln,
		       uint32_t endln,
		       const char* rdesc,
		       uint8_t rtype)
{
  uint32_t sid;
  uint32_t rid;
  uint32_t rdid;

  vt_check_thrd_id(VT_MY_THREAD);

  sid = vt_def_file_loc(fid, begln, endln);
  rid = curid++;

  if((rtype == VT_OMP_BARRIER || rtype == VT_OMP_IBARRIER))
  {
    if(ompcollop_id == 0) 
    {
      VTGen_write_DEF_COLLECTIVE_OPERATION(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
					    rid,
					    "OMP_Barrier",
					    OTF_COLLECTIVE_TYPE_BARRIER);
      ompcollop_id = rid;
    }
#if (defined (RFG))
    {
      RFG_RegionInfo* rinf =
	RFG_Regions_add(VTTHRD_RFGREGIONS(thrdv[0]), rname, rid);
      if(rinf == NULL) vt_error();
      RFG_Regions_addGroupAssign(VTTHRD_RFGREGIONS(thrdv[0]),
				 "OMP-SYNC", 1, rname);
    }
#endif
    rdid = vt_def_region_desc("OMP-SYNC");
  }
  else if(rtype == VT_FUNCTION_COLL_OTHER ||
	  rtype == VT_FUNCTION_COLL_BARRIER ||
	  rtype == VT_FUNCTION_COLL_ONE2ALL ||
	  rtype == VT_FUNCTION_COLL_ALL2ONE ||
	  rtype == VT_FUNCTION_COLL_ALL2ALL)
  {
    VTGen_write_DEF_COLLECTIVE_OPERATION(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
      rid,   /* collective id equal region id */
      rname, /* collective name equal region name */
      (rtype == VT_FUNCTION_COLL_OTHER) ? OTF_COLLECTIVE_TYPE_UNKNOWN :
      (rtype == VT_FUNCTION_COLL_BARRIER) ? OTF_COLLECTIVE_TYPE_BARRIER :
      (rtype == VT_FUNCTION_COLL_ONE2ALL) ? OTF_COLLECTIVE_TYPE_ONE2ALL :
      (rtype == VT_FUNCTION_COLL_ALL2ONE) ? OTF_COLLECTIVE_TYPE_ALL2ONE :
      (rtype == VT_FUNCTION_COLL_ALL2ALL) ? OTF_COLLECTIVE_TYPE_ALL2ALL : 
      OTF_COLLECTIVE_TYPE_UNKNOWN);

#if (defined (RFG))
    {
      RFG_RegionInfo* rinf =
	RFG_Regions_add(VTTHRD_RFGREGIONS(thrdv[0]), rname, rid);
      if(rinf == NULL) vt_error();
      RFG_Regions_addGroupAssign(VTTHRD_RFGREGIONS(thrdv[0]),
				 "MPI", 1, rname);
    }
#endif
    rdid = vt_def_region_desc(rdesc);
  }
  else
  {
#if (defined (RFG))
    {
      RFG_RegionInfo* rinf =
	RFG_Regions_add(VTTHRD_RFGREGIONS(thrdv[0]), rname, rid);
      if(rinf == NULL) vt_error();

      if(strcmp(rdesc, VT_DEF_GROUP) != 0)
      {
	RFG_Regions_addGroupAssign(VTTHRD_RFGREGIONS(thrdv[0]),
				   rdesc, 1, rname);
	rdid = vt_def_region_desc(rdesc);
      }
      else
      {
	rdid = vt_def_region_desc(rinf->groupName);
      }
    }
#else
    rdid = vt_def_region_desc(rdesc);
#endif
  }

  VTGen_write_DEF_FUNCTION(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			   rid,
			   rname,
			   rdid,
			   sid);

  return rid;
}

uint32_t vt_def_counter_group(const char* gname)
{
  uint32_t gid;

  vt_check_thrd_id(VT_MY_THREAD);

  gid = curid++;

  VTGen_write_DEF_COUNTER_GROUP(VTTHRD_GEN(thrdv[VT_MY_THREAD]), gid, gname);

  return gid;
}

uint32_t vt_def_counter(const char* cname,
			uint32_t cprop,
			uint32_t gid,
			const char* cunit)
{
  uint32_t cid = 0;

  vt_check_thrd_id(VT_MY_THREAD);

#if defined (VT_METR)
  cid = num_metrics;
#endif

  cid += curid++;

  VTGen_write_DEF_COUNTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			  cid,
			  cname,
			  cprop,
			  gid,
			  cunit);

  return cid;
}

void vt_def_mpi_comm(uint32_t cid,
		     uint32_t grpc,
		     uint8_t grpv[])
{
  int i;

  uint32_t  cgrpc;
  uint32_t* cgrpv;
  char      cname[20];

  vt_check_thrd_id(VT_MY_THREAD);

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

  VTGen_write_DEF_PROCESS_GROUP(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
				cid+1, cname, cgrpc, cgrpv);

  free(cgrpv);
}

/*
 *-----------------------------------------------------------------------------
 * Recording events
 *-----------------------------------------------------------------------------
 */

/* -- Region -- */

void vt_enter(uint64_t* time, uint32_t rid) {
  int8_t trace;

  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) == -1) return;

  VTTHRD_STACK_PUSH(thrdv[VT_MY_THREAD]);
  trace = VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]);

#if (defined (RFG))
  {
    RFG_RegionInfo* rinf;

    if( !RFG_Regions_stackPush(VTTHRD_RFGREGIONS(thrdv[VT_MY_THREAD]),
                               rid, trace, &rinf) )
    {
#     if (defined (VT_OMPI) || defined (VT_OMP))
      RFG_RegionInfo* rinf_master =
	 RFG_Regions_get(VTTHRD_RFGREGIONS(thrdv[0]), rid);
      if (rinf_master == NULL)
	vt_error();

      rinf = RFG_Regions_add(VTTHRD_RFGREGIONS(thrdv[VT_MY_THREAD]),
                             rinf_master->regionName, rid);

      /* copy master's call limit */
      rinf->callLimit = rinf_master->callLimit;

      /* initialize call limit count down */
      rinf->callLimitCD = rinf->callLimit;

      if (!RFG_Regions_stackPush(VTTHRD_RFGREGIONS(thrdv[VT_MY_THREAD]),
                                 rid, trace, &rinf))
        vt_error();
#     else
      vt_error();
#     endif
    }

    if(rinf->callLimitCD == 0)
      trace = 0;
  }
#endif

  if (trace)
  {
#   if defined(VT_METR)
      if ( num_metrics > 0 )
      {
	vt_metric_read(VTTHRD_METV(thrdv[VT_MY_THREAD]),
		       VTTHRD_VALV(thrdv[VT_MY_THREAD]));

	VTGen_write_ENTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			  time,
			  rid,
			  0,
			  num_metrics,
			  VTTHRD_VALV(thrdv[VT_MY_THREAD]));
      }
      else
      {
	VTGen_write_ENTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			  time,
			  rid,
			  0,
			  0, NULL);
      }
#   else
      VTGen_write_ENTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			time,
			rid,
			0,
			0, NULL);
#   endif
  }
}

void vt_exit(uint64_t* time) {
  uint8_t trace;

  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) == -1) return;

  VTTHRD_STACK_POP(thrdv[VT_MY_THREAD]);
  trace = VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]);

#if (defined (RFG))
  {
    RFG_RegionInfo* rinf;
    int climitbyenter;

    if(!RFG_Regions_stackPop(VTTHRD_RFGREGIONS(thrdv[VT_MY_THREAD]),
                             &rinf, &climitbyenter))
    {
      vt_error();
    }

    if(climitbyenter == 0)
      trace = 0;
  }
#endif

  if ( trace )
  {
#   if defined(VT_METR)
      if ( num_metrics > 0 )
      {
	vt_metric_read(VTTHRD_METV(thrdv[VT_MY_THREAD]),
		       VTTHRD_VALV(thrdv[VT_MY_THREAD]));

	VTGen_write_LEAVE(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			  time,
			  0,
			  0,
			  num_metrics,
			  VTTHRD_VALV(thrdv[VT_MY_THREAD]));
      }
      else
      {
	VTGen_write_LEAVE(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			  time,
			  0,
			  0,
			  0, NULL);
      }
#   else
      VTGen_write_LEAVE(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			time,
			0,
			0,
			0, NULL);
#   endif
  }
}

/* -- File I/O -- */

void vt_ioexit(uint64_t* time, uint64_t* etime, uint32_t fid, uint64_t hid, uint32_t op, uint64_t bytes) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTGen_write_FILE_OPERATION(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			     time,
			     etime,
			     fid,
			     hid,
			     op,
			     bytes,
			     0);

  vt_exit(etime);
}

/* -- Memory -- */

#if (defined (VT_MEMHOOK))

void vt_mem_alloc(uint64_t* time, uint64_t bytes) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTTHRD_MEM_APP_ALLOC(thrdv[VT_MY_THREAD]) += bytes;

  VTGen_write_COUNTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		      time,
		      mem_app_alloc_cid,
		      VTTHRD_MEM_APP_ALLOC(thrdv[VT_MY_THREAD]));
}

void vt_mem_free(uint64_t* time, uint64_t bytes) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  if( bytes <= VTTHRD_MEM_APP_ALLOC(thrdv[VT_MY_THREAD]) )
    VTTHRD_MEM_APP_ALLOC(thrdv[VT_MY_THREAD]) -= bytes;
  else
    VTTHRD_MEM_APP_ALLOC(thrdv[VT_MY_THREAD]) = 0;

  VTGen_write_COUNTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		      time,
		      mem_app_alloc_cid,
		      VTTHRD_MEM_APP_ALLOC(thrdv[VT_MY_THREAD]));
}

#endif

/* -- Counter -- */

void vt_count(uint64_t* time, uint32_t cid, uint64_t cval) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTGen_write_COUNTER(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		      time,
		      cid,
		      cval);
}

/* -- Comment -- */

void vt_comment(uint64_t* time, const char* comment) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTGen_write_COMMENT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		      time,
		      comment);
}

/* -- MPI-1 -- */

void vt_mpi_send(uint64_t* time, uint32_t dpid, uint32_t cid, uint32_t tag, uint32_t sent) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTGen_write_SEND_MSG(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		       time,
		       dpid+1,
		       cid+1,
		       tag,
		       sent,
		       0);
}

void vt_mpi_recv(uint64_t* time, uint32_t spid, uint32_t cid, uint32_t tag, uint32_t recvd) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTGen_write_RECV_MSG(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
		       time,
		       spid+1,
		       cid+1,
		       tag,
		       recvd,
		       0);
}

void vt_mpi_collexit(uint64_t* time, uint64_t* etime, uint32_t rid, uint32_t rpid, uint32_t cid, uint32_t sent, uint32_t recvd) {
  vt_check_thrd_id(VT_MY_THREAD);
   
  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  VTGen_write_COLLECTIVE_OPERATION(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
				   time,
				   etime,
				   rid,
				   cid+1,
				   rpid != VT_NO_ID ? rpid+1 : 0,
				   sent,
				   recvd,
				   0);

  vt_exit(etime);
}

/* -- OpenMP -- */

void vt_omp_fork(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

#if (defined (VT_OMPI) || defined (VT_OMP))
  if (!omp_in_parallel())
    VTGen_write_OMP_FORK(VTTHRD_GEN(thrdv[VT_MY_THREAD]), time);
#endif
}

void vt_omp_join(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

#if (defined (VT_OMPI) || defined (VT_OMP))
  if (!omp_in_parallel())
    VTGen_write_OMP_JOIN(VTTHRD_GEN(thrdv[VT_MY_THREAD]), time);
#endif
}

void vt_omp_alock(uint64_t* time, uint32_t lkid) {
#if 0
  vt_check_thrd_id(VT_MY_THREAD);
  VTGen_write_OMP_ALOCK(VTTHRD_GEN(thrdv[VT_MY_THREAD]), lkid);
#endif
}

void vt_omp_rlock(uint64_t* time, uint32_t lkid) {
#if 0
  vt_check_thrd_id(VT_MY_THREAD);
  VTGen_write_OMP_RLOCK(VTTHRD_GEN(thrdv[VT_MY_THREAD]), lkid);
#endif
}

void vt_omp_collenter(uint64_t* time, uint32_t rid) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  vt_enter(time, rid);

  /* store timestamp of beginning for vt_omp_collexit() to
     calculate collop. duration */
  VTTHRD_OMP_COLLOP_STIME(thrdv[VT_MY_THREAD]) = *time;
}

void vt_omp_collexit(uint64_t* etime) {
  uint32_t cid;
  uint64_t time;

  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  cid = vt_def_omp_comm();
  time = VTTHRD_OMP_COLLOP_STIME(thrdv[VT_MY_THREAD]);

  VTGen_write_COLLECTIVE_OPERATION(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
				   &time,
				   etime,
				   ompcollop_id,
				   cid+1,
				   0, 0, 0,
				   0);

  vt_exit(etime);
}

void vt_omp_parallel_begin(void)
{
#if defined(VT_METR)
  if ( NULL == thrdv[VT_MY_THREAD]->metv && vt_metric_num() > 0 ) {
    /* create metrics in worker threads */
    vt_cntl_msg("restarting counters in thread %d",VT_MY_THREAD);
    thrdv[VT_MY_THREAD]->metv = vt_metric_create();
  }  
#endif
}


void vt_omp_parallel_end(void)
{
#if defined(VT_METR)
  if ( VT_MY_THREAD > 0 && vt_metric_num() > 0) {
    /* shut down metrics in worker threads */
    vt_metric_free(thrdv[VT_MY_THREAD]->metv);
    thrdv[VT_MY_THREAD]->metv=NULL;
    vt_metric_thread_fini();
  }
#endif
}


/* -- VampirTrace Internal -- */

void vt_enter_user(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  vt_enter(time, vt_trc_regid[VT__USER]);
}

void vt_exit_user(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

  vt_exit(time);
}

void vt_enter_stat(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(thrdv[VT_MY_THREAD]),
		     VTTHRD_VALV(thrdv[VT_MY_THREAD]));

      VTGen_write_ENTER_STAT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			     time,
			     num_metrics,
			     VTTHRD_VALV(thrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_ENTER_STAT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			     time,
			     0, NULL);
    }
#else
  VTGen_write_ENTER_STAT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			 time,
			 0, NULL);
#endif
}

void vt_exit_stat(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(thrdv[VT_MY_THREAD]),
		     VTTHRD_VALV(thrdv[VT_MY_THREAD]));

      VTGen_write_EXIT_STAT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			    time,
			    num_metrics,
			    VTTHRD_VALV(thrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_EXIT_STAT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			    time,
			    0, NULL);
    }
#else
  VTGen_write_EXIT_STAT(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			time,
			0, NULL);
#endif
}

void vt_enter_flush(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(thrdv[VT_MY_THREAD]),
		     VTTHRD_VALV(thrdv[VT_MY_THREAD]));

      VTGen_write_ENTER_FLUSH(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			      time,
			      num_metrics,
			      VTTHRD_VALV(thrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_ENTER_FLUSH(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			      time,
			      0, NULL);
    }
#else
  VTGen_write_ENTER_FLUSH(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			  time,
			  0, NULL);
#endif
}

void vt_exit_flush(uint64_t* time) {
  vt_check_thrd_id(VT_MY_THREAD);

  if (VTTHRD_IS_TRACE_ON(thrdv[VT_MY_THREAD]) < 1) return;

#if defined(VT_METR)
  if ( num_metrics > 0 )
    {
      vt_metric_read(VTTHRD_METV(thrdv[VT_MY_THREAD]),
		     VTTHRD_VALV(thrdv[VT_MY_THREAD]));

      VTGen_write_EXIT_FLUSH(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			     time,
			     num_metrics,
			     VTTHRD_VALV(thrdv[VT_MY_THREAD]));
    }
  else
    {
      VTGen_write_EXIT_FLUSH(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			     time,
			     0, NULL);
    }
#else
  VTGen_write_EXIT_FLUSH(VTTHRD_GEN(thrdv[VT_MY_THREAD]),
			 time,
			 0, NULL);
#endif
}
