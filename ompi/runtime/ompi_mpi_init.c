/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2009 University of Houston. All rights reserved.
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/threads.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/stacktrace.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/mca/event/event.h"

#include "ompi/constants.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/request/request.h"
#include "ompi/message/message.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/file/file.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/rte/base/base.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/dpm/base/base.h"
#include "ompi/mca/pubsub/base/base.h"
#include "ompi/mpiext/mpiext.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif
#include "ompi/runtime/ompi_cr.h"

#if defined(MEMORY_LINUX_PTMALLOC2) && MEMORY_LINUX_PTMALLOC2
#include "opal/mca/memory/linux/memory_linux.h"
/* So this sucks, but with OPAL in its own library that is brought in
   implicity from libmpi, there are times when the malloc initialize
   hook in the memory component doesn't work.  So we have to do it
   from here, since any MPI code is going to call MPI_Init... */
OPAL_DECLSPEC void (*__malloc_initialize_hook) (void) = 
    opal_memory_linux_malloc_init_hook;
#endif

/* This is required for the boundaries of the hash tables used to store
 * the F90 types returned by the MPI_Type_create_f90_XXX functions.
 */
#include <float.h>

#if OPAL_CC_USE_PRAGMA_IDENT
#pragma ident OMPI_IDENT_STRING
#elif OPAL_CC_USE_IDENT
#ident OMPI_IDENT_STRING
#endif
const char ompi_version_string[] = OMPI_IDENT_STRING;

/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_init_started = false;
bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;

opal_thread_t *ompi_mpi_main_thread = NULL;

bool ompi_warn_on_fork;

/*
 * These variables are for the MPI F08 bindings (F08 must bind Fortran
 * varaiables to symbols; it cannot bind Fortran variables to the
 * address of a C variable).
 */

ompi_predefined_datatype_t *ompi_mpi_character_addr = &ompi_mpi_character;
ompi_predefined_datatype_t *ompi_mpi_logical_addr   = &ompi_mpi_logical;
ompi_predefined_datatype_t *ompi_mpi_logical1_addr  = &ompi_mpi_logical1;
ompi_predefined_datatype_t *ompi_mpi_logical2_addr  = &ompi_mpi_logical2;
ompi_predefined_datatype_t *ompi_mpi_logical4_addr  = &ompi_mpi_logical4;
ompi_predefined_datatype_t *ompi_mpi_logical8_addr  = &ompi_mpi_logical8;
ompi_predefined_datatype_t *ompi_mpi_integer_addr   = &ompi_mpi_integer;
ompi_predefined_datatype_t *ompi_mpi_integer1_addr  = &ompi_mpi_integer1;
ompi_predefined_datatype_t *ompi_mpi_integer2_addr  = &ompi_mpi_integer2;
ompi_predefined_datatype_t *ompi_mpi_integer4_addr  = &ompi_mpi_integer4;
ompi_predefined_datatype_t *ompi_mpi_integer8_addr  = &ompi_mpi_integer8;
ompi_predefined_datatype_t *ompi_mpi_integer16_addr = &ompi_mpi_integer16;
ompi_predefined_datatype_t *ompi_mpi_real_addr      = &ompi_mpi_real;
ompi_predefined_datatype_t *ompi_mpi_real4_addr     = &ompi_mpi_real4;
ompi_predefined_datatype_t *ompi_mpi_real8_addr     = &ompi_mpi_real8;
ompi_predefined_datatype_t *ompi_mpi_real16_addr    = &ompi_mpi_real16;
ompi_predefined_datatype_t *ompi_mpi_dblprec_addr   = &ompi_mpi_dblprec;
ompi_predefined_datatype_t *ompi_mpi_cplex_addr     = &ompi_mpi_cplex;
ompi_predefined_datatype_t *ompi_mpi_complex8_addr  = &ompi_mpi_complex8;
ompi_predefined_datatype_t *ompi_mpi_complex16_addr = &ompi_mpi_complex16;
ompi_predefined_datatype_t *ompi_mpi_complex32_addr = &ompi_mpi_complex32;
ompi_predefined_datatype_t *ompi_mpi_dblcplex_addr  = &ompi_mpi_dblcplex;
ompi_predefined_datatype_t *ompi_mpi_2real_addr     = &ompi_mpi_2real;
ompi_predefined_datatype_t *ompi_mpi_2dblprec_addr  = &ompi_mpi_2dblprec;
ompi_predefined_datatype_t *ompi_mpi_2integer_addr  = &ompi_mpi_2integer;

struct ompi_status_public_t *ompi_mpi_status_ignore_addr =
    (ompi_status_public_t *) 0;
struct ompi_status_public_t *ompi_mpi_statuses_ignore_addr =
    (ompi_status_public_t *) 0;

#if OPAL_HAVE_POSIX_THREADS
static bool fork_warning_issued = false;
static bool atfork_called = false;

static void warn_fork_cb(void)
{
    if (ompi_mpi_initialized && !ompi_mpi_finalized && !fork_warning_issued) {
        opal_show_help("help-mpi-runtime.txt", "mpi_init:warn-fork", true,
                       ompi_process_info.nodename, getpid(),
                       ompi_mpi_comm_world.comm.c_my_rank);
        fork_warning_issued = true;
    }
}
#endif

void ompi_warn_fork(void)
{
#if OPAL_HAVE_POSIX_THREADS
    if (ompi_warn_on_fork && !atfork_called) {
        pthread_atfork(warn_fork_cb, NULL, NULL);
        atfork_called = true;
    }
#endif
}

/*
 * These variables are here, rather than under ompi/mpi/c/foo.c
 * because it is not sufficient to have a .c file that only contains
 * variables -- you must have a function that is invoked from
 * elsewhere in the code to guarantee that all linkers will pull in
 * the .o file from the library.  Hence, although these are MPI
 * constants, we might as well just define them here (i.e., in a file
 * that already has a function that is guaranteed to be linked in,
 * rather than make a new .c file with the constants and a
 * corresponding dummy function that is invoked from this function).
 *
 * Additionally, there can be/are strange linking paths such that
 * ompi_info needs symbols such as ompi_fortran_status_ignore,
 * which, if they weren't here with a collection of other global
 * symbols that are initialized (which seems to force this .o file to
 * be pulled into the resolution process, because ompi_info certainly
 * does not call ompi_mpi_init()), would not be able to be found by
 * the OSX linker.
 *
 * NOTE: See the big comment in ompi/mpi/fortran/base/constants.h
 * about why we have four symbols for each of the common blocks (e.g.,
 * the Fortran equivalent(s) of MPI_STATUS_IGNORE).  Here, we can only
 * have *one* value (not four).  So the only thing we can do is make
 * it equal to the fortran compiler convention that was selected at
 * configure time.  Note that this is also true for the value of
 * .TRUE. from the Fortran compiler, so even though Open MPI supports
 * all four Fortran symbol conventions, it can only support one
 * convention for the two C constants (MPI_FORTRAN_STATUS[ES]_IGNORE)
 * and only support one compiler for the value of .TRUE.  Ugh!!
 *
 * Note that the casts here are ok -- we're *only* comparing pointer
 * values (i.e., they'll never be de-referenced).  The global symbols
 * are actually of type (ompi_fortran_common_t) (for alignment
 * issues), but MPI says that MPI_F_STATUS[ES]_IGNORE must be of type
 * (MPI_Fint*).  Hence, we have to cast to make compilers not
 * complain.
 */
#if OMPI_BUILD_FORTRAN_BINDINGS
#  if OMPI_FORTRAN_CAPS
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUS_IGNORE;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUSES_IGNORE;
#  elif OMPI_FORTRAN_PLAIN
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore;
#  elif OMPI_FORTRAN_SINGLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore_;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore_;
#  elif OMPI_FORTRAN_DOUBLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore__;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore__;
#  else
#    error Unrecognized Fortran name mangling scheme
#  endif
#else
MPI_Fint *MPI_F_STATUS_IGNORE = NULL;
MPI_Fint *MPI_F_STATUSES_IGNORE = NULL;
#endif  /* OMPI_BUILD_FORTRAN_BINDINGS */


/* Constants for the Fortran layer.  These values are referred to via
   common blocks in the Fortran equivalents.  See
   ompi/mpi/fortran/base/constants.h for a more detailed explanation.

   The values are *NOT* initialized.  We do not use the values of
   these constants; only their addresses (because they're always
   passed by reference by Fortran).  

   Initializing upon instantiation these can reveal size and/or
   alignment differences between Fortran and C (!) which can cause
   warnings or errors upon linking (e.g., making static libraries with
   the intel 9.0 compilers on 64 bit platforms shows alignment
   differences between libmpi.a and the user's application, resulting
   in a linker warning).  FWIW, if you initialize these variables in
   functions (i.e., not at the instantiation in the global scope), the
   linker somehow "figures it all out" (w.r.t. different alignments
   between fortan common blocks and the corresponding C variables) and
   no linker warnings occur.

   Note that the rationale for the types of each of these variables is
   discussed in ompi/include/mpif-common.h.  Do not change the types
   without also modifying ompi/mpi/fortran/base/constants.h and
   ompi/include/mpif-common.h.
 */

#define INST(type, upper_case, lower_case, single_u, double_u)   \
type lower_case; \
type upper_case; \
type single_u;  \
type double_u

INST(int, MPI_FORTRAN_BOTTOM, mpi_fortran_bottom,
     mpi_fortran_bottom_, mpi_fortran_bottom__);
INST(int, MPI_FORTRAN_IN_PLACE, mpi_fortran_in_place,
     mpi_fortran_in_place_, mpi_fortran_in_place__);
INST(int, MPI_FORTRAN_UNWEIGHTED, mpi_fortran_unweighted,
     mpi_fortran_unweighted_, mpi_fortran_unweighted__);
INST(int, MPI_FORTRAN_WEIGHTS_EMPTY, mpi_fortran_weights_empty,
     mpi_fortran_weights_empty_, mpi_fortran_weights_empty__);
INST(char *, MPI_FORTRAN_ARGV_NULL, mpi_fortran_argv_null,
     mpi_fortran_argv_null_, mpi_fortran_argv_null__);
INST(char *, MPI_FORTRAN_ARGVS_NULL, mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_, mpi_fortran_argvs_null__);
INST(int *, MPI_FORTRAN_ERRCODES_IGNORE, mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_, mpi_fortran_errcodes_ignore__);
INST(int *, MPI_FORTRAN_STATUS_IGNORE, mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_, mpi_fortran_status_ignore__);
INST(int *, MPI_FORTRAN_STATUSES_IGNORE, mpi_fortran_statuses_ignore,
      mpi_fortran_statuses_ignore_, mpi_fortran_statuses_ignore__);

/*
 * Hash tables for MPI_Type_create_f90* functions
 */
opal_hash_table_t ompi_mpi_f90_integer_hashtable;
opal_hash_table_t ompi_mpi_f90_real_hashtable;
opal_hash_table_t ompi_mpi_f90_complex_hashtable;

/*
 * Per MPI-2:9.5.3, MPI_REGISTER_DATAREP is a memory leak.  There is
 * no way to *de*register datareps once they've been registered.  So
 * we have to track all registrations here so that they can be
 * de-registered during MPI_FINALIZE so that memory-tracking debuggers
 * don't show Open MPI as leaking memory.
 */
opal_list_t ompi_registered_datareps;

bool ompi_enable_timing;
extern bool ompi_mpi_yield_when_idle;
extern int ompi_mpi_event_tick_rate;


void ompi_mpi_thread_level(int requested, int *provided)
{
    /**
     * These values are monotonic; MPI_THREAD_SINGLE < MPI_THREAD_FUNNELED
     *                             < MPI_THREAD_SERIALIZED < MPI_THREAD_MULTIPLE.
     * If possible, the call will return provided = required. Failing this,
     * the call will return the least supported level such that
     * provided > required. Finally, if the user requirement cannot be
     * satisfied, then the call will return in provided the highest
     * supported level.
     */
    ompi_mpi_thread_requested = requested;

    if (OMPI_ENABLE_THREAD_MULTIPLE == 1) {
        ompi_mpi_thread_provided = *provided = requested;
    } else {
        if (MPI_THREAD_MULTIPLE == requested) {
            ompi_mpi_thread_provided = *provided = MPI_THREAD_SERIALIZED;
        } else {
            ompi_mpi_thread_provided = *provided = requested;
        }
    }
    ompi_mpi_main_thread = opal_thread_get_self();

    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == 
                                MPI_THREAD_MULTIPLE);
}

static int ompi_register_mca_variables(void)
{
    int ret;

    /* Register MPI variables */
    if (OMPI_SUCCESS != (ret = ompi_mpi_register_params())) {
        return ret;
    }

    /* check to see if we want timing information */
    ompi_enable_timing = false;
    (void) mca_base_var_register("ompi", "ompi", NULL, "timing",
                                 "Request that critical timing loops be measured",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &ompi_enable_timing);

    return OMPI_SUCCESS;
}

int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret;
    ompi_proc_t** procs;
    size_t nprocs;
    char *error = NULL;
    struct timeval ompistart, ompistop;
    bool rte_setup = false;
    ompi_rte_collective_t *coll;
    char *cmd=NULL, *av=NULL;

    /* bitflag of the thread level support provided. To be used
     * for the modex in order to work in heterogeneous environments. */
    uint8_t threadlevel_bf; 

    /* Indicate that we have *started* MPI_INIT*.  MPI_FINALIZE has
       something sorta similar in a static local variable in
       ompi_mpi_finalize(). */
    ompi_mpi_init_started = true;

    /* Setup enough to check get/set MCA params */

    if (OPAL_SUCCESS != (ret = opal_init_util(&argc, &argv))) {
        error = "ompi_mpi_init: opal_init_util failed";
        goto error;
    }

    /* Register MCA variables */
    if (OPAL_SUCCESS != (ret = ompi_register_mca_variables())) {
        error = "ompi_mpi_init: ompi_register_mca_variables failed";
        goto error;
    }

    /* _After_ opal_init_util() but _before_ orte_init(), we need to
       set an MCA param that tells libevent that it's ok to use any
       mechanism in libevent that is available on this platform (e.g.,
       epoll and friends).  Per opal/event/event.s, we default to
       select/poll -- but we know that MPI processes won't be using
       pty's with the event engine, so it's ok to relax this
       constraint and let any fd-monitoring mechanism be used. */

    ret = mca_base_var_find("opal", "event", "*", "event_include");
    if (ret >= 0) {
        char *allvalue = "all";
        /* We have to explicitly "set" the MCA param value here
           because libevent initialization will re-register the MCA
           param and therefore override the default. Setting the value
           here puts the desired value ("all") in different storage
           that is not overwritten if/when the MCA param is
           re-registered. This is unless the user has specified a different
           value for this MCA parameter. Make sure we check to see if the
           default is specified before forcing "all" in case that is not what
           the user desires. Note that we do *NOT* set this value as an
           environment variable, just so that it won't be inherited by
           any spawned processes and potentially cause unintented
           side-effects with launching RTE tools... */
        mca_base_var_set_value(ret, allvalue, 4, MCA_BASE_VAR_SOURCE_DEFAULT, NULL);
    }

    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistart, NULL);
    }

    /* if we were not externally started, then we need to setup
     * some envars so the MPI_INFO_ENV can get the cmd name
     * and argv (but only if the user supplied a non-NULL argv!), and
     * the requested thread level
     */
    if (NULL == getenv("OMPI_COMMAND") && NULL != argv && NULL != argv[0]) {
        asprintf(&cmd, "OMPI_COMMAND=%s", argv[0]);
        putenv(cmd);
    }
    if (NULL == getenv("OMPI_ARGV") && 1 < argc) {
        char *tmp;
        tmp = opal_argv_join(&argv[1], ' ');
        asprintf(&av, "OMPI_ARGV=%s", tmp);
        free(tmp);
        putenv(av);
    }

    /* open the rte framework */
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_rte_base_framework, 0))) {
        error = "ompi_rte_base_open() failed";
        goto error;
    }
    /* no select is required as this is a static framework */

    /* Setup RTE - note that we are an MPI process  */
    if (OMPI_SUCCESS != (ret = ompi_rte_init(NULL, NULL))) {
        error = "ompi_mpi_init: ompi_rte_init failed";
        goto error;
    }
    rte_setup = true;
    
    /* check for timing request - get stop time and report elapsed time if so */
    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init [%ld]: time from start to completion of rte_init %ld usec",
                    (long)OMPI_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

#if OPAL_HAVE_HWLOC
    /* if hwloc is available but didn't get setup for some
     * reason, do so now
     */
    if (NULL == opal_hwloc_topology) {
        if (OPAL_SUCCESS != (ret = opal_hwloc_base_get_topology())) {
            error = "Topology init";
            goto error;
        }
    }
#endif

    /* Register the default errhandler callback - RTE will ignore if it
     * doesn't support this capability
     */
    ompi_rte_register_errhandler(ompi_errhandler_runtime_callback,
                                 OMPI_RTE_ERRHANDLER_LAST);

    /* Figure out the final MPI thread levels.  If we were not
       compiled for support for MPI threads, then don't allow
       MPI_THREAD_MULTIPLE.  Set this stuff up here early in the
       process so that other components can make decisions based on
       this value. */

    ompi_mpi_thread_level(requested, provided);

    /* determine the bitflag belonging to the threadlevel_support provided */
    memset ( &threadlevel_bf, 0, sizeof(uint8_t));
    OMPI_THREADLEVEL_SET_BITFLAG ( ompi_mpi_thread_provided, threadlevel_bf );

    /* add this bitflag to the modex */
    if ( OMPI_SUCCESS != (ret = ompi_modex_send_string("MPI_THREAD_LEVEL", &threadlevel_bf, sizeof(uint8_t)))) {
        error = "ompi_mpi_init: modex send thread level";
        goto error;
    }

    /* initialize datatypes. This step should be done early as it will
     * create the local convertor and local arch used in the proc
     * init.
     */
    if (OMPI_SUCCESS != (ret = ompi_datatype_init())) {
        error = "ompi_datatype_init() failed";
        goto error;
    }

    /* Initialize OMPI procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        error = "mca_proc_init() failed";
        goto error;
    }

    /* Initialize the op framework. This has to be done *after*
       ddt_init, but befor mca_coll_base_open, since some collective
       modules (e.g., the hierarchical coll component) may need ops in
       their query function. */
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_op_base_framework, 0))) {
        error = "ompi_op_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != 
        (ret = ompi_op_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                           OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "ompi_op_base_find_available() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = ompi_op_init())) {
        error = "ompi_op_init() failed";
        goto error;
    }

    /* Open up MPI-related MCA components */

    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_allocator_base_framework, 0))) {
        error = "mca_allocator_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_rcache_base_framework, 0))) {
        error = "mca_rcache_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_mpool_base_framework, 0))) {
        error = "mca_mpool_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_bml_base_framework, 0))) {
        error = "mca_bml_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_pml_base_framework, 0))) {
        error = "mca_pml_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_coll_base_framework, 0))) {
        error = "mca_coll_base_open() failed";
        goto error;
    }

    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_osc_base_framework, 0))) {
        error = "ompi_osc_base_open() failed";
        goto error;
    }

#if OPAL_ENABLE_FT_CR == 1
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_crcp_base_framework, 0))) {
        error = "ompi_crcp_base_open() failed";
        goto error;
    }
#endif

    /* In order to reduce the common case for MPI apps (where they
       don't use MPI-2 IO or MPI-1 topology functions), the io and
       topo frameworks are initialized lazily, at the first use of
       relevant functions (e.g., MPI_FILE_*, MPI_CART_*, MPI_GRAPH_*),
       so they are not opened here. */

    /* Select which MPI components to use */

    if (OMPI_SUCCESS != 
        (ret = mca_mpool_base_init(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_mpool_base_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_pml_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_pml_base_select() failed";
        goto error;
    }

    /* check for timing request - get stop time and report elapsed time if so */
    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from completion of rte_init to modex %ld usec",
                    (long)OMPI_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }
    
    /* exchange connection info - this function also acts as a barrier
     * as it will not return until the exchange is complete
     */
    coll = OBJ_NEW(ompi_rte_collective_t);
    coll->id = ompi_process_info.peer_modex;
    coll->active = true;
    if (OMPI_SUCCESS != (ret = ompi_rte_modex(coll))) {
        error = "rte_modex failed";
        goto error;
    }
    /* wait for modex to complete - this may be moved anywhere in mpi_init
     * so long as it occurs prior to calling a function that needs
     * the modex info!
     */
    OMPI_WAIT_FOR_COMPLETION(coll->active);
    OBJ_RELEASE(coll);

    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time to execute modex %ld usec",
                    (long)OMPI_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

    /* select buffered send allocator component to be used */
    if( OMPI_SUCCESS !=
	(ret = mca_pml_base_bsend_init(OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_pml_base_bsend_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                            OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_coll_base_find_available() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = ompi_osc_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                            OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "ompi_osc_base_find_available() failed";
        goto error;
    }

#if OPAL_ENABLE_FT_CR == 1
    if (OMPI_SUCCESS != (ret = ompi_crcp_base_select() ) ) {
        error = "ompi_crcp_base_select() failed";
        goto error;
    }
#endif

    /* io and topo components are not selected here -- see comment
       above about the io and topo frameworks being loaded lazily */

    /* Initialize each MPI handle subsystem */
    /* initialize requests */
    if (OMPI_SUCCESS != (ret = ompi_request_init())) {
        error = "ompi_request_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != (ret = ompi_message_init())) {
        error = "ompi_message_init() failed";
        goto error;
    }

    /* initialize info */
    if (OMPI_SUCCESS != (ret = ompi_info_init())) {
        error = "ompi_info_init() failed";
        goto error;
    }

    /* initialize error handlers */
    if (OMPI_SUCCESS != (ret = ompi_errhandler_init())) {
        error = "ompi_errhandler_init() failed";
        goto error;
    }

    /* initialize error codes */
    if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_init())) {
        error = "ompi_mpi_errcode_init() failed";
        goto error;
    }
    
    /* initialize internal error codes */
    if (OMPI_SUCCESS != (ret = ompi_errcode_intern_init())) {
        error = "ompi_errcode_intern_init() failed";
        goto error;
    }
     
    /* initialize groups  */
    if (OMPI_SUCCESS != (ret = ompi_group_init())) {
        error = "ompi_group_init() failed";
        goto error;
    }

    /* initialize communicators */
    if (OMPI_SUCCESS != (ret = ompi_comm_init())) {
        error = "ompi_comm_init() failed";
        goto error;
    }

    /* initialize file handles */
    if (OMPI_SUCCESS != (ret = ompi_file_init())) {
        error = "ompi_file_init() failed";
        goto error;
    }

    /* initialize windows */
    if (OMPI_SUCCESS != (ret = ompi_win_init())) {
        error = "ompi_win_init() failed";
        goto error;
    }

    /* initialize attribute meta-data structure for comm/win/dtype */
    if (OMPI_SUCCESS != (ret = ompi_attr_init())) {
        error = "ompi_attr_init() failed";
        goto error;
    }

    /* identify the architectures of remote procs and setup
     * their datatype convertors, if required
     */
    if (OMPI_SUCCESS != (ret = ompi_proc_complete_init())) {
        error = "ompi_proc_complete_init failed";
        goto error;
    }

    /* If thread support was enabled, then setup OPAL to allow for
       them. */
    if ((OMPI_ENABLE_PROGRESS_THREADS == 1) ||
        (*provided != MPI_THREAD_SINGLE)) {
        opal_set_using_threads(true);
    }

    /* start PML/BTL's */
    ret = MCA_PML_CALL(enable(true));
    if( OMPI_SUCCESS != ret ) {
        error = "PML control failed";
        goto error;
    }

    /* add all ompi_proc_t's to PML */
    if (NULL == (procs = ompi_proc_world(&nprocs))) {
        error = "ompi_proc_world() failed";
        goto error;
    }
    ret = MCA_PML_CALL(add_procs(procs, nprocs));
    free(procs);
    /* If we got "unreachable", then print a specific error message.
       Otherwise, if we got some other failure, fall through to print
       a generic message. */
    if (OMPI_ERR_UNREACH == ret) {
        opal_show_help("help-mpi-runtime",
                       "mpi_init:startup:pml-add-procs-fail", true);
        error = NULL;
        goto error;
    } else if (OMPI_SUCCESS != ret) {
        error = "PML add procs failed";
        goto error;
    }

    MCA_PML_CALL(add_comm(&ompi_mpi_comm_world.comm));
    MCA_PML_CALL(add_comm(&ompi_mpi_comm_self.comm));

    /*
     * Dump all MCA parameters if requested
     */
    if (ompi_mpi_show_mca_params) {
        ompi_show_all_mca_params(ompi_mpi_comm_world.comm.c_my_rank, 
                                 nprocs, 
                                 ompi_process_info.nodename);
    }

    /* Do we need to wait for a debugger? */
    ompi_rte_wait_for_debugger();

    /* check for timing request - get stop time and report elapsed
       time if so, then start the clock again */
    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from modex to first barrier %ld usec",
                    (long)OMPI_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

    /* wait for everyone to reach this point */
    coll = OBJ_NEW(ompi_rte_collective_t);
    coll->id = ompi_process_info.peer_init_barrier;
    coll->active = true;
    if (OMPI_SUCCESS != (ret = ompi_rte_barrier(coll))) {
        error = "rte_barrier failed";
        goto error;
    }
    /* wait for barrier to complete */
    OMPI_WAIT_FOR_COMPLETION(coll->active);
    OBJ_RELEASE(coll);

    /* check for timing request - get stop time and report elapsed
       time if so, then start the clock again */
    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time to execute barrier %ld usec",
                    (long)OMPI_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 0
    /* Start setting up the event engine for MPI operations.  Don't
       block in the event library, so that communications don't take
       forever between procs in the dynamic code.  This will increase
       CPU utilization for the remainder of MPI_INIT when we are
       blocking on RTE-level events, but may greatly reduce non-TCP
       latency. */
    opal_progress_set_event_flag(OPAL_EVLOOP_NONBLOCK);
#endif
    
    /* wire up the mpi interface, if requested.  Do this after the
       non-block switch for non-TCP performance.  Do before the
       polling change as anyone with a complex wire-up is going to be
       using the oob. */
    if (OMPI_SUCCESS != (ret = ompi_init_preconnect_mpi())) {
        error = "ompi_mpi_do_preconnect_all() failed";
        goto error;
    }

    /* Setup the publish/subscribe (PUBSUB) framework */
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_pubsub_base_framework, 0))) {
        error = "mca_pubsub_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = ompi_pubsub_base_select())) {
        error = "ompi_pubsub_base_select() failed";
        goto error;
    }
    
    /* Setup the dynamic process management (DPM) framework */
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_dpm_base_framework, 0))) {
        error = "ompi_dpm_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = ompi_dpm_base_select())) {
        error = "ompi_dpm_base_select() failed";
        goto error;
    }

    /* Determine the overall threadlevel support of all processes 
       in MPI_COMM_WORLD. This has to be done before calling 
       coll_base_comm_select, since some of the collective components
       e.g. hierarch, might create subcommunicators. The threadlevel
       requested by all processes is required in order to know
       which cid allocation algorithm can be used. */
    if ( OMPI_SUCCESS != 
	 ( ret = ompi_comm_cid_init ())) {
	error = "ompi_mpi_init: ompi_comm_cid_init failed";
	goto error;
    }

    /* Init coll for the comms. This has to be after dpm_base_select, 
       (since dpm.mark_dyncomm is not set in the communicator creation
       function else), but before dpm.dyncom_init, since this function
       might require collective for the CID allocation. */
    if (OMPI_SUCCESS !=
        (ret = mca_coll_base_comm_select(MPI_COMM_WORLD))) {
        error = "mca_coll_base_comm_select(MPI_COMM_WORLD) failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_comm_select(MPI_COMM_SELF))) {
        error = "mca_coll_base_comm_select(MPI_COMM_SELF) failed";
        goto error;
    }


    
    /* Check whether we have been spawned or not.  We introduce that
       at the very end, since we need collectives, datatypes, ptls
       etc. up and running here.... */
    if (OMPI_SUCCESS != (ret = ompi_dpm.dyn_init())) {
        error = "ompi_comm_dyn_init() failed";
        goto error;
    }

    /*
     * Startup the Checkpoint/Restart Mech.
     * Note: Always do this so tools don't hang when
     * in a non-checkpointable build
     */
    if (OMPI_SUCCESS != (ret = ompi_cr_init())) {
        error = "ompi_cr_init";
        goto error;
    }

    /* Undo OPAL calling opal_progress_event_users_increment() during 
       opal_init, to get better latency when not using TCP.  Do 
       this *after* dyn_init, as dyn init uses lots of RTE 
       communication and we don't want to hinder the performance of 
       that code. */ 
    opal_progress_event_users_decrement(); 

    /* see if yield_when_idle was specified - if so, use it */
    opal_progress_set_yield_when_idle(ompi_mpi_yield_when_idle);
    
    /* negative value means use default - just don't do anything */
    if (ompi_mpi_event_tick_rate >= 0) {
        opal_progress_set_event_poll_rate(ompi_mpi_event_tick_rate);
    }

    /* At this point, we are fully configured and in MPI mode.  Any
       communication calls here will work exactly like they would in
       the user's code.  Setup the connections between procs and warm
       them up with simple sends, if requested */

    if (OMPI_SUCCESS != (ret = ompi_mpiext_init())) {
        error = "ompi_mpiext_init";
        goto error;
    }

    /* Fall through */
 error:
    if (ret != OMPI_SUCCESS) {
        /* Only print a message if one was not already printed */
        if (NULL != error) {
            const char *err_msg = opal_strerror(ret);
            /* If RTE was not setup yet, don't use opal_show_help */
            if (rte_setup) {
                opal_show_help("help-mpi-runtime",
                               "mpi_init:startup:internal-failure", true,
                               "MPI_INIT", "MPI_INIT", error, err_msg, ret);
            } else {
                opal_show_help("help-mpi-runtime",
                               "mpi_init:startup:internal-failure", true,
                               "MPI_INIT", "MPI_INIT", error, err_msg, ret);
            }
        }
        return ret;
    }

    /* Initialize the registered datarep list to be empty */
    OBJ_CONSTRUCT(&ompi_registered_datareps, opal_list_t);

    /* Initialize the arrays used to store the F90 types returned by the
     *  MPI_Type_create_f90_XXX functions.
     */
    OBJ_CONSTRUCT( &ompi_mpi_f90_integer_hashtable, opal_hash_table_t);
    opal_hash_table_init(&ompi_mpi_f90_integer_hashtable, 16 /* why not? */);

    OBJ_CONSTRUCT( &ompi_mpi_f90_real_hashtable, opal_hash_table_t);
    opal_hash_table_init(&ompi_mpi_f90_real_hashtable, FLT_MAX_10_EXP);

    OBJ_CONSTRUCT( &ompi_mpi_f90_complex_hashtable, opal_hash_table_t);
    opal_hash_table_init(&ompi_mpi_f90_complex_hashtable, FLT_MAX_10_EXP);

    /* All done.  Wasn't that simple? */

    ompi_mpi_initialized = true;

    /* check for timing request - get stop time and report elapsed time if so */
    if (ompi_enable_timing && 0 == OMPI_PROC_MY_NAME->vpid) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from barrier to complete mpi_init %ld usec",
                    (long)OMPI_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
    }

    return MPI_SUCCESS;
}
