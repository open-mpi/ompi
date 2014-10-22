/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <float.h>

#include "math.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/threads.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/stacktrace.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/mca/ess/ess.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/allocator/base/base.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/mpiruntime.h"

#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/runtime/params.h"
#include "oshmem/runtime/oshmem_shmem_preconnect.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/scoll/base/base.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/proc/proc.h"
#include "oshmem/proc/proc_group_cache.h"
#include "oshmem/op/op.h"
#include "oshmem/request/request.h"
#include "oshmem/shmem/shmem_api_logger.h"

#include "oshmem/shmem/shmem_lock.h"

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#if OPAL_CC_USE_PRAGMA_IDENT
#pragma ident OMPI_IDENT_STRING
#elif OPAL_CC_USE_IDENT
#ident OSHMEM_IDENT_STRING
#endif

/*
 * WHAT: add thread for invoking opal_progress() function
 * WHY:  SHMEM based on current ompi/trunk (by the time of integrating into Open MPI)
 *       has put/get implementation via send and needs opal_progress() invocation
 *       on the remote side (i.e. not true one-sided operations).
 */
#define OSHMEM_OPAL_THREAD_ENABLE 0

const char oshmem_version_string[] = OSHMEM_IDENT_STRING;

/*
 * Global variables and symbols for the MPI layer
 */

bool oshmem_shmem_initialized = false;
bool oshmem_shmem_aborted = false;
bool oshmem_mpi_thread_multiple = false;
int oshmem_mpi_thread_requested = SHMEM_THREAD_SINGLE;
int oshmem_mpi_thread_provided = SHMEM_THREAD_SINGLE;
long *preconnect_value = 0;
int shmem_api_logger_output = -1;

MPI_Comm oshmem_comm_world;

opal_thread_t *oshmem_mpi_main_thread = NULL;

/* Constants for the Fortran layer.  These values are referred to via
 common blocks in the Fortran equivalents.  See
 ompi/mpi/f77/constants.h for a more detailed explanation.

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
 without also modifying ompi/mpi/f77/constants.h and
 ompi/include/mpif-common.h.
 */

#define INST(type, upper_case, lower_case, single_u, double_u)   \
    type lower_case; \
type upper_case; \
type single_u;  \
type double_u

INST(int,
     MPI_FORTRAN_BOTTOM,
     mpi_fortran_bottom,
     mpi_fortran_bottom_,
     mpi_fortran_bottom__);
INST(int,
     MPI_FORTRAN_IN_PLACE,
     mpi_fortran_in_place,
     mpi_fortran_in_place_,
     mpi_fortran_in_place__);
INST(char *,
     MPI_FORTRAN_ARGV_NULL,
     mpi_fortran_argv_null,
     mpi_fortran_argv_null_,
     mpi_fortran_argv_null__);
INST(double,
     MPI_FORTRAN_ARGVS_NULL,
     mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_,
     mpi_fortran_argvs_null__);
INST(int *,
     MPI_FORTRAN_ERRCODES_IGNORE,
     mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_,
     mpi_fortran_errcodes_ignore__);
INST(int *,
     MPI_FORTRAN_STATUS_IGNORE,
     mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_,
     mpi_fortran_status_ignore__);
INST(double,
     MPI_FORTRAN_STATUSES_IGNORE,
     mpi_fortran_statuses_ignore,
     mpi_fortran_statuses_ignore_,
     mpi_fortran_statuses_ignore__);

/*
 * Hash tables for MPI_Type_create_f90* functions
 */
opal_hash_table_t ompi_mpi_f90_integer_hashtable;
opal_hash_table_t ompi_mpi_f90_real_hashtable;
opal_hash_table_t ompi_mpi_f90_complex_hashtable;

static int _shmem_init(int argc, char **argv, int requested, int *provided);

#if OSHMEM_OPAL_THREAD_ENABLE
static void* shmem_opal_thread(void* argc)
{
/*
 * WHAT: sleep() invocation
 * WHY:  there occures a segfault sometimes and sleep()
 *       reduces it's possibility
 */
    sleep(1);
    while(oshmem_shmem_initialized)
        opal_progress();
    return NULL;
}
#endif

int oshmem_shmem_inglobalexit;
int oshmem_shmem_globalexit_status;

static void sighandler__SIGUSR1(int signum)
{
    if (0 != oshmem_shmem_inglobalexit)
    {
	return;
    }
    _exit(0);
}
static void sighandler__SIGTERM(int signum)
{
    /* Do nothing. Just replace other unpredictalbe handlers with this one (e.g. mxm handler). */
}

int oshmem_shmem_init(int argc, char **argv, int requested, int *provided)
{
    int ret = OSHMEM_SUCCESS;

    if (!oshmem_shmem_initialized) {
        if (!ompi_mpi_initialized && !ompi_mpi_finalized) {
            ret = ompi_mpi_init(argc, argv, requested, provided);
        }
        MPI_Comm_dup(MPI_COMM_WORLD, &oshmem_comm_world);

        if (OSHMEM_SUCCESS == ret) {
            ret = _shmem_init(argc, argv, requested, provided);
        }

        if (OSHMEM_SUCCESS == ret) {
            oshmem_shmem_initialized = true;

            if (OSHMEM_SUCCESS != shmem_lock_init()) {
                SHMEM_API_ERROR( "shmem_lock_init() failed");
                return OSHMEM_ERROR;
            }

            /* this is a collective op, implies barrier */
            MCA_MEMHEAP_CALL(get_all_mkeys());

            oshmem_shmem_preconnect_all();
#if OSHMEM_OPAL_THREAD_ENABLE
            pthread_t thread_id;
            int perr;
            perr = pthread_create(&thread_id, NULL, &shmem_opal_thread, NULL);
            if (perr != 0)
            {
                SHMEM_API_ERROR("cannot creat opal thread for SHMEM");
                return OSHMEM_ERROR;
            }
#endif
        }
    }
#ifdef SIGUSR1
    signal(SIGUSR1,sighandler__SIGUSR1);
    signal(SIGTERM,sighandler__SIGTERM);
#endif
    return ret;
}

int oshmem_shmem_preconnect_all(void)
{
    int rc = OSHMEM_SUCCESS;

    /* force qp creation and rkey exchange for memheap. Does not force exchange of static vars */
    if (oshmem_preconnect_all) {
        long val;
        int nproc = 0;
        int i;

        val = 0xdeadbeaf;

        if (!preconnect_value) {
            rc =
                    MCA_MEMHEAP_CALL(private_alloc(sizeof(long), (void **)&preconnect_value));
        }
        if (!preconnect_value || (rc != OSHMEM_SUCCESS)) {
            SHMEM_API_ERROR("shmem_preconnect_all failed");
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        }
        nproc = _num_pes();
        for (i = 0; i < nproc; i++) {
            shmem_long_p(preconnect_value, val, i);
        }
        shmem_fence();
        shmem_barrier_all();
        SHMEM_API_VERBOSE(5, "Preconnected all PEs");
    }

    return OSHMEM_SUCCESS;
}

int oshmem_shmem_preconnect_all_finalize(void)
{
    if (preconnect_value) {
        MCA_MEMHEAP_CALL(private_free(preconnect_value));
        preconnect_value = 0;
    }

    return OSHMEM_SUCCESS;
}

static int _shmem_init(int argc, char **argv, int requested, int *provided)
{
    int ret = OSHMEM_SUCCESS;
    char *error = NULL;

    if (OSHMEM_SUCCESS != (ret = oshmem_proc_init())) {
        error = "oshmem_proc_init() failed";
        goto error;
    }

    /* We need to do this anyway.
     * This place requires to be reviewed and more elegant way is expected
     */
    ompi_proc_local_proc = (ompi_proc_t*) oshmem_proc_local_proc;

    /* Register the OSHMEM layer's MCA parameters */
    if (OSHMEM_SUCCESS != (ret = oshmem_shmem_register_params())) {
        error = "oshmem_info_register: oshmem_register_params failed";
        goto error;
    }
    /* Setting verbosity for macros like SHMEM_API_VERBOSE, SHMEM_API_ERROR.
     * We need to set it right after registering mca verbosity variables
     */
    shmem_api_logger_output = opal_output_open(NULL);
    opal_output_set_verbosity(shmem_api_logger_output,
                              oshmem_shmem_api_verbose);

    if (OSHMEM_SUCCESS != (ret = oshmem_group_cache_list_init())) {
        error = "oshmem_group_cache_list_init() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = oshmem_op_init())) {
        error = "oshmem_op_init() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_spml_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_spml_base_open() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_scoll_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_scoll_base_open() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS
            != (ret = mca_spml_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                           OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_spml_base_select() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS
            != (ret =
                    mca_scoll_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                                  OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_scoll_base_find_available() failed";
        goto error;
    }

    /* Initialize each SHMEM handle subsystem */
    /* Initialize requests */
    if (OSHMEM_SUCCESS != (ret = oshmem_request_init())) {
        error = "oshmem_request_init() failed";
        goto error;
    }

    /* identify the architectures of remote procs and setup
     * their datatype convertors, if required
     */
    if (OSHMEM_SUCCESS != (ret = oshmem_proc_set_arch())) {
        error = "oshmem_proc_set_arch failed";
        goto error;
    }

    /* start SPML/BTL's */
    ret = MCA_SPML_CALL(enable(true));
    if (OSHMEM_SUCCESS != ret) {
        error = "SPML control failed";
        goto error;
    }

    /* There is issue with call add_proc twice so
     * we need to use btl info got from PML add_procs() before call of SPML add_procs()
     */
    {
        ompi_proc_t** procs = NULL;
        size_t nprocs = 0;
        procs = ompi_proc_world(&nprocs);
        while (nprocs--) {
            oshmem_group_all->proc_array[nprocs]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML] =
                    procs[nprocs]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
        }
        free(procs);
    }

    ret =
            MCA_SPML_CALL(add_procs(oshmem_group_all->proc_array, oshmem_group_all->proc_count));
    if (OSHMEM_SUCCESS != ret) {
        error = "SPML add procs failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_sshmem_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_sshmem_base_open() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_sshmem_base_select())) {
        error = "mca_sshmem_base_select() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_memheap_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_memheap_base_open() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_memheap_base_select())) {
        error = "mca_memheap_base_select() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_atomic_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_atomic_base_open() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS
            != (ret =
                    mca_atomic_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                                   OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_atomic_base_find_available() failed";
        goto error;
    }

    /* This call should be done after memheap initialization */
    if (OSHMEM_SUCCESS != (ret = mca_scoll_enable())) {
        error = "mca_scoll_enable() failed";
        goto error;
    }

    error: if (ret != OSHMEM_SUCCESS) {
        const char *err_msg = opal_strerror(ret);
        orte_show_help("help-shmem-runtime.txt",
                       "shmem_init:startup:internal-failure",
                       true,
                       "SHMEM_INIT",
                       "SHMEM_INIT",
                       error,
                       err_msg,
                       ret);
        return ret;
    }

    return ret;
}

