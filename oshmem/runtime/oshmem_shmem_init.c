/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/allocator/base/base.h"
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
#include "oshmem/info/info.h"
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

MPI_Comm oshmem_comm_world = {0};

opal_thread_t *oshmem_mpi_main_thread = NULL;

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

int oshmem_shmem_inglobalexit = 0;
int oshmem_shmem_globalexit_status = -1;

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
        PMPI_Comm_dup(MPI_COMM_WORLD, &oshmem_comm_world);

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
        int nproc;
        int my_pe;
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

        nproc = oshmem_num_procs();
        my_pe = oshmem_my_proc_id();
        for (i = 0; i < nproc; i++) {
            shmem_long_p(preconnect_value, val, (my_pe + i) % nproc);
        }
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

    /* initialize info */
    if (OSHMEM_SUCCESS != (ret = oshmem_info_init())) {
        error = "oshmem_info_init() failed";
        goto error;
    }

    /* initialize proc */
    if (OSHMEM_SUCCESS != (ret = oshmem_proc_init())) {
        error = "oshmem_proc_init() failed";
        goto error;
    }

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
            != (ret = mca_spml_base_select(OPAL_ENABLE_PROGRESS_THREADS,
                                           OMPI_ENABLE_THREAD_MULTIPLE))) {
        error = "mca_spml_base_select() failed";
        goto error;
    }

    if (OSHMEM_SUCCESS
            != (ret =
                    mca_scoll_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
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

    if (OSHMEM_SUCCESS != (ret = oshmem_proc_group_init())) {
        error = "oshmem_proc_group_init() failed";
        goto error;
    }

    /* start SPML/BTL's */
    ret = MCA_SPML_CALL(enable(true));
    if (OSHMEM_SUCCESS != ret) {
        error = "SPML control failed";
        goto error;
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
                    mca_atomic_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
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

