/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015-2018 Cisco Systems, Inc.  All rights reserved
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
#include <pthread.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <float.h>

#include "math.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/threads/threads.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/stacktrace.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"

#include "ompi/datatype/ompi_datatype.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/util/timings.h"

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

shmem_internal_mutex_t shmem_internal_mutex_alloc = {{0}};

shmem_ctx_t oshmem_ctx_default = NULL;

shmem_team_t oshmem_team_shared = NULL;
shmem_team_t oshmem_team_world  = NULL;

static int _shmem_init(int argc, char **argv, int requested, int *provided);

#if OSHMEM_OPAL_THREAD_ENABLE
static void* shmem_opal_thread(void* argc)
{
/*
 * WHAT: sleep() invocation
 * WHY:  there occurs a segfault sometimes and sleep()
 *       reduces its possibility
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

    OMPI_TIMING_INIT(128);

    if (!oshmem_shmem_initialized) {
        ret = ompi_mpi_init(argc, argv, requested, provided, true);
        OMPI_TIMING_NEXT("ompi_mpi_init");

        if (OSHMEM_SUCCESS != ret) {
            return ret;
        }

        PMPI_Comm_dup(MPI_COMM_WORLD, &oshmem_comm_world);
        OMPI_TIMING_NEXT("PMPI_Comm_dup");

        SHMEM_MUTEX_INIT(shmem_internal_mutex_alloc);

        ret = _shmem_init(argc, argv, requested, provided);
        OMPI_TIMING_NEXT("_shmem_init");
        OMPI_TIMING_IMPORT_OPAL("_shmem_init");
        OMPI_TIMING_IMPORT_OPAL("mca_scoll_mpi_comm_query");
        OMPI_TIMING_IMPORT_OPAL("mca_scoll_enable");
        OMPI_TIMING_IMPORT_OPAL("mca_scoll_base_select");
        OMPI_TIMING_IMPORT_OPAL("mca_memheap_base_select");
        OMPI_TIMING_IMPORT_OPAL("_memheap_create");
        OMPI_TIMING_IMPORT_OPAL_PREFIX("regular_mem", "mca_memheap_base_alloc_init");
        OMPI_TIMING_IMPORT_OPAL_PREFIX("device_mem", "mca_memheap_base_alloc_init");

        if (OSHMEM_SUCCESS != ret) {
            return ret;
        }
        oshmem_shmem_initialized = true;

        if (OSHMEM_SUCCESS != shmem_lock_init()) {
            SHMEM_API_ERROR( "shmem_lock_init() failed");
            return OSHMEM_ERROR;
        }
        OMPI_TIMING_NEXT("shmem_lock_init");

        /* this is a collective op, implies barrier */
        MCA_MEMHEAP_CALL(get_all_mkeys());
        OMPI_TIMING_NEXT("get_all_mkeys()");
        OMPI_TIMING_IMPORT_OPAL("mca_memheap_modex_recv_all");

        oshmem_shmem_preconnect_all();
        OMPI_TIMING_NEXT("shmem_preconnect_all");

#if OSHMEM_OPAL_THREAD_ENABLE
        pthread_t thread_id;
        int perr;
        perr = pthread_create(&thread_id, NULL, &shmem_opal_thread, NULL);
        if (0 != perr) {
            SHMEM_API_ERROR("cannot create opal thread for SHMEM");
            return OSHMEM_ERROR;
        }
#endif
        OMPI_TIMING_NEXT("THREAD_ENABLE");
    }
#ifdef SIGUSR1
    signal(SIGUSR1,sighandler__SIGUSR1);
    signal(SIGTERM,sighandler__SIGTERM);
#endif
    OMPI_TIMING_OUT;
    OMPI_TIMING_FINALIZE;
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

    oshmem_mpi_thread_requested = requested;
    oshmem_mpi_thread_provided = requested;

    OPAL_TIMING_ENV_INIT(timing);

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

    OPAL_TIMING_ENV_NEXT(timing, "shmem_params");
    /* initialize info */
    if (OSHMEM_SUCCESS != (ret = oshmem_info_init())) {
        error = "oshmem_info_init() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_info_init()");

    /* initialize proc */
    if (OSHMEM_SUCCESS != (ret = oshmem_proc_init())) {
        error = "oshmem_proc_init() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_proc_init()");

    if (OSHMEM_SUCCESS != (ret = oshmem_op_init())) {
        error = "oshmem_op_init() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_op_init()");

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_spml_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_spml_base_open() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "open SPML framework");

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_scoll_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_scoll_base_open() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "open SCOLL framework");

    if (OSHMEM_SUCCESS != (ret = mca_spml_base_select(OPAL_ENABLE_PROGRESS_THREADS, 1))) {
        error = "mca_spml_base_select() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "select SPML framework");

    if (OSHMEM_SUCCESS != (ret = mca_scoll_base_find_available(OPAL_ENABLE_PROGRESS_THREADS, 1))) {
        error = "mca_scoll_base_find_available() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "find SCOLL components");

    /* Initialize each SHMEM handle subsystem */
    /* Initialize requests */
    if (OSHMEM_SUCCESS != (ret = oshmem_request_init())) {
        error = "oshmem_request_init() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_request_init()");

    if (OSHMEM_SUCCESS != (ret = oshmem_proc_group_init())) {
        error = "oshmem_proc_group_init() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_proc_group_init()");

    /* start SPML/BTL's */
    ret = MCA_SPML_CALL(enable(true));
    if (OSHMEM_SUCCESS != ret) {
        error = "SPML control failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "MCA_SPML_CALL(enable())");

    ret =
            MCA_SPML_CALL(add_procs(oshmem_group_all, oshmem_group_all->proc_count));
    if (OSHMEM_SUCCESS != ret) {
        error = "SPML add procs failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "MCA_SPML_CALL(add_procs())");

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_sshmem_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_sshmem_base_open() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "open SSHMEM framework");

    if (OSHMEM_SUCCESS != (ret = mca_sshmem_base_select())) {
        error = "mca_sshmem_base_select() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "select SSHMEM framework");

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_memheap_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_memheap_base_open() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "open MEMHEAP framework");


    if (OSHMEM_SUCCESS != (ret = mca_memheap_base_select())) {
        error = "mca_memheap_base_select() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "select MEMHEAP framework");

    if (OSHMEM_SUCCESS != (ret = mca_base_framework_open(&oshmem_atomic_base_framework, MCA_BASE_OPEN_DEFAULT))) {
        error = "mca_atomic_base_open() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "open ATOMIC framework");

    if (OSHMEM_SUCCESS != (ret = mca_atomic_base_find_available(OPAL_ENABLE_PROGRESS_THREADS, 1))) {
        error = "mca_atomic_base_find_available() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "find avail ATOMIC framework");

    /* This call should be done after memheap initialization */
    if (OSHMEM_SUCCESS != (ret = mca_scoll_enable())) {
        error = "mca_scoll_enable() failed";
        goto error;
    }

    OPAL_TIMING_ENV_NEXT(timing, "mca_scoll_enable()");

    (*provided) = oshmem_mpi_thread_provided;

    oshmem_mpi_thread_multiple = (oshmem_mpi_thread_provided == SHMEM_THREAD_MULTIPLE) ? true : false;


    error: if (ret != OSHMEM_SUCCESS) {
        const char *err_msg = opal_strerror(ret);
        opal_show_help("help-shmem-runtime.txt",
                       "shmem_init:startup:internal-failure",
                       true,
                       "SHMEM_INIT",
                       "SHMEM_INIT",
                       error,
                       err_msg,
                       ret);
        return ret;
    }
    OPAL_TIMING_ENV_NEXT(timing, "DONE");

    return ret;
}
