/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2022      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "instance.h"

#include "opal/util/arch.h"

#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal_params.h"

#include "ompi/mca/pml/pml.h"
#include "ompi/runtime/params.h"

#include "ompi/interlib/interlib.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/message/message.h"
#include "ompi/info/info.h"
#include "ompi/attribute/attribute.h"
#include "ompi/op/op.h"
#include "ompi/dpm/dpm.h"
#include "ompi/file/file.h"
#include "ompi/mpiext/mpiext.h"

#include "ompi/mca/hook/base/base.h"
#include "ompi/mca/op/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/smsc/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "opal/mca/pmix/base/base.h"

#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/util/timings.h"
#include "opal/mca/mpool/base/mpool_base_tree.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/clock_gettime.h"

ompi_predefined_instance_t ompi_mpi_instance_null = {{{{0}}}};

#if defined(OPAL_RECURSIVE_MUTEX_STATIC_INIT)
static opal_recursive_mutex_t instance_lock = OPAL_RECURSIVE_MUTEX_STATIC_INIT;
#elif defined(OPAL_HAVE_ATTRIBUTE_CONSTRUCTOR)
static opal_recursive_mutex_t instance_lock;
__opal_attribute_constructor__ static void instance_lock_init(void) {
    OBJ_CONSTRUCT(&instance_lock, opal_recursive_mutex_t);
}
#else
#error "No support for recursive mutexes available on this platform."
#endif  /* defined(OPAL_RECURSIVE_MUTEX_STATIC_INIT) */

/** MPI_Init instance */
ompi_instance_t *ompi_mpi_instance_default = NULL;

/**
 * @brief: Base timer initialization. All timers returned to the user via MPI_Wtime
 *         are relative to this timer. Setting it early in during the common
 *         initialization (world or session model) allows for measuring the cost of
 *         the MPI initialization.
 */
struct timespec ompi_wtime_time_origin = {.tv_sec = 0};

enum {
    OMPI_INSTANCE_INITIALIZING = -1,
    OMPI_INSTANCE_FINALIZING   = -2,
};

opal_atomic_int32_t ompi_instance_count = 0;

static const char *ompi_instance_builtin_psets[] = {
    "mpi://WORLD",
    "mpi://SELF",
    "mpix://SHARED",
};

static const int32_t ompi_instance_builtin_count = 3;

/** finalization functions that need to be called on teardown */
static opal_finalize_domain_t ompi_instance_basic_domain;
static opal_finalize_domain_t ompi_instance_common_domain;

static void ompi_instance_construct (ompi_instance_t *instance)
{
    instance->i_f_to_c_index = opal_pointer_array_add (&ompi_instance_f_to_c_table, instance);
    instance->i_name[0] = '\0';
    instance->i_flags = 0;
    instance->i_keyhash = NULL;
    OBJ_CONSTRUCT(&instance->s_lock, opal_mutex_t);
    instance->errhandler_type = OMPI_ERRHANDLER_TYPE_INSTANCE;
}

static void ompi_instance_destruct(ompi_instance_t *instance)
{
    OBJ_DESTRUCT(&instance->s_lock);
}

OBJ_CLASS_INSTANCE(ompi_instance_t, opal_infosubscriber_t, ompi_instance_construct, ompi_instance_destruct);

/* NTH: frameworks needed by MPI */
static mca_base_framework_t *ompi_framework_dependencies[] = {
    &ompi_hook_base_framework, &ompi_op_base_framework,
    &opal_allocator_base_framework, &opal_rcache_base_framework, &opal_mpool_base_framework, &opal_smsc_base_framework,
    &ompi_bml_base_framework, &ompi_pml_base_framework, &ompi_coll_base_framework,
    &ompi_osc_base_framework, NULL,
};

static mca_base_framework_t *ompi_lazy_frameworks[] = {
    &ompi_io_base_framework, &ompi_topo_base_framework, NULL,
};


static int ompi_mpi_instance_finalize_common (void);

/*
 * Hash tables for MPI_Type_create_f90* functions
 */
opal_hash_table_t ompi_mpi_f90_integer_hashtable = {{0}};
opal_hash_table_t ompi_mpi_f90_real_hashtable = {{0}};
opal_hash_table_t ompi_mpi_f90_complex_hashtable = {{0}};

static size_t  ompi_mpi_instance_num_pmix_psets;
static char  **ompi_mpi_instance_pmix_psets;
/*
 * Per MPI-2:9.5.3, MPI_REGISTER_DATAREP is a memory leak.  There is
 * no way to *de*register datareps once they've been registered.  So
 * we have to track all registrations here so that they can be
 * de-registered during MPI_FINALIZE so that memory-tracking debuggers
 * don't show Open MPI as leaking memory.
 */
opal_list_t ompi_registered_datareps = {{0}};

opal_pointer_array_t ompi_instance_f_to_c_table = {{0}};

/*
 * PMIx event handlers
 */

static size_t ompi_default_pmix_err_handler = 0;
static size_t ompi_ulfm_pmix_err_handler = 0;

static int ompi_instance_print_error (const char *error, int ret)
{
    /* Only print a message if one was not already printed */
    if (NULL != error && OMPI_ERR_SILENT != ret) {
        const char *err_msg = opal_strerror(ret);
        opal_show_help("help-mpi-runtime.txt",
                       "mpi_init:startup:internal-failure", true,
                       "MPI_INIT", "MPI_INIT", error, err_msg, ret);
    }

    return ret;
}

static int ompi_mpi_instance_cleanup_pml (void)
{
    /* call del_procs on all allocated procs even though some may not be known
     * to the pml layer. the pml layer is expected to be resilient and ignore
     * any unknown procs. */
    size_t nprocs = 0;
    ompi_proc_t **procs;

    procs = ompi_proc_get_allocated (&nprocs);
    MCA_PML_CALL(del_procs(procs, nprocs));
    free(procs);

    return OMPI_SUCCESS;
}

/**
 * Static functions used to configure the interactions between the OPAL and
 * the runtime.
 */
static char *_process_name_print_for_opal (const opal_process_name_t procname)
{
    ompi_process_name_t *rte_name = (ompi_process_name_t*)&procname;
    return OMPI_NAME_PRINT(rte_name);
}

static int _process_name_compare (const opal_process_name_t p1, const opal_process_name_t p2)
{
    ompi_process_name_t *o1 = (ompi_process_name_t *) &p1;
    ompi_process_name_t *o2 = (ompi_process_name_t *) &p2;
    return ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL, o1, o2);
}

static int _convert_string_to_process_name (opal_process_name_t *name, const char* name_string)
{
    return ompi_rte_convert_string_to_process_name(name, name_string);
}

static int _convert_process_name_to_string (char **name_string, const opal_process_name_t *name)
{
    return ompi_rte_convert_process_name_to_string(name_string, name);
}

static int32_t ompi_mpi_instance_init_basic_count;
static bool ompi_instance_basic_init;

void ompi_mpi_instance_release (void)
{
    opal_mutex_lock (&instance_lock);

    if (0 != --ompi_mpi_instance_init_basic_count) {
        opal_mutex_unlock (&instance_lock);
        return;
    }

    opal_argv_free (ompi_mpi_instance_pmix_psets);
    ompi_mpi_instance_pmix_psets = NULL;

    OBJ_DESTRUCT(&ompi_mpi_instance_null);

    opal_finalize_cleanup_domain (&ompi_instance_basic_domain);
    OBJ_DESTRUCT(&ompi_instance_basic_domain);

    opal_finalize_util ();

    opal_mutex_unlock (&instance_lock);
}

int ompi_mpi_instance_retain (void)
{
    int ret;

    opal_mutex_lock (&instance_lock);

    if (0 < ompi_mpi_instance_init_basic_count++) {
        opal_mutex_unlock (&instance_lock);
        return OMPI_SUCCESS;
    }

    /* Setup enough to check get/set MCA params */
    if (OPAL_SUCCESS != (ret = opal_init_util (NULL, NULL))) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_mpi_instance_init: opal_init_util failed", ret);
    }

    ompi_instance_basic_init = true;

    OBJ_CONSTRUCT(&ompi_instance_basic_domain, opal_finalize_domain_t);
    opal_finalize_domain_init (&ompi_instance_basic_domain, "ompi_mpi_instance_retain");
    opal_finalize_set_domain (&ompi_instance_basic_domain);

    /* Setup f to c table */
    OBJ_CONSTRUCT(&ompi_instance_f_to_c_table, opal_pointer_array_t);
    if (OPAL_SUCCESS != opal_pointer_array_init (&ompi_instance_f_to_c_table, 8,
                                                 OMPI_FORTRAN_HANDLE_MAX, 32)) {
        opal_mutex_unlock (&instance_lock);
        return OMPI_ERROR;
    }

    /* setup the default error handler on instance_null */
    OBJ_CONSTRUCT(&ompi_mpi_instance_null, ompi_instance_t);
    ompi_mpi_instance_null.instance.error_handler = &ompi_mpi_errors_return.eh;

    /* Convince OPAL to use our naming scheme */
    opal_process_name_print = _process_name_print_for_opal;
    opal_compare_proc = _process_name_compare;
    opal_convert_string_to_process_name = _convert_string_to_process_name;
    opal_convert_process_name_to_string = _convert_process_name_to_string;
    opal_proc_for_name = ompi_proc_for_name;

    /* Register MCA variables */
    if (OPAL_SUCCESS != (ret = ompi_mpi_register_params ())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_mpi_init: ompi_register_mca_variables failed", ret);
    }

    /* initialize error handlers */
    if (OMPI_SUCCESS != (ret = ompi_errhandler_init ())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_errhandler_init() failed", ret);
    }

    /* initialize error codes */
    if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_init ())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_mpi_errcode_init() failed", ret);
    }

    /* initialize internal error codes */
    if (OMPI_SUCCESS != (ret = ompi_errcode_intern_init ())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_errcode_intern_init() failed", ret);
    }

    /* initialize info */
    if (OMPI_SUCCESS != (ret = ompi_mpiinfo_init ())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_info_init() failed", ret);
    }

    ompi_instance_basic_init = false;

    opal_mutex_unlock (&instance_lock);

    return OMPI_SUCCESS;
}

static void fence_release(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;
    OPAL_ACQUIRE_OBJECT(active);
    *active = false;
    OPAL_POST_OBJECT(active);
}

static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;

    lock->status = status;
    lock->errhandler_ref = evhandler_ref;

    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static void evhandler_dereg_callbk(pmix_status_t status,
                                 void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;
    
    lock->status = status;

    OPAL_PMIX_WAKEUP_THREAD(lock);
}       



/**
 * @brief Function that starts up the common components needed by all instances
 */
static int ompi_mpi_instance_init_common (int argc, char **argv)
{
    int ret;
    ompi_proc_t **procs;
    size_t nprocs;
    volatile bool active;
    bool background_fence = false;
    pmix_info_t info[2];
    pmix_status_t rc;
    opal_pmix_lock_t mylock;
    OMPI_TIMING_INIT(64);

    // We intentionally don't use the OPAL timer framework here.  See
    // https://github.com/open-mpi/ompi/issues/3003 for more details.
    (void) opal_clock_gettime(&ompi_wtime_time_origin);

    ret = ompi_mpi_instance_retain ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&ompi_instance_common_domain, opal_finalize_domain_t);
    opal_finalize_domain_init (&ompi_instance_common_domain, "ompi_mpi_instance_init_common");
    opal_finalize_set_domain (&ompi_instance_common_domain);

    if (OPAL_SUCCESS != (ret = opal_arch_set_fortran_logical_size(sizeof(ompi_fortran_logical_t)))) {
        return ompi_instance_print_error ("ompi_mpi_init: opal_arch_set_fortran_logical_size failed", ret);
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
           any spawned processes and potentially cause unintended
           side-effects with launching RTE tools... */
        mca_base_var_set_value(ret, allvalue, 4, MCA_BASE_VAR_SOURCE_DEFAULT, NULL);
    }

    OMPI_TIMING_NEXT("initialization");

    /* Setup RTE */
    if (OMPI_SUCCESS != (ret = ompi_rte_init (&argc, &argv))) {
        return ompi_instance_print_error ("ompi_mpi_init: ompi_rte_init failed", ret);
    }

    /* open the ompi hook framework */
    for (int i = 0 ; ompi_framework_dependencies[i] ; ++i) {
        ret = mca_base_framework_open (ompi_framework_dependencies[i], 0);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            char error_msg[256];
            snprintf (error_msg, sizeof(error_msg), "mca_base_framework_open on %s_%s failed",
                      ompi_framework_dependencies[i]->framework_project,
                      ompi_framework_dependencies[i]->framework_name);
            return ompi_instance_print_error (error_msg, ret);
        }
    }

    OMPI_TIMING_NEXT("rte_init");
    OMPI_TIMING_IMPORT_OPAL("orte_ess_base_app_setup");
    OMPI_TIMING_IMPORT_OPAL("rte_init");

    ompi_rte_initialized = true;
    /* if we are oversubscribed, then set yield_when_idle
     * accordingly */
    if (ompi_mpi_oversubscribed) {
        ompi_mpi_yield_when_idle = true;
    }


    /* Register the default errhandler callback  */
    /* give it a name so we can distinguish it */
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "MPI-Default", PMIX_STRING);
    OPAL_PMIX_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(NULL, 0, info, 1, ompi_errhandler_callback, evhandler_reg_callbk, (void*)&mylock);
    OPAL_PMIX_WAIT_THREAD(&mylock);
    rc = mylock.status;
    ompi_default_pmix_err_handler = mylock.errhandler_ref;
    OPAL_PMIX_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_DESTRUCT(&info[0]);
    if (PMIX_SUCCESS != rc) {
        ompi_default_pmix_err_handler = 0;
        ret = opal_pmix_convert_status(rc);
        return ret;
    }

    /* Register the ULFM errhandler callback  */
    /* we want to go first */
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_PREPEND, NULL, PMIX_BOOL);
    /* give it a name so we can distinguish it */
    PMIX_INFO_LOAD(&info[1], PMIX_EVENT_HDLR_NAME, "ULFM-Event-handler", PMIX_STRING);
    OPAL_PMIX_CONSTRUCT_LOCK(&mylock);
    pmix_status_t codes[3] = {
        PMIX_ERR_PROC_TERM_WO_SYNC,
        PMIX_ERR_PROC_ABORTED_BY_SIG,
        PMIX_ERR_PROC_ABORTED
    };
    PMIx_Register_event_handler(codes, 3, info, 2, ompi_errhandler_callback, evhandler_reg_callbk, (void*)&mylock);
    OPAL_PMIX_WAIT_THREAD(&mylock);
    rc = mylock.status;
    ompi_ulfm_pmix_err_handler = mylock.errhandler_ref;
    OPAL_PMIX_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIX_INFO_DESTRUCT(&info[1]);
    if (PMIX_SUCCESS != rc) {
        ompi_ulfm_pmix_err_handler = 0;
        ret = opal_pmix_convert_status(rc);
        return ret;
    }

    /* initialize MPI_INFO_ENV */
    if (OMPI_SUCCESS != (ret = ompi_mpiinfo_init_env(0, NULL, &ompi_mpi_info_env.info))) {
        return ompi_instance_print_error ("ompi_info_init_env() failed", ret);
    }

    /* declare our presence for interlib coordination, and
     * register for callbacks when other libs declare. XXXXXX -- TODO -- figure out how
     * to specify the thread level when different instances may request different levels. */
    if (OMPI_SUCCESS != (ret = ompi_interlib_declare(MPI_THREAD_MULTIPLE, OMPI_IDENT_STRING))) {
        return ompi_instance_print_error ("ompi_interlib_declare", ret);
    }

    /* initialize datatypes. This step should be done early as it will
     * create the local convertor and local arch used in the proc
     * init.
     */
    if (OMPI_SUCCESS != (ret = ompi_datatype_init())) {
        return ompi_instance_print_error ("ompi_datatype_init() failed", ret);
    }

    /* Initialize OMPI procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        return ompi_instance_print_error ("mca_proc_init() failed", ret);
    }

    /* Initialize the op framework. This has to be done *after*
       ddt_init, but before mca_coll_base_open, since some collective
       modules (e.g., the hierarchical coll component) may need ops in
       their query function. */
    if (OMPI_SUCCESS != (ret = ompi_op_base_find_available (OPAL_ENABLE_PROGRESS_THREADS, ompi_mpi_thread_multiple))) {
        return ompi_instance_print_error ("ompi_op_base_find_available() failed", ret);
    }

    if (OMPI_SUCCESS != (ret = ompi_op_init())) {
        return ompi_instance_print_error ("ompi_op_init() failed", ret);
    }

    /* In order to reduce the common case for MPI apps (where they
       don't use MPI-2 IO or MPI-1/3 topology functions), the io and
       topo frameworks are initialized lazily, at the first use of
       relevant functions (e.g., MPI_FILE_*, MPI_CART_*, MPI_GRAPH_*),
       so they are not opened here. */

    /* Select which MPI components to use */

    if (OPAL_SUCCESS != (ret = mca_smsc_base_select())) {
        return ompi_instance_print_error ("mca_smsc_base_select() failed", ret);
    }

    if (OMPI_SUCCESS != (ret = mca_pml_base_select (OPAL_ENABLE_PROGRESS_THREADS, ompi_mpi_thread_multiple))) {
        return ompi_instance_print_error ("mca_pml_base_select() failed", ret);
    }

    OMPI_TIMING_IMPORT_OPAL("orte_init");
    OMPI_TIMING_NEXT("rte_init-commit");

    /* exchange connection info - this function may also act as a barrier
     * if data exchange is required. The modex occurs solely across procs
     * in our job. If a barrier is required, the "modex" function will
     * perform it internally */
    rc = PMIx_Commit();
    if (PMIX_SUCCESS != rc) {
        ret = opal_pmix_convert_status(rc);
        return ret;  /* TODO: need to fix this */
    }

   OMPI_TIMING_NEXT("commit");
#if (OPAL_ENABLE_TIMING)
    if (OMPI_TIMING_ENABLED && !opal_pmix_base_async_modex &&
            opal_pmix_collect_all_data && !opal_process_info.is_singleton) {
        if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0, NULL, 0))) {
            ret = opal_pmix_convert_status(rc);
            return ompi_instance_print_error ("timing: pmix-barrier-1 failed", ret);
        }
        OMPI_TIMING_NEXT("pmix-barrier-1");
        if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0, NULL, 0))) {
            return ompi_instance_print_error ("timing: pmix-barrier-2 failed", ret);
        }
        OMPI_TIMING_NEXT("pmix-barrier-2");
    }
#endif

   if (! opal_process_info.is_singleton) {
        if (opal_pmix_base_async_modex) {
            /* if we are doing an async modex, but we are collecting all
             * data, then execute the non-blocking modex in the background.
             * All calls to modex_recv will be cached until the background
             * modex completes. If collect_all_data is false, then we skip
             * the fence completely and retrieve data on-demand from the
             * source node.
             */
            if (opal_pmix_collect_all_data) {
                /* execute the fence_nb in the background to collect
                 * the data */
                background_fence = true;
                active = true;
                OPAL_POST_OBJECT(&active);
                PMIX_INFO_LOAD(&info[0], PMIX_COLLECT_DATA, &opal_pmix_collect_all_data, PMIX_BOOL);
                if( PMIX_SUCCESS != (rc = PMIx_Fence_nb(NULL, 0, NULL, 0,
                                                        fence_release,
                                                        (void*)&active))) {
                    ret = opal_pmix_convert_status(rc);
                    return ompi_instance_print_error ("PMIx_Fence_nb() failed", ret);
                }
            }
        } else {
            /* we want to do the modex - we block at this point, but we must
             * do so in a manner that allows us to call opal_progress so our
             * event library can be cycled as we have tied PMIx to that
             * event base */
            active = true;
            OPAL_POST_OBJECT(&active);
            PMIX_INFO_LOAD(&info[0], PMIX_COLLECT_DATA, &opal_pmix_collect_all_data, PMIX_BOOL);
            rc = PMIx_Fence_nb(NULL, 0, info, 1, fence_release, (void*)&active);
            if( PMIX_SUCCESS != rc) {
                ret = opal_pmix_convert_status(rc);
                return ompi_instance_print_error ("PMIx_Fence() failed", ret);
            }
            /* cannot just wait on thread as we need to call opal_progress */
            OMPI_LAZY_WAIT_FOR_COMPLETION(active);
        }
    }

    OMPI_TIMING_NEXT("modex");

    /* select buffered send allocator component to be used */
    if (OMPI_SUCCESS != (ret = mca_pml_base_bsend_init ())) {
        return ompi_instance_print_error ("mca_pml_base_bsend_init() failed", ret);
    }

    if (OMPI_SUCCESS != (ret = mca_coll_base_find_available (OPAL_ENABLE_PROGRESS_THREADS, ompi_mpi_thread_multiple))) {
        return ompi_instance_print_error ("mca_coll_base_find_available() failed", ret);
    }

    if (OMPI_SUCCESS != (ret = ompi_osc_base_find_available (OPAL_ENABLE_PROGRESS_THREADS, ompi_mpi_thread_multiple))) {
        return ompi_instance_print_error ("ompi_osc_base_find_available() failed", ret);
    }

    /* io and topo components are not selected here -- see comment
       above about the io and topo frameworks being loaded lazily */

    /* Initialize each MPI handle subsystem */
    /* initialize requests */
    if (OMPI_SUCCESS != (ret = ompi_request_init ())) {
        return ompi_instance_print_error ("ompi_request_init() failed", ret);
    }

    if (OMPI_SUCCESS != (ret = ompi_message_init ())) {
        return ompi_instance_print_error ("ompi_message_init() failed", ret);
    }

    /* initialize groups  */
    if (OMPI_SUCCESS != (ret = ompi_group_init ())) {
        return ompi_instance_print_error ("ompi_group_init() failed", ret);
    }

    ompi_mpi_instance_append_finalize (ompi_mpi_instance_cleanup_pml);

    /* initialize communicator subsystem */
    if (OMPI_SUCCESS != (ret = ompi_comm_init ())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_comm_init() failed", ret);
    }

    /* Construct predefined keyvals */

    if (OMPI_SUCCESS != (ret = ompi_attr_create_predefined_keyvals())) {
        opal_mutex_unlock (&instance_lock);
        return ompi_instance_print_error ("ompi_attr_create_predefined_keyvals() failed", ret);
    }

    if (mca_pml_base_requires_world ()) {
        /* need to set up comm world for this instance -- XXX -- FIXME -- probably won't always
         * be the case. */
        if (OMPI_SUCCESS != (ret = ompi_comm_init_mpi3 ())) {
            return ompi_instance_print_error ("ompi_comm_init_mpi3 () failed", ret);
        }
    }

    /* initialize file handles */
    if (OMPI_SUCCESS != (ret = ompi_file_init ())) {
        return ompi_instance_print_error ("ompi_file_init() failed", ret);
    }

    /* initialize windows */
    if (OMPI_SUCCESS != (ret = ompi_win_init ())) {
        return ompi_instance_print_error ("ompi_win_init() failed", ret);
    }

    /* initialize partcomm */
    if (OMPI_SUCCESS != (ret = mca_base_framework_open(&ompi_part_base_framework, 0))) {
        return ompi_instance_print_error ("mca_part_base_select() failed", ret);
    }

    if (OMPI_SUCCESS != (ret = mca_part_base_select (true, true))) {
        return ompi_instance_print_error ("mca_part_base_select() failed", ret);
    }

    /* Setup the dynamic process management (DPM) subsystem */
    if (OMPI_SUCCESS != (ret = ompi_dpm_init ())) {
        return ompi_instance_print_error ("ompi_dpm_init() failed", ret);
    }


    /* identify the architectures of remote procs and setup
     * their datatype convertors, if required
     */
    if (OMPI_SUCCESS != (ret = ompi_proc_complete_init())) {
        return ompi_instance_print_error ("ompi_proc_complete_init failed", ret);
    }

    /* start PML/BTL's */
    ret = MCA_PML_CALL(enable(true));
    if( OMPI_SUCCESS != ret ) {
        return ompi_instance_print_error ("PML control failed", ret);
    }

    /* some btls/mtls require we call add_procs with all procs in the job.
     * since the btls/mtls have no visibility here it is up to the pml to
     * convey this requirement */
    if (mca_pml_base_requires_world ()) {
        if (NULL == (procs = ompi_proc_world (&nprocs))) {
            return ompi_instance_print_error ("ompi_proc_get_allocated () failed", ret);
        }
    } else {
        /* add all allocated ompi_proc_t's to PML (below the add_procs limit this
         * behaves identically to ompi_proc_world ()) */
        if (NULL == (procs = ompi_proc_get_allocated (&nprocs))) {
            return ompi_instance_print_error ("ompi_proc_get_allocated () failed", ret);
        }
    }

    ret = MCA_PML_CALL(add_procs(procs, nprocs));
    free(procs);
    /* If we got "unreachable", then print a specific error message.
       Otherwise, if we got some other failure, fall through to print
       a generic message. */
    if (OMPI_ERR_UNREACH == ret) {
        opal_show_help("help-mpi-runtime.txt",
                       "mpi_init:startup:pml-add-procs-fail", true);
        return ret;
    } else if (OMPI_SUCCESS != ret) {
        return ompi_instance_print_error ("PML add procs failed", ret);
    }

    /* Determine the overall threadlevel support of all processes
       in MPI_COMM_WORLD. This has to be done before calling
       coll_base_comm_select, since some of the collective components
       e.g. hierarch, might create subcommunicators. The threadlevel
       requested by all processes is required in order to know
       which cid allocation algorithm can be used. */
    if (OMPI_SUCCESS != ( ret = ompi_comm_cid_init ())) {
        return ompi_instance_print_error ("ompi_mpi_init: ompi_comm_cid_init failed", ret);
    }

    /* Do we need to wait for a debugger? */
    ompi_rte_wait_for_debugger();

    /* Next timing measurement */
    OMPI_TIMING_NEXT("modex-barrier");

    if (!opal_process_info.is_singleton) {
        /* if we executed the above fence in the background, then
         * we have to wait here for it to complete. However, there
         * is no reason to do two barriers! */
        if (background_fence) {
            OMPI_LAZY_WAIT_FOR_COMPLETION(active);
        } else if (!ompi_async_mpi_init) {
            /* wait for everyone to reach this point - this is a hard
             * barrier requirement at this time, though we hope to relax
             * it at a later point */
            bool flag = false;
            active = true;
            OPAL_POST_OBJECT(&active);
            PMIX_INFO_LOAD(&info[0], PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
            if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(NULL, 0, info, 1,
                                                    fence_release, (void*)&active))) {
                ret = opal_pmix_convert_status(rc);
                return ompi_instance_print_error ("PMIx_Fence_nb() failed", ret);
            }
            OMPI_LAZY_WAIT_FOR_COMPLETION(active);
        }
    }

    /* check for timing request - get stop time and report elapsed
       time if so, then start the clock again */
    OMPI_TIMING_NEXT("barrier");

#if OPAL_ENABLE_PROGRESS_THREADS == 0
    /* Start setting up the event engine for MPI operations.  Don't
       block in the event library, so that communications don't take
       forever between procs in the dynamic code.  This will increase
       CPU utilization for the remainder of MPI_INIT when we are
       blocking on RTE-level events, but may greatly reduce non-TCP
       latency. */
    opal_progress_set_event_flag(OPAL_EVLOOP_NONBLOCK);
#endif

    /* Undo OPAL calling opal_progress_event_users_increment() during
       opal_init, to get better latency when not using TCP.  Do
       this *after* dyn_init, as dyn init uses lots of RTE
       communication and we don't want to hinder the performance of
       that code. */
    opal_progress_event_users_decrement();

    /* see if yield_when_idle was specified - if so, use it */
    opal_progress_set_yield_when_idle (ompi_mpi_yield_when_idle);

    /* negative value means use default - just don't do anything */
    if (ompi_mpi_event_tick_rate >= 0) {
        opal_progress_set_event_poll_rate(ompi_mpi_event_tick_rate);
    }

    /* At this point, we are fully configured and in MPI mode.  Any
       communication calls here will work exactly like they would in
       the user's code.  Setup the connections between procs and warm
       them up with simple sends, if requested */

    if (OMPI_SUCCESS != (ret = ompi_mpiext_init())) {
        return ompi_instance_print_error ("ompi_mpiext_init", ret);
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

    return OMPI_SUCCESS;
}

int ompi_mpi_instance_init (int ts_level,  opal_info_t *info, ompi_errhandler_t *errhandler, ompi_instance_t **instance, int argc, char **argv)
{
    ompi_instance_t *new_instance;
    int ret;

    *instance = &ompi_mpi_instance_null.instance;

    /* If thread support was enabled, then setup OPAL to allow for them by default. This must be done
     * early to prevent a race condition that can occur with orte_init(). */
    if (ts_level == MPI_THREAD_MULTIPLE) {
        opal_set_using_threads(true);
    }

    opal_mutex_lock (&instance_lock);
    if (0 == opal_atomic_fetch_add_32 (&ompi_instance_count, 1)) {
        ret = ompi_mpi_instance_init_common (argc, argv);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            opal_mutex_unlock (&instance_lock);
            return ret;
        }
    }

    new_instance = OBJ_NEW(ompi_instance_t);
    if (OPAL_UNLIKELY(NULL == new_instance)) {
        if (0 == opal_atomic_add_fetch_32 (&ompi_instance_count, -1)) {
            // We can't do anything if an error occurs here because
            // we're already in an error path, so don't even bother to
            // look at the return value.
            (void) ompi_mpi_instance_finalize_common ();
        }
        opal_mutex_unlock (&instance_lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    new_instance->error_handler = errhandler;
    OBJ_RETAIN(new_instance->error_handler);

    /* Copy info if there is one. */
    if (OPAL_UNLIKELY(NULL != info)) {
        new_instance->super.s_info = OBJ_NEW(opal_info_t);
        if (info) {
            opal_info_dup(info, &new_instance->super.s_info);
        }
    }

    *instance = new_instance;
    opal_mutex_unlock (&instance_lock);

    return OMPI_SUCCESS;
}

static int ompi_mpi_instance_finalize_common (void)
{
    uint32_t key;
    ompi_datatype_t *datatype;
    int ret;
    opal_pmix_lock_t mylock;

    /* As finalize is the last legal MPI call, we are allowed to force the release
     * of the user buffer used for bsend, before going anywhere further.
     */
    (void) mca_pml_base_bsend_detach (NULL, NULL);

    /* Shut down any bindings-specific issues: C++, F77, F90 */

    /* Remove all memory associated by MPI_REGISTER_DATAREP (per
       MPI-2:9.5.3, there is no way for an MPI application to
       *un*register datareps, but we don't want the OMPI layer causing
       memory leaks). */
    OPAL_LIST_DESTRUCT(&ompi_registered_datareps);

    /* Remove all F90 types from the hash tables */
    OPAL_HASH_TABLE_FOREACH(key, uint32, datatype, &ompi_mpi_f90_integer_hashtable)
        OBJ_RELEASE(datatype);
    OBJ_DESTRUCT(&ompi_mpi_f90_integer_hashtable);
    OPAL_HASH_TABLE_FOREACH(key, uint32, datatype, &ompi_mpi_f90_real_hashtable)
        OBJ_RELEASE(datatype);
    OBJ_DESTRUCT(&ompi_mpi_f90_real_hashtable);
    OPAL_HASH_TABLE_FOREACH(key, uint32, datatype, &ompi_mpi_f90_complex_hashtable)
        OBJ_RELEASE(datatype);
    OBJ_DESTRUCT(&ompi_mpi_f90_complex_hashtable);

    /* If requested, print out a list of memory allocated by ALLOC_MEM
       but not freed by FREE_MEM */
    if (0 != ompi_debug_show_mpi_alloc_mem_leaks) {
        mca_mpool_base_tree_print (ompi_debug_show_mpi_alloc_mem_leaks);
    }

    opal_finalize_cleanup_domain (&ompi_instance_common_domain);

    if (NULL != ompi_mpi_main_thread) {
        OBJ_RELEASE(ompi_mpi_main_thread);
        ompi_mpi_main_thread = NULL;
    }

    if (0 != ompi_default_pmix_err_handler) {
        OPAL_PMIX_CONSTRUCT_LOCK(&mylock);
        PMIx_Deregister_event_handler(ompi_default_pmix_err_handler, evhandler_dereg_callbk, &mylock);
        OPAL_PMIX_WAIT_THREAD(&mylock);
        OPAL_PMIX_DESTRUCT_LOCK(&mylock);
        ompi_default_pmix_err_handler = 0;
    }

    if (0 != ompi_ulfm_pmix_err_handler) {
        OPAL_PMIX_CONSTRUCT_LOCK(&mylock);
        PMIx_Deregister_event_handler(ompi_ulfm_pmix_err_handler, evhandler_dereg_callbk, &mylock);
        OPAL_PMIX_WAIT_THREAD(&mylock);
        OPAL_PMIX_DESTRUCT_LOCK(&mylock);
        ompi_ulfm_pmix_err_handler = 0;
    }

    /* Leave the RTE */
    if (OMPI_SUCCESS != (ret = ompi_rte_finalize())) {
        return ret;
    }

    ompi_rte_initialized = false;

    for (int i = 0 ; ompi_lazy_frameworks[i] ; ++i) {
        if (0 < ompi_lazy_frameworks[i]->framework_refcnt) {
            /* May have been "opened" multiple times. We want it closed now! */
            ompi_lazy_frameworks[i]->framework_refcnt = 1;

            ret = mca_base_framework_close (ompi_lazy_frameworks[i]);
            if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
                return ret;
            }
        }
    }

    int last_framework = 0;
    for (int i = 0 ; ompi_framework_dependencies[i] ; ++i) {
        last_framework = i;
    }

    for (int j = last_framework ; j >= 0; --j) {
        ret = mca_base_framework_close (ompi_framework_dependencies[j]);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            return ret;
        }
    }

    ompi_proc_finalize();

    ompi_mpi_instance_release ();

    return OMPI_SUCCESS;
}

int ompi_mpi_instance_finalize (ompi_instance_t **instance)
{
    int ret = OMPI_SUCCESS;

    OBJ_RELEASE(*instance);

    opal_mutex_lock (&instance_lock);
    if (0 == opal_atomic_add_fetch_32 (&ompi_instance_count, -1)) {
        ret = ompi_mpi_instance_finalize_common ();
    }
    opal_mutex_unlock (&instance_lock);

    *instance = &ompi_mpi_instance_null.instance;

    return ret;
}

static void ompi_instance_get_num_psets_complete (pmix_status_t status, 
		                                  pmix_info_t *info,
		                                  size_t ninfo,
                                                  void *cbdata, 
                                                  pmix_release_cbfunc_t release_fn,
                                                  void *release_cbdata)
{
    size_t n;
    pmix_status_t rc;
    size_t sz;
    size_t num_pmix_psets = 0;
    char *pset_names = NULL;

    opal_pmix_lock_t *lock = (opal_pmix_lock_t *) cbdata;

    for (n=0; n < ninfo; n++) {
        if (0 == strcmp(info[n].key,PMIX_QUERY_NUM_PSETS)) {
            PMIX_VALUE_UNLOAD(rc,
                              &info[n].value,
                              (void **)&num_pmix_psets,
                              &sz);
            if (rc != PMIX_SUCCESS) {
                opal_argv_free (ompi_mpi_instance_pmix_psets);
                ompi_mpi_instance_pmix_psets = NULL;
                goto done;
            }
            if (num_pmix_psets != ompi_mpi_instance_num_pmix_psets) {
                opal_argv_free (ompi_mpi_instance_pmix_psets);
                ompi_mpi_instance_pmix_psets = NULL;
            }
            ompi_mpi_instance_num_pmix_psets = num_pmix_psets;
        } else if (0 == strcmp (info[n].key, PMIX_QUERY_PSET_NAMES)) {
            if (ompi_mpi_instance_pmix_psets) {
                opal_argv_free (ompi_mpi_instance_pmix_psets);
            }
            if (NULL != pset_names) {
                free(pset_names);
                pset_names = NULL;
            }
            PMIX_VALUE_UNLOAD(rc,
                              &info[n].value,
                              (void **)&pset_names,
                              &sz);
            if (rc != PMIX_SUCCESS) {
                opal_argv_free (ompi_mpi_instance_pmix_psets);
                ompi_mpi_instance_pmix_psets = NULL;
                goto done;
            }
            ompi_mpi_instance_pmix_psets = opal_argv_split (pset_names, ',');
            ompi_mpi_instance_num_pmix_psets = opal_argv_count (ompi_mpi_instance_pmix_psets);
        }
    }

done:
    if (NULL != pset_names) {
        free(pset_names);
    }

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static void ompi_instance_refresh_pmix_psets (const char *key)
{
    pmix_status_t rc;
    pmix_query_t query;
    opal_pmix_lock_t lock;
    bool refresh = true;

    opal_mutex_lock (&instance_lock);

    PMIX_QUERY_CONSTRUCT(&query);
    PMIX_ARGV_APPEND(rc, query.keys, key);
    PMIX_INFO_CREATE(query.qualifiers, 1);
    query.nqual = 1;
    PMIX_INFO_LOAD(&query.qualifiers[0], PMIX_QUERY_REFRESH_CACHE, &refresh, PMIX_BOOL);

    OPAL_PMIX_CONSTRUCT_LOCK(&lock);

    /*
     * TODO: need to handle this better
     */
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(&query, 1, 
                                                 ompi_instance_get_num_psets_complete,
                                                 (void*)&lock))) {
       opal_mutex_unlock (&instance_lock);
       return;
    }

    OPAL_PMIX_WAIT_THREAD(&lock);
    OPAL_PMIX_DESTRUCT_LOCK(&lock);

    opal_mutex_unlock (&instance_lock);
}


int ompi_instance_get_num_psets (ompi_instance_t *instance, int *npset_names)
{
    ompi_instance_refresh_pmix_psets (PMIX_QUERY_NUM_PSETS);
    *npset_names = ompi_instance_builtin_count + ompi_mpi_instance_num_pmix_psets;

    return OMPI_SUCCESS;
}

int ompi_instance_get_nth_pset (ompi_instance_t *instance, int n, int *len, char *pset_name)
{
    if (NULL == ompi_mpi_instance_pmix_psets && n >= ompi_instance_builtin_count) {
        ompi_instance_refresh_pmix_psets (PMIX_QUERY_PSET_NAMES);
    }

    if ((size_t) n >= (ompi_instance_builtin_count + ompi_mpi_instance_num_pmix_psets) || n < 0) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (0 == *len) {
        if (n < ompi_instance_builtin_count) {
            *len = strlen(ompi_instance_builtin_psets[n]) + 1;
        } else {
            *len = strlen (ompi_mpi_instance_pmix_psets[n - ompi_instance_builtin_count]) + 1;
        }
        return OMPI_SUCCESS;
    }

    if (n < ompi_instance_builtin_count) {
        strncpy (pset_name, ompi_instance_builtin_psets[n], *len);
    } else {
        strncpy (pset_name, ompi_mpi_instance_pmix_psets[n - ompi_instance_builtin_count], *len);
    }

    return OMPI_SUCCESS;
}

static int ompi_instance_group_world (ompi_instance_t *instance, ompi_group_t **group_out)
{
    ompi_group_t *group;
    size_t size;

    size = ompi_process_info.num_procs;

    group = ompi_group_allocate (NULL,size);
    if (OPAL_UNLIKELY(NULL == group)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (size_t i = 0 ; i < size ; ++i) {
        opal_process_name_t name = {.vpid = i, .jobid = OMPI_PROC_MY_NAME->jobid};
        /* look for existing ompi_proc_t that matches this name */
        group->grp_proc_pointers[i] = (ompi_proc_t *) ompi_proc_lookup (name);
        if (NULL == group->grp_proc_pointers[i]) {
            /* set sentinel value */
            group->grp_proc_pointers[i] = (ompi_proc_t *) ompi_proc_name_to_sentinel (name);
        } else {
            OBJ_RETAIN (group->grp_proc_pointers[i]);
        }
    }

    ompi_set_group_rank (group, ompi_proc_local());

    group->grp_instance = instance;

    *group_out = group;
    return OMPI_SUCCESS;
}

static int ompi_instance_group_shared (ompi_instance_t *instance, ompi_group_t **group_out)
{
    ompi_group_t *group;
    opal_process_name_t wildcard_rank;
    int ret;
    size_t size;
    char **peers;
    char *val;

    /* Find out which processes are local */
    wildcard_rank.jobid = OMPI_PROC_MY_NAME->jobid;
    wildcard_rank.vpid = OMPI_NAME_WILDCARD->vpid;

    OPAL_MODEX_RECV_VALUE(ret, PMIX_LOCAL_PEERS, &wildcard_rank, &val, PMIX_STRING);
    if (OPAL_SUCCESS != ret || NULL == val) {
        return OMPI_ERROR;
    }

    peers = opal_argv_split(val, ',');
    free (val);
    if (OPAL_UNLIKELY(NULL == peers)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    size = opal_argv_count (peers);

    group = ompi_group_allocate (NULL,size);
    if (OPAL_UNLIKELY(NULL == group)) {
        opal_argv_free (peers);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (size_t i = 0 ; NULL != peers[i] ; ++i) {
        opal_process_name_t name = {.vpid = strtoul(peers[i], NULL, 10), .jobid = OMPI_PROC_MY_NAME->jobid};
        /* look for existing ompi_proc_t that matches this name */
        group->grp_proc_pointers[i] = (ompi_proc_t *) ompi_proc_lookup (name);
        if (NULL == group->grp_proc_pointers[i]) {
            /* set sentinel value */
            group->grp_proc_pointers[i] = (ompi_proc_t *) ompi_proc_name_to_sentinel (name);
        } else {
            OBJ_RETAIN (group->grp_proc_pointers[i]);
        }
    }

    opal_argv_free (peers);

    /* group is dense */
    ompi_set_group_rank (group, ompi_proc_local());

    group->grp_instance = instance;

    *group_out = group;
    return OMPI_SUCCESS;
}

static int ompi_instance_group_self (ompi_instance_t *instance, ompi_group_t **group_out)
{
    ompi_group_t *group;
    size_t size;

    group = OBJ_NEW(ompi_group_t);
    if (OPAL_UNLIKELY(NULL == group)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    group->grp_proc_pointers = ompi_proc_self(&size);
    group->grp_my_rank       = 0;
    group->grp_proc_count   = size;

    /* group is dense */
    OMPI_GROUP_SET_DENSE (group);

    group->grp_instance = instance;

    *group_out = group;
    return OMPI_SUCCESS;
}

static int ompi_instance_group_pmix_pset (ompi_instance_t *instance, const char *pset_name, ompi_group_t **group_out)
{
    pmix_status_t rc;
    pmix_proc_t p;
    ompi_group_t *group;
    pmix_value_t *pval = NULL;
    char *stmp = NULL;
    size_t size = 0;

    /* make the group large enough to hold world */
    group = ompi_group_allocate (NULL, ompi_process_info.num_procs);
    if (OPAL_UNLIKELY(NULL == group)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }


    for (size_t i = 0 ; i < ompi_process_info.num_procs ; ++i) {
        opal_process_name_t name = {.vpid = i, .jobid = OMPI_PROC_MY_NAME->jobid};

        OPAL_PMIX_CONVERT_NAME(&p, &name);
        rc = PMIx_Get(&p, PMIX_PSET_NAME, NULL, 0, &pval);
        if (OPAL_UNLIKELY(PMIX_SUCCESS != rc)) {
            OBJ_RELEASE(group);
            return opal_pmix_convert_status(rc);
        }

        PMIX_VALUE_UNLOAD(rc,
                          pval,
                          (void **)&stmp,
                          &size);
        if (0 != strcmp (pset_name, stmp)) {
            PMIX_VALUE_RELEASE(pval);
            free(stmp);
            continue;
        }
        PMIX_VALUE_RELEASE(pval);
        free(stmp);

        /* look for existing ompi_proc_t that matches this name */
        group->grp_proc_pointers[size] = (ompi_proc_t *) ompi_proc_lookup (name);
        if (NULL == group->grp_proc_pointers[size]) {
            /* set sentinel value */
            group->grp_proc_pointers[size] = (ompi_proc_t *) ompi_proc_name_to_sentinel (name);
        } else {
            OBJ_RETAIN (group->grp_proc_pointers[size]);
        }
        ++size;
    }

    /* shrink the proc array if needed */
    if (size < (size_t) group->grp_proc_count) {
        void *tmp = realloc (group->grp_proc_pointers, size * sizeof (group->grp_proc_pointers[0]));
        if (OPAL_UNLIKELY(NULL == tmp)) {
            OBJ_RELEASE(group);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        group->grp_proc_pointers = (ompi_proc_t **) tmp;
        group->grp_proc_count = (int) size;
    }

    ompi_set_group_rank (group, ompi_proc_local());

    group->grp_instance = instance;

    *group_out = group;
    return OMPI_SUCCESS;
}

static int ompi_instance_get_pmix_pset_size (ompi_instance_t *instance, const char *pset_name, size_t *size_out)
{
    pmix_status_t rc;
    pmix_proc_t p;
    pmix_value_t *pval = NULL;
    size_t size = 0;
    char *stmp = NULL;

    for (size_t i = 0 ; i < ompi_process_info.num_procs ; ++i) {
        opal_process_name_t name = {.vpid = i, .jobid = OMPI_PROC_MY_NAME->jobid};

        OPAL_PMIX_CONVERT_NAME(&p, &name);
        rc = PMIx_Get(&p, PMIX_PSET_NAME, NULL, 0, &pval);
        if (OPAL_UNLIKELY(PMIX_SUCCESS != rc)) {
            return opal_pmix_convert_status(rc);
        }

        PMIX_VALUE_UNLOAD(rc,
                          pval,
                          (void **)&stmp,
                          &size);

        size += (0 == strcmp (pset_name, stmp));
        PMIX_VALUE_RELEASE(pval);
        free(stmp);
        stmp = NULL;

        ++size;
    }

    *size_out = size;

    return OMPI_SUCCESS;
}

int ompi_group_from_pset (ompi_instance_t *instance, const char *pset_name, ompi_group_t **group_out)
{
    if (NULL == group_out) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (0 == strncmp (pset_name, "mpi://", 6)) {
        pset_name += 6;
        if (0 == strcasecmp (pset_name, "WORLD")) {
            return ompi_instance_group_world (instance, group_out);
        }
        if (0 == strcasecmp (pset_name, "SELF")) {
            return ompi_instance_group_self (instance, group_out);
        }
    }

    if (0 == strncmp (pset_name, "mpix://", 7)) {
        pset_name += 7;
        if (0 == strcasecmp (pset_name, "SHARED")) {
            return ompi_instance_group_shared (instance, group_out);
        }
    }

    return ompi_instance_group_pmix_pset (instance, pset_name, group_out);
}

int ompi_instance_get_pset_info (ompi_instance_t *instance, const char *pset_name, opal_info_t **info_used)
{
    ompi_info_t *info = ompi_info_allocate ();
    char tmp[16];
    size_t size = 0UL;
    int ret = OMPI_SUCCESS ;

    *info_used = (opal_info_t *) MPI_INFO_NULL;

    if (OPAL_UNLIKELY(NULL == info)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (0 == strncmp (pset_name, "mpi://", 6)) {
        pset_name += 6;
        if (0 == strcasecmp (pset_name, "WORLD")) {
            size = ompi_process_info.num_procs;
        } else if (0 == strcasecmp (pset_name, "SELF")) {
            size = 1;
        } 
    } else if (0 == strncmp (pset_name, "mpix://", 7)) {
        pset_name += 7;
        if (0 == strcasecmp (pset_name, "SHARED")) {
            size = ompi_process_info.num_local_peers + 1;
        }
    } else {
        ret = ompi_instance_get_pmix_pset_size (instance, pset_name, &size);
    }

    if (OMPI_SUCCESS == ret) {
        snprintf (tmp, 16, "%" PRIsize_t, size);
        ret = opal_info_set (&info->super, MPI_INFO_KEY_SESSION_PSET_SIZE, tmp);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            ompi_info_free (&info);
            return ret;
        }
        *info_used = &info->super;
    } else { 
        ompi_info_free (&info);
    }

    return ret;
}
