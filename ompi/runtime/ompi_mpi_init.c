/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2009 University of Houston. All rights reserved.
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2020 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2018 Mellanox Technologies Ltd. All rights reserved.
 *
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2021-2022 Triad National Security, LLC. All rights
 *                         reserved.
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
#include <pthread.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/threads/threads.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/stacktrace.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"
#include "opal/util/event.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/util/opal_environ.h"

#include "ompi/constants.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/interlib/interlib.h"
#include "ompi/request/request.h"
#include "ompi/message/message.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/file/file.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/dpm/dpm.h"
#include "ompi/mpiext/mpiext.h"
#include "ompi/mca/hook/base/base.h"
#include "ompi/util/timings.h"

/* newer versions of gcc have poisoned this deprecated feature */
#ifdef HAVE___MALLOC_INITIALIZE_HOOK
#include "opal/mca/memory/base/base.h"
/* So this sucks, but with OPAL in its own library that is brought in
   implicitly from libmpi, there are times when the malloc initialize
   hook in the memory component doesn't work.  So we have to do it
   from here, since any MPI code is going to call MPI_Init... */
OPAL_DECLSPEC void (*__malloc_initialize_hook) (void) =
    opal_memory_base_malloc_init_hook;
#endif

/* This is required for the boundaries of the hash tables used to store
 * the F90 types returned by the MPI_Type_create_f90_XXX functions.
 */
#include <float.h>

const char ompi_version_string[] = OMPI_IDENT_STRING;

/*
 * Global variables and symbols for the MPI layer
 */

opal_atomic_int32_t ompi_mpi_state = OMPI_MPI_STATE_NOT_INITIALIZED;
volatile bool ompi_rte_initialized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;

opal_thread_t *ompi_mpi_main_thread = NULL;

/*
 * These variables are for the MPI F08 bindings (F08 must bind Fortran
 * variables to symbols; it cannot bind Fortran variables to the
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
ompi_predefined_datatype_t *ompi_mpi_real2_addr     = &ompi_mpi_real2;
ompi_predefined_datatype_t *ompi_mpi_real4_addr     = &ompi_mpi_real4;
ompi_predefined_datatype_t *ompi_mpi_real8_addr     = &ompi_mpi_real8;
ompi_predefined_datatype_t *ompi_mpi_real16_addr    = &ompi_mpi_real16;
ompi_predefined_datatype_t *ompi_mpi_dblprec_addr   = &ompi_mpi_dblprec;
ompi_predefined_datatype_t *ompi_mpi_cplex_addr     = &ompi_mpi_cplex;
ompi_predefined_datatype_t *ompi_mpi_complex4_addr  = &ompi_mpi_complex4;
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
MPI_Fint *MPI_F08_STATUS_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUS_IGNORE;
MPI_Fint *MPI_F08_STATUSES_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUSES_IGNORE;
#  elif OMPI_FORTRAN_PLAIN
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore;
MPI_Fint *MPI_F08_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore;
MPI_Fint *MPI_F08_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore;
#  elif OMPI_FORTRAN_SINGLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore_;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore_;
MPI_Fint *MPI_F08_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore_;
MPI_Fint *MPI_F08_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore_;
#  elif OMPI_FORTRAN_DOUBLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore__;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore__;
MPI_Fint *MPI_F08_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore__;
MPI_Fint *MPI_F08_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore__;
#  else
#    error Unrecognized Fortran name mangling scheme
#  endif
#else
MPI_Fint *MPI_F_STATUS_IGNORE = NULL;
MPI_Fint *MPI_F_STATUSES_IGNORE = NULL;
MPI_Fint *MPI_F08_STATUS_IGNORE = NULL;
MPI_Fint *MPI_F08_STATUSES_IGNORE = NULL;
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
   between fortran common blocks and the corresponding C variables) and
   no linker warnings occur.

   Note that the rationale for the types of each of these variables is
   discussed in ompi/include/mpif-common.h.  Do not change the types
   without also modifying ompi/mpi/fortran/base/constants.h and
   ompi/include/mpif-common.h.
 */

#include "mpif-c-constants.h"

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

    ompi_mpi_thread_provided = *provided = requested;

    if (!ompi_mpi_main_thread) {
        ompi_mpi_main_thread = opal_thread_get_self();
    }

    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided ==
                                MPI_THREAD_MULTIPLE);
}

static void fence_release(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;
    OPAL_ACQUIRE_OBJECT(active);
    *active = false;
    OPAL_POST_OBJECT(active);
}

int ompi_mpi_init(int argc, char **argv, int requested, int *provided,
                  bool reinit_ok)
{
    int ret;
    char *error = NULL;
#if OPAL_USING_INTERNAL_PMIX
    char *evar;
#endif
    volatile bool active;
    bool background_fence = false;
    pmix_info_t info[2];
    pmix_status_t rc;
    OMPI_TIMING_INIT(64);

    ompi_hook_base_mpi_init_top(argc, argv, requested, provided);

    /* Ensure that we were not already initialized or finalized. */
    int32_t expected = OMPI_MPI_STATE_NOT_INITIALIZED;
    int32_t desired  = OMPI_MPI_STATE_INIT_STARTED;
    opal_atomic_wmb();
    if (!opal_atomic_compare_exchange_strong_32(&ompi_mpi_state, &expected,
                                                desired)) {
        // If we failed to atomically transition ompi_mpi_state from
        // NOT_INITIALIZED to INIT_STARTED, then someone else already
        // did that, and we should return.
        if (expected >= OMPI_MPI_STATE_FINALIZE_STARTED) {
            opal_show_help("help-mpi-runtime.txt",
                           "mpi_init: already finalized", true);
            return MPI_ERR_OTHER;
        } else if (expected >= OMPI_MPI_STATE_INIT_STARTED) {
            // In some cases (e.g., oshmem_shmem_init()), we may call
            // ompi_mpi_init() multiple times.  In such cases, just
            // silently return successfully once the initializing
            // thread has completed.
            if (reinit_ok) {
                while (ompi_mpi_state < OMPI_MPI_STATE_INIT_COMPLETED) {
                    usleep(1);
                }
                return MPI_SUCCESS;
            }

            opal_show_help("help-mpi-runtime.txt",
                           "mpi_init: invoked multiple times", true);
            return MPI_ERR_OTHER;
        }
    }

    /* deal with OPAL_PREFIX to ensure that an internal PMIx installation
     * is also relocated if necessary */
#if OPAL_USING_INTERNAL_PMIX
    if (NULL != (evar = getenv("OPAL_PREFIX"))) {
        opal_setenv("PMIX_PREFIX", evar, true, &environ);
    }
#endif

    ompi_mpi_thread_level(requested, provided);

    ret = ompi_mpi_instance_init (*provided, &ompi_mpi_info_null.info.super, MPI_ERRORS_ARE_FATAL, &ompi_mpi_instance_default, argc, argv);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        error = "ompi_mpi_init: ompi_mpi_instance_init failed";
        goto error;
    }

    ompi_hook_base_mpi_init_top_post_opal(argc, argv, requested, provided);

    /* initialize communicator subsystem */
    if (OMPI_SUCCESS != (ret = ompi_comm_init_mpi3 ())) {
        error = "ompi_mpi_init: ompi_comm_init_mpi3 failed";
        goto error;
    }

    /* Bozo argument check */
    if (NULL == argv && argc > 1) {
        ret = OMPI_ERR_BAD_PARAM;
        error = "argc > 1, but argv == NULL";
        goto error;
    }

    /* if we were not externally started, then we need to setup
     * some envars so the MPI_INFO_ENV can get the cmd name
     * and argv (but only if the user supplied a non-NULL argv!), and
     * the requested thread level
     */
    if (NULL == getenv("OMPI_COMMAND") && NULL != argv && NULL != argv[0]) {
        opal_setenv("OMPI_COMMAND", argv[0], true, &environ);
    }
    if (NULL == getenv("OMPI_ARGV") && 1 < argc) {
        char *tmp;
        tmp = opal_argv_join(&argv[1], ' ');
        opal_setenv("OMPI_ARGV", tmp, true, &environ);
        free(tmp);
    }

#if (OPAL_ENABLE_TIMING)
    if (OMPI_TIMING_ENABLED && !opal_pmix_base_async_modex &&
            opal_pmix_collect_all_data && !opal_process_info.is_singleton) {
        if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0, NULL, 0))) {
            ret = opal_pmix_convert_status(rc);
            error = "timing: pmix-barrier-1 failed";
            goto error;
        }
        OMPI_TIMING_NEXT("pmix-barrier-1");
        if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0, NULL, 0))) {
            ret = opal_pmix_convert_status(rc);
            error = "timing: pmix-barrier-2 failed";
            goto error;
        }
        OMPI_TIMING_NEXT("pmix-barrier-2");
    }
#endif

    if (!opal_process_info.is_singleton) {
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
                    error = "PMIx_Fence_nb() failed";
                    goto error;
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
                error = "PMIx_Fence() failed";
                goto error;
            }
            /* cannot just wait on thread as we need to call opal_progress */
            OMPI_LAZY_WAIT_FOR_COMPLETION(active);
        }
    }

    OMPI_TIMING_NEXT("modex");

    MCA_PML_CALL(add_comm(&ompi_mpi_comm_world.comm));
    MCA_PML_CALL(add_comm(&ompi_mpi_comm_self.comm));

#if OPAL_ENABLE_FT_MPI
    /* initialize the fault tolerant infrastructure (revoke, detector,
     * propagator) */
    if( ompi_ftmpi_enabled ) {
        const char *evmethod;
        rc = ompi_comm_rbcast_init();
        if( OMPI_SUCCESS != rc ) return rc;
        rc = ompi_comm_revoke_init();
        if( OMPI_SUCCESS != rc ) return rc;
        rc = ompi_comm_failure_propagator_init();
        if( OMPI_SUCCESS != rc ) return rc;
        rc = ompi_comm_failure_detector_init();
        if( OMPI_SUCCESS != rc ) return rc;

        evmethod = event_base_get_method(opal_sync_event_base);
        if( 0 == strcmp("select", evmethod) ) {
            opal_show_help("help-mpi-ft.txt", "module:event:selectbug", true);
        }
    }
#endif

    /*
     * Dump all MCA parameters if requested
     */
    if (ompi_mpi_show_mca_params) {
        ompi_show_all_mca_params(ompi_mpi_comm_world.comm.c_my_rank,
                                 ompi_process_info.num_procs,
                                 ompi_process_info.nodename);
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
                error = "PMIx_Fence_nb() failed";
                goto error;
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
    int old_event_flags = opal_progress_set_event_flag(0);
    opal_progress_set_event_flag(old_event_flags | OPAL_EVLOOP_NONBLOCK);
#endif

    /* wire up the mpi interface, if requested.  Do this after the
       non-block switch for non-TCP performance.  Do before the
       polling change as anyone with a complex wire-up is going to be
       using the oob. */
    if (OMPI_SUCCESS != (ret = ompi_init_preconnect_mpi())) {
        error = "ompi_mpi_do_preconnect_all() failed";
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

#if OPAL_ENABLE_FT_MPI
    /* start the failure detector */
    if( ompi_ftmpi_enabled ) {
        rc = ompi_comm_failure_detector_start();
        if( OMPI_SUCCESS != rc ) return rc;
    }
#endif

    /* Check whether we have been spawned or not.  We introduce that
       at the very end, since we need collectives, datatypes, ptls
       etc. up and running here.... */
    if (OMPI_SUCCESS != (ret = ompi_dpm_dyn_init())) {
        return ret;
    }

    /* Fall through */
 error:
    if (ret != OMPI_SUCCESS) {
        /* Only print a message if one was not already printed */
        if (NULL != error && OMPI_ERR_SILENT != ret) {
            const char *err_msg = opal_strerror(ret);
            opal_show_help("help-mpi-runtime.txt",
                           "mpi_init:startup:internal-failure", true,
                           "MPI_INIT", "MPI_INIT", error, err_msg, ret);
        }
        ompi_hook_base_mpi_init_error(argc, argv, requested, provided);
        OMPI_TIMING_FINALIZE;
        return ret;
    }

    /* All done.  Wasn't that simple? */
    opal_atomic_wmb();
    opal_atomic_swap_32(&ompi_mpi_state, OMPI_MPI_STATE_INIT_COMPLETED);

    /* Finish last measurement, output results
     * and clear timing structure */
    OMPI_TIMING_NEXT("barrier-finish");
    OMPI_TIMING_OUT;
    OMPI_TIMING_FINALIZE;

    ompi_hook_base_mpi_init_bottom(argc, argv, requested, provided);

    return MPI_SUCCESS;
}
