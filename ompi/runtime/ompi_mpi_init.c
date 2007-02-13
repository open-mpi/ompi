/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006      University of Houston. All rights reserved.
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

#include "mpi.h"
#include "opal/mca/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/threads.h"
#include "opal/util/show_help.h"
#include "opal/util/stacktrace.h"
#include "opal/runtime/opal.h"
#include "opal/event/event.h"

#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/info/info.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"
#include "ompi/file/file.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/allocator/allocator.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"

/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;

opal_thread_t *ompi_mpi_main_thread = NULL;

bool ompi_mpi_maffinity_setup = false;

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
 * NOTE: See the big comment in ompi/mpi/f77/constants.h about why we
 * have four symbols for each of the common blocks (e.g., the Fortran
 * equivalent(s) of MPI_STATUS_IGNORE).  Here, we can only have *one*
 * value (not four).  So the only thing we can do is make it equal to
 * the fortran compiler convention that was selected at configure
 * time.  Note that this is also true for the value of .TRUE. from the
 * Fortran compiler, so even though Open MPI supports all four Fortran
 * symbol conventions, it can only support one convention for the two
 * C constants (MPI_FORTRAN_STATUS[ES]_IGNORE) and only support one
 * compiler for the value of .TRUE.  Ugh!!
 *
 * Note that the casts here are ok -- we're *only* comparing pointer
 * values (i.e., they'll never be de-referenced).  The global symbols
 * are actually of type (ompi_fortran_common_t) (for alignment
 * issues), but MPI says that MPI_F_STATUS[ES]_IGNORE must be of type
 * (MPI_Fint*).  Hence, we have to cast to make compilers not
 * complain.
 */
#if OMPI_WANT_F77_BINDINGS
#  if OMPI_F77_CAPS
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUS_IGNORE;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &MPI_FORTRAN_STATUSES_IGNORE;
#  elif OMPI_F77_PLAIN
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore;
#  elif OMPI_F77_SINGLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore_;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore_;
#  elif OMPI_F77_DOUBLE_UNDERSCORE
MPI_Fint *MPI_F_STATUS_IGNORE = (MPI_Fint*) &mpi_fortran_status_ignore__;
MPI_Fint *MPI_F_STATUSES_IGNORE = (MPI_Fint*) &mpi_fortran_statuses_ignore__;
#  else
#    error Unrecognized Fortran 77 name mangling scheme
#  endif
#else
MPI_Fint *MPI_F_STATUS_IGNORE = NULL;
MPI_Fint *MPI_F_STATUSES_IGNORE = NULL;
#endif  /* OMPI_WANT_F77_BINDINGS */


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

INST(int, MPI_FORTRAN_BOTTOM, mpi_fortran_bottom,
     mpi_fortran_bottom_, mpi_fortran_bottom__);
INST(int, MPI_FORTRAN_IN_PLACE, mpi_fortran_in_place,
     mpi_fortran_in_place_, mpi_fortran_in_place__);
INST(char *, MPI_FORTRAN_ARGV_NULL, mpi_fortran_argv_null,
     mpi_fortran_argv_null_, mpi_fortran_argv_null__);
INST(double, MPI_FORTRAN_ARGVS_NULL, mpi_fortran_argvs_null,
     mpi_fortran_argvs_null_, mpi_fortran_argvs_null__);
INST(int *, MPI_FORTRAN_ERRCODES_IGNORE, mpi_fortran_errcodes_ignore,
     mpi_fortran_errcodes_ignore_, mpi_fortran_errcodes_ignore__);
INST(int *, MPI_FORTRAN_STATUS_IGNORE, mpi_fortran_status_ignore,
     mpi_fortran_status_ignore_, mpi_fortran_status_ignore__);
INST (double, MPI_FORTRAN_STATUSES_IGNORE, mpi_fortran_statuses_ignore,
      mpi_fortran_statuses_ignore_, mpi_fortran_statuses_ignore__);


int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret;
    ompi_proc_t** procs;
    size_t nprocs;
    char *error = NULL;
    bool compound_cmd = false;
    bool timing = false;
    int param, value;
    struct timeval ompistart, ompistop, stg2start, stg3start;

    /* Join the run-time environment - do the things that don't hit
       the registry */

    if (ORTE_SUCCESS != (ret = opal_init())) {
        error = "ompi_mpi_init: opal_init failed";
        goto error;
    }

    /* check to see if we want timing information */
    param = mca_base_param_reg_int_name("ompi", "timing",
                                        "Request that critical timing loops be measured",
                                        false, false, 0, &value);
    if (value != 0) {
        timing = true;
        if (0 != gettimeofday(&ompistart, NULL)) {
            opal_output(0, "ompi_mpi_init: could not obtain start time");
            ompistart.tv_sec = 0;
            ompistart.tv_usec = 0;
        }
    }
    
    /* Setup ORTE stage 1, note that we are not infrastructre  */
    
    if (ORTE_SUCCESS != (ret = orte_init_stage1(false))) {
        error = "ompi_mpi_init: orte_init_stage1 failed";
        goto error;
    }

    /* If we are not the seed nor a singleton, AND we have not set the
       orte_debug flag, then start recording the compound command that
       starts us up.  if we are the seed or a singleton, then don't do
       this - the registry is local, so we'll just drive it
       directly */

    if (orte_process_info.seed ||
        NULL == orte_process_info.ns_replica ||
        orte_debug_flag) {
        compound_cmd = false;
    } else {
        if (ORTE_SUCCESS != (ret = orte_gpr.begin_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
            error = "ompi_mpi_init: orte_gpr.begin_compound_cmd failed";
            goto error;
        }
        compound_cmd = true;
    }

    /* Now do the things that hit the registry */

    if (ORTE_SUCCESS != (ret = orte_init_stage2())) {
        ORTE_ERROR_LOG(ret);
        error = "ompi_mpi_init: orte_init_stage2 failed";
        goto error;
    }

    /* Once we've joined the RTE, see if any MCA parameters were
       passed to the MPI level */

    if (OMPI_SUCCESS != (ret = ompi_mpi_register_params())) {
        error = "mca_mpi_register_params() failed";
        goto error;
    }

    /* Setup process affinity */

    if (ompi_mpi_paffinity_alone) {
        int param, value;
        bool set = false;
        param = mca_base_param_find("mpi", NULL, "paffinity_processor");
        if (param >= 0) {
            if (OMPI_SUCCESS == mca_base_param_lookup_int(param, &value)) {
                if (value >= 0) {
                    if (OPAL_SUCCESS == opal_paffinity_base_set(value)) {
                        set = true;
                    }
                }
            }
            if (!set) {
                char *vpid;
                orte_ns.get_vpid_string(&vpid, orte_process_info.my_name);
                opal_show_help("help-mpi-runtime",
                               "mpi_init:startup:paffinity-unavailable", 
                               true, vpid);
                free(vpid);
            }

            /* If we were able to set processor affinity, try setting
               up memory affinity */

            else {
                if (OPAL_SUCCESS == opal_maffinity_base_open() &&
                    OPAL_SUCCESS == opal_maffinity_base_select()) {
                    ompi_mpi_maffinity_setup = true;
                }
            }
        }
    }

    /* initialize datatypes. This step should be done early as it will
     * create the local convertor and local arch used in the proc
     * init.
     */
    if (OMPI_SUCCESS != (ret = ompi_ddt_init())) {
        error = "ompi_ddt_init() failed";
        goto error;
    }

    /* Initialize OMPI procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        error = "mca_proc_init() failed";
        goto error;
    }

    /* initialize the progress engine for MPI functionality */
    if (OMPI_SUCCESS != opal_progress_mpi_init()) {
        error = "opal_progress_mpi_init() failed";
        goto error;
    }


    /* initialize ops. This has to be done *after* ddt_init, but
       befor mca_coll_base_open, since come collective modules
       (e.g. the hierarchical) need them in the query function
    */
    if (OMPI_SUCCESS != (ret = ompi_op_init())) {
        error = "ompi_op_init() failed";
        goto error;
    }


    /* Open up MPI-related MCA components */

    if (OMPI_SUCCESS != (ret = mca_allocator_base_open())) {
        error = "mca_allocator_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_rcache_base_open())) {
        error = "mca_rcache_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_open())) {
        error = "mca_mpool_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_pml_base_open())) {
        error = "mca_pml_base_open() failed";
        goto error;
    }
    if (OMPI_SUCCESS != (ret = mca_coll_base_open())) {
        error = "mca_coll_base_open() failed";
        goto error;
    }

    if (OMPI_SUCCESS != (ret = ompi_osc_base_open())) {
        error = "ompi_osc_base_open() failed";
        goto error;
    }

    /* In order to reduce the common case for MPI apps (where they
       don't use MPI-2 IO or MPI-1 topology functions), the io and
       topo frameworks are initialized lazily, at the first use of
       relevant functions (e.g., MPI_FILE_*, MPI_CART_*, MPI_GRAPH_*),
       so they are not opened here. */

    /* Initialize module exchange */

    if (OMPI_SUCCESS != (ret = mca_pml_base_modex_init())) {
        error = "mca_pml_base_modex_init() failed";
        goto error;
    }

    /* Select which MPI components to use */

    if (OMPI_SUCCESS != 
        (ret = mca_mpool_base_init(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_mpool_base_init() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_pml_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_pml_base_select() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                            OMPI_ENABLE_MPI_THREADS))) {
        error = "mca_coll_base_find_available() failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = ompi_osc_base_find_available(OMPI_ENABLE_PROGRESS_THREADS,
                                           OMPI_ENABLE_MPI_THREADS))) {
        error = "ompi_osc_base_find_available() failed";
        goto error;
    }

    /* io and topo components are not selected here -- see comment
       above about the io and topo frameworks being loaded lazily */

    /* Initialize each MPI handle subsystem */
    /* initialize requests */
    if (OMPI_SUCCESS != (ret = ompi_request_init())) {
        error = "ompi_request_init() failed";
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
    /* do module exchange */
    if (OMPI_SUCCESS != (ret = mca_pml_base_modex_exchange())) {
        error = "mca_pml_base_modex_exchange() failed";
        goto error;
    }

    /* Let system know we are at STG1 Barrier */
    if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                ORTE_PROC_STATE_AT_STG1, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "set process state failed";
        goto error;
    }

    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        if (0 != gettimeofday(&ompistop, NULL)) {
            opal_output(0, "ompi_mpi_init: could not obtain stop time");
        } else {
            opal_output(0, "ompi_mpi_init: time from start to exec_compound_cmd %ld usec",
                        (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                        (ompistop.tv_usec - ompistart.tv_usec)));
            if (0 != gettimeofday(&ompistart, NULL)) {
                opal_output(0, "ompi_mpi_init: could not obtain new start time");
                ompistart.tv_sec = ompistop.tv_sec;
                ompistart.tv_usec = ompistop.tv_usec;
            }
        }
    }
    
    /* if the compound command is operative, execute it */

    if (compound_cmd) {
        if (OMPI_SUCCESS != (ret = orte_gpr.exec_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
            error = "ompi_rte_init: orte_gpr.exec_compound_cmd failed";
            goto error;
        }
    }

    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        if (0 != gettimeofday(&ompistop, NULL)) {
            opal_output(0, "ompi_mpi_init: could not obtain stop time after compound_cmd");
        } else {
            opal_output(0, "ompi_mpi_init: time to execute compound command %ld usec",
                        (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                        (ompistop.tv_usec - ompistart.tv_usec)));
        }
        gettimeofday(&ompistart, NULL);
    }
    
    /* FIRST BARRIER - WAIT FOR MSG FROM RMGR_PROC_STAGE_GATE_MGR TO ARRIVE */
    if (ORTE_SUCCESS != (ret = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid, true, NULL,
                                 orte_gpr.deliver_notify_msg))) {
        ORTE_ERROR_LOG(ret);
        error = "ompi_mpi_init: failed to see all procs register\n";
        goto error;
    }

    /* check for timing request - get start time */
    if (timing) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time to execute xcast %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&stg2start, NULL);
    }
    
    /* start PTL's */
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
    if( OMPI_SUCCESS != ret ) {
        error = "PML add procs failed";
        goto error;
    }

    MCA_PML_CALL(add_comm(&ompi_mpi_comm_world));
    MCA_PML_CALL(add_comm(&ompi_mpi_comm_self));

    /* Figure out the final MPI thread levels.  If we were not
       compiled for support for MPI threads, then don't allow
       MPI_THREAD_MULTIPLE. */

    ompi_mpi_thread_requested = requested;
    if (OMPI_HAVE_THREAD_SUPPORT == 0) {
        ompi_mpi_thread_provided = *provided = MPI_THREAD_SINGLE;
        ompi_mpi_main_thread = NULL;
    } else if (OMPI_ENABLE_MPI_THREADS == 1) {
        ompi_mpi_thread_provided = *provided = requested;
        ompi_mpi_main_thread = opal_thread_get_self();
    } else {
        if (MPI_THREAD_MULTIPLE == requested) {
            ompi_mpi_thread_provided = *provided = MPI_THREAD_SERIALIZED;
        } else {
            ompi_mpi_thread_provided = *provided = requested;
        }
        ompi_mpi_main_thread = opal_thread_get_self();
    }

    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == 
                                MPI_THREAD_MULTIPLE);
    if ((OMPI_ENABLE_PROGRESS_THREADS == 1) ||
        (*provided != MPI_THREAD_SINGLE)) {
        opal_set_using_threads(true);
    }

    /* Init coll for the comms */

    if (OMPI_SUCCESS !=
        (ret = mca_coll_base_comm_select(MPI_COMM_WORLD, NULL))) {
        error = "mca_coll_base_comm_select(MPI_COMM_WORLD) failed";
        goto error;
    }

    if (OMPI_SUCCESS != 
        (ret = mca_coll_base_comm_select(MPI_COMM_SELF, NULL))) {
        error = "mca_coll_base_comm_select(MPI_COMM_SELF) failed";
        goto error;
    }

#if OMPI_ENABLE_PROGRESS_THREADS && 0
    /* BWB - XXX - FIXME - is this actually correct? */
    /* setup I/O forwarding */
    if (orte_process_info.seed == false) {
        if (OMPI_SUCCESS != (ret = ompi_mpi_init_io())) {
            error = "ompi_rte_init_io failed";
            goto error;
        }
    }
#endif

    /*
     * Dump all MCA parameters if requested
     */
    if (ompi_mpi_show_mca_params) {
       ompi_show_all_mca_params(ompi_mpi_comm_world.c_my_rank, 
                                nprocs, 
                                orte_system_info.nodename);
    }

    /* Let system know we are at STG2 Barrier */

    if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                ORTE_PROC_STATE_AT_STG2, 0))) {
        ORTE_ERROR_LOG(ret);
        error = "set process state failed";
        goto error;
    }

    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        if (0 != gettimeofday(&ompistop, NULL)) {
            opal_output(0, "ompi_mpi_init: could not obtain stop time after compound_cmd");
        } else {
            opal_output(0, "ompi_mpi_init: time from stage1 to stage2 %ld usec",
                        (long int)((ompistop.tv_sec - stg2start.tv_sec)*1000000 +
                        (ompistop.tv_usec - stg2start.tv_usec)));
        }
    }
    
    /* Second barrier -- wait for message from
       RMGR_PROC_STAGE_GATE_MGR to arrive */

    if (ORTE_SUCCESS != (ret = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid, false, NULL,
                                               orte_gpr.deliver_notify_msg))) {
        ORTE_ERROR_LOG(ret);
        error = "ompi_mpi_init: failed to see all procs register\n";
        goto error;
    }

    /* check for timing request - get start time */
    if (timing) {
        if (0 != gettimeofday(&stg3start, NULL)) {
            opal_output(0, "ompi_mpi_init: could not obtain start time for stg3");
        }
    }

    /* wire up the oob interface, if requested.  Do this here because
       it will go much faster before the event library is switched
       into non-blocking mode */
    if (ompi_mpi_preconnect_oob) { 
        if (OMPI_SUCCESS != (ret = ompi_init_do_oob_preconnect())) {
            error = "ompi_mpi_do_preconnect_oob() failed";
            goto error;
        }
    }

    /* check for timing request - get stop time and report elapsed
       time if so, then start the clock again */
    if (timing) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from stage 2 cast to complete oob wireup %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
        gettimeofday(&ompistart, NULL);
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 0
    /* Start setting up the event engine for MPI operations.  Don't
       block in the event library, so that communications don't take
       forever between procs in the dynamic code.  This will increase
       CPU utilization for the remainder of MPI_INIT when we are
       blocking on ORTE-level events, but may greatly reduce non-TCP
       latency. */
    opal_progress_events(OPAL_EVLOOP_NONBLOCK);
#endif
    
    /* Check whether we have been spawned or not.  We introduce that
       at the very end, since we need collectives, datatypes, ptls
       etc. up and running here.... */
    if (OMPI_SUCCESS != (ret = ompi_comm_dyn_init())) {
        error = "ompi_comm_dyn_init() failed";
        goto error;
    }

 error:
    if (ret != OMPI_SUCCESS) {
        const char *err_msg = opal_strerror(ret);
        opal_show_help("help-mpi-runtime",
                       "mpi_init:startup:internal-failure", true,
                       "MPI_INIT", "MPI_INIT", error, err_msg, ret);
        return ret;
    }

    /* put the event library in "high performance MPI mode" */
    if (OMPI_SUCCESS != (ret = opal_progress_mpi_enable())) {
        error = "opal_progress_mpi_enable() failed";
        /* This will loop back up above, but ret != OMPI_SUCCESS, so
           we'll end up returning out of this function before getting
           here (and therefore avoiding an infinite loop) */
        goto error;
    }

    /* If we want the connection warmup, go do it */
   if (ompi_mpi_preconnect_all) { 
       if (OMPI_SUCCESS != (ret = ompi_init_do_preconnect())) {
           error = "ompi_mpi_do_preconnect_all() failed";
           /* This will loop back up above, but ret != OMPI_SUCCESS,
              so we'll end up returning out of this function before
              getting here (and therefore avoiding an infinite
              loop) */
           goto error;
       }
    }

    /* All done.  Wasn't that simple? */

    ompi_mpi_initialized = true;

    if (orte_debug_flag) {
        opal_output(0, "[%lu,%lu,%lu] ompi_mpi_init completed",
                    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    /* Do we need to wait for a TotalView-like debugger? */
    ompi_wait_for_totalview();

    /* check for timing request - get stop time and report elapsed time if so */
    if (timing) {
        gettimeofday(&ompistop, NULL);
        opal_output(0, "ompi_mpi_init[%ld]: time from oob wireup to complete mpi_init %ld usec",
                    (long)ORTE_PROC_MY_NAME->vpid,
                    (long int)((ompistop.tv_sec - ompistart.tv_sec)*1000000 +
                               (ompistop.tv_usec - ompistart.tv_usec)));
    }
    
    return MPI_SUCCESS;
}
