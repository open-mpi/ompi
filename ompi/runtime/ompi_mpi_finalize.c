/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/base/base.h"
#include "opal/sys/atomic.h"
#include "opal/runtime/opal.h"
#include "opal/util/show_help.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/base/mpool_base_tree.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/timings.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/message/message.h"
#include "ompi/op/op.h"
#include "ompi/file/file.h"
#include "ompi/info/info.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/attribute/attribute.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/rte/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/dpm/base/base.h"
#include "ompi/mca/pubsub/base/base.h"
#include "ompi/mpiext/mpiext.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif
#include "ompi/runtime/ompi_cr.h"

extern bool ompi_enable_timing;
extern bool ompi_enable_timing_ext;

int ompi_mpi_finalize(void)
{
    int ret;
    static int32_t finalize_has_already_started = 0;
    opal_list_item_t *item;
    ompi_proc_t** procs;
    size_t nprocs;
    OPAL_TIMING_DECLARE(tm);
    OPAL_TIMING_INIT(&tm);


    /* Be a bit social if an erroneous program calls MPI_FINALIZE in
       two different threads, otherwise we may deadlock in
       ompi_comm_free() (or run into other nasty lions, tigers, or
       bears) */

    if (! opal_atomic_cmpset_32(&finalize_has_already_started, 0, 1)) {
        /* Note that if we're already finalized, we cannot raise an
           MPI exception.  The best that we can do is write something
           to stderr. */
        char hostname[MAXHOSTNAMELEN];
        pid_t pid = getpid();
        gethostname(hostname, sizeof(hostname));

        opal_show_help("help-mpi-runtime.txt",
                       "mpi_finalize:invoked_multiple_times",
                       true, hostname, pid);
        return MPI_ERR_OTHER;
    }

    ompi_mpiext_fini();

    /* Per MPI-2:4.8, we have to free MPI_COMM_SELF before doing
       anything else in MPI_FINALIZE (to include setting up such that
       MPI_FINALIZED will return true). */

    if (NULL != ompi_mpi_comm_self.comm.c_keyhash) {
        ompi_attr_delete_all(COMM_ATTR, &ompi_mpi_comm_self,
                             ompi_mpi_comm_self.comm.c_keyhash);
        OBJ_RELEASE(ompi_mpi_comm_self.comm.c_keyhash);
        ompi_mpi_comm_self.comm.c_keyhash = NULL;
    }

    /* Proceed with MPI_FINALIZE */

    ompi_mpi_finalized = true;

    /* As finalize is the last legal MPI call, we are allowed to force the release
     * of the user buffer used for bsend, before going anywhere further.
     */
    (void)mca_pml_base_bsend_detach(NULL, NULL);

#if OPAL_ENABLE_PROGRESS_THREADS == 0
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE | OPAL_EVLOOP_NONBLOCK);
#endif

    /* Redo ORTE calling opal_progress_event_users_increment() during
       MPI lifetime, to get better latency when not using TCP */
    opal_progress_event_users_increment();

    /* check to see if we want timing information */
    OPAL_TIMING_MSTART((&tm,"time to execute finalize barrier"));

    /* NOTE: MPI-2.1 requires that MPI_FINALIZE is "collective" across
       *all* connected processes.  This only means that all processes
       have to call it.  It does *not* mean that all connected
       processes need to synchronize (either directly or indirectly).  

       For example, it is quite easy to construct complicated
       scenarios where one job is "connected" to another job via
       transitivity, but have no direct knowledge of each other.
       Consider the following case: job A spawns job B, and job B
       later spawns job C.  A "connectedness" graph looks something
       like this:

           A <--> B <--> C

       So what are we *supposed* to do in this case?  If job A is
       still connected to B when it calls FINALIZE, should it block
       until jobs B and C also call FINALIZE?

       After lengthy discussions many times over the course of this
       project, the issue was finally decided at the Louisville Feb
       2009 meeting: no.

       Rationale:

       - "Collective" does not mean synchronizing.  It only means that
         every process call it.  Hence, in this scenario, every
         process in A, B, and C must call FINALIZE.

       - KEY POINT: if A calls FINALIZE, then it is erroneous for B or
         C to try to communicate with A again.

       - Hence, OMPI is *correct* to only effect a barrier across each
         jobs' MPI_COMM_WORLD before exiting.  Specifically, if A
         calls FINALIZE long before B or C, it's *correct* if A exits
         at any time (and doesn't notify B or C that it is exiting).

       - Arguably, if B or C do try to communicate with the now-gone
         A, OMPI should try to print a nice error ("you tried to
         communicate with a job that is already gone...") instead of
         segv or other Badness.  However, that is an *extremely*
         difficult problem -- sure, it's easy for A to tell B that it
         is finalizing, but how can A tell C?  A doesn't even know
         about C.  You'd need to construct a "connected" graph in a
         distributed fashion, which is fraught with race conditions,
         etc.

      Hence, our conclusion is: OMPI is *correct* in its current
      behavior (of only doing a barrier across its own COMM_WORLD)
      before exiting.  Any problems that occur are as a result of
      erroneous MPI applications.  We *could* tighten up the erroneous
      cases and ensure that we print nice error messages / don't
      crash, but that is such a difficult problem that we decided we
      have many other, much higher priority issues to handle that deal
      with non-erroneous cases. */

    /* Wait for everyone to reach this point.  This is a grpcomm
       barrier instead of an MPI barrier for (at least) two reasons:

       1. An MPI barrier doesn't ensure that all messages have been
          transmitted before exiting (e.g., a BTL can lie and buffer a
          message without actually injecting it to the network, and
          therefore require further calls to that BTL's progress), so
          the possibility of a stranded message exists.

       2. If the MPI communication is using an unreliable transport,
          there's a problem of knowing that everyone has *left* the
          barrier.  E.g., one proc can send its ACK to the barrier
          message to a peer and then leave the barrier, but the ACK
          can get lost and therefore the peer is left in the barrier.

       Point #1 has been known for a long time; point #2 emerged after
       we added the first unreliable BTL to Open MPI and fixed the
       del_procs behavior around May of 2014 (see
       https://svn.open-mpi.org/trac/ompi/ticket/4669#comment:4 for
       more details). */
    opal_pmix.fence(NULL, 0);

    /* check for timing request - get stop time and report elapsed
     time if so */
    OPAL_TIMING_MSTOP(&tm);
    OPAL_TIMING_DELTAS(ompi_enable_timing, &tm);
    OPAL_TIMING_REPORT(ompi_enable_timing_ext, &tm);
    OPAL_TIMING_RELEASE(&tm);

    /*
     * Shutdown the Checkpoint/Restart Mech.
     */
    if (OMPI_SUCCESS != (ret = ompi_cr_finalize())) {
        OMPI_ERROR_LOG(ret);
    }

    /* Shut down any bindings-specific issues: C++, F77, F90 */

    /* Remove all memory associated by MPI_REGISTER_DATAREP (per
       MPI-2:9.5.3, there is no way for an MPI application to
       *un*register datareps, but we don't want the OMPI layer causing
       memory leaks). */
    while (NULL != (item = opal_list_remove_first(&ompi_registered_datareps))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ompi_registered_datareps);

    /* Remove all F90 types from the hash tables. As the OBJ_DESTRUCT will
     * call a special destructor able to release predefined types, we can
     * simply call the OBJ_DESTRUCT on the hash table and all memory will
     * be correctly released.
     */
    OBJ_DESTRUCT( &ompi_mpi_f90_integer_hashtable );
    OBJ_DESTRUCT( &ompi_mpi_f90_real_hashtable );
    OBJ_DESTRUCT( &ompi_mpi_f90_complex_hashtable );

    /* Free communication objects */

    /* free file resources */
    if (OMPI_SUCCESS != (ret = ompi_file_finalize())) {
        return ret;
    }

    /* free window resources */
    if (OMPI_SUCCESS != (ret = ompi_win_finalize())) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = ompi_osc_base_finalize())) {
        return ret;
    }

    /* free communicator resources. this MUST come before finalizing the PML
     * as this will call into the pml */
    if (OMPI_SUCCESS != (ret = ompi_comm_finalize())) {
        return ret;
    }

    nprocs = 0;
    procs = ompi_proc_world(&nprocs);
    MCA_PML_CALL(del_procs(procs, nprocs));
    free(procs);

    /* free pml resource */ 
    if(OMPI_SUCCESS != (ret = mca_pml_base_finalize())) { 
      return ret;
    }

    /* free requests */
    if (OMPI_SUCCESS != (ret = ompi_request_finalize())) {
        return ret;
    }

    if (OMPI_SUCCESS != (ret = ompi_message_finalize())) {
        return ret;
    }

    /* If requested, print out a list of memory allocated by ALLOC_MEM
       but not freed by FREE_MEM */
    if (0 != ompi_debug_show_mpi_alloc_mem_leaks) {
        mca_mpool_base_tree_print(ompi_debug_show_mpi_alloc_mem_leaks);
    }

    /* Now that all MPI objects dealing with communications are gone,
       shut down MCA types having to do with communications */
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_pml_base_framework) ) ) {
        OMPI_ERROR_LOG(ret);
        return ret;
    }

    /* shut down buffered send code */
    mca_pml_base_bsend_fini();

#if OPAL_ENABLE_FT_CR == 1
    /*
     * Shutdown the CRCP Framework, must happen after PML shutdown
     */
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_crcp_base_framework) ) ) {
        OMPI_ERROR_LOG(ret);
        return ret;
    }
#endif

    /* Free secondary resources */

    /* free attr resources */
    if (OMPI_SUCCESS != (ret = ompi_attr_finalize())) {
        return ret;
    }

    /* free group resources */
    if (OMPI_SUCCESS != (ret = ompi_group_finalize())) {
        return ret;
    }

    /* finalize the pubsub functions */
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_pubsub_base_framework) ) ) {
        return ret;
    }
    
    /* finalize the DPM framework */
    if ( OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_dpm_base_framework))) {
        return ret;
    }
    
    /* free internal error resources */
    if (OMPI_SUCCESS != (ret = ompi_errcode_intern_finalize())) {
        return ret;
    }
     
    /* free error code resources */
    if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_finalize())) {
        return ret;
    }

    /* free errhandler resources */
    if (OMPI_SUCCESS != (ret = ompi_errhandler_finalize())) {
        return ret;
    }

    /* Free all other resources */

    /* free op resources */
    if (OMPI_SUCCESS != (ret = ompi_op_finalize())) {
        return ret;
    }

    /* free ddt resources */
    if (OMPI_SUCCESS != (ret = ompi_datatype_finalize())) {
        return ret;
    }

    /* free info resources */
    if (OMPI_SUCCESS != (ret = ompi_info_finalize())) {
        return ret;
    }

    /* Close down MCA modules */

    /* io is opened lazily, so it's only necessary to close it if it
       was actually opened */
    if (0 < ompi_io_base_framework.framework_refcnt) {
        /* May have been "opened" multiple times. We want it closed now */
        ompi_io_base_framework.framework_refcnt = 1;

        if (OMPI_SUCCESS != mca_base_framework_close(&ompi_io_base_framework)) {
            return ret;
        }
    }
    (void) mca_base_framework_close(&ompi_topo_base_framework);
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_osc_base_framework))) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_coll_base_framework))) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_bml_base_framework))) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&opal_mpool_base_framework))) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&opal_rcache_base_framework))) {
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&opal_allocator_base_framework))) {
        return ret;
    }

    /* free proc resources */
    if ( OMPI_SUCCESS != (ret = ompi_proc_finalize())) {
        return ret;
    }

    if (NULL != ompi_mpi_main_thread) {
        OBJ_RELEASE(ompi_mpi_main_thread);
        ompi_mpi_main_thread = NULL;
    }

    /* Leave the RTE */

    if (OMPI_SUCCESS != (ret = ompi_rte_finalize())) {
        return ret;
    }
    ompi_rte_initialized = false;

    /* now close the rte framework */
    if (OMPI_SUCCESS != (ret = mca_base_framework_close(&ompi_rte_base_framework) ) ) {
        OMPI_ERROR_LOG(ret);
        return ret;
    }

    if (OPAL_SUCCESS != (ret = opal_finalize_util())) {
        return ret;
    }

    /* All done */

    return MPI_SUCCESS;
}
