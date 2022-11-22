/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2020 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2019-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
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

#include "opal/util/event.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "opal/mca/base/base.h"
#include "opal/sys/atomic.h"
#include "opal/runtime/opal.h"
#include "opal/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/base/mpool_base_tree.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
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
#include "ompi/mca/part/base/base.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/runtime/params.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/hook/hook.h"
#include "ompi/mca/hook/base/base.h"
#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/instance/instance.h"

#include "mpi.h"
#include "ompi/constants.h"

extern bool ompi_enable_timing;

static void fence_cbfunc(pmix_status_t status, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;
    OPAL_ACQUIRE_OBJECT(active);
    *active = false;
    OPAL_POST_OBJECT(active);
}

int ompi_mpi_finalize(void)
{
    int ret = MPI_SUCCESS;
    pmix_status_t rc;
    volatile bool active;

    ompi_hook_base_mpi_finalize_top();

    int32_t state = ompi_mpi_state;
    if (state < OMPI_MPI_STATE_INIT_COMPLETED ||
        state >= OMPI_MPI_STATE_FINALIZE_STARTED) {
        /* Note that if we're not initialized or already finalized, we
           cannot raise an MPI error.  The best that we can do is
           write something to stderr. */
        const char *hostname;
        pid_t pid = getpid();
        hostname = opal_gethostname();

        if (state < OMPI_MPI_STATE_INIT_COMPLETED) {
            opal_show_help("help-mpi-runtime.txt",
                           "mpi_finalize: not initialized",
                           true, hostname, pid);
        } else if (state >= OMPI_MPI_STATE_FINALIZE_STARTED) {
            opal_show_help("help-mpi-runtime.txt",
                           "mpi_finalize:invoked_multiple_times",
                           true, hostname, pid);
        }
        return MPI_ERR_OTHER;
    }
    opal_atomic_wmb();
    opal_atomic_swap_32(&ompi_mpi_state, OMPI_MPI_STATE_FINALIZE_STARTED);

    /* Per MPI-2:4.8, we have to free MPI_COMM_SELF before doing
       anything else in MPI_FINALIZE (to include setting up such that
       MPI_FINALIZED will return true). */

    if (NULL != ompi_mpi_comm_self.comm.c_keyhash) {
        ompi_attr_delete_all(COMM_ATTR, &ompi_mpi_comm_self,
                             ompi_mpi_comm_self.comm.c_keyhash);
        OBJ_RELEASE(ompi_mpi_comm_self.comm.c_keyhash);
        ompi_mpi_comm_self.comm.c_keyhash = NULL;
    }

#if OPAL_ENABLE_FT_MPI
    if( ompi_ftmpi_enabled ) {
        ompi_communicator_t* comm = &ompi_mpi_comm_world.comm;
        OPAL_OUTPUT_VERBOSE((50, ompi_ftmpi_output_handle, "FT: Rank %d entering finalize", ompi_comm_rank(comm)));

        /* grpcomm barrier does not tolerate /new/ failures. Let's make sure
         * we drain all preexisting failures before we proceed;
         * TODO: when we have better failure support in the runtime, we can
         * remove that agreement */
        ompi_communicator_t* ncomm;
        ret = ompi_comm_shrink_internal(comm, &ncomm);
        if( MPI_SUCCESS != ret ) {
            OMPI_ERROR_LOG(ret);
            goto done;
        }
        /* do a barrier with closest neighbors in the ring, using doublering as
         * it is synchronous and will help flush all past communications */
        ret = ompi_coll_base_barrier_intra_doublering(ncomm, ncomm->c_coll->coll_barrier_module);
        if( MPI_SUCCESS != ret ) {
            OMPI_ERROR_LOG(ret);
            goto done;
        }
        OBJ_RELEASE(ncomm);
        /* End of failure drain */

        /* finalize the fault tolerant infrastructure (revoke,
         * failure propagator, etc). From now-on we do not tolerate new failures. */
        OPAL_OUTPUT_VERBOSE((50, ompi_ftmpi_output_handle, "FT: Rank %05d turning off FT", ompi_comm_rank(comm)));
        ompi_comm_failure_detector_finalize();
        ompi_comm_failure_propagator_finalize();
        ompi_comm_revoke_finalize();
        ompi_comm_rbcast_finalize();
        opal_output_verbose(40, ompi_ftmpi_output_handle, "Rank %05d: DONE WITH FINALIZE", ompi_comm_rank(comm));
    }
#endif /* OPAL_ENABLE_FT_MPI */

    /* Mark that we are past COMM_SELF destruction so that
       MPI_FINALIZED can return an accurate value (per MPI-3.1,
       FINALIZED needs to return FALSE to MPI_FINALIZED until after
       COMM_SELF is destroyed / all the attribute callbacks have been
       invoked) */
    opal_atomic_wmb();
    opal_atomic_swap_32(&ompi_mpi_state,
                        OMPI_MPI_STATE_FINALIZE_PAST_COMM_SELF_DESTRUCT);

#if OPAL_ENABLE_PROGRESS_THREADS == 0
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE | OPAL_EVLOOP_NONBLOCK);
#endif

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

    /* Wait for everyone to reach this point.  This is a PMIx
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
    if (!ompi_async_mpi_finalize && !opal_process_info.is_singleton) {
        active = true;
        OPAL_POST_OBJECT(&active);
        /* Note that use of the non-blocking PMIx fence will
         * allow us to lazily cycle calling
         * opal_progress(), which will allow any other pending
         * communications/actions to complete.  See
         * https://github.com/open-mpi/ompi/issues/1576 for the
         * original bug report. */
        if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(NULL, 0, NULL, 0, fence_cbfunc, (void*)&active))) {
            ret = opal_pmix_convert_status(rc);
            OMPI_ERROR_LOG(ret);
            /* Reset the active flag to false, to avoid waiting for
             * completion when the fence was failed. */
            active = false;
        }
        OMPI_LAZY_WAIT_FOR_COMPLETION(active);
    }

    ompi_mpi_instance_finalize (&ompi_mpi_instance_default);

    /* cleanup environment */
    opal_unsetenv("OMPI_COMMAND", &environ);
    opal_unsetenv("OMPI_ARGV", &environ);

    /* All done */

   done:
    opal_atomic_wmb();
    opal_atomic_swap_32(&ompi_mpi_state, OMPI_MPI_STATE_FINALIZE_COMPLETED);

    ompi_hook_base_mpi_finalize_bottom();

    return ret;
}
