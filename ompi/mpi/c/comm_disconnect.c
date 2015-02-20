/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_disconnect = PMPI_Comm_disconnect
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/dstore/dstore.h"
#include "orte/mca/oob/base/base.h"


static const char FUNC_NAME[] = "MPI_Comm_disconnect";


int MPI_Comm_disconnect(MPI_Comm *comm) 
{
    int ret = MPI_SUCCESS;
    ompi_proc_t ** procs = NULL;
    int proc_count = 0;

    MEMCHECKER(
        memchecker_comm(*comm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( ompi_comm_invalid (*comm))
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
    }
    
    if (MPI_COMM_WORLD == *comm || MPI_COMM_SELF == *comm ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    if ( OMPI_COMM_IS_DYNAMIC(*comm)) {
        if (OMPI_SUCCESS != ompi_dpm.disconnect (*comm)) {
            ret = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        }
        proc_count = (*comm)->c_remote_group->grp_proc_count;
        procs = alloca(proc_count * sizeof(ompi_proc_t *));
        memcpy(procs, (*comm)->c_remote_group->grp_proc_pointers, proc_count * sizeof(ompi_proc_t *));
    }
    else {
        (*comm)->c_coll.coll_barrier(*comm, (*comm)->c_coll.coll_barrier_module);
    }

    ompi_comm_free(comm);

    if (0 < proc_count) {
        int i;
        for (i=0 ; i < proc_count; i++) {
            /* FIXME
             * if btl/tcp is used, then proc->obj_reference_count is 3 but del_procs expects 2
             * (for now at least ...) , this ugly hack makes del_procs happy
             */
            OBJ_RELEASE(procs[i]);
        }
        MCA_PML_CALL(del_procs(procs, proc_count));
        for (i=0 ; i < proc_count; i++) {
            /* FIXME
             * proc->obj_reference_count is still one, which makes sense since the
             * proc is in the orte_oob_base.peers list
             * for now, manually remove it from the list and OBJ_RELEASE it
             */
            uint64_t ui64;
            orte_oob_base_peer_t * peer;
            memcpy(&ui64, &procs[i]->super.proc_name, sizeof(uint64_t));
            opal_dstore.remove(opal_dstore_internal, &procs[i]->super.proc_name, NULL);
            if (OPAL_SUCCESS == opal_hash_table_get_value_uint64(&orte_oob_base.peers, ui64, (void **)&peer)) {
                if (NULL != peer) {
                    int rc;
                    if (OPAL_SUCCESS != (rc = opal_hash_table_set_value_uint64(&orte_oob_base.peers,
                                                                               ui64, NULL))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    OBJ_RELEASE(peer);
                }
            }
            OBJ_RELEASE(procs[i]);
        }
    }

    OPAL_CR_EXIT_LIBRARY();
    return ret;
}
