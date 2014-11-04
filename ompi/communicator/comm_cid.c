/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Voltaire All rights reserved.
 * Copyright (c) 2006-2010 University of Houston.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/dss/dss.h"
#include "ompi/proc/proc.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "ompi/constants.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/request/request.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"

BEGIN_C_DECLS

/**
 * These functions make sure, that we determine the global result over
 * an intra communicators (simple), an inter-communicator and a
 * pseudo inter-communicator described by two separate intra-comms
 * and a bridge-comm (intercomm-create scenario).
 */


typedef int ompi_comm_cid_allredfct (int *inbuf, int* outbuf,
                                     int count, struct ompi_op_t *op,
                                     ompi_communicator_t *comm,
                                     ompi_communicator_t *bridgecomm,
                                     void* lleader, void* rleader,
                                     int send_first );

static int ompi_comm_allreduce_intra (int *inbuf, int* outbuf,
                                      int count, struct ompi_op_t *op,
                                      ompi_communicator_t *intercomm,
                                      ompi_communicator_t *bridgecomm,
                                      void* local_leader,
                                      void* remote_ledaer,
                                      int send_first );

static int ompi_comm_allreduce_inter (int *inbuf, int *outbuf,
                                      int count, struct ompi_op_t *op,
                                      ompi_communicator_t *intercomm,
                                      ompi_communicator_t *bridgecomm,
                                      void* local_leader,
                                      void* remote_leader,
                                      int send_first );

static int ompi_comm_allreduce_intra_bridge(int *inbuf, int* outbuf,
                                            int count, struct ompi_op_t *op,
                                            ompi_communicator_t *intercomm,
                                            ompi_communicator_t *bridgecomm,
                                            void* local_leader,
                                            void* remote_leader,
                                            int send_first);

static int ompi_comm_allreduce_intra_oob (int *inbuf, int* outbuf,
                                          int count, struct ompi_op_t *op,
                                          ompi_communicator_t *intercomm,
                                          ompi_communicator_t *bridgecomm,
                                          void* local_leader,
                                          void* remote_leader,
                                          int send_first );

static int ompi_comm_allreduce_group (int *inbuf, int* outbuf,
                                      int count, struct ompi_op_t *op,
                                      ompi_communicator_t *intercomm,
                                      ompi_communicator_t *bridgecomm,
                                      void* local_leader,
                                      void* remote_leader,
                                      int send_first);

/* non-blocking intracommunicator allreduce */
static int ompi_comm_allreduce_intra_nb (int *inbuf, int *outbuf,
                                         int count, struct ompi_op_t *op,
                                         ompi_communicator_t *comm,
                                         ompi_communicator_t *bridgecomm,
                                         ompi_request_t **req);

/* non-blocking intercommunicator allreduce */
static int ompi_comm_allreduce_inter_nb (int *inbuf, int *outbuf,
                                         int count, struct ompi_op_t *op,
                                         ompi_communicator_t *intercomm,
                                         ompi_communicator_t *bridgecomm,
                                         ompi_request_t **req);


static int      ompi_comm_register_cid (uint32_t contextid);
static int      ompi_comm_unregister_cid (uint32_t contextid);
static uint32_t ompi_comm_lowest_cid ( void );

struct ompi_comm_reg_t{
    opal_list_item_t super;
    uint32_t           cid;
};
typedef struct ompi_comm_reg_t ompi_comm_reg_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_comm_reg_t);

static void ompi_comm_reg_constructor(ompi_comm_reg_t *regcom);
static void ompi_comm_reg_destructor(ompi_comm_reg_t *regcom);

OBJ_CLASS_INSTANCE (ompi_comm_reg_t,
                    opal_list_item_t,
                    ompi_comm_reg_constructor,
                    ompi_comm_reg_destructor );

static opal_mutex_t ompi_cid_lock;
static opal_list_t ompi_registered_comms;


/* This variable is zero (false) if all processes in MPI_COMM_WORLD
 * did not require MPI_THREAD_MULTIPLE support, and is 1 (true) as
 * soon as at least one process requested support for THREAD_MULTIPLE */
static int ompi_comm_world_thread_level_mult=0;


int ompi_comm_cid_init (void)
{
#if OMPI_ENABLE_THREAD_MULTIPLE
    ompi_proc_t **procs, *thisproc;
    uint8_t thread_level;
    void *tlpointer;
    int ret;
    size_t i, size, numprocs;

    /** Note that the following call only returns processes
     * with the same jobid. This is on purpose, since
     * we switch for the dynamic communicators anyway
     * to the original (slower) cid allocation algorithm.
     */
    procs = ompi_proc_world ( &numprocs );

    for ( i=0; i<numprocs; i++ ) {
        thisproc = procs[i];

        ret = ompi_modex_recv_string("MPI_THREAD_LEVEL", thisproc, &tlpointer, &size);
        if (OMPI_SUCCESS == ret) {
            thread_level = *((uint8_t *) tlpointer);
            if ( OMPI_THREADLEVEL_IS_MULTIPLE (thread_level) ) {
                ompi_comm_world_thread_level_mult = 1;
                break;
            }
        } else if (OMPI_ERR_NOT_IMPLEMENTED == ret) {
            if (ompi_mpi_thread_multiple) {
                ompi_comm_world_thread_level_mult = 1;
            }
            break;
        } else {
            return ret;
        }
    }
    free(procs);
#else
    ompi_comm_world_thread_level_mult = 0; // silence compiler warning if not used
#endif

    return OMPI_SUCCESS;
}

int ompi_comm_nextcid ( ompi_communicator_t* newcomm,
                        ompi_communicator_t* comm,
                        ompi_communicator_t* bridgecomm,
                        void* local_leader,
                        void* remote_leader,
                        int mode, int send_first )
{
    int ret;
    int nextcid;
    bool flag;
    int nextlocal_cid;
    int done=0;
    int response, glresponse=0;
    int start;
    unsigned int i;

    ompi_comm_cid_allredfct* allredfnct;

    /**
     * Determine which implementation of allreduce we have to use
     * for the current scenario
     */

    switch (mode)
        {
        case OMPI_COMM_CID_INTRA:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra;
            break;
        case OMPI_COMM_CID_INTER:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_inter;
            break;
        case OMPI_COMM_CID_INTRA_BRIDGE:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra_bridge;
            break;
        case OMPI_COMM_CID_INTRA_OOB:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra_oob;
            break;
        case OMPI_COMM_CID_GROUP:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_group;
            break;
        default:
            return MPI_UNDEFINED;
            break;
        }

    ret = ompi_comm_register_cid (comm->c_contextid);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    start = ompi_mpi_communicators.lowest_free;

    while (!done) {
        /**
         * This is the real algorithm described in the doc
         */
        OPAL_THREAD_LOCK(&ompi_cid_lock);
        if (comm->c_contextid != ompi_comm_lowest_cid() ) {
            /* if not lowest cid, we do not continue, but sleep and try again */
            OPAL_THREAD_UNLOCK(&ompi_cid_lock);
            continue;
        }
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);

        nextlocal_cid = mca_pml.pml_max_contextid;
        flag = false;
        for (i=start; i < mca_pml.pml_max_contextid ; i++) {
            flag = opal_pointer_array_test_and_set_item(&ompi_mpi_communicators,
                                                        i, comm);
            if (true == flag) {
                nextlocal_cid = i;
                break;
            }
        }

        ret = (allredfnct)(&nextlocal_cid, &nextcid, 1, MPI_MAX, comm, bridgecomm,
                           local_leader, remote_leader, send_first );
        if( OMPI_SUCCESS != ret ) {
            opal_pointer_array_set_item(&ompi_mpi_communicators, nextlocal_cid, NULL);
            goto release_and_return;
        }

        if (mca_pml.pml_max_contextid == (unsigned int) nextcid) {
            /* at least one peer ran out of CIDs */
            if (flag) {
                opal_pointer_array_set_item(&ompi_mpi_communicators, nextlocal_cid, NULL);
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto release_and_return;
            }
        }

        if (nextcid == nextlocal_cid) {
            response = 1; /* fine with me */
        }
        else {
            opal_pointer_array_set_item(&ompi_mpi_communicators,
                                        nextlocal_cid, NULL);

            flag = opal_pointer_array_test_and_set_item(&ompi_mpi_communicators,
                                                        nextcid, comm );
            if (true == flag) {
                response = 1; /* works as well */
            }
            else {
                response = 0; /* nope, not acceptable */
            }
        }

        ret = (allredfnct)(&response, &glresponse, 1, MPI_MIN, comm, bridgecomm,
                           local_leader, remote_leader, send_first );
        if( OMPI_SUCCESS != ret ) {
            opal_pointer_array_set_item(&ompi_mpi_communicators, nextcid, NULL);
            goto release_and_return;
        }
        if (1 == glresponse) {
            done = 1;             /* we are done */
            break;
        }
        else if ( 0 == glresponse ) {
            if ( 1 == response ) {
                /* we could use that, but other don't agree */
                opal_pointer_array_set_item(&ompi_mpi_communicators,
                                            nextcid, NULL);
            }
            start = nextcid+1; /* that's where we can start the next round */
        }
    }

    /* set the according values to the newcomm */
    newcomm->c_contextid = nextcid;
    opal_pointer_array_set_item (&ompi_mpi_communicators, nextcid, newcomm);

 release_and_return:
    ompi_comm_unregister_cid (comm->c_contextid);

    return ret;
}

/* Non-blocking version of ompi_comm_nextcid */
struct mca_comm_nextcid_context {
    ompi_communicator_t* newcomm;
    ompi_communicator_t* comm;
    ompi_communicator_t* bridgecomm;
    int mode;
    int nextcid;
    int nextlocal_cid;
    int start;
    int flag, rflag;
};

/* find the next available local cid and start an allreduce */
static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request);
/* verify that the maximum cid is locally available and start an allreduce */
static int ompi_comm_checkcid (ompi_comm_request_t *request);
/* verify that the cid was available globally */
static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request);

int ompi_comm_nextcid_nb (ompi_communicator_t* newcomm,
                          ompi_communicator_t* comm,
                          ompi_communicator_t* bridgecomm,
                          int mode, ompi_request_t **req)
{
    struct mca_comm_nextcid_context *context;
    ompi_comm_request_t *request;
    int ret;

    /**
     * Determine which implementation of allreduce we have to use
     * for the current scenario
     */
    if (OMPI_COMM_CID_INTRA != mode && OMPI_COMM_CID_INTER != mode) {
        return MPI_UNDEFINED;
    }

    ret = ompi_comm_register_cid (comm->c_contextid);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    context = calloc (1, sizeof (*context));
    if (NULL == context) {
        ompi_comm_unregister_cid (comm->c_contextid);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request = ompi_comm_request_get ();
    if (NULL == request) {
        ompi_comm_unregister_cid (comm->c_contextid);
        free (context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context->newcomm       = newcomm;
    context->comm          = comm;
    context->bridgecomm    = bridgecomm;
    context->mode          = mode;
    context->start         = ompi_mpi_communicators.lowest_free;

    request->context = context;

    ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    ompi_comm_request_start (request);

    *req = &request->super;

    return OMPI_SUCCESS;
}

static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request)
{
    struct mca_comm_nextcid_context *context = request->context;
    ompi_request_t *subreq;
    unsigned int i;
    bool flag;
    int ret;

    /**
     * This is the real algorithm described in the doc
     */
    OPAL_THREAD_LOCK(&ompi_cid_lock);
    if (context->comm->c_contextid != ompi_comm_lowest_cid() ) {
        /* if not lowest cid, we do not continue, but sleep and try again */
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);
        ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);

        return OMPI_SUCCESS;
    }
    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    flag = false;
    context->nextlocal_cid = mca_pml.pml_max_contextid;
    for (i = context->start ; i < mca_pml.pml_max_contextid ; ++i) {
        flag = opal_pointer_array_test_and_set_item(&ompi_mpi_communicators,
                                                    i, context->comm);
        if (true == flag) {
            context->nextlocal_cid = i;
            break;
        }
    }

    if (context->mode == OMPI_COMM_CID_INTRA) {
        ret = ompi_comm_allreduce_intra_nb (&context->nextlocal_cid, &context->nextcid, 1, MPI_MAX,
                                            context->comm, context->bridgecomm, &subreq);
    } else {
        ret = ompi_comm_allreduce_inter_nb (&context->nextlocal_cid, &context->nextcid, 1, MPI_MAX,
                                            context->comm, context->bridgecomm, &subreq);
    }

    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if ((unsigned int) context->nextlocal_cid == mca_pml.pml_max_contextid) {
        /* at least one peer ran out of CIDs */
        if (flag) {
            opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, context->nextlocal_cid, NULL);
        }

        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* next we want to verify that the resulting commid is ok */
    ompi_comm_request_schedule_append (request, ompi_comm_checkcid, &subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_checkcid (ompi_comm_request_t *request)
{
    struct mca_comm_nextcid_context *context = request->context;
    ompi_request_t *subreq;
    int ret;

    context->flag = (context->nextcid == context->nextlocal_cid);

    if (!context->flag) {
        opal_pointer_array_set_item(&ompi_mpi_communicators, context->nextlocal_cid, NULL);

        context->flag = opal_pointer_array_test_and_set_item(&ompi_mpi_communicators,
                                                             context->nextcid, context->comm);
    }

    if (context->mode == OMPI_COMM_CID_INTRA) {
        ret = ompi_comm_allreduce_intra_nb (&context->flag, &context->rflag, 1, MPI_MIN, context->comm,
                                            context->bridgecomm, &subreq);
    } else {
        ret = ompi_comm_allreduce_inter_nb (&context->flag, &context->rflag, 1, MPI_MIN, context->comm,
                                            context->bridgecomm, &subreq);
    }

    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_nextcid_check_flag, &subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request)
{
    struct mca_comm_nextcid_context *context = request->context;

    if (1 == context->rflag) {
        /* set the according values to the newcomm */
        context->newcomm->c_contextid = context->nextcid;
        opal_pointer_array_set_item (&ompi_mpi_communicators, context->nextcid, context->newcomm);

        ompi_comm_unregister_cid (context->comm->c_contextid);

        /* done! */
        return OMPI_SUCCESS;
    }

    if (1 == context->flag) {
        /* we could use this cid, but other don't agree */
        opal_pointer_array_set_item(&ompi_mpi_communicators, context->nextcid, NULL);
        context->start = context->nextcid + 1; /* that's where we can start the next round */
    }

    /* try again */
    return ompi_comm_allreduce_getnextcid (request);
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
static void ompi_comm_reg_constructor (ompi_comm_reg_t *regcom)
{
    regcom->cid=MPI_UNDEFINED;
}

static void ompi_comm_reg_destructor (ompi_comm_reg_t *regcom)
{
}

void ompi_comm_reg_init (void)
{
    OBJ_CONSTRUCT(&ompi_registered_comms, opal_list_t);
    OBJ_CONSTRUCT(&ompi_cid_lock, opal_mutex_t);
}

void ompi_comm_reg_finalize (void)
{
    OBJ_DESTRUCT(&ompi_registered_comms);
    OBJ_DESTRUCT(&ompi_cid_lock);
}


static int ompi_comm_register_cid (uint32_t cid)
{
    ompi_comm_reg_t *regcom;
    ompi_comm_reg_t *newentry = OBJ_NEW(ompi_comm_reg_t);
    bool registered = false;

    do {
        /* Only one communicator function allowed in same time on the
         * same communicator.
         */
        OPAL_THREAD_LOCK(&ompi_cid_lock);

        newentry->cid = cid;
        if ( !(opal_list_is_empty (&ompi_registered_comms)) ) {
            bool ok = true;

            OPAL_LIST_FOREACH(regcom, &ompi_registered_comms, ompi_comm_reg_t) {
                if ( regcom->cid > cid ) {
                    break;
                }
#if OMPI_ENABLE_THREAD_MULTIPLE
                if( regcom->cid == cid ) {
                    /**
                     * The MPI standard state that is the user responsability to
                     * schedule the global communications in order to avoid any
                     * kind of troubles. As, managing communicators involve several
                     * collective communications, we should enforce a sequential
                     * execution order. This test only allow one communicator
                     * creation function based on the same communicator.
                     */
                    ok = false;
                    break;
                }
#endif  /* OMPI_ENABLE_THREAD_MULTIPLE */
            }
            if (ok) {
                opal_list_insert_pos (&ompi_registered_comms, (opal_list_item_t *) regcom,
                                      (opal_list_item_t *)newentry);
                registered = true;
            }
        } else {
            opal_list_append (&ompi_registered_comms, (opal_list_item_t *)newentry);
            registered = true;
        }

        /* drop the lock before trying again */
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);
    } while (!registered);

    return OMPI_SUCCESS;
}

static int ompi_comm_unregister_cid (uint32_t cid)
{
    ompi_comm_reg_t *regcom;

    OPAL_THREAD_LOCK(&ompi_cid_lock);

    OPAL_LIST_FOREACH(regcom, &ompi_registered_comms, ompi_comm_reg_t) {
        if(regcom->cid == cid) {
            opal_list_remove_item(&ompi_registered_comms, (opal_list_item_t *) regcom);
            OBJ_RELEASE(regcom);
            break;
        }
    }

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    return OMPI_SUCCESS;
}

static uint32_t ompi_comm_lowest_cid (void)
{
    ompi_comm_reg_t *regcom=NULL;
    opal_list_item_t *item=opal_list_get_first (&ompi_registered_comms);

    regcom = (ompi_comm_reg_t *)item;
    return regcom->cid;
}
/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/* This routine serves two purposes:
 * - the allreduce acts as a kind of Barrier,
 *   which avoids, that we have incoming fragments
 *   on the new communicator before everybody has set
 *   up the comm structure.
 * - some components (e.g. the collective MagPIe component
 *   might want to generate new communicators and communicate
 *   using the new comm. Thus, it can just be called after
 *   the 'barrier'.
 *
 * The reason that this routine is in comm_cid and not in
 * comm.c is, that this file contains the allreduce implementations
 * which are required, and thus we avoid having duplicate code...
 */
int ompi_comm_activate ( ompi_communicator_t** newcomm,
                         ompi_communicator_t* comm,
                         ompi_communicator_t* bridgecomm,
                         void* local_leader,
                         void* remote_leader,
                         int mode,
                         int send_first )
{
    int ret = 0;

    int ok=0, gok=0;
    ompi_comm_cid_allredfct* allredfnct;

    /* Step 1: the barrier, after which it is allowed to
     * send messages over the new communicator
     */
    switch (mode)
        {
        case OMPI_COMM_CID_INTRA:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra;
            break;
        case OMPI_COMM_CID_INTER:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_inter;
            break;
        case OMPI_COMM_CID_INTRA_BRIDGE:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra_bridge;
            break;
        case OMPI_COMM_CID_INTRA_OOB:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra_oob;
            break;
        case OMPI_COMM_CID_GROUP:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_group;
            break;
        default:
            return MPI_UNDEFINED;
            break;
        }

    if (MPI_UNDEFINED != (*newcomm)->c_local_group->grp_my_rank) {
        /* Initialize the PML stuff in the newcomm  */
        if ( OMPI_SUCCESS != (ret = MCA_PML_CALL(add_comm(*newcomm))) ) {
            goto bail_on_error;
        }

        OMPI_COMM_SET_PML_ADDED(*newcomm);
    }


    ret = (allredfnct)(&ok, &gok, 1, MPI_MIN, comm, bridgecomm,
                       local_leader, remote_leader, send_first );
    if( OMPI_SUCCESS != ret ) {
        goto bail_on_error;
    }



    /**
     * Check to see if this process is in the new communicator.
     *
     * Specifically, this function is invoked by all proceses in the
     * old communicator, regardless of whether they are in the new
     * communicator or not.  This is because it is far simpler to use
     * MPI collective functions on the old communicator to determine
     * some data for the new communicator (e.g., remote_leader) than
     * to kludge up our own pseudo-collective routines over just the
     * processes in the new communicator.  Hence, *all* processes in
     * the old communicator need to invoke this function.
     *
     * That being said, only processes in the new communicator need to
     * select a coll module for the new communicator.  More
     * specifically, proceses who are not in the new communicator
     * should *not* select a coll module -- for example,
     * ompi_comm_rank(newcomm) returns MPI_UNDEFINED for processes who
     * are not in the new communicator.  This can cause errors in the
     * selection / initialization of a coll module.  Plus, it's
     * wasteful -- processes in the new communicator will end up
     * freeing the new communicator anyway, so we might as well leave
     * the coll selection as NULL (the coll base comm unselect code
     * handles that case properly).
     */
    if (MPI_UNDEFINED == (*newcomm)->c_local_group->grp_my_rank) {
        return OMPI_SUCCESS;
    }

    /* Let the collectives components fight over who will do
       collective on this new comm.  */
    if (OMPI_SUCCESS != (ret = mca_coll_base_comm_select(*newcomm))) {
        goto bail_on_error;
    }

    /* For an inter communicator, we have to deal with the potential
     * problem of what is happening if the local_comm that we created
     * has a lower CID than the parent comm. This is not a problem
     * as long as the user calls MPI_Comm_free on the inter communicator.
     * However, if the communicators are not freed by the user but released
     * by Open MPI in MPI_Finalize, we walk through the list of still available
     * communicators and free them one by one. Thus, local_comm is freed before
     * the actual inter-communicator. However, the local_comm pointer in the
     * inter communicator will still contain the 'previous' address of the local_comm
     * and thus this will lead to a segmentation violation. In order to prevent
     * that from happening, we increase the reference counter local_comm
     * by one if its CID is lower than the parent. We cannot increase however
     *  its reference counter if the CID of local_comm is larger than
     * the CID of the inter communicators, since a regular MPI_Comm_free would
     * leave in that the case the local_comm hanging around and thus we would not
     * recycle CID's properly, which was the reason and the cause for this trouble.
     */
    if ( OMPI_COMM_IS_INTER(*newcomm)) {
        if ( OMPI_COMM_CID_IS_LOWER(*newcomm, comm)) {
            OMPI_COMM_SET_EXTRA_RETAIN (*newcomm);
            OBJ_RETAIN (*newcomm);
        }
    }


    return OMPI_SUCCESS;

 bail_on_error:
    OBJ_RELEASE(*newcomm);
    *newcomm = MPI_COMM_NULL;
    return ret;
}

/* Non-blocking version of ompi_comm_activate */
struct ompi_comm_activate_nb_context {
    ompi_communicator_t **newcomm;
    ompi_communicator_t *comm;

    /* storage for activate barrier */
    int ok;
};

static int ompi_comm_activate_nb_complete (ompi_comm_request_t *request);

int ompi_comm_activate_nb (ompi_communicator_t **newcomm,
                           ompi_communicator_t *comm,
                           ompi_communicator_t *bridgecomm,
                           int mode, ompi_request_t **req)
{
    struct ompi_comm_activate_nb_context *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq;
    int ret = 0;

    request = ompi_comm_request_get ();
    if (NULL == request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = calloc (1, sizeof (*context));
    if (NULL == context) {
        ompi_comm_request_return (request);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context->newcomm = newcomm;
    context->comm    = comm;

    request->context = context;

    if (OMPI_COMM_CID_INTRA != mode && OMPI_COMM_CID_INTER != mode) {
        return MPI_UNDEFINED;
    }

    if (MPI_UNDEFINED != (*newcomm)->c_local_group->grp_my_rank) {
        /* Initialize the PML stuff in the newcomm  */
        if ( OMPI_SUCCESS != (ret = MCA_PML_CALL(add_comm(*newcomm))) ) {
            OBJ_RELEASE(newcomm);
            *newcomm = MPI_COMM_NULL;
            return ret;
        }
        OMPI_COMM_SET_PML_ADDED(*newcomm);
    }

    /* Step 1: the barrier, after which it is allowed to
     * send messages over the new communicator
     */
    if (mode == OMPI_COMM_CID_INTRA) {
        ret = ompi_comm_allreduce_intra_nb (&context->ok, &context->ok, 1, MPI_MIN,
                                            context->comm, bridgecomm, &subreq);
    } else {
        ret = ompi_comm_allreduce_inter_nb (&context->ok, &context->ok, 1, MPI_MIN,
                                            context->comm, bridgecomm, &subreq);
    }

    if (OMPI_SUCCESS != ret) {
        ompi_comm_request_return (request);
        return ret;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_activate_nb_complete, &subreq, 1);
    ompi_comm_request_start (request);

    *req = &request->super;

    return OMPI_SUCCESS;
}

static int ompi_comm_activate_nb_complete (ompi_comm_request_t *request)
{
    struct ompi_comm_activate_nb_context *context =
        (struct ompi_comm_activate_nb_context *) request->context;
    int ret;

    /**
     * Check to see if this process is in the new communicator.
     *
     * Specifically, this function is invoked by all proceses in the
     * old communicator, regardless of whether they are in the new
     * communicator or not.  This is because it is far simpler to use
     * MPI collective functions on the old communicator to determine
     * some data for the new communicator (e.g., remote_leader) than
     * to kludge up our own pseudo-collective routines over just the
     * processes in the new communicator.  Hence, *all* processes in
     * the old communicator need to invoke this function.
     *
     * That being said, only processes in the new communicator need to
     * select a coll module for the new communicator.  More
     * specifically, proceses who are not in the new communicator
     * should *not* select a coll module -- for example,
     * ompi_comm_rank(newcomm) returns MPI_UNDEFINED for processes who
     * are not in the new communicator.  This can cause errors in the
     * selection / initialization of a coll module.  Plus, it's
     * wasteful -- processes in the new communicator will end up
     * freeing the new communicator anyway, so we might as well leave
     * the coll selection as NULL (the coll base comm unselect code
     * handles that case properly).
     */
    if (MPI_UNDEFINED == (*context->newcomm)->c_local_group->grp_my_rank) {
        return OMPI_SUCCESS;
    }

    /* Let the collectives components fight over who will do
       collective on this new comm.  */
    if (OMPI_SUCCESS != (ret = mca_coll_base_comm_select(*context->newcomm))) {
        OBJ_RELEASE(*context->newcomm);
        *context->newcomm = MPI_COMM_NULL;
        return ret;
    }

    /* For an inter communicator, we have to deal with the potential
     * problem of what is happening if the local_comm that we created
     * has a lower CID than the parent comm. This is not a problem
     * as long as the user calls MPI_Comm_free on the inter communicator.
     * However, if the communicators are not freed by the user but released
     * by Open MPI in MPI_Finalize, we walk through the list of still available
     * communicators and free them one by one. Thus, local_comm is freed before
     * the actual inter-communicator. However, the local_comm pointer in the
     * inter communicator will still contain the 'previous' address of the local_comm
     * and thus this will lead to a segmentation violation. In order to prevent
     * that from happening, we increase the reference counter local_comm
     * by one if its CID is lower than the parent. We cannot increase however
     *  its reference counter if the CID of local_comm is larger than
     * the CID of the inter communicators, since a regular MPI_Comm_free would
     * leave in that the case the local_comm hanging around and thus we would not
     * recycle CID's properly, which was the reason and the cause for this trouble.
     */
    if (OMPI_COMM_IS_INTER(*context->newcomm)) {
        if (OMPI_COMM_CID_IS_LOWER(*context->newcomm, context->comm)) {
            OMPI_COMM_SET_EXTRA_RETAIN (*context->newcomm);
            OBJ_RETAIN (*context->newcomm);
        }
    }

    /* done */
    return OMPI_SUCCESS;
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/* Arguments not used in this implementation:
 *  - bridgecomm
 *  - local_leader
 *  - remote_leader
 *  - send_first
 */
static int ompi_comm_allreduce_intra ( int *inbuf, int *outbuf,
                                       int count, struct ompi_op_t *op,
                                       ompi_communicator_t *comm,
                                       ompi_communicator_t *bridgecomm,
                                       void* local_leader,
                                       void* remote_leader,
                                       int send_first )
{
    return comm->c_coll.coll_allreduce ( inbuf, outbuf, count, MPI_INT, op, comm,
                                         comm->c_coll.coll_allreduce_module );
}

static int ompi_comm_allreduce_intra_nb (int *inbuf, int *outbuf,
                                         int count, struct ompi_op_t *op,
                                         ompi_communicator_t *comm,
                                         ompi_communicator_t *bridgecomm,
                                         ompi_request_t **req)
{
    return comm->c_coll.coll_iallreduce (inbuf, outbuf, count, MPI_INT, op, comm,
                                         req, comm->c_coll.coll_iallreduce_module);
}


/* Arguments not used in this implementation:
 *  - bridgecomm
 *  - local_leader
 *  - remote_leader
 *  - send_first
 */
static int ompi_comm_allreduce_inter ( int *inbuf, int *outbuf,
                                       int count, struct ompi_op_t *op,
                                       ompi_communicator_t *intercomm,
                                       ompi_communicator_t *bridgecomm,
                                       void* local_leader,
                                       void* remote_leader,
                                       int send_first )
{
    int local_rank, rsize;
    int rc;
    int *sbuf;
    int *tmpbuf=NULL;
    int *rcounts=NULL, scount=0;
    int *rdisps=NULL;

    if ( !OMPI_COMM_IS_INTER (intercomm)) {
        return MPI_ERR_COMM;
    }

    /* Allocate temporary arrays */
    rsize      = ompi_comm_remote_size (intercomm);
    local_rank = ompi_comm_rank ( intercomm );

    tmpbuf  = (int *) malloc ( count * sizeof(int));
    rdisps  = (int *) calloc ( rsize, sizeof(int));
    rcounts = (int *) calloc ( rsize, sizeof(int) );
    if ( OPAL_UNLIKELY (NULL == tmpbuf || NULL == rdisps || NULL == rcounts)) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    /* Execute the inter-allreduce: the result of our group will
       be in the buffer of the remote group */
    rc = intercomm->c_coll.coll_allreduce ( inbuf, tmpbuf, count, MPI_INT,
                                            op, intercomm,
                                            intercomm->c_coll.coll_allreduce_module);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( 0 == local_rank ) {
        MPI_Request req;

        /* for the allgatherv later */
        scount = count;

        /* local leader exchange their data and determine the overall result
           for both groups */
        rc = MCA_PML_CALL(irecv (outbuf, count, MPI_INT, 0,
                                 OMPI_COMM_ALLREDUCE_TAG,
                                 intercomm, &req));
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = MCA_PML_CALL(send (tmpbuf, count, MPI_INT, 0,
                                OMPI_COMM_ALLREDUCE_TAG,
                                MCA_PML_BASE_SEND_STANDARD,
                                intercomm));
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = ompi_request_wait ( &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

	ompi_op_reduce (op, tmpbuf, outbuf, count, MPI_INT);
    }

    /* distribute the overall result to all processes in the other group.
       Instead of using bcast, we are using here allgatherv, to avoid the
       possible deadlock. Else, we need an algorithm to determine,
       which group sends first in the inter-bcast and which receives
       the result first.
    */
    rcounts[0] = count;
    sbuf       = outbuf;
    rc = intercomm->c_coll.coll_allgatherv (sbuf, scount, MPI_INT, outbuf,
                                            rcounts, rdisps, MPI_INT,
                                            intercomm,
                                            intercomm->c_coll.coll_allgatherv_module);

 exit:
    if ( NULL != tmpbuf ) {
        free ( tmpbuf );
    }
    if ( NULL != rcounts ) {
        free ( rcounts );
    }
    if ( NULL != rdisps ) {
        free ( rdisps );
    }

    return (rc);
}

/* Non-blocking version of ompi_comm_allreduce_inter */
struct ompi_comm_allreduce_inter_context {
  int *inbuf;
  int *outbuf;
  int count;
  struct ompi_op_t *op;
  ompi_communicator_t *intercomm;
  ompi_communicator_t *bridgecomm;
  int *tmpbuf;
  int *rcounts;
  int *rdisps;
};

static void ompi_comm_allreduce_inter_context_free (struct ompi_comm_allreduce_inter_context *context)
{
    if (context->tmpbuf) {
        free (context->tmpbuf);
    }

    if (context->rdisps) {
        free (context->rdisps);
    }

    if (context->rcounts) {
        free (context->rcounts);
    }

    free (context);
}

static int ompi_comm_allreduce_inter_leader_exchange (ompi_comm_request_t *request);
static int ompi_comm_allreduce_inter_leader_reduce (ompi_comm_request_t *request);
static int ompi_comm_allreduce_inter_allgather (ompi_comm_request_t *request);
static int ompi_comm_allreduce_inter_allgather_complete (ompi_comm_request_t *request);

/* Arguments not used in this implementation:
 *  - bridgecomm
 */
static int ompi_comm_allreduce_inter_nb (int *inbuf, int *outbuf,
                                         int count, struct ompi_op_t *op,
                                         ompi_communicator_t *intercomm,
                                         ompi_communicator_t *bridgecomm,
                                         ompi_request_t **req)
{
    struct ompi_comm_allreduce_inter_context *context = NULL;
    ompi_comm_request_t *request = NULL;
    ompi_request_t *subreq;
    int local_rank, rsize, rc;

    if (!OMPI_COMM_IS_INTER (intercomm)) {
        return MPI_ERR_COMM;
    }

    request = ompi_comm_request_get ();
    if (NULL == request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = calloc (1, sizeof (*context));
    if (NULL == context) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    context->inbuf      = inbuf;
    context->outbuf     = outbuf;
    context->count      = count;
    context->op         = op;
    context->intercomm  = intercomm;
    context->bridgecomm = bridgecomm;

    /* Allocate temporary arrays */
    rsize      = ompi_comm_remote_size (intercomm);
    local_rank = ompi_comm_rank (intercomm);

    context->tmpbuf  = (int *) calloc (count, sizeof(int));
    context->rdisps  = (int *) calloc (rsize, sizeof(int));
    context->rcounts = (int *) calloc (rsize, sizeof(int));
    if (OPAL_UNLIKELY (NULL == context->tmpbuf || NULL == context->rdisps || NULL == context->rcounts)) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    request->context = context;

    /* Execute the inter-allreduce: the result from the local will be in the buffer of the remote group
     * and vise-versa. */
    rc = intercomm->c_coll.coll_iallreduce (inbuf, context->tmpbuf, count, MPI_INT, op, intercomm,
                                            &subreq, intercomm->c_coll.coll_iallreduce_module);
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    if (0 == local_rank) {
        ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_leader_exchange, &subreq, 1);
    } else {
        ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_allgather, &subreq, 1);
    }

    ompi_comm_request_start (request);
    *req = &request->super;

exit:
    if (OMPI_SUCCESS != rc) {
        if (context) {
            ompi_comm_allreduce_inter_context_free (context);
        }

        if (request) {
            request->context = NULL;
            ompi_comm_request_return (request);
        }
    }

    return rc;
}


static int ompi_comm_allreduce_inter_leader_exchange (ompi_comm_request_t *request)
{
    struct ompi_comm_allreduce_inter_context *context =
        (struct ompi_comm_allreduce_inter_context *) request->context;
    ompi_request_t *subreqs[2];
    int rc;

    /* local leader exchange their data and determine the overall result
       for both groups */
    rc = MCA_PML_CALL(irecv (context->outbuf, context->count, MPI_INT, 0, OMPI_COMM_ALLREDUCE_TAG,
                             context->intercomm, subreqs));
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    rc = MCA_PML_CALL(isend (context->tmpbuf, context->count, MPI_INT, 0, OMPI_COMM_ALLREDUCE_TAG,
                             MCA_PML_BASE_SEND_STANDARD, context->intercomm, subreqs + 1));
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_leader_reduce, subreqs, 2);

exit:
    if (OMPI_SUCCESS != rc) {
        ompi_comm_allreduce_inter_context_free (context);
        request->context = NULL;
    }

    return rc;
}

static int ompi_comm_allreduce_inter_leader_reduce (ompi_comm_request_t *request)
{
    struct ompi_comm_allreduce_inter_context *context =
        (struct ompi_comm_allreduce_inter_context *) request->context;

    ompi_op_reduce (context->op, context->tmpbuf, context->outbuf, context->count, MPI_INT);

    return ompi_comm_allreduce_inter_allgather (request);
}


static int ompi_comm_allreduce_inter_allgather (ompi_comm_request_t *request)
{
    struct ompi_comm_allreduce_inter_context *context =
        (struct ompi_comm_allreduce_inter_context *) request->context;
    ompi_request_t *subreq;
    int scount = 0, rc;

    /* distribute the overall result to all processes in the other group.
       Instead of using bcast, we are using here allgatherv, to avoid the
       possible deadlock. Else, we need an algorithm to determine,
       which group sends first in the inter-bcast and which receives
       the result first.
    */

    if (0 != ompi_comm_rank (context->intercomm)) {
        context->rcounts[0] = context->count;
    } else {
        scount = context->count;
    }

    rc = context->intercomm->c_coll.coll_iallgatherv (context->outbuf, scount, MPI_INT, context->outbuf,
                                                      context->rcounts, context->rdisps, MPI_INT,
                                                      context->intercomm, &subreq,
                                                      context->intercomm->c_coll.coll_iallgatherv_module);
    if (OMPI_SUCCESS != rc) {
        ompi_comm_allreduce_inter_context_free (context);
        request->context = NULL;
        return rc;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_allgather_complete, &subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_allreduce_inter_allgather_complete (ompi_comm_request_t *request)
{
    /* free this request's context */
    ompi_comm_allreduce_inter_context_free (request->context);
    /* prevent a double-free from the progress engine */
    request->context = NULL;

    /* done */
    return OMPI_SUCCESS;
}

/* Arguments not used in this implementation:
 * - send_first
 */
static int ompi_comm_allreduce_intra_bridge (int *inbuf, int *outbuf,
                                             int count, struct ompi_op_t *op,
                                             ompi_communicator_t *comm,
                                             ompi_communicator_t *bcomm,
                                             void* lleader, void* rleader,
                                             int send_first )
{
    int *tmpbuf=NULL;
    int local_rank;
    int i;
    int rc;
    int local_leader, remote_leader;

    local_leader  = (*((int*)lleader));
    remote_leader = (*((int*)rleader));

    if ( &ompi_mpi_op_sum.op != op && &ompi_mpi_op_prod.op != op &&
         &ompi_mpi_op_max.op != op && &ompi_mpi_op_min.op  != op ) {
        return MPI_ERR_OP;
    }

    local_rank = ompi_comm_rank ( comm );
    tmpbuf     = (int *) malloc ( count * sizeof(int));
    if ( NULL == tmpbuf ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    /* Intercomm_create */
    rc = comm->c_coll.coll_allreduce ( inbuf, tmpbuf, count, MPI_INT,
                                       op, comm, comm->c_coll.coll_allreduce_module );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if (local_rank == local_leader ) {
        MPI_Request req;

        rc = MCA_PML_CALL(irecv ( outbuf, count, MPI_INT, remote_leader,
                                  OMPI_COMM_ALLREDUCE_TAG,
                                  bcomm, &req));
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = MCA_PML_CALL(send (tmpbuf, count, MPI_INT, remote_leader,
                                OMPI_COMM_ALLREDUCE_TAG,
                                MCA_PML_BASE_SEND_STANDARD,  bcomm));
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = ompi_request_wait( &req, MPI_STATUS_IGNORE);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        if ( &ompi_mpi_op_max.op == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] > outbuf[i]) {
                    outbuf[i] = tmpbuf[i];
                }
            }
        }
        else if ( &ompi_mpi_op_min.op == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] < outbuf[i]) {
                    outbuf[i] = tmpbuf[i];
                }
            }
        }
        else if ( &ompi_mpi_op_sum.op == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] += tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_prod.op == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] *= tmpbuf[i];
            }
        }
    }

    rc = comm->c_coll.coll_bcast ( outbuf, count, MPI_INT, local_leader,
                                   comm, comm->c_coll.coll_bcast_module );

 exit:
    if (NULL != tmpbuf ) {
        free (tmpbuf);
    }

    return (rc);
}

typedef struct {
    opal_buffer_t buf;
    bool active;
} comm_cid_return_t;

static void comm_cid_recv(int status,
                          ompi_process_name_t* peer,
                          opal_buffer_t* buffer,
                          ompi_rml_tag_t tag,
                          void* cbdata)
{
    comm_cid_return_t *rcid = (comm_cid_return_t*)cbdata;

    opal_dss.copy_payload(&rcid->buf, buffer);
    rcid->active = false;
}

/* Arguments not used in this implementation:
 *    - bridgecomm
 *
 * lleader is the local rank of root in comm
 * rleader is the OOB contact information of the
 * root processes in the other world.
 */
static int ompi_comm_allreduce_intra_oob (int *inbuf, int *outbuf,
                                          int count, struct ompi_op_t *op,
                                          ompi_communicator_t *comm,
                                          ompi_communicator_t *bridgecomm,
                                          void* lleader, void* rleader,
                                          int send_first )
{
    int *tmpbuf=NULL;
    int rc;
    int local_leader, local_rank;
    ompi_process_name_t *remote_leader=NULL;
    int32_t size_count;
    comm_cid_return_t rcid;

    local_leader  = (*((int*)lleader));
    remote_leader = (ompi_process_name_t*)rleader;
    size_count = count;

    local_rank = ompi_comm_rank ( comm );
    tmpbuf     = (int *) malloc ( count * sizeof(int));
    if ( NULL == tmpbuf ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    /* comm is an intra-communicator */
    rc = comm->c_coll.coll_allreduce(inbuf,tmpbuf,count,MPI_INT,op, comm,
                                     comm->c_coll.coll_allreduce_module);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if (local_rank == local_leader ) {
        opal_buffer_t *sbuf;

        sbuf = OBJ_NEW(opal_buffer_t);

        if (OPAL_SUCCESS != (rc = opal_dss.pack(sbuf, tmpbuf, (int32_t)count, OPAL_INT))) {
            goto exit;
        }

        if ( send_first ) {
            if (0 > (rc = ompi_rte_send_buffer_nb(remote_leader, sbuf,
                                                  OMPI_RML_TAG_COMM_CID_INTRA,
                                                  ompi_rte_send_cbfunc, NULL))) {
                goto exit;
            }
            OBJ_CONSTRUCT(&rcid.buf, opal_buffer_t);
            rcid.active = true;
            ompi_rte_recv_buffer_nb(remote_leader, OMPI_RML_TAG_COMM_CID_INTRA,
                                    OMPI_RML_NON_PERSISTENT, comm_cid_recv, &rcid);
            while (rcid.active) {
                opal_progress();
            }
        }
        else {
            OBJ_CONSTRUCT(&rcid.buf, opal_buffer_t);
            rcid.active = true;
            ompi_rte_recv_buffer_nb(remote_leader, OMPI_RML_TAG_COMM_CID_INTRA,
                                    OMPI_RML_NON_PERSISTENT, comm_cid_recv, &rcid);
            while (rcid.active) {
                opal_progress();
            }
            if (0 > (rc = ompi_rte_send_buffer_nb(remote_leader, sbuf,
                                                  OMPI_RML_TAG_COMM_CID_INTRA,
                                                  ompi_rte_send_cbfunc, NULL))) {
                goto exit;
            }
        }

        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&rcid.buf, outbuf, &size_count, OPAL_INT))) {
            goto exit;
        }
        OBJ_DESTRUCT(&rcid.buf);
        count = (int)size_count;

	ompi_op_reduce (op, tmpbuf, outbuf, count, MPI_INT);
    }

    rc = comm->c_coll.coll_bcast (outbuf, count, MPI_INT,
                                  local_leader, comm,
                                  comm->c_coll.coll_bcast_module);

 exit:
    if (NULL != tmpbuf ) {
        free (tmpbuf);
    }

    return (rc);
}

static int ompi_comm_allreduce_group (int *inbuf, int* outbuf,
                                      int count, struct ompi_op_t *op,
                                      ompi_communicator_t *comm,
                                      ompi_communicator_t *newcomm,
                                      void* local_leader,
                                      void* remote_leader,
                                      int send_first)
{
    ompi_group_t *group = newcomm->c_local_group;
    int peers_group[3], peers_comm[3];
    const int group_size = ompi_group_size (group);
    const int group_rank = ompi_group_rank (group);
    int tag = *((int *) local_leader);
    int *tmp1;
    int i, rc=OMPI_SUCCESS;

    /* basic recursive doubling allreduce on the group */
    peers_group[0] = group_rank ? ((group_rank - 1) >> 1) : MPI_PROC_NULL;
    peers_group[1] = (group_rank * 2 + 1) < group_size ? group_rank * 2 + 1: MPI_PROC_NULL;
    peers_group[2] = (group_rank * 2 + 2) < group_size ? group_rank * 2 + 2 : MPI_PROC_NULL;

    /* translate the ranks into the ranks of the parent communicator */
    ompi_group_translate_ranks (group, 3, peers_group, comm->c_local_group, peers_comm);

    tmp1 = malloc (sizeof (int) * count);

    /* reduce */
    memmove (outbuf, inbuf, sizeof (int) * count);

    for (i = 1 ; i < 3 ; ++i) {
        if (MPI_PROC_NULL != peers_comm[i]) {
            rc = MCA_PML_CALL(recv(tmp1, count, MPI_INT, peers_comm[i], tag, comm,
                                   MPI_STATUS_IGNORE));
            if (OMPI_SUCCESS != rc) {
                goto out;
            }
            /* this is integer reduction so we do not care about ordering */
            ompi_op_reduce (op, tmp1, outbuf, count, MPI_INT);
        }
    }

    if (MPI_PROC_NULL != peers_comm[0]) {
        rc = MCA_PML_CALL(send(outbuf, count, MPI_INT, peers_comm[0],
                               tag, MCA_PML_BASE_SEND_STANDARD, comm));
        if (OMPI_SUCCESS != rc) {
            goto out;
        }

        rc = MCA_PML_CALL(recv(outbuf, count, MPI_INT, peers_comm[0],
                               tag, comm, MPI_STATUS_IGNORE));
        if (OMPI_SUCCESS != rc) {
            goto out;
        }
    }

    /* broadcast */
    for (i = 1 ; i < 3 ; ++i) {
        if (MPI_PROC_NULL != peers_comm[i]) {
            rc = MCA_PML_CALL(send(outbuf, count, MPI_INT, peers_comm[i], tag,
                                   MCA_PML_BASE_SEND_STANDARD, comm));
            if (OMPI_SUCCESS != rc) {
                goto out;
            }
        }
    }

 out:
    free (tmp1);

    return rc;
}

END_C_DECLS
