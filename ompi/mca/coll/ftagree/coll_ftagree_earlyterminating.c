/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2013-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_ftagree.h"
#include "coll_ftagree_era.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

#include MCA_timer_IMPLEMENTATION_HEADER
#include "coll_ftagree.h"

/**
 * This Agreement implements the protocol proposed in
 *   Early Consensus in Message-passing Systems Enriched with a Perfect Failure Detector
 *     and its Application in the Theta Model.
 *   by Francois Bonnet, Michel Raynal
 *   in 2010 European Dependable Computing Conference
 */

/** Those are the possible status of the process following the algorithm */
#define STATUS_NO_INFO               0
#define STATUS_CRASHED           (1<<0)
#define STATUS_ACRASHED         ((1<<1) | STATUS_CRASHED)
#define STATUS_TOLD_ME_HE_KNOWS  (1<<2)
#define STATUS_KNOWS_I_KNOW      (1<<3)
/** Those are used solely to track requests completion */
#define STATUS_SEND_COMPLETE     (1<<4)
#define STATUS_RECV_COMPLETE     (1<<5)

typedef struct {
    int knows;
    int pf;
    char est_value[];
} ftagree_eta_agreement_msg_t;

#define FTAGREE_ETA_TAG_AGREEMENT MCA_COLL_BASE_TAG_AGREEMENT

/*
 *	eta_intra
 *
 *	Function:	- MPI_Comm_agree()
 *	Accepts:	- same as MPI_Comm_agree()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */

int
mca_coll_ftagree_eta_intra(void *contrib,
                           int dt_count,
                           ompi_datatype_t *dt,
                           ompi_op_t *op,
                           ompi_group_t **group, bool update_grp,
                           ompi_communicator_t* comm,
                           mca_coll_base_module_t *module)
{
    ftagree_eta_agreement_msg_t *out, *in;
    size_t dt_size, msg_size;
    int *proc_status; /**< char would be enough, but we use the same area to build the group of dead processes at the end */
    ompi_request_t **reqs;
    MPI_Status *statuses;
    int me, i, ri, nr, np, nbrecv, rc, ret = MPI_SUCCESS, nbknow = 0, nbcrashed = 0, round;

    np = ompi_comm_size(comm);
    me = ompi_comm_rank(comm);
    proc_status = (int *)calloc( np, sizeof(int) );

    ompi_datatype_type_size(dt, &dt_size);
    msg_size = sizeof(ftagree_eta_agreement_msg_t) + dt_count * dt_size;

    /* This should go in the module query, and a module member should be used here */
    reqs = (ompi_request_t **)calloc( 2 * np, sizeof(ompi_request_t *) ); /** < Need to calloc or set to MPI_REQUEST_NULL to ensure cleanup in all cases. */
    statuses = (MPI_Status*)malloc( 2 * np * sizeof(MPI_Status) );
    in = (ftagree_eta_agreement_msg_t*)calloc( np, msg_size );
    out = (ftagree_eta_agreement_msg_t*)malloc( msg_size );

    memcpy(out->est_value, contrib, dt_count * dt_size);
    out->knows = 0;
    out->pf = 0;
    round = 1;

    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ftagree:agreement (ETA) Starting Agreement with message size of %lu bytes (%lu bytes for the agreement value)\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), msg_size, dt_count * dt_size));

    { /* ignore acked failures (add them later to the result) */
        ompi_group_t* ackedgrp = NULL; int npa; int *aranks, *cranks;
        ackedgrp = *group;
        if( 0 != (npa = (NULL == ackedgrp? 0: ompi_group_size(ackedgrp))) ) {
            aranks = calloc( npa, sizeof(int) );
            for( i = 0; i < npa; i++ ) aranks[i] = i;
            cranks = calloc( npa, sizeof(int) );
            ompi_group_translate_ranks( ackedgrp, npa, aranks, comm->c_remote_group, cranks );
            for( i = 0; i < npa; i++ ) {
                OPAL_OUTPUT_VERBOSE((1, ompi_ftmpi_output_handle,
                                     "%s has acknowledged rank %d, ignoring\n",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), cranks[i] ));
                proc_status[cranks[i]] = STATUS_ACRASHED;
            }
            free(aranks);
            free(cranks);
        }
    }

#define NEED_TO_RECV(_i) (me != _i && (!(proc_status[_i] & STATUS_CRASHED)) && (!(proc_status[_i] & STATUS_TOLD_ME_HE_KNOWS)))
#define NEED_TO_SEND(_i) (me != _i && (!(proc_status[_i] & STATUS_CRASHED)) && (!(proc_status[_i] & STATUS_KNOWS_I_KNOW)))

    while(round <= (np + 1)) {
        OPAL_OUTPUT_VERBOSE((50, ompi_ftmpi_output_handle,
                             "%s ftagree:agreement (ETA) Starting Round %d\n",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), round));

        /**
         * Post all the requests, first the receives and then the sends.
         */
        nr = 0;
        for(i = 0; i < np; i++) {

#if defined(FTAGREE_DEBUG_FAILURE_INJECT)
            if( (double)rand() / (double)RAND_MAX < mca_coll_ftagree_rank_fault_proba ) {
                OPAL_OUTPUT_VERBOSE((0, ompi_ftmpi_output_handle,
                                     "%s ftagree:agreement (ETA) INJECT: Killing myself just before posting message reception to/from %d\n",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                                     i));
                raise(SIGKILL);
            }
#endif

            if( NEED_TO_RECV(i) ) {
                /* Need to know more about this guy */
                MCA_PML_CALL(irecv(((char*)in) + (i*msg_size), msg_size, MPI_BYTE,
                                   i, FTAGREE_ETA_TAG_AGREEMENT, comm,
                                   &reqs[nr++]));
                proc_status[i] &= ~STATUS_RECV_COMPLETE;
                OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                     "%s ftagree:agreement (ETA) Request for recv of rank %d is at %d(%p)\n",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), i, nr-1, (void*)reqs[nr-1]));
            } else {
                proc_status[i] |= STATUS_RECV_COMPLETE;
            }
            if( NEED_TO_SEND(i) ) {
                /* Need to communicate with this guy */
                MCA_PML_CALL(isend(out, msg_size, MPI_BYTE,
                                   i, FTAGREE_ETA_TAG_AGREEMENT,
                                   MCA_PML_BASE_SEND_STANDARD, comm,
                                   &reqs[nr++]));
                proc_status[i] &= ~STATUS_SEND_COMPLETE;
                OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                     "%s ftagree:agreement (ETA) Request for send of rank %d is at %d(%p)\n",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), i, nr-1, (void*)reqs[nr-1]));
            } else {
                proc_status[i] |= STATUS_SEND_COMPLETE;
            }
        }
        for(i = nr; i < 2*np; i++) {
            reqs[i] = MPI_REQUEST_NULL;
        }

        do {
            OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                 "%s ftagree:agreement (ETA) Entering waitall(%d)\n",
                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), nr));

#if defined(FTAGREE_DEBUG_FAILURE_INJECT)
            if( (double)rand() / (double)RAND_MAX < mca_coll_ftagree_rank_fault_proba ) {
                OPAL_OUTPUT_VERBOSE((0, ompi_ftmpi_output_handle,
                                     "%s ftagree:agreement (ETA) INJECT: Killing myself just before waitall\n",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME)));
                raise(SIGKILL);
            }
#endif

            rc = ompi_request_wait_all(nr, reqs, statuses);

#if defined(FTAGREE_DEBUG_FAILURE_INJECT)
            if( (double)rand() / (double)RAND_MAX < mca_coll_ftagree_rank_fault_proba ) {
                OPAL_OUTPUT_VERBOSE((0, ompi_ftmpi_output_handle,
                                     "%s ftagree:agreement (ETA) INJECT: Killing myself just after waitall\n",
                                     OMPI_NAME_PRINT(OMPI_PROC_MY_NAME)));
                raise(SIGKILL);
            }
#endif

            /**< If we need to re-wait on some requests, we're going to pack them at index nr */
            nr = 0;

            nbrecv = 0;

            if( rc != MPI_ERR_IN_STATUS && rc != MPI_SUCCESS ) {
                ret = rc;
                goto clean_and_exit;
            }

            /* Long loop if somebody failed */
            ri = 0;
            for(i = 0; i < np; i++) {
                if( !(proc_status[i] & STATUS_RECV_COMPLETE) ) {
                    if( (rc == MPI_SUCCESS) || (MPI_SUCCESS == statuses[ri].MPI_ERROR) ) {
                        assert(MPI_REQUEST_NULL == reqs[ri]);

                        /* Implements the binary and of answers */
                        ompi_op_reduce(op, in[i].est_value, out->est_value, dt_count, dt);

                        /* Implements the logical or of ERR_PROC_FAILED returns */
                        out->pf |= in[i].pf;
                        proc_status[i] |= ( (in[i].knows * STATUS_TOLD_ME_HE_KNOWS) | STATUS_RECV_COMPLETE);
                        nbrecv++;

                        OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                             "%s ftagree:agreement (ETA) Request %d(%p) for recv of rank %d is completed.\n",
                                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), ri, (void*)reqs[ri], i));
                    } else {
                        if( (MPI_ERR_PROC_FAILED == statuses[ri].MPI_ERROR) ) {
                            /* Failure detected */
                            proc_status[i] |= (STATUS_CRASHED | STATUS_RECV_COMPLETE);
                            OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                                                 "%s ftagree:agreement (ETA) recv with rank %d failed on request at index %d(%p). Mark it as dead!",
                                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), i, ri, (void*)reqs[ri]));
                            out->pf = 1;
/* per spec this should already be completed; TODO remove of proven correct */
                            /* Release the request, it can't be subsequently completed */
                            if(MPI_REQUEST_NULL != reqs[ri])
                                ompi_request_free(&reqs[ri]);
                        } else if( (MPI_ERR_PENDING == statuses[ri].MPI_ERROR) ) {
                            /* The pending request(s) will be waited on at the next iteration. */
                            assert( ri >= nr );
                            assert( MPI_REQUEST_NULL != reqs[ri] );
                            assert( ri == nr || reqs[nr] == MPI_REQUEST_NULL );
                            OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                                 "%s ftagree:agreement (ETA) Request %d(%p) for recv of rank %d remains pending. Renaming it as Request %d\n",
                                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), ri, (void*)reqs[ri], i, nr));
                            reqs[nr] = reqs[ri];
                            if( ri != nr )
                                reqs[ri] = MPI_REQUEST_NULL;
                            nr++;
                        } else {
                            ret = statuses[ri].MPI_ERROR;
                            goto clean_and_exit;
                        }
                    }
                    ri++;
                }

                if( !(proc_status[i] & STATUS_SEND_COMPLETE) ) {
                    if( (rc == MPI_SUCCESS) || (MPI_SUCCESS == statuses[ri].MPI_ERROR) ) {
                        assert(MPI_REQUEST_NULL == reqs[ri]);
                        proc_status[i] |= ((out->knows * STATUS_KNOWS_I_KNOW) | STATUS_SEND_COMPLETE);

                        OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                             "%s ftagree:agreement (ETA) Request %d(%p) for send of rank %d is completed.\n",
                                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), ri, (void*)reqs[ri], i));
                    } else {
                        if( (MPI_ERR_PROC_FAILED == statuses[ri].MPI_ERROR) ) {
                            /* Failure detected */
                            proc_status[i] |= (STATUS_CRASHED | STATUS_SEND_COMPLETE);

                            OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                                                 "%s ftagree:agreement (ETA) send with rank %d failed on Request %d(%p). Mark it as dead!",
                                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), i, ri, (void*)reqs[ri]));
                            out->pf = 1;
/* per spec this should already be completed; TODO understand why not */
                            /* Release the request, it can't be subsequently completed */
                            if(MPI_REQUEST_NULL != reqs[ri])
                                ompi_request_free(&reqs[ri]);
                        } else if( (MPI_ERR_PENDING == statuses[ri].MPI_ERROR) ) {
                            /* The pending request(s) will be waited on at the next iteration. */
                            assert( ri >= nr );
                            assert( MPI_REQUEST_NULL != reqs[ri] );
                            assert( ri == nr || reqs[nr] == MPI_REQUEST_NULL );
                            OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                                                 "%s ftagree:agreement (ETA) Request %d(%p) for send of rank %d remains pending. Renaming it as Request %d\n",
                                                 OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), ri, (void*)reqs[ri], i, nr));
                            reqs[nr] = reqs[ri];
                            if( ri != nr )
                                reqs[ri] = MPI_REQUEST_NULL;
                            nr++;
                        } else {
                            ret = statuses[ri].MPI_ERROR;
                            goto clean_and_exit;
                        }
                    }
                    ri++;
                }

            }

        } while( 0 != nr );

#undef NEED_TO_SEND
#undef NEED_TO_RECV

        nbcrashed = 0;
        for(i = 0; i < np; i++)
            if( proc_status[i] & STATUS_CRASHED )
                nbcrashed++;
        nbknow = 0;
        for(i = 0; i < np; i++)
            if( (!(proc_status[i] & STATUS_CRASHED)) && (proc_status[i] & STATUS_TOLD_ME_HE_KNOWS) )
                nbknow++;

        OPAL_OUTPUT_VERBOSE((50, ompi_ftmpi_output_handle,
                             "%s ftagree:agreement (ETA) end of Round %d: nbcrashed = %d, nbknow = %d, nbrecv = %d. out.knows = %d\n",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                             round, nbcrashed, nbknow, nbrecv, out->knows));

        if( (nbknow + nbcrashed >= np - 1) && (out->knows == 1) ) {
            break;
        }

        out->knows = (nbknow > 0) || (nbrecv >= np - round + 1);
        round++;
    }

 clean_and_exit:
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                "%s ftbasis:agreement (ETA) decided in %d rounds ", OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), round));
    for (ri = 0; ri < 2*np; ++ri)
        if( NULL != reqs[ri] && MPI_REQUEST_NULL != reqs[ri])
            ompi_request_free( &reqs[ri] );
    free(reqs);
    free(statuses);
    free(in);
    /* Let's build the group of failed processes */
    if( update_grp ) {
        int pos;
        /* We overwrite proc_status because it is not used anymore */
        int *failed = proc_status;

        for( pos = i = 0; i < np; i++ ) {
            if( STATUS_CRASHED & proc_status[i] ) {
                failed[pos++] = i;
            }
        }
        if( NULL != *group ) {
            OBJ_RELEASE(*group);
        }
        ompi_group_incl(comm->c_remote_group, pos, failed, group);
    }
    free(proc_status);

    if( (MPI_SUCCESS == ret) && out->pf ) {
        ret = MPI_ERR_PROC_FAILED;
    }

    memcpy(contrib, out->est_value, dt_count * dt_size);
    free(out);

    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ftagree:agreement (ETA) return %d with 4 first bytes of result 0x%08x and dead group with %d processes",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), ret, *(int*)contrib,
                         (NULL == *group) ? 0 : (*group)->grp_proc_count));
    return ret;
}

