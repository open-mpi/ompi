/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_tuned.h"
#include "ompi/mca/pml/pml.h"
#include "opal/util/bit_ops.h"

#include "coll_tuned.h"


/*
 * Notes on evaluation rules and ordering 
 * 
 * The order is: 
 *      use file based rules if presented (-coll_tuned_dynamic_rules_filename = rules)
 * Else
 *      use forced rules (-coll_tuned_dynamic_ALG_intra_algorithm = algorithm-number)
 * Else
 *      use fixed (compiled) rule set (or nested ifs)
 *
 */

/*
 *  allreduce_intra
 *
 *  Function:   - allreduce using other MPI collectives
 *  Accepts:    - same as MPI_Allreduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_allreduce_intra_dec_dynamic (void *sbuf, void *rbuf, int count,
                                             struct ompi_datatype_t *dtype,
                                             struct ompi_op_t *op,
                                             struct ompi_communicator_t *comm)
{

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_allreduce_intra_dec_dynamic"));

    /* check to see if we have some filebased rules */
    if (comm->c_coll_selected_data->com_rules[ALLREDUCE]) {

        /* we do, so calc the message size or what ever we need and use this for the evaluation */
        int alg, faninout, segsize;
        size_t dsize;

        ompi_ddt_type_size (dtype, &dsize);
        dsize *= count;

        alg = ompi_coll_tuned_get_target_method_params (comm->c_coll_selected_data->com_rules[ALLREDUCE], 
                                                        dsize, &faninout, &segsize);

        if (alg) { /* we have found a valid choice from the file based rules for this message size */
            return ompi_coll_tuned_allreduce_intra_do_this (sbuf, rbuf, count, dtype, op, comm, 
                                                            alg, faninout, segsize);
        } /* found a method */
    } /*end if any com rules to check */

    if (comm->c_coll_selected_data->user_forced[ALLREDUCE].algorithm) {
        return ompi_coll_tuned_allreduce_intra_do_forced (sbuf, rbuf, count, dtype, op, comm);
    }
    return ompi_coll_tuned_allreduce_intra_dec_fixed (sbuf, rbuf, count, dtype, op, comm);
}

/*
 *    alltoall_intra_dec 
 *
 *    Function:    - seletects alltoall algorithm to use
 *    Accepts:    - same arguments as MPI_Alltoall()
 *    Returns:    - MPI_SUCCESS or error code (passed from the bcast implementation)
 */

int ompi_coll_tuned_alltoall_intra_dec_dynamic(void *sbuf, int scount, 
                                               struct ompi_datatype_t *sdtype,
                                               void* rbuf, int rcount, 
                                               struct ompi_datatype_t *rdtype, 
                                               struct ompi_communicator_t *comm)
{

    OPAL_OUTPUT((ompi_coll_tuned_stream, "ompi_coll_tuned_alltoall_intra_dec_dynamic"));

    /* check to see if we have some filebased rules */
    if (comm->c_coll_selected_data->com_rules[ALLTOALL]) {

        /* we do, so calc the message size or what ever we need and use this for the evaluation */
        int comsize;
        int alg, faninout, segsize;
        size_t dsize;

        ompi_ddt_type_size (sdtype, &dsize);
        comsize = ompi_comm_size(comm);
        dsize *= comsize * scount;

        alg = ompi_coll_tuned_get_target_method_params (comm->c_coll_selected_data->com_rules[ALLTOALL], 
                                                        dsize, &faninout, &segsize);

        if (alg) { /* we have found a valid choice from the file based rules for this message size */
            return ompi_coll_tuned_alltoall_intra_do_this (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm,
                                                           alg, faninout, segsize);
        } /* found a method */
    } /*end if any com rules to check */


    if (comm->c_coll_selected_data->user_forced[ALLTOALL].algorithm) {
        return ompi_coll_tuned_alltoall_intra_do_forced (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
    }
    return ompi_coll_tuned_alltoall_intra_dec_fixed (sbuf, scount, sdtype, rbuf, rcount, rdtype, comm);
}

/*
 *    barrier_intra_dec 
 *
 *    Function:    - seletects barrier algorithm to use
 *    Accepts:    - same arguments as MPI_Barrier()
 *    Returns:    - MPI_SUCCESS or error code (passed from the barrier implementation)
 */
int ompi_coll_tuned_barrier_intra_dec_dynamic(struct ompi_communicator_t *comm)
{

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_barrier_intra_dec_dynamic"));

    /* check to see if we have some filebased rules */
    if (comm->c_coll_selected_data->com_rules[BARRIER]) {

        /* we do, so calc the message size or what ever we need and use this for the evaluation */
        int alg, faninout, segsize;

        alg = ompi_coll_tuned_get_target_method_params (comm->c_coll_selected_data->com_rules[BARRIER], 
                                                        0, &faninout, &segsize);

        if (alg) { /* we have found a valid choice from the file based rules for this message size */
            return ompi_coll_tuned_barrier_intra_do_this (comm,
                                                          alg, faninout, segsize);
        } /* found a method */
    } /*end if any com rules to check */

    if (comm->c_coll_selected_data->user_forced[BARRIER].algorithm) {
        return ompi_coll_tuned_barrier_intra_do_forced (comm);
    }
    return ompi_coll_tuned_barrier_intra_dec_fixed (comm);
}

/*
 *   bcast_intra_dec 
 *
 *   Function:   - seletects broadcast algorithm to use
 *   Accepts:   - same arguments as MPI_Bcast()
 *   Returns:   - MPI_SUCCESS or error code (passed from the bcast implementation)
 */
int ompi_coll_tuned_bcast_intra_dec_dynamic(void *buff, int count,
                                            struct ompi_datatype_t *datatype, int root,
                                            struct ompi_communicator_t *comm)
{

    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:bcast_intra_dec_dynamic"));

    /* check to see if we have some filebased rules */
    if (comm->c_coll_selected_data->com_rules[BCAST]) {

        /* we do, so calc the message size or what ever we need and use this for the evaluation */
        int alg, faninout, segsize;
        size_t dsize;

        ompi_ddt_type_size (datatype, &dsize);
        dsize *= count;

        alg = ompi_coll_tuned_get_target_method_params (comm->c_coll_selected_data->com_rules[BCAST], 
                                                        dsize, &faninout, &segsize);

        if (alg) { /* we have found a valid choice from the file based rules for this message size */
            return ompi_coll_tuned_bcast_intra_do_this (buff, count, datatype, root, comm,
                                                        alg, faninout, segsize);
        } /* found a method */
    } /*end if any com rules to check */


    if (comm->c_coll_selected_data->user_forced[BCAST].algorithm) {
        return ompi_coll_tuned_bcast_intra_do_forced (buff, count, datatype, root, comm);
    }
    return ompi_coll_tuned_bcast_intra_dec_fixed (buff, count, datatype, root, comm);
}

/*
 *    reduce_intra_dec 
 *
 *    Function:    - seletects reduce algorithm to use
 *    Accepts:    - same arguments as MPI_reduce()
 *    Returns:    - MPI_SUCCESS or error code (passed from the reduce implementation)
 *                                        
 */
int ompi_coll_tuned_reduce_intra_dec_dynamic( void *sendbuf, void *recvbuf,
                                              int count, struct ompi_datatype_t* datatype,
                                              struct ompi_op_t* op, int root,
                                              struct ompi_communicator_t* comm)
{

    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:reduce_intra_dec_dynamic"));

    /* check to see if we have some filebased rules */
    if (comm->c_coll_selected_data->com_rules[REDUCE]) {

        /* we do, so calc the message size or what ever we need and use this for the evaluation */
        int alg, faninout, segsize;
        size_t dsize;

        ompi_ddt_type_size (datatype, &dsize);
        dsize *= count;

        alg = ompi_coll_tuned_get_target_method_params (comm->c_coll_selected_data->com_rules[REDUCE], 
                                                        dsize, &faninout, &segsize);

        if (alg) { /* we have found a valid choice from the file based rules for this message size */
            return  ompi_coll_tuned_reduce_intra_do_this (sendbuf, recvbuf, count, datatype, op, root, comm, 
                                                          alg, faninout, segsize);
        } /* found a method */
    } /*end if any com rules to check */

    if (comm->c_coll_selected_data->user_forced[REDUCE].algorithm) {
        return ompi_coll_tuned_reduce_intra_do_forced (sendbuf, recvbuf, count, datatype, op, root, comm);
    }
    return ompi_coll_tuned_reduce_intra_dec_fixed (sendbuf, recvbuf, count, datatype, op, root, comm);
}

/*
 *    allgather_intra_dec 
 *
 *    Function:    - seletects allgather algorithm to use
 *    Accepts:    - same arguments as MPI_Allgather()
 *    Returns:    - MPI_SUCCESS or error code (passed from the selected
 *                        allgather function).
 */

int ompi_coll_tuned_allgather_intra_dec_dynamic(void *sbuf, int scount, 
                                                struct ompi_datatype_t *sdtype,
                                                void* rbuf, int rcount, 
                                                struct ompi_datatype_t *rdtype, 
                                                struct ompi_communicator_t *comm)
{
   
   OPAL_OUTPUT((ompi_coll_tuned_stream, 
                "ompi_coll_tuned_allgather_intra_dec_dynamic"));
   
   if (comm->c_coll_selected_data->com_rules[ALLGATHER]) {
      /* We have file based rules:
         - calculate message size and other necessary information */
      int comsize;
      int alg, faninout, segsize;
      size_t dsize;
      
      ompi_ddt_type_size (sdtype, &dsize);
      comsize = ompi_comm_size(comm);
      dsize *= comsize * scount;
      
      alg = ompi_coll_tuned_get_target_method_params (comm->c_coll_selected_data->com_rules[ALLGATHER], dsize, &faninout, &segsize);
      if (alg) { 
         /* we have found a valid choice from the file based rules for 
            this message size */
         return ompi_coll_tuned_allgather_intra_do_this (sbuf, scount, sdtype,
                                                         rbuf, rcount, rdtype,
                                                         comm, alg, faninout, 
                                                         segsize);
      }
   } 

   /* We do not have file based rules */
   if (comm->c_coll_selected_data->user_forced[ALLGATHER].algorithm) {
      /* User-forced algorithm */
      return ompi_coll_tuned_allgather_intra_do_forced (sbuf, scount, sdtype, 
                                                        rbuf, rcount, rdtype, 
                                                        comm);
   }

   /* Use default decision */
   return ompi_coll_tuned_allgather_intra_dec_fixed (sbuf, scount, sdtype, 
                                                     rbuf, rcount, rdtype, 
                                                     comm);
}

