/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mca/coll/ml/coll_ml.h"

/*
 * This routine is used to progress a series of communication
 * primitives.
 *
 *  Assumptions:
 *      - A message is described by a message descriptor
 *      - Each message has a setup function associated with it, which is
 *        algorithm specific.  When a fragment is being prepared, this
 *        progress is used to setup the arguments that will be passed into
 *        each routine called to complete a given function.  The idea here
 *        is that when the progress routines is called, the full communication
 *        pattern has already been described in the setup function, with
 *        progress function being generic.
 *      - Each fragment is described by a fragment descriptor
 *      - Each message descriptor has a fragment descriptor permanently
 *        associated with it.
 *      - The message will be proressed as long as the individul
 *        functions complete.  When an indivicual funciton does not
 *        complete, the current state will be saved, for future
 *        restart.
 *      - return status
 *          OMPI_COMPLETE: funciton completed
 *          OMPI_INCOMPLETE: need to continue progressing the funciton
 *          any other return value - error condition
 */

int coll_ml_progress_individual_message(mca_coll_ml_fragment_t *frag_descriptor)
{
   /* local variables */
   int fn_index, ret = OMPI_SUCCESS;
   uint32_t n_frags_complete;
   int starting_fn_index=frag_descriptor->current_fn_index;
   coll_ml_collective_description_t *local_comm_description=
       frag_descriptor->full_msg_descriptor->local_comm_description;

   /* loop over functions */
   for( fn_index=starting_fn_index ; fn_index < local_comm_description->n_functions;
        fn_index ++ ) {
       mca_bcol_base_module_t *bcol_module=
           local_comm_description->functions[fn_index].bcol_module;
       ret =(bcol_module->bcol_function_table[local_comm_description->functions[fn_index].fn_idx])
               (&(frag_descriptor->fn_args[fn_index]), &local_comm_description->functions[fn_index]);
       if( ML_OMPI_COMPLETE != ret ) {
           /* since function incomplete, need to decide what to do */
           if( ML_OMPI_INCOMPLETE == ret ) {
               /* need to return to this later */
               /* mark where to continue */
               frag_descriptor->current_fn_index=fn_index;
               /* RLG - is this really best ?  Only advantage is that
                * if we exit the loop, we can assume message is
                * complete
                */
               return OMPI_SUCCESS;
           } else {
               /* some sort of error condition */
               frag_descriptor->current_fn_index=fn_index;
               return ret;
           }
       }

   }

   /* looks like we are done */
   /* increment counter for number of completed fragments */
   n_frags_complete = OPAL_THREAD_ADD_SIZE_T(
       &(frag_descriptor->full_msg_descriptor->frags_complete), 1);

   /* 
    * release resrouces 
    */

   /* fragment resources */

   /* full message resources */
   if ( n_frags_complete == frag_descriptor->full_msg_descriptor->n_fragments)
   {
       /* free any fragments that still need to be freed 
        * NOTE: at this level we do not handle any resrouces
        * aside from the pre-registered buffers, all these
        * are handled in the bcol level */

       /* return the buffers to the ml free list */

       /* mark as complete - so MPI can complete 
        * the message descriptor will be freed by a call
        * to mpi_test/mpi_wait/... as the message descriptor
        * also holds the mpi request object */

   }

   return OMPI_SUCCESS;
}
