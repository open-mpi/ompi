/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_COMM_H
#define MCA_PML_COMM_H

#include "threads/mutex.h"
#include "threads/condition.h"
#include "mca/ptl/ptl.h"
#include "class/ompi_list.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern ompi_class_t mca_pml_ptl_comm_t_class;

/**
 *  Cached on ompi_communicator_t to hold queues/state
 *  used by the PML<->PTL interface for matching logic. 
 */
struct mca_pml_comm_t {
    ompi_object_t super;
    mca_ptl_sequence_t *c_msg_seq;      /**< send message sequence number - sender side */
    mca_ptl_sequence_t *c_next_msg_seq; /**< send message sequence number - receiver side */
    mca_ptl_sequence_t c_recv_seq;      /**< recv request sequence number - receiver side */
    ompi_mutex_t c_matching_lock;             /**< matching lock */
    ompi_list_t *c_unexpected_frags;          /**< unexpected fragment queues */
    ompi_list_t *c_frags_cant_match;          /**< out-of-order fragment queues */
    ompi_list_t *c_specific_receives;         /**< queues of unmatched specific (source process specified) receives */
    ompi_list_t c_wild_receives;              /**< queue of unmatched wild (source process not specified) receives */
};
typedef struct mca_pml_comm_t mca_pml_ptl_comm_t;


/**
 * Initialize an instance of mca_pml_ptl_comm_t based on the communicator size.
 *
 * @param  comm   Instance of mca_pml_ptl_comm_t
 * @param  size   Size of communicator 
 * @return        OMPI_SUCCESS or error status on failure.
 */

extern int mca_pml_ptl_comm_init_size(mca_pml_ptl_comm_t* comm, size_t size);

/**
 * Obtain the next sequence number (MPI) for a given destination rank.
 *
 * @param  comm   Instance of mca_pml_ptl_comm_t
 * @param  dst    Rank of destination.
 * @return        Next available sequence number.
 */

static inline mca_ptl_sequence_t mca_pml_ptl_comm_send_sequence(mca_pml_ptl_comm_t* comm, int dst)
{
   mca_ptl_sequence_t sequence;
   OMPI_THREAD_SCOPED_LOCK(&comm->c_matching_lock,
                           sequence = comm->c_msg_seq[dst]++);
   return sequence;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

