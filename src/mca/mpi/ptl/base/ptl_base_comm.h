#ifndef MCA_PML_COMM_H
#define MCA_PML_COMM_H

#include "lam/threads/mutex.h"
#include "mca/mpi/ptl/ptl.h"
#include "lam/lfc/list.h"

/*
 *  Structure associated w/ lam_communicator_t that contains data 
 *  specific to the PML. 
 */

extern lam_class_info_t mca_ptl_comm_cls;

struct mca_pml_comm_t {
    
    /* send message sequence-number support - sender side */
    mca_ptl_base_sequence_t *c_msg_seq_num;

    /* send message sequence-number support - receiver side */
    mca_ptl_base_sequence_t *c_next_msg_seq_num;

    /* matching lock */
    lam_mutex_t *c_matching_lock;

    /* unexpected fragments queues */
    lam_list_t *unexpected_frags;
    /* these locks are needed to avoid a probe interfering with a match
     */
    lam_mutex_t *unexpected_frags_lock;

    /* out-of-order fragments queues */
    lam_list_t *frags_cant_match;

    /* queues of unmatched specific (source process specified) receives
     * sorted by source process */
    lam_list_t *specific_receives;

    /* queue of unmatched wild (source process not specified) receives
     * */
    lam_list_t wild_receives;
};
typedef struct mca_pml_comm_t mca_pml_comm_t;


extern void mca_pml_ptl_comm_init(struct mca_pml_comm_t*, size_t);
extern void mca_pml_ptl_comm_destroy(struct mca_pml_comm_t*);

#endif

