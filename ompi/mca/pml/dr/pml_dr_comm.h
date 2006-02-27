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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_DR_COMM_H
#define MCA_PML_DR_COMM_H

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/class/opal_list.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_dr_comm_proc_t {
    opal_object_t super;
    uint16_t expected_sequence;    /**< send message sequence number - receiver side */
    int32_t vfrag_id;              /**< virtual fragment identifier */
#if OMPI_HAVE_THREAD_SUPPORT
    volatile int32_t send_sequence; /**< send side sequence number */
#else
    int32_t send_sequence; /**< send side sequence number */
#endif
    opal_list_t frags_cant_match;  /**< out-of-order fragment queues */
    opal_list_t specific_receives; /**< queues of unmatched specific receives */
    opal_list_t unexpected_frags;  /**< unexpected fragment queues */
    ompi_proc_t* ompi_proc;        /**< back pointer to ompi_proc_t */
    opal_list_t acked_vfrags;      /**< list of vfrags id's that have been acked */
    opal_list_item_t* acked_vfrags_ptr; /**< a pointer to the last place we were in the list */
};
typedef struct mca_pml_dr_comm_proc_t mca_pml_dr_comm_proc_t;

struct mca_pml_dr_acked_item_t{ 
    opal_list_item_t super; 
    int32_t vfrag_id_high;
    int32_t vfrag_id_low;
}; 
typedef struct mca_pml_dr_acked_item_t mca_pml_dr_acked_item_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_dr_acked_item_t);

/**
 *  Cached on ompi_communicator_t to hold queues/state
 *  used by the PML<->PTL interface for matching logic. 
 */
struct mca_pml_comm_t {
    opal_object_t super;
#if OMPI_HAVE_THREAD_SUPPORT
    volatile uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
#else
    uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
#endif
    opal_mutex_t matching_lock;   /**< matching lock */
    opal_list_t wild_receives;    /**< queue of unmatched wild (source process not specified) receives */
    mca_pml_dr_comm_proc_t* procs;
    size_t num_procs;
};
typedef struct mca_pml_comm_t mca_pml_dr_comm_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_dr_comm_t);


/**
 * Initialize an instance of mca_pml_dr_comm_t based on the communicator size.
 *
 * @param  dr_comm   Instance of mca_pml_dr_comm_t
 * @param  pml_comm  Communicator 
 * @return           OMPI_SUCCESS or error status on failure.
 */

OMPI_DECLSPEC extern int mca_pml_dr_comm_init(mca_pml_dr_comm_t* dr_comm, ompi_communicator_t* ompi_comm);

static inline bool mac_pml_dr_comm_proc_check_acked(mca_pml_dr_comm_proc_t* proc, int32_t vfrag_id) { 
    mca_pml_dr_acked_item_t* item = (mca_pml_dr_acked_item_t*) proc->acked_vfrags_ptr;
    int8_t direction = 0; /* 1 is next, -1 is previous */
    while(true) { 
        if(NULL == item) { 
            return false;
        } else if(item->vfrag_id_high >= vfrag_id && item->vfrag_id_low <= vfrag_id) { 
            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return true; 
        } else if(vfrag_id > item->vfrag_id_high && direction != -1) { 
            direction = 1; 
            item = (mca_pml_dr_acked_item_t*) opal_list_get_next(item); 
        } else if(vfrag_id < item->vfrag_id_low && direction != 1) { 
            direction = -1;
            item = (mca_pml_dr_acked_item_t*) opal_list_get_prev(item); 
        } else { 
            return false;
        }
    }
}

static inline void mac_pml_dr_comm_proc_set_acked(mca_pml_dr_comm_proc_t* proc, int32_t vfrag_id) { 
    mca_pml_dr_acked_item_t* item = (mca_pml_dr_acked_item_t*) proc->acked_vfrags_ptr;
    int8_t direction = 0; /* 1 is next, -1 is previous */
    mca_pml_dr_acked_item_t *new_item, *next_item, *prev_item;
    while(true) { 
        if(NULL == item && direction == 0) { 

            new_item = OBJ_NEW(mca_pml_dr_acked_item_t);
            new_item->vfrag_id_low = new_item->vfrag_id_high = vfrag_id; 
            opal_list_append(&proc->acked_vfrags, (opal_list_item_t*) item);
            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return;
        
        } else if (NULL == item && direction == 1) { 

            opal_list_append(&proc->acked_vfrags, (opal_list_item_t*) item);
            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return;
            
        } else if (NULL == item && direction == -1) { 

            opal_list_prepend(&proc->acked_vfrags, (opal_list_item_t*) item); 
            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return; 
            
        } else if(item->vfrag_id_high >= vfrag_id && item->vfrag_id_low <= vfrag_id ) { 

            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return; 

        } else if((item->vfrag_id_high + 1) == vfrag_id) { 
            
            next_item = (mca_pml_dr_acked_item_t*) opal_list_get_next(item); 
            /* try to consolidate */ 
            if(next_item && next_item->vfrag_id_low == (vfrag_id+1)) { 
                item->vfrag_id_high = next_item->vfrag_id_high;
                opal_list_remove_item(&proc->acked_vfrags, (opal_list_item_t*) next_item);
                OBJ_RELEASE(next_item);
            } else { 
                item->vfrag_id_high = vfrag_id;
            }    
            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return; 
            
        } else if((item->vfrag_id_low - 1) == vfrag_id) { 
            
            prev_item = (mca_pml_dr_acked_item_t*) opal_list_get_prev(item);
            /* try to consolidate */
            if(prev_item && prev_item->vfrag_id_high == (vfrag_id-1)) {  
                item->vfrag_id_low = prev_item->vfrag_id_low;
                opal_list_remove_item(&proc->acked_vfrags, (opal_list_item_t*) prev_item);
                OBJ_RELEASE(prev_item);
            } else { 
                item->vfrag_id_low = vfrag_id; 
            }
            proc->acked_vfrags_ptr = (opal_list_item_t*) item;
            return; 
            
        } else if(vfrag_id > item->vfrag_id_high ) { 
            if(direction == -1) { 
                /* we have gone back in the list, and we went one item too far */ 
                new_item = OBJ_NEW(mca_pml_dr_acked_item_t);
                new_item->vfrag_id_low = new_item->vfrag_id_high = vfrag_id; 
                /* insert new_item directly before item */
                opal_list_insert_pos(&proc->acked_vfrags, 
                                     (opal_list_item_t*) item, 
                                     (opal_list_item_t*) new_item); 
                proc->acked_vfrags_ptr = (opal_list_item_t*) item;
                return;
            } else { 
                direction = 1;
                item = (mca_pml_dr_acked_item_t*) opal_list_get_next(item);
            }
        } else if(vfrag_id < item->vfrag_id_low) { 
            if(direction == 1) { 
                /* we have gone forward in the list, and we went one item too far */ 
                new_item = OBJ_NEW(mca_pml_dr_acked_item_t); 
                next_item = (mca_pml_dr_acked_item_t*) opal_list_get_next(item);
                if(NULL == next_item) { 
                    opal_list_append(&proc->acked_vfrags, (opal_list_item_t*) new_item);
                } else { 
                    opal_list_insert_pos(&proc->acked_vfrags, 
                                         (opal_list_item_t*) next_item, 
                                         (opal_list_item_t*) new_item); 
                }
                proc->acked_vfrags_ptr = (opal_list_item_t*) item;
                return;
            } else { 
                direction = -1;
                item = (mca_pml_dr_acked_item_t*) opal_list_get_prev(item); 
            }
        } else { 
            return;
        }
    }
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

