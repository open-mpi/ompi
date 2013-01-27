/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_BASESMUMA_UTILS_H
#define MCA_BCOL_BASESMUMA_UTILS_H

#include "ompi_config.h"

BEGIN_C_DECLS

#define BASESMUMA_K_NOMIAL_SEND_CHILDREN(radix_mask,radix,relative_index, \
        my_group_index, group_size, ready_flag) \
do {  \
    int k, child; \
    while(radix_mask > 0){ \
        for(k = 1; k < radix && relative_index+radix_mask*k<group_size; \
                k++) {\
            child = my_group_index+radix_mask*k;  \
            if(child >= group_size) {   \
                child -= group_size; \
            } \
            /*fprintf(stderr,"I am %d sending to child %d\n",my_group_index,child);*/ \
            child_ctl_pointer = data_buffs[child].ctl_struct; \
            child_ctl_pointer->src = my_group_index;  \
            /* this can be improved to make better asynchronous progress, but it's 
             * fine for now.            
             */                                                                 \
            while(child_ctl_pointer->sequence_number != sequence_number );       \
            child_ctl_pointer->flags[BCAST_FLAG][bcol_id] = ready_flag;  \
        } \
        radix_mask = radix_mask/radix; \
    } \
} while( 0 )




/*
 *  Return closet power of K that is greater than or equal to "number".
 */
int pow_sm_k(int radix_k, int group_size, int *pow_k_group_size);

/*
 * Get list of possible sources from which data may arrive based on a K-nomial tree fan-out.
 */

int get_k_nomial_src_list(int group_size, int radix, 
                          int my_index, int *src_list);


int get_k_nomial_dst_size(int group_size, int radix, int my_index);

END_C_DECLS

#endif
