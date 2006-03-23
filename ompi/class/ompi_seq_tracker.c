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

#include "ompi_config.h"

#include "ompi/class/ompi_seq_tracker.h"
#include "opal/sys/cache.h"
#include "opal/util/output.h"



OBJ_CLASS_INSTANCE(ompi_seq_tracker_range_t, 
                   opal_list_item_t, 
                   NULL, 
                   NULL);


static void ompi_seq_tracker_construct(ompi_seq_tracker_t* seq_tracker) { 
    OBJ_CONSTRUCT(&seq_tracker->seq_ids, opal_list_t);
    seq_tracker->seq_ids_current = NULL;
}


static void ompi_seq_tracker_destruct(ompi_seq_tracker_t* seq_tracker) { 
    OBJ_DESTRUCT(&seq_tracker->seq_ids);
}

OBJ_CLASS_INSTANCE(
                   ompi_seq_tracker_t,
                   opal_object_t,
                   ompi_seq_tracker_construct,
                   ompi_seq_tracker_destruct);


/**
 *  Look for duplicate sequence number in current range.
 *  Must be called w/ matching lock held.
 */

bool ompi_seq_tracker_check_duplicate(
    ompi_seq_tracker_t* seq_tracker, 
    uint32_t seq_id) 
{ 
    ompi_seq_tracker_range_t* item;
    int8_t direction = 0; /* 1 is next, -1 is previous */

    item = seq_tracker->seq_ids_current;
    while(true) { 
        if(NULL == item) { 
            return false;
        } else if(item->seq_id_high >= seq_id && item->seq_id_low <= seq_id) { 
            seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) item;
            return true; 
        } else if(seq_id > item->seq_id_high && direction != -1) { 
            direction = 1; 
            item = (ompi_seq_tracker_range_t*) opal_list_get_next(item); 
        } else if(seq_id < item->seq_id_low && direction != 1) { 
            direction = -1;
            item = (ompi_seq_tracker_range_t*) opal_list_get_prev(item); 
        } else { 
            return false;
        }
    }
}


/*
 * insert item into sequence tracking list,
 *   compacts continuous regions into a single entry
 */
void ompi_seq_tracker_insert(ompi_seq_tracker_t* seq_tracker, 
                                                uint32_t seq_id) 
{ 
    opal_list_t* seq_ids = &seq_tracker->seq_ids; 
    ompi_seq_tracker_range_t* item = seq_tracker->seq_ids_current;
    int8_t direction = 0; /* 1 is next, -1 is previous */
    ompi_seq_tracker_range_t *new_item, *next_item, *prev_item;
    while(true) { 
        if( item == NULL || item == (ompi_seq_tracker_range_t*) &seq_ids->opal_list_tail )  { 
            new_item = OBJ_NEW(ompi_seq_tracker_range_t);
            new_item->seq_id_low = new_item->seq_id_high = seq_id; 
            opal_list_append(seq_ids, (opal_list_item_t*) new_item);
            seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) new_item;
            return;
        } else if( item == (ompi_seq_tracker_range_t*) &seq_ids->opal_list_head ) { 
            new_item = OBJ_NEW(ompi_seq_tracker_range_t);
            new_item->seq_id_low = new_item->seq_id_high = seq_id; 
            opal_list_prepend(seq_ids, (opal_list_item_t*) new_item); 
            seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) new_item;
            return; 
            
        } else if(item->seq_id_high >= seq_id && item->seq_id_low <= seq_id ) { 

            seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) item;
            return; 

        } else if((item->seq_id_high + 1) == seq_id) { 
            
            next_item = (ompi_seq_tracker_range_t*) opal_list_get_next(item); 
            /* try to consolidate */ 
            if(next_item && next_item->seq_id_low == (seq_id+1)) { 
                item->seq_id_high = next_item->seq_id_high;
                opal_list_remove_item(seq_ids, (opal_list_item_t*) next_item);
                OBJ_RELEASE(next_item);
            } else { 
                item->seq_id_high = seq_id;
            }    
            seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) item;
            return; 
            
        } else if((item->seq_id_low - 1) == seq_id) { 
            
            prev_item = (ompi_seq_tracker_range_t*) opal_list_get_prev(item);
            /* try to consolidate */
            if(prev_item && prev_item->seq_id_high == (seq_id-1)) {  
                item->seq_id_low = prev_item->seq_id_low;
                opal_list_remove_item(seq_ids, (opal_list_item_t*) prev_item);
                OBJ_RELEASE(prev_item);
            } else { 
                item->seq_id_low = seq_id; 
            }
            seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) item;
            return; 
            
        } else if(seq_id > item->seq_id_high ) { 
            if(direction == -1) { 
                /* we have gone back in the list, and we went one item too far */ 
                new_item = OBJ_NEW(ompi_seq_tracker_range_t);
                new_item->seq_id_low = new_item->seq_id_high = seq_id; 
                /* insert new_item directly before item */
                opal_list_insert_pos(seq_ids, 
                                     (opal_list_item_t*) item, 
                                     (opal_list_item_t*) new_item); 
                seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) new_item;
                return;
            } else { 
                direction = 1;
                item = (ompi_seq_tracker_range_t*) opal_list_get_next(item);
            }
        } else if(seq_id < item->seq_id_low) { 
            if(direction == 1) { 
                /* we have gone forward in the list, and we went one item too far */ 
                new_item = OBJ_NEW(ompi_seq_tracker_range_t); 
                next_item = (ompi_seq_tracker_range_t*) opal_list_get_next(item);
                if(NULL == next_item) { 
                    opal_list_append(seq_ids, (opal_list_item_t*) new_item);
                } else { 
                    opal_list_insert_pos(seq_ids, 
                                         (opal_list_item_t*) next_item, 
                                         (opal_list_item_t*) new_item); 
                }
                seq_tracker->seq_ids_current = (ompi_seq_tracker_range_t*) new_item;
                return;
            } else { 
                direction = -1;
                item = (ompi_seq_tracker_range_t*) opal_list_get_prev(item); 
            }
        } else { 
            return;
        }
    }
}





