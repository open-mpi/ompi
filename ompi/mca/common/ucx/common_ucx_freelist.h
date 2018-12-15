/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef COMMON_UCX_FREELIST_H_
#define COMMON_UCX_FREELIST_H_

#include "opal/class/opal_free_list.h"


#define mca_common_ucx_freelist_t opal_free_list_t

#define COMMON_UCX_FREELIST_GET(_freelist) \
     opal_free_list_get (_freelist)

#define COMMON_UCX_FREELIST_RETURN(_freelist, _item) \
        opal_free_list_return(_freelist, _item) 

#define COMMON_UCX_FREELIST_INIT(_fl, _type, _headroom, _initial, _max, _batch) \
    opal_free_list_init(_fl, sizeof(_type) + _headroom, 8, OBJ_CLASS(_type), \
                        0, 0, _initial, _max, _batch, NULL, 0, NULL, NULL, NULL)


#endif /* COMMON_UCX_FREELIST_H_ */
