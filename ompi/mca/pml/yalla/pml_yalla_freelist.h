/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_YALLA_FREELIST_H_
#define PML_YALLA_FREELIST_H_

#include "ompi_config.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/class/opal_free_list.h"


#if OMPI_ENABLE_THREAD_MULTIPLE

#define mca_pml_yalla_freelist_t ompi_free_list_t

#define PML_YALLA_FREELIST_GET(_freelist) \
    ({ \
        ompi_free_list_item_t *item; \
        OMPI_FREE_LIST_GET_MT(_freelist, item); \
        (void*)(item); \
    })

#define PML_YALLA_FREELIST_RETURN(_freelist, _item) \
    { \
        OMPI_FREE_LIST_RETURN_MT(_freelist, _item); \
    }

#define PML_YALLA_FREELIST_INIT(_fl, _type, _initial, _max, _batch) \
    ompi_free_list_init_new(_fl, sizeof(_type), opal_cache_line_size, \
                            OBJ_CLASS(_type), 0, opal_cache_line_size, \
                            _initial, _max, _batch, NULL);


#else

#define mca_pml_yalla_freelist_t opal_free_list_t

#define PML_YALLA_FREELIST_GET(_freelist) \
    ({ \
        opal_free_list_item_t *item; \
        int rc; \
        OPAL_FREE_LIST_GET(_freelist, item, rc); \
        (void*)(item); \
    })

#define PML_YALLA_FREELIST_RETURN(_freelist, _item) \
    { \
        OPAL_FREE_LIST_RETURN(_freelist, _item); \
    }

#define PML_YALLA_FREELIST_INIT(_fl, _type, _initial, _max, _batch) \
    opal_free_list_init(_fl, sizeof(_type), OBJ_CLASS(_type), \
                        _initial, _max, _batch);

#endif



#endif /* PML_YALLA_FREELIST_H_ */
