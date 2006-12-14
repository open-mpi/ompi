/*
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_PTL_IB_PARAMS_H
#define MCA_PTL_IB_PARAMS_H

#include "btl_openib.h"


/*
 * Struct to hold the settable values that may be specified in the INI
 * file
 */
typedef struct ompi_btl_openib_ini_values_t {
    uint32_t mtu;
    bool mtu_set;

    uint32_t use_eager_rdma;
    bool use_eager_rdma_set;
} ompi_btl_openib_ini_values_t;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Read in the INI files containing HCA params
     */
    int ompi_btl_openib_ini_init(void);

    /**
     * Query the read-in params for a given HCA
     */
    int ompi_btl_openib_ini_query(uint32_t vendor_id,
                                  uint32_t vendor_part_id,
                                  ompi_btl_openib_ini_values_t *values);

    /**
     * Shut down / release all internal state
     */
    int ompi_btl_openib_ini_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
