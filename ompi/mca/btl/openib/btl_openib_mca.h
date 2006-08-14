/*
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_IB_MCA_H
#define MCA_BTL_IB_MCA_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Function to register MCA params and check for sane values 
     */
    int btl_openib_register_mca_params(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
