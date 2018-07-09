/*
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_IWARP_MCA_H
#define MCA_BTL_IWARP_MCA_H

BEGIN_C_DECLS

    /**
     * Function to register MCA params and check for sane values
     */
    int btl_iwarp_register_mca_params(void);
    int btl_iwarp_verify_mca_params (void);

END_C_DECLS
#endif
