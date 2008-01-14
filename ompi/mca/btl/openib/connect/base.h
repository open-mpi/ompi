/*
 * Copyright (c) 2007 Cisco, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef BTL_OPENIB_CONNECT_BASE_H
#define BTL_OPENIB_CONNECT_BASE_H

#include "connect/connect.h"

BEGIN_C_DECLS

/**
 * Global variable with the selected function pointers in it
 */
extern ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect;

/*
 * Open function
 */
int ompi_btl_openib_connect_base_open(void);

/*
 * Select function
 */
int ompi_btl_openib_connect_base_select(char*, char*);
int ompi_btl_openib_connect_base_query(char**, mca_btl_openib_hca_t*);

END_C_DECLS

#endif
