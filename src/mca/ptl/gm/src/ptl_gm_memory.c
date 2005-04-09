/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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

#include "ptl_gm.h"
#include "ptl_gm_priv.h"

gm_status_t mca_ptl_gm_register_memory(struct gm_port *port, void *ptr, unsigned len)
{
#if OMPI_MCA_PTL_GM_SUPPORT_REGISTERING
#if OMPI_MCA_PTL_GM_CACHE_ENABLE
    gmpi_use_interval( port, (gm_up_t)ptr, len );
    return GM_SUCCESS;
#else
    return gm_register_memory( port, ptr, len );
#endif  /* OMPI_MCA_PTL_GM_CACHE_ENABLE */
#else
    return GM_FAILURE;
#endif  /* OMPI_MCA_PTL_GM_SUPPORT_REGISTERING */
}

gm_status_t mca_ptl_gm_deregister_memory( struct gm_port *port, void *ptr, unsigned len )
{
#if OMPI_MCA_PTL_GM_SUPPORT_REGISTERING
#if OMPI_MCA_PTL_GM_CACHE_ENABLE
    return gmpi_unuse_interval( port, (gm_up_t)ptr, len );
#else
    return gm_deregister_memory( port, ptr, len );
#endif  /* OMPI_MCA_PTL_GM_CACHE_ENABLE */
#else
    return GM_FAILURE;
#endif  /*  OMPI_MCA_PTL_GM_SUPPORT_REGISTERING */
}
