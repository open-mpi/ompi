/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
    
#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "util/output.h"


int mca_oob_ping(ompi_process_name_t* peer, struct timeval* tv)
{
    return(mca_oob.oob_ping(peer, tv));
}

