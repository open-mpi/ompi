/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
    
#include "include/constants.h"
#include "opal/util/argv.h"
#include "mca/ns/ns_types.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


int mca_oob_ping(const char* contact_info, struct timeval* tv)
{
    orte_process_name_t name;
    char** uris;
    char** ptr;
    int rc;

    if(ORTE_SUCCESS != (rc = mca_oob_parse_contact_info(contact_info, &name, &uris))) {
        return rc;
    }
 
    ptr = uris;
    while(ptr && *ptr) {
        if(ORTE_SUCCESS == (rc = mca_oob.oob_ping(&name, *ptr, tv)))
            break;
        ptr++;
    }
    opal_argv_free(uris);
    return rc;
}

