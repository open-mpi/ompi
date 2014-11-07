/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _PMIX_SERVER_H_
#define _PMIX_SERVER_H_

#include "orte_config.h"
#include "opal/mca/dstore/base/base.h"


BEGIN_C_DECLS

ORTE_DECLSPEC int pmix_server_init(void);
ORTE_DECLSPEC void pmix_server_finalize(void);
ORTE_DECLSPEC void pmix_server_register(void);

/* provide access to the pmix server uri */
ORTE_DECLSPEC extern char *pmix_server_uri;

ORTE_DECLSPEC extern opal_dstore_attr_t *pmix_server_create_shared_segment(orte_jobid_t jid);

END_C_DECLS

#endif /* PMIX_SERVER_H_ */

