/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/** @file:
 *
 */

#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/mca/mca.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"

/*
 * "not available" functions
 */
int
orte_ns_base_module_init_not_available(void)
{
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_assign_rml_tag_not_available(orte_rml_tag_t *tag, char *name)
{
    *tag = ORTE_RML_TAG_MAX;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_define_data_type_not_available(
                                  const char *name,
                                  orte_data_type_t *type)
{
    *type = ORTE_DSS_ID_MAX;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

/****    GET PEERS    ****/
int orte_ns_base_get_peers_not_available(orte_process_name_t **procs,
                                         orte_std_cntr_t *num_procs, opal_list_t *attributes)
{
    *procs = NULL;
    *num_procs = 0;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}
