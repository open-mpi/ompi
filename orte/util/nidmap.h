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
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_NIDMAP_H
#define ORTE_NIDMAP_H

#include "orte_config.h"

#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"
#include "orte/runtime/orte_globals.h"

/* pass info about the nodes in an allocation */
ORTE_DECLSPEC int orte_util_nidmap_create(opal_pointer_array_t *pool,
                                          opal_buffer_t *buf);

ORTE_DECLSPEC int orte_util_decode_nidmap(opal_buffer_t *buf);


/* pass topology and #slots info */
ORTE_DECLSPEC int orte_util_pass_node_info(opal_buffer_t *buf);

ORTE_DECLSPEC int orte_util_parse_node_info(opal_buffer_t *buf);


/* pass info about node assignments for a specific job */
ORTE_DECLSPEC int orte_util_generate_ppn(orte_job_t *jdata,
                                         opal_buffer_t *buf);

ORTE_DECLSPEC int orte_util_decode_ppn(orte_job_t *jdata,
                                       opal_buffer_t *buf);

#endif /* ORTE_NIDMAP_H */
