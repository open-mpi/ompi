/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
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
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#ifndef _ORTE_NIDMAP_H_
#define _ORTE_NIDMAP_H_

#include "orte_config.h"
#include "orte/types.h"

#include "opal/dss/dss_types.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

#define ORTE_MAX_NODE_PREFIX        50
#define ORTE_CONTIG_NODE_CMD        0x01
#define ORTE_NON_CONTIG_NODE_CMD    0x02

ORTE_DECLSPEC int orte_util_nidmap_init(opal_buffer_t *buffer);
ORTE_DECLSPEC void orte_util_nidmap_finalize(void);

ORTE_DECLSPEC int orte_util_encode_nodemap(opal_byte_object_t *boptr, bool update);
ORTE_DECLSPEC int orte_util_decode_nodemap(opal_byte_object_t *boptr);
ORTE_DECLSPEC int orte_util_decode_daemon_nodemap(opal_byte_object_t *bo);

ORTE_DECLSPEC int orte_util_encode_pidmap(opal_byte_object_t *boptr, bool update);
ORTE_DECLSPEC int orte_util_decode_pidmap(opal_byte_object_t *boptr);
ORTE_DECLSPEC int orte_util_decode_daemon_pidmap(opal_byte_object_t *bo);

#if ORTE_ENABLE_STATIC_PORTS
ORTE_DECLSPEC int orte_util_build_daemon_nidmap(char **nodes);
#endif

END_C_DECLS

#endif
