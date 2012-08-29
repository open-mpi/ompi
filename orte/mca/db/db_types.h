/*
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE Database Framework
 *
 */

#ifndef ORTE_DB_TYPES_H
#define ORTE_DB_TYPES_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

/* define some common keys used in ORTE */
#define ORTE_DB_HOSTNAME     "orte.hostname"
#define ORTE_DB_DAEMON_VPID  "orte.daemon.vpid"
#define ORTE_DB_NODERANK     "orte.node.rank"
#define ORTE_DB_LOCALRANK    "orte.local.rank"
#define ORTE_DB_BIND_LEVEL   "orte.bind.level"
#define ORTE_DB_BIND_INDEX   "orte.bind.index"
#define ORTE_DB_LOCALITY     "orte.locality"
#define ORTE_DB_ARCH         "orte.arch"
#define ORTE_DB_NPROCS       "orte.nprocs"
#define ORTE_DB_RMLURI       "orte.rmluri"
#define ORTE_DB_BIND_BITMAP  "orte.bind.bitmap"

END_C_DECLS

#endif
