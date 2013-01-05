/*
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#include <sys/types.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#include "orte/mca/db/base/base.h"
#include "db_gpdb.h"

static int init(void);
static void finalize(void);
static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs);

orte_db_base_module_t orte_db_gpdb_module = {
    init,
    finalize,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    add_log
};

/* local variables */
static int init(void)
{
    return ORTE_SUCCESS;
}

static void finalize(void)
{
}

static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs)
{
    opal_output_verbose(2, orte_db_base.output,
                        "%s Logging data for table %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), table);

    return ORTE_ERR_NOT_IMPLEMENTED;
}
