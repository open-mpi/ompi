/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include "rml_oob.h"

int
orte_rml_oob_ping(void *mod,
                  const char* uri,
                  const struct timeval* tv)
{
    orte_rml_oob_module_t *module = (orte_rml_oob_module_t*)mod;
    return ORTE_ERR_NOT_SUPPORTED;
}
