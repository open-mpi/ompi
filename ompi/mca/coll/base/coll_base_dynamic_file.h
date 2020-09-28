/*
 * Copyright (c) 2020      Bull SAS. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_DYNAMIC_FILE_H_HAS_BEEN_INCLUDED
#define MCA_COLL_BASE_DYNAMIC_FILE_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"

/* also need the dynamic rule structures */
#include "coll_base_dynamic_rules.h"


BEGIN_C_DECLS

int ompi_coll_base_read_rules_config_file (char *fname, int format_version, ompi_coll_base_alg_rule_t** rules, int n_collectives);


END_C_DECLS
#endif /* MCA_COLL_BASE_DYNAMIC_FILE_H_HAS_BEEN_INCLUDED */


