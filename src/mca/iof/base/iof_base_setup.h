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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#ifndef IOF_BASE_SETUP_H_
#define IOF_BASE_SETUP_H_

#include "mca/ns/ns.h"

struct orte_iof_base_io_conf_t {
    int p_stdin[2];
    int p_stdout[2];
    int p_stderr[2];

    int usepty;
};
typedef struct orte_iof_base_io_conf_t orte_iof_base_io_conf_t;


/**
 * Do pre-fork IOF setup tasks
 *
 * Do all stdio forwarding that must be done before fork() is called.
 * This might include creating pipes or ptys or similar work.
 */
int orte_iof_base_setup_prefork(orte_iof_base_io_conf_t *opts);

int orte_iof_base_setup_child(orte_iof_base_io_conf_t *opts);

int orte_iof_base_setup_parent(const orte_process_name_t* name,
                               orte_iof_base_io_conf_t *opts);

#endif
