/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTED_H
#define PRTED_H

#include "prte_config.h"
#include "types.h"

#include <time.h>

#include "src/class/pmix_pointer_array.h"
#include "src/rml/rml_types.h"
#include "src/mca/schizo/schizo.h"
#include "src/util/prte_cmd_line.h"

BEGIN_C_DECLS

/* main orted routine */
PRTE_EXPORT int prte_daemon(int argc, char *argv[]);

/* orted communication functions */
PRTE_EXPORT void prte_daemon_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                  prte_rml_tag_t tag, void *cbdata);

/* direct cmd processing entry points */
PRTE_EXPORT void prte_daemon_cmd_processor(int fd, short event, void *data);
PRTE_EXPORT int prte_daemon_process_commands(pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                             prte_rml_tag_t tag);

PRTE_EXPORT int prte_parse_locals(prte_schizo_base_module_t *schizo, pmix_list_t *jdata,
                                  char **argv, char ***hostfiles, char ***hosts);

PRTE_EXPORT int prun_common(pmix_cli_result_t *cli,
                            prte_schizo_base_module_t *schizo,
                            int argc, char **argv);

END_C_DECLS

#endif /* PRTED_H */
