/*
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTED_SUBMIT_H
#define ORTED_SUBMIT_H

#include "orte_config.h"

#include "orte/mca/plm/plm.h"
#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS


typedef void (*orte_submit_cbfunc_t)(int index, orte_job_t *jdata, int ret, void *cbdata);

ORTE_DECLSPEC int orte_submit_init(int argc, char *argv[],
                                   opal_cmd_line_t *opts);
ORTE_DECLSPEC int orte_submit_cancel(int index);
ORTE_DECLSPEC void orte_submit_finalize(void);
ORTE_DECLSPEC int orte_submit_job(char *cmd[], int *index,
                                  orte_submit_cbfunc_t launch_cb, void *launch_cbdata,
                                  orte_submit_cbfunc_t complete_cb, void *complete_cbdata);
ORTE_DECLSPEC int orte_submit_halt(void);


END_C_DECLS

#endif /* ORTED_SUBMIT_H */
