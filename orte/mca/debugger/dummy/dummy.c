/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Test C99 struct initialization. Remove on 1/20/201 after MTT has run.
 *
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif  /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/os_path.h"
#include "opal/util/opal_sos.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_getcwd.h"
#include "opal/mca/event/event.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"

#include "orte/mca/debugger/base/base.h"
#include "dummy.h"

/* Static API's */
static int init(void);
static void finalize(void);
static void init_before_spawn(orte_job_t *jdata);

/* Module definition */
orte_debugger_base_module_t orte_debugger_dummy_module = {
    .init              = init,
    .finalize          = finalize,
    .init_before_spawn = init_before_spawn,
    .init_after_spawn  = orte_debugger_base_init_after_spawn
};

/* local globals and functions */

static int init(void)
{
    return ORTE_SUCCESS;
}

void finalize(void)
{
}

void init_before_spawn(orte_job_t *jdata)
{
}
