/*
 * Copyright (c) 2016      Intel, Inc. All rights reserved.
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

#ifndef _ORTE_CMD_LINE_H_
#define _ORTE_CMD_LINE_H_

#include "orte_config.h"

#ifdef HAVE_STDINT_h
#include <stdint.h>
#endif

#include "orte/types.h"

#include "opal/util/cmd_line.h"

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_cmd_line_create(opal_cmd_line_t *cmd_line,
                                       int argc, char **argv,
                                       char ***context_env, char ***global_env,
                                       bool *version, bool *help);

END_C_DECLS
#endif
