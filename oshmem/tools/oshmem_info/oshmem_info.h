/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_INFO_TOOL_H
#define OSHMEM_INFO_TOOL_H
#include "oshmem_config.h"

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/cmd_line.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

/*
 * Globals
 */

extern const char *oshmem_info_type_oshmem;

void oshmem_info_do_config(bool want_all);

END_C_DECLS

#endif /* OSHMEM_INFO_TOOL_H */
