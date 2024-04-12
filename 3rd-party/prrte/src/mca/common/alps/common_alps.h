/*
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2012-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _COMMON_ALPS_H_
#define _COMMON_ALPS_H_

#include "prte_config.h"

BEGIN_C_DECLS

/**
 * Determine if calling process is in a Cray PAGG job container.
 * flag set to TRUE if the process is in a PAGG, otherwise FALSE.
 */
PRTE_EXPORT int prte_common_alps_proc_in_pagg(bool *flag);

END_C_DECLS

#endif
