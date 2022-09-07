/* -*- c -*-
 *
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration
 */

#ifndef OSHMEM_CONFIG_H
#define OSHMEM_CONFIG_H

/* Need to include a bunch of infrastructure from the OMPI layer */
#include "ompi_config.h"

#define OSHMEM_IDENT_STRING OPAL_IDENT_STRING

#  if OPAL_C_HAVE_VISIBILITY
#    ifndef OSHMEM_DECLSPEC
#      define OSHMEM_DECLSPEC            __opal_attribute_visibility__("default")
#    endif
#  else
#    ifndef OSHMEM_DECLSPEC
#      define OSHMEM_DECLSPEC
#    endif
#  endif

#endif
