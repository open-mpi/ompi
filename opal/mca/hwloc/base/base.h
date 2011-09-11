/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_HWLOC_BASE_H
#define OPAL_HWLOC_BASE_H

#include "opal_config.h"

#include "opal/dss/dss_types.h"

#include "opal/mca/hwloc/hwloc.h"

/*
 * Global functions for MCA overall hwloc open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the hwloc MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the hwloc MCA
 * framework.  It initializes the hwloc MCA framework, finds
 * and opens hwloc components, etc.
 *
 * This function is invoked during opal_init().
 * 
 * This function fills in the internal global variable
 * opal_hwloc_base_components_opened, which is a list of all
 * hwloc components that were successfully opened.  This
 * variable should \em only be used by other hwloc base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OPAL_DECLSPEC int opal_hwloc_base_open(void);

/**
 * Shut down the hwloc MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the hwloc MCA
 * framework, and is called during opal_finalize().
 *
 * It must be the last function invoked on the hwloc MCA
 * framework.
 */
OPAL_DECLSPEC int opal_hwloc_base_close(void);

/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern int opal_hwloc_base_output;
OPAL_DECLSPEC extern opal_list_t opal_hwloc_components;
OPAL_DECLSPEC extern bool opal_hwloc_base_inited;
OPAL_DECLSPEC extern bool opal_hwloc_topology_inited;

#if OPAL_HAVE_HWLOC
/* datatype support */
OPAL_DECLSPEC int opal_hwloc_pack(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals,
                                  opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_unpack(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals,
                                    opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_copy(hwloc_topology_t *dest,
                                  hwloc_topology_t src,
                                  opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_compare(const hwloc_topology_t topo1,
                                     const hwloc_topology_t topo2,
                                     opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_print(char **output, char *prefix,
                                   hwloc_topology_t src,
                                   opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_size(size_t *size,
                                  hwloc_topology_t src,
                                  opal_data_type_t type);
OPAL_DECLSPEC void opal_hwloc_release(opal_dss_value_t *value);
#endif

END_C_DECLS

#endif /* OPAL_BASE_HWLOC_H */
