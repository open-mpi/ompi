/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#if !ORTE_DISABLE_FULL_SUPPORT

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"

#include "orte/util/parse_options.h"
#include "orte/mca/errmgr/errmgr.h"

#endif

#include "orte/mca/rmcast/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmcast/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_rmcast_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Global variables
 */
orte_rmcast_module_t orte_rmcast = {
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
orte_rmcast_base_t orte_rmcast_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmcast_base_open(void)
{
    int value, pval, i;
    char *tmp, **ports=NULL;
    
    /* public multicast channel for this job */
    mca_base_param_reg_int_name("rmcast", "base_multicast_subnet",
                                "Multicast subnet for the ORTE system (default: 239)",
                                false, false, (int)ORTE_RMCAST_DEFAULT_SUBNET, &value);
    /* check for correctness of value */
    orte_rmcast_base.subnet = (uint8_t)value;
    
    /* scope of the public channel */
    mca_base_param_reg_int_name("rmcast", "base_multicast_scope",
                                "Scope of the multicast channel (default: 255)",
                                false, false, (int)ORTE_RMCAST_DEFAULT_SCOPE, &value);
    /* check for correctness of value */
    orte_rmcast_base.scope = (uint8_t)value;
    
    
    /* range of available ports */
    mca_base_param_reg_string_name("rmcast", "base_multicast_ports",
                                "Ports available for multicast channels (default: 6900-7155)",
                                false, false, "6900-7155", &tmp);
    orte_util_parse_range_options(tmp, &ports);
    for (i=0; i < opal_argv_count(ports); i++) {
        pval = strtoul(ports[i], NULL, 10);
        if (pval >= UINT16_MAX) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
        }
        orte_rmcast_base.ports[i] = pval;
    }
    
    /* create the base IP multicase addr */
    orte_rmcast_base.af_family = AF_INET;
    orte_rmcast_base.base_ip_addr = ((orte_rmcast_base.subnet << 24) & 0xFF000000) |
                                    ((orte_rmcast_base.scope << 16) & 0x00FF0000);
    
    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_rmcast_base.rmcast_output = opal_output_open(NULL);
    
    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("rmcast", orte_rmcast_base.rmcast_output,
                                 mca_rmcast_base_static_components, 
                                 &orte_rmcast_base.rmcast_opened, true)) {
        return ORTE_ERROR;
    }

    /* All done */
    return ORTE_SUCCESS;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */
