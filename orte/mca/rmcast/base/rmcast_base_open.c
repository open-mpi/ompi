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
#include "orte/util/show_help.h"

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
    mca_base_param_reg_string_name("rmcast", "base_scope",
                                "Scope of the multicast system [link (default) | site | org | global]",
                                false, false, "link", &tmp);
    if (0 == strcasecmp(tmp, "site")) {
        orte_rmcast_base.octet1[0] = 239;
        orte_rmcast_base.octet1[1] = 239;
        orte_rmcast_base.octet2[0] = 255;
        orte_rmcast_base.octet2[1] = 255;
        orte_rmcast_base.octet3[0] = 0;
        orte_rmcast_base.octet3[1] = 255;
    } else if (0 == strcasecmp(tmp, "org")) {
        orte_rmcast_base.octet1[0] = 239;
        orte_rmcast_base.octet1[1] = 239;
        orte_rmcast_base.octet2[0] = 192;
        orte_rmcast_base.octet2[1] = 195;
        orte_rmcast_base.octet3[0] = 0;
        orte_rmcast_base.octet3[1] = 255;
    } else if (0 == strcasecmp(tmp, "global")) {
        orte_rmcast_base.octet1[0] = 224;
        orte_rmcast_base.octet1[1] = 238;
        orte_rmcast_base.octet2[0] = 0;
        orte_rmcast_base.octet2[1] = 255;
        orte_rmcast_base.octet3[0] = 1;
        orte_rmcast_base.octet3[1] = 255;
    } else if (0 == strcasecmp(tmp, "link")) {
        /* default to link */
        orte_rmcast_base.octet1[0] = 224;
        orte_rmcast_base.octet1[1] = 224;
        orte_rmcast_base.octet2[0] = 0;
        orte_rmcast_base.octet2[1] = 0;
        orte_rmcast_base.octet3[0] = 0;
        orte_rmcast_base.octet3[1] = 0;
    } else {
        orte_show_help("help-rmcast-base.txt", "unrecognized-scope", true, tmp);
        return ORTE_ERR_SILENT;
    }

    /* channel offset */
    mca_base_param_reg_int_name("rmcast", "base_starting_channel",
                                "Offset to use within each network when computing channel (default: 0)",
                                false, false, 0, &value);
    /* check for correctness of value */
    if (value < 0 || value > 255) {
        orte_show_help("help-rmcast-base.txt", "value-range", true,
                       "starting channel", value, "0-255");
        return ORTE_ERR_SILENT;
    }
    orte_rmcast_base.channel_offset = (uint8_t)value;

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
