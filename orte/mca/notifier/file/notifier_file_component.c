/* -*- C -*-
*
* Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
*                         University Research and Technology
*                         Corporation.  All rights reserved.
* Copyright (c) 2004-2005 The University of Tennessee and The University
*                         of Tennessee Research Foundation.  All rights
*                         reserved.
* Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
*                         University of Stuttgart.  All rights reserved.
* Copyright (c) 2004-2005 The Regents of the University of California.
*                         All rights reserved.
* Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
* Copyright (c) 2009      Bull SAS.  All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/
/** @file:
*
*/

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"

#include "notifier_file.h"


static int orte_notifier_file_register(void);
static int orte_notifier_file_component_query(mca_base_module_t **, int *);
static int orte_notifier_file_close(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_notifier_file_component_t mca_notifier_file_component = {
    {
        {
            ORTE_NOTIFIER_BASE_VERSION_1_0_0,

            "file", /* MCA module name */
            ORTE_MAJOR_VERSION,  /* MCA module major version */
            ORTE_MINOR_VERSION,  /* MCA module minor version */
            ORTE_RELEASE_VERSION,  /* MCA module release version */
            NULL,
            orte_notifier_file_close,
            orte_notifier_file_component_query,
            orte_notifier_file_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },

    /* Log file name */
    "wdc",

    /* Priority */
    10
};

static int orte_notifier_file_register(void)
{
    mca_base_component_t *mcb = &mca_notifier_file_component.super.base_version;
    orte_notifier_file_component_t *nfc = &mca_notifier_file_component;

    mca_base_param_reg_string(mcb, "name",
                              "File name the traces should be redirected to",
                              false, false, nfc->fname, &nfc->fname);
    mca_base_param_reg_int(mcb, "priority",
                           "Priority of the file notifier component",
                           false, false, nfc->priority, &nfc->priority);

    return ORTE_SUCCESS;
}

static int orte_notifier_file_close(void)
{
    if (NULL != mca_notifier_file_component.fname) {
        free(mca_notifier_file_component.fname);
    }

    return ORTE_SUCCESS;
}

static int orte_notifier_file_component_query(mca_base_module_t **module, 
                                              int *priority)
{
    *priority = mca_notifier_file_component.priority;
    *module = (mca_base_module_t *)&orte_notifier_file_module;

    if (NULL == mca_notifier_file_component.fname ||
        !strlen(mca_notifier_file_component.fname)) {
        orte_show_help("help-orte-notifier-file.txt",
                       "file name not specified", true);
        return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;    
}

