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
 * Copyright (c) 2009     Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
*/

/*
 * Simple smtp notifier (using libesmtp)
 */

#include "orte_config.h"

#include "opal/mca/base/mca_base_var.h"

#include "orte/constants.h"
#include "orte/util/show_help.h"

#include "notifier_smtp.h"

static int smtp_component_query(mca_base_module_t **module, int *priority);
static int smtp_close(void);
static int smtp_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_notifier_smtp_component_t mca_notifier_smtp_component = {
    {
        {
            ORTE_NOTIFIER_BASE_VERSION_1_0_0,
            
            "smtp",
            
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            NULL,
            smtp_close,
            smtp_component_query,
            smtp_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },
};

static int smtp_register(void)
{
    char version[256];

    /* Server stuff */
    mca_notifier_smtp_component.server = strdup("localhost");
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "server",
                                           "SMTP server name or IP address",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.server);

    mca_notifier_smtp_component.port = 25;
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "port",
                                           "SMTP server port",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.port);

    /* Email stuff */
    mca_notifier_smtp_component.to = NULL;
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "to",
                                           "Comma-delimited list of email addresses to send to",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.to);
    mca_notifier_smtp_component.from_addr = NULL;
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "from_addr",
                                           "Email address that messages will be from",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.from_addr);
    mca_notifier_smtp_component.from_name = strdup("ORTE Notifier");
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "from_name",
                                           "Email name that messages will be from",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.from_name);
    mca_notifier_smtp_component.subject = strdup("ORTE Notifier");
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "subject",
                                           "Email subject",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.subject);

    /* Mail body prefix and suffix */
    mca_notifier_smtp_component.body_prefix = strdup("The ORTE SMTP notifier wishes to inform you of the following message:\n\n");
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "body_prefix",
                                           "Text to put at the beginning of the mail message",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.body_prefix);
    mca_notifier_smtp_component.body_suffix = strdup("\n\nSincerely,\nOscar the ORTE Owl");
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "body_prefix",
                                           "Text to put at the end of the mail message",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.body_suffix);

    /* Priority */
    mca_notifier_smtp_component.priority = 10;
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "priority",
                                           "Priority of this component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.priority);
    /* Libesmtp version */
    smtp_version(version, sizeof(version), 0);
    version[sizeof(version) - 1] = '\0';
    mca_notifier_smtp_component.version = strdup(version);
    (void) mca_base_component_var_register(&mca_notifier_smtp_component.super.base_version, "libesmtp_version",
                                           "Version of libesmtp that this component is linked against",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_notifier_smtp_component.version);

    return ORTE_SUCCESS;
}

static int smtp_close(void)
{
    return ORTE_SUCCESS;
}

static int smtp_component_query(mca_base_module_t **module, 
                                int *priority)
{
    *priority = 0;
    *module = NULL;

    /* If there's no to or from, there's no love */
    if (NULL == mca_notifier_smtp_component.to ||
        '\0' == mca_notifier_smtp_component.to[0] ||
        NULL == mca_notifier_smtp_component.from_addr ||
        '\0' == mca_notifier_smtp_component.from_addr[0]) {
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "to/from not specified", true);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Sanity checks */
    if (NULL == mca_notifier_smtp_component.server ||
        '\0' == mca_notifier_smtp_component.server[0]) {
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "server not specified", true);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Since we have to open a socket later, try to resolve the IP
       address of the server now.  Save the result, or abort if we
       can't resolve it. */
    mca_notifier_smtp_component.server_hostent =
        gethostbyname(mca_notifier_smtp_component.server);
    if (NULL == mca_notifier_smtp_component.server_hostent) {
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "unable to resolve server",
                       true, mca_notifier_smtp_component.server);
        return ORTE_ERR_NOT_FOUND;
    }

    *priority = 10;
    *module = (mca_base_module_t *)&orte_notifier_smtp_module;
    return ORTE_SUCCESS;    
}
