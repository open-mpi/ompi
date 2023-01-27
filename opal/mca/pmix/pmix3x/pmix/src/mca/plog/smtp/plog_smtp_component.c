/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
*/

/*
 * Simple smtp plog (using libesmtp)
 */

#include "pmix_config.h"

#include "src/mca/base/pmix_mca_base_var.h"

#include "include/pmix_common.h"
#include "src/util/show_help.h"

#include "plog_smtp.h"

static pmix_status_t smtp_component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_status_t smtp_close(void);
static pmix_status_t smtp_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
pmix_plog_smtp_component_t mca_plog_smtp_component = {
    {
        .base = {
            PMIX_PLOG_BASE_VERSION_1_0_0,

            .pmix_mca_component_name = "smtp",

            PMIX_MCA_BASE_MAKE_VERSION(component, PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION,
                                       PMIX_RELEASE_VERSION),
            .pmix_mca_close_component = smtp_close,
            .pmix_mca_query_component = smtp_component_query,
            .pmix_mca_register_component_params = smtp_register,
        },
        .data = {
            /* The component is checkpoint ready */
            PMIX_MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    },
};

static pmix_status_t smtp_register(void)
{
    char version[256];

    /* Server stuff */
    mca_plog_smtp_component.server = strdup("localhost");
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "server",
                                           "SMTP server name or IP address",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.server);

    mca_plog_smtp_component.port = 25;
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "port",
                                           "SMTP server port",
                                           PMIX_MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.port);

    /* Email stuff */
    mca_plog_smtp_component.to = NULL;
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "to",
                                           "Comma-delimited list of email addresses to send to",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.to);
    mca_plog_smtp_component.from_addr = NULL;
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "from_addr",
                                           "Email address that messages will be from",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.from_addr);
    mca_plog_smtp_component.from_name = strdup("PMIx Plog");
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "from_name",
                                           "Email name that messages will be from",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.from_name);
    mca_plog_smtp_component.subject = strdup("PMIx Plog");
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "subject",
                                           "Email subject",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.subject);

    /* Mail body prefix and suffix */
    mca_plog_smtp_component.body_prefix = strdup("The PMIx SMTP plog wishes to inform you of the following message:\n\n");
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "body_prefix",
                                           "Text to put at the beginning of the mail message",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.body_prefix);
    mca_plog_smtp_component.body_suffix = strdup("\n\nSincerely,\nOscar the PMIx Owl");
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "body_prefix",
                                           "Text to put at the end of the mail message",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.body_suffix);

    /* Priority */
    mca_plog_smtp_component.priority = 10;
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "priority",
                                           "Priority of this component",
                                           PMIX_MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.priority);
    /* Libesmtp version */
    smtp_version(version, sizeof(version), 0);
    version[sizeof(version) - 1] = '\0';
    mca_plog_smtp_component.version = strdup(version);
    (void) pmix_mca_base_component_var_register(&mca_plog_smtp_component.super.base, "libesmtp_version",
                                           "Version of libesmtp that this component is linked against",
                                           PMIX_MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           PMIX_INFO_LVL_9,
                                           PMIX_MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_plog_smtp_component.version);

    return PMIX_SUCCESS;
}

static pmix_status_t smtp_close(void)
{
    return PMIX_SUCCESS;
}

static pmix_status_t smtp_component_query(pmix_mca_base_module_t **module,
                                          int *priority)
{
    *priority = 0;
    *module = NULL;

    /* If there's no to or from, there's no love */
    if (NULL == mca_plog_smtp_component.to ||
        '\0' == mca_plog_smtp_component.to[0] ||
        NULL == mca_plog_smtp_component.from_addr ||
        '\0' == mca_plog_smtp_component.from_addr[0]) {
        pmix_show_help("help-pmix-plog-smtp.txt",
                       "to/from not specified", true);
        return PMIX_ERR_NOT_FOUND;
    }

    /* Sanity checks */
    if (NULL == mca_plog_smtp_component.server ||
        '\0' == mca_plog_smtp_component.server[0]) {
        pmix_show_help("help-pmix-plog-smtp.txt",
                       "server not specified", true);
        return PMIX_ERR_NOT_FOUND;
    }

    /* Since we have to open a socket later, try to resolve the IP
       address of the server now.  Save the result, or abort if we
       can't resolve it. */
    mca_plog_smtp_component.server_hostent =
        gethostbyname(mca_plog_smtp_component.server);
    if (NULL == mca_plog_smtp_component.server_hostent) {
        pmix_show_help("help-pmix-plog-smtp.txt",
                       "unable to resolve server",
                       true, mca_plog_smtp_component.server);
        return PMIX_ERR_NOT_FOUND;
    }

    *priority = 10;
    *module = (pmix_mca_base_module_t *)&pmix_plog_smtp_module;
    return PMIX_SUCCESS;
}
