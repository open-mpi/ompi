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
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/

/*
 * Because we can. :-)
 */

#include "orte_config.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/constants.h"
#include "orte/util/show_help.h"

#include "notifier_twitter.h"

static int twitter_open(void);
static int twitter_component_query(mca_base_module_t **module, int *priority);
static int twitter_close(void);
static int twitter_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_notifier_twitter_component_t mca_notifier_twitter_component = {
    {
        {
            ORTE_NOTIFIER_BASE_VERSION_1_0_0,
            
            "twitter",
            
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            twitter_open,
            twitter_close,
            twitter_component_query,
            twitter_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },

    /* Twitter url, username, password */
    "http://twitter.com/statuses/update.json",
    NULL,
    NULL,
    NULL,
    NULL,

    /* Struct hostent */
    NULL,
    80,

    /* Priority */
    10,
};

static int twitter_register(void)
{
    mca_base_param_reg_string(&mca_notifier_twitter_component.super.base_version,
                              "url",
                              "Twitter update URL",
                              false, false,
                              mca_notifier_twitter_component.url,
                              &mca_notifier_twitter_component.url);
    mca_base_param_reg_string(&mca_notifier_twitter_component.super.base_version,
                              "username",
                              "Twitter username",
                              false, false, NULL, 
                              &mca_notifier_twitter_component.username);
    mca_base_param_reg_string(&mca_notifier_twitter_component.super.base_version,
                              "password",
                              "Twitter password",
                              false, false, NULL, 
                              &mca_notifier_twitter_component.password);

    mca_base_param_reg_int(&mca_notifier_twitter_component.super.base_version,
                           "priority",
                           "Priority of this component",
                           false, false, 
                           mca_notifier_twitter_component.priority,
                           &mca_notifier_twitter_component.priority);
    return ORTE_SUCCESS;
}

static int twitter_open(void)
{
    /* Nothing to do */
    return ORTE_SUCCESS;
}

static int twitter_close(void)
{
    if (NULL != mca_notifier_twitter_component.url) {
        free(mca_notifier_twitter_component.url);
    }
    if (NULL != mca_notifier_twitter_component.username) {
        free(mca_notifier_twitter_component.username);
    }
    if (NULL != mca_notifier_twitter_component.password) {
        free(mca_notifier_twitter_component.password);
    }

    return ORTE_SUCCESS;
}

static int twitter_component_query(mca_base_module_t **module, 
                                   int *priority)
{
    char *str;

    *priority = 10;
    *module = (mca_base_module_t *)&orte_notifier_twitter_module;

    /* If we have no username or password, there's no love */
    if (NULL == mca_notifier_twitter_component.username ||
        NULL == mca_notifier_twitter_component.password) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* Parse out the URL into a server and URI, ensuring that we can
       handle it */
    if (0 != strncmp(mca_notifier_twitter_component.url, "http://", 7)) {
        orte_show_help("help-orte-notifier-twitter.txt", "only supports http",
                       true, mca_notifier_twitter_component.url);
        return ORTE_ERR_NOT_SUPPORTED;
    }

    /* There's a / between the server name and URI */
    str = strchr(mca_notifier_twitter_component.url + 7, '/');
    if (NULL == str) {
        orte_show_help("help-orte-notifier-twitter.txt", 
                       "unable to parse URL",
                       true, mca_notifier_twitter_component.url);
        return ORTE_ERR_NOT_FOUND;
    }
    *str = '\0';
    mca_notifier_twitter_component.server = 
        mca_notifier_twitter_component.url + 7;
    mca_notifier_twitter_component.uri = str + 1;
    /* Sanity checks */
    if (NULL == mca_notifier_twitter_component.server ||
        NULL == mca_notifier_twitter_component.uri ||
        '\0' == mca_notifier_twitter_component.server[0] ||
        '\0' == mca_notifier_twitter_component.uri[0]) {
        orte_show_help("help-orte-notifier-twitter.txt", 
                       "unable to parse URL",
                       true, mca_notifier_twitter_component.url);
        return ORTE_ERR_NOT_FOUND;
    }

    /* See if there's a port number in the server name */
    str = strchr(mca_notifier_twitter_component.server, ':');
    if (NULL != str) {
        *str = '\0';
        mca_notifier_twitter_component.port = atoi(str + 1);
    } else {
        mca_notifier_twitter_component.port = 80;
    }

    /* Since we have to open a socket later, try to resolve the IP
       address of the server now.  Save the result, or abort if we
       can't resolve it. */
    mca_notifier_twitter_component.server_hostent =
        gethostbyname(mca_notifier_twitter_component.server);
    if (NULL == mca_notifier_twitter_component.server_hostent) {
        orte_show_help("help-orte-notifier-twitter.txt", 
                       "unable to resolve server",
                       true, mca_notifier_twitter_component.server);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;    
}
