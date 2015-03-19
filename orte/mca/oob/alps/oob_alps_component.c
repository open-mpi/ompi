/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will not conflict with other error codes that
 * are returned by these functions under UNIX/Linux environments 
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <ctype.h>

#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/argv.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/common/alps/common_alps.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

static int alps_component_open(void);
static int alps_component_close(void);
static int alps_component_register(void);
static bool component_available(void);
static int component_startup(void);
static void component_shutdown(void);
static int component_send(orte_rml_send_t *msg);
static char* component_get_addr(void);
static int component_set_addr(orte_process_name_t *peer, char **uris);
static bool component_is_reachable(orte_process_name_t *peer);
#if OPAL_ENABLE_FT_CR == 1
static int component_ft_event(int state);
#endif

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_base_component_t mca_oob_alps_component = {
        {
            MCA_OOB_BASE_VERSION_2_0_0,
            "alps", /* MCA module name */
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            alps_component_open,  /* component open */
            alps_component_close, /* component close */
            NULL, /* component query */
            alps_component_register, /* component register */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        0,   // reserve space for an assigned index
        30, // default priority of this transport
        component_available,
        component_startup,
        component_shutdown,
        component_send,
        component_get_addr,
        component_set_addr,
        component_is_reachable
#if OPAL_ENABLE_FT_CR == 1
        ,
        component_ft_event
#endif
};

/*
 * Initialize global variables used w/in this module.
 */
static int alps_component_open(void)
{
    return ORTE_SUCCESS;
}

static int alps_component_close(void)
{
    return ORTE_SUCCESS;
}

static int alps_component_register(void)
{
    return ORTE_SUCCESS;
}

static bool component_available(void)
{
    int rc;
    bool flag = false;

    /*
     * If I'm not a app proc can't use this component
     */

    if (!ORTE_PROC_IS_APP) {
        return false;
    }

    /*
     * If I have a orte daemon, then I don't want to use this component
     */

    if (NULL != orte_process_info.my_daemon_uri) {
        return false;
    }

    /*
     * make sure we're in a Cray PAGG container, and that we are also on
     * a compute node (i.e. we are thought of as a application task by
     * the cray job kernel module  - the thing that creates the PAGG
     */

    rc = orte_common_alps_proc_in_pagg(&flag);
    if ((ORTE_SUCCESS == rc) && (ORTE_PROC_IS_APP)) {
        return flag;
    }

    if (flag) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                            "oob:alps: component_available called");
    }

    return flag;
}

/* Start all modules */
static int component_startup(void)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s ALPS STARTUP",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    return ORTE_SUCCESS;
}

static void component_shutdown(void)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s ALPS SHUTDOWN",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
}

static int component_send(orte_rml_send_t *msg)
{
    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                        "%s oob:alps:send_nb to peer %s:%d this should not be happening",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&msg->dst), msg->tag);

    return ORTE_ERR_NOT_SUPPORTED;
}

static char* component_get_addr(void)
{
    int len;
    char hn[MAXHOSTNAMELEN], *cptr;

    /*
     * TODO: for aries want to plug in GNI addr here instead to
     * eventually be able to support connect/accept using aprun.
     */

    len = gethostname(hn, MAXHOSTNAMELEN - 1);
    hn[len]='\0';

    asprintf(&cptr, "gni://%s:%d", hn, getpid());

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                        "%s oob:alps: component_get_addr invoked - %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),cptr);
    return cptr;
}

static int component_set_addr(orte_process_name_t *peer,
                              char **uris)
{
    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                        "%s oob:alps: component_set_addr invoked - this should not be happening",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    return ORTE_ERR_NOT_SUPPORTED;
}

static bool component_is_reachable(orte_process_name_t *peer)
{
    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                        "%s oob:alps: component_set_addr invoked - this should not be happening",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    return false;
}

#if OPAL_ENABLE_FT_CR == 1
static int component_ft_event(int state)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s ALPS EVENT", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    return ORTE_ERR_NOT_SUPPORTED;
}
#endif
