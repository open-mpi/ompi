/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/cma/smsc_cma_internal.h"

#include <fcntl.h>
#include <stdio.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

static int mca_smsc_cma_component_register(void);
static int mca_smsc_cma_component_open(void);
static int mca_smsc_cma_component_close(void);
static int mca_smsc_cma_component_query(void);
static mca_smsc_module_t *mca_smsc_cma_component_enable(void);

#define MCA_SMSC_CMA_DEFAULT_PRIORITY 37
static const int mca_smsc_cma_default_priority = MCA_SMSC_CMA_DEFAULT_PRIORITY;

mca_smsc_component_t mca_smsc_cma_component = {
  .smsc_version = {
    MCA_SMSC_DEFAULT_VERSION("cma"),
    .mca_open_component = mca_smsc_cma_component_open,
    .mca_close_component = mca_smsc_cma_component_close,
    .mca_register_component_params = mca_smsc_cma_component_register,
  },
  .priority = MCA_SMSC_CMA_DEFAULT_PRIORITY,
  .query = mca_smsc_cma_component_query,
  .enable = mca_smsc_cma_component_enable,
};

static int mca_smsc_cma_component_register(void)
{
    mca_smsc_base_register_default_params(&mca_smsc_cma_component, mca_smsc_cma_default_priority);
    return OPAL_SUCCESS;
}

static int mca_smsc_cma_component_open(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

static int mca_smsc_cma_component_close(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

/*
 * mca_btl_sm_parse_proc_ns_user() tries to get the user namespace ID
 * of the current process.
 * Returns the ID of the user namespace. In the case of an error '0' is returned.
 */
ino_t mca_smsc_cma_get_user_ns_id(void)
{
    struct stat buf;

    if (0 > stat("/proc/self/ns/user", &buf)) {
        /*
         * Something went wrong, probably an old kernel that does not support namespaces
         * simply assume all processes are in the same user namespace and return 0
         */
        return 0;
    }

    return buf.st_ino;
}

static int mca_smsc_cma_send_modex(void)
{
    mca_smsc_cma_modex_t modex;

    modex.pid = getpid();
    modex.user_ns_id = mca_smsc_cma_get_user_ns_id();

    int rc;
    OPAL_MODEX_SEND(rc, PMIX_LOCAL, &mca_smsc_cma_component.smsc_version, &modex, sizeof(modex));
    return rc;
}

static int mca_smsc_cma_component_query(void)
{
    /* Check if we have the proper permissions for CMA */
    char buffer = '0';
    bool cma_happy = false;

    /* check system setting for current ptrace scope */
    int fd = open("/proc/sys/kernel/yama/ptrace_scope", O_RDONLY);
    if (0 <= fd) {
        int ret = read(fd, &buffer, 1);
        if (ret < 0) {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                                opal_smsc_base_framework.framework_output,
                                "mca_smsc_cma_component_query: could not read ptrace_scope. "
                                "assuming ptrace scope is 0");
        }
        close(fd);
    }

    /* ptrace scope 0 will allow an attach from any of the process owner's
     * processes. ptrace scope 1 limits attachers to the process tree
     * starting at the parent of this process. */
    if ('0' != buffer) {
#if defined PR_SET_PTRACER
        /* try setting the ptrace scope to allow attach */
        int ret = prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
        if (0 == ret) {
            cma_happy = true;
        }
#endif
    } else {
        cma_happy = true;
    }

    if (!cma_happy) {
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
                            "mca_smsc_cma_component_query: could not select for use. insufficient "
                            "ptrace permissions.");
        mca_smsc_cma_component.priority = -1;
        return OPAL_ERR_NOT_AVAILABLE;
    }

    mca_smsc_cma_send_modex();

    return OPAL_SUCCESS;
}

static mca_smsc_module_t *mca_smsc_cma_component_enable(void)
{
    if (0 > mca_smsc_cma_component.priority) {
        return NULL;
    }

    return &mca_smsc_cma_module;
}
