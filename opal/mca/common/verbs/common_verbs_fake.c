/*
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * The code in this file prevents spurious libibverbs warnings on
 * stderr about devices that it doesn't recognize.
 *
 * Specifically, Cisco usNIC devices are exposed through the Linux
 * InfiniBand kernel interface (i.e., they show up in
 * /sys/class/infiniband).  However, the userspace side of these
 * drivers is not exposed through libibverbs (i.e., there is no
 * libibverbs provider/plugin for usNIC).  Therefore, when
 * ibv_get_device_list() is invoked, libibverbs cannot find a plugin
 * for usnic devices.  This causes libibverbs to emit a spurious
 * warning message on stderr.
 *
 * To avoid these extra stderr warnings, we insert a fake usnic verbs
 * libibverbs provider that safely squelches these warnings.
 *
 * More specifically: the userspace side of usNIC is exposed through
 * libfabric; we don't need libibverbs warnings about not being able
 * to find a usnic driver.
 */

#include "opal_config.h"

#include <infiniband/verbs.h>
#ifdef HAVE_INFINIBAND_DRIVER_H
#include <infiniband/driver.h>
#endif

#include "common_verbs.h"

/***********************************************************************/

#define PCI_VENDOR_ID_CISCO (0x1137)

static struct ibv_context *fake_alloc_context(struct ibv_device *ibdev,
                                              int cmd_fd)
{
    /* Nothing to do here */
    return NULL;
}

static void fake_free_context(struct ibv_context *ibctx)
{
    /* Nothing to do here */
}

/* Put just enough in here to convince libibverbs that this is a valid
   device, and a little extra just in case someone looks at this
   struct in a debugger. */
static struct ibv_device fake_dev = {
    .ops = {
        .alloc_context = fake_alloc_context,
        .free_context = fake_free_context
    },
    .name = "fake ibv_device inserted by Open MPI for non-verbs devices"
};

static struct ibv_device *fake_driver_init(const char *uverbs_sys_path,
                                                 int abi_version)
{
    char value[8];
    int vendor;

    /* This function should only be invoked for
       /sys/class/infiniband/usnic_X devices, but double check just to
       be absolutely sure: read the vendor ID and ensure that it is
       Cisco. */
    if (ibv_read_sysfs_file(uverbs_sys_path, "device/vendor",
                            value, sizeof(value)) < 0) {
        return NULL;
    }
    sscanf(value, "%i", &vendor);

    if (vendor == PCI_VENDOR_ID_CISCO) {
        return &fake_dev;
    }

    /* We didn't find a device that we want to support */
    return NULL;
}


void opal_common_verbs_register_fake_drivers(void)
{
    /* Register a fake driver for "usnic_verbs" devices */
    ibv_register_driver("usnic_verbs", fake_driver_init);
}
