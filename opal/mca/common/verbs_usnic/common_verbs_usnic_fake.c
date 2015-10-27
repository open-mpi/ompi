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
 *
 * Note: this code is statically linked into libopen-pal.  It is
 * registered via ibv_register_driver(), and there is no corresponding
 * *un*register IBV API.  Hence, we cannot allow this code to be
 * dlclosed (e.g., if it is a DSO or a dependent common library) -- it
 * must be in libopen-pal itself, which will stay resident in the MPI
 * application.
 */

#include "opal_config.h"

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <infiniband/verbs.h>
#ifdef HAVE_INFINIBAND_DRIVER_H
#include <infiniband/driver.h>
#endif

#include "common_verbs_usnic.h"

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
    if (sscanf(value, "%i", &vendor) != 1) {
        return NULL;
    }

    if (vendor == PCI_VENDOR_ID_CISCO) {
        return &fake_dev;
    }

    /* We didn't find a device that we want to support */
    return NULL;
}


void opal_common_verbs_usnic_register_fake_drivers(void)
{
    /* No need to do this more than once */
    static bool already_done = false;
    if (already_done) {
        return;
    }
    already_done = true;

    /* If there are any usnic devices, then register a fake driver */
    DIR *class_dir;
    class_dir = opendir("/sys/class/infiniband");
    if (NULL == class_dir) {
        return;
    }

    bool found = false;
    struct dirent *dent;
    while ((dent = readdir(class_dir)) != NULL) {
        if (strncmp(dent->d_name, "usnic_", 6) == 0) {
            found = true;
            break;
        }
    }
    closedir(class_dir);

    if (found) {
        ibv_register_driver("usnic_verbs", fake_driver_init);
    }
}
