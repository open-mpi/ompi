/*
 * Copyright (c) 2015, Cisco Systems, Inc. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
 * Since libfabric can have a verbs provider, libibverbs is invoked,
 * triggering the sequence described above, resulting in warning
 * messages about usnic devices.  To avoid these extra stderr
 * warnings, we insert a fake usnic verbs libibverbs provider that
 * safely squelches these warnings.
 *
 * More specifically: the userspace side of usNIC is exposed through
 * libfabric; we don't need libibverbs warnings about not being able
 * to find a usnic driver.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>

#include <infiniband/verbs.h>
#include <infiniband/driver.h>

/***********************************************************************/

#ifndef PCI_VENDOR_ID_CISCO
#define PCI_VENDOR_ID_CISCO	0x1137
#endif

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
    .name = "fake ibv_device inserted by libfabric:usNIC"
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


void usdf_setup_fake_ibv_provider(void)
{
    /* Register a fake driver for "usnic_verbs" devices */
    ibv_register_driver("usnic_verbs", fake_driver_init);
}
