/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
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
 *
 * LICENSE_END
 *
 *
 */

#include <stdint.h>
#include <errno.h>

#include "kcompat.h"
#include "vnic_dev.h"
#include "vnic_enet.h"

#include "usd.h"
#include "usd_util.h"
#include "usd_vnic.h"

#define GET_CONFIG(m) \
        do { \
                ret = vnic_dev_spec(qp->uq_vf->vf_vdev, \
                        usd_offset_of(struct vnic_enet_config, m), \
                        sizeof(c->m), &c->m); \
                if (ret) { \
                    printf("Error %d getting " #m "\n", ret); \
                } else { \
                    printf(#m " = 0x%lx\n", (u64)c->m); \
                } \
        } while (0)


#if 0
/*
 * Dump the devspec (for debugging)
 */
int
usd_dump_devspec(
    struct usd_qp_impl *qp)
{
    struct vnic_enet_config config;
    struct vnic_enet_config *c;
    int ret;

    c = &config;
    memset(&config, 0, sizeof(config));

    GET_CONFIG(flags);
    GET_CONFIG(wq_desc_count);
    GET_CONFIG(rq_desc_count);
    GET_CONFIG(mtu);
    GET_CONFIG(intr_timer_deprecated);
    GET_CONFIG(intr_timer_type);
    GET_CONFIG(intr_mode);
    GET_CONFIG(intr_timer_usec);
    GET_CONFIG(loop_tag);
    GET_CONFIG(vf_rq_count);
    GET_CONFIG(num_arfs);
    GET_CONFIG(mem_paddr);

    ret = vnic_dev_spec(qp->uq_vf->vf_vdev,
            usd_offset_of(struct vnic_enet_config, devname),
            8, &c->devname[0]);
    ret |= vnic_dev_spec(qp->uq_vf->vf_vdev,
            usd_offset_of(struct vnic_enet_config, devname) + 8,
            8, &c->devname[8]);
    printf("devname = \"%s\", ret = %d\n", c->devname, ret);

    return 0;
}
#endif

/*
 * Get some QP settings from devspec
 */
int
usd_get_devspec(
    struct usd_qp_impl *qp)
{
    struct vnic_enet_config config;
    unsigned int offset;
    int ret;

    offset = usd_offset_of(struct vnic_enet_config, mem_paddr);
    ret = vnic_dev_spec(qp->uq_vf->vf_vdev, offset,
            sizeof(config.mem_paddr), &config.mem_paddr);
    if (ret != 0) {
        return ret;
    }

    qp->uq_attrs.uqa_pio_paddr = config.mem_paddr;

    return 0;
}

/*
 * Issue HANG_NOTIFY to the VNIC
 */
int
usd_vnic_hang_notify(
    struct usd_qp *uqp)
{
    struct usd_qp_impl *qp;
    u64 a0;
    int ret;

    qp = to_qpi(uqp);
    ret = vnic_dev_cmd(qp->uq_vf->vf_vdev, CMD_HANG_NOTIFY,
            &a0, &a0, 1000);
    if (ret != 0) {
        fprintf(stderr, "hang_notify ret = %d\n", ret);
        return ret;
    }

    return 0;
}
