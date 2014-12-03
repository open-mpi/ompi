/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
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

#if HAVE_CONFIG_H
# include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"
#include "fi_enosys.h"

#include "usnic_direct.h"
#include "usdf.h"

static int
usdf_domain_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
        struct usdf_domain *udp;

        udp = dom_fidtou(fid);

        switch (bfid->fclass) {
        case FI_CLASS_EQ:
                if (udp->dom_eq != NULL) {
                        return -FI_EINVAL;
                }
                udp->dom_eq = eq_fidtou(bfid);
                atomic_inc(&udp->dom_eq->eq_refcnt);
                break;
        default:
                return -FI_EINVAL;
        }

        return 0;
}

static int
usdf_domain_close(fid_t fid)
{
	struct usdf_domain *udp;
	int ret;

	udp = container_of(fid, struct usdf_domain, dom_fid.fid);
	if (atomic_get(&udp->dom_refcnt) > 0) {
		return -FI_EBUSY;
	}

	if (udp->dom_dev != NULL) {
		ret = usd_close(udp->dom_dev);
		if (ret != 0) {
			return ret;
		}
	}

	if (udp->dom_eq != NULL) {
		atomic_dec(&udp->dom_eq->eq_refcnt);
	}
	atomic_dec(&udp->dom_fabric->fab_refcnt);
	free(udp);

	return 0;
}

static struct fi_ops usdf_fid_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_domain_close,
	.bind = usdf_domain_bind,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_mr usdf_domain_mr_ops = {
	.size = sizeof(struct fi_ops_mr),
	.reg = usdf_reg_mr,
};

static struct fi_ops_domain usdf_domain_ops = {
	.size = sizeof(struct fi_ops_domain),
	.cq_open = usdf_cq_open,
	.av_open = usdf_av_open,
	.endpoint = usdf_endpoint_open,
};

int
usdf_domain_open(struct fid_fabric *fabric, struct fi_info *info,
	   struct fid_domain **domain, void *context)
{
	struct usdf_fabric *fp;
	struct usdf_domain *udp;
	struct usdf_usnic_info *dp;
	struct usdf_dev_entry *dep;
	int d;
	int ret;

	udp = calloc(1, sizeof *udp);
	if (udp == NULL) {
		ret = -FI_ENOMEM;
		goto fail;
	}

	fp = fab_fidtou(fabric);

	/* steal cached device from info if we can */
	dp = __usdf_devinfo;
	for (d = 0; d < dp->uu_num_devs; ++d) {
		dep = &dp->uu_info[d];
		if (dep->ue_dev != NULL &&
		    strcmp(fp->fab_dev_attrs->uda_devname,
			    dep->ue_dattr.uda_devname) == 0) {
			udp->dom_dev = dep->ue_dev;
			dep->ue_dev = NULL;
			break;
		}
	}

	if (udp->dom_dev == NULL) {
		ret = usd_open(fp->fab_dev_attrs->uda_devname, &udp->dom_dev);
		if (ret != 0) {
			goto fail;
		}
	}

	udp->dom_fid.fid.fclass = FI_CLASS_DOMAIN;
	udp->dom_fid.fid.context = context;
	udp->dom_fid.fid.ops = &usdf_fid_ops;
	udp->dom_fid.ops = &usdf_domain_ops;
	udp->dom_fid.mr = &usdf_domain_mr_ops;

	udp->dom_fabric = fp;
	atomic_init(&udp->dom_refcnt, 0);
	atomic_inc(&fp->fab_refcnt);

	*domain = &udp->dom_fid;
	return 0;

fail:
	if (udp != NULL) {
		free(udp);
	}
	return ret;
}
