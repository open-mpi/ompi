/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "psmx.h"

static int psmx_domain_close(fid_t fid)
{
	struct psmx_fid_domain *domain;
	int err;

	domain = container_of(fid, struct psmx_fid_domain, domain.fid);

	psmx_am_fini(domain);

	if (domain->ns_thread) {
		pthread_cancel(domain->ns_thread);
		pthread_join(domain->ns_thread, NULL);
	}

#if 0
	/* AM messages could arrive after MQ is finalized, causing segfault
	 * when trying to dereference the MQ pointer. There is no mechanism
	 * to properly shutdown AM. The workaround is to keep MQ valid.
	 */
	psm_mq_finalize(domain->psm_mq);
#endif

	err = psm_ep_close(domain->psm_ep, PSM_EP_CLOSE_GRACEFUL,
			   (int64_t) PSMX_TIME_OUT * 1000000000LL);
	if (err != PSM_OK)
		psm_ep_close(domain->psm_ep, PSM_EP_CLOSE_FORCE, 0);

	free(domain);

	return 0;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_domain_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
};

static struct fi_ops_domain psmx_domain_ops = {
	.size = sizeof(struct fi_ops_domain),
	.av_open = psmx_av_open,
	.cq_open = psmx_cq_open,
	.endpoint = psmx_ep_open,
	.cntr_open = psmx_cntr_open,
	.wait_open = psmx_wait_open,
	.poll_open = psmx_poll_open,
};

int psmx_domain_open(struct fid_fabric *fabric, struct fi_info *info,
		     struct fid_domain **domain, void *context)
{
	struct psmx_fid_domain *domain_priv;
	struct psm_ep_open_opts opts;
	psm_uuid_t uuid;
	int err = -ENOMEM;

	psmx_debug("%s\n", __func__);

	if (!info->domain_attr->name || strncmp(info->domain_attr->name, "psm", 3))
		return -EINVAL;

	psmx_query_mpi();

	domain_priv = (struct psmx_fid_domain *) calloc(1, sizeof *domain_priv);
	if (!domain_priv)
		goto err_out;

	domain_priv->domain.fid.fclass = FI_CLASS_DOMAIN;
	domain_priv->domain.fid.context = context;
	domain_priv->domain.fid.ops = &psmx_fi_ops;
	domain_priv->domain.ops = &psmx_domain_ops;
	domain_priv->domain.mr = &psmx_mr_ops;
	domain_priv->mode = info->mode;

	psm_ep_open_opts_get_defaults(&opts);

	psmx_get_uuid(uuid);
	err = psm_ep_open(uuid, &opts,
			  &domain_priv->psm_ep, &domain_priv->psm_epid);
	if (err != PSM_OK) {
		fprintf(stderr, "%s: psm_ep_open returns %d, errno=%d\n",
			__func__, err, errno);
		err = psmx_errno(err);
		goto err_out_free_domain;
	}

	err = psm_mq_init(domain_priv->psm_ep, PSM_MQ_ORDERMASK_ALL,
			  NULL, 0, &domain_priv->psm_mq);
	if (err != PSM_OK) {
		fprintf(stderr, "%s: psm_mq_init returns %d, errno=%d\n",
			__func__, err, errno);
		err = psmx_errno(err);
		goto err_out_close_ep;
	}

	domain_priv->ns_port = psmx_uuid_to_port(uuid);

	if (psmx_env.name_server)
		err = pthread_create(&domain_priv->ns_thread, NULL, psmx_name_server, (void *)domain_priv);
	else
		err = -1;

	if (err)
		domain_priv->ns_thread = 0;

	if (psmx_domain_enable_ep(domain_priv, NULL) < 0) {
		if (domain_priv->ns_thread) {
			pthread_cancel(domain_priv->ns_thread);
			pthread_join(domain_priv->ns_thread, NULL);
		}
		psm_mq_finalize(domain_priv->psm_mq);
		goto err_out_close_ep;
	}

	*domain = &domain_priv->domain;
	return 0;

err_out_close_ep:
	if (psm_ep_close(domain_priv->psm_ep, PSM_EP_CLOSE_GRACEFUL,
			 (int64_t) PSMX_TIME_OUT * 1000000000LL) != PSM_OK)
		psm_ep_close(domain_priv->psm_ep, PSM_EP_CLOSE_FORCE, 0);

err_out_free_domain:
	free(domain_priv);

err_out:
	return err;
}

int psmx_domain_check_features(struct psmx_fid_domain *domain, int ep_cap)
{
	if ((ep_cap & PSMX_CAPS) != ep_cap)
		return -EINVAL;

	if ((ep_cap & FI_TAGGED) && domain->tagged_ep)
		return -EBUSY;

	if ((ep_cap & FI_MSG) && domain->msg_ep)
		return -EBUSY;

	if ((ep_cap & FI_RMA) && domain->rma_ep)
		return -EBUSY;

	if ((ep_cap & FI_ATOMICS) && domain->atomics_ep)
		return -EBUSY;

	return 0;
}

int psmx_domain_enable_ep(struct psmx_fid_domain *domain, struct psmx_fid_ep *ep)
{
	uint64_t ep_cap = 0;

	if (ep)
		ep_cap = ep->caps;

	if (ep_cap & FI_MSG)
		domain->reserved_tag_bits |= PSMX_MSG_BIT;

	if (psmx_env.am_msg)
		domain->reserved_tag_bits &= ~PSMX_MSG_BIT;

	if ((ep_cap & FI_RMA) && psmx_env.tagged_rma)
		domain->reserved_tag_bits |= PSMX_RMA_BIT;

	if (((ep_cap & FI_RMA) || (ep_cap & FI_ATOMICS) || psmx_env.am_msg) &&
	    !domain->am_initialized) {
		int err = psmx_am_init(domain);
		if (err)
			return err;

		domain->am_initialized = 1;
	}

	if (ep_cap & FI_RMA)
		domain->rma_ep = ep;

	if (ep_cap & FI_ATOMICS)
		domain->atomics_ep = ep;

	if (ep_cap & FI_TAGGED)
		domain->tagged_ep = ep;

	if (ep_cap & FI_MSG)
		domain->msg_ep = ep;

	return 0;
}

void psmx_domain_disable_ep(struct psmx_fid_domain *domain, struct psmx_fid_ep *ep)
{
	if (!ep)
		return;

	if ((ep->caps & FI_RMA) && domain->rma_ep == ep)
		domain->rma_ep = NULL;

	if ((ep->caps & FI_ATOMICS) && domain->atomics_ep == ep)
		domain->atomics_ep = NULL;

	if ((ep->caps & FI_TAGGED) && domain->tagged_ep == ep)
		domain->tagged_ep = NULL;

	if ((ep->caps & FI_MSG) && domain->msg_ep == ep)
		domain->msg_ep = NULL;
}

