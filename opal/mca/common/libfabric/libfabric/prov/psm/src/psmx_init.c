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
#include "fi.h"
#include "prov.h"

struct psmx_env psmx_env;
volatile int psmx_init_count = 0;
struct psmx_fid_fabric *psmx_active_fabric = NULL;

static int psmx_reserve_tag_bits(int *caps, uint64_t *max_tag_value)
{
	int reserved_bits = 0;
	int ret_caps;
	int ask_caps = *caps;

	ret_caps = ask_caps ? ask_caps : PSMX_CAPS;

	if ((ret_caps & FI_MSG) && !psmx_env.am_msg) {
		if (*max_tag_value < PSMX_MSG_BIT) {
			reserved_bits |= PSMX_MSG_BIT;
		}
		else if (ask_caps) {
			PSMX_DEBUG("unable to reserve tag bit for FI_MSG support.\n"
				   "ADVICE: please reduce the asked max_tag_value, "
				   "or remove FI_MSG from the asked capabilities, "
				   "or set OFI_PSM_AM_MSG=1 to use an alternative (but less "
				   "optimized) message queue implementation.\n");
			return -1;
		}
		else {
			PSMX_DEBUG("unable to reserve tag bit for FI_MSG support. "
				   "FI_MSG is removed from the capabilities.\n"
				   "ADVICE: please reduce the asked max_tag_value, "
				   "or set OFI_PSM_AM_MSG=1 to use an alternative (but less "
				   "optimized) message queue implementation.\n");
			ret_caps &= ~FI_MSG;
		}
	}

	if ((ret_caps & FI_RMA) && psmx_env.tagged_rma) {
		if (*max_tag_value < PSMX_RMA_BIT) {
			reserved_bits |= PSMX_RMA_BIT;
		}
		else if (ask_caps) {
			PSMX_DEBUG("unable to reserve tag bit for tagged RMA acceleration.\n"
				   "ADVICE: please reduce the asked max_tag_value, "
				   "or remove FI_RMA from the asked capabilities, "
				   "or set OFI_PSM_TAGGED_RMA=0 to disable RMA acceleration.\n");
			return -1;
		}
		else {
			PSMX_DEBUG("unable to reserve tag bit for tagged RMA acceleration. "
				   "FI_RMA is removed from the capabilities.\n"
				   "ADVICE: please reduce the asked max_tag_value, "
				   "or set OFI_PSM_TAGGED_RMA=0 to disable RMA acceleration.\n");
			ret_caps &= ~FI_RMA;
		}
	}

	reserved_bits |= (reserved_bits << 1);

	*caps = ret_caps;
	*max_tag_value = ~reserved_bits;
	return 0;
}

static int psmx_getinfo(uint32_t version, const char *node, const char *service,
			uint64_t flags, struct fi_info *hints, struct fi_info **info)
{
	struct fi_info *psmx_info;
	uint32_t cnt = 0;
	void *dest_addr = NULL;
	int ep_type = FI_EP_RDM;
	int caps = 0;
	uint64_t max_tag_value = 0;
	int err = -FI_ENODATA;

	PSMX_DEBUG("\n");

	*info = NULL;

	if (psm_ep_num_devunits(&cnt) || !cnt) {
		PSMX_DEBUG("no PSM device is found.\n");
		return -FI_ENODATA;
	}

	if (node && !(flags & FI_SOURCE))
		dest_addr = psmx_resolve_name(node, 0);

	if (hints) {
		if (hints->ep_attr) {
			switch (hints->ep_attr->type) {
			case FI_EP_UNSPEC:
			case FI_EP_DGRAM:
			case FI_EP_RDM:
				break;
			default:
				PSMX_DEBUG("hints->ep_attr->type=%d, supported=%d,%d,%d.\n",
						hints->ep_attr->type, FI_EP_UNSPEC,
						FI_EP_DGRAM, FI_EP_RDM);
				goto err_out;
			}

			switch (hints->ep_attr->protocol) {
			case FI_PROTO_UNSPEC:
			case FI_PROTO_PSMX:
				break;
			default:
				PSMX_DEBUG("hints->protocol=%d, supported=%d %d\n",
						hints->ep_attr->protocol,
						FI_PROTO_UNSPEC, FI_PROTO_PSMX);
				goto err_out;
			}

			if (hints->ep_attr->tx_ctx_cnt > 1) {
				PSMX_DEBUG("hints->ep_attr->tx_ctx_cnt=%d, supported=0,1\n",
						hints->ep_attr->tx_ctx_cnt);
				goto err_out;
			}

			if (hints->ep_attr->rx_ctx_cnt > 1) {
				PSMX_DEBUG("hints->ep_attr->rx_ctx_cnt=%d, supported=0,1\n",
						hints->ep_attr->rx_ctx_cnt);
				goto err_out;
			}
		}

		if ((hints->caps & PSMX_CAPS) != hints->caps &&
		    (hints->caps & PSMX_CAPS2) != hints->caps) {
			PSMX_DEBUG("hints->caps=0x%llx, supported=0x%llx,0x%llx\n",
					hints->caps, PSMX_CAPS, PSMX_CAPS2);
			goto err_out;
		}

		if (hints->tx_attr) {
			if ((hints->tx_attr->op_flags & PSMX_OP_FLAGS) !=
			    hints->tx_attr->op_flags) {
				PSMX_DEBUG("hints->tx->flags=0x%llx, "
					   "supported=0x%llx\n",
					   hints->tx_attr->op_flags,
					   PSMX_OP_FLAGS);
				goto err_out;
			}
			if (hints->tx_attr->inject_size > PSMX_INJECT_SIZE) {
				PSMX_DEBUG("hints->tx_attr->inject_size=%ld,"
					   "supported=%ld.\n",
					   hints->tx_attr->inject_size,
					   PSMX_INJECT_SIZE);
				goto err_out;
			}
		}

		if (hints->rx_attr &&
		    (hints->rx_attr->op_flags & PSMX_OP_FLAGS) !=
		     hints->rx_attr->op_flags) {
			PSMX_DEBUG("hints->rx->flags=0x%llx, supported=0x%llx\n",
					hints->rx_attr->op_flags, PSMX_OP_FLAGS);
			goto err_out;
		}

		if ((hints->mode & PSMX_MODE) != PSMX_MODE) {
			PSMX_DEBUG("hints->mode=0x%llx, required=0x%llx\n",
					hints->mode, PSMX_MODE);
			goto err_out;
		}

		if (hints->fabric_attr && hints->fabric_attr->name &&
		    strncmp(hints->fabric_attr->name, "psm", 3)) {
			PSMX_DEBUG("hints->fabric_name=%s, supported=psm\n",
					hints->fabric_attr->name);
			goto err_out;
		}

		if (hints->domain_attr && hints->domain_attr->name &&
		    strncmp(hints->domain_attr->name, "psm", 3)) {
			PSMX_DEBUG("hints->domain_name=%s, supported=psm\n",
					hints->domain_attr->name);
			goto err_out;
		}

		if (hints->ep_attr) {
			if (hints->ep_attr->max_msg_size > PSMX_MAX_MSG_SIZE) {
				PSMX_DEBUG("hints->ep_attr->max_msg_size=%ld,"
						"supported=%ld.\n",
						hints->ep_attr->max_msg_size,
						PSMX_MAX_MSG_SIZE);
				goto err_out;
			}
			max_tag_value = fi_tag_bits(hints->ep_attr->mem_tag_format);
		}

		caps = hints->caps;

		/* TODO: check other fields of hints */
	}

	if (psmx_reserve_tag_bits(&caps, &max_tag_value) < 0)
		goto err_out;

	psmx_info = fi_allocinfo();
	if (!psmx_info) {
		err = -FI_ENOMEM;
		goto err_out;
	}

	psmx_info->ep_attr->type = ep_type;
	psmx_info->ep_attr->protocol = FI_PROTO_PSMX;
	psmx_info->ep_attr->max_msg_size = PSMX_MAX_MSG_SIZE;
	psmx_info->ep_attr->mem_tag_format = fi_tag_format(max_tag_value);
	psmx_info->ep_attr->msg_order = FI_ORDER_SAS;
	psmx_info->ep_attr->comp_order = FI_ORDER_NONE;
	psmx_info->ep_attr->tx_ctx_cnt = 1;
	psmx_info->ep_attr->rx_ctx_cnt = 1;

	psmx_info->domain_attr->threading = FI_THREAD_COMPLETION;
	psmx_info->domain_attr->control_progress = FI_PROGRESS_MANUAL;
	psmx_info->domain_attr->data_progress = FI_PROGRESS_MANUAL;
	psmx_info->domain_attr->name = strdup("psm");

	psmx_info->next = NULL;
	psmx_info->caps = (hints && hints->caps) ? hints->caps : caps;
	psmx_info->mode = PSMX_MODE;
	psmx_info->addr_format = FI_ADDR_PSMX;
	psmx_info->src_addrlen = 0;
	psmx_info->dest_addrlen = sizeof(psm_epid_t);
	psmx_info->src_addr = NULL;
	psmx_info->dest_addr = dest_addr;
	psmx_info->fabric_attr->name = strdup("psm");
	psmx_info->fabric_attr->prov_name = strdup("psm");

	psmx_info->tx_attr->caps = psmx_info->caps;
	psmx_info->tx_attr->mode = psmx_info->mode;
	psmx_info->tx_attr->op_flags = (hints && hints->tx_attr && hints->tx_attr->op_flags)
					? hints->tx_attr->op_flags : 0;
	psmx_info->tx_attr->msg_order = psmx_info->ep_attr->msg_order;
	psmx_info->tx_attr->comp_order = psmx_info->ep_attr->comp_order;
	psmx_info->tx_attr->inject_size = PSMX_INJECT_SIZE;
	psmx_info->tx_attr->size = UINT64_MAX;
	psmx_info->tx_attr->iov_limit = 1;

	psmx_info->rx_attr->caps = psmx_info->caps;
	psmx_info->rx_attr->mode = psmx_info->mode;
	psmx_info->rx_attr->op_flags = (hints && hints->rx_attr && hints->tx_attr->op_flags)
					? hints->tx_attr->op_flags : 0;
	psmx_info->rx_attr->msg_order = psmx_info->ep_attr->msg_order;
	psmx_info->rx_attr->comp_order = psmx_info->ep_attr->comp_order;
	psmx_info->rx_attr->total_buffered_recv = ~(0ULL); /* that's how PSM handles it internally! */
	psmx_info->rx_attr->size = UINT64_MAX;
	psmx_info->rx_attr->iov_limit = 1;

	*info = psmx_info;
	return 0;

err_out:
	return err;
}

static int psmx_fabric_close(fid_t fid)
{
	struct psmx_fid_fabric *fabric;

	PSMX_DEBUG("\n");

	fabric = container_of(fid, struct psmx_fid_fabric, fabric.fid);
	if (--fabric->refcnt) {
		if (fabric->active_domain)
			fi_close(&fabric->active_domain->domain.fid);
		assert(fabric == psmx_active_fabric);
		psmx_active_fabric = NULL;
		free(fid);
	}

	return 0;
}

static struct fi_ops psmx_fabric_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_fabric_close,
};

static struct fi_ops_fabric psmx_fabric_ops = {
	.size = sizeof(struct fi_ops_fabric),
	.domain = psmx_domain_open,
	.wait_open = psmx_wait_open,
};

static int psmx_fabric(struct fi_fabric_attr *attr,
		       struct fid_fabric **fabric, void *context)
{
	struct psmx_fid_fabric *fabric_priv;
	pthread_t thread;
	pthread_attr_t thread_attr;

	PSMX_DEBUG("\n");

	if (strncmp(attr->name, "psm", 3))
		return -FI_ENODATA;

	if (psmx_active_fabric) {
		psmx_active_fabric->refcnt++;
		*fabric = &psmx_active_fabric->fabric;
		return 0;
	}

	fabric_priv = calloc(1, sizeof(*fabric_priv));
	if (!fabric_priv)
		return -FI_ENOMEM;

	fabric_priv->fabric.fid.fclass = FI_CLASS_FABRIC;
	fabric_priv->fabric.fid.context = context;
	fabric_priv->fabric.fid.ops = &psmx_fabric_fi_ops;
	fabric_priv->fabric.ops = &psmx_fabric_ops;

	psmx_get_uuid(fabric_priv->uuid);

	if (psmx_env.name_server) {
		pthread_attr_init(&thread_attr);
		pthread_attr_setdetachstate(&thread_attr,PTHREAD_CREATE_DETACHED);
		pthread_create(&thread, &thread_attr, psmx_name_server, (void *)fabric_priv);
	}

	psmx_query_mpi();

	fabric_priv->refcnt = 1;
	*fabric = &fabric_priv->fabric;
	return 0;
}

static void psmx_fini(void)
{
	PSMX_DEBUG("\n");

	if (! --psmx_init_count)
		psm_finalize();
}

static struct fi_provider psmx_prov = {
	.name = PSMX_PROVNAME,
	.version = FI_VERSION(0, 9),
	.fi_version = FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION),
	.getinfo = psmx_getinfo,
	.fabric = psmx_fabric,
	.cleanup = psmx_fini
};

static int psmx_get_int_env(char *name, int default_value)
{
	char *s;

	s = getenv(name);
	if (s) {
		if (s[0]>='0' && s[0]<='9')
			return atoi(s);

		if (!strcasecmp(s, "yes") || !strcasecmp(s, "on"))
			return 1;

		if (!strcasecmp(s, "no") || !strcasecmp(s, "off"))
			return 0;
	}

	return default_value;
}

PSM_INI
{
	int major, minor;
	int check_version;
	int err;

	fi_log_init();

	psmx_env.name_server	= psmx_get_int_env("OFI_PSM_NAME_SERVER", 1);
	psmx_env.am_msg		= psmx_get_int_env("OFI_PSM_AM_MSG", 0);
	psmx_env.tagged_rma	= psmx_get_int_env("OFI_PSM_TAGGED_RMA", 0);
	psmx_env.warning	= psmx_get_int_env("OFI_PSM_WARNING", 1);
	psmx_env.uuid		= getenv("OFI_PSM_UUID");
	if (!psmx_env.uuid)
		psmx_env.uuid	= PSMX_DEFAULT_UUID;

	PSMX_DEBUG("\n");

        psm_error_register_handler(NULL, PSM_ERRHANDLER_NO_HANDLER);

	major = PSM_VERNO_MAJOR;
	minor = PSM_VERNO_MINOR;

        err = psm_init(&major, &minor);
	if (err != PSM_OK) {
		PSMX_WARN("%s: psm_init failed: %s\n", __func__,
			psm_error_get_string(err));
		return NULL;
	}

	PSMX_DEBUG("PSM header version = (%d, %d)\n", PSM_VERNO_MAJOR, PSM_VERNO_MINOR);
	PSMX_DEBUG("PSM library version = (%d, %d)\n", major, minor);

	check_version = psmx_get_int_env("OFI_PSM_VERSION_CHECK", 1);

	if (check_version && major != PSM_VERNO_MAJOR) {
		FI_WARN(PSMX_PROVNAME, "%s: PSM version mismatch: header %d.%d, library %d.%d.\n",
			__func__, PSM_VERNO_MAJOR, PSM_VERNO_MINOR, major, minor);
		FI_WARN(PSMX_PROVNAME, "\tSet envar OFI_PSM_VERSION_CHECK=0 to bypass version check.\n");
		return NULL;
	}

	PSMX_DEBUG("OFI_PSM_NAME_SERVER = %d\n", psmx_env.name_server);
	PSMX_DEBUG("OFI_PSM_AM_MSG = %d\n", psmx_env.am_msg);
	PSMX_DEBUG("OFI_PSM_TAGGED_RMA = %d\n", psmx_env.tagged_rma);
	PSMX_DEBUG("OFI_PSM_WARNING = %d\n", psmx_env.warning);
	PSMX_DEBUG("OFI_PSM_UUID = %s\n", psmx_env.uuid);

	psmx_init_count++;
	return (&psmx_prov);
}

