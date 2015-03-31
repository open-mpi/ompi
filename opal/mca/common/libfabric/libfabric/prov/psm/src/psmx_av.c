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

static void psmx_set_epaddr_context(struct psmx_fid_domain *domain,
				    psm_epid_t epid, psm_epaddr_t epaddr)
{
	struct psmx_epaddr_context *context;

	context = (void *)psm_epaddr_getctxt(epaddr);
	if (context) {
		if (context->domain != domain || context->epid != epid) {
			FI_WARN(&psmx_prov, FI_LOG_AV,
				"domain or epid doesn't match\n");
			context = NULL;
		}
	}

	if (context)
		return;

	context = malloc(sizeof *context);
	if (!context) {
		FI_WARN(&psmx_prov, FI_LOG_AV,
			"cannot allocate context\n");
		return;
	}

	context->domain = domain;
	context->epid = epid;
	psm_epaddr_setctxt(epaddr, context);
}

int psmx_epid_to_epaddr(struct psmx_fid_domain *domain,
			psm_epid_t epid, psm_epaddr_t *epaddr)
{
        int err;
        psm_error_t errors;
	psm_epconn_t epconn;
	struct psmx_epaddr_context *context;

	err = psm_ep_epid_lookup(epid, &epconn);
	if (err == PSM_OK) {
		context = psm_epaddr_getctxt(epconn.addr);
		if (context && context->epid  == epid) {
			*epaddr = epconn.addr;
			return 0;
		}
	}

        err = psm_ep_connect(domain->psm_ep, 1, &epid, NULL, &errors, epaddr, 30*1e9);
        if (err != PSM_OK)
                return psmx_errno(err);

	psmx_set_epaddr_context(domain,epid,*epaddr);

        return 0;
}

static int psmx_av_check_table_size(struct psmx_fid_av *av, size_t count)
{
	size_t new_count;
	psm_epid_t *new_psm_epids;
	psm_epaddr_t *new_psm_epaddrs;

	new_count = av->count;
	while (new_count < av->last + count)
		new_count = new_count * 2 + 1;

	if ((new_count <= av->count) && av->psm_epids)
		return 0;

	new_psm_epids = realloc(av->psm_epids, new_count * sizeof(*new_psm_epids));
	if (!new_psm_epids)
		return -FI_ENOMEM;

	av->psm_epids = new_psm_epids;

	new_psm_epaddrs = realloc(av->psm_epaddrs, new_count * sizeof(*new_psm_epaddrs));
	if (!new_psm_epaddrs)
		return -FI_ENOMEM;

	av->psm_epaddrs = new_psm_epaddrs;
	av->count = new_count;
	return 0;
}

static int psmx_av_insert(struct fid_av *av, const void *addr, size_t count,
			  fi_addr_t *fi_addr, uint64_t flags, void *context)
{
	struct psmx_fid_av *av_priv;
	psm_error_t *errors;
	int error_count = 0;
	int *mask;
	int i, j;
	fi_addr_t *result = NULL;
	struct psmx_epaddr_context *epaddr_context;

	av_priv = container_of(av, struct psmx_fid_av, av);

	errors = (psm_error_t *) calloc(count, sizeof *errors);
	if (!errors)
		return -FI_ENOMEM;

	mask = (int *) calloc(count, sizeof *mask);
	if (!mask) {
		free(errors);
		return -FI_ENOMEM;
	}

	if (av_priv->type == FI_AV_TABLE) {
		if (psmx_av_check_table_size(av_priv, count)) {
			free(mask);
			free(errors);
			return -FI_ENOMEM;
		}

		for (i=0; i<count; i++)
			av_priv->psm_epids[av_priv->last + i] = ((psm_epid_t *)addr)[i];

		result = fi_addr;
		addr = (const void *)(av_priv->psm_epids + av_priv->last);
		fi_addr = (fi_addr_t *)(av_priv->psm_epaddrs + av_priv->last);
	}

	/* prevent connecting to the same ep twice, which is fatal in PSM */
	for (i=0; i<count; i++) {
		psm_epconn_t epconn;
		if (psm_ep_epid_lookup(((psm_epid_t *) addr)[i], &epconn) == PSM_OK) {
			epaddr_context = psm_epaddr_getctxt(epconn.addr);
			if (epaddr_context && epaddr_context->epid  == ((psm_epid_t *) addr)[i])
				((psm_epaddr_t *) fi_addr)[i] = epconn.addr;
			else
				mask[i] = 1;
		}
		else {
			mask[i] = 1;
		}
	}

	psm_ep_connect(av_priv->domain->psm_ep, count, 
			(psm_epid_t *) addr, mask, errors,
			(psm_epaddr_t *) fi_addr, 30*1e9);

	for (i=0; i<count; i++){
		if (!mask[i])
			continue;

		if (errors[i] == PSM_OK || errors[i] == PSM_EPID_ALREADY_CONNECTED) {
			psmx_set_epaddr_context(av_priv->domain,
						((psm_epid_t *) addr)[i],
						((psm_epaddr_t *) fi_addr)[i]);
		}
		else {
			FI_INFO(&psmx_prov, FI_LOG_AV,
				"%d: psm_ep_connect returned %s. remote epid=%lx.\n",
				i, psm_error_get_string(errors[i]),
				((psm_epid_t *)addr)[i]);
			if (((psm_epid_t *)addr)[i] == 0)
				FI_INFO(&psmx_prov, FI_LOG_AV,
					"does the application depend on the provider"
					"to resolve IP address into endpoint id? if so"
					"check if the name server has started correctly"
					"at the other side.\n");
			fi_addr[i] = FI_ADDR_NOTAVAIL;
			error_count++;
		}
	}

	free(mask);
	free(errors);

	if (av_priv->type == FI_AV_TABLE) {
		/* NOTE: unresolved addresses are left in the AV table */
		if (result) {
			for (i=0; i<count; i++) {
				j = av_priv->last + i;
				if ((fi_addr_t)av_priv->psm_epaddrs[j] == FI_ADDR_NOTAVAIL)
					result[i] = FI_ADDR_NOTAVAIL;
				else
					result[i] = j;
			}
		}
		av_priv->last += count;
	}

	return count - error_count;
}

static int psmx_av_remove(struct fid_av *av, fi_addr_t *fi_addr, size_t count,
			  uint64_t flags)
{
	int err = PSM_OK;

	return psmx_errno(err);
}

static int psmx_av_lookup(struct fid_av *av, fi_addr_t fi_addr, void *addr,
			  size_t *addrlen)
{
	struct psmx_fid_av *av_priv;
	struct psmx_epaddr_context *context;
	psm_epid_t epid;
	int idx;

	if (!addr || !addrlen)
		return -FI_EINVAL;

	av_priv = container_of(av, struct psmx_fid_av, av);

	if (av_priv->type == FI_AV_TABLE) {
		idx = (int)(int64_t)fi_addr;
		if (idx >= av_priv->last)
			return -FI_EINVAL;

		epid = av_priv->psm_epids[idx];
	}
	else {
		context = psm_epaddr_getctxt((void *)fi_addr);
		epid = context->epid;
	}

	if (*addrlen >= sizeof(epid))
		*(psm_epid_t *)addr = epid;
	else
		memcpy(addr, &epid, *addrlen);
	*addrlen = sizeof(epid);

	return 0;
}

static const char *psmx_av_straddr(struct fid_av *av, const void *addr,
				   char *buf, size_t *len)
{
	int n;

	if (!buf || !len)
		return NULL;

	n = snprintf(buf, *len, "%lx", (uint64_t)(uintptr_t)addr);
	if (n < 0)
		return NULL;

	*len = n + 1;
	return buf;
}

static int psmx_av_close(fid_t fid)
{
	struct psmx_fid_av *av;
	av = container_of(fid, struct psmx_fid_av, av.fid);
	if (av->psm_epids)
		free(av->psm_epids);
	if (av->psm_epaddrs)
		free(av->psm_epaddrs);
	free(av);
	return 0;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_av_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_av psmx_av_ops = {
	.size = sizeof(struct fi_ops_av),
	.insert = psmx_av_insert,
	.insertsvc = fi_no_av_insertsvc,
	.insertsym = fi_no_av_insertsym,
	.remove = psmx_av_remove,
	.lookup = psmx_av_lookup,
	.straddr = psmx_av_straddr,
};

int psmx_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		 struct fid_av **av, void *context)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_av *av_priv;
	int type = FI_AV_MAP;
	size_t count = 64;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);

	if (attr) {
		switch (attr->type) {
		case FI_AV_MAP:
		case FI_AV_TABLE:
			type = attr->type;
			break;
		default:
			FI_INFO(&psmx_prov, FI_LOG_AV,
				"attr->type=%d, supported=%d %d\n",
				attr->type, FI_AV_MAP, FI_AV_TABLE);
			return -FI_EINVAL;
		}

		count = attr->count;
	}

	av_priv = (struct psmx_fid_av *) calloc(1, sizeof *av_priv);
	if (!av_priv)
		return -FI_ENOMEM;

	av_priv->domain = domain_priv;
	av_priv->type = type;
	av_priv->addrlen = sizeof(psm_epaddr_t);
	av_priv->count = count;

	av_priv->av.fid.fclass = FI_CLASS_AV;
	av_priv->av.fid.context = context;
	av_priv->av.fid.ops = &psmx_fi_ops;
	av_priv->av.ops = &psmx_av_ops;

	*av = &av_priv->av;
	return 0;
}

