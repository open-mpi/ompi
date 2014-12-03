/*
 * Copyright (c) 2013 Intel Corporation. All rights reserved.
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

#define PSMX_MR_HASH_SIZE	103
#define PSMX_MR_HASH(x)		((x) % PSMX_MR_HASH_SIZE)

static struct psmx_mr_hash_entry {
	struct psmx_fid_mr *mr;
	struct psmx_mr_hash_entry *next;
} psmx_mr_hash[PSMX_MR_HASH_SIZE];

static int psmx_mr_hash_add(struct psmx_fid_mr *mr)
{
	int idx;
	struct psmx_mr_hash_entry *head;
	struct psmx_mr_hash_entry *entry;

	idx = PSMX_MR_HASH(mr->mr.key);
	head = &psmx_mr_hash[idx];
	if (!head->mr) {
		head->mr = mr;
		head->next = NULL;
		return 0;
	}

	entry = calloc(1, sizeof(*entry));
	if (!entry)
		return -ENOMEM;

	entry->mr = mr;
	entry->next = head->next;
	head->next = entry;
	return 0;
}

static int psmx_mr_hash_del(struct psmx_fid_mr *mr)
{
	int idx;
	struct psmx_mr_hash_entry *head;
	struct psmx_mr_hash_entry *entry;
	struct psmx_mr_hash_entry *prev;

	idx = PSMX_MR_HASH(mr->mr.key);
	head = &psmx_mr_hash[idx];

	if (head->mr == mr) {
		entry = head->next;
		if (entry) {
			head->mr = entry->mr;
			head->next = entry->next;
			free(entry);
		}
		else {
			head->mr = NULL;
		}
		return 0;
	}

	prev = head;
	entry = head->next;
	while (entry) {
		if (entry->mr == mr) {
			prev->next = entry->next;
			free(entry);
			return 0;
		}
		prev = entry;
		entry = entry->next;
	}

	return -ENOENT;
}

struct psmx_fid_mr *psmx_mr_hash_get(uint64_t key)
{
	int idx;
	struct psmx_mr_hash_entry *entry;

	idx = PSMX_MR_HASH(key);
	entry = &psmx_mr_hash[idx];

	while (entry) {
		if (entry->mr && entry->mr->mr.key == key)
			return entry->mr;
		entry = entry->next;
	}

	return NULL;
}

int psmx_mr_validate(struct psmx_fid_mr *mr, uint64_t addr, size_t len, uint64_t access)
{
	int i;

	addr += mr->offset;

	if (!addr)
		return -EINVAL;

	if ((access & mr->access) != access)
		return -EACCES;

	for (i = 0; i < mr->iov_count; i++) {
		if ((uint64_t)mr->iov[i].iov_base <= addr &&
		    (uint64_t)mr->iov[i].iov_base + mr->iov[i].iov_len >= addr + len)
			return 0;
	}

	return -EACCES;
}

static int psmx_mr_close(fid_t fid)
{
	struct psmx_fid_mr *mr;

	mr = container_of(fid, struct psmx_fid_mr, mr.fid);
	psmx_mr_hash_del(mr);
	free(mr);

	return 0;
}

static int psmx_mr_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct psmx_fid_mr *mr;
	struct psmx_fid_cq *cq;
	struct psmx_fid_ep *ep;
	struct psmx_fid_cntr *cntr;

	mr = container_of(fid, struct psmx_fid_mr, mr.fid);

	if (!bfid)
		return -EINVAL;
	switch (bfid->fclass) {
	case FI_CLASS_EP:
		ep = container_of(bfid, struct psmx_fid_ep, ep.fid);
		if (mr->domain != ep->domain)
			return -EINVAL;
		break;

	case FI_CLASS_CQ:
		cq = container_of(bfid, struct psmx_fid_cq, cq.fid);
		if (mr->cq && mr->cq != cq)
			return -EEXIST;
		if (mr->domain != cq->domain)
			return -EINVAL;
		mr->cq = cq;
		break;

	case FI_CLASS_CNTR:
		cntr = container_of(bfid, struct psmx_fid_cntr, cntr.fid);
		if (mr->cntr && mr->cntr != cntr)
			return -EEXIST;
		if (mr->domain != cntr->domain)
			return -EINVAL;
		mr->cntr = cntr;
		break;

	default:
		return -ENOSYS;
	}

	return 0;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_mr_close,
	.bind = psmx_mr_bind,
	.control = fi_no_control,
};

static void psmx_mr_normalize_iov(struct iovec *iov, size_t *count)
{
	struct iovec tmp_iov;
	int i, j, n, new_len;

	n = *count;

	if (!n)
		return;

	/* sort segments by base address */
	for (i = 0; i < n - 1; i++) {
		for (j = i + 1; j < n; j++) {
			if (iov[i].iov_base > iov[j].iov_base) {
				tmp_iov = iov[i];
				iov[i] = iov[j];
				iov[j] = tmp_iov;
			}
		}
	}

	/* merge overlapping segments */
	for (i = 0; i < n - 1; i++) {
		if (iov[i].iov_len == 0)
			continue;

		for (j = i + 1; j < n; j++) {
			if (iov[j].iov_len == 0)
				continue;

			if (iov[i].iov_base + iov[i].iov_len >= iov[j].iov_base) {
				new_len = iov[j].iov_base + iov[j].iov_len - iov[i].iov_base;
				if (new_len > iov[i].iov_len)
					iov[i].iov_len = new_len;
				iov[j].iov_len = 0;
			}
			else {
				break;
			}
		}
	}

	/* remove empty segments */
	for (i = 0, j = 1; i < n; i++, j++) {
		if (iov[i].iov_len)
			continue;

		while (j < n && iov[j].iov_len == 0)
			j++;

		if (j >= n)
			break;

		iov[i] = iov[j];
		iov[j].iov_len = 0;
	}

	*count = i;
}

static int psmx_mr_reg(struct fid_domain *domain, const void *buf, size_t len,
			uint64_t access, uint64_t offset, uint64_t requested_key,
			uint64_t flags, struct fid_mr **mr, void *context)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_mr *mr_priv;
	uint64_t key;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);
	if (!(domain_priv->mode & FI_PROV_MR_ATTR) && psmx_mr_hash_get(requested_key))
			return -FI_ENOKEY;

	mr_priv = (struct psmx_fid_mr *) calloc(1, sizeof(*mr_priv) + sizeof(struct iovec));
	if (!mr_priv)
		return -ENOMEM;

	mr_priv->mr.fid.fclass = FI_CLASS_MR;
	mr_priv->mr.fid.context = context;
	mr_priv->mr.fid.ops = &psmx_fi_ops;
	mr_priv->mr.mem_desc = mr_priv;
	if (!(domain_priv->mode & FI_PROV_MR_ATTR)) {
		key = requested_key;
	}
	else {
		key = (uint64_t)(uintptr_t)mr_priv;
		while (psmx_mr_hash_get(key))
			key++;
	}
	mr_priv->mr.key = key;
	mr_priv->domain = domain_priv;
	mr_priv->access = access;
	mr_priv->flags = flags;
	mr_priv->offset = (flags & FI_MR_OFFSET) ? offset : 0;
	mr_priv->iov_count = 1;
	mr_priv->iov[0].iov_base = (void *)buf;
	mr_priv->iov[0].iov_len = len;

	psmx_mr_hash_add(mr_priv);

	*mr = &mr_priv->mr;

	return 0;
}

static int psmx_mr_regv(struct fid_domain *domain,
			const struct iovec *iov, size_t count,
			uint64_t access, uint64_t offset, uint64_t requested_key,
			uint64_t flags, struct fid_mr **mr, void *context)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_mr *mr_priv;
	int i;
	uint64_t key;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);
	if (!(domain_priv->mode & FI_PROV_MR_ATTR) && psmx_mr_hash_get(requested_key))
			return -FI_ENOKEY;

	if (count == 0 || iov == NULL)
		return -EINVAL;

	mr_priv = (struct psmx_fid_mr *)
			calloc(1, sizeof(*mr_priv) +
				  sizeof(struct iovec) * count);
	if (!mr_priv)
		return -ENOMEM;

	mr_priv->mr.fid.fclass = FI_CLASS_MR;
	mr_priv->mr.fid.context = context;
	mr_priv->mr.fid.ops = &psmx_fi_ops;
	mr_priv->mr.mem_desc = mr_priv;
	if (!(domain_priv->mode & FI_PROV_MR_ATTR)) {
		key = requested_key;
	}
	else {
		key = (uint64_t)(uintptr_t)mr_priv;
		while (psmx_mr_hash_get(key))
			key++;
	}
	mr_priv->mr.key = key;
	mr_priv->domain = domain_priv;
	mr_priv->access = access;
	mr_priv->flags = flags;
	mr_priv->offset = (flags & FI_MR_OFFSET) ? offset : 0;
	mr_priv->iov_count = count;
	for (i=0; i<count; i++)
		mr_priv->iov[i] = iov[i];

	psmx_mr_normalize_iov(mr_priv->iov, &mr_priv->iov_count);
	psmx_mr_hash_add(mr_priv);

	*mr = &mr_priv->mr;

	return 0;
}

static int psmx_mr_regattr(struct fid_domain *domain, const struct fi_mr_attr *attr,
			uint64_t flags, struct fid_mr **mr)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_mr *mr_priv;
	int i;
	uint64_t key;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);
	if (!(domain_priv->mode & FI_PROV_MR_ATTR) && psmx_mr_hash_get(attr->requested_key))
			return -FI_ENOKEY;

	if (!attr)
		return -EINVAL;

	if (attr->iov_count == 0 || attr->mr_iov == NULL)
		return -EINVAL;

	mr_priv = (struct psmx_fid_mr *)
			calloc(1, sizeof(*mr_priv) +
				  sizeof(struct iovec) * attr->iov_count);
	if (!mr_priv)
		return -ENOMEM;

	mr_priv->mr.fid.fclass = FI_CLASS_MR;
	mr_priv->mr.fid.ops = &psmx_fi_ops;
	mr_priv->mr.mem_desc = mr_priv;
	if (!(domain_priv->mode & FI_PROV_MR_ATTR)) {
		key = attr->requested_key;
	}
	else {
		key = (uint64_t)(uintptr_t)mr_priv;
		while (psmx_mr_hash_get(key))
			key++;
	}
	mr_priv->mr.key = key;
	mr_priv->domain = domain_priv;
	mr_priv->access = FI_READ | FI_WRITE | FI_REMOTE_READ | FI_REMOTE_WRITE;
	mr_priv->flags = flags;
	mr_priv->offset = (flags & FI_MR_OFFSET) ? attr->offset : 0;
	mr_priv->iov_count = attr->iov_count;
	for (i=0; i<attr->iov_count; i++)
		mr_priv->iov[i] = attr->mr_iov[i];

	mr_priv->mr.fid.context = attr->context;
	mr_priv->access = attr->access;

	psmx_mr_normalize_iov(mr_priv->iov, &mr_priv->iov_count);
	psmx_mr_hash_add(mr_priv);

	*mr = &mr_priv->mr;

	return 0;
}

struct fi_ops_mr psmx_mr_ops = {
	.size = sizeof(struct fi_ops_mr),
	.reg = psmx_mr_reg,
	.regv = psmx_mr_regv,
	.regattr = psmx_mr_regattr,
};

