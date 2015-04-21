/*
 * Copyright (c) 2014 Intel Corp., Inc.  All rights reserved.
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
 *	copyright notice, this list of conditions and the following
 *	disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *	copyright notice, this list of conditions and the following
 *	disclaimer in the documentation and/or other materials
 *	provided with the distribution.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <rdma/fabric.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include "fi.h"

/* Print fi_info and related structs, enums, OR_able flags, addresses.
 * 
 * Each printable type should be mostly well formatted YAML.
 *
 * A struct is a dictionary containing one key named after the struct tag 
 * which contains a dictionary of member-value mappings.
 *
 * Enum values are currently just bare strings.
 * OR-able flags are a list of the values, ie: [ VAL1, VAL2 ]
 *
 * YAML does not contain tabs.
 * Indentation delineates lists and dictionaries (or they can be inline).
 */

#define TAB "    "

#define CASEENUMSTR(SYM) \
	case SYM: { strcatf(buf, #SYM); break; }
#define IFFLAGSTR(flags, SYM) \
	do { if (flags & SYM) strcatf(buf, #SYM ", "); } while(0)

static void fi_remove_comma(char *buffer)
{
	size_t sz = strlen(buffer);
	if (strcmp(&buffer[sz-2], ", ") == 0)
		buffer[sz-2] = '\0';
}

static void strcatf(char *dest, const char *fmt, ...)
{
	size_t len = strlen(dest);
	va_list arglist;

	va_start (arglist, fmt);
	vsnprintf(&dest[len], BUFSIZ - 1 - len, fmt, arglist);
	va_end (arglist);
}

static void fi_tostr_flags(char *buf, uint64_t flags)
{
	IFFLAGSTR(flags, FI_INJECT);
	IFFLAGSTR(flags, FI_MULTI_RECV);
	IFFLAGSTR(flags, FI_SOURCE);
	IFFLAGSTR(flags, FI_SYMMETRIC);

	IFFLAGSTR(flags, FI_READ);
	IFFLAGSTR(flags, FI_WRITE);
	IFFLAGSTR(flags, FI_RECV);
	IFFLAGSTR(flags, FI_SEND);
	IFFLAGSTR(flags, FI_REMOTE_READ);
	IFFLAGSTR(flags, FI_REMOTE_WRITE);

	IFFLAGSTR(flags, FI_REMOTE_CQ_DATA);
	IFFLAGSTR(flags, FI_EVENT);
	IFFLAGSTR(flags, FI_INJECT_COMPLETE);
	IFFLAGSTR(flags, FI_TRANSMIT_COMPLETE);
	IFFLAGSTR(flags, FI_CANCEL);
	IFFLAGSTR(flags, FI_MORE);
	IFFLAGSTR(flags, FI_PEEK);
	IFFLAGSTR(flags, FI_TRIGGER);

	fi_remove_comma(buf);
}

static void fi_tostr_addr_format(char *buf, uint32_t addr_format)
{
	switch (addr_format) {
	CASEENUMSTR(FI_FORMAT_UNSPEC);
	CASEENUMSTR(FI_SOCKADDR);
	CASEENUMSTR(FI_SOCKADDR_IN);
	CASEENUMSTR(FI_SOCKADDR_IN6);
	CASEENUMSTR(FI_SOCKADDR_IB);
	CASEENUMSTR(FI_ADDR_PSMX);
	default:
		if (addr_format & FI_PROV_SPECIFIC)
			strcatf(buf, "Provider specific");
		else
			strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_progress(char *buf, enum fi_progress progress)
{
	switch (progress) {
	CASEENUMSTR(FI_PROGRESS_UNSPEC);
	CASEENUMSTR(FI_PROGRESS_AUTO);
	CASEENUMSTR(FI_PROGRESS_MANUAL);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_threading(char *buf, enum fi_threading threading)
{
	switch (threading) {
	CASEENUMSTR(FI_THREAD_UNSPEC);
	CASEENUMSTR(FI_THREAD_SAFE);
	CASEENUMSTR(FI_THREAD_FID);
	CASEENUMSTR(FI_THREAD_DOMAIN);
	CASEENUMSTR(FI_THREAD_COMPLETION);
	CASEENUMSTR(FI_THREAD_ENDPOINT);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_order(char *buf, uint64_t flags)
{
	IFFLAGSTR(flags, FI_ORDER_RAR);
	IFFLAGSTR(flags, FI_ORDER_RAW);
	IFFLAGSTR(flags, FI_ORDER_RAS);
	IFFLAGSTR(flags, FI_ORDER_WAR);
	IFFLAGSTR(flags, FI_ORDER_WAW);
	IFFLAGSTR(flags, FI_ORDER_WAS);
	IFFLAGSTR(flags, FI_ORDER_SAR);
	IFFLAGSTR(flags, FI_ORDER_SAW);
	IFFLAGSTR(flags, FI_ORDER_SAS);

	fi_remove_comma(buf);
}

static void fi_tostr_caps(char *buf, uint64_t caps)
{
	IFFLAGSTR(caps, FI_MSG);
	IFFLAGSTR(caps, FI_RMA);
	IFFLAGSTR(caps, FI_TAGGED);
	IFFLAGSTR(caps, FI_ATOMICS);
	IFFLAGSTR(caps, FI_DYNAMIC_MR);
	fi_tostr_flags(buf, caps);

	fi_remove_comma(buf);
}

static void fi_tostr_ep_type(char *buf, enum fi_ep_type ep_type)
{
	switch (ep_type) {
	CASEENUMSTR(FI_EP_UNSPEC);
	CASEENUMSTR(FI_EP_MSG);
	CASEENUMSTR(FI_EP_DGRAM);
	CASEENUMSTR(FI_EP_RDM);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_protocol(char *buf, uint32_t protocol)
{
	switch (protocol) {
	CASEENUMSTR(FI_PROTO_UNSPEC);
	CASEENUMSTR(FI_PROTO_RDMA_CM_IB_RC);
	CASEENUMSTR(FI_PROTO_IWARP);
	CASEENUMSTR(FI_PROTO_IB_UD);
	CASEENUMSTR(FI_PROTO_PSMX);
	CASEENUMSTR(FI_PROTO_UDP);
	default:
		if (protocol & FI_PROV_SPECIFIC)
			strcatf(buf, "Provider specific");
		else
			strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_mode(char *buf, uint64_t mode)
{
	IFFLAGSTR(mode, FI_CONTEXT);
	IFFLAGSTR(mode, FI_LOCAL_MR);
	IFFLAGSTR(mode, FI_PROV_MR_ATTR);
	IFFLAGSTR(mode, FI_MSG_PREFIX);
	IFFLAGSTR(mode, FI_RX_CQ_DATA);

	fi_remove_comma(buf);
}

static void fi_tostr_addr(char *buf, uint32_t addr_format,
		void *addr)
{
	char *p;
	p = buf + strlen(buf);

	if (addr == NULL) {
		strcatf(p, "(null)");
		return;
	}

	switch (addr_format) {
	case FI_SOCKADDR:
		/* translate and recurse... */
		switch (((struct sockaddr *)addr)->sa_family) {
		case AF_INET:
			fi_tostr_addr(p, FI_SOCKADDR_IN, addr);
			break;
		case AF_INET6:
			fi_tostr_addr(p, FI_SOCKADDR_IN6, addr);
			break;
		default:
			fi_tostr_addr(p, FI_FORMAT_UNSPEC, addr);
			break;
		}
		break;
	case FI_SOCKADDR_IN:
		inet_ntop(AF_INET, &((struct sockaddr_in *)addr)->sin_addr,
			p, 64);
		break;
	case FI_SOCKADDR_IN6:
		inet_ntop(AF_INET6, &((struct sockaddr_in6 *)addr)->sin6_addr,
			p, 64);
		break;
	default:
		sprintf(p, "%p", addr);
		break;
	}
}

static void fi_tostr_tx_attr(char *buf, const struct fi_tx_attr *attr,
			     const char *prefix)
{
	if (!attr) {
		strcatf(buf, "%sfi_tx_attr: (null)\n", prefix);
		return;
	}

	strcatf(buf, "%sfi_tx_attr:\n", prefix);
	strcatf(buf, "%s%scaps: [ ", prefix, TAB);
	fi_tostr_caps(buf, attr->caps);
	strcatf(buf, " ]\n");

	strcatf(buf, "%s%sop_flags: [ ", prefix, TAB);
	fi_tostr_flags(buf, attr->op_flags);
	strcatf(buf, " ]\n");

	strcatf(buf, "%s%smsg_order: [ ", prefix, TAB);
	fi_tostr_order(buf, attr->msg_order);
	strcatf(buf, " ]\n");

	strcatf(buf, "%s%sinject_size: %zd\n", prefix, TAB, attr->inject_size);
	strcatf(buf, "%s%ssize: %zd\n", prefix, TAB, attr->size);
	strcatf(buf, "%s%siov_limit: %zd\n", prefix, TAB, attr->iov_limit);
}

static void fi_tostr_rx_attr(char *buf, const struct fi_rx_attr *attr,
			     const char *prefix)
{
	if (!attr) {
		strcatf(buf, "%sfi_rx_attr: (null)\n", prefix);
		return;
	}

	strcatf(buf, "%sfi_rx_attr:\n", prefix);
	strcatf(buf, "%s%scaps: [ ", prefix, TAB);
	fi_tostr_caps(buf, attr->caps);
	strcatf(buf, " ]\n");

	strcatf(buf, "%s%sop_flags: [ ", prefix, TAB);
	fi_tostr_flags(buf, attr->op_flags);
	strcatf(buf, " ]\n");

	strcatf(buf, "%s%smsg_order: [ ", prefix, TAB);
	fi_tostr_order(buf, attr->msg_order);
	strcatf(buf, " ]\n");

	strcatf(buf, "%s%stotal_buffered_recv: %zd\n", prefix, TAB, attr->total_buffered_recv);
	strcatf(buf, "%s%ssize: %zd\n", prefix, TAB, attr->size);
	strcatf(buf, "%s%siov_limit: %zd\n", prefix, TAB, attr->iov_limit);
}

static void fi_tostr_ep_attr(char *buf, const struct fi_ep_attr *attr, const char *prefix)
{
	if (!attr) {
		strcatf(buf, "%sfi_ep_attr: (null)\n", prefix);
		return;
	}

	strcatf(buf, "%sfi_ep_attr:\n", prefix);
	strcatf(buf, "%sep_type: ", TAB);
	fi_tostr_ep_type(buf, attr->type);
	strcatf(buf, "\n");
	strcatf(buf, "%s%sprotocol: ", prefix, TAB);
	fi_tostr_protocol(buf, attr->protocol);
	strcatf(buf, "\n");
	strcatf(buf, "%s%smax_msg_size: %zd\n", prefix, TAB, attr->max_msg_size);
	strcatf(buf, "%s%smax_order_raw_size: %zd\n", prefix, TAB, attr->max_order_raw_size);
	strcatf(buf, "%s%smax_order_war_size: %zd\n", prefix, TAB, attr->max_order_war_size);
	strcatf(buf, "%s%smax_order_waw_size: %zd\n", prefix, TAB, attr->max_order_waw_size);
	strcatf(buf, "%s%smem_tag_format: 0x%016llx\n", prefix, TAB, attr->mem_tag_format);

	strcatf(buf, "%s%stx_ctx_cnt: %zd\n", prefix, TAB, attr->tx_ctx_cnt);
	strcatf(buf, "%s%srx_ctx_cnt: %zd\n", prefix, TAB, attr->rx_ctx_cnt);
}

static void fi_tostr_resource_mgmt(char *buf, enum fi_resource_mgmt rm)
{
	switch (rm) {
	CASEENUMSTR(FI_RM_UNSPEC);
	CASEENUMSTR(FI_RM_DISABLED);
	CASEENUMSTR(FI_RM_ENABLED);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_av_type(char *buf, enum fi_av_type type)
{
	switch (type) {
	CASEENUMSTR(FI_AV_UNSPEC);
	CASEENUMSTR(FI_AV_MAP);
	CASEENUMSTR(FI_AV_TABLE);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_domain_attr(char *buf, const struct fi_domain_attr *attr,
				 const char *prefix)
{
	if (!attr) {
		strcatf(buf, "%sfi_domain_attr: (null)\n", prefix);
		return;
	}

	strcatf(buf, "%sfi_domain_attr:\n", prefix);
	strcatf(buf, "%s%sname: %s\n", prefix, TAB, attr->name);
	strcatf(buf, "%s%sthreading: ", prefix, TAB);
	fi_tostr_threading(buf, attr->threading);
	strcatf(buf, "\n");

	strcatf(buf, "%s%scontrol_progress: ", prefix,TAB);
	fi_tostr_progress(buf, attr->control_progress);
	strcatf(buf, "\n");
	strcatf(buf, "%s%sdata_progress: ", prefix, TAB);
	fi_tostr_progress(buf, attr->data_progress);
	strcatf(buf, "\n");
	strcatf(buf, "%s%sresouce_mgmt: ", prefix, TAB);
	fi_tostr_resource_mgmt(buf, attr->resource_mgmt);
	strcatf(buf, "\n");
	strcatf(buf, "%s%sav_type: ", prefix, TAB);
	fi_tostr_av_type(buf, attr->av_type);
	strcatf(buf, "\n");

	strcatf(buf, "%s%smr_key_size: %zd\n", prefix, TAB, attr->mr_key_size);
	strcatf(buf, "%s%scq_data_size: %zd\n", prefix, TAB, attr->cq_data_size);
	strcatf(buf, "%s%sep_cnt: %zd\n", prefix, TAB, attr->ep_cnt);
	strcatf(buf, "%s%stx_ctx_cnt: %zd\n", prefix, TAB, attr->tx_ctx_cnt);
	strcatf(buf, "%s%srx_ctx_cnt: %zd\n", prefix, TAB, attr->rx_ctx_cnt);
	strcatf(buf, "%s%smax_ep_tx_ctx: %zd\n", prefix, TAB, attr->max_ep_tx_ctx);
	strcatf(buf, "%s%smax_ep_rx_ctx: %zd\n", prefix, TAB, attr->max_ep_rx_ctx);
}

static void fi_tostr_fabric_attr(char *buf, const struct fi_fabric_attr *attr,
				 const char *prefix)
{
	if (!attr) {
		strcatf(buf, "%sfi_fabric_attr: (null)\n", prefix);
		return;
	}

	strcatf(buf, "%sfi_fabric_attr:\n", prefix);
	strcatf(buf, "%s%sname: %s\n", prefix, TAB, attr->name);
	strcatf(buf, "%s%sprov_name: %s\n", prefix, TAB, attr->prov_name);
	strcatf(buf, "%s%sprov_version: %d.%d\n", prefix, TAB,
		FI_MAJOR(attr->prov_version), FI_MINOR(attr->prov_version));
}

static void fi_tostr_info(char *buf, const struct fi_info *info)
{
	strcatf(buf, "fi_info:\n");
	strcatf(buf, "%scaps: [ ", TAB);
	fi_tostr_caps(buf, info->caps);
	strcatf(buf, " ]\n");

	strcatf(buf, "%smode: [ ", TAB);
	fi_tostr_mode(buf, info->mode);
	strcatf(buf, " ]\n");

	strcatf(buf, "%sfi_addr_format: ", TAB);
	fi_tostr_addr_format(buf, info->addr_format);
	strcatf(buf, "\n");

	strcatf(buf, "%ssrc_addrlen: %zd\n", TAB, info->src_addrlen);
	strcatf(buf, "%sdest_addrlen: %zd\n", TAB, info->dest_addrlen);
	strcatf(buf, "%ssrc_addr: ", TAB);
	fi_tostr_addr(buf, info->addr_format, info->src_addr);
	strcatf(buf, "\n");
	strcatf(buf, "%sdest_addr: ", TAB);
	fi_tostr_addr(buf, info->addr_format, info->dest_addr);
	strcatf(buf, "\n");
	strcatf(buf, "%sconnreq: %s\n", TAB, info->connreq);

	fi_tostr_tx_attr(buf, info->tx_attr, TAB);
	fi_tostr_rx_attr(buf, info->rx_attr, TAB);
	fi_tostr_ep_attr(buf, info->ep_attr, TAB);
	fi_tostr_domain_attr(buf, info->domain_attr, TAB);
	fi_tostr_fabric_attr(buf, info->fabric_attr, TAB);
}

static void fi_tostr_atomic_type(char *buf, enum fi_datatype type)
{
	switch (type) {
	CASEENUMSTR(FI_INT8);
	CASEENUMSTR(FI_UINT8);
	CASEENUMSTR(FI_INT16);
	CASEENUMSTR(FI_UINT16);
	CASEENUMSTR(FI_INT32);
	CASEENUMSTR(FI_UINT32);
	CASEENUMSTR(FI_INT64);
	CASEENUMSTR(FI_UINT64);
	CASEENUMSTR(FI_FLOAT);
	CASEENUMSTR(FI_DOUBLE);
	CASEENUMSTR(FI_FLOAT_COMPLEX);
	CASEENUMSTR(FI_DOUBLE_COMPLEX);
	CASEENUMSTR(FI_LONG_DOUBLE);
	CASEENUMSTR(FI_LONG_DOUBLE_COMPLEX);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_atomic_op(char *buf, enum fi_op op)
{
	switch (op) {
	CASEENUMSTR(FI_MIN);
	CASEENUMSTR(FI_MAX);
	CASEENUMSTR(FI_SUM);
	CASEENUMSTR(FI_PROD);
	CASEENUMSTR(FI_LOR);
	CASEENUMSTR(FI_LAND);
	CASEENUMSTR(FI_BOR);
	CASEENUMSTR(FI_BAND);
	CASEENUMSTR(FI_LXOR);
	CASEENUMSTR(FI_BXOR);
	CASEENUMSTR(FI_ATOMIC_READ);
	CASEENUMSTR(FI_ATOMIC_WRITE);
	CASEENUMSTR(FI_CSWAP);
	CASEENUMSTR(FI_CSWAP_NE);
	CASEENUMSTR(FI_CSWAP_LE);
	CASEENUMSTR(FI_CSWAP_LT);
	CASEENUMSTR(FI_CSWAP_GE);
	CASEENUMSTR(FI_CSWAP_GT);
	CASEENUMSTR(FI_MSWAP);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_version(char *buf)
{
	strcatf(buf, VERSION);
}

static void fi_tostr_eq_event(char *buf, int type)
{
	switch (type) {
	CASEENUMSTR(FI_NOTIFY);
	CASEENUMSTR(FI_CONNREQ);
	CASEENUMSTR(FI_CONNECTED);
	CASEENUMSTR(FI_SHUTDOWN);
	CASEENUMSTR(FI_MR_COMPLETE);
	CASEENUMSTR(FI_AV_COMPLETE);
	default:
		strcatf(buf, "Unknown");
		break;
	}
}

static void fi_tostr_cq_event_flags(char *buf, uint64_t flags)
{
	IFFLAGSTR(flags, FI_SEND);
	IFFLAGSTR(flags, FI_RECV);
	IFFLAGSTR(flags, FI_RMA);
	IFFLAGSTR(flags, FI_ATOMIC);
	IFFLAGSTR(flags, FI_MSG);
	IFFLAGSTR(flags, FI_TAGGED);
	IFFLAGSTR(flags, FI_READ);
	IFFLAGSTR(flags, FI_WRITE);
	IFFLAGSTR(flags, FI_REMOTE_READ);
	IFFLAGSTR(flags, FI_REMOTE_WRITE);
	IFFLAGSTR(flags, FI_REMOTE_CQ_DATA);
	IFFLAGSTR(flags, FI_MULTI_RECV);
	fi_remove_comma(buf);
}

__attribute__((visibility ("default")))
char *DEFAULT_SYMVER_PRE(fi_tostr)(const void *data, enum fi_type datatype)
{
	static char *buf = NULL;
	uint64_t val64;
	uint32_t val32;
	int enumval;

	if (!data)
		return NULL;

	val64 = *(const uint64_t *) data;
	val32 = *(const uint32_t *) data;
	enumval = *(const int *) data;

	if (!buf) {
		buf = calloc(BUFSIZ, 1);
		if (!buf)
			return NULL;
	}
	buf[0] = '\0';

	switch (datatype) {
	case FI_TYPE_INFO:
		fi_tostr_info(buf, data);
		break;
	case FI_TYPE_EP_TYPE:
		fi_tostr_ep_type(buf, enumval);
		break;
	case FI_TYPE_CAPS:
		fi_tostr_caps(buf, val64);
		break;
	case FI_TYPE_OP_FLAGS:
		fi_tostr_flags(buf, val64);
		break;
	case FI_TYPE_ADDR_FORMAT:
		fi_tostr_addr_format(buf, val32);
		break;
	case FI_TYPE_TX_ATTR:
		fi_tostr_tx_attr(buf, data, "");
		break;
	case FI_TYPE_RX_ATTR:
		fi_tostr_rx_attr(buf, data, "");
		break;
	case FI_TYPE_EP_ATTR:
		fi_tostr_ep_attr(buf, data, "");
		break;
	case FI_TYPE_DOMAIN_ATTR:
		fi_tostr_domain_attr(buf, data, "");
		break;
	case FI_TYPE_FABRIC_ATTR:
		fi_tostr_fabric_attr(buf, data, "");
		break;
	case FI_TYPE_THREADING:
		fi_tostr_threading(buf, enumval);
		break;
	case FI_TYPE_PROGRESS:
		fi_tostr_progress(buf, enumval);
		break;
	case FI_TYPE_PROTOCOL:
		fi_tostr_protocol(buf, val32);
		break;
	case FI_TYPE_MSG_ORDER:
		fi_tostr_order(buf, val64);
		break;
	case FI_TYPE_MODE:
		fi_tostr_mode(buf, val64);
		break;
	case FI_TYPE_AV_TYPE:
		fi_tostr_av_type(buf, enumval);
		break;
	case FI_TYPE_ATOMIC_TYPE:
		fi_tostr_atomic_type(buf, enumval);
		break;
	case FI_TYPE_ATOMIC_OP:
		fi_tostr_atomic_op(buf, enumval);
		break;
	case FI_TYPE_VERSION:
		fi_tostr_version(buf);
		break;
	case FI_TYPE_EQ_EVENT:
		fi_tostr_eq_event(buf, enumval);
		break;
	case FI_TYPE_CQ_EVENT_FLAGS:
		fi_tostr_cq_event_flags(buf, val64);
		break;
	default:
		strcatf(buf, "Unknown type");
		break;
	}
	return buf;
}
DEFAULT_SYMVER(fi_tostr_, fi_tostr);

#undef CASEENUMSTR
#undef IFFLAGSTR
