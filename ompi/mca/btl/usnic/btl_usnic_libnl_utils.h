/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef LIBNL_UTILS_H
#define LIBNL_UTILS_H

/* This header file and btl_usnic_libnl1_utils.c/btl_usnic_libnl3_utils.c are
 * here to support OS routing lookups.  They use the Linux "routing netlink"
 * communication subsystem (see "man 7 rtnetlink") via the "libnl" helper
 * library.  Unfortunately, libnl comes in two major versions: libnl (v1) and
 * libnl-3 with significant API differences between them.
 *
 * Quick glossary to some of the abbreviations here:
 *   rtnl -- rtnetlink (routing netlink)
 *   sk -- socket
 */

struct usnic_rtnl_sk;

/* returns zero on success or negative errno values on failure */
int ompi_btl_usnic_rtnl_sk_alloc(struct usnic_rtnl_sk **p_sk);

void ompi_btl_usnic_rtnl_sk_free(struct usnic_rtnl_sk* u_nlsk);

/* src_addr and dst_addr are IPv4 addresses in network byte order.  Returns
 * zero on successful route lookup, -1 otherwise. */
int ompi_btl_usnic_nl_ip_rt_lookup(struct usnic_rtnl_sk *unlsk,
                                   const char *src_ifname,
                                   uint32_t src_addr,
                                   uint32_t dst_addr, int *metric);

#endif /* LIBNL_UTILS_H */
