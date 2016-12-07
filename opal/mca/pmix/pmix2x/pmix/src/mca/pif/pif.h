/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_MCA_PIF_PIF_H
#define PMIX_MCA_PIF_PIF_H

#include "pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NET_IF_H
#if defined(__APPLE__) && defined(_LP64)
/* Apple engineering suggested using options align=power as a
   workaround for a bug in OS X 10.4 (Tiger) that prevented ioctl(...,
   SIOCGIFCONF, ...) from working properly in 64 bit mode on Power PC.
   It turns out that the underlying issue is the size of struct
   ifconf, which the kernel expects to be 12 and natural 64 bit
   alignment would make 16.  The same bug appears in 64 bit mode on
   Intel macs, but align=power is a no-op there, so instead, use the
   pack pragma to instruct the compiler to pack on 4 byte words, which
   has the same effect as align=power for our needs and works on both
   Intel and Power PC Macs. */
#pragma pack(push,4)
#endif
#include <net/if.h>
#if defined(__APPLE__) && defined(_LP64)
#pragma pack(pop)
#endif
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "src/util/pif.h"
#include "src/mca/mca.h"
#include "src/mca/base/base.h"

BEGIN_C_DECLS

/*
 * Define INADDR_NONE if we don't have it.  Solaris is the only system
 * where I have found that it does not exist, and the man page for
 * inet_addr() says that it returns -1 upon failure.  On Linux and
 * other systems with INADDR_NONE, it's just a #define to -1 anyway.
 * So just #define it to -1 here if it doesn't already exist.
 */

#if !defined(INADDR_NONE)
#define INADDR_NONE -1
#endif

#define DEFAULT_NUMBER_INTERFACES 10
#define MAX_PIFCONF_SIZE 10 * 1024 * 1024


typedef struct pmix_pif_t {
    pmix_list_item_t     super;
    char                if_name[IF_NAMESIZE+1];
    int                 if_index;
    uint16_t            if_kernel_index;
    uint16_t            af_family;
    int                 if_flags;
    int                 if_speed;
    struct sockaddr_storage  if_addr;
    uint32_t            if_mask;
    uint32_t            if_bandwidth;
    uint8_t             if_mac[6];
    int                 ifmtu; /* Can't use if_mtu because of a
                                  #define collision on some BSDs */
} pmix_pif_t;
PMIX_CLASS_DECLARATION(pmix_pif_t);


/* "global" list of available interfaces */
extern pmix_list_t pmix_if_list;

/* global flags */
extern bool pmix_if_do_not_resolve;
extern bool pmix_if_retain_loopback;

/**
 * Structure for if components.
 */
struct pmix_pif_base_component_2_0_0_t {
    /** MCA base component */
    pmix_mca_base_component_t base;
    /** MCA base data */
    pmix_mca_base_component_data_t data;
};
/**
 * Convenience typedef
 */
typedef struct pmix_pif_base_component_2_0_0_t pmix_pif_base_component_t;

/*
 * Macro for use in components that are of type pif
 */
#define PMIX_PIF_BASE_VERSION_2_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("pif", 2, 0, 0)

END_C_DECLS

#endif /* PMIX_MCA_PIF_PIF_H */
