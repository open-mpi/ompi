/*
   IPv6 compatible layer to enable builds on SUSv2 machines lacking
   the new API

   This file is free software, no copyright at all
*/

/** @file **/

#ifndef OPAL_IPV6_COMPAT_H
#define OPAL_IPV6_COMPAT_H

#if (!OPAL_WANT_IPV6)

#ifndef sockaddr_storage
# define sockaddr_storage sockaddr
# define ss_family sa_family
#endif

#ifndef AF_UNSPEC
# define AF_UNSPEC AF_INET
#endif

#ifndef PF_UNSPEC
# define PF_UNSPEC PF_INET
#endif

#endif /* if (!OPAL_WANT_IPV6) */

#if 0
/* gently borrowed from bind9 */
#ifndef SIOCGLIFCONF
#define SIOCGLIFCONF SIOCGIFCONF
#define lifc_len ifc_len
#define lifc_buf ifc_buf
#define lifc_req ifc_req
#define lifconf ifconf
#endif

#ifndef SIOCGLIFADDR
#define SIOCGLIFADDR SIOCGIFADDR
#define SIOCGLIFFLAGS SIOCGIFFLAGS
#define SIOCGLIFDSTADDR SIOCGIFDSTADDR
#define SIOCGLIFNETMASK SIOCGIFNETMASK
#define lifr_addr ifr_addr
#define lifr_name ifr_name
#define lifr_dstaddr ifr_dstaddr
#define lifr_flags ifr_flags
#define lifreq ifreq
#define lifr_family sa_family
#else /* ifned SIOCGLIFADDR */
#define lifr_family ss_family
#endif

#endif /* 0 */

#endif /* OPAL_IPV6_OMPAT_H */
