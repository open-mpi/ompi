/*
   IPv6 compatible layer to enable builds on SUSv2 machines lacking
   the new API

   This file is free software, no copyright at all
*/

/** @file **/

#ifndef OPAL_IPV6_COMPAT_H
#define OPAL_IPV6_COMPAT_H

#if (!OPAL_WANT_IPV6)

/* RFC2553 propose sockaddr_storage to cope with multiple address families */
#ifndef sockaddr_storage
#  define sockaddr_storage sockaddr
#  define ss_family sa_family
#endif

#ifndef AF_UNSPEC
#  define AF_UNSPEC AF_INET
#endif

#ifndef PF_UNSPEC
#  define PF_UNSPEC PF_INET
#endif

#endif /* if (!OPAL_WANT_IPV6) */

#endif /* OPAL_IPV6_OMPAT_H */
