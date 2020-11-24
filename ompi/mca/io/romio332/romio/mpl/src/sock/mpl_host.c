/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

#if (!defined MAXHOSTNAMELEN) && (!defined MAX_HOSTNAME_LEN)
#define MAX_HOSTNAME_LEN 256
#elif !defined MAX_HOSTNAME_LEN
#define MAX_HOSTNAME_LEN MAXHOSTNAMELEN
#endif

#define MAX_LOCAL_HOSTNAMES  (100)
static char lhost[MAX_LOCAL_HOSTNAMES][MAX_HOSTNAME_LEN];
static int lhost_count = 0;

static void append_lhost(const char *host)
{
    int i;

    for (i = 0; i < lhost_count; i++)
        if (!strcmp(lhost[i], host))
            return;

    MPL_strncpy(lhost[lhost_count], host, MAX_HOSTNAME_LEN);
    lhost_count++;
}

static void init_lhost_list(void)
{
    /* if the local host list is already initialized, return */
    if (lhost_count)
        return;

#if defined(MPL_HAVE_GETIFADDRS) && defined (MPL_HAVE_INET_NTOP)
    char tmp_lhost[MAX_HOSTNAME_LEN];
    int ret;
    MPL_sockaddr_t addr;
    struct ifaddrs *ifaddr, *ifa;
    char buf[MAX_HOSTNAME_LEN];

    if (gethostname(tmp_lhost, MAX_HOSTNAME_LEN) < 0) {
        /* we can't figure out our local hostname.  *sigh* */
        goto common_names;
    }
    append_lhost(tmp_lhost);

    /* we have our host name; try gethostbyname */
    ret = MPL_get_sockaddr(tmp_lhost, &addr);
    if (ret == 0) {
        /* Find the IP address of the host */
        ret = MPL_sockaddr_to_str(&addr, buf, MAX_HOSTNAME_LEN);
        if (ret == 0)
            append_lhost(buf);
    }

    /* try getifaddrs to see if we can get additional IPs */
    if (getifaddrs(&ifaddr) == -1)
        goto common_names;

    /* Find the IP addresses of all local interfaces */
    for (ifa = ifaddr; ifa; ifa = ifa->ifa_next) {
        /* FIXME: IPv4 only local addresses */
        if (ifa->ifa_addr && ifa->ifa_addr->sa_family == AF_INET) {
            ret = MPL_sockaddr_to_str((MPL_sockaddr_t *) ifa->ifa_addr, buf, MAX_HOSTNAME_LEN);
            if (ret == 0)
                append_lhost(buf);
        }
    }
    freeifaddrs(ifaddr);
#endif

  common_names:
    /* list the common localhost names */
    append_lhost("localhost");
    append_lhost("127.0.0.1");
    append_lhost("127.0.1.1");
}

int MPL_host_is_local(const char *host)
{
    int i;

    init_lhost_list();

    for (i = 0; i < lhost_count; i++)
        if (!strcmp(lhost[i], host))
            return 1;

    return 0;
}
