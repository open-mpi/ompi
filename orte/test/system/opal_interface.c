/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/util/if.h"
#include "opal/runtime/opal.h"

int main(int argc, char* argv[])
{
    int rc, idx;
    uint32_t addr, netmask, netaddr;
    struct sockaddr_in inaddr;

    if (0 > (rc = opal_init(&argc, &argv))) {
        fprintf(stderr, "orte_interface: couldn't init opal - error code %d\n", rc);
        return rc;
    }
    
    rc = opal_iftupletoaddr(argv[1], &netaddr, &netmask);
    
    fprintf(stderr, "netaddr %03d.%03d.%03d.%03d netmask %03d.%03d.%03d.%03d rc %d\n",
            OPAL_IF_FORMAT_ADDR(netaddr), OPAL_IF_FORMAT_ADDR(netmask), rc);
    
    /* search for a matching interface - take the first one within the returned scope */
    idx = opal_ifbegin();
    while (0 < idx) {
        /* ignore the loopback interface */
        if (opal_ifisloopback(idx)) {
            fprintf(stderr, "LOOPBACK IGNORED\n");
            idx = opal_ifnext(idx);
            continue;
        }
        if (0 != (rc = opal_ifindextoaddr(idx, (struct sockaddr*)&inaddr, sizeof(inaddr)))) {
            break;
        }
        addr = ntohl(inaddr.sin_addr.s_addr);
        fprintf(stderr, "checking netaddr %03d.%03d.%03d.%03d addr %03d.%03d.%03d.%03d netmask %03d.%03d.%03d.%03d rc %d\n",
                OPAL_IF_FORMAT_ADDR(netaddr), OPAL_IF_FORMAT_ADDR(addr), OPAL_IF_FORMAT_ADDR(netmask), rc);
        if (netaddr == (addr & netmask)) {
            fprintf(stderr, "MATCH FOUND\n");
        }
        idx = opal_ifnext(idx);
    }

    opal_finalize();
    return 0;
}
