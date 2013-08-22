/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/util/if.h"
#include "opal/runtime/opal.h"

#include "opal/mca/if/base/base.h"

int main(int argc, char* argv[])
{
    int rc, idx;
    uint32_t addr, netmask, netaddr;
    struct sockaddr_in inaddr, *paddr;
    char **aliases=NULL;
    opal_if_t *intf;

    if (0 > (rc = opal_init(&argc, &argv))) {
        fprintf(stderr, "opal_interface: couldn't init opal - error code %d\n", rc);
        return rc;
    }
    
    if (OPAL_SUCCESS != mca_base_framework_open(&opal_if_base_framework, 0)) {
        fprintf(stderr, "opal_interface: couldn't get interfaces\n");
        return;
    }

    for (intf =  (opal_if_t*)opal_list_get_first(&opal_if_list);
        intf != (opal_if_t*)opal_list_get_end(&opal_if_list);
        intf =  (opal_if_t*)opal_list_get_next(intf)) {
        paddr = (struct sockaddr_in*) &intf->if_addr;
        fprintf(stderr, "intf: %s AF: %s indx: %d kindx: %d\n", intf->if_name,
                (AF_INET == paddr->sin_family) ? "v4" : "v6",
                intf->if_index, intf->if_kernel_index);
    }

    if (2 == argc) {
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
    }

    /* check the aliases */
    opal_ifgetaliases(&aliases);
    idx = 0;
    while (NULL != aliases[idx]) {
        fprintf(stderr, "alias: %s\n", aliases[idx]);
        idx++;
    }

    opal_finalize();
    return 0;
}
