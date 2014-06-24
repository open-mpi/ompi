/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <unistd.h>
#include <infiniband/verbs.h>

#include "opal/util/show_help.h"

#include "ompi/constants.h"

#include "btl_usnic_util.h"
#include "opal/util/if.h"


void ompi_btl_usnic_exit(void)
{
    ompi_rte_abort(1, NULL);

    /* If the error manager returns, wait to be killed */
    while (1) {
        sleep(99999);
    }
}


void
ompi_btl_usnic_dump_hex(uint8_t *addr, int len)
{
    char buf[128];
    size_t bufspace;
    int i, ret;
    char *p;
    uint32_t sum=0;

    p = buf;
    memset(buf, 0, sizeof(buf));
    bufspace = sizeof(buf) - 1;

    for (i=0; i<len; ++i) {
        ret = snprintf(p, bufspace, "%02x ", addr[i]);
        p += ret;
        bufspace -= ret;

        sum += addr[i];
        if ((i&15) == 15) {
            opal_output(0, "%4x: %s\n", i&~15, buf);

            p = buf;
            memset(buf, 0, sizeof(buf));
            bufspace = sizeof(buf) - 1;
        }
    }
    if ((i&15) != 0) {
        opal_output(0, "%4x: %s\n", i&~15, buf);
    }
    /*opal_output(0, "buffer sum = %x\n", sum); */
}


/*
 * Trivial wrapper around snprintf'ing an IPv4 address, with or
 * without a CIDR mask (we don't usually carry around addresses in
 * struct sockaddr form, so this wrapper is marginally easier than
 * using inet_ntop()).
 */
void ompi_btl_usnic_snprintf_ipv4_addr(char *out, size_t maxlen,
                                       uint32_t addr, uint32_t cidrmask)
{
    uint8_t *p = (uint8_t*) &addr;
    if (cidrmask > 0) {
        snprintf(out, maxlen, "%u.%u.%u.%u/%u",
                 p[0],
                 p[1],
                 p[2],
                 p[3],
                 cidrmask);
    } else {
        snprintf(out, maxlen, "%u.%u.%u.%u",
                 p[0],
                 p[1],
                 p[2],
                 p[3]);
    }
}


void ompi_btl_usnic_sprintf_mac(char *out, const uint8_t mac[6])
{
    snprintf(out, 32, "%02x:%02x:%02x:%02x:%02x:%02x", 
             mac[0],
             mac[1],
             mac[2],
             mac[3],
             mac[4],
             mac[5]);
}


void ompi_btl_usnic_sprintf_gid_mac(char *out, union ibv_gid *gid)
{
    uint8_t mac[6];
    ompi_btl_usnic_gid_to_mac(gid, mac);
    ompi_btl_usnic_sprintf_mac(out, mac);
}

/* Pretty-print the given boolean array as a hexadecimal string.  slen should
 * include space for any null terminator. */
void ompi_btl_usnic_snprintf_bool_array(char *s, size_t slen, bool a[], size_t alen)
{
    size_t i = 0;
    size_t j = 0;

    /* could accommodate other cases, but not needed right now */
    assert(slen % 4 == 0);

    /* compute one nybble at a time */
    while (i < alen && (j < slen - 1)) {
        unsigned char tmp = 0;

        /* first bool is the leftmost (most significant) bit of the nybble */
        tmp |= !!a[i+0] << 3;
        tmp |= !!a[i+1] << 2;
        tmp |= !!a[i+2] << 1;
        tmp |= !!a[i+3] << 0;
        tmp += '0';
        s[j] = tmp;

        ++j;
        i += 4;
    }

    s[j++] = '\0';
    assert(i <= alen);
    assert(j <= slen);
}


int ompi_btl_usnic_find_ip(ompi_btl_usnic_module_t *module, uint8_t mac[6])
{
    int i;
    uint8_t localmac[6];
    char addr_string[32], mac_string[32];
    struct sockaddr sa;
    struct sockaddr_in *sai;

    /* Loop through all IP interfaces looking for the one with the
       right MAC */
    for (i = opal_ifbegin(); i != -1; i = opal_ifnext(i)) {
        if (OPAL_SUCCESS == opal_ifindextomac(i, localmac)) {

            /* Is this the MAC I'm looking for? */
            if (0 != memcmp(mac, localmac, 6)) {
                continue;
            }

            /* Yes, it is! */
            if (OPAL_SUCCESS != opal_ifindextoname(i, module->if_name, 
                                                   sizeof(module->if_name)) ||
                OPAL_SUCCESS != opal_ifindextoaddr(i, &sa, sizeof(sa)) ||
                OPAL_SUCCESS != opal_ifindextomask(i, &module->if_cidrmask,
                                                   sizeof(module->if_cidrmask)) ||
                OPAL_SUCCESS != opal_ifindextomac(i, module->if_mac) ||
                OPAL_SUCCESS != opal_ifindextomtu(i, &module->if_mtu)) {
                continue;
            }

            sai = (struct sockaddr_in *) &sa;
            memcpy(&module->if_ipv4_addr, &sai->sin_addr, 4);

            /* Save this information to my local address field on the
               module so that it gets sent in the modex */
            module->local_addr.ipv4_addr = module->if_ipv4_addr;
            module->local_addr.cidrmask = module->if_cidrmask;

            /* Since verbs doesn't offer a way to get standard
               Ethernet MTUs (as of libibverbs 1.1.5, the MTUs are
               enums, and don't inlcude values for 1500 or 9000), look
               up the MTU in the corresponding enic interface. */
            module->local_addr.mtu = module->if_mtu;

            inet_ntop(AF_INET, &(module->if_ipv4_addr),
                      addr_string, sizeof(addr_string));
            ompi_btl_usnic_sprintf_mac(mac_string, mac);
            opal_output_verbose(5, USNIC_OUT,
                                "btl:usnic: found usNIC device corresponds to IP device %s, %s/%d, MAC %s",
                                module->if_name, addr_string, module->if_cidrmask, 
                                mac_string);
            return OMPI_SUCCESS;
        }
    }

    return OMPI_ERR_NOT_FOUND;
}


/*
 * Reverses the encoding done in usnic_main.c:usnic_mac_to_gid() in
 * the usnic.ko kernel code.
 *
 * Got this scheme from Mellanox RoCE; Emulex did the same thing.  So
 * we followed convention.
 * http://www.mellanox.com/related-docs/prod_software/RoCE_with_Priority_Flow_Control_Application_Guide.pdf
 */
void ompi_btl_usnic_gid_to_mac(union ibv_gid *gid, uint8_t mac[6])
{
    mac[0] = gid->raw[8] ^ 2;
    mac[1] = gid->raw[9];
    mac[2] = gid->raw[10];
    mac[3] = gid->raw[13];
    mac[4] = gid->raw[14];
    mac[5] = gid->raw[15];
}

/* takes an IPv4 address in network byte order and a CIDR prefix length (the
 * "X" in "a.b.c.d/X") and returns the subnet in network byte order. */
uint32_t ompi_btl_usnic_get_ipv4_subnet(uint32_t addrn, uint32_t cidr_len)
{
    uint32_t mask;

    assert(cidr_len <= 32);

    /* perform arithmetic in host byte order for shift correctness */
    mask = (~0) << (32 - cidr_len);
    return htonl(ntohl(addrn) & mask);
}

/*
 * Simple utility in a .c file, mainly so that inline functions in .h
 * files don't need to include RTE header files.
 */
void ompi_btl_usnic_util_abort(const char *msg, const char *file, int line,
                               int ret)
{
    opal_show_help("help-mpi-btl-usnic.txt", "internal error after init",
                   true,
                   ompi_process_info.nodename,
                   msg, file, line, strerror(ret));

    ompi_rte_abort(ret, NULL);
    /* Never returns */
}


/* Return the largest size data size that can be packed into max_len using the
 * given convertor.  For example, a 1000 byte max_len buffer may only be able
 * to hold 998 bytes if an indivisible convertor element straddles the 1000
 * byte boundary.
 *
 * This routine internally clones the convertor and does not mutate it!
 */
size_t ompi_btl_usnic_convertor_pack_peek(
    const opal_convertor_t *conv,
    size_t max_len)
{
    int rc;
    size_t packable_len, position;
    opal_convertor_t temp;

    OBJ_CONSTRUCT(&temp, opal_convertor_t);
    position = conv->bConverted + max_len;
    rc = opal_convertor_clone_with_position(conv, &temp, 1, &position);
    if (OPAL_UNLIKELY(rc < 0)) {
        BTL_ERROR(("unexpected convertor error"));
        abort(); /* XXX */
    }
    assert(position >= conv->bConverted);
    packable_len = position - conv->bConverted;
    OBJ_DESTRUCT(&temp);
    return packable_len;
}
