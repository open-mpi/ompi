/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <unistd.h>

#include "opal/util/show_help.h"
#include "opal/constants.h"
#include "opal/util/if.h"

#include "btl_usnic_module.h"
#include "btl_usnic_util.h"


void opal_btl_usnic_exit(opal_btl_usnic_module_t *module)
{
    if (NULL == module) {
        /* Find the first module with an error callback */
        for (int i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
            if (NULL != mca_btl_usnic_component.usnic_active_modules[i]->pml_error_callback) {
                module = mca_btl_usnic_component.usnic_active_modules[i];
                break;
            }
        }
        /* If we didn't find a PML error callback, just exit. */
        if (NULL == module) {
            exit(1);
        }
    }

    /* After discussion with George, we decided that it was safe to
       cast away the const from opal_proc_local_get() -- the error
       function needs to be smart enough to not take certain actions
       if the passed proc is yourself (e.g., don't call del_procs() on
       yourself). */
    if (NULL != module->pml_error_callback) {
        module->pml_error_callback(&module->super,
                                   MCA_BTL_ERROR_FLAGS_FATAL,
                                   (opal_proc_t*) opal_proc_local_get(),
                                   "usnic");
    }

    /* If the PML error callback returns (or if there wasn't one),
       just exit.  Shrug. */
    exit(1);
}


/*
 * Simple utility in a .c file, mainly so that inline functions in .h
 * files don't need to include the show_help header file.
 */
void opal_btl_usnic_util_abort(const char *msg, const char *file, int line)
{
    opal_show_help("help-mpi-btl-usnic.txt", "internal error after init",
                   true,
                   opal_process_info.nodename,
                   msg, file, line);

    opal_btl_usnic_exit(NULL);
    /* Never returns */
}


void
opal_btl_usnic_dump_hex(void *vaddr, int len)
{
    char buf[128];
    size_t bufspace;
    int i, ret;
    char *p;
    uint32_t sum=0;
    uint8_t *addr;

    addr = vaddr;
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
void opal_btl_usnic_snprintf_ipv4_addr(char *out, size_t maxlen,
                                       uint32_t addr, uint32_t netmask)
{
    int prefixlen;
    uint8_t *p = (uint8_t*) &addr;
    if (netmask != 0) {
        prefixlen = 33 - ffs(netmask);
        snprintf(out, maxlen, "%u.%u.%u.%u/%u",
                 p[0],
                 p[1],
                 p[2],
                 p[3],
                 prefixlen);
    } else {
        snprintf(out, maxlen, "%u.%u.%u.%u",
                 p[0],
                 p[1],
                 p[2],
                 p[3]);
    }
}


/* Pretty-print the given boolean array as a hexadecimal string.  slen should
 * include space for any null terminator. */
void opal_btl_usnic_snprintf_bool_array(char *s, size_t slen, bool a[],
                                        size_t alen)
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

/* Return the largest size data size that can be packed into max_len using the
 * given convertor.  For example, a 1000 byte max_len buffer may only be able
 * to hold 998 bytes if an indivisible convertor element straddles the 1000
 * byte boundary.
 *
 * This routine internally clones the convertor and does not mutate it!
 */
size_t opal_btl_usnic_convertor_pack_peek(
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
