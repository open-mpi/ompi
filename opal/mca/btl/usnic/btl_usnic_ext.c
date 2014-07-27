/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/output.h"
#include "opal/mca/btl/base/base.h"

#include "btl_usnic_compat.h"
#include "btl_usnic_ext.h"

/*
 * Global variable of usnic extension function pointers
 */
opal_btl_usnic_ext_fns_t opal_btl_usnic_ext;


/*
 * Initialize opal_btl_usnic_ext
 */
void opal_btl_usnic_ext_init(struct ibv_context *context)
{
    memset(&opal_btl_usnic_ext, 0, sizeof(opal_btl_usnic_ext));

    /* See if this context supports the usnic extensions.  Do the
       magic query port on port number 42 (which is THE ANSWER).  If
       it works, we'll get rc==0 and the magic number in the struct
       will be set.  Note, however, that due to a bug in early
       versions of libusnic_verbs, we *may* get rc==0 even if it
       doesn't work, which is why we also must check for the magic
       value, too. */
    int rc;
    struct ibv_port_attr attr;
    rc = ibv_query_port(context, 42, &attr);
    assert(sizeof(opal_btl_usnic_ext) <= sizeof(attr));
    memcpy(&opal_btl_usnic_ext, &attr, sizeof(opal_btl_usnic_ext));
    if (0 != rc || USNIC_PORT_QUERY_MAGIC != opal_btl_usnic_ext.qpt.magic) {
        /* If the probe fails, we must re-memset() the function
           pointer block */
        memset(&opal_btl_usnic_ext, 0, sizeof(opal_btl_usnic_ext));
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: verbs plugin does not support extensions");
        return;
    }

    /* If the libusnic_verbs plugin under the verbs API supporting
       this context supports the usnic extensions, it'll return 0==rc
       and give us a function that we can use to look up other usnic
       verb extension function pointers.  If the lookup_version is one
       that we understand, use it to look up the extensions we care
       about. */
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: verbs plugin has extension lookup ABI version %d",
                        opal_btl_usnic_ext.qpt.lookup_version);
    if (1 != opal_btl_usnic_ext.qpt.lookup_version) {
        /* If the probe fails, we must re-memset() the function
           pointer block, because it may/will return junk in the qpt */
        memset(&opal_btl_usnic_ext, 0, sizeof(opal_btl_usnic_ext));
        opal_output_verbose(5, USNIC_OUT,
                            "btl:usnic: unrecognized lookup ABI version"
                            " (I only recognize version 1) "
                            " -- extensions ignored");
        return;
    }
    opal_output_verbose(5, USNIC_OUT,
                        "btl:usnic: BTL recognizes this lookup ABI -- yay!");

    *(void **) (&opal_btl_usnic_ext.enable_udp) =
        opal_btl_usnic_ext.qpt.lookup("enable_udp");
    *(void **) (&opal_btl_usnic_ext.get_ud_header_len) =
        opal_btl_usnic_ext.qpt.lookup("get_ud_header_len");
}
