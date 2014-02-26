/*
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_usnic_test.h"

/* see README.test for info about why/how this file is included into another
 * source file */
#if OMPI_BTL_USNIC_UNIT_TESTS

static int test_parse_ifex_str(void *ctx)
{
    usnic_if_filter_t *f;

    f = parse_ifex_str(NULL, "include");
    check(f == NULL);
    free_filter(f);

    f = parse_ifex_str("", "include");
    check(f == NULL);
    free_filter(f);

    f = parse_ifex_str("usnic_1,usnic_0", "include");
    check(f != NULL);
    check(f->n_elt == 2);
    check(f->elts != NULL);
    check(f->elts[0].is_netmask == false);
    check_str_eq(f->elts[0].if_name, "usnic_1");
    check(f->elts[1].is_netmask == false);
    check_str_eq(f->elts[1].if_name, "usnic_0");
    free_filter(f);

    f = parse_ifex_str("usnic_1,1.2.3.0/24", "exclude");
    check(f != NULL);
    check(f->n_elt == 2);
    check(f->elts != NULL);
    check(f->elts[0].is_netmask == false);
    check_str_eq(f->elts[0].if_name, "usnic_1");
    check(f->elts[1].is_netmask == true);
    check(f->elts[1].addr == htonl(0x01020300));
    check(f->elts[1].prefixlen == 24);
    free_filter(f);

    return 0;
}

USNIC_REGISTER_TEST("test_parse_ifex_str", test_parse_ifex_str, NULL)

#endif
