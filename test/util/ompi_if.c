/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "runtime/runtime.h"
#include "util/if.h"
#include "support.h"
#include "include/constants.h"


static bool
test_ifaddrtoname(char *addr)
{
    int ret;
    char addrname[100];
    int len = 99;

    ret = ompi_ifaddrtoname(addr, addrname, len);

    if (ret == OMPI_SUCCESS) {
        return true;
    } else {
        return false;
    }
}

int
main(int argc, char *argv[])
{
    char hostname[MAXHOSTNAMELEN];

    orte_init();
    test_init("ompi_if");

    /* 127.0.0.1 */
    if (test_ifaddrtoname("127.0.0.1")) {
        test_success();
    } else {
        test_failure("ifaddrtoname test failed for 127.0.0.1");
    }
    if (ompi_ifislocal("127.0.0.1")) {
        test_success();
    } else {
        test_failure("ifislocal test failed for 127.0.0.1");
    }

    /* localhost */
    if (test_ifaddrtoname("localhost")) {
        test_success();
    } else {
        test_failure("ifaddrtoname test failed for localhost");
    }
    if (ompi_ifislocal("localhost")) {
        test_success();
    } else {
        test_failure("ifislocal test failed for localhost");
    }

    /* 0.0.0.0 */
    if (test_ifaddrtoname("0.0.0.0")) {
        test_failure("ifaddrtoname test failed for 0.0.0.0");
    } else {
        test_success();
    }
    if (ompi_ifislocal("0.0.0.0")) {
        test_failure("ompi_ifislocal test failed for 0.0.0.0");
    } else {
        test_success();
    }

    /* foo.example.com */
    if (test_ifaddrtoname("foo.example.com")) {
        test_failure("ifaddrtoname test failed for foo.example.com");
    } else {
        test_success();
    }
    if (ompi_ifislocal("foo.example.com")) {
        test_failure("ifislocal test failed for foo.example.com");
    } else {
        test_success();
    }

    /* local host name */
    gethostname(hostname, MAXHOSTNAMELEN);
    if (test_ifaddrtoname(hostname)) {
        test_success();
    } else {
        test_failure("ifaddrtoname test failed for local host name");
    }
    if (ompi_ifislocal(hostname)) {
        test_success();
    } else {
        test_failure("ifislocal test failed for local host name");
    }

    test_finalize();
    orte_finalize();

    return 0;
}
