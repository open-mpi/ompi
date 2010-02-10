/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "support.h"
#include "opal/runtime/opal.h"
#include "opal/util/if.h"
#include "opal/constants.h"


static bool
test_ifaddrtoname(char *addr)
{
    int ret;
    char addrname[100];
    int len = 99;

    ret = opal_ifaddrtoname(addr, addrname, len);

    if (ret == OPAL_SUCCESS) {
        return true;
    } else {
        return false;
    }
}

int
main(int argc, char *argv[])
{
    char hostname[MAXHOSTNAMELEN];

    opal_init(&argc, &argv);
    test_init("opal_if");

    /* 127.0.0.1 */
    if (test_ifaddrtoname("127.0.0.1")) {
        test_success();
    } else {
        test_failure("ifaddrtoname test failed for 127.0.0.1");
    }
    if (opal_ifislocal("127.0.0.1")) {
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
    if (opal_ifislocal("localhost")) {
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
    if (opal_ifislocal("0.0.0.0")) {
        test_failure("opal_ifislocal test failed for 0.0.0.0");
    } else {
        test_success();
    }

    /* foo.example.com */
    printf("This should generate a warning:\n");
    fflush(stdout);
    if (test_ifaddrtoname("foo.example.com")) {
        test_failure("ifaddrtoname test failed for foo.example.com");
    } else {
        test_success();
    }
    printf("This should generate a warning:\n");
    fflush(stdout);
    if (opal_ifislocal("foo.example.com")) {
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
    if (opal_ifislocal(hostname)) {
        test_success();
    } else {
        test_failure("ifislocal test failed for local host name");
    }

    test_finalize();
    opal_finalize();

    return 0;
}
