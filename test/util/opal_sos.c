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
#include <errno.h>

#include "support.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "opal/util/opal_sos.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "orte/runtime/runtime.h"
#include "orte/constants.h"
#include "ompi/constants.h"

static bool opal_sos_test(void);

int
main(int argc, char *argv[])
{
    opal_init_util(&argc, &argv);
    test_init("opal_sos test suite");
    opal_sos_test();
    opal_finalize();
    test_finalize();
    return 0;
}

/** OPAL_SOS_* macro test */
static bool opal_sos_test(void)
{
	int errnum1 = 0, errnum2 = 0;
	char *err_str;

    /* Checking for the correctness of GET_ and SET_ error code
     * operations */
    errnum1 = OPAL_SOS_GET_ERROR_CODE(OMPI_ERR_OUT_OF_RESOURCE);
    test_verify("failed", OMPI_ERR_OUT_OF_RESOURCE == errnum1);

    OPAL_SOS_SET_ERROR_CODE(errnum1, OMPI_ERR_IN_ERRNO);
    test_verify("failed", OMPI_ERR_IN_ERRNO ==
                OPAL_SOS_GET_ERROR_CODE(errnum1));

    /* Check if OMPI_ERR_OUT_OF_RESOURCE is a native error code or
     * not. Since OMPI_ERR_OUT_OF_RESOURCE is native, this should
     * return true. */
    test_verify("failed", true ==
                OPAL_SOS_IS_NATIVE(OMPI_ERR_OUT_OF_RESOURCE));

    test_verify("failed", true == OPAL_SOS_IS_NATIVE(errnum1));

    /* Encode a native error (OMPI_ERR_OUT_OF_RESOURCE) by
     * logging it in the SOS framework using one of the SOS
     * reporter macros. This returns an encoded error code
     * (errnum1) with information about the native error such
     * as the severity, the native error code, the attached
     * error index etc. */
    errnum1 = OPAL_SOS_INFO((OMPI_ERR_OUT_OF_RESOURCE, false,
                             "Error %d: out of resource",
                             OMPI_ERR_OUT_OF_RESOURCE));

    /* Check if errnum1 is native or not. This should return false */
    test_verify("failed", false == OPAL_SOS_IS_NATIVE(errnum1));
    test_verify("failed",
                OPAL_SOS_SEVERITY_INFO == OPAL_SOS_GET_SEVERITY(errnum1));

    /* Extract the native error code out of errnum1. This should
     * return the encoded native error code associated with errnum1
     * (i.e. OMPI_ERR_OUT_OF_RESOURCE). */
    test_verify("failed", OMPI_ERR_OUT_OF_RESOURCE ==
                OPAL_SOS_GET_ERROR_CODE(errnum1));

    /* We log another error event as a child of the previous error
     * errnum1. In the process, we decide to raise the severity
     * level from INFO to WARN. */
    err_str = opal_output_string(0, 0, "my error string -100");
    errnum1 = OPAL_SOS_WARN((errnum1, false, err_str));
    test_verify("failed",
                OPAL_SOS_SEVERITY_WARN == OPAL_SOS_GET_SEVERITY(errnum1));

    test_verify("failed", OMPI_ERR_OUT_OF_RESOURCE ==
                OPAL_SOS_GET_ERROR_CODE(errnum1));
    free(err_str);

	/* Let's report another event with severity ERROR using
	 * OPAL_SOS_ERROR() and in effect promote errnum1 to
     * severity 'ERROR'. */
    err_str = opal_show_help_string("help-opal-util.txt",
                                    "stacktrace signal override",
                                    false, 10, 10, 10, "15");
    errnum1 = OPAL_SOS_ERROR((errnum1, false, err_str));
    test_verify("failed",
                OPAL_SOS_SEVERITY_ERROR == OPAL_SOS_GET_SEVERITY(errnum1));
    free(err_str);

    /* Check the native code associated with the previously encoded
     * error. This should still return (OMPI_ERR_OUT_OF_RESOURCE)
     * since the entire error history originates from the native 
     * error OMPI_ERR_OUT_OF_RESOURCE */
    test_verify("failed", OMPI_ERR_OUT_OF_RESOURCE ==
                OPAL_SOS_GET_ERROR_CODE(errnum1));

    /* We start off another error history stack originating with a 
     * native error, ORTE_ERR_FATAL. */
    asprintf(&err_str, "Fatal error occurred in ORTE %d", errnum1);
    errnum2 = OPAL_SOS_ERROR((ORTE_ERR_FATAL, true, err_str));
    free(err_str);
    test_verify("failed",
                OPAL_SOS_SEVERITY_ERROR == OPAL_SOS_GET_SEVERITY(errnum2));
    test_verify("failed", OMPI_ERR_FATAL ==
                OPAL_SOS_GET_ERROR_CODE(errnum2));

	/* Registering another error with severity ERROR.
     * There is no change in the severity */
    errnum2 = OPAL_SOS_WARN((errnum2, false, "this process must die."));
    test_verify("failed",
                OPAL_SOS_SEVERITY_WARN == OPAL_SOS_GET_SEVERITY(errnum2));
    test_verify("failed", OMPI_ERR_FATAL ==
                OPAL_SOS_GET_ERROR_CODE(errnum2));

	/* We attach the two error traces originating from errnum1
	 * and errnum2. The "attached error index" in errnum1 is
     * set to errnum2 to indicate that the two error stacks
     * are forked down from this point on. */
    OPAL_SOS_ATTACH(errnum1, errnum2);

    /* Print out the entire error event history originating from errnum1 */
#if 0
    printf("<------ BEGIN output of OPAL SOS error message ------->\n");
    OPAL_SOS_PRINT(errnum1, true);
    printf("<------ END output of OPAL SOS error message ------->\n");
#endif
    test_success();

    /* Cleanup */
    OPAL_SOS_FREE(&errnum1);
    OPAL_SOS_FREE(&errnum2);

    return true;
}
