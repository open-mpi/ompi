/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>

#include <pmix/pmix_common.h>
#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include PMIX_EVENT_HEADER

#include "src/util/argv.h"
#include "src/util/output.h"
#include "pmix_sec.h"

#include "src/sec/pmix_native.h"
#if PMIX_HAVE_MUNGE
#include "src/sec/pmix_munge.h"
#endif
#

/*
 * Array of all possible SPCs
 */

/****  ENSURE THE FOLLOWING VALUE IS AT LEAST AS
 ****  LARGE AS THE TOTAL NUMBER OF SUPPORTED SPCs
 ****  IN THE ARRAY BELOW
 */
#define PMIX_SEC_NAVAIL  3

static pmix_sec_base_module_t *all[] = {
#if PMIX_HAVE_MUNGE
    /* Start the array with the least heavy option, if available */
    &pmix_munge_module,
#endif

    /* Our native module should always be the lowest priority */
    &pmix_native_module,

    /* Always end the array with a NULL */
    NULL
};

pmix_sec_base_module_t pmix_sec = {0};

int pmix_sec_init(void)
{
    pmix_sec_base_module_t *available[PMIX_SEC_NAVAIL];
    int i, j, navail=0;
    char *evar, **options;
    bool exclude;

    /* see if the PMIX_SEC_MODE envar has been provided */
    if (NULL != (evar = getenv("PMIX_SECURITY_MODE"))) {
        /* if the leading character is '^', then we are excluding */
        if ('^' == evar[0]) {
            options = pmix_argv_split(&evar[1], ',');
            /* cycle thru the built modules and xfer them
             * to the available array if they aren't on the list */
            for (i = 0; NULL != all[i]; ++i) {
                exclude = false;
                for (j = 0; NULL != options[j]; ++j) {
                    if (0 == strcmp(options[j], all[i]->name)) {
                        pmix_output_verbose(30, pmix_globals.debug_output,
                                           "Security mode %s excluded", all[i]->name);
                        exclude = true;
                        break;
                    }
                }
                if (!exclude) {
                    available[navail] = all[i];
                    ++navail;
                }
            }
        } else {
            options = pmix_argv_split(evar, ',');
            /* cycle thru the built modules and xfer them
             * to the available array in the specified order */
            for (j = 0; NULL != options[j]; ++j) {
                for (i = 0; NULL != all[i]; ++i) {
                    if (0 == strcmp(options[j], all[i]->name)) {
                        pmix_output_verbose(30, pmix_globals.debug_output,
                                           "Security SPC include: %s", all[i]->name);
                        available[navail] = all[i];
                        ++navail;
                        break;
                    }
                }
                if (NULL == all[i]) {
                    /* we didn't find one they specified */
                    pmix_output(0, "Security mode %s is not available", options[j]);
                    pmix_argv_free(options);
                    return PMIX_ERR_NOT_FOUND;
                }
            }
        }
        pmix_argv_free(options);
    } else {
        /* everything is available */
        for (i=0; NULL != all[i]; i++) {
            available[i] = all[i];
            navail++;
        }
    }

    /* if nothing is available, then that is an error - we need
     * at least our native SPC */
    if (0 == navail) {
        pmix_output(0, "No Security modes are available");
        return PMIX_ERR_NOT_FOUND;
    }

    /* now go thru the available modules in order until someone
     * indicates they are able/willing to run */
    for (i=0; i < navail; i++) {
        if (NULL == available[i]->init) {
            /* we assume this is acceptable */
            pmix_sec = *available[i];
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "sec: SPC %s active", pmix_sec.name);
            return PMIX_SUCCESS;
        }
        if (PMIX_SUCCESS == available[i]->init()) {
            pmix_sec = *available[i];
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "sec: SPC %s active", pmix_sec.name);
            return PMIX_SUCCESS;
        }
    }
    /* if we get here, then none of the available modules
     * returned success */
    return PMIX_ERR_NOT_FOUND;
}

void pmix_sec_finalize(void)
{
    /* tell the active module to finalize */
    if (NULL != pmix_sec.finalize) {
        pmix_sec.finalize();
    }
}
