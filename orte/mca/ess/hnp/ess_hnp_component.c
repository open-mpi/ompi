/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal/util/argv.h"

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/hnp/ess_hnp.h"
#include "orte/runtime/orte_globals.h"

extern orte_ess_base_module_t orte_ess_hnp_module;
static int hnp_component_register (void);
static int hnp_component_open(void);
static int hnp_component_close(void);
static int hnp_component_query(mca_base_module_t **module, int *priority);

struct known_signal {
    /** signal number */
    int signal;
    /** signal name */
    char *signame;
    /** can this signal be forwarded */
    bool can_forward;
};

static struct known_signal known_signals[] = {
    {SIGTERM, "SIGTERM", false},
    {SIGHUP, "SIGHUP", false},
    {SIGINT, "SIGINT", false},
    {SIGKILL, "SIGKILL", false},
#ifdef SIGSYS
    {SIGSYS, "SIGSYS", true},
#endif
#ifdef SIGXCPU
    {SIGXCPU, "SIGXCPU", true},
#endif
    {SIGXFSZ, "SIGXFSZ", true},
#ifdef SIGVTALRM
    {SIGVTALRM, "SIGVTALRM", true},
#endif
#ifdef SIGPROF
    {SIGPROF, "SIGPROF", true},
#endif
#ifdef SIGINFO
    {SIGINFO, "SIGINFO", true},
#endif
#ifdef SIGPWR
    {SIGPWR, "SIGPWR", true},
#endif
    {0, NULL},
};

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_hnp_component_t mca_ess_hnp_component = {
    .base = {
        .base_version = {
            ORTE_ESS_BASE_VERSION_3_0_0,

            /* Component name and version */
            .mca_component_name = "hnp",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                                  ORTE_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = hnp_component_open,
            .mca_close_component = hnp_component_close,
            .mca_query_component = hnp_component_query,
            .mca_register_component_params = hnp_component_register,
        },
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static char *additional_signals;

static int hnp_component_register (void)
{
    additional_signals = NULL;
    (void) mca_base_component_var_register (&mca_ess_hnp_component.base.base_version,
                                            "forward_signals", "Comma-delimited list "
                                            "of additional signals (names or integers) to forward to "
                                            "application processes [\"none\" => forward nothing]", MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
                                            &additional_signals);

    return ORTE_SUCCESS;
}

#define ESS_ADDSIGNAL(x, s)                                                 \
    do {                                                                    \
        ess_hnp_signal_t *_sig;                                             \
        _sig = OBJ_NEW(ess_hnp_signal_t);                                   \
        _sig->signal = (x);                                                 \
        _sig->signame = strdup((s));                                        \
        opal_list_append(&mca_ess_hnp_component.signals, &_sig->super);     \
    } while(0)

static int hnp_component_open(void)
{
    int i, sval;
    char **signals, *tmp;
    ess_hnp_signal_t *sig;
    bool ignore, found;

    OBJ_CONSTRUCT(&mca_ess_hnp_component.signals, opal_list_t);

    /* we know that some signals are (nearly) always defined, regardless
     * of environment, so add them here */
    ESS_ADDSIGNAL(SIGTSTP, "SIGTSTP");
    ESS_ADDSIGNAL(SIGUSR1, "SIGUSR1");
    ESS_ADDSIGNAL(SIGUSR2, "SIGUSR2");
    ESS_ADDSIGNAL(SIGABRT, "SIGABRT");
    ESS_ADDSIGNAL(SIGALRM, "SIGALRM");
    ESS_ADDSIGNAL(SIGCONT, "SIGCONT");
#ifdef SIGURG
    ESS_ADDSIGNAL(SIGURG, "SIGURG");
#endif

    /* see if they asked for anything beyond those - note that they may
     * have asked for some we already cover, and so we ignore any duplicates */
    if (NULL != additional_signals) {
        /* if they told us "none", then dump the list */
        if (0 == strcmp(additional_signals, "none")) {
            OPAL_LIST_DESTRUCT(&mca_ess_hnp_component.signals);
            /* need to reconstruct it for when we close */
            OBJ_CONSTRUCT(&mca_ess_hnp_component.signals, opal_list_t);
            return ORTE_SUCCESS;
        }
        signals = opal_argv_split(additional_signals, ',');
        for (i=0; NULL != signals[i]; i++) {
            sval = 0;
            if (0 != strncmp(signals[i], "SIG", 3)) {
                /* treat it like a number */
                errno = 0;
                sval = strtoul(signals[i], &tmp, 10);
                if (0 != errno || '\0' != *tmp) {
                    orte_show_help("help-ess-hnp.txt", "ess-hnp:unknown-signal",
                                   true, signals[i], additional_signals);
                    opal_argv_free(signals);
                    return OPAL_ERR_SILENT;
                }
            }

            /* see if it is one we already covered */
            ignore = false;
            OPAL_LIST_FOREACH(sig, &mca_ess_hnp_component.signals, ess_hnp_signal_t) {
                if (0 == strcasecmp(signals[i], sig->signame) || sval == sig->signal) {
                    /* got it - we will ignore */
                    ignore = true;
                    break;
                }
            }

            if (ignore) {
                continue;
            }

            /* see if they gave us a signal name */
            found = false;
            for (int j = 0 ; known_signals[j].signame ; ++j) {
                if (0 == strcasecmp (signals[i], known_signals[j].signame) || sval == known_signals[j].signal) {
                    if (!known_signals[j].can_forward) {
                        orte_show_help("help-ess-hnp.txt", "ess-hnp:cannot-forward",
                                       true, known_signals[j].signame, additional_signals);
                        opal_argv_free(signals);
                        return OPAL_ERR_SILENT;
                    }
                    found = true;
                    ESS_ADDSIGNAL(known_signals[j].signal, known_signals[j].signame);
                    break;
                }
            }

            if (!found) {
                if (0 == strncmp(signals[i], "SIG", 3)) {
                    orte_show_help("help-ess-hnp.txt", "ess-hnp:unknown-signal",
                                   true, signals[i], additional_signals);
                    opal_argv_free(signals);
                    return OPAL_ERR_SILENT;
                }

                ESS_ADDSIGNAL(sval, signals[i]);
            }
        }
        opal_argv_free (signals);
    }

    return ORTE_SUCCESS;
}


static int hnp_component_query(mca_base_module_t **module, int *priority)
{

    /* we are the hnp module - we need to be selected
     * IFF we are designated as the hnp
     */
    if (ORTE_PROC_IS_HNP) {
        *priority = 100;
        *module = (mca_base_module_t *)&orte_ess_hnp_module;
        return ORTE_SUCCESS;
    }

    /* else, we are not */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}


static int hnp_component_close(void)
{
    return ORTE_SUCCESS;
}

/* instantiate the class */
static void scon(ess_hnp_signal_t *t)
{
    t->signame = NULL;
}
static void sdes(ess_hnp_signal_t *t)
{
    if (NULL != t->signame) {
        free(t->signame);
    }
}
OBJ_CLASS_INSTANCE(ess_hnp_signal_t,
                   opal_list_item_t,
                   scon, sdes);
