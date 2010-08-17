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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/util/argv.h"
#include "opal/util/opal_sos.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"

#include "orte/mca/notifier/base/base.h"

#if !ORTE_DISABLE_FULL_SUPPORT

/* Global variables */
/*
 * orte_notifier_base_XXX_selected is set to true if at least 1 module has
 * been selected for the notifier XXX API interface.
 */
bool orte_notifier_base_log_selected = false;
bool orte_notifier_base_help_selected = false;
bool orte_notifier_base_log_peer_selected = false;
bool orte_notifier_base_log_event_selected = false;

static opal_sos_reporter_callback_fn_t prev_reporter_callback;
static inline char **orte_notifier_get_include_list(const char *,
                                                    const char *,
                                                    char **);
static bool orte_notifier_add_module(mca_base_component_t *component,
                                     orte_notifier_base_module_t *module,
                                     int priority,
                                     char **include_list,
                                     opal_list_t *selected_modules);

static void onbsp_construct(orte_notifier_base_selected_pair_t *obj)
{
    obj->onbsp_component = NULL;
    obj->onbsp_module = NULL;
    obj->onbsp_priority = -1;
}

static void onbsp_destruct(orte_notifier_base_selected_pair_t *obj)
{
    onbsp_construct(obj);
}

OBJ_CLASS_INSTANCE(orte_notifier_base_selected_pair_t,
                   opal_list_item_t,
                   onbsp_construct,
                   onbsp_destruct);


/**
 * Function for selecting a set of components from all those that are
 * available.
 *
 * It is possible to select a subset of these components for any interface.
 * The syntax is the following:
 * [ -mca notifier <list0> ] [ -mca notifier_log <list1> ]
 *                           [ -mca notifier_help <list2> ]
 *                           [ -mca notifier_log_peer <list3> ]
 *                           [ -mca notifier_log_event <list4> ]
 * Rules:
 * . <list0> empty means nothing selected
 * . <list0> to <list4> = comma separated lists of component names
 * . <list1> to <list4> may be one of:
 *     . subsets of <list0>
 *     . "none" keyword (means empty)
 * . 1 of <list1> to <list4> empty means = <list0>
 * Last point makes it possible to preserve the way it works today
 *
 * Examples:
 * 1)
 * -mca notifier syslog,smtp
 *      --> syslog and smtp are selected for the log, show_help, log_peer and
 *          log_event interfaces.
 * 2)
 * -mca notifier_log syslog
 *      --> no interface is activated, no component is selected
 * 3)
 * -mca notifier syslog -mca notifier_help none
 *                      -mca notifier_log_peer none
 *                      -mca notifier_log_event none
 *      --> only the log interface is activated, with the syslog component
 * 4)
 * -mca notifier syslog,smtp,hnp -mca notifier_help syslog
 *                               -mca notifier_log_peer smtp
 *                               -mca notifier_log_event none
 *      --> the log interface is activated, with the syslog, smtp and hnp
 *                                               components
 *          the log_help interface is activated, with the syslog component
 *          the log_peer interface is activated, with the smtp component
 *          the log_event interface is not activated
 */
int orte_notifier_base_select(void)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    int i, ret, priority, exit_status = ORTE_SUCCESS;
    opal_list_item_t *item;
    orte_notifier_base_module_t *nmodule;
    char **imodules;
    char **imodules_log, **imodules_help, **imodules_log_peer;
    char **imodules_log_event = NULL;
    bool module_needed;

    /*
     * Register the framework MCA param and look up include list
     */
    imodules = orte_notifier_get_include_list("notifier",
                   "Comma-delimisted list of notifier components to use "
                   "(empty = none)", NULL);
    if (NULL == imodules) {
        return ORTE_SUCCESS;
    }

    /*
     * Also get the include lists for each interface
     */
    imodules_log = orte_notifier_get_include_list("notifier_log",
                       "Comma-delimisted list of notifier components to use "
                       "for orte_notifier_log (empty = all selected)",
                       imodules);

    imodules_help = orte_notifier_get_include_list("notifier_help",
                        "Comma-delimisted list of notifier components to use "
                        "for orte_notifier_show_help (empty = all selected)",
                        imodules);

    imodules_log_peer = orte_notifier_get_include_list("notifier_log_peer",
                            "Comma-delimisted list of notifier components to "
                            "use for orte_notifier_log_peer (empty = all "
                            "selected)", imodules);

#if ORTE_WANT_NOTIFIER_LOG_EVENT
    imodules_log_event = orte_notifier_get_include_list("notifier_log_event",
                             "Comma-delimisted list of notifier components to "
                             "use for ORTE_NOTIFIER_LOG_EVENT (empty = all "
                             "selected)",
                             imodules);
#endif /* ORTE_WANT_NOTIFIER_LOG_EVENT */

    /* Query all available components and ask if they have a module */
    for (item = opal_list_get_first(&orte_notifier_base_components_available);
         opal_list_get_end(&orte_notifier_base_components_available) != item;
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;

        /* If this component is not in the include list, skip it */
        for (i = 0; NULL != imodules[i]; ++i) {
            if (0 == strcmp(imodules[i], component->mca_component_name)) {
                break;
            }
        }
        if (NULL == imodules[i]) {
            continue;
        }

        /* If there's no query function, skip it */
        if (NULL == component->mca_query_component) {
            opal_output_verbose(5, orte_notifier_base_output,
                                "mca:notify:select: Skipping component [%s]. It does not implement a query function",
                                component->mca_component_name );
            continue;
        }

        /* Query the component */
        opal_output_verbose(5, orte_notifier_base_output,
                            "mca:notify:select: Querying component [%s]",
                            component->mca_component_name);
        ret = component->mca_query_component(&module, &priority);

        /* If no module was returned, then skip component */
        if (ORTE_SUCCESS != ret || NULL == module) {
            opal_output_verbose(5, orte_notifier_base_output,
                                "mca:notify:select: Skipping component [%s]. Query failed to return a module",
                                component->mca_component_name );
            continue;
        }

        /* If we got a module, initialize it */
        nmodule = (orte_notifier_base_module_t*) module;
        if (NULL != nmodule->init) {
            /* If the module doesn't want to be used, skip it */
            if (ORTE_SUCCESS != (ret = nmodule->init()) ) {
                if (ORTE_ERR_NOT_SUPPORTED != OPAL_SOS_GET_ERROR_CODE(ret) &&
                    ORTE_ERR_NOT_IMPLEMENTED != OPAL_SOS_GET_ERROR_CODE(ret)) {
                    exit_status = ret;
                    goto cleanup;
                }

                if (NULL != nmodule->finalize) {
                    nmodule->finalize();
                }
                continue;
            }
        }

        /*
         * OK, one module has been selected for the notifier framework, and
         * successfully initialized.
         * Now we have to include it in the per interface selected modules
         * lists if needed.
         */
        ret = orte_notifier_add_module(component,
                                       nmodule,
                                       priority,
                                       imodules_log,
                                       &orte_notifier_log_selected_modules);

        orte_notifier_base_log_selected = orte_notifier_base_log_selected
                                          || ret;
        /*
         * This variable is set to check if the module is needed by at least
         * one interface.
         */
        module_needed = ret;

        ret = orte_notifier_add_module(component,
                                       nmodule,
                                       priority,
                                       imodules_help,
                                       &orte_notifier_help_selected_modules);
        orte_notifier_base_help_selected = orte_notifier_base_help_selected
                                           || ret;
        module_needed = module_needed || ret;

        ret = orte_notifier_add_module(component,
                                       nmodule,
                                       priority,
                                       imodules_log_peer,
                                    &orte_notifier_log_peer_selected_modules);
        orte_notifier_base_log_peer_selected =
            orte_notifier_base_log_peer_selected || ret;
        module_needed = module_needed || ret;

        ret = orte_notifier_add_module(component,
                                       nmodule,
                                       priority,
                                       imodules_log_event,
                                    &orte_notifier_log_event_selected_modules);
        orte_notifier_base_log_event_selected =
            orte_notifier_base_log_event_selected || ret;
        module_needed = module_needed || ret;

        /*
         * If the module is needed by at least one interface:
         * Unconditionally update the global list that will be used during
         * the close step. Else unload it.
         */
        if (module_needed) {
            orte_notifier_add_module(component,
                                     nmodule,
                                     priority,
                                     imodules,
                                     &orte_notifier_base_selected_modules);
        } else {
            if (NULL != nmodule->finalize) {
                nmodule->finalize();
            }
        }
    }

    if (orte_notifier_base_log_event_selected) {
        /*
         * This has to be done whatever the selected module. That's why it's
         * done here.
         */
        orte_notifier_base_events_init();
    }

    /* Register a callback with OPAL SOS so that we can intercept
     * error messages */
    opal_sos_reg_reporter_callback((opal_sos_reporter_callback_fn_t) orte_notifier_log,
                                   &prev_reporter_callback);

 cleanup:
    return exit_status;
}

/**
 * Register an mca param that represents an include list and build that list.
 *
 * @param param_name       (IN)  param name to be registered
 * @param help_message     (IN)  help message for that param
 * @param default_modules  (IN)  list of module names to be inherited if an
 *                               empty include list is provided
 * @return                       list of modules names
 */
static inline char **orte_notifier_get_include_list(const char *param_name,
                                                    const char *help_message,
                                                    char **default_modules)
{
    char *include_list = NULL;
    char **imodules;

    mca_base_param_reg_string_name(param_name, NULL, help_message,
                                   false, false, NULL, &include_list);
    imodules = opal_argv_split(include_list, ',');
    if (NULL == imodules) {
        /*
         * Inherit the default list if nothing specified
         */
        return default_modules;
    }
    if (!strcmp(include_list, "none")) {
        return NULL;
    }
    return imodules;
}


/**
 * Check if a component name belongs to an include list and add it to the
 * list of selected modules.
 *
 * @param component        (IN)  component to be included
 * @param module           (IN)  module to be included
 * @param priority         (IN)  module priority
 * @param include_list     (IN)  list of module names to go through
 * @param selected_modules (OUT) list of selected modules to be updated
 * @return                       true/false depending on whether the module
 *                               has been added or not
 */
static bool orte_notifier_add_module(mca_base_component_t *component,
                                     orte_notifier_base_module_t *module,
                                     int priority,
                                     char **include_list,
                                     opal_list_t *selected_modules)
{
    orte_notifier_base_selected_pair_t *pair, *pair2;
    char *module_name;
    opal_list_item_t *item;
    int i;

    if (NULL == include_list) {
        return false;
    }

    module_name = component->mca_component_name;

    /* If this component is not in the include list, skip it */
    for (i = 0; NULL != include_list[i]; i++) {
        if (!strcmp(include_list[i], module_name)) {
            break;
        }
    }
    if (NULL == include_list[i]) {
        return false;
    }

    /* Make an item for the list */
    pair = OBJ_NEW(orte_notifier_base_selected_pair_t);
    pair->onbsp_component = (orte_notifier_base_component_t*) component;
    pair->onbsp_module = module;
    pair->onbsp_priority = priority;

    /* Put it in the list in priority order */
    for (item = opal_list_get_first(selected_modules);
         opal_list_get_end(selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair2 = (orte_notifier_base_selected_pair_t*) item;
        if (priority > pair2->onbsp_priority) {
            opal_list_insert_pos(selected_modules, item, &(pair->super));
            break;
        }
    }
    if (opal_list_get_end(selected_modules) == item) {
        opal_list_append(selected_modules, &(pair->super));
    }

    return true;
}

#endif
