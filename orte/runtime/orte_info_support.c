/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/class/opal_pointer_array.h"
#include "opal/runtime/opal_info_support.h"

#include "orte/mca/dfs/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/routed/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/base/base.h"
#endif
#if ORTE_ENABLE_SENSORS
#include "orte/mca/sensor/base/base.h"
#endif
#include "orte/mca/filem/base/base.h"
#include "orte/mca/state/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"

#include "orte/runtime/orte_info_support.h"

const char *orte_info_type_orte = "orte";

void orte_info_register_types(opal_pointer_array_t *mca_types)
{
    /* frameworks */
    opal_pointer_array_add(mca_types, "dfs");
    opal_pointer_array_add(mca_types, "errmgr");
    opal_pointer_array_add(mca_types, "ess");
    opal_pointer_array_add(mca_types, "filem");
    opal_pointer_array_add(mca_types, "grpcomm");
    opal_pointer_array_add(mca_types, "iof");
    opal_pointer_array_add(mca_types, "odls");
    opal_pointer_array_add(mca_types, "oob");
    opal_pointer_array_add(mca_types, "orte");
    opal_pointer_array_add(mca_types, "plm");
    opal_pointer_array_add(mca_types, "ras");
    opal_pointer_array_add(mca_types, "rmaps");
    opal_pointer_array_add(mca_types, "rml");
    opal_pointer_array_add(mca_types, "routed");
#if ORTE_ENABLE_SENSORS
    opal_pointer_array_add(mca_types, "sensor");
#endif
#if OPAL_ENABLE_FT_CR == 1
    opal_pointer_array_add(mca_types, "snapc");
    opal_pointer_array_add(mca_types, "sstore");
#endif
    opal_pointer_array_add(mca_types, "state");
}

static int info_register_framework (mca_base_framework_t *framework, opal_pointer_array_t *component_map) {
    opal_info_component_map_t *map;
    int rc;

    if ((OPAL_SUCCESS != (rc = mca_base_framework_register(framework, MCA_BASE_REGISTER_ALL))) ||
        OPAL_ERR_BAD_PARAM != rc) {
        return rc;
    }

    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup(framework->framework_name);
    map->components = &framework->framework_components;
    opal_pointer_array_add(component_map, map);

    return rc;
}

int orte_info_register_components(opal_pointer_array_t *mca_types,
                                  opal_pointer_array_t *component_map)
{
    char *env, *str;
    int i, rc;
    char *target, *save, *type;
    char **env_save=NULL;

    /* Clear out the environment.  Use strdup() to orphan the resulting
     * strings because items are placed in the environment by reference,
     * not by value.
     */
    for (i = 0; i < mca_types->size; ++i) {
        if (NULL == (type = (char*)opal_pointer_array_get_item(mca_types, i))) {
            continue;
        }
        asprintf(&env, "OMPI_MCA_%s", type);
        if (NULL != (save = getenv(env))) {
            /* save this param so it can later be restored */
            asprintf(&str, "%s=%s", env, save);
            opal_argv_append_nosize(&env_save, str);
            free(str);
            /* can't manipulate it directly, so make a copy first */
            asprintf(&target, "%s=", env);
            putenv(target);
        }
        free(env);
    }

    /* Set orte_process_info.proc_type to HNP to force all frameworks to
     * open components
     */
    orte_process_info.proc_type = ORTE_PROC_HNP;
    /* set the event base to be the opal event base as we
     * aren't attempting to do anything with progress threads here
     */
    orte_event_base = opal_event_base;

    /* Register the ORTE layer's MCA parameters */
    
    if (ORTE_SUCCESS != (rc = orte_register_params()) &&
        ORTE_ERR_BAD_PARAM != rc) {
        str = "orte_register_params";
        goto error;
    }
    
    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_dfs_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "dfs register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_errmgr_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "errmgr register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_ess_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "ess register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_filem_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "filem register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_grpcomm_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "grpcomm register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_iof_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "iof register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_odls_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "odls register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_oob_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "oob register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_plm_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "plm register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_ras_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "ras register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_rmaps_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "rmaps register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_rml_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "rml register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_routed_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "routed register";
        goto breakout;
    }

#if ORTE_ENABLE_SENSORS
    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_sensor_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "sensor register";
        goto breakout;
    }
#endif
    
#if OPAL_ENABLE_FT_CR == 1
    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_snapc_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "snapc register";
        goto breakout;
    }

    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_sstore_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "sstore register";
        goto breakout;
    }
#endif
    
    if (OPAL_SUCCESS != (rc = info_register_framework(&orte_state_base_framework, component_map))) {
        fprintf (stderr, "rc = %d\n", rc);
        str = "state register";
        goto breakout;
    }

 breakout:
    /* Restore the environment to what it was before we started so that
     * if users setenv OMPI_MCA_<framework name> to some value, they'll
     * see that value when it is shown via --param output.
     */
    
    if (NULL != env_save) {
        for (i = 0; i < opal_argv_count(env_save); ++i) {
            putenv(env_save[i]);
        }
    }

    if (ORTE_ERR_BAD_PARAM == rc) {
        fprintf(stderr, "\nA \"bad parameter\" error was encountered when opening the ORTE %s framework\n", str);
        fprintf(stderr, "The output received from that framework includes the following parameters:\n\n");
    }

    return rc;

 error:
    fprintf(stderr, "orte_info_register: %s failed\n", str);
    return ORTE_ERROR;
}

void orte_info_close_components(void)
{
    (void) mca_base_framework_close(&orte_dfs_base_framework);
    (void) mca_base_framework_close(&orte_errmgr_base_framework);
    (void) mca_base_framework_close(&orte_ess_base_framework);
    (void) mca_base_framework_close(&orte_filem_base_framework);
    (void) mca_base_framework_close(&orte_grpcomm_base_framework);
    (void) mca_base_framework_close(&orte_iof_base_framework);
    (void) mca_base_framework_close(&orte_odls_base_framework);
    (void) mca_base_framework_close(&orte_oob_base_framework);
    (void) mca_base_framework_close(&orte_plm_base_framework);
    (void) mca_base_framework_close(&orte_ras_base_framework);
    (void) mca_base_framework_close(&orte_rmaps_base_framework);
    (void) mca_base_framework_close(&orte_rml_base_framework);
    (void) mca_base_framework_close(&orte_routed_base_framework);
#if ORTE_ENABLE_SENSORS
    (void) mca_base_framework_close(&orte_sensor_base_framework);
#endif
#if OPAL_ENABLE_FT_CR == 1
    (void) mca_base_framework_close(&orte_snapc_base_framework);
    (void) mca_base_framework_close(&orte_sstore_base_framework);
#endif
    (void) mca_base_framework_close(&orte_state_base_framework);
}

void orte_info_show_orte_version(const char *scope)
{
    char *tmp, *tmp2;

    asprintf(&tmp, "%s:version:full", orte_info_type_orte);
    tmp2 = opal_info_make_version_str(scope, 
                                      ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION, 
                                      ORTE_RELEASE_VERSION, 
                                      ORTE_GREEK_VERSION,
                                      ORTE_WANT_REPO_REV, ORTE_REPO_REV);
    opal_info_out("Open RTE", tmp, tmp2);
    free(tmp);
    free(tmp2);
    asprintf(&tmp, "%s:version:repo", orte_info_type_orte);
    opal_info_out("Open RTE repo revision", tmp, ORTE_REPO_REV);
    free(tmp);
    asprintf(&tmp, "%s:version:release_date", orte_info_type_orte);
    opal_info_out("Open RTE release date", tmp, ORTE_RELEASE_DATE);
    free(tmp);
}

