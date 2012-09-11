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
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/util/argv.h"
#include "opal/runtime/opal_info_support.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_info_support.h"

#include "ompi/mca/allocator/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/pubsub/base/base.h"
#include "ompi/mca/dpm/base/base.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/vprotocol/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"
#include "ompi/runtime/params.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#endif


#include "ompi/tools/ompi_info/ompi_info.h"


/*
 * Private variables
 */

static bool opened_components = false;


/*
 * Open all MCA components so that they can register their MCA
 * parameters.  Take a shotgun approach here and indiscriminately open
 * all components -- don't be selective.  To this end, we need to clear
 * out the environment of all OMPI_MCA_<type> variables to ensure
 * that the open algorithms don't try to only open one component.
 */
int ompi_info_register_components(opal_pointer_array_t *mca_types,
                                  opal_pointer_array_t *component_map)
{
    int i, rc;
    char *env, *str;
    char *target, *save, *type;
    char **env_save=NULL;
    bool need_close_components = false;
    opal_info_component_map_t *map;
    
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
            free(target);
        }
        free(env);
    }
    
    /* Register the MPI layer's MCA parameters */
    if (OMPI_SUCCESS != (rc = ompi_mpi_register_params())) {
        str = "ompi_mpi_register_params";
        if (OMPI_ERR_BAD_PARAM == rc)  {
            goto breakout;
        }
        goto error;
    }
    
    /* Find / open all components */
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("base");
    opal_pointer_array_add(component_map, map);
    
    /* MPI frameworks */
    if (OMPI_SUCCESS != (rc = mca_allocator_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "allocator open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("allocator");
    map->components = &mca_allocator_base_components;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "allocator";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_btl_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "btl open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("btl");
    map->components = &mca_btl_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "btl";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_coll_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "coll open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("coll");
    map->components = &mca_coll_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "coll";
        goto breakout;
    }

#if OPAL_ENABLE_FT_CR == 1
    if (OMPI_SUCCESS != (rc = ompi_crcp_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "crcp open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("crcp");
    map->components = &ompi_crcp_base_components_available;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "crcp";
        goto breakout;
    }
#endif
    
    if (OMPI_SUCCESS != (rc = ompi_dpm_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "dpm open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("dpm");
    map->components = &ompi_dpm_base_components_available;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "dpm";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_fbtl_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "fbtl open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("fbtl");
    map->components = &mca_fbtl_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "fbtl";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_fcoll_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "fcoll open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("fcoll");
    map->components = &mca_fcoll_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "fcoll";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_fs_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "fs open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("fs");
    map->components = &mca_fs_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "fs";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_io_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "io open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("io");
    map->components = &mca_io_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "io";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_mpool_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "mpool open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("mpool");
    map->components = &mca_mpool_base_components;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "mpool";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = ompi_mtl_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "mtl open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("mtl");
    map->components = &ompi_mtl_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "mtl";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = ompi_op_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "op open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("op");
    map->components = &ompi_op_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "op";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = ompi_osc_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "osc open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("osc");
    map->components = &ompi_osc_base_open_components;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "osc";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_pml_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "pml open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("pml");
    map->components = &mca_pml_base_components_available;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "pml";
        goto breakout;
    }

    /* No need to call the bml_base_open() because the ob1 pml calls it.
     * mca_bml_base_open();
     */
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("bml");
    map->components = &mca_bml_base_components_available;
    opal_pointer_array_add(component_map, map);
    
    if (OMPI_SUCCESS != (rc = ompi_pubsub_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "pubsub open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("pubsub");
    map->components = &ompi_pubsub_base_components_available;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "pubsub";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_rcache_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "rcache open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("rcache");
    map->components = &mca_rcache_base_components;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "rcache";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_sharedfp_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "sharedfp open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("sharedfp");
    map->components = &mca_sharedfp_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "sharedfp";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_topo_base_open()) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "topo open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("topo");
    map->components = &mca_topo_base_components_opened;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "topo";
        goto breakout;
    }

    if (OMPI_SUCCESS != (rc = mca_vprotocol_base_open(NULL)) &&
        OMPI_ERR_BAD_PARAM != rc) {
        str = "vprotocol open";
        goto error;
    }
    map = OBJ_NEW(opal_info_component_map_t);
    map->type = strdup("vprotocol");
    map->components = &mca_vprotocol_base_components_available;
    opal_pointer_array_add(component_map, map);
    if (OMPI_ERR_BAD_PARAM == rc)  {
        str = "vprotocol";
        goto breakout;
    }

    /* flag that we need to close components */
    need_close_components = true;

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
    
    if (OPAL_ERR_BAD_PARAM == rc) {
        fprintf(stderr, "\nA \"bad parameter\" error was encountered when opening the OMPI %s framework\n", str);
        fprintf(stderr, "The output received from that framework includes the following parameters:\n\n");
    }

    opened_components = true;
    return rc;

 error:
    fprintf(stderr, "ompi_info_register: %s failed\n", str);
    fprintf(stderr, "ompi_info will likely not display all configuration information\n");
    return OMPI_ERROR;
}


void ompi_info_close_components()
{
    /* Note that the order of shutdown here doesn't matter because
     * we aren't *using* any components -- none were selected, so
     * there are no dependencies between the frameworks.  We list
     * them generally "in order", but it doesn't really matter.
         
     * We also explicitly ignore the return values from the
     * close() functions -- what would we do if there was an
     * error?
     */
        
#if OPAL_ENABLE_FT_CR == 1
    (void) ompi_crcp_base_close();
#endif
    (void) ompi_op_base_close();
    (void) ompi_dpm_base_close();
    (void) ompi_pubsub_base_close();
    (void) mca_topo_base_close();
    (void) mca_btl_base_close();
    (void) ompi_mtl_base_close();
    (void) mca_pml_base_close();
    (void) mca_mpool_base_close();
    (void) mca_rcache_base_close();
    (void) mca_io_base_close();
    (void) mca_fbtl_base_close();
    (void) mca_fcoll_base_close();
    (void) mca_fs_base_close();
    (void) mca_sharedfp_base_close();
    (void) mca_coll_base_close();
    (void) mca_allocator_base_close();
    (void) ompi_osc_base_close();

    /* close the ORTE components */
    (void) orte_info_close_components();
}
