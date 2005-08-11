/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "orte_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>

#include "include/orte_constants.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "util/sys_info.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ras/ras.h"
#include "mca/rds/rds.h"
#include "mca/rds/base/base.h"
#include "mca/rds/hostfile/rds_hostfile.h"
#include "mca/rds/hostfile/rds_hostfile_lex.h"

#include "runtime/runtime_types.h"

static orte_cellid_t local_cellid;
static bool need_cellid = true;

static void orte_rds_hostfile_parse_error(void)
{
    opal_output(0, "Error reading hostfile at line %d: %s\n",
        orte_rds_hostfile_line, orte_rds_hostfile_value.sval);
}


static int orte_rds_hostfile_parse_int(void)
{
    if (ORTE_RDS_HOSTFILE_EQUAL != orte_rds_hostfile_lex()) 
        return -1;
    if (ORTE_RDS_HOSTFILE_INT != orte_rds_hostfile_lex()) 
        return -1;
    return orte_rds_hostfile_value.ival;
}


static orte_ras_node_t* orte_rds_hostfile_lookup(opal_list_t* nodes, const char* name)
{
    opal_list_item_t* item;
    for(item =  opal_list_get_first(nodes);
        item != opal_list_get_end(nodes);
        item =  opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;
        if(strcmp(node->node_name, name) == 0) {
            opal_list_remove_item(nodes, item);
            return node;
        }
    }
    return NULL;
}

static int orte_rds_hostfile_parse_line(int token, opal_list_t* existing, opal_list_t* updates)
{
    int rc;
    orte_ras_node_t* node;
    bool update = false;
    bool got_count = false;

    if (ORTE_RDS_HOSTFILE_STRING == token) {
        char* node_name = orte_rds_hostfile_value.sval;

        /* convert this into something globally unique */
        if(strcmp(node_name, "localhost") == 0) {
            node_name = orte_system_info.nodename;
        }

        /* Do we need to make a new node object?  First check to see
           if it's in the existing list. */
        
        if (NULL == (node = orte_rds_hostfile_lookup(existing, node_name))) {

            /* If it wasn't, see if it's already in the updates list */

            if (NULL == (node = orte_rds_hostfile_lookup(updates, 
                                                         node_name))) {
                node = OBJ_NEW(orte_ras_node_t);
                node->node_name = strdup(node_name);
                node->node_slots = 0;

#if 0
                /* get a new cellid for this node */
                /* JMS Temporarily turned off until cell IDs are
                   properly handled elsewhere in the code */
                /* JJH This assumes that each hostname listed should be
                   placed in a new cell. Is this accurate to the design?
                */
                if (ORTE_SUCCESS != 
                    (rc = orte_ns.create_cellid(&(node->node_cellid),
                                                "UNKNOWN-SITE",
                                                node->node_name))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
#endif
            }

            /* Note that we need to set update to true regardless of
               whether the node was found on the updates list or not.
               If it was found, we just removed it (in
               orte_rds_hostfile_lookup()), so the update puts it back
               (potentially after updating it, of course).  If it was
               not found, then we have a new node instance that needs
               to be added to the updates list. */

            update = true;
        }
        else {
            /* If it was in the existing list, then we can use its cellid
             * to add the reset of the hosts in the file to. */
            local_cellid = node->node_cellid;
            need_cellid = false;
        }
    } else {
        orte_rds_hostfile_parse_error();
        return OMPI_ERROR;
    }

    got_count = false;
    while (!orte_rds_hostfile_done) {
        token = orte_rds_hostfile_lex();
        switch (token) {
        case ORTE_RDS_HOSTFILE_DONE:
            goto done;

        case ORTE_RDS_HOSTFILE_NEWLINE:
            goto done;

        case ORTE_RDS_HOSTFILE_COUNT:
        case ORTE_RDS_HOSTFILE_CPU:
        case ORTE_RDS_HOSTFILE_SLOTS:
            rc = orte_rds_hostfile_parse_int();
            if (rc < 0) {
                OBJ_RELEASE(node);
                return OMPI_ERROR;
            }
            node->node_slots += rc;
            update = true;
            got_count = true;

            /* Ensure that node_slots_max >= node_slots */
            if (node->node_slots_max != 0 && node->node_slots_max < node->node_slots) {
                node->node_slots_max = node->node_slots;
            }
            break;

        case ORTE_RDS_HOSTFILE_SLOTS_MAX:
            rc = orte_rds_hostfile_parse_int();
            if (rc < 0) {
                OBJ_RELEASE(node);
                return OMPI_ERROR;
            }
            /* Only take this update if it puts us >= node_slots */
            if (((size_t) rc) >= node->node_slots) {
                if (node->node_slots_max != (size_t)rc) {
                    node->node_slots_max = rc;
                    update = true;
                }
            } else {
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                OBJ_RELEASE(node);
                return OMPI_ERROR;
            }
            break;

        default:
            orte_rds_hostfile_parse_error();
            OBJ_RELEASE(node);
            return OMPI_ERROR;
        }
    }

done:
    if (update) {
        if (!got_count) {
            ++node->node_slots;
        }
        opal_list_append(updates, &node->super);
    } else {
        OBJ_RELEASE(node);
    }
    return OMPI_SUCCESS;
}


/**
 * Parse the specified file into a node list.
 */

static int orte_rds_hostfile_parse(const char *hostfile, opal_list_t* existing, opal_list_t* updates)
{
    int token;
    int rc = ORTE_SUCCESS;

    OPAL_LOCK(&mca_rds_hostfile_component.lock);

    orte_rds_hostfile_done = false;
    orte_rds_hostfile_in = fopen(hostfile, "r");
    if (NULL == orte_rds_hostfile_in) {
        rc = ORTE_ERR_NOT_FOUND;
        goto unlock;
    }

    while (!orte_rds_hostfile_done) {
        token = orte_rds_hostfile_lex();
        switch (token) {
        case ORTE_RDS_HOSTFILE_DONE:
            orte_rds_hostfile_done = true;
            break;

        case ORTE_RDS_HOSTFILE_NEWLINE:
            break;

        case ORTE_RDS_HOSTFILE_STRING:
            rc = orte_rds_hostfile_parse_line(token, existing, updates);
            if (ORTE_SUCCESS != rc) {
                goto unlock;
            }
            break;

        default:
            orte_rds_hostfile_parse_error();
            goto unlock;
        }
    }
    fclose(orte_rds_hostfile_in);
    orte_rds_hostfile_in = NULL;

unlock:
    OPAL_UNLOCK(&mca_rds_hostfile_component.lock);
    return rc;
}

/**
 * Parse the default file as specified by the MCA parameter,
 * rds_hostfile_path, and add the nodes to the registry.
 */

static int orte_rds_hostfile_query(void)
{
    opal_list_t existing;
    opal_list_t updates, rds_updates;
    opal_list_item_t *item;
    int rc;
    
    OBJ_CONSTRUCT(&existing, opal_list_t);
    OBJ_CONSTRUCT(&updates, opal_list_t);
    OBJ_CONSTRUCT(&rds_updates, opal_list_t);
    rc = orte_ras.node_query(&existing);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    rc = mca_base_param_find("rds", "hostfile", "path");
    mca_base_param_lookup_string(rc, &mca_rds_hostfile_component.path);

    rc = orte_rds_hostfile_parse(mca_rds_hostfile_component.path, &existing, &updates);
    if (ORTE_ERR_NOT_FOUND == rc) {
        if(mca_rds_hostfile_component.default_hostfile) {
            rc = ORTE_SUCCESS;
        } else {
            opal_output(0, "orte_rds_hostfile: could not open %s\n", mca_rds_hostfile_component.path);
        }
        goto cleanup;
    } else if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    if ( !opal_list_is_empty(&updates) ) {
        orte_rds_cell_desc_t *rds_item;
        orte_rds_cell_attr_t *new_attr;
        orte_ras_node_t *ras_item;
        opal_list_item_t *item;

        /* Convert RAS update list to RDS update list */
        for ( item  = opal_list_get_first(&updates);
              item != opal_list_get_end(  &updates);
              item  = opal_list_get_next( item)) {
            ras_item = (orte_ras_node_t *) item;
            
            rds_item = OBJ_NEW(orte_rds_cell_desc_t);
            if (NULL == rds_item) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }

            rds_item->site  = strdup("Hostfile");
            rds_item->name  = strdup(ras_item->node_name);
            if (need_cellid) {
#if 0 /* JJH Repair when cellid's are fixed */
                /* Create a new cellid for this hostfile */
                rc = orte_ns.create_cellid(&local_cellid, rds_item->site, rds_item->name);
                if (ORTE_SUCCESS != rc) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
#endif
                local_cellid = 0;
                need_cellid = false;
            }
            
            rds_item->cellid      = local_cellid;
            ras_item->node_cellid = local_cellid;

            new_attr = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == new_attr) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            new_attr->keyval.key          = strdup(ORTE_RDS_NAME);
            new_attr->keyval.type         = ORTE_STRING;
            new_attr->keyval.value.strptr = strdup(ras_item->node_name);
            opal_list_append(&(rds_item->attributes), &new_attr->super);
            
            new_attr = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == new_attr) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            new_attr->keyval.key          = strdup(ORTE_CELLID_KEY);
            new_attr->keyval.type         = ORTE_CELLID;
            new_attr->keyval.value.cellid = rds_item->cellid;
            opal_list_append(&(rds_item->attributes), &new_attr->super);
            
            opal_list_append(&rds_updates, &rds_item->super);
        }

        /* Insert the new node into the RDS */
        rc = orte_rds.store_resource(&rds_updates);
        if (ORTE_SUCCESS != rc) {
            goto cleanup;
        }

        /* Then the RAS, since we can assume that any
         * resources listed in the hostfile have been
         * already allocated for our use.
         */
        rc = orte_ras.node_insert(&updates);
        if (ORTE_SUCCESS != rc) {
            goto cleanup;
        }
    }
   
cleanup:
    if (NULL != mca_rds_hostfile_component.path) {
        free(mca_rds_hostfile_component.path);
        mca_rds_hostfile_component.path = NULL;
    }
    
    while(NULL != (item = opal_list_remove_first(&existing))) {
        OBJ_RELEASE(item);
    }

    while(NULL != (item = opal_list_remove_first(&updates))) {
        OBJ_RELEASE(item);
    }

    while(NULL != (item = opal_list_remove_first(&rds_updates))) {
        orte_rds_cell_desc_t *rds_item;
        rds_item = (orte_rds_cell_desc_t *) item;
        while (NULL != (item = opal_list_remove_first(&(rds_item->attributes))) ) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&existing);
    OBJ_DESTRUCT(&updates);
    OBJ_DESTRUCT(&rds_updates);

    return rc;
}


static int orte_rds_hostfile_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_rds_base_module_t orte_rds_hostfile_module = {
    orte_rds_hostfile_query,
    orte_rds_base_store_resource,
    orte_rds_hostfile_finalize
};

