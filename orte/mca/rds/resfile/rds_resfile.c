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
#include "orte_config.h"

#include <stdio.h>

#include "orte/orte_constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"

#include "orte/mca/rds/base/rds_private.h"
#include "orte/mca/rds/resfile/rds_resfile.h"

#define ORTE_RDS_RESFILE_MAX_LINE_LENGTH 512

static opal_list_t orte_rds_resfile_resource_list;

static int orte_rds_resfile_parse_site(char *site, FILE *fp);

static int orte_rds_resfile_parse_resource(orte_rds_cell_desc_t *cell, FILE *fp);

static int orte_rds_resfile_parse_resource(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line;
    bool name_given = false;
    int rc;
    orte_rds_cell_attr_t *na;

    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        /* check for end of resource description */
        if (0 == strncmp(line, "</resource", strlen("</resource"))) {
            return ORTE_SUCCESS;
        }
        /* check for attribute and redirect to proper parser */
        if (0 == strncmp(line, "<name", strlen("<name"))) {
            /* check to ensure only ONE name is given */
            if (name_given) {
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                return ORTE_ERR_BAD_PARAM;
            }
            name_given = true;
            if (NULL == (cell->name = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            /* get new cellid for this site/resource */
            if (ORTE_SUCCESS != (rc = orte_ns.create_cellid(&(cell->cellid), cell->site, cell->name))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            na = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == na) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.key = strdup(ORTE_RDS_NAME);
            na->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == na->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.value->type = ORTE_STRING;
            na->keyval.value->data = strdup(cell->name);
            opal_list_append(&(cell->attributes), &na->super);

            na = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == na) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.key = strdup(ORTE_CELLID_KEY);
            na->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == na->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.value->type = ORTE_CELLID;
            if (ORTE_SUCCESS != (rc = orte_dss.copy(&(na->keyval.value->data), &(cell->cellid), ORTE_CELLID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            opal_list_append(&(cell->attributes), &na->super);

        } else if (0 == strncmp(line, "<type", strlen("<type"))) {
            if (NULL == (cell->type = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            na = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == na) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.key = strdup(ORTE_RDS_TYPE);
            na->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == na->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.value->type = ORTE_STRING;
            na->keyval.value->data = strdup(cell->type);
            opal_list_append(&(cell->attributes), &na->super);

        } else if (0 == strncmp(line, "<front-end", strlen("<front-end"))) {
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_fe(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        } else if (0 == strncmp(line, "<compute-domains", strlen("<compute-domains"))) {
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_cd(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        } else if (0 == strncmp(line, "<os", strlen("<os"))) {
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_os(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        } else if (0 == strncmp(line, "<filesystem", strlen("<filesystem"))) {
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_fs(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        } else if (0 == strncmp(line, "<allocator", strlen("<allocator"))) {
            na = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == na) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.key = strdup(ORTE_RDS_ALLOCATOR);
            na->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == na->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.value->type = ORTE_STRING;
            if (NULL == (na->keyval.value->data = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            opal_list_append(&(cell->attributes), &na->super);

        } else if (0 == strncmp(line, "<launcher", strlen("<launcher"))) {
            na = OBJ_NEW(orte_rds_cell_attr_t);
            if (NULL == na) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.key = strdup(ORTE_RDS_LAUNCHER);
            na->keyval.value = OBJ_NEW(orte_data_value_t);
            if (NULL == na->keyval.value) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            na->keyval.value->type = ORTE_STRING;
            if (NULL == (na->keyval.value->data = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            opal_list_append(&(cell->attributes), &na->super);

        } else if (0 == strncmp(line, "<sequence", strlen("<sequence"))) {
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_se(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        } else if (0 == strncmp(line, "<arch", strlen("<arch"))) {
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_na(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}


static int orte_rds_resfile_parse_site(char *site, FILE *fp)
{
    char *line;
    orte_rds_cell_desc_t *cell;
    int rc;

    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "<resource", strlen("<resource"))) {
            cell = OBJ_NEW(orte_rds_cell_desc_t);
            if (NULL == cell) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            cell->site = strdup(site);
            /* parse the resource description */
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_resource(cell, fp))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

            opal_list_append(&orte_rds_resfile_resource_list, &cell->super);
        }
    }
    return ORTE_SUCCESS;
}


int orte_rds_resfile_query(orte_jobid_t job)
{
    int fileid, rc;
    FILE *fp;
    char *input_line, *site;

    if (orte_rds_resfile_queried) {
        /* if we have previously been queried, then our info
         * is already on the registry, so just return
         */
        return ORTE_SUCCESS;
    }
    
    OPAL_LOCK(&mca_rds_resfile_component.lock);

    orte_rds_resfile_queried = true;

    /* get the resource filename */
    fileid = mca_base_param_find("rds", "resfile", "name");
    mca_base_param_lookup_string(fileid, &mca_rds_resfile_component.filename);

    if (NULL == mca_rds_resfile_component.filename) {  /* no resource file provided */
        /* DO NOT ORTE_ERROR_LOG OR RETURN AN ERROR - THIS IS NOT AN ERROR CONDITION */
       OPAL_UNLOCK(&mca_rds_resfile_component.lock);
       return ORTE_SUCCESS;
    }

    /* open the resource file */
    fp = fopen(mca_rds_resfile_component.filename, "r");
    if (NULL == fp) {
       ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
       OPAL_UNLOCK(&mca_rds_resfile_component.lock);
       return ORTE_ERR_NOT_FOUND;
    }

    /* setup the resource list */
    OBJ_CONSTRUCT(&orte_rds_resfile_resource_list, opal_list_t);

    /* dump the initial line containing the DOM */
    input_line = orte_rds_resfile_getline(fp);
    if (NULL == input_line || 0 != strncmp(input_line, "<?xml", 5)) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    }
    free(input_line);

    /* parse file */
    while (NULL != (input_line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(input_line, "<site-name>", strlen("<site-name>"))) {
            /* extract the site name from input_line */
            if (NULL == (site = orte_rds_resfile_parse_field(input_line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(input_line);
                rc = ORTE_ERR_FILE_READ_FAILURE;
                goto CLEANUP;
            }
            if (ORTE_SUCCESS != (rc = orte_rds_resfile_parse_site(site, fp))) {
                ORTE_ERROR_LOG(rc);
                free(input_line);
                goto CLEANUP;
            }
        }
        free(input_line);
    }

    /* place resource list on registry */
    if (ORTE_SUCCESS != (rc = orte_rds_base_store_resource(&orte_rds_resfile_resource_list))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

CLEANUP:
    fclose(fp);
    OBJ_DESTRUCT(&orte_rds_resfile_resource_list);

    OPAL_UNLOCK(&mca_rds_resfile_component.lock);

    return ORTE_SUCCESS;
}


char *orte_rds_resfile_getline(FILE *fp)
{
    int i;
    char *ret, *buff, *start;
    char input[ORTE_RDS_RESFILE_MAX_LINE_LENGTH];

    /* find the next non-blank line, stop at end-of-file */
    ret = fgets(input, ORTE_RDS_RESFILE_MAX_LINE_LENGTH, fp);
    while (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        /* strip leading whitespace */
        for (i=0; i < (int)strlen(input) && (input[i] == ' ' || input[i] == '\t'); i++);
        if (i < (int)strlen(input)) {
            start = &input[i];
            buff = strdup(start);
            return buff;
        }
        ret = fgets(input, ORTE_RDS_RESFILE_MAX_LINE_LENGTH, fp);
    }

    return NULL;
}


char *orte_rds_resfile_parse_field(char *input)
{
    char *start, *stop, *ans;

    start = strchr(input, '>');
    if (NULL == start) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        return NULL;
    }
    start++;
    stop = strchr(start, '<');
    if (NULL == stop) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        return NULL;
    }
    *stop = '\0'; /* terminate the string */

    if (start == stop) {
        return NULL;
    }

    ans = strdup(start);
    return ans;
}
