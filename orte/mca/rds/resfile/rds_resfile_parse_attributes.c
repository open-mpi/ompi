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

#include <stdio.h>
#include <string.h>


#include "include/orte_constants.h"
#include "util/output.h"
#include "mca/errmgr/errmgr.h"

#include "mca/rds/resfile/rds_resfile.h"

int orte_rds_resfile_parse_fe(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line, *ssh;
    orte_rds_cell_attr_t *na;
    
    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "</front-end", strlen("</front-end"))) {
            return ORTE_SUCCESS;
        }
        na = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == na) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (0 == strncmp(line, "<name", strlen("<name"))) {
            na->keyval.key = strdup(ORTE_RDS_FE_NAME);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else if (0 == strncmp(line, "<tmp-dir", strlen("<tmp-dir"))) {
            na->keyval.key = strdup(ORTE_RDS_FE_TMP);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else if (0 == strncmp(line, "<ssh", strlen("<ssh"))) {
            na->keyval.key = strdup(ORTE_RDS_FE_SSH);
            na->keyval.type = ORTE_BOOL;
            if (NULL == (ssh = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            if (0 == strncmp(ssh, "true", 4)) {
                na->keyval.value.tf_flag = true;
            } else {
                na->keyval.value.tf_flag = false;
            }
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
        }
        opal_list_append(&(cell->attributes), &na->super);
    }
    
    return ORTE_SUCCESS;
}

int orte_rds_resfile_parse_cd(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line, *tmp;
    orte_rds_cell_attr_t *na;
    
    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "</compute-domains", strlen("</compute-domains"))) {
            free(line);
            return ORTE_SUCCESS;
        }
        na = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == na) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(line);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (0 == strncmp(line, "<num-domains", strlen("<num-domains"))) {
            na->keyval.key = strdup(ORTE_RDS_COMP_NUM_DOMAINS);
            na->keyval.type = ORTE_INT16;
            if (NULL == (tmp = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            na->keyval.value.i16 = 13; /* strtoi(tmp); */
        } else if (0 == strncmp(line, "<nodes-per-domain", strlen("<nodes-per-domain"))) {
            na->keyval.key = strdup(ORTE_RDS_COMP_NODES_DOMAIN);
            na->keyval.type = ORTE_INT16;
            if (NULL == (tmp = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            na->keyval.value.i16 = 13; /*strtoi(tmp); */
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(line);
            return ORTE_ERR_BAD_PARAM;
        }
        opal_list_append(&(cell->attributes), &na->super);
        free(line);
    }
    
    return ORTE_SUCCESS;
}

int orte_rds_resfile_parse_os(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line;
    orte_rds_cell_attr_t *na;
    
    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "</os", strlen("</os"))) {
            free(line);
            return ORTE_SUCCESS;
        }
        na = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == na) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(line);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (0 == strncmp(line, "<type", strlen("<type"))) {
            na->keyval.key = strdup(ORTE_RDS_OS_TYPE);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else if (0 == strncmp(line, "<vendor", strlen("<vendor"))) {
            na->keyval.key = strdup(ORTE_RDS_OS_VENDOR);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else if (0 == strncmp(line, "<version", strlen("<version"))) {
            na->keyval.key = strdup(ORTE_RDS_OS_VERSION);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(line);
            return ORTE_ERR_BAD_PARAM;
        }
        opal_list_append(&(cell->attributes), &na->super);
        free(line);
    }
    
    return ORTE_SUCCESS;
}

int orte_rds_resfile_parse_fs(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line;
/*    orte_rds_cell_attr_t *na; */
    
    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "</filesystem", strlen("</filesystem"))) {
            free(line);
            return ORTE_SUCCESS;
        }
#if 0
        na = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == na) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(line);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (0 == strncmp(line, "<type", strlen("<type"))) {
            na->keyval.key = strdup(ORTE_RDS_FS_TYPE);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else if (0 == strncmp(line, "<home-root", strlen("<home-root"))) {
            na->keyval.key = strdup(ORTE_RDS_FS_HOME);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
        } else if (0 == strncmp(line, "<scratch", strlen("<scratch"))) {
            na->keyval.key = strdup(ORTE_RDS_FS_SCRATCH);
            na->keyval.type = ORTE_STRING;
            if (NULL == (na->keyval.value.strptr = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            } else if (0 == strncmp(line, "<num-domains", strlen("<num-domains"))) {
            na->keyval.key = strdup(ORTE_RDS_FS_DOMAINS);
            na->keyval.type = ORTE_INT16;
            if (NULL == (tmp = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            na->keyval.value.i16 = 13; /* strtoi(tmp); */
        } else if (0 == strncmp(line, "<nodes-per-domain", strlen("<nodes-per-domain"))) {
            na->keyval.key = strdup(ORTE_RDS_FS_NODES_DOMAIN);
            na->keyval.type = ORTE_INT16;
            if (NULL == (tmp = orte_rds_resfile_parse_field(line))) {
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
                free(line);
                return ORTE_ERR_FILE_READ_FAILURE;
            }
            na->keyval.value.i16 = 13; /*strtoi(tmp); */
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(line);
            return ORTE_ERR_BAD_PARAM;
        }
        opal_list_append(&(cell->attributes), &na->super);
#endif
        free(line);
    }
    
    return ORTE_SUCCESS;
}

int orte_rds_resfile_parse_se(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line;
    orte_rds_cell_attr_t *na;
    char *tmp;
    
    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "</sequence", strlen("</sequence"))) {
            free(line);
            return ORTE_SUCCESS;
        }
        na = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == na) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(line);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        na->keyval.key = strdup(ORTE_RDS_ALLOCATION_SEQUENCE);
        na->keyval.type = ORTE_INT16;
        if (NULL == (tmp = orte_rds_resfile_parse_field(line))) {
            ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
            free(line);
            return ORTE_ERR_FILE_READ_FAILURE;
        }
        na->keyval.value.i16 = 13; /*strtoi(tmp); */
        free(line);
    }
    
    return ORTE_SUCCESS;
}

int orte_rds_resfile_parse_na(orte_rds_cell_desc_t *cell, FILE *fp)
{
    char *line;
/*    orte_rds_cell_attr_t *na; */
    
    while (NULL != (line = orte_rds_resfile_getline(fp))) {
        if (0 == strncmp(line, "</arch", strlen("</arch"))) {
            free(line);
            return ORTE_SUCCESS;
        }
        free(line);
    }
    
    return ORTE_SUCCESS;
}

