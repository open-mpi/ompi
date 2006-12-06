/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
/**
 * @file
 *
 * Resource Discovery (Hostfile)
 */
#ifndef ORTE_RDS_RESFILE_H
#define ORTE_RDS_RESFILE_H

#include "orte_config.h"

#include "opal/threads/mutex.h"

#include "orte/mca/rds/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * RDS Resource file functions
 */
int orte_rds_resfile_query(orte_jobid_t job);

int orte_rds_resfile_finalize(void);

/* RDS resource file internal functions */
char *orte_rds_resfile_getline(FILE *fp);

char *orte_rds_resfile_parse_field(char *input);

/* RDS resource file attribute parsers */
int orte_rds_resfile_parse_fe(orte_rds_cell_desc_t *cell, FILE *fp);

int orte_rds_resfile_parse_cd(orte_rds_cell_desc_t *cell, FILE *fp);

int orte_rds_resfile_parse_os(orte_rds_cell_desc_t *cell, FILE *fp);

int orte_rds_resfile_parse_fs(orte_rds_cell_desc_t *cell, FILE *fp);

int orte_rds_resfile_parse_se(orte_rds_cell_desc_t *cell, FILE *fp);

int orte_rds_resfile_parse_na(orte_rds_cell_desc_t *cell, FILE *fp);

/**
 * RDS Resource file Component 
 */
struct orte_rds_resfile_component_t {
    orte_rds_base_component_t super;
    int debug;
    char *filename;
    opal_mutex_t lock;
};
typedef struct orte_rds_resfile_component_t orte_rds_resfile_component_t;

ORTE_MODULE_DECLSPEC extern orte_rds_resfile_component_t mca_rds_resfile_component;
extern orte_rds_base_module_t orte_rds_resfile_module;

extern bool orte_rds_resfile_queried;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
