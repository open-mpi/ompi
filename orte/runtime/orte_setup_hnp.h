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
 * Establish a Head Node Process on a cluster's front end
 */

#ifndef ORTE_SETUP_HNP_H
#define ORTE_SETUP_HNP_H

/*
 * Local data structure
 */
typedef struct {
    char *target_cluster;
    char *headnode;
    orte_process_name_t *name;
    orte_jobid_t jobid;
} orte_setup_hnp_cb_data_t;


    /**
     * Establish a Head Node Process on a cluster's front end
     */
ORTE_DECLSPEC int orte_setup_hnp(char *target_cluster, char *headnode, char *username);

#endif
