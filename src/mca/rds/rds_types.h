/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_MCA_RDS_TYPES_H
#define ORTE_MCA_RDS_TYPES_H

/* type of resource (e.g., "cluster") */
#define ORTE_RDS_TYPE               "orte-rds-type"

/* the name of a machine operating as the frontend of a resource */
#define ORTE_RDS_FE_NAME            "orte-rds-fe-name"
/* location of the temporary directory on the frontend - required because
 * some machines do not allow the frontend to access scratch filesystem space */
#define ORTE_RDS_FE_TMP             "orte-rds-fe-tmpdir"    

/* node architecture info */
#define ORTE_RDS_NUM_NODES          "orte-rds-arch-num-nodes"
#define ORTE_RDS_CPUS_NODE          "orte-rds-arch-cpus-per-node"
#define ORTE_RDS_CPU_TYPE           "orte-rds-arch-cpu-type"
#define ORTE_RDS_CPU_VENDOR         "orte-rds-arch-cpu-vendor"
#define ORTE_RDS_CPU_SPEED          "orte-rds-arch-cpu-speed"    /* floating pt number in MHz */
#define ORTE_RDS_NODE_MEM           "orte-rds-arch-mem-per-node" /* floating pt number in GBytes */
#define ORTE_RDS_NODE_CACHE         "orte-rds-arch-node-cache"   /* floating pt number in MBytes */
#define ORTE_RDS_INT_TYPE           "orte-rds-arch-interconnect-type"
#define ORTE_RDS_INT_RAILS          "orte-rds-arch-interconnect-num-rails"
#define ORTE_RDS_INT_SW             "orte-rds-arch-interconnect-sw"
#define ORTE_RDS_INT_SW_VERS        "orte-rds-arch-interconnect-sw-version"
#define ORTE_RDS_MAX_BW             "orte-rds-arch-interconnect-max-bw" /* floating pt number in Mbits/second */
/* Some nodes in a system may be configured in a custom fashion. At this time, we
 * aren't sure how to use this info. For now, this key will allow us to "flag" those
 * nodes that are configured differently, although we won't save the custom config
 * info until we figure out how to do so. The custom node key, therefore, will be
 * associated with a string nodename to indicate the customized node
 */
#define ORTE_RDS_CUSTOM_NODE        "orte-rds-arch-custom-node"

/* operating system used on nodes within the resource */
#define ORTE_RDS_OS_TYPE            "orte-rds-os-type"
#define ORTE_RDS_OS_VENDOR          "orte-rds-os-vendor"
#define ORTE_RDS_OS_VERSION         "orte-rds-os-version"

/* filesystem type (e.g., "panasys") */
#define ORTE_RDS_FS_TYPE            "orte-rds-fs-type"
/* home directory for users */
#define ORTE_RDS_FS_HOME            "orte-rds-fs-home"
/* scratch directory */
#define ORTE_RDS_FS_SCRATCH         "orte-rds-fs-scratch-dir"
/* size of the scratch filesystem */
#define ORTE_RDS_FS_SCRATCH_SIZE    "orte-rds-fs-scratch-size"
/* number of domains in the filesystem */
#define ORTE_RDS_FS_DOMAINS         "orte-rds-fs-num-domains"
/* number of nodes in each domain */
#define ORTE_RDS_FS_NODES_DOMAIN    "orte-rds-fs-nodes-per-domain"
/* the "give" directory - used in some secure systems to share a file with
 * another user
 */
#define ORTE_RDS_FS_GIVE            "orte-rds-fs-give-dir"

/* allocator and launcher info */
#define ORTE_RDS_ALLOCATOR      "orte-rds-allocator"
#define ORTE_RDS_LAUNCHER       "orte-rds-launcher"

/* operational limits */
#define ORTE_RDS_MAX_PROCS_CPU  "orte-rds-max-procs-per-cpu"
#define ORTE_RDS_MAX_PROCS_NODE "orte-rds-max-procs-per-node"


/*
 * SEARCH FLAGS
 * These defined flags are to be used only for search operations. They will return
 * all information from the specified site/resource that pertains to a particular
 * subject area. For example, a registry "get" operation that specified tokens
 * of "lanl" and "pink", and a key of "ORTE_RDS_FILESYSTEM", would return all info
 * on the filesystem attached to the pink cluster at lanl.
 */
#define ORTE_RDS_FRONT_END      "orte-rds-fe*"            /* info on frontend systems */
#define ORTE_RDS_OS             "orte-rds-os*"            /* operating system info */
#define ORTE_RDS_FILESYSTEM     "orte-rds-fs*"            /* filesystem info */
#define ORTE_RDSD_NODE_ARCH     "orte-rds-arch*"          /* architecture info */
#define ORTE_RDS_INTERCONNECT   "orte-rds-arch-interconnect*"
#define ORTE_RDS_CPU            "orte-rds-arch-cpu*"

#endif
