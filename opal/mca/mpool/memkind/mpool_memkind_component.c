/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H*/
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#include <errno.h>
#include <memkind.h>
#include "opal/mca/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "mpool_memkind.h"


/*
 * Local functions
 */

static int
mca_mpool_memkind_register(void);

static int
mca_mpool_memkind_open(void);

static int
mca_mpool_memkind_close(void);

static mca_mpool_base_module_t*
mca_mpool_memkind_init(struct mca_mpool_base_resources_t* resources);


mca_mpool_memkind_component_t mca_mpool_memkind_component = {
    {
        /* First, the mca_base_component_t struct containing meta
         information about the component itself */
        {
          MCA_MPOOL_BASE_VERSION_2_0_0,
          "memkind", /* MCA component name */
          OPAL_MAJOR_VERSION,  /* MCA component major version */
          OPAL_MINOR_VERSION,  /* MCA component minor version */
          OPAL_RELEASE_VERSION,  /* MCA component release version */
          mca_mpool_memkind_open,  /* component open  */
          mca_mpool_memkind_close,
          NULL,
          mca_mpool_memkind_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

       mca_mpool_memkind_init
    }
};


static int opal_mpool_memkind_verbose;
static int mca_mpool_memkind_register(void)
{

    char *memkind_names;
    /* register MEMKIND component parameters */
    mca_mpool_memkind_component.hbw = 1;
    mca_mpool_memkind_component.bind = 0;
    mca_mpool_memkind_component.pagesize = mca_mpool_memkind_default_pagesize;

    memkind_names = (char *) malloc (2048 * sizeof(char));
    if (NULL == memkind_names){
      return OPAL_ERROR;
    }

    mca_mpool_memkind_component.memkind_name = (char *) malloc (4096 * sizeof(char));
    if (NULL == mca_mpool_memkind_component.memkind_name){
      return OPAL_ERROR;
    }

    mca_mpool_memkind_component.memkind_file = (char *) malloc (4096 * sizeof(char));
    if (NULL ==mca_mpool_memkind_component.memkind_file){
      return OPAL_ERROR;
    }

    strncpy (mca_mpool_memkind_component.memkind_name, "memkind_default", 17);
    sprintf(memkind_names, "Use a specific kind of memory from (memkind_default, memkind_hugetlb, memkind_hbw, memkind_hbw_preferred, memkind_hbw_hugetlb, memkind_hbw_preferred_hugetlb, memkind_hbw_gbtlb memkind_hbw_preferred_gbtlb, memkind_gbtlb)");

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,"high_bandwidth",
                                           "Allocate in high bandwidth node (0-> no, 1 -> yes)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_memkind_component.hbw);

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,"page_size",
                                           "Allocate with different page size (4096, 2097152, 1073741824)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_memkind_component.pagesize);

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version, "bind",
                                           "Bind allocations to specific nodes (0->preferred, 1-> bind)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_memkind_component.bind);

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "verbose", "Enable verbose output for mpool memkind component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &opal_mpool_memkind_verbose);

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "name",
                                           memkind_names,
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_memkind_component.memkind_name);

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "config",
                                           "Config file user defined hints",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_mpool_memkind_component.memkind_file);


    opal_output(mca_mpool_memkind_component.verbose,
                "mca_mpool_memkind_register: Allocating with hbw: %d, pagesize: %d",
                mca_mpool_memkind_component.hbw, mca_mpool_memkind_component.pagesize);

    /*Setting the appropriate based on mca parameters
      default allocates with 4K pages on high bandwidth memory on preferred mode.
      For more information about the kinds, (refer to the manpage of memkind)
      www.github.com/memkind
     */
    if (!mca_mpool_memkind_component.hbw) {
        switch (mca_mpool_memkind_component.pagesize) {
        case 4096:
            mca_mpool_memkind_component.kind = MEMKIND_DEFAULT;
            break;
        case 2097152:
            mca_mpool_memkind_component.kind = MEMKIND_HUGETLB;
            break;
        case 1073741824:
            mca_mpool_memkind_component.kind = MEMKIND_GBTLB;
            break;
        }
    }
    else if (1 == mca_mpool_memkind_component.hbw){
        switch(mca_mpool_memkind_component.pagesize) {
        case 4096:
            mca_mpool_memkind_component.kind = MEMKIND_HBW;
            break;
        case 2097152:
            mca_mpool_memkind_component.kind = MEMKIND_HBW_HUGETLB;
            break;
        case 1073741824:
            mca_mpool_memkind_component.kind = MEMKIND_HBW_GBTLB;
            break;
        }
    }
    else {
        switch (mca_mpool_memkind_component.pagesize) {
        case 4096:
            mca_mpool_memkind_component.kind = MEMKIND_HBW_PREFERRED;
            break;
        case 2097152:
            mca_mpool_memkind_component.kind = MEMKIND_HBW_PREFERRED_HUGETLB;
            break;
        case 1073741824:
            mca_mpool_memkind_component.kind = MEMKIND_HBW_PREFERRED_GBTLB;
            break;
        }
    }

    if(NULL != memkind_names){
      free (memkind_names);
      memkind_names = NULL;
    }

    return OPAL_SUCCESS;
}

/**
  * component open/close/init function
  */
static int mca_mpool_memkind_open(void)
{
    if (opal_mpool_memkind_verbose != 0) {
        mca_mpool_memkind_component.verbose = opal_output_open(NULL);
    }
    else {
        mca_mpool_memkind_component.verbose = -1;
    }

    return OPAL_SUCCESS;
}

static int mca_mpool_memkind_close(void)
{
    return OPAL_SUCCESS;
}

static mca_mpool_base_module_t*
mca_mpool_memkind_init(struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_memkind_module_t* mpool_module;
    mca_mpool_base_module_t* ret_val;

    /*Check if the high bandwidth node is available ?*/
    if (!memkind_check_available(MEMKIND_HBW)) {
        opal_output(mca_mpool_memkind_component.verbose,
                    "mca_mpool_memkind_init: High bandwidth node not available");
        ret_val = NULL;
    }
    else {
        mpool_module =
            (mca_mpool_memkind_module_t*)malloc(sizeof(mca_mpool_memkind_module_t));
        mca_mpool_memkind_module_init(mpool_module);

        mpool_module->alloc_size = resources->size;
        opal_output(mca_mpool_memkind_component.verbose,
                    "mca_mpool_memkind_init: allocation size requested: (%ld)",
                    mpool_module->alloc_size);

        ret_val = &mpool_module->super;
    }

    return ret_val;
}
