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

#include "opal_config.h"

#include <string.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"


/*
 * Function for comparing two mca_base_component_priority_t structs so
 * that we can build prioritized listss of them.  This assumed that
 * the types of the modules are the same.  Sort first by priority,
 * second by module name, third by module version.
 *
 * Note that we acutally want a *reverse* ordering here -- the al_*
 * functions will put "smaller" items at the head, and "larger" items
 * at the tail.  Since we want the highest priority at the head, it
 * may help the gentle reader to consider this an inverse comparison.
 * :-)
 */
int
mca_base_component_compare_priority(mca_base_component_priority_list_item_t *a,
                                    mca_base_component_priority_list_item_t *b)
{
  /* First, compare the priorties */

  if (a->cpli_priority > b->cpli_priority) {
    return -1;
  } else if (a->cpli_priority < b->cpli_priority) {
    return 1;
  } else {
    return mca_base_component_compare(a->super.cli_component,
                                      b->super.cli_component);
  }
}


int mca_base_component_compare(const mca_base_component_t* aa,
                               const mca_base_component_t* bb)
{
    int val;

    val = strncmp(aa->mca_type_name, bb->mca_type_name,
                  MCA_BASE_MAX_TYPE_NAME_LEN);
    if (val != 0) {
      return -val;
    }

    val = strncmp(aa->mca_component_name, bb->mca_component_name,
                      MCA_BASE_MAX_COMPONENT_NAME_LEN);
    if (val != 0) {
      return -val;
    }

    /* The names were equal, so compare the versions */

    if (aa->mca_component_major_version >
        bb->mca_component_major_version) {
      return -1;
    } else if (aa->mca_component_major_version <
               bb->mca_component_major_version) {
      return 1;
    } else if (aa->mca_component_minor_version >
               bb->mca_component_minor_version) {
      return -1;
    } else if (aa->mca_component_minor_version <
               bb->mca_component_minor_version) {
      return 1;
    } else if (aa->mca_component_release_version >
               bb->mca_component_release_version) {
      return -1;
    } else if (aa->mca_component_release_version <
               bb->mca_component_release_version) {
      return 1;
    }

    return 0;
}


/**
 * compare but exclude the release version - declare compatible
 * if the major/minor version are the same.
 */
int mca_base_component_compatible(
    const mca_base_component_t* aa,
    const mca_base_component_t* bb)
{
    int val;

    val = strncmp(aa->mca_type_name, bb->mca_type_name,
                  MCA_BASE_MAX_TYPE_NAME_LEN);
    if (val != 0) {
      return -val;
    }

    val = strncmp(aa->mca_component_name, bb->mca_component_name,
                  MCA_BASE_MAX_COMPONENT_NAME_LEN);
    if (val != 0) {
      return -val;
    }

    /* The names were equal, so compare the versions */

    if (aa->mca_component_major_version >
        bb->mca_component_major_version) {
      return -1;
    } else if (aa->mca_component_major_version <
               bb->mca_component_major_version) {
      return 1;
    } else if (aa->mca_component_minor_version >
               bb->mca_component_minor_version) {
      return -1;
    } else if (aa->mca_component_minor_version <
               bb->mca_component_minor_version) {
      return 1;
    }
    return 0;
}

/**
 * Returns a string which represents the component name and version.
 * Has the form: comp_type.comp_name.major_version.minor_version
 */
char * mca_base_component_to_string(const mca_base_component_t *a) {
    char * str = NULL;
    if(0 > asprintf(&str, "%s.%s.%d.%d", a->mca_type_name,
                    a->mca_component_name, a->mca_component_major_version,
                    a->mca_component_minor_version)) {
        return NULL;
    }
    return str;
}

