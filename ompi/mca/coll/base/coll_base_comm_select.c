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

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"


/*
 * Local variables
 */
static int basic_priority = -1;
static mca_coll_base_module_1_0_0_t null_module = {

  /* Module init and finalize */

  NULL, NULL,

  /* Collective function pointers */

  NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL
};


/*
 * Local types
 */
struct avail_coll_t {
    opal_list_item_t super;

    int ac_priority;
    const mca_coll_base_component_1_0_0_t *ac_component;
    const mca_coll_base_module_1_0_0_t *ac_module;
    struct mca_coll_base_comm_t *ac_data;
};
typedef struct avail_coll_t avail_coll_t;


/*
 * Local functions
 */
static opal_list_t *check_components(opal_list_t *components, 
                                     ompi_communicator_t *comm, 
                                     char **names, int num_names);
static int check_one_component(ompi_communicator_t *comm, 
                               const mca_base_component_t *component,
                               const mca_coll_base_module_1_0_0_t **module,
                               struct mca_coll_base_comm_t **data);

static int query(const mca_base_component_t *component, 
                 ompi_communicator_t *comm, int *priority,
                 const mca_coll_base_module_1_0_0_t **module,
                 struct mca_coll_base_comm_t **data);
static int query_1_0_0(const mca_coll_base_component_1_0_0_t *coll_component, 
                       ompi_communicator_t *comm, int *priority,
                       const mca_coll_base_module_1_0_0_t **module,
                       struct mca_coll_base_comm_t **data);

static void unquery(const mca_coll_base_component_1_0_0_t *coll_component,
                    ompi_communicator_t *comm,
                    struct mca_coll_base_comm_t *data);
static void unquery_1_0_0(const mca_coll_base_component_1_0_0_t *coll_component, 
                          ompi_communicator_t *comm,
                          struct mca_coll_base_comm_t *data);

static int module_init(const mca_coll_base_module_1_0_0_t *module, 
                       ompi_communicator_t *comm);

static int query_basic(ompi_communicator_t *comm);
static int replace_null_with_basic(ompi_communicator_t *comm);


/*
 * Stuff for the OBJ interface
 */
static OBJ_CLASS_INSTANCE(avail_coll_t, opal_list_item_t, NULL, NULL);


/*
 * This function is called at the initialization time of every
 * communicator.  It is used to select which coll component will be
 * active for a given communicator.
 *
 * This selection logic is not for the weak.
 */
int mca_coll_base_comm_select(ompi_communicator_t *comm, 
                              mca_base_component_t *preferred)
{
  bool found, using_basic;
  int err, num_names;
  char name[MPI_MAX_OBJECT_NAME + 32];
  char *names, **name_array;
  char *str;
  avail_coll_t *avail;
  opal_list_t *selectable;
  opal_list_item_t *item;
  const mca_coll_base_component_1_0_0_t *selected_component, *component;
  const mca_coll_base_module_1_0_0_t *selected_module;
  struct mca_coll_base_comm_t *selected_data;

  /* Announce */

  snprintf(name, sizeof(name), "%s (cid %d)", comm->c_name, 
           comm->c_contextid);
  name[sizeof(name) - 1] = '\0';
  opal_output_verbose(10, mca_coll_base_output,
                      "coll:base:comm_select: new communicator: %s", 
                      name);
  
  /* Initialize all the relevant pointers, since they're used as
     sentinel values */
  
  comm->c_coll = null_module;

  comm->c_coll_selected_component = NULL;
  comm->c_coll_selected_data = NULL;
  comm->c_coll_selected_module = NULL;

  comm->c_coll_basic_data = NULL;
  comm->c_coll_basic_module = NULL;
  
  /* See if a set of component was requested by the MCA parameter.
     Don't check for error. */

  names = NULL;
  mca_base_param_lookup_string(mca_coll_base_param, &names);

  /* Compute the intersection of all of my available components with
     the components from all the other processes in this
     communicator */

  /* JMS CONTINUE HERE */

  /* See if a preferred component was provided.  If so, try to select
     it.  If we don't succeed, fall through and do a normal
     selection. */

  err = OMPI_ERROR;
  if (NULL != preferred) {
    str = &(preferred->mca_component_name[0]);

    opal_output_verbose(10, mca_coll_base_output, 
                       "coll:base:comm_select: Checking preferred module: %s",
                       str);
    selectable = check_components(&mca_coll_base_components_available, 
                                  comm, &str, 1);
    
    /* If we didn't get a preferred module, then call again without a
       preferred module.  This makes the logic below dramatically
       simpler. */
    
    if (NULL == selectable) {
      return mca_coll_base_comm_select(comm, NULL);
    }

    /* We only fall through here if we were able to select one of the
       preferred modules */
  }

  /* If there was no preferred module, then see if there were any listed
     in the MCA parameter; parse them and check them all */

  else if (NULL != names && 0 < strlen(names)) {
    name_array = opal_argv_split(names, ',');
    num_names = opal_argv_count(name_array);

    opal_output_verbose(10, mca_coll_base_output, 
                       "coll:base:comm_select: Checking specific modules: %s",
                       names);
    selectable = check_components(&mca_coll_base_components_available, 
                                  comm, name_array, num_names);
    opal_argv_free(name_array);
  }

  /* Nope -- a specific [set of] component[s] was not requested.  Go
     check them all. */
  
  else {
    opal_output_verbose(10, mca_coll_base_output, 
                       "coll:base:comm_select: Checking all available modules");
    selectable = check_components(&mca_coll_base_components_available, 
                                  comm, NULL, 0);
  }

  /* Upon return from the above, the modules list will contain the
     list of modules that returned (priority >= 0).  If we have no
     collective modules available, then use the basic component */

  if (NULL == selectable) {
    found = false;
    if (NULL != mca_coll_base_basic_component) {
      query_basic(comm);
      if (NULL != comm->c_coll_basic_module) {
        found = true;
      }
    }

    if (!found) {
      /* There's no modules available -- including basic.  Doh! */
      opal_show_help("help-mca-coll-base",
                     "comm-select:none-available", true);
      return OMPI_ERROR;
    }
  }

  /* Do some kind of collective operation to find a module that
     everyone has available */

#if 1
  /* For the moment, just take the top module off the list */

  if (NULL != selectable) {
    using_basic = false;
    item = opal_list_remove_first(selectable);
    avail = (avail_coll_t *) item;

    /* Check to see if the basic component has a higher priority than
       the highest priority component on the selectable list.  If so,
       use basic. */

    if (NULL != mca_coll_base_basic_component) {
        query_basic(comm);
    }
    if (avail->ac_priority > basic_priority) {
        selected_component = avail->ac_component;
        selected_module = avail->ac_module;
        selected_data = avail->ac_data;
        OBJ_RELEASE(avail);
    } else {
        opal_output_verbose(10, mca_coll_base_output,
                            "coll:base:comm_select: component available: basic, priority: %d\n", basic_priority);
        using_basic = true;
        selected_component = mca_coll_base_basic_component;
        selected_module = comm->c_coll_basic_module;
        selected_data = comm->c_coll_basic_data;
    }
  } else {
    using_basic = true;
    selected_component = mca_coll_base_basic_component;
    selected_module = comm->c_coll_basic_module;
    selected_data = comm->c_coll_basic_data;
  }
#else
  /* JMS CONTINUE HERE */
#endif

  /* Everything left in the selectable list is therefore unwanted,
     and we call their unquery() method (because they all had query()
     invoked, but will never have init() invoked in this scope). */

  if (NULL != selectable) {
    for (item = opal_list_remove_first(selectable); item != NULL;
         item = opal_list_remove_first(selectable)) {
      avail = (avail_coll_t *) item;
      component = avail->ac_component;
      unquery(component, comm, avail->ac_data);
      OBJ_RELEASE(avail);
    }
    OBJ_RELEASE(selectable);
  }

  /* If we're not using the basic module, then set it up, replace all
     NULL function pointers with those from basic, and then initialize
     it. */

  comm->c_coll_selected_component = selected_component;
  comm->c_coll_selected_module = selected_module;
  comm->c_coll_selected_data = selected_data;
  if (!using_basic) {
    comm->c_coll = *selected_module;
    replace_null_with_basic(comm);

    /* Finally -- intialize the selected module.  If it's the basic
       module, we've initialized it already. */

    err = module_init(selected_module, comm);
    if (OMPI_SUCCESS != err) {
      return err;
    }

    /* Now double check because we may have gotten a different module
       back from the init function; ensure that there are no NULL's in
       there */

    replace_null_with_basic(comm);
  }

  /* Announce the winner */
  
  opal_output_verbose(10, mca_coll_base_output,
                     "coll:base:comm_select: Selected coll module %s", 
                     selected_component->collm_version.mca_component_name);
  
  return OMPI_SUCCESS;
}


/*
 * For each module in the list, if it is in the list of names (or the
 * list of names is NULL), then check and see if it wants to run, and
 * do the resulting priority comparison.  Make a list of modules to be
 * only those who returned that they want to run, and put them in
 * priority order.
 */
static opal_list_t *check_components(opal_list_t *components, 
                                     ompi_communicator_t *comm, 
                                     char **names, int num_names)
{
  int i, priority;
  const mca_base_component_t *component;
  opal_list_item_t *item, *item2;
  const mca_coll_base_module_1_0_0_t *module;
  bool want_to_check;
  opal_list_t *selectable;
  avail_coll_t *avail, *avail2;
  struct mca_coll_base_comm_t *data;
  
  /* Make a list of the components that query successfully */

  selectable = OBJ_NEW(opal_list_t);

  /* Scan through the list of components.  This nested loop is O(N^2),
     but we should never have too many components and/or names, so this
     *hopefully* shouldn't matter... */
  
  for (item = opal_list_get_first(components); 
       item != opal_list_get_end(components); 
       item = opal_list_get_next(item)) {
    component = ((mca_base_component_priority_list_item_t *) 
                 item)->super.cli_component;

    /* If we have a list of names, scan through it */

    if (0 == num_names) {
      want_to_check = true;
    } else {
      want_to_check = false;
      for (i = 0; i < num_names; ++i) {
        if (0 == strcmp(names[i], component->mca_component_name)) {
          want_to_check = true;
        }
      }
    }

    /* If we determined that we want to check this component, then do
       so */

    if (want_to_check) {
      priority = check_one_component(comm, component, &module, &data);
      if (priority >= 0) {

        /* We have a component that indicated that it wants to run by
           giving us a module */

        avail = OBJ_NEW(avail_coll_t);
        avail->ac_priority = priority;
        avail->ac_component = (mca_coll_base_component_1_0_0_t *) component;
        avail->ac_module = module;
        avail->ac_data = data;

        /* Put this item on the list in priority order (highest
           priority first).  Should it go first? */

        if (opal_list_is_empty(selectable)) {
            opal_list_prepend(selectable, (opal_list_item_t *) avail);
        } else {
            item2 = opal_list_get_first(selectable); 
            avail2 = (avail_coll_t *) item2;
            if (avail->ac_priority > avail2->ac_priority) {
                opal_list_prepend(selectable, (opal_list_item_t *) avail);
            } else {
                for (i = 1; item2 != opal_list_get_end(selectable); 
                     item2 = opal_list_get_next(item2), ++i) {
                    avail2 = (avail_coll_t *) item2;
                    if (avail->ac_priority > avail2->ac_priority) {
                        opal_list_insert(selectable, 
                                         (opal_list_item_t *) avail, i);
                        break;
                    }
                }
                
                /* If we didn't find a place to put it in the list, then
                   append it (because it has the lowest priority found so
                   far) */

                if (opal_list_get_end(selectable) == item2) {
                    opal_list_append(selectable, (opal_list_item_t *) avail);
                }
            }
        }
      }
    }
  }

  /* If we didn't find any available components, return an error */

  if (0 == opal_list_get_size(selectable)) {
    OBJ_RELEASE(selectable);
    return NULL;
  }

  /* All done */

  return selectable;
}


/*
 * Check a single component
 */
static int check_one_component(ompi_communicator_t *comm, 
                               const mca_base_component_t *component,
                               const mca_coll_base_module_1_0_0_t **module,
                               struct mca_coll_base_comm_t **data)
{
  int err;
  int priority = -1;

  err = query(component, comm, &priority, module, data);

  if (OMPI_SUCCESS == err) {
    priority = (priority < 100) ? priority : 100;
    opal_output_verbose(10, mca_coll_base_output, 
                        "coll:base:comm_select: component available: %s, priority: %d", 
                        component->mca_component_name, priority);

  } else {
    priority = -1;
    opal_output_verbose(10, mca_coll_base_output, 
                        "coll:base:comm_select: component not available: %s",
                        component->mca_component_name);
  }

  return priority;
}


/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Take any version of a coll module, query it, and return the right
 * module struct
 */
static int query(const mca_base_component_t *component, 
                 ompi_communicator_t *comm, 
                 int *priority, const mca_coll_base_module_1_0_0_t **module,
                 struct mca_coll_base_comm_t **data)
{
  /* coll v1.0.0 */

  *module = NULL;
  if (1 == component->mca_major_version &&
      0 == component->mca_minor_version &&
      0 == component->mca_release_version) {
    const mca_coll_base_component_1_0_0_t *coll100 = 
      (mca_coll_base_component_1_0_0_t *) component;

    return query_1_0_0(coll100, comm, priority, module, data);
  } 

  /* Unknown coll API version -- return error */

  return OMPI_ERROR;
}


static int query_1_0_0(const mca_coll_base_component_1_0_0_t *component,
                       ompi_communicator_t *comm, int *priority,
                       const mca_coll_base_module_1_0_0_t **module,
                       struct mca_coll_base_comm_t **data)
{
  const mca_coll_base_module_1_0_0_t *ret;

  /* There's currently no need for conversion */

  ret = component->collm_comm_query(comm, priority, data);
  if (NULL != ret) {
    *module = ret;
    return OMPI_SUCCESS;
  }

  return OMPI_ERROR;
}


/**************************************************************************
 * Unquery functions
 **************************************************************************/

static void unquery(const mca_coll_base_component_1_0_0_t *component, 
                    ompi_communicator_t *comm,
                    struct mca_coll_base_comm_t *data)
{
  if (1 == component->collm_version.mca_major_version &&
      0 == component->collm_version.mca_minor_version &&
      0 == component->collm_version.mca_release_version) {
    const mca_coll_base_component_1_0_0_t *coll100 = 
      (mca_coll_base_component_1_0_0_t *) component;

    unquery_1_0_0(coll100, comm, data);
  }

  /* There's no way to have a version that we don't recognize here --
     it would have already been removed from the list */
}


static void unquery_1_0_0(const mca_coll_base_component_1_0_0_t *component, 
                          ompi_communicator_t *comm,
                          struct mca_coll_base_comm_t *data)
{
    if (NULL != component->collm_comm_unquery) {
        component->collm_comm_unquery(comm, data);
    }
}


/**************************************************************************
 * Module_Init functions
 **************************************************************************/

/*
 * Initialize a module
 */
static int module_init(const mca_coll_base_module_1_0_0_t *module, 
                       ompi_communicator_t *comm)
{
  const mca_coll_base_module_1_0_0_t *ret;

  /* There's currently no need for conversion */

  ret = module->coll_module_init(comm);
  if (NULL != ret) {
    if (comm->c_coll_selected_module != ret) {
      comm->c_coll = *ret;
      comm->c_coll_selected_module = ret;
    }
    return OMPI_SUCCESS;
  }
  return OMPI_ERROR;
}


/**************************************************************************
 * Misc functions
 **************************************************************************/

/*
 * If the basic module has not already been setup on this
 * communicator, query and initialize it.
 */
static int query_basic(ompi_communicator_t *comm) 
{
  int ret;
  struct mca_coll_base_comm_t *data;

  ret = OMPI_SUCCESS;
  if (NULL == comm->c_coll_basic_module) {
    ret = query((mca_base_component_t *) mca_coll_base_basic_component, comm, 
                &basic_priority, &comm->c_coll_basic_module, &data);
    if (ret != OMPI_SUCCESS) {
      comm->c_coll_basic_module = NULL;
      return ret;
    }

    comm->c_coll_basic_data = data;
    ret = module_init(comm->c_coll_basic_module, comm);
  }
  return ret;
}


/* 
 * Replace the NULL pointers by corresponsing ompi_basic pointers 
 */
static int replace_null_with_basic(ompi_communicator_t *comm)
{
  int err;

#define CHECK(name) \
  if (NULL == comm->c_coll.coll_##name) { \
    if (OMPI_SUCCESS != (err = query_basic(comm))) { \
      return err; \
    } \
    comm->c_coll.coll_##name = comm->c_coll_basic_module->coll_##name; \
  }

  CHECK(allgather); 
  CHECK(allgatherv); 
  CHECK(allreduce); 
  CHECK(alltoall); 
  CHECK(alltoallv); 
  CHECK(alltoallw); 
  CHECK(barrier); 
  CHECK(bcast); 
  CHECK(exscan); 
  CHECK(gather); 
  CHECK(gatherv); 
  CHECK(reduce); 
  CHECK(reduce_scatter); 
  CHECK(scan); 
  CHECK(scatter); 
  CHECK(scatterv); 

  /* Happiness; all done */

  return OMPI_SUCCESS;
}
