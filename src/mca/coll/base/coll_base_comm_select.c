/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "util/argv.h"
#include "class/ompi_list.h"
#include "class/ompi_object.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


/*
 * Local variables
 */
static mca_coll_1_0_0_t null_actions = {
  NULL, NULL,  /* init and finalize */
  /* Checkpoint / restart */

  NULL, NULL, NULL, NULL,

  /* Collective function pointers */

  NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  false, NULL, NULL, NULL, NULL,
  false, NULL, NULL, NULL, NULL, NULL
};


/*
 * Local types
 */
struct avail_coll_t {
  ompi_list_item_t super;

  int ac_priority;
  const mca_coll_base_module_1_0_0_t *ac_component;
  const mca_coll_1_0_0_t *ac_module;
};
typedef struct avail_coll_t avail_coll_t;

/*
 * Local functions
 */
static ompi_list_t *check_components(ompi_list_t *components, 
                                     ompi_communicator_t *comm, 
                                     char **names, int num_names);
static int check_one_component(ompi_communicator_t *comm, 
                               const mca_base_module_t *component,
                               const mca_coll_1_0_0_t **module);

static int query(const mca_base_module_t *component, 
                 ompi_communicator_t *comm, int *priority,
                 const mca_coll_1_0_0_t **module);
static int query_1_0_0(const mca_coll_base_module_1_0_0_t *coll_component, 
                       ompi_communicator_t *comm, int *priority,
                       const mca_coll_1_0_0_t **module);

static void unquery(const mca_coll_base_module_1_0_0_t *coll_component,
                    ompi_communicator_t *comm);
static void unquery_1_0_0(const mca_coll_base_module_1_0_0_t *coll_component, 
                          ompi_communicator_t *comm);

static int module_init(const mca_coll_1_0_0_t *module, 
                       ompi_communicator_t *comm);

static int query_basic(ompi_communicator_t *comm);
static void replace_null_with_basic(ompi_communicator_t *comm);


/*
 * Stuff for the OBJ interface
 */
OBJ_CLASS_INSTANCE(avail_coll_t, ompi_list_item_t, NULL, NULL);


/*
 * This function is called at the initialization time of every
 * communicator.  It is used to select which coll component will be
 * active for a given communicator.
 *
 * This selection logic is not for the weak.
 *
 * Note that this is not thread safe.  It's probably ok to just put a
 * lock around this function, though -- this is only called at the end
 * of a communicator constructor, so making a few threads wait while
 * we construct is probably ok. 
 */
int mca_coll_base_comm_select(ompi_communicator_t *comm, 
                              mca_base_module_t *preferred)
{
  bool found, using_basic;
  int err, num_names;
  char name[MPI_MAX_OBJECT_NAME + 32];
  char *names, **name_array;
  char *str;
  avail_coll_t *avail;
  ompi_list_t *selectable;
  ompi_list_item_t *item;
  const mca_coll_base_module_1_0_0_t *selected_component, *component;
  const mca_coll_1_0_0_t *selected_module;

  /* Announce */

  snprintf(name, sizeof(name), "%s (cid %d)", comm->c_name, 
           comm->c_contextid);
  name[sizeof(name) - 1] = '\0';
  ompi_output_verbose(10, mca_coll_base_output,
                      "coll:base:comm_select: new communicator: %s", 
                      name);
  
  /* Initialize all the relevant pointers, since they're used as
     sentinel values */
  
  comm->c_coll = null_actions;

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
    str = &(preferred->mca_module_name[0]);

    ompi_output_verbose(10, mca_coll_base_output, 
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
    name_array = ompi_argv_split("names", ',');
    num_names = ompi_argv_count(name_array);

    ompi_output_verbose(10, mca_coll_base_output, 
                       "coll:base:comm_select: Checking specific modules: %s",
                       names);
    selectable = check_components(&mca_coll_base_components_available, 
                                  comm, name_array, num_names);
    ompi_argv_free(name_array);
  }

  /* Nope -- a specific [set of] component[s] was not requested.  Go
     check them all. */
  
  else {
    ompi_output_verbose(10, mca_coll_base_output, 
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
      /* show_help */
      return OMPI_ERROR;
    }
  }

  /* Do some kind of collective operation to find a module that
     everyone has available */

#if 1
  /* For the moment, just take the top module off the list */

  if (NULL != selectable) {
    using_basic = false;
    item = ompi_list_remove_first(selectable);
    avail = (avail_coll_t *) item;
    selected_component = avail->ac_component;
    selected_module = avail->ac_module;
    OBJ_RELEASE(avail);
  } else {
    using_basic = true;
    selected_component = mca_coll_base_basic_component;
    selected_module = comm->c_coll_basic_module;
  }
#else
  /* JMS CONTINUE HERE */
#endif

  /* Everything left in the selectable list is therefore unwanted,
     and we call their unquery() method (because they all had query()
     invoked, but will never have init() invoked in this scope). */

  if (NULL != selectable) {
    for (item = ompi_list_remove_first(selectable); item != NULL;
         item = ompi_list_remove_first(selectable)) {
      avail = (avail_coll_t *) item;
      component = avail->ac_component;
      unquery(component, comm);
      OBJ_RELEASE(avail);
    }
    OBJ_DESTRUCT(selectable);
  }

  /* If we're not using the basic module, then set it up, replace all
     NULL function pointers with those from basic, and then initialize
     it. */

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
  
  ompi_output_verbose(10, mca_coll_base_output,
                     "coll:base:comm_select: Selected coll module %s", 
                     selected_component->collm_version.mca_module_name);
  
  return OMPI_SUCCESS;
}


/*
 * For each module in the list, if it is in the list of names (or the
 * list of names is NULL), then check and see if it wants to run, and
 * do the resulting priority comparison.  Make a list of modules to be
 * only those who returned that they want to run, and put them in
 * priority order.
 */
static ompi_list_t *check_components(ompi_list_t *components, 
                                     ompi_communicator_t *comm, 
                                     char **names, int num_names)
{
  int i, priority;
  const mca_base_module_t *component;
  ompi_list_item_t *item, *next;
  const mca_coll_1_0_0_t *actions;
  bool want_to_check;
  ompi_list_t *selectable;
  avail_coll_t *avail;

  /* Make a list of the components that query successfully */

  selectable = OBJ_NEW(ompi_list_t);

  /* Scan through the list of components.  This nested loop is O(N^2),
     but we should never have too many components and/or names, so this
     *hopefully* shouldn't matter... */
  
  for (item = ompi_list_get_first(components); 
       item != ompi_list_get_end(components); 
       item = next) {
    component = ((mca_base_component_priority_list_item_t *) 
                 item)->cpli_component;
    next = ompi_list_get_next(component);

    /* If we have a list of names, scan through it */

    if (0 == num_names) {
      want_to_check = true;
    } else {
      want_to_check = false;
      for (i = 0; i < num_names; ++i) {
        if (0 == strcmp(names[i], component->mca_module_name)) {
          want_to_check = true;
        }
      }
    }

    /* If we determined that we want to check this component, then do
       so */

    if (want_to_check) {
      priority = check_one_component(comm, component, &actions);
      if (priority > 0) {

        /* We have a component that indicated that it wants to run by
           giving us a module */

        avail = OBJ_NEW(avail_coll_t);
        avail->ac_priority = 0;
        avail->ac_component = (mca_coll_base_module_1_0_0_t *) component;
        ompi_list_append(selectable, item);
      } else {
        ompi_list_remove_item(components, item);
      }
    }

    /* If we didn't want to check, then eliminate this entry from the
       list */

    else {
      ompi_list_remove_item(components, item);
    }
  }

  /* If we didn't find any available components, return an error */

  if (0 == ompi_list_get_size(selectable)) {
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
                               const mca_base_module_t *component,
                               const mca_coll_1_0_0_t **module)
{
  int err;
  int priority = -1;

  err = query(component, comm, &priority, module);

  if (OMPI_SUCCESS == err) {
    priority = (priority < 100) ? priority : 100;
    ompi_output_verbose(10, mca_coll_base_output, 
                        "coll:base:comm_select: component available: %s, priority: %d", 
                        component->mca_module_name, priority);

  } else {
    priority = -1;
    ompi_output_verbose(10, mca_coll_base_output, 
                        "coll:base:comm_select: component not available: %s",
                        component->mca_module_name);
  }

  return priority;
}


/**************************************************************************
 * Query functions
 **************************************************************************/

/*
 * Take any version of a coll module, query it, and return the right
 * actions struct
 */
static int query(const mca_base_module_t *component, 
                 ompi_communicator_t *comm, 
                 int *priority, const mca_coll_1_0_0_t **module)
{
  /* coll v1.0.0 */

  *module = NULL;
  if (1 == component->mca_major_version &&
      0 == component->mca_minor_version &&
      0 == component->mca_release_version) {
    const mca_coll_base_module_1_0_0_t *coll100 = 
      (mca_coll_base_module_1_0_0_t *) component;

    return query_1_0_0(coll100, comm, priority, module);
  } 

  /* Unknown coll API version -- return error */

  return OMPI_ERROR;
}


static int query_1_0_0(const mca_coll_base_module_1_0_0_t *component,
                       ompi_communicator_t *comm, int *priority,
                       const mca_coll_1_0_0_t **module)
{
  const mca_coll_1_0_0_t *ret;

  /* There's currently no need for conversion */

  ret = component->collm_comm_query(comm, priority);
  if (NULL != ret) {
    *module = ret;
    return OMPI_SUCCESS;
  }

  return OMPI_ERROR;
}


/**************************************************************************
 * Unquery functions
 **************************************************************************/

static void unquery(const mca_coll_base_module_1_0_0_t *component, 
                    ompi_communicator_t *comm)
{
  if (1 == component->collm_version.mca_major_version &&
      0 == component->collm_version.mca_minor_version &&
      0 == component->collm_version.mca_release_version) {
    const mca_coll_base_module_1_0_0_t *coll100 = 
      (mca_coll_base_module_1_0_0_t *) component;

    unquery_1_0_0(coll100, comm);
  }

  /* There's no way to have a version that we don't recognize here --
     it would have already been removed from the list */
}


static void unquery_1_0_0(const mca_coll_base_module_1_0_0_t *component, 
                          ompi_communicator_t *comm)
{
  component->collm_comm_unquery(comm);
}


/**************************************************************************
 * Module_Init functions
 **************************************************************************/

/*
 * Initialize a module
 */
static int module_init(const mca_coll_1_0_0_t *module, 
                       ompi_communicator_t *comm)
{
  const mca_coll_1_0_0_t *ret;

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
  int priority;

  ret = OMPI_SUCCESS;
  if (NULL == comm->c_coll_basic_module) {
    ret = query((mca_base_module_t *) mca_coll_base_basic_component, comm, 
                &priority, &comm->c_coll_basic_module);
    if (ret != OMPI_SUCCESS) {
      comm->c_coll_basic_module = NULL;
      return ret;
    }

    ret = module_init(comm->c_coll_basic_module, comm);
  }
  return ret;
}


/* 
 * Replace the NULL pointers by corresponsing ompi_basic pointers 
 */
static void replace_null_with_basic(ompi_communicator_t *comm)
{
#define CHECK(name) \
  if (NULL == comm->c_coll.coll_##name) { \
    query_basic(comm); \
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
}
