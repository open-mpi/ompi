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
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/mca/dl/base/base.h"
#include "opal/constants.h"

#if OPAL_HAVE_DL_SUPPORT

/*
 * Private types
 */
struct repository_item_t {
  opal_list_item_t super;

  char ri_type[MCA_BASE_MAX_TYPE_NAME_LEN + 1];
  opal_dl_handle_t *ri_dlhandle;
  const mca_base_component_t *ri_component_struct;
  opal_list_t ri_dependencies;
};
typedef struct repository_item_t repository_item_t;
static void ri_constructor(opal_object_t *obj);
static void ri_destructor(opal_object_t *obj);
static OBJ_CLASS_INSTANCE(repository_item_t, opal_list_item_t, 
                          ri_constructor, ri_destructor);

struct dependency_item_t {
  opal_list_item_t super;

  repository_item_t *di_repository_entry;
};
typedef struct dependency_item_t dependency_item_t;
static void di_constructor(opal_object_t *obj);
static void di_destructor(opal_object_t *obj);
static OBJ_CLASS_INSTANCE(dependency_item_t, opal_list_item_t, 
                          di_constructor, di_destructor);

#endif /* OPAL_HAVE_DL_SUPPORT */


/*
 * Private variables
 */
static bool initialized = false;


#if OPAL_HAVE_DL_SUPPORT

static opal_list_t repository;


/*
 * Private functions
 */
static repository_item_t *find_component(const char *type, const char *name);
static int link_items(repository_item_t *src, repository_item_t *depend);

#endif /* OPAL_HAVE_DL_SUPPORT */


/*
 * Initialize the repository
 */
int mca_base_component_repository_init(void)
{
  /* Setup internal structures */

  if (!initialized) {
#if OPAL_HAVE_DL_SUPPORT

    /* Initialize the dl framework */
    int ret = mca_base_framework_open(&opal_dl_base_framework, 0);
    if (OPAL_SUCCESS != ret) {
        opal_output(0, "%s %d:%s failed -- process will likely abort (open the dl framework returned %d instead of OPAL_SUCCESS)\n",
                    __FILE__, __LINE__, __func__, ret);
        return ret;
    }
    opal_dl_base_select();

    /* Bump the refcount to indicate that this framework is "special"
       -- it can't be finalized until all other frameworks have been
       finalized.  E.g., in opal/runtime/opal_info_support.c, there's
       a loop calling mca_base_framework_close() on all OPAL
       frameworks.  But that function simply decrements each
       framework's refcount, and if it's zero, closes it.  This
       additional increment ensures that the "dl" framework is not
       closed as part of that loop. */
    ++opal_dl_base_framework.framework_refcnt;

    OBJ_CONSTRUCT(&repository, opal_list_t);
#endif

    initialized = true;
  }

  /* All done */

  return OPAL_SUCCESS;
}


/*
 * Add a newly-opened dyanmic component to the repository of open
 * components.  The component's type, handle, and public struct are
 * saved.
 */
int mca_base_component_repository_retain(char *type, 
                                         opal_dl_handle_t *component_handle,
                                         const mca_base_component_t *component_struct)
{
#if OPAL_HAVE_DL_SUPPORT
  repository_item_t *ri;

  /* Allocate a new repository item */

  ri = OBJ_NEW(repository_item_t);
  if (NULL == ri) {
    return OPAL_ERR_OUT_OF_RESOURCE;
  }

  /* Initialize the repository item */

  strncpy(ri->ri_type, type, MCA_BASE_MAX_TYPE_NAME_LEN);
  ri->ri_type[MCA_BASE_MAX_TYPE_NAME_LEN] = '\0';
  ri->ri_dlhandle = component_handle;
  ri->ri_component_struct = component_struct;

  /* Append the new item to the repository */

  opal_list_append(&repository, (opal_list_item_t *) ri);

  /* All done */

  return OPAL_SUCCESS;
#else
  return OPAL_ERR_NOT_SUPPORTED;
#endif
}


/*
 * Bump up the refcount on a component
 */
int mca_base_component_repository_retain_component(const char *type, 
                                                   const char *name)
{
#if OPAL_HAVE_DL_SUPPORT
    repository_item_t *ri = find_component(type, name);
    if (NULL != ri) {
        OBJ_RETAIN(ri);
        return OPAL_SUCCESS;
    }
    return OPAL_ERR_NOT_FOUND;
#else
    return OPAL_ERR_NOT_SUPPORTED;
#endif
}


/*
 * Create a dependency from one component entry to another
 */
int mca_base_component_repository_link(const char *src_type, 
                                       const char *src_name,
                                       const char *depend_type,
                                       const char *depend_name)
{
#if OPAL_HAVE_DL_SUPPORT
  repository_item_t *src, *depend;

  /* Look up the two components */

  src = find_component(src_type, src_name);
  if (NULL == src) {
    return OPAL_ERR_BAD_PARAM;
  }
  depend = find_component(depend_type, depend_name);
  if (NULL == depend) {
    return OPAL_ERR_BAD_PARAM;
  }

  /* Link them */

  return link_items(src, depend);
#else
    return OPAL_ERR_NOT_SUPPORTED;
#endif
}


/*
 * If it's in the repository, close a specified component and remove
 * it from the repository.
 */
void mca_base_component_repository_release(const mca_base_component_t *component)
{
#if OPAL_HAVE_DL_SUPPORT
  if (initialized) {
    repository_item_t *ri = find_component(component->mca_type_name, 
                                           component->mca_component_name);
    if (NULL != ri) {
      OBJ_RELEASE(ri);
    }
  }
#endif
}


/*
 * Finalize the repository -- close everything that's still open.
 */
void mca_base_component_repository_finalize(void)
{
#if OPAL_HAVE_DL_SUPPORT
  repository_item_t *ri, *next;
#endif

  if (initialized) {
#if OPAL_HAVE_DL_SUPPORT

    /* Have to be slightly careful about this because of dependencies,
       particularly on OS's where it matters (i.e., closing a
       component that is depended on by other components actually
       causes missing symbols because the OS actually does unload it
       from memory!), such as OS X.

       So instead of just blindly closing everything, we have iterate
       over the array of open components releasing everything with a
       refcount of 1 -- skip anything with a refcount of more than 1.
       Repeat this procedure until either we have nothing open or we
       made one full pass and no refcounts went to 1 (which is
       technically an error). */

    do {
      OPAL_LIST_FOREACH_SAFE(ri, next, &repository, repository_item_t) {
        OBJ_RELEASE(ri);
      }
    } while (opal_list_get_size(&repository) > 0);

    /* Close the dl framework (see comment about refcnt in
       mca_base_component_repository_init()) */
    --opal_dl_base_framework.framework_refcnt;
    (void) mca_base_framework_close(&opal_dl_base_framework);
#endif

    initialized = false;
  }
}

#if OPAL_HAVE_DL_SUPPORT

static repository_item_t *find_component(const char *type, const char *name)
{
  opal_list_item_t *item;
  repository_item_t *ri;

  for (item = opal_list_get_first(&repository);
       opal_list_get_end(&repository) != item;
       item = opal_list_get_next(item)) {
    ri = (repository_item_t *) item;
    if (0 == strcmp(ri->ri_type, type) && 
        0 == strcmp(ri->ri_component_struct->mca_component_name, name)) {
      return ri;
    }
  }

  /* Not found */

  return NULL;
}


static int link_items(repository_item_t *src, repository_item_t *depend)
{
  dependency_item_t *di;

  /* Bozo check */

  if (NULL == src || NULL == depend) {
    return OPAL_ERR_BAD_PARAM;
  }

  /* Make a new depedency item */

  di = OBJ_NEW(dependency_item_t);
  if (NULL == di) {
    return OPAL_ERR_OUT_OF_RESOURCE;
  }

  /* Initialize the new dependency item */

  di->di_repository_entry = depend;

  /* Add it to the dependency list on the source repository entry */

  opal_list_append(&src->ri_dependencies, (opal_list_item_t *) di);

  /* Increment the refcount in the dependency */

  OBJ_RETAIN(depend);

  /* All done */

  return OPAL_SUCCESS;
}


/*
 * Basic sentinel values, and construct the inner list
 */
static void ri_constructor(opal_object_t *obj)
{
  repository_item_t *ri = (repository_item_t *) obj;

  memset(ri->ri_type, 0, sizeof(ri->ri_type));
  ri->ri_dlhandle = NULL;
  ri->ri_component_struct = NULL;

  OBJ_CONSTRUCT(&ri->ri_dependencies, opal_list_t);
}


/*
 * Close a component 
 */
static void ri_destructor(opal_object_t *obj)
{
  repository_item_t *ri = (repository_item_t *) obj;
  opal_list_item_t *item;
  int group_id;

  group_id = mca_base_var_group_find (NULL, ri->ri_type,
                                      ri->ri_component_struct->mca_component_name);
  if (0 <= group_id) {
    mca_base_var_group_deregister (group_id);
  }

  /* Close the component (and potentially unload it from memory */
  opal_dl_close(ri->ri_dlhandle);

  /* It should be obvious, but I'll state it anyway because it bit me
     during debugging: after the dlclose(), the mca_base_component_t
     pointer is no longer valid because it has [potentially] been
     unloaded from memory.  So don't try to use it.  :-) */

  /* Now go release/close (at a minimum: decrement the refcount) any
     dependencies of this component */

  while (NULL != (item = opal_list_remove_first(&ri->ri_dependencies))) {
    OBJ_RELEASE(item);
  }
  OBJ_DESTRUCT(&ri->ri_dependencies);
  opal_list_remove_item(&repository, (opal_list_item_t *) ri);
}


/*
 * Basic sentinel values
 */
static void di_constructor(opal_object_t *obj)
{
  dependency_item_t *di = (dependency_item_t *) obj;

  di->di_repository_entry = NULL;
}


/*
 * When a dependency item is released, go release the repository entry
 * that it points to
 */
static void di_destructor(opal_object_t *obj)
{
  dependency_item_t *di = (dependency_item_t *) obj;

  OBJ_RELEASE(di->di_repository_entry);
}

#endif /* OPAL_HAVE_DL_SUPPORT */
