/*
 * $HEADER$
 */

#include "lam_config.h"

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Ensure to get the right <ltdl.h> */ 
#include "libltdl/ltdl.h"

#include "constants.h"
#include "lfc/lam_list.h"
#include "mca/mca.h"
#include "mca/base/base.h"


/*
 * Private types
 */
struct registry_item_t {
  lam_list_item_t super;

  char ri_type[MCA_BASE_MAX_TYPE_NAME_LEN];
  lt_dlhandle ri_dlhandle;
  const mca_base_module_t *ri_module_struct;
  int ri_refcount;
  lam_list_t ri_dependencies;
};
typedef struct registry_item_t registry_item_t;

struct dependency_item_t {
  lam_list_item_t super;

  registry_item_t *di_registry_entry;
};
typedef struct dependency_item_t dependency_item_t;


/*
 * Private variables
 */
static bool initialized = false;
static lam_list_t registry;


/*
 * Private functions
 */
static registry_item_t *find_module(const char *type, const char *name);
static int link_items(registry_item_t *src, registry_item_t *depend);
static void release_registry_item(registry_item_t *ri);


/*
 * Initialize the registry
 */
int mca_base_module_registry_construct(void)
{
  /* Setup internal structures */

  if (!initialized) {

    /* Initialize libltdl */

    if (lt_dlinit() != 0)
      return LAM_ERR_OUT_OF_RESOURCE;

    OBJ_CONSTRUCT(&registry, lam_list_t);
    initialized = true;
  }

  /* All done */

  return LAM_SUCCESS;
}


/*
 * Add a newly-opened dyanmic module to the registry of open modules.
 * The module's type, handle, and public struct are saved.
 */
int mca_base_module_registry_retain(char *type, lt_dlhandle module_handle, 
                                    const mca_base_module_t *module_struct)
{
  registry_item_t *ri;

  /* Allocate a new registry item */

  ri = malloc(sizeof(registry_item_t));
  if (NULL == ri)
    return LAM_ERR_OUT_OF_RESOURCE;

  /* Initialize the registry item */

  OBJ_CONSTRUCT(ri, lam_list_item_t);
  strcpy(ri->ri_type, type);
  ri->ri_dlhandle = module_handle;
  ri->ri_module_struct = module_struct;
  ri->ri_refcount = 1;
  OBJ_CONSTRUCT(&ri->ri_dependencies, lam_list_t);

  /* Append the new item to the registry */

  lam_list_append(&registry, (lam_list_item_t *) ri);

  /* All done */

  return LAM_SUCCESS;
}


/*
 * Create a dependency from one module entry to another
 */
int mca_base_module_registry_link(const char *src_type, 
                                  const char *src_name,
                                  const char *depend_type,
                                  const char *depend_name)
{
  registry_item_t *src, *depend;

  /* Look up the two modules */

  src = find_module(src_type, src_name);
  if (NULL == src)
    return LAM_ERR_BAD_PARAM;
  depend = find_module(depend_type, depend_name);
  if (NULL == depend)
    return LAM_ERR_BAD_PARAM;

  /* Link them */

  return link_items(src, depend);
}


/*
 * If it's in the registr, close a specified module and remove it from
 * the registry.
 */
void mca_base_module_registry_release(const mca_base_module_t *module)
{
  registry_item_t *ri = find_module(module->mca_type_name, 
                                    module->mca_module_name);
  if (NULL != ri)
    release_registry_item(ri);
}


/*
 * Finalize the registry -- close everything that's still open.
 */
void mca_base_module_registry_finalize(void)
{
  lam_list_item_t *item;
  registry_item_t *ri;
  bool changed;

  if (initialized) {

    /* Have to be slightly careful about this because of dependencies,
       particularly on OS's where it matters (i.e., closing a module
       that is depended on by other modules actually causes missing
       symbols because the OS actually does unload it from memory!),
       such as OS X.

       So instead of just blindly closing everything, we have iterate
       over the array of open modules releasing everything with a
       refcount of 1 -- skip anything with a refcount of more than 1.
       Repeat this procedure until either we have nothing open or we
       made one full pass and no refcounts went to 1 (which is
       technically an error). */

    do {
      changed = false;
      for (item = lam_list_get_first(&registry);
           lam_list_get_end(&registry) != item && changed;
           item = lam_list_get_next(item)) {
        ri = (registry_item_t *) ri;

        if (ri->ri_refcount == 1) {
          release_registry_item(ri);
          changed = true;
        }
      }
    } while (lam_list_get_size(&registry) > 0 && changed);

    /* Close down libltdl */

    lt_dlexit();
    initialized = false;
  }
}


static registry_item_t *find_module(const char *type, const char *name)
{
  lam_list_item_t *item;
  registry_item_t *ri;

  for (item = lam_list_get_first(&registry);
       lam_list_get_end(&registry) != item;
       item = lam_list_get_next(item)) {
    ri = (registry_item_t *) ri;
    if (0 == strcmp(ri->ri_type, type) && 
        0 == strcmp(ri->ri_module_struct->mca_module_name, name))
      return ri;
  }

  /* Not found */

  return NULL;
}


static int link_items(registry_item_t *src, registry_item_t *depend)
{
  dependency_item_t *di;

  /* Bozo check */

  if (NULL == src || NULL == depend)
    return LAM_ERR_BAD_PARAM;

  /* Make a new depedency item */

  di = malloc(sizeof(dependency_item_t));
  if (NULL == di)
    return LAM_ERR_OUT_OF_RESOURCE;

  /* Initialize the new dependency item */

  OBJ_CONSTRUCT((lam_list_item_t *) di, lam_list_item_t);
  di->di_registry_entry = depend;

  /* Add it to the dependency list on the source registry entry */

  lam_list_append(&src->ri_dependencies, (lam_list_item_t *) di);

  /* Increment the refcount in the dependency */

  ++src->ri_refcount;

  /* All done */

  return LAM_SUCCESS;
}


static void release_registry_item(registry_item_t *ri)
{
  dependency_item_t *di;
  lam_list_item_t *item;

  /* Bozo check */

  if (NULL == ri)
    return;

  /* Decrement this module's refcount.  If zero, close and free it. */

  --ri->ri_refcount;
  if (0 == ri->ri_refcount) {
    lt_dlclose(ri->ri_dlhandle);

    /* Now go release/close (at a minimum: decrement the refcount) any
       dependencies of this module */

    for (item = lam_list_remove_first(&ri->ri_dependencies);
         NULL != item; 
         item = lam_list_remove_first(&ri->ri_dependencies)) {
      di = (dependency_item_t *) item;
      --di->di_registry_entry->ri_refcount;
      free(di);
    }

    /* It should be obvious, but I'll state it anyway because it bit
       me during debugging: after the dlclose(), the mca_base_module_t
       pointer is no longer valid because it has [potentially] been
       unloaded from memory.  So don't try to use it.  :-) */

    OBJ_DESTRUCT(&di->di_registry_entry->ri_dependencies);
    lam_list_remove_item(&registry, (lam_list_item_t *) ri);
    free(ri);
  }
}
