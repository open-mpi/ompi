/*
 * $HEADER$
 *
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "communicator/communicator.h"
#include "lfc/lam_list.h"
#include "include/constants.h"


/*
 * Global variables
 */
lam_list_t mca_coll_base_available;


/*
 * Private functions
 */
static int thread_query(mca_base_module_t *ls, 
			mca_base_module_priority_list_item_t *entry);
static int thread_query_1_0_0(mca_base_module_t *ls, 
			      mca_base_module_priority_list_item_t *entry);

/*
 * Scan down the list of successfully opened modules and query each of
 * them (the opened list will be one or more modules.  If the user
 * requested a specific module, it will be the only module in the
 * opened list).  Create and populate the available list of all
 * modules who indicate that they want to be considered for selection.
 * Close all modules who do not want to be considered for selection,
 * and destroy the opened list.
 */
int
mca_coll_base_query()
{
    int found, count = 0;
    mca_base_module_priority_list_item_t *entry;
    lam_list_item_t *p;

    /* Initialize the list */

    /* VPS: This is not thread safe, this needs to be a local thingy  */
    OBJ_CONSTRUCT(&mca_coll_base_available, lam_list_t);

    /* In 64 bit mode, this struct can have empty padding */
#if 0
    LAM_ZERO_ME(entry);
#endif

    /* The list of modules that we should check has already been
       established in mca_coll_base_opened. */

    for (found = 0, p = lam_list_get_first(&mca_coll_base_modules_opened);
	 p != lam_list_get_end(&mca_coll_base_modules_opened);
	 p = lam_list_get_next(p)) {

	entry = malloc (sizeof(mca_base_module_priority_list_item_t));
	if (NULL == entry)
	    return LAM_ERROR;

	OBJ_CONSTRUCT(&entry->super, lam_list_item_t);
	
	entry->mpli_module = ((mca_base_module_list_item_t *) p)->mli_module;

	/* Call a subroutine to do the work, because the module may
	   represent different versions of the coll SSI struct. */

	if (thread_query(entry->mpli_module, entry) == 0) {

	    /* Save the results in the list.  The priority isn't
	       relevant, because selection is decided at
	       communicator-constructor time.  But we save the
	       thread_min and thread_max arguments (set in the
	       thread_query() function) so that the initial selection
	       algorithm can negotiate the overall thread level for
	       this process. */

	    ++count;

	    entry->mpli_priority = 0;
	    lam_list_append(&mca_coll_base_available, 
			    (lam_list_item_t *) entry);
	    found = 1;
	} else {

	    /* If the module doesn't want to run, then close it.  It's
	       already had its close() method invoked; now close it out of
	       the dynamic registry (if it's there). */

	    mca_base_module_repository_release(entry->mpli_module);

	    /* VPS: This thing is not tested, I think it should be
	       this way, but really gotto check if this does not
	       result in duplicate frees  */

	    OBJ_DESTRUCT(entry);
	    free(entry);
	}
    }

    /* The opened list is now no longer useful and we can free it */

    OBJ_DESTRUCT(&mca_coll_base_modules_opened);

    /* If we have no collective modules available, it's an error.
       Thanks for playing! */

    if (found == 0) {
	OBJ_DESTRUCT(&mca_coll_base_available);

#if 0
	if (mca_coll_verbose >= 10)
	    lam_debug(mca_coll_did, "query: no collectives available!");
	show_help("ssi-coll", "none-available", NULL);
#endif

	return LAM_ERROR;
    }

    /* All done */

    return LAM_SUCCESS;
}


static int 
thread_query(mca_base_module_t *ls, 
	     mca_base_module_priority_list_item_t *entry)
{
    int ret;

#if 0
    if (lam_ssi_coll_verbose > 10)
	lam_debug(lam_ssi_coll_did, "query: querying coll module %s", 
		  ls->ssi_module_name);
    fprintf(stderr, "query: querying coll module %s\n", 
	    ls->mca_module_name);
#endif

    /* This module has already been successfully opened.  So now query
       it. */

    if (ls->mca_major_version == 1 &&
	ls->mca_minor_version == 0 &&
	ls->mca_release_version == 0)
	ret = thread_query_1_0_0(ls, entry);

    /* Query done -- look at the return value to see what happened */

    if (ret != 0) {

#if 0
	if (lam_mca_coll_verbose > 10)
	    lam_debug(lam_mca_coll_did, 
		      "query: coll module %s is not available", 
		      ls->mca_module_name);
	fprintf(stderr, "query: coll module %s is not available\n", 
                ls->mca_module_name);
#endif
 
	if (ls->mca_close_module != NULL)
	    ls->mca_close_module();

    } else {
    
#if 0
	if (lam_mca_coll_verbose >= 10)
	    lam_debug(lam_mca_coll_did, "query: coll module %s available", 
		      ls->mca_module_name);
	fprintf(stderr, "query: coll module %s available\n", 
		ls->mca_module_name);
#endif
    }      
    return ret;
}


static int 
thread_query_1_0_0(mca_base_module_t *module, 
		   mca_base_module_priority_list_item_t *entry)
{
    mca_coll_base_module_1_0_0_t *coll = 
	(mca_coll_base_module_1_0_0_t *) module;

    return coll->collm_init_query(&(entry->mpli_thread_min), 
				  &(entry->mpli_thread_max));
}
