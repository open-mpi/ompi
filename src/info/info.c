/*
 * $HEADER$
 */

#include "info/info.h"
#include "include/constants.h"
#include "mpi/runtime/mpiruntime.h"
#include "util/output.h"


/*
 * Global variables
 */
ompi_info_t ompi_mpi_info_null;


/*
 * Local functions
 */
static void ompi_info_construct(ompi_info_t *info);
static void ompi_info_destruct(ompi_info_t *info);
static void ompi_info_entry_construct(ompi_info_entry_t *entry);
static void ompi_info_entry_destruct(ompi_info_entry_t *entry);
static ompi_info_entry_t *ompi_info_find_key (ompi_info_t *info, char *key);


/*
 * ompi_info_t classes
 */
OBJ_CLASS_INSTANCE(ompi_info_t,
                   ompi_list_t,
                   ompi_info_construct,
                   ompi_info_destruct);

/*
 * ompi_info_entry_t classes
 */
OBJ_CLASS_INSTANCE(ompi_info_entry_t,
                   ompi_list_item_t,
                   ompi_info_entry_construct,
                   ompi_info_entry_destruct);

/*
 * The global fortran <-> C translation table
 */
ompi_pointer_array_t *ompi_info_f_to_c_table;


/*
 * This function is called during ompi_init and initializes the
 * fortran to C translation table.
 */ 
int ompi_info_init(void) 
{
    /* initialize table */

    ompi_info_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
    if (NULL == ompi_info_f_to_c_table) {
        return OMPI_ERROR;
    }

    /* Create MPI_INFO_NULL */

    OBJ_CONSTRUCT(&ompi_mpi_info_null, ompi_info_t);
    ompi_mpi_info_null.i_fhandle = 0;

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Shut down MPI_Info handling
 */
int ompi_info_finalize(void) 
{
  size_t i, max;
  ompi_info_t *info;
  ompi_list_item_t *item;
  ompi_info_entry_t *entry;
  bool found = false;

  /* Destroy MPI_INFO_NULL */

  OBJ_DESTRUCT(&ompi_mpi_info_null);

  /* Go through the f2c table and see if anything is left.  Free them
     all. */

  max = ompi_pointer_array_get_size(ompi_info_f_to_c_table);
  for (i = 0; i < max; ++i) {
    info = ompi_pointer_array_get_item(ompi_info_f_to_c_table, i);

    /* If the info was freed but still exists because the user told us
       to never free handles, then do an OBJ_RELEASE it and all is
       well.  Then get the value again and see if it's actually been
       freed. */

    if (NULL != info && ompi_debug_handle_never_free && info->i_freed) {
      OBJ_RELEASE(info);
      info = ompi_pointer_array_get_item(ompi_info_f_to_c_table, i);
    } 

    /* If it still exists here and was never freed, then it's an
       orphan */

    if (NULL != info) {

      /* If the user wanted warnings about MPI object leaks, print out
         a message */

      if (!info->i_freed && ompi_debug_show_handle_leaks) {
        if (ompi_debug_show_handle_leaks) {
          ompi_output(0, "WARNING: MPI_Info still allocated at MPI_FINALIZE");
          for (item = ompi_list_get_first(&(info->super));
               ompi_list_get_end(&(info->super)) != item;
               item = ompi_list_get_next(item)) {
            entry = (ompi_info_entry_t *) item;
            ompi_output(0, "WARNING:   key=\"%s\", value=\"%s\"", 
                        entry->ie_key,
                        NULL != entry->ie_value ? entry->ie_value : "(null)");
            found = true;
          }
        }
        OBJ_RELEASE(info);
      }

      /* Don't bother setting each element back down to NULL; it would
         just take a lot of thread locks / unlocks and since we're
         destroying everything, it isn't worth it */

      if (!found && ompi_debug_show_handle_leaks) {
        ompi_output(0, "WARNING:   (no keys)");
      }
    }
  }
  
  /* All done -- release the table */

  OBJ_RELEASE(ompi_info_f_to_c_table);
  return OMPI_SUCCESS;
}


/*
 * Duplicate an info
 */
int ompi_info_dup (ompi_info_t *info, ompi_info_t **newinfo) 
{
    int err;
    ompi_list_item_t *item;
    ompi_info_entry_t *iterator;

    OMPI_THREAD_LOCK(info->i_lock);
    for (item = ompi_list_get_first(&(info->super));
         item != ompi_list_get_end(&(info->super));
         item = ompi_list_get_next(iterator)) {
         iterator = (ompi_info_entry_t *) item;
         err = ompi_info_set(*newinfo, iterator->ie_key, iterator->ie_value);
         if (MPI_SUCCESS != err) {
            OMPI_THREAD_UNLOCK(info->i_lock);
            return err;
         }
     }
    OMPI_THREAD_UNLOCK(info->i_lock);
     return MPI_SUCCESS;
}


/*
 * Set a value on the info
 */
int ompi_info_set (ompi_info_t *info, char *key, char *value) 
{
    char *new_value;
    ompi_info_entry_t *new_info;
    ompi_info_entry_t *old_info;

    new_value = strdup(value);
    if (NULL == new_value) {
      return MPI_ERR_NO_MEM;
    }

    OMPI_THREAD_LOCK(info->i_lock);
    old_info = ompi_info_find_key (info, key);
    if (NULL != old_info) {
        /*
         * key already exists. remove the value associated with it
         */
         free(old_info->ie_value);
         old_info->ie_value = new_value;
    } else {
         new_info = OBJ_NEW(ompi_info_entry_t);
         if (NULL == new_info) {
            OMPI_THREAD_UNLOCK(info->i_lock);
            return MPI_ERR_NO_MEM;
         }
        strcpy (new_info->ie_key, key);
        new_info->ie_value = new_value;
        ompi_list_append (&(info->super), (ompi_list_item_t *) new_info);
    }
    OMPI_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Free an info handle and all of its keys and values.
 */
int ompi_info_free (ompi_info_t **info) 
{
    (*info)->i_freed = true;
    OBJ_RELEASE(*info);
    *info = MPI_INFO_NULL;
    return MPI_SUCCESS;
}


/*
 * Get a value from an info
 */
int ompi_info_get (ompi_info_t *info, char *key, int valuelen,
                   char *value, int *flag) 
{
    ompi_info_entry_t *search;
    int value_length;

    OMPI_THREAD_LOCK(info->i_lock);
    search = ompi_info_find_key (info, key);
    if (NULL == search){
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag, value_length and value
         */
         *flag = 1;
         value_length = strlen(search->ie_value);
         /*
          * If the stored value is shorter than valuelen, then
          * we can copy the entire value out. Else, we have to
          * copy ONLY valuelen bytes out
          */
          if (value_length < valuelen ) {
               strcpy(value, search->ie_value);
          } else {
               ompi_strncpy(value, search->ie_value, valuelen);
               value[valuelen] = 0;
          }
    }
    OMPI_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Delete a key from an info
 */
int ompi_info_delete (ompi_info_t *info, char *key) 
{
    ompi_info_entry_t *search;
    ompi_info_entry_t *found;

    OMPI_THREAD_LOCK(info->i_lock);
    search = ompi_info_find_key (info, key);
    if (NULL == search){
         OMPI_THREAD_UNLOCK(info->i_lock);
         return MPI_ERR_INFO_NOKEY;
    } else {
         /*
          * An entry with this key value was found. Remove the item
          * and free the memory allocated to it
          */
          found = (ompi_info_entry_t *)
            ompi_list_remove_item (&(info->super),
                                   (ompi_list_item_t *)search);
          OBJ_RELEASE(search);
    }
    OMPI_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Return the length of a value
 */
int ompi_info_get_valuelen (ompi_info_t *info, char *key, int *valuelen,
                            int *flag) 
{
    ompi_info_entry_t *search;

    OMPI_THREAD_LOCK(info->i_lock);
    search = ompi_info_find_key (info, key);
    if (NULL == search){
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag, value_length and value
         */
         *flag = 1;
         *valuelen = strlen(search->ie_value);
    }
    OMPI_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * Get the nth key
 */
int ompi_info_get_nthkey (ompi_info_t *info, int n, char *key)
{
    ompi_info_entry_t *iterator;

    /*
     * Iterate over and over till we get to the nth key
     */
    OMPI_THREAD_LOCK(info->i_lock);
    for (iterator = (ompi_info_entry_t *)ompi_list_get_first(&(info->super));
         n > 0;
         --n) {
         iterator = (ompi_info_entry_t *)ompi_list_get_next(iterator);
         if (ompi_list_get_end(&(info->super)) == 
             (ompi_list_item_t *) iterator) {
             OMPI_THREAD_UNLOCK(info->i_lock);
             return MPI_ERR_ARG;
         }
    }
    /*
     * iterator is of the type ompi_list_item_t. We have to
     * cast it to ompi_info_entry_t before we can use it to
     * access the value
     */
    strcpy(key, iterator->ie_key);
    OMPI_THREAD_UNLOCK(info->i_lock);
    return MPI_SUCCESS;
}


/*
 * This function is invoked when OBJ_NEW() is called. Here, we add this
 * info pointer to the table and then store its index as the handle
 */
static void ompi_info_construct(ompi_info_t *info) 
{
    info->i_fhandle = ompi_pointer_array_add(ompi_info_f_to_c_table, info);
    info->i_lock = OBJ_NEW(ompi_mutex_t);
    info->i_freed = false;

    /* If the user doesn't want us to ever free it, then add an extra
       RETAIN here */

    if (ompi_debug_handle_never_free) {
      OBJ_RETAIN(&(info->super));
    }
}


/*
 * This function is called during OBJ_DESTRUCT of "info". When this 
 * done, we need to remove the entry from the ompi fortran to C 
 * translation table
 */ 
static void ompi_info_destruct(ompi_info_t *info) 
{
  ompi_list_item_t *item;
  ompi_info_entry_t *iterator;

  /* Remove every key in the list */
  
  for (item = ompi_list_remove_first(&(info->super));
       NULL != item;
       item = ompi_list_remove_first(&(info->super))) {
    iterator = (ompi_info_entry_t *) item;
    OBJ_RELEASE(iterator);
  }

  /* reset the ompi_info_f_to_c_table entry - make sure that the
     entry is in the table */

  if (NULL != ompi_pointer_array_get_item(ompi_info_f_to_c_table, 
                                          info->i_fhandle)){
    ompi_pointer_array_set_item(ompi_info_f_to_c_table, 
                                info->i_fhandle, NULL);
  }
}


/*
 * ompi_info_entry_t interface functions
 */
static void ompi_info_entry_construct(ompi_info_entry_t *entry) 
{
    memset(entry->ie_key, 0, sizeof(entry->ie_key));
    entry->ie_key[MPI_MAX_INFO_KEY] = 0;
}


static void ompi_info_entry_destruct(ompi_info_entry_t *entry) 
{
    if (NULL != entry->ie_value) {
      free(entry->ie_value);
    }
}


/*
 * Find a key
 *
 * Do NOT thread lock in here -- the calling function is responsible
 * for that.
 */
static ompi_info_entry_t *ompi_info_find_key (ompi_info_t *info, char *key)
{
    ompi_info_entry_t *iterator;

    /* No thread locking in here! */

    /* Iterate over all the entries. If the key is found, then 
     * return immediately. Else, the loop will fall of the edge
     * and NULL is returned
     */
    for (iterator = (ompi_info_entry_t *)ompi_list_get_first(&(info->super));
         ompi_list_get_end(&(info->super)) != (ompi_list_item_t*) iterator;
         iterator = (ompi_info_entry_t *)ompi_list_get_next(iterator)) {
        if (0 == strcmp(key, iterator->ie_key)) {
            return iterator;
        }
    }
    return NULL;
}



