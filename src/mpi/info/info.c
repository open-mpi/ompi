/**
 * $HEADER$
 */

#include "mpi/info/info.h"

/*
 * lam_info_t classes
 */
lam_class_t lam_info_t_class = {
    "lam_info_t",
    OBJ_CLASS(lam_list_t),
    (lam_construct_t)lam_info_construct,
    (lam_destruct_t)lam_info_destruct
};

/*
 * lam_info_entry_t classes
 */
lam_class_t lam_info_entry_t_class = {
    "lam_info_entry_t",
    OBJ_CLASS(lam_list_item_t),
    (lam_construct_t)lam_info_entry_construct,
    (lam_destruct_t)lam_info_entry_destruct
};

/*
 * lam_info_t interface functions
 */
void lam_info_construct(lam_info_t *info) {
    info->i_fhandle = -1;
}

void lam_info_destruct(lam_info_t *info) {
}

/*
 * lam_info_entry_t interface functions
 */
void lam_info_entry_construct(lam_info_entry_t *entry) {
    memset(entry->ie_key, 0, sizeof(entry->ie_key));
    entry->ie_key[MPI_MAX_INFO_KEY] = 0;
}

void lam_info_entry_destruct(lam_info_entry_t *entry) {
    if (NULL != entry->ie_value) {
      free(entry->ie_value);
    }
}

/**
 *   MPI_Info_dup - Duplicate an 'MPI_Info' object
 *
 *   @param info source info object (handle)
 *   @param newinfo pointer to the new info object (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_SYSRESOURCE
 *
 *   Not only will the (key, value) pairs be duplicated, the order of keys
 *   will be the same in 'newinfo' as it is in 'info'.
 *   When an info object is no longer being used, it should be freed with
 *   'MPI_Info_free'.
 */
int lam_info_dup (lam_info_t *info, lam_info_t **newinfo) {
    int err;
    int nkeys;
    lam_info_entry_t *iterator;

    err = lam_info_get_nkeys (info, &nkeys);

    for (iterator = (lam_info_entry_t *)lam_list_get_first(&(info->super));
         nkeys > 0;
         nkeys--) {
         err = lam_info_set(*newinfo, iterator->ie_key, iterator->ie_value);
         if (MPI_SUCCESS != err) {
            return err;
         }
         iterator = (lam_info_entry_t *)iterator->super.lam_list_next;
     }
     return MPI_SUCCESS;
}


/*
 * Set a new key,value pair on info
 *
 * @param info pointer to lam_info_t object
 * @param key pointer to the new key object
 * @param value pointer to the new value object
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_SYSRESOURCE
 */
int lam_info_set (lam_info_t *info, char *key, char *value) {
    char *new_value;
    lam_info_entry_t *new_info;
    lam_info_entry_t *old_info;
    int value_length;

    value_length = strlen(value);
    new_value = malloc(value_length * sizeof(char));
    if (NULL == new_value) {
         return MPI_ERR_SYSRESOURCE;
    }

    strcpy (new_value, value);
    old_info = lam_info_find_key (info, key);

    if (NULL != old_info) {
        /*
         * key already exists. remove the value associated with it
         */
         free(old_info->ie_value);
         old_info->ie_value = new_value;
    } else {
         new_info = OBJ_NEW(lam_info_entry_t);
         if (NULL == new_info) {
            return MPI_ERR_SYSRESOURCE;
         }
        strcpy (new_info->ie_key, key);
        new_info->ie_value = new_value;
        lam_list_append (&(info->super), (lam_list_item_t *) new_info);
    }
    return MPI_SUCCESS;
}

/**
 * lam_info_free - Free an 'MPI_Info' object.
 *
 *   @param info pointer to info (lam_info_t *) object to be freed (handle)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *
 *   Upon successful completion, 'info' will be set to 'MPI_INFO_NULL'.
 */
int lam_info_free (lam_info_t **info) {
    lam_info_entry_t *iterator;
    int nkeys;
    int err;

    err = lam_info_get_nkeys(*info, &nkeys);
    /*
     * We could just get each element from the list and then call
     * MPI_Info_delete. But this causes unnecessary delay because
     * MPI_Info_delete has extra logic to it. So, do the simple
     * remove operation to save time.
     */
     for (iterator = (lam_info_entry_t *)lam_list_get_first(&((*info)->super));
          nkeys > 0;
          nkeys--) {
          iterator = (lam_info_entry_t *)iterator->super.lam_list_next;
          OBJ_RELEASE(iterator->super.lam_list_prev);
     }
     /*
      * Anju:
      * Add things to remove the fortran handle from the mapping table
      */
      OBJ_RELEASE(*info);
      *info = MPI_INFO_NULL;
      return MPI_SUCCESS;
}

/**
 *   lam_info_get- Get a (key, value) pair from an 'MPI_Info' object
 *
 *   @param info Pointer to lam_info_t object
 *   @param key null-terminated character string of the index key
 *   @param valuelen maximum length of 'value' (integer)
 *   @param value null-terminated character string of the value
 *   @param flag true (1) if 'key' defined on 'info', false (0) if not
 *               (logical)
 *
 *   @retval MPI_SUCCESS
 *
 *   In C and C++, 'valuelen' should be one less than the allocated space
 *   to allow for for the null terminator.
 */
int lam_info_get (lam_info_t *info, char *key, int valuelen,
    char *value, int *flag) {
    lam_info_entry_t *search;
    int value_length;

    search = lam_info_find_key (info, key);

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
               lam_strncpy(value, search->ie_value, valuelen);
               value[valuelen] = 0;
          }
    }
    return MPI_SUCCESS;
}

/**
 * Delete a (key,value) pair from "info"
 *
 * @param info lam_info_t pointer on which we need to operate
 * @param key The key portion of the (key,value) pair that
 *            needs to be deleted
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_NOKEY
 */
int lam_info_delete (lam_info_t *info, char *key) {
    lam_info_entry_t *search;
    lam_info_entry_t *found;

    search = lam_info_find_key (info, key);

    if (NULL == search){
         return MPI_ERR_INFO_NOKEY;
    } else {
         /*
          * An entry with this key value was found. Remove the item
          * and free the memory allocated to it
          */
          found = (lam_info_entry_t *)
          lam_list_remove_item (&(info->super),
          (lam_list_item_t *)search);
          OBJ_RELEASE(search);
    }
    return MPI_SUCCESS;
}

/**
 *   @param info - lam_info_t pointer object (handle)
 *   @param key - null-terminated character string of the index key
 *   @param valuelen - length of the value associated with 'key' (integer)
 *   @param flag - true (1) if 'key' defined on 'info', false (0) if not
 *   (logical)
 *
 *   @retval MPI_SUCCESS
 *   @retval MPI_ERR_ARG
 *   @retval MPI_ERR_INFO_KEY
 *
 *   The length returned in C and C++ does not include the end-of-string
 *   character.  If the 'key' is not found on 'info', 'valuelen' is left
 *   alone.
 */
int lam_info_get_valuelen (lam_info_t *info, char *key, int *valuelen,
                       int *flag) {
    lam_info_entry_t *search;

    search = lam_info_find_key (info, key);
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
    return MPI_SUCCESS;
}

