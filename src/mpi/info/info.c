/**
 * $HEADER$
 */

#include "mpi/info/info.h"

/*
 * lam_info_t classes
 */
lam_class_info_t lam_info_t_class_info = {
    "lam_info_t",
    CLASS_INFO(lam_list_t),
    (lam_construct_t)lam_info_construct,
    (lam_destruct_t)lam_info_destruct
};

/*
 * lam_info_entry_t classes
 */
lam_class_info_t lam_info_entry_t_class_info = {
    "lam_info_entry_t",
    CLASS_INFO(lam_list_item_t),
    (lam_construct_t)lam_info_entry_construct,
    (lam_destruct_t)lam_info_entry_destruct
};

/*
 * lam_info_t interface functions
 */
void lam_info_construct(lam_info_t *info) {
    OBJ_CONSTRUCT_SUPER(info, lam_list_t);
    info->i_fhandle = -1;
}

void lam_info_destruct(lam_info_t *info) {
    OBJ_DESTRUCT_SUPER(info, lam_list_t);
}

/*
 * lam_info_entry_t interface functions
 */
void lam_info_entry_construct(lam_info_entry_t *entry) {
    OBJ_CONSTRUCT_SUPER(entry, lam_object_t);
    memset(entry->ie_key, 0, sizeof(entry->ie_key));
    entry->ie_key[MPI_MAX_INFO_KEY] = 0;
}

void lam_info_entry_destruct(lam_info_entry_t *entry) {
    OBJ_DESTRUCT_SUPER(entry, lam_object_t);
    if (NULL != entry->ie_value) {
      free(entry->ie_value);
    }
}
