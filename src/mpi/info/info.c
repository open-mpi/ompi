/**
 * $HEADER$
 */

#include "mpi/info/info.h"

/*
 * lam_info_t classes
 */
lam_class_info_t lam_info_cls = { "lam_info_t",
                                  &lam_list_cls,
                                  (class_init_t)lam_info_init,
                                  (class_destroy_t)lam_info_destroy};

/*
 * lam_info_entry_t classes
 */
lam_class_info_t lam_info_entry_cls = {
                            "lam_info_entry_t",
                            &lam_list_item_cls,
                            (class_init_t)lam_info_entry_init,
                            (class_destroy_t)lam_info_entry_destroy};

/*
 * lam_info_t interface functions
 */
void lam_info_init(lam_info_t *info) {
    SUPER_INIT(info, lam_info_cls.cls_parent);
    info->i_fhandle = -1;
}

void lam_info_destroy(lam_info_t *info) {
    SUPER_DESTROY(info, lam_info_cls.cls_parent);
}

/*
 * lam_info_entry_t interface functions
 */
void lam_info_entry_init(lam_info_entry_t *entry) {
    SUPER_INIT(entry, lam_list_item_cls.cls_parent);
    memset(entry->ie_key, 0, sizeof(entry->ie_key));
    entry->ie_key[MPI_MAX_INFO_KEY] = 0;
}

void lam_info_entry_destroy(lam_info_entry_t *entry) {
    SUPER_DESTROY(entry, lam_list_item_cls.cls_parent);
    if (NULL != entry->ie_value) {
      free(entry->ie_value);
    }
}
