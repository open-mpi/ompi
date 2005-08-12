mca_bml_base_module_t* mca_bml_base_module_lookup(const char* name)
{
    opal_list_item_t* item;

    for (item = opal_list_get_first(&mca_bml_base_modules);
         item != opal_list_get_end(&mca_bml_base_modules);
         item = opal_list_get_next(item)) {
        mca_mpool_base_selected_module_t *mli =
            (mca_mpool_base_selected_module_t *) item;
        if(0 == strcmp(mli->mpool_component->mpool_version.mca_component_name,
                       name)) {
            return mli->mpool_module;
        }
    }

    return NULL;
}
