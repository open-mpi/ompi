/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_errmgr_hnp_component;
extern const mca_base_component_t mca_errmgr_orted_component;
extern const mca_base_component_t mca_errmgr_proxy_component;

const mca_base_component_t *mca_errmgr_base_static_components[] = {
  &mca_errmgr_hnp_component, 
  &mca_errmgr_orted_component, 
  &mca_errmgr_proxy_component, 
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

