/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_coll_basic_component;
extern const mca_base_component_t mca_coll_hierarch_component;
extern const mca_base_component_t mca_coll_self_component;
extern const mca_base_component_t mca_coll_sm_component;
extern const mca_base_component_t mca_coll_tuned_component;

const mca_base_component_t *mca_coll_base_static_components[] = {
  &mca_coll_basic_component, 
  &mca_coll_hierarch_component, 
  &mca_coll_self_component, 
  &mca_coll_sm_component, 
  &mca_coll_tuned_component, 
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

