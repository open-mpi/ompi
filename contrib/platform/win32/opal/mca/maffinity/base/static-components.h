/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_maffinity_first_use_component;

const mca_base_component_t *mca_maffinity_base_static_components[] = {
  &mca_maffinity_first_use_component, 
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

