/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_rmaps_round_robin_component;

const mca_base_component_t *mca_rmaps_base_static_components[] = {
  &mca_rmaps_round_robin_component, 
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

