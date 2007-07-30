/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_gpr_null_component;
extern const mca_base_component_t mca_gpr_proxy_component;
extern const mca_base_component_t mca_gpr_replica_component;

const mca_base_component_t *mca_gpr_base_static_components[] = {
  &mca_gpr_null_component, 
  &mca_gpr_proxy_component, 
  &mca_gpr_replica_component, 
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

