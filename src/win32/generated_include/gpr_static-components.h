/*
 * $HEADER$
 */

extern const mca_base_component_t mca_gpr_replica_component;
extern const mca_base_component_t mca_gpr_proxy_component;

const mca_base_component_t *mca_gpr_base_static_components[] = {
  &mca_gpr_replica_component, 
  &mca_gpr_proxy_component, 
  NULL
};
