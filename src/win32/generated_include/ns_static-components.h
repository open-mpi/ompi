/*
 * $HEADER$
 */

extern const mca_base_component_t mca_ns_replica_component;
extern const mca_base_component_t mca_ns_proxy_component;

const mca_base_component_t *mca_ns_base_static_components[] = {
  &mca_ns_replica_component, 
  &mca_ns_proxy_component, 
  NULL
};
