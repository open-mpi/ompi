/*
 * $HEADER$
 */

extern const mca_base_component_t mca_ptl_tcp_component;
extern const mca_base_component_t mca_ptl_self_component;
extern const mca_base_component_t mca_ptl_prof_component;

const mca_base_component_t *mca_ptl_base_static_components[] = {
  &mca_ptl_tcp_component, 
  NULL
};
