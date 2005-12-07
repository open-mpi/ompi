/*
 * $HEADER$
 */

extern const mca_base_component_t mca_pls_rsh_component;
extern const mca_base_component_t mca_pls_proxy_component;
extern const mca_base_component_t mca_pls_fork_component;

const mca_base_component_t *mca_pls_base_static_components[] = {
   &mca_pls_rsh_component, 
   &mca_pls_proxy_component, 
   &mca_pls_fork_component, 
  NULL
};
