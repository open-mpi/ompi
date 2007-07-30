/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_pls_proxy_component;
extern const mca_base_component_t mca_pls_process_component;

const mca_base_component_t *mca_pls_base_static_components[] = {
  &mca_pls_proxy_component, 
  &mca_pls_process_component, 
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

