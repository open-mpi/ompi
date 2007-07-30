/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern const mca_base_component_t mca_sds_env_component;
extern const mca_base_component_t mca_sds_seed_component;
extern const mca_base_component_t mca_sds_singleton_component;
extern const mca_base_component_t mca_sds_pipe_component;

const mca_base_component_t *mca_sds_base_static_components[] = {
    &mca_sds_env_component,
    &mca_sds_seed_component,
    &mca_sds_singleton_component,
    &mca_sds_pipe_component,
    NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

