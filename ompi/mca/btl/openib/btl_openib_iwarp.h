extern uint64_t mca_btl_openib_get_iwarp_subnet_id(struct ibv_device *ib_dev);
extern uint32_t mca_btl_openib_rdma_get_ipv4addr(struct ibv_context *verbs, uint8_t port);
extern int mca_btl_openib_build_rdma_addr_list(void);
extern void mca_btl_openib_free_rdma_addr_list(void);
