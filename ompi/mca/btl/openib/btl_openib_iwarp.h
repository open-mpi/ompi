extern uint64_t get_iwarp_subnet_id(struct ibv_device *ib_dev);
extern uint32_t rdma_get_ipv4addr(struct ibv_context *verbs, uint8_t port);
extern int build_rdma_addr_list(void);
extern void free_rdma_addr_list(void);
