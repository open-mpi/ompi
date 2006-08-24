#define IB
#include "ib.h"

double t;

/* is IB initialized? */
static int IB_Ginitialized=0;
/* the keyval (global) */
static int IB_Gkeyval=MPI_KEYVAL_INVALID; 

static struct {
  VAPI_hca_hndl_t hca_hndl;
  VAPI_pd_hndl_t pd_hndl;

  hb_tree *memlist; /* this is the libdict structure to hang off the search tree */
} IB_Hca_info;

/* function definitions */
static __inline__ void IB_Memlist_memlist_delete(IB_Memlistel *entry);
static __inline__ void IB_Memlist_delete_key(IB_Memlistel *k);
static __inline__ int IB_Memlist_compare_entries(IB_Memlistel *a, IB_Memlistel *b, void *param);
static __inline__ void IB_Taglist_delete(IB_Taglstel *entry);
static __inline__ void IB_Taglist_delete_key(IB_Taglstel *k);
static __inline__ int IB_Taglist_compare_entries(IB_Taglstel *a, IB_Taglstel *b, void *param);

#if 0
static __inline__ int IB_Addtotaglst(IB_Comminfo *comminfo, int tag, IB_Req *req, int peer) {
  IB_Taglstel *new;

  new = malloc(sizeof(IB_Taglstel));
  new->tag = tag;
  new->peer = peer;
  new->req = req;
  new->next = NULL;

  /* first element in list */
  if(comminfo->taglistend == NULL) {
    comminfo->taglisthead = new;
  } else {
    comminfo->taglistend->next = new;
  }
  //printf("added tag %i/peer %i as new element to taglist\n", tag, peer);
  comminfo->taglistend = new;

  return IB_OK;
}

static __inline__ IB_Req *IB_Gettagreq(IB_Comminfo *comminfo, int tag, int peer) {
  IB_Taglstel *ptr, *tmp;
  IB_Req *req;

  /* empty list */
  if(comminfo->taglisthead == NULL) return NULL;

  ptr = comminfo->taglisthead;
  /* tag/peer is in first element */
  if((ptr->tag == tag) && (ptr->peer == peer)) {
    req = comminfo->taglisthead->req;
    comminfo->taglisthead = ptr->next;
    /* list is empty */
    if(comminfo->taglisthead == NULL) comminfo->taglistend = NULL;
    free(ptr);
    //printf("removed tag %i\n", tag);
    return req;
  }
  
  while ((ptr->next != NULL)) {
    if((ptr->next->tag == tag) && (ptr->next->peer == peer)) break;
    ptr = ptr->next;
  }
  
  if(ptr->next == NULL) 
    /* we did not find it */
    return NULL;
  else  {
    /* we found it somewhere in the middle */
    req = ptr->next->req;
    tmp = ptr->next;
    ptr->next = ptr->next->next;
    /* it was the last element */
    if(ptr->next == NULL) comminfo->taglistend = ptr;
    free(tmp);
    //printf("removed tag %i\n", tag);
    return req;
  }
}
#endif

static int IB_Create_qp(	int rank, int remote, VAPI_hca_hndl_t *hca_hndl_p, VAPI_cq_hndl_t *sr_cq_hndl_p, VAPI_cq_hndl_t *rr_cq_hndl_p, VAPI_qp_hndl_t *qp_hndl_p, VAPI_pd_hndl_t *pd_hndp_p, MPI_Comm comm);

static int IB_Key_copy(MPI_Comm oldcomm, int keyval, void *extra_state, void *attribute_val_in, void *attribute_val_out, int *flag) {
  /* delete the attribute in the new comm  - it will be created at the
   * first usage */
  *flag = 0;
  
  return MPI_SUCCESS;
} 
  
static int IB_Key_delete(MPI_Comm comm, int keyval, void *attribute_val, void *extra_state) {
  IB_Comminfo *comminfo;
  
  if(keyval == IB_Gkeyval) {
    comminfo=(IB_Comminfo*)attribute_val;
    free(comminfo);
  } else {
    printf("Got wrong keyval!(%i)\n", keyval); 
  }
  
  return MPI_SUCCESS;
} 

static __inline__ void IB_stat( VAPI_ret_t ret, char *string ) {
  int rank;
  
  if( ret != VAPI_OK) {
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    printf("[%u]: *** [ERROR] *** %s -> %s (%s)\n", rank, string, VAPI_strerror( ret ), VAPI_strerror_sym( ret ));
//    return;
    MPI_Finalize();
    exit(1); 
  }
} 

static __inline__ void IB_CQ_stat( VAPI_wc_desc_t ret, char *string ) {
  int rank;

  if(ret.status != VAPI_SUCCESS) {
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    printf("[%u]: *** [ERROR] *** %s -> %s\n", rank, string, VAPI_wc_status_sym( ret.status ));
    printf("[%u]: *** [ERROR] *** opcode -> %s\n", rank, VAPI_cqe_opcode_sym(ret.opcode));
//    return;
    MPI_Finalize();
    exit(1);
  }
}

static int IB_Init() {
  u_int32_t num_of_hcas; /* actual number of hcas */
  VAPI_hca_id_t *hca_id_buf_p; /* HCA result buffer */
  int ret, res;
  
  /* keyval is not initialized yet, we have to init it */
  if(MPI_KEYVAL_INVALID == IB_Gkeyval) {
    res = MPI_Keyval_create(IB_Key_copy, IB_Key_delete, &(IB_Gkeyval), NULL);
    if((MPI_SUCCESS != res)) { printf("Error in MPI_Keyval_create() (%i)\n", res); return IB_OOR; }
  }

  hca_id_buf_p = malloc(sizeof(VAPI_hca_id_t) * 2);
  
  /* get all HCAs */
  ret = EVAPI_list_hcas(  (u_int32_t)2,
                          &num_of_hcas,
                          hca_id_buf_p  );
  if( ret == VAPI_OK) {
    fprintf(stderr, "[INFO] found %d adapter(s), first-name: %s\n", num_of_hcas, (char *)hca_id_buf_p);
  } else {
    IB_stat( ret, "EVAPI_list_hcas()" );
  }

  /* get handle of first HCA */
  ret = EVAPI_get_hca_hndl( *hca_id_buf_p, &IB_Hca_info.hca_hndl );
  IB_stat( ret, "EVAPI_get_hca_hndl()" );
 
  // allocate PD
  ret = VAPI_alloc_pd(  IB_Hca_info.hca_hndl,
                &IB_Hca_info.pd_hndl  );
  IB_stat( ret, "VAPI_alloc_pd()" );

  IB_Hca_info.memlist = hb_tree_new((dict_cmp_func)IB_Memlist_compare_entries, (void *) IB_Memlist_delete_key, (void *)IB_Memlist_memlist_delete);
  if(IB_Hca_info.memlist == NULL) { printf("error in hb_dict_new()\n"); return IB_OOR; }

  IB_Ginitialized = 1;

  return IB_OK;
}

static __inline__ IB_Comminfo *IB_Comm_init(MPI_Comm comm) {
  IB_Comminfo *comminfo;
  int res, flag;

  if(!IB_Ginitialized) IB_Init();

  res = MPI_Attr_get(comm, IB_Gkeyval, &comminfo, &flag);
  if((MPI_SUCCESS != res)) { printf("Error in MPI_Attr_get() (%i)\n", res); return NULL; }
  if (!flag) {
    VAPI_cqe_num_t  num_of_entries_p; /* # CQ entries */
    VAPI_mrw_t req_mrw_p;
    VAPI_mrw_t rep_mrw_p; /* responded memory region */
    IB_Peer_info *a2abuf1, *a2abuf2, *a2abuf3;
    int p, i, j, rank;
    
    res = MPI_Comm_size(comm, &p);
    res = MPI_Comm_rank(comm, &rank);
  
    /* we have to create a new one */
    comminfo = malloc(sizeof(IB_Comminfo));
    if(comminfo == NULL) { printf("Error in malloc()\n"); return NULL; }

    comminfo->taglisthead = NULL;
    comminfo->taglistend = NULL;

    //printf("[%i] build up %i connections in comm %p \n", rank, p-1, comm);
    /* allocate QPs */
    comminfo->qp_hndl_arr = malloc(p*sizeof(VAPI_qp_hndl_t));
    if(comminfo->qp_hndl_arr == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate SR CQs */
    comminfo->sr_cq_hndl_arr = malloc(p*sizeof(VAPI_cq_hndl_t));
    if(comminfo->sr_cq_hndl_arr == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate RR CQs */
    comminfo->rr_cq_hndl_arr = malloc(p*sizeof(VAPI_cq_hndl_t));
    if(comminfo->rr_cq_hndl_arr == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate a tag list for each host */
    comminfo->taglist=malloc(p*sizeof(hb_tree*));
    if(comminfo->taglist == NULL) { printf("malloc() error\n"); return NULL; }
    for(i=0; i<p;i++) {
      comminfo->taglist[i] = hb_tree_new((dict_cmp_func)IB_Taglist_compare_entries, (void *) IB_Taglist_delete_key, (void *)IB_Taglist_delete);
      if(comminfo->taglist[i] == NULL) { printf("hb_tree_new() error\n"); return NULL; }
    }

    /* allocate rtr send queue */
    comminfo->rtr_send=malloc(IB_RTR_SIZE*sizeof(IB_Peer_info_tag));
    if(comminfo->rtr_send == NULL) { printf("malloc() error\n"); return NULL; }
    for(i=0; i<IB_RTR_SIZE;i++) comminfo->rtr_send[i].tag=-1;
    /* allocate rtr queue */
    comminfo->rtr=malloc(p*sizeof(IB_Peer_info_tag*));
    if(comminfo->rtr== NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate rtr free queue */
    comminfo->rtr_peer_free=malloc(p*sizeof(int*));
    if(comminfo->rtr_peer_free == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate rtr info */
    comminfo->rtr_info=malloc(p*sizeof(IB_Peer_info));
    if(comminfo->rtr_info == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate rtr l_key */
    comminfo->rtr_l_key=malloc(p*sizeof(VAPI_lkey_t));
    if(comminfo->rtr_l_key == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate rtr memory region handle */
    comminfo->rtr_mr_hndl_p=malloc(p*sizeof(VAPI_mr_hndl_t));
    if(comminfo->rtr_mr_hndl_p == NULL) { printf("malloc() error\n"); return NULL; }
    
    /* allocate eager send queue */
    comminfo->eager_send=malloc(IB_EAGER_SIZE*sizeof(IB_Eager_data));
    if(comminfo->eager_send == NULL) { printf("malloc() error\n"); return NULL; }
    for(i=0; i<IB_EAGER_SIZE;i++) comminfo->eager_send[i].tag=-1;
    /* allocate eager queue */
    comminfo->eager=malloc(p*sizeof(IB_Eager_data*));
    if(comminfo->eager== NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate eager free queue */
    comminfo->eager_peer_free=malloc(p*sizeof(int*));
    if(comminfo->eager_peer_free == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate eager info */
    comminfo->eager_info=malloc(p*sizeof(IB_Peer_info));
    if(comminfo->eager_info == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate eager l_key */
    comminfo->eager_l_key=malloc(p*sizeof(VAPI_lkey_t));
    if(comminfo->eager_l_key == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate eager memory region handle */
    comminfo->eager_mr_hndl_p=malloc(p*sizeof(VAPI_mr_hndl_t));
    if(comminfo->eager_mr_hndl_p == NULL) { printf("malloc() error\n"); return NULL; }

    /* allocate eager free info */
    comminfo->eager_free_info=malloc(p*sizeof(IB_Peer_info));
    if(comminfo->eager_free_info == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate eager free memory region handle */
    comminfo->eager_free_mr_hndl_p=malloc(p*sizeof(VAPI_mr_hndl_t));
    if(comminfo->eager_free_mr_hndl_p == NULL) { printf("malloc() error\n"); return NULL; }
    
    /* allocate a2abuf1 */
    a2abuf1=malloc(p*sizeof(IB_Peer_info));
    if(a2abuf1 == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate a2abuf2 */
    a2abuf2=malloc(p*sizeof(IB_Peer_info));
    if(a2abuf2 == NULL) { printf("malloc() error\n"); return NULL; }
    /* allocate a2abuf3 */
    a2abuf3=malloc(p*sizeof(IB_Peer_info));
    if(a2abuf3 == NULL) { printf("malloc() error\n"); return NULL; }
    
    for(i = 0; i < p; i++) {
      if(i == rank) continue;
      res = VAPI_create_cq(IB_Hca_info.hca_hndl, 100000, &comminfo->sr_cq_hndl_arr[i], &num_of_entries_p );
      IB_stat( res, "VAPI_create_cq()" );
      res = VAPI_create_cq(IB_Hca_info.hca_hndl, 100000, &comminfo->rr_cq_hndl_arr[i], &num_of_entries_p );
      IB_stat( res, "VAPI_create_cq()" );

      res = IB_Create_qp(rank, i, &IB_Hca_info.hca_hndl, &(comminfo->sr_cq_hndl_arr[i]), &(comminfo->rr_cq_hndl_arr[i]), &(comminfo->qp_hndl_arr[i]), &IB_Hca_info.pd_hndl, comm );
      //printf("[%i] rank %i has sr_cq: %i and rr_cq: %i and qp_hndl: %i\n", rank, i, (int)comminfo->sr_cq_hndl_arr[i], (int)comminfo->rr_cq_hndl_arr[i], (int)comminfo->qp_hndl_arr[i]);
      if(res != 0) { printf("Error in IB_Create_qp (%i)\n", res); return NULL; }

      /* allocate rtr element */
      comminfo->rtr[i] = malloc(sizeof(IB_Peer_info_tag)*IB_RTR_SIZE);
      if(comminfo->rtr[i] == NULL) { printf("malloc() error\n"); return NULL; }
      for(j=0; j<IB_RTR_SIZE; j++) comminfo->rtr[i][j].tag = -1;
      /* allocate rtr free queue */
      comminfo->rtr_peer_free[i]=malloc(sizeof(int)*IB_RTR_SIZE);
      if(comminfo->rtr_peer_free[i] == NULL) { printf("malloc() error\n"); return NULL; }
      /* set free rtr queue to free :) */
      for(j=0; j<IB_RTR_SIZE; j++) comminfo->rtr_peer_free[i][j] = -1;
      
      /* allocate eager element */
      comminfo->eager[i] = malloc(sizeof(IB_Eager_data)*IB_EAGER_SIZE);
      if(comminfo->eager[i] == NULL) { printf("malloc() error\n"); return NULL; }
      for(j=0; j<IB_EAGER_SIZE; j++) { memset((void*)(&comminfo->eager[i][j]), 0, sizeof(IB_Eager_data)); }
      /* allocate free queue */
      comminfo->eager_peer_free[i]=malloc(sizeof(int)*IB_EAGER_SIZE);
      if(comminfo->eager_peer_free[i] == NULL) { printf("malloc() error\n"); return NULL; }
      /* set free queue to free :) */
      for(j=0; j<IB_EAGER_SIZE; j++) comminfo->eager_peer_free[i][j] = -1;
    }
  
    for(i=0; i<p;i++) {
      if (rank == i) continue;
      /* register rtr buffer */
      memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
      req_mrw_p.type = VAPI_MR;
      req_mrw_p.start = (VAPI_virt_addr_t)(comminfo->rtr[i]);
      req_mrw_p.size = sizeof(IB_Peer_info_tag)*IB_RTR_SIZE; 
      req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
      req_mrw_p.acl = VAPI_EN_LOCAL_WRITE |
                  VAPI_EN_REMOTE_WRITE |
                  VAPI_EN_REMOTE_READ;
    
      res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                      &req_mrw_p,
                      &comminfo->rtr_mr_hndl_p[i],
                      &rep_mrw_p  );
      IB_stat( res, "VAPI_register_mr() for RTR buffers" );

      comminfo->rtr_l_key[i] = rep_mrw_p.l_key;

      a2abuf1[i].r_key = (unsigned long)rep_mrw_p.r_key;
      a2abuf1[i].addr = (unsigned long)comminfo->rtr[i];
      //printf("[%i] my info - r_key: %lu addr for rank %i: %lu\n", rank, a2abuf[i].r_key, i, a2abuf[i].addr); 
      
      /* register eager buffer */
      memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
      req_mrw_p.type = VAPI_MR;
      req_mrw_p.start = (VAPI_virt_addr_t)(comminfo->eager[i]);
      req_mrw_p.size = sizeof(IB_Eager_data)*IB_EAGER_SIZE; 
      req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
      req_mrw_p.acl = VAPI_EN_LOCAL_WRITE |
                  VAPI_EN_REMOTE_WRITE |
                  VAPI_EN_REMOTE_READ;
    
      res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                      &req_mrw_p,
                      &comminfo->eager_mr_hndl_p[i],
                      &rep_mrw_p  );
      IB_stat( res, "VAPI_register_mr() for eager buffers" );

      comminfo->eager_l_key[i] = rep_mrw_p.l_key;

      a2abuf2[i].r_key = (unsigned long)rep_mrw_p.r_key;
      a2abuf2[i].addr = (unsigned long)comminfo->eager[i];
      
      /* register eager free buffer */
      memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
      req_mrw_p.type = VAPI_MR;
      req_mrw_p.start = (VAPI_virt_addr_t)(comminfo->eager_peer_free[i]);
      req_mrw_p.size = sizeof(int)*IB_EAGER_SIZE; 
      req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
      req_mrw_p.acl = VAPI_EN_LOCAL_WRITE |
                  VAPI_EN_REMOTE_WRITE |
                  VAPI_EN_REMOTE_READ;
    
      res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                      &req_mrw_p,
                      &comminfo->eager_free_mr_hndl_p[i],
                      &rep_mrw_p  );
      IB_stat( res, "VAPI_register_mr() for eager free buffer" );

      a2abuf3[i].r_key = (unsigned long)rep_mrw_p.r_key;
      a2abuf3[i].addr = (unsigned long)comminfo->eager_peer_free[i];
      //printf("[%i] my info - r_key: %lu addr for rank %i: %lu\n", rank, a2abuf1[i].r_key, i, a2abuf1[i].addr); 
      //printf("[%i] my info - r_key: %lu addr for rank %i: %lu\n", rank, a2abuf2[i].r_key, i, a2abuf2[i].addr); 
      //printf("[%i] my info - r_key: %lu addr for rank %i: %lu\n", rank, a2abuf3[i].r_key, i, a2abuf3[i].addr); 
    }
    MPI_Alltoall(a2abuf1, 2, MPI_UNSIGNED_LONG, comminfo->rtr_info, 2, MPI_UNSIGNED_LONG, comm);
    free(a2abuf1);
    MPI_Alltoall(a2abuf2, 2, MPI_UNSIGNED_LONG, comminfo->eager_info, 2, MPI_UNSIGNED_LONG, comm);
    free(a2abuf2);
    MPI_Alltoall(a2abuf3, 2, MPI_UNSIGNED_LONG, comminfo->eager_free_info, 2, MPI_UNSIGNED_LONG, comm);
    free(a2abuf3);
    for(i=0; i<p;i++) {
      if (rank == i) continue;
      //printf("[%i] rtr rem info - r_key: %lu addr for me at node %i: %lu\n", rank, comminfo->rtr_info[i].r_key, i, comminfo->rtr_info[i].addr); 
      //printf("[%i] eager rem info - r_key: %lu addr for me at node %i: %lu\n", rank, comminfo->eager_info[i].r_key, i, comminfo->eager_info[i].addr); 
      //printf("[%i] eager_free rem info - r_key: %lu addr for me at node %i: %lu\n", rank, comminfo->eager_free_info[i].r_key, i, comminfo->eager_free_info[i].addr); 
    }

    /* register rtr send buffer */
    memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
    req_mrw_p.type = VAPI_MR;
    req_mrw_p.start = (VAPI_virt_addr_t)(comminfo->rtr_send);
    req_mrw_p.size = sizeof(IB_Peer_info_tag)*IB_RTR_SIZE; 
    req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
    req_mrw_p.acl = VAPI_EN_LOCAL_WRITE |
                VAPI_EN_REMOTE_WRITE |
                VAPI_EN_REMOTE_READ;
    
    res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                    &req_mrw_p,
                    &comminfo->rtr_send_mr_hndl_p,
                    &rep_mrw_p  );
    IB_stat( res, "VAPI_register_mr()" );

    comminfo->rtr_send_l_key = rep_mrw_p.l_key;

    /* register eager send buffer */
    memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
    req_mrw_p.type = VAPI_MR;
    req_mrw_p.start = (VAPI_virt_addr_t)(comminfo->eager_send);
    req_mrw_p.size = sizeof(IB_Eager_data)*IB_EAGER_SIZE; 
    req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
    req_mrw_p.acl = VAPI_EN_LOCAL_WRITE |
                VAPI_EN_REMOTE_WRITE |
                VAPI_EN_REMOTE_READ;
    
    res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                    &req_mrw_p,
                    &comminfo->eager_send_mr_hndl_p,
                    &rep_mrw_p  );
    IB_stat( res, "VAPI_register_mr()" );

    comminfo->eager_send_l_key = rep_mrw_p.l_key;

    /* register empty send buffer */
    memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
    req_mrw_p.type = VAPI_MR;
    req_mrw_p.start = (VAPI_virt_addr_t)(&comminfo->empty);
    req_mrw_p.size = sizeof(int32_t); 
    req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
    req_mrw_p.acl = VAPI_EN_LOCAL_WRITE |
                VAPI_EN_REMOTE_WRITE |
                VAPI_EN_REMOTE_READ;
    
    res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                    &req_mrw_p,
                    &comminfo->eager_send_mr_hndl_p,
                    &rep_mrw_p  );
    IB_stat( res, "VAPI_register_mr()" );

    comminfo->empty_l_key = rep_mrw_p.l_key;

    comminfo->empty = -1;

    /* put the new attribute to the comm */
    res = MPI_Attr_put(comm, IB_Gkeyval, comminfo);
    if((MPI_SUCCESS != res)) { printf("Error in MPI_Attr_put() (%i)\n", res); return NULL; }
  }

  return comminfo;
}

static __inline__ int IB_Register_mem(void *buf, int size, VAPI_mr_hndl_t *mr, VAPI_rkey_t *r_key, VAPI_lkey_t *l_key) {
  VAPI_mrw_t req_mrw_p;
  VAPI_mrw_t rep_mrw_p; /* responded memory region */
  int res, rank;
  IB_Memlistel *memel, *newel, keyel;
  
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  //printf("[%i] in IB_Register_mem\n", rank);

  keyel.buf = buf;
  keyel.size = size;
  memel = hb_tree_search(IB_Hca_info.memlist, &keyel);
  if(memel != NULL) {
    //printf("[%i] we found a region from %lu to %lu :-)\n", rank, (unsigned long)memel->buf,(unsigned long)(memel->buf+memel->size));
    if(r_key != NULL) *r_key = memel->r_key;
    if(l_key != NULL) *l_key = memel->l_key;
    return IB_OK;
  }
  
  //printf("[%i] we did not find a region - registering %i bytes from addr %lu to %lu :-(\n", rank, size, (unsigned long)buf, (unsigned long)(buf+size));
  memset(&req_mrw_p, 0, sizeof(VAPI_mrw_t));
  req_mrw_p.type = VAPI_MR;
  req_mrw_p.start = (VAPI_virt_addr_t)buf;
  req_mrw_p.size = size; 
  req_mrw_p.pd_hndl = IB_Hca_info.pd_hndl;
  req_mrw_p.acl =   VAPI_EN_LOCAL_WRITE |
              VAPI_EN_REMOTE_WRITE |
              VAPI_EN_REMOTE_READ;
  
  res = VAPI_register_mr( IB_Hca_info.hca_hndl,
                  &req_mrw_p,
                  mr,
                  &rep_mrw_p  );
  IB_stat( res, "VAPI_register_mr()" );

  /* TODO: we should react to "Resources temporary unavailable" (EAGAIN)
   * and free some MRs ... to continue :) */
  newel = malloc(sizeof(IB_Memlistel));
  newel->buf = buf;
  newel->size = size;
  newel->mr=mr;
  newel->r_key=rep_mrw_p.r_key;
  newel->l_key=rep_mrw_p.l_key;
	res = hb_tree_insert (IB_Hca_info.memlist, newel, newel, 0);
  if(res != 0) printf("[%i] error in dict_insert() (%i) while inserting region from %lu to %lu\n", rank, res, (unsigned long)newel->buf, (unsigned long)(newel->buf+newel->size));
  //if(res == 0) printf("[%i] inserted region from %lu to %lu\n", rank, (unsigned long)newel->buf, (unsigned long)newel->top);
  
  if(r_key != NULL) *r_key = rep_mrw_p.r_key;
  if(l_key != NULL) *l_key = rep_mrw_p.l_key;

  return IB_OK;
}
  
static __inline__ int IB_Do_send(IB_Req *req) {
  int res;

  //printf("[%i] posting data SR from %p size: %i tag: %i to %i (addr: %lu, r_key: %u)\n", req->rank, (void*)req->sr_sg_lst.addr, req->sr_sg_lst.len, req->sr_desc.imm_data, req->peer, (unsigned long)req->sr_desc.remote_addr, req->sr_desc.r_key);
  res = VAPI_post_sr(  IB_Hca_info.hca_hndl, req->comminfo->qp_hndl_arr[req->peer], &req->sr_desc );
  IB_stat( res, "VAPI_post_sr()" );

  req->status = SEND_POSTED_SR;

  return IB_OK;
}

int IB_Isend(void *buf, int count, MPI_Datatype type, int dst, int tag, MPI_Comm comm, IB_Request *request) {
  int res,sendentry,i;
  MPI_Aint ext;
  IB_Req *req;
  char *flag;
  VAPI_sg_lst_entry_t sr_sg_lst; /* the IB SG list */
  VAPI_sr_desc_t sr_desc; /* the IB SR descr. */
  
  *request = malloc(sizeof(IB_Req));
  req = *request;
  
  if(!count) {
    req->status = SEND_DONE;
    return IB_OK;
  }

  MPI_Type_extent(type, &ext);
  
  req->comminfo = IB_Comm_init(comm); 
  if(req->comminfo == NULL) { printf("Error in IB_Comm_init()\n"); return IB_OOR; }

  if(count*ext <= IB_EAGER_LIMIT) {
    /* we send this as eager message */

    /* find free eager send buffer */
    for(sendentry=0; sendentry<IB_EAGER_SIZE; sendentry++) {
      if(req->comminfo->eager_send[sendentry].tag == -1) break;
    }
    if(sendentry==IB_EAGER_SIZE) { printf("*** eager send list full - we should retry later but crash\n"); exit(1); }
    //printf("[%i] found local eager sendbuffer entry %i\n", req->rank, sendentry);
    req->sendel = sendentry;

    /* copy data into buffer */
    memcpy(&req->comminfo->eager_send[sendentry].buf, buf, count*ext);
    /* set header */
    req->comminfo->eager_send[sendentry].size=count*ext;
    req->comminfo->eager_send[sendentry].tag=tag;
    /* set next byte after message to '1' */
    flag = (char*)&req->comminfo->eager_send[sendentry].buf + req->comminfo->eager_send[sendentry].size;
    *flag = (char)1;
    
    /* prepare eager send request */
    sr_sg_lst.addr = (VAPI_virt_addr_t)(&req->comminfo->eager_send[sendentry]);
    sr_sg_lst.len = sizeof(int32_t)+2*sizeof(int16_t)+sizeof(char)+req->comminfo->eager_send[sendentry].size; /* TODO: dangerous - datatype */
    sr_sg_lst.lkey = req->comminfo->eager_send_l_key;
    sr_desc.id = (u_int64_t)req;
    sr_desc.opcode = VAPI_RDMA_WRITE;
    sr_desc.set_se = 0;
    sr_desc.comp_type = VAPI_SIGNALED; 
    sr_desc.sg_lst_p = &sr_sg_lst; 
    sr_desc.sg_lst_len = 1; 
    sr_desc.imm_data = (u_int64_t)0; /* not used */ 
    sr_desc.fence = 0; 
    sr_desc.compare_add = 0; 
    sr_desc.r_key = (VAPI_rkey_t)(req->comminfo->eager_info[dst].r_key);
    /* get offset in the receiver's rtr eager array */
    for(i=0; i<IB_EAGER_SIZE; i++) {
      if(req->comminfo->eager_peer_free[dst][i] == -1) break;
    }
    if(i==IB_EAGER_SIZE) { printf("******************* [%i] eager list on peer %i full - we should retry later but crash\n", req->rank, dst); }
    req->comminfo->eager_peer_free[dst][i] = tag;
    sr_desc.remote_addr = req->comminfo->eager_info[dst].addr+i*sizeof(IB_Eager_data); /* TODO: 64 Bit */ 
    req->comminfo->eager_send[sendentry].index=i;
    //printf("[%i] found free eager slot index %i on node %i (addr: %lu) free)\n", req->rank, i, dst, sr_desc.remote_addr);
    
    res = VAPI_post_sr(  IB_Hca_info.hca_hndl, req->comminfo->qp_hndl_arr[dst], &sr_desc );
    IB_stat( res, "VAPI_post_sr()" );
    //printf("[%i] post EAGER SR from %lu to node %i addr %lu rkey: %u, len: %o\n", req->rank, sr_sg_lst.addr, req->peer, (unsigned long)sr_desc.remote_addr, sr_desc.r_key, sr_sg_lst.len);
    
      
    req->status = SEND_SENT_EAGER;
    //while(IB_Test(&req) != IB_OK);

  } else {
    /* we send this as rendezvous */

    /* register memory region for send */
    //printf("[%i] register send memory %lu size: %i\n", req->rank, (unsigned long)buf, (int)(count*ext));
    res = IB_Register_mem(buf, count*ext, &req->mr_hndl_p, NULL, &req->sr_sg_lst.lkey); /* TODO: count*ext Danger for datatypes ... */

    /* initialize sr_desc as far as we can (remote r_key and addr are
     * missing set after we received RTR */
    req->sr_sg_lst.addr = (VAPI_virt_addr_t)buf; 
    req->sr_sg_lst.len = count*ext;  /* TODO: count*ext Danger for datatypes ... */
    req->sr_desc.id = (u_int64_t)req; 
    req->sr_desc.opcode = VAPI_SEND_WITH_IMM; 
    req->sr_desc.set_se = 0;
    req->sr_desc.comp_type = VAPI_SIGNALED; 
    req->sr_desc.sg_lst_p = &req->sr_sg_lst; 
    req->sr_desc.sg_lst_len = 1; 
    req->sr_desc.imm_data = (u_int64_t)tag; 
    req->sr_desc.fence = 0; 
    req->sr_desc.compare_add= 0; 

    req->status = SEND_WAITING_RTR;
  }
  
  //IB_Test(&req);
  MPI_Comm_rank(comm, &req->rank);
  MPI_Comm_size(comm, &req->p);
  req->tag = tag;
  req->peer= dst;

  return IB_OK;
}

int IB_Irecv(void *buf, int count, MPI_Datatype type, int src, int tag, MPI_Comm comm, IB_Request *request) {
  int res, i, sendentry;
  MPI_Aint ext;
  VAPI_sg_lst_entry_t sr_sg_lst; /* the IB SG list */
  VAPI_sr_desc_t sr_desc; /* the IB SR descr. */
  VAPI_sg_lst_entry_t rr_sg_lst;
  VAPI_rr_desc_t rr_desc;
  IB_Req *req;
  IB_Taglstel *newel;

  *request = malloc(sizeof(IB_Req));
  req = *request;
  
  if(count == 0) {
    req->status = RECV_DONE;
    return IB_OK;
  }

  MPI_Type_extent(type, &ext);
  MPI_Comm_size(comm, &req->p);
  MPI_Comm_rank(comm, &req->rank);
  req->tag = tag;
  req->peer= src;
  req->buf=buf;
  
  req->comminfo = IB_Comm_init(comm); 
  if(req->comminfo == NULL) { printf("Error in IB_Comm_init()\n"); return IB_OOR; }
  
  if(count*ext <= IB_EAGER_LIMIT) {
    /* do nothing, wait for eager message */
    req->status = RECV_WAITING_EAGER;
    /* we take a look if we received it already */
    //IB_Test(&req);
  } else {
    /* find a new empty sendentry in the comminfo->send array which is
     * pre-registered to send RTR messages from */
    for(sendentry=0; sendentry<IB_RTR_SIZE; sendentry++) {
      if(req->comminfo->rtr_send[sendentry].tag == -1) break;
    }
    if(sendentry==IB_RTR_SIZE) { printf("*** rtr send list full - we should retry later but crash\n"); exit(1); }
    /* fill selected send entry */
    req->comminfo->rtr_send[sendentry].tag = tag;
    req->comminfo->rtr_send[sendentry].addr = (unsigned long)buf;
    /* remember index in sendlist to free it fast after sending */
    req->sendel = sendentry;
    
    /* register memory region for recv */
    res = IB_Register_mem(buf, count*ext, &req->mr_hndl_p, (VAPI_rkey_t*)&req->comminfo->rtr_send[sendentry].r_key, &rr_sg_lst.lkey); /* TODO: count*ext Danger for datatypes ... */

    /* prepare data receive request */
    rr_sg_lst.addr = (VAPI_virt_addr_t)buf;
    rr_sg_lst.len = count*ext; 
    rr_desc.sg_lst_p = &rr_sg_lst; 
    rr_desc.sg_lst_len = 1; 
    rr_desc.id = (u_int64_t)req; 
    rr_desc.opcode = VAPI_RECEIVE; 
    rr_desc.comp_type = VAPI_SIGNALED; 

    res = VAPI_post_rr( IB_Hca_info.hca_hndl, req->comminfo->qp_hndl_arr[src], &rr_desc );
    IB_stat( res, "VAPI_post_rr()" );

    /* add the tag and the peer to the taglist */
    /* legacy old crappy taglist :) */
    //res = IB_Addtotaglst(comminfo, tag, req, src);

    /* new fancy taglist */
    newel = malloc(sizeof(IB_Taglstel));
    newel->tag=tag;
    newel->req=req;
    res = hb_tree_insert(req->comminfo->taglist[src], newel, newel, 0);
    //printf("[%i] inserted request %lu with tag %i and src %i\n", req->rank, (unsigned long)req, tag, src);
   
    /* prepare RTR send request */
    sr_sg_lst.addr = (VAPI_virt_addr_t)(&req->comminfo->rtr_send[sendentry]);
    sr_sg_lst.len = sizeof(IB_Peer_info_tag);
    sr_sg_lst.lkey = req->comminfo->rtr_send_l_key; 
    sr_desc.id = (u_int64_t)req; 
    sr_desc.opcode = VAPI_RDMA_WRITE; 
    sr_desc.set_se = 0;
    sr_desc.comp_type = VAPI_SIGNALED; 
    sr_desc.sg_lst_p = &sr_sg_lst; 
    sr_desc.sg_lst_len = 1; 
    sr_desc.imm_data = (u_int64_t)0; /* not used */ 
    sr_desc.fence = 0; 
    sr_desc.compare_add = 0; 
    sr_desc.r_key = (VAPI_rkey_t)(req->comminfo->rtr_info[src].r_key);
    /* get offset in the receiver's rtr RTR array */
    for(i=0; i<IB_RTR_SIZE; i++) {
      if(req->comminfo->rtr_peer_free[src][i] == -1) break;
    }
    if(i==IB_RTR_SIZE) { printf("*** unexpected list full - we should retry later but crash\n"); }
    req->comminfo->rtr_peer_free[src][i] = tag;
    sr_desc.remote_addr = req->comminfo->rtr_info[src].addr+i*sizeof(IB_Peer_info_tag); /* TODO: 64 Bit */ 
    //printf("[%i] found free RTR slot index %i on node %i (addr: %lu) free)\n", req->rank, i, src, sr_desc.remote_addr);
    
    /* post RTR request */
    res = VAPI_post_sr(  IB_Hca_info.hca_hndl, req->comminfo->qp_hndl_arr[req->peer], &sr_desc );
    IB_stat( res, "VAPI_post_sr()" );
    //printf("[%i] post RTR SR from %lu to node %i addr %lu rkey: %u\n", req->rank, sr_sg_lst.addr, req->peer, (unsigned long)sr_desc.remote_addr, sr_desc.r_key);
    
    req->status = RECV_SENDING_RTR;
  }

  //IB_Test(&req);

  return IB_OK;
}

int IB_Test(IB_Request *request) {
  int i, j, res;
  VAPI_wc_desc_t comp_desc_p; /* work completion descriptor */
  IB_Req *tmpreq, *req;
  IB_Taglstel *memel, keyel;
  char *flag;

  req = *request;

  if((req->status == SEND_DONE) || (req->status == RECV_DONE) || (req->status == RECV_EAGER_DONE)) 
    return IB_OK;
  
  /* if I wait for RTR - search rtr array for my tag ... */
  if(req->status == SEND_WAITING_RTR) {
    for(i=0; i<IB_RTR_SIZE; i++) {
      if(req->comminfo->rtr[req->peer][i].tag == req->tag) {
        //printf("[%i] found RTR from peer %i at addr %lu (tag: %i, r_key: %lu, addr: %lu)\n", req->rank, req->peer, (unsigned long)(&req->comminfo->rtr[req->peer][i]), req->comminfo->rtr[req->peer][i].tag, req->comminfo->rtr[req->peer][i].r_key, (unsigned long)req->comminfo->rtr[req->peer][i].addr);
        req->sr_desc.r_key = (VAPI_rkey_t)req->comminfo->rtr[req->peer][i].r_key; 
        req->sr_desc.remote_addr = req->comminfo->rtr[req->peer][i].addr; /* TODO: 64 Bit */ 
        /* 'free' rtr element */
        req->comminfo->rtr[req->peer][i].tag = -1;
        IB_Do_send(req);
        break;
      }
    }
  }
  
  /* I wait for an eager message */
  if(req->status == RECV_WAITING_EAGER) {
    /* ok, poll all eager slots we have from the peer we wait for */
    for(i=0; i<IB_EAGER_SIZE; i++) {
      if(req->comminfo->eager[req->peer][i].tag == req->tag) {
        VAPI_sg_lst_entry_t sr_sg_lst; /* the IB SG list */
        VAPI_sr_desc_t sr_desc; /* the IB SR descr. */
        int index;
  
        /* poll last byte until we can be sure that we have the *full*
         * message in the buffer */
        flag = (char*)&req->comminfo->eager[req->peer][i].buf + req->comminfo->eager[req->peer][i].size;
        while(*flag != (char)1);
        //printf("[%i] found eager message from peer %i at addr %lu (tag: %i)\n", req->rank, req->peer, (unsigned long)(&req->comminfo->eager[req->peer][i]), (int)req->comminfo->eager[req->peer][i].tag);
        /* copy message to recv buffer */
        memcpy(req->buf, &req->comminfo->eager[req->peer][i].buf, req->comminfo->eager[req->peer][i].size);
        index = req->comminfo->eager[req->peer][i].index;
        /* set the buffer to '0' to make flag-polling useful */
        memset((void*)(&req->comminfo->eager[req->peer][i]), 0, sizeof(int32_t)+2*sizeof(int16_t)+sizeof(char)+req->comminfo->eager[req->peer][i].size);
        /* RDMA into the free-buffer on the sender to indicate that my 
         * buffer can be reused */
        
        //printf("[%i] RDMA EAGER_RECVD (%i) to node %i in buffer %lu at index %i\n", req->rank, req->comminfo->empty, req->peer, (unsigned long)req->comminfo->eager_free_info[req->peer].addr, index);
        /* prepare EAGER_RECVD send request */
        sr_sg_lst.addr = (VAPI_virt_addr_t)(&req->comminfo->empty);
        sr_sg_lst.len = sizeof(int);
        sr_sg_lst.lkey = req->comminfo->empty_l_key; 
        sr_desc.id = (u_int64_t)0; 
        sr_desc.opcode = VAPI_RDMA_WRITE; 
        sr_desc.set_se = 0;
        sr_desc.comp_type = VAPI_SIGNALED; 
        sr_desc.sg_lst_p = &sr_sg_lst; 
        sr_desc.sg_lst_len = 1; 
        sr_desc.imm_data = (u_int64_t)0; /* not used */ 
        sr_desc.fence = 0; 
        sr_desc.compare_add = 0; 
        sr_desc.r_key = (VAPI_rkey_t)(req->comminfo->eager_free_info[req->peer].r_key);
        sr_desc.remote_addr = req->comminfo->eager_free_info[req->peer].addr+sizeof(int)*index; /* TODO: 64 Bit */ 

        /* post EAGER_RECVD request */
        res = VAPI_post_sr(  IB_Hca_info.hca_hndl, req->comminfo->qp_hndl_arr[req->peer], &sr_desc );
        IB_stat( res, "VAPI_post_sr()" );
        /* mark receive as done */
        req->status = RECV_EAGER_DONE;
        /* leave loop */
        break;
 //       return IB_OK;
 // -> we need to poll CQs ...
      }
    }
 //   return IB_CONTINUE;
  }

//t=0-MPI_Wtime();
  /* poll all CQs of the comm related to req (we should probably only poll those where we wait for something */
  //for(i=0; i<req->p; i++) {
  //for(i=req->peer; i<=req->peer; i++) {
    //if(i == req->rank) continue;
    i=req->peer;
    /************************************** SEND QUEUE handling ************************************/
    res = VAPI_poll_cq( IB_Hca_info.hca_hndl, req->comminfo->sr_cq_hndl_arr[i], &comp_desc_p );
    if((res != VAPI_EBUSY) && (res != VAPI_CQ_EMPTY)) {
      //printf("[%i] have SR CQE from host %i\n", req->rank, i);
      IB_CQ_stat( comp_desc_p, "VAPI_poll_cq(SR)" );
      /* id == 0 for eager recvd messages - we do not need to wait for them ... */
      if(comp_desc_p.id != 0) {
        tmpreq = (IB_Req*)(comp_desc_p.id);
        if(tmpreq->status == SEND_POSTED_SR) {
          /* we sent the message and are ready  */
          /* TODO: free *all* request resources here */
          tmpreq->status = SEND_DONE;
          //printf("[%i] req %lu send to %i with tag %i is done ...\n", tmpreq->rank, (unsigned long)tmpreq, tmpreq->peer, tmpreq->tag);
        } else if (tmpreq->status == SEND_SENT_EAGER) {
          /* set rtr sendlist element to free */
          tmpreq->comminfo->eager_send[tmpreq->sendel].tag = -1;
          tmpreq->status = SEND_DONE;
          //printf("[%i] eager req %lu send to %i with tag %i is done ...\n", tmpreq->rank, (unsigned long)tmpreq, tmpreq->peer, tmpreq->tag);
        } else if ((tmpreq->status == RECV_SENDING_RTR) || (tmpreq->status == RECV_DONE)) {
          /* set rtr sendlist element to free */
          tmpreq->comminfo->rtr_send[tmpreq->sendel].tag = -1;
          /* do not change DONE requests back :o) */
          if(tmpreq->status != RECV_DONE) tmpreq->status = RECV_SENT_RTR;
        } else {
          printf("[%i] req %lu unexpected status (%i) for send to %i (tag: %i) after poll sr_cq \n", tmpreq->rank, (unsigned long)tmpreq, tmpreq->status, tmpreq->peer, tmpreq->tag);
        }
      }
    }
    /************************************** RECEIVE QUEUE handling ************************************/
    res = VAPI_poll_cq( IB_Hca_info.hca_hndl, req->comminfo->rr_cq_hndl_arr[i], &comp_desc_p );
    if((res != VAPI_EBUSY) && (res != VAPI_CQ_EMPTY)) {
      //printf("[%i] have RR CQE from host %i\n", req->rank, i);
      IB_CQ_stat( comp_desc_p, "VAPI_poll_cq(RR)" );

      /* we received real data - match it (tag, peer)*/
      keyel.tag=comp_desc_p.imm_data;
      memel = hb_tree_search(req->comminfo->taglist[i], &keyel);
      if(memel == NULL) { 
        printf("[%i] got unexpected packet with tag: %i, peer: %i\n", req->rank, comp_desc_p.imm_data, i);
        printf(" this CANNOT happen!!!\n");
        return IB_CONTINUE;
      }
      /* delete element - TODO: this should actually be done together with
       * the find ... we should extend libdict */
      res = hb_tree_remove(req->comminfo->taglist[i], &keyel, 0);
      if(res != 0) { printf("error deleting tag element in hb_tree_remove()\n"); }
      tmpreq = memel->req;
      //printf("[%i] found request %lu for tag %i and src %i\n", tmpreq->rank, (unsigned long)tmpreq, comp_desc_p.imm_data, i);
      if((tmpreq->status == RECV_SENDING_RTR) || (tmpreq->status == RECV_SENT_RTR)) {
        /* TODO: free *all* req resources here */
        //printf("[%i] req %lu we received data message from host %i for tag %i-> done \n", req->rank, (unsigned long)tmpreq, req->peer, req->tag);
        
        /* get offset in the receiver's array */
        for(j=0; j<IB_RTR_SIZE; j++) {
          if(tmpreq->comminfo->rtr_peer_free[tmpreq->peer][j] == tmpreq->tag) break;
        }
        if(j==IB_RTR_SIZE) { printf("[%i] we did not find tag %i to delete - should not HAPPEN!!!\n", tmpreq->rank, tmpreq->tag); }
        tmpreq->comminfo->rtr_peer_free[tmpreq->peer][j] = -1;
        
        tmpreq->status = RECV_DONE;
      } else {
        printf("[%i] req %lu (tag: %i) unexpected status (%i) after poll rr_cq \n", tmpreq->rank, (unsigned long)tmpreq, tmpreq->tag, tmpreq->status);
      }
    }
  //}
//t+=MPI_Wtime();
//printf("time: %lf\n", t*1e6);

  if((req->status == SEND_DONE) || (req->status == RECV_DONE) || (req->status == RECV_EAGER_DONE)) 
    return IB_OK;
  else
    return IB_CONTINUE;
}

int IB_Wait(IB_Request *request) {
  
  while(IB_Test(request) != IB_OK) {};

  return IB_OK;
}

int IB_Testall(int count, IB_Request *requests, int *flag) {
  int i, res;

  *flag = 1;
  for(i=0; i<count; i++) {
    res = IB_Test(&requests[i]);
    if(res != IB_OK) *flag = 0;
  }

  return IB_OK;
}

int IB_Waitall(int count, IB_Request *requests) {
  int i, res, done;

  done = IB_CONTINUE;
  do  {
    done = IB_OK;
    for(i=0; i<count; i++) {
      if((requests[i]->status != SEND_DONE) && (requests[i]->status != RECV_DONE) && (requests[i]->status != RECV_EAGER_DONE)) 
      {
        res = IB_Test(&requests[i]); /* we shouldn't test ready requests ... */
        /* in case of error */
        if((res != IB_OK) && (res != IB_CONTINUE)) break;
        /* we have at least one unfinished request ... */
        if(res == IB_CONTINUE) done = IB_CONTINUE;
      }
    }
  } while(done == IB_CONTINUE);

  return done;
}

static int IB_Create_qp(	int rank, int remote, 
					    				VAPI_hca_hndl_t *hca_hndl_p, 
							    		VAPI_cq_hndl_t *sr_cq_hndl_p,
									    VAPI_cq_hndl_t *rr_cq_hndl_p,
                      VAPI_qp_hndl_t *qp_hndl_p,
							    		VAPI_pd_hndl_t *pd_hndp_p,
                      MPI_Comm comm) {
	// variables
	VAPI_qp_init_attr_t qp_init_attr_p; // QP init attribs
	VAPI_ret_t ret; // VAPI return value
	VAPI_qp_prop_t	qp_prop_p, rem_qp_prop_p; // QP properties
	VAPI_hca_port_t hca_port_p, rem_hca_port_p; // port properties 
	VAPI_qp_attr_mask_t	qp_attr_mask_p; // QP attribute mask
	VAPI_qp_attr_t qp_attr_p; // QP attributes
	VAPI_qp_cap_t qp_cap_p; // QP capabilities 

	MPI_Status stat; // mpi status
	
	// get my LID
	ret = VAPI_query_hca_port_prop( 	*hca_hndl_p,
												            (IB_port_t)1,
												            &hca_port_p	);
	IB_stat( ret, "EVAPI_get_hca_hndl()" );
	
	
	// set QP Properties ...
	qp_init_attr_p.sq_cq_hndl = *sr_cq_hndl_p; 		// cq associated with sr's
	qp_init_attr_p.rq_cq_hndl = *rr_cq_hndl_p;			// cq associated with rr's
	qp_init_attr_p.cap.max_oust_wr_sq = 1000; 	// TODO guessed
	qp_init_attr_p.cap.max_oust_wr_rq = 1000; 	// TODO guessed 
	qp_init_attr_p.cap.max_sg_size_sq = 1; 		// TODO guessed 
	qp_init_attr_p.cap.max_sg_size_rq = 1; 		// TODO guessed
	qp_init_attr_p.cap.max_inline_data_sq = 1; 	// TODO guessed
	//	qp_init_attr_p.rdd_hndl = no rdd (Reliable Datagram Domain) used
	qp_init_attr_p.sq_sig_type = VAPI_SIGNAL_ALL_WR;	// signal all submitted WR's
	qp_init_attr_p.rq_sig_type = VAPI_SIGNAL_ALL_WR;	// signal all submitted WR's
	qp_init_attr_p.pd_hndl = *pd_hndp_p;				// the PD
	// QP Transport Service Type	
	qp_init_attr_p.ts_type = VAPI_TS_RC;

	// create QP
	ret = VAPI_create_qp(	*hca_hndl_p, 
									&qp_init_attr_p,
									qp_hndl_p,
									&qp_prop_p	);
	IB_stat( ret, "VAPI_create_qp()" ); 

	MPI_Send(&hca_port_p.lid, 1, MPI_INT, remote, 0, comm);
	MPI_Recv(&rem_hca_port_p.lid, 1, MPI_INT, remote, 0, comm, &stat);
	MPI_Send(&qp_prop_p.qp_num, 1, MPI_INT, remote, 0, comm);
	MPI_Recv(&rem_qp_prop_p.qp_num, 1, MPI_INT, remote, 0, comm, &stat);
	
  //fprintf(stderr, "[INFO] (lid:qp) - %x:%u -> %x:%u\n", hca_port_p.lid, qp_prop_p.qp_num, rem_hca_port_p.lid, rem_qp_prop_p.qp_num);

	// see page 222 for details ...
	/* Transition RST to INIT	*/
	QP_ATTR_MASK_CLR_ALL(qp_attr_mask_p);
	// QP State
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_QP_STATE);
	qp_attr_p.qp_state = VAPI_INIT;
	
	// partition key index (see page 319)
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_PKEY_IX);
	qp_attr_p.pkey_ix = 0; // first partition key
	
	// queue key -> only for datagram (RD, UD) TODO set this!
	//	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_QKEY);
	//	qp_attr_p.qkey = 0; 
	
	// enable/disable RDMA R / Atomic -> allow all :)
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_REMOTE_ATOMIC_FLAGS);
	qp_attr_p.remote_atomic_flags = VAPI_EN_REM_WRITE;
	
	// physical port
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_PORT);
	qp_attr_p.port = 1; // TODO should not be static!

	ret = VAPI_modify_qp(	*hca_hndl_p,
									*qp_hndl_p,
									&qp_attr_p,
									&qp_attr_mask_p,
									&qp_cap_p	);
	IB_stat( ret, "VAPI_modify_qp() (RST->INIT)" ); 

	// Transition INIT to RTR
	QP_ATTR_MASK_CLR_ALL(qp_attr_mask_p);
	
	// new state
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_QP_STATE);
	qp_attr_p.qp_state = VAPI_RTR;
	
	// remote node address vector
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_AV);
	qp_attr_p.av.sl = 0; // TODO guessed
	qp_attr_p.av.dlid = rem_hca_port_p.lid; // partners lid
	qp_attr_p.av.src_path_bits = 0; // TODO guessed
	qp_attr_p.av.static_rate = 0; // TODO guessed
	qp_attr_p.av.grh_flag = 0; // non set ...
	qp_attr_p.av.traffic_class = 0; // TODO guessed
	qp_attr_p.av.hop_limit = 255; // should be ok - only global routing
	qp_attr_p.av.flow_label = 0; // only global routing
	qp_attr_p.av.sgid_index = 0; // only global routing
	qp_attr_p.av.port = 1; // TODO guessed
	
	// packet sequence number 
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_RQ_PSN);
	qp_attr_p.rq_psn = 0;

	// number of responder resources for RDMA R + Atomic
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_QP_OUS_RD_ATOM);
	qp_attr_p.qp_ous_rd_atom = 5; //TODO tune here?
	
	// minimum rnr nak timer
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_MIN_RNR_TIMER);
	qp_attr_p.min_rnr_timer = 1; // TODO guessed
	
	// dest QP number
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_DEST_QP_NUM);
	qp_attr_p.dest_qp_num = rem_qp_prop_p.qp_num; // partners qp num

	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_PATH_MTU);
	qp_attr_p.path_mtu         = MTU1024; // TODO tune here

	ret = VAPI_modify_qp(	*hca_hndl_p,
									*qp_hndl_p,
									&qp_attr_p,
									&qp_attr_mask_p,
									&qp_cap_p	);
	IB_stat( ret, "VAPI_modify_qp() (INIT->RTR)" );


	// Transition RTR to RTS
	QP_ATTR_MASK_CLR_ALL(qp_attr_mask_p);
	// new state
	QP_ATTR_MASK_SET( qp_attr_mask_p, QP_ATTR_QP_STATE);
	qp_attr_p.qp_state = VAPI_RTS;
	
	// SQ sequence number
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_SQ_PSN);
	qp_attr_p.sq_psn = 0;
	
	// local ACK timeout
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_TIMEOUT);
	qp_attr_p.timeout = 10; // TODO tune here
	
	// retry count
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_RNR_RETRY);
	qp_attr_p.rnr_retry = 255; // increased due to VAPI_RETRY_EXC_ERR if 30 nodes fire 0.5MB at one :) -> maximum :)
	
	// number of outstanding RDMA R / atomic at destination
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_OUS_DST_RD_ATOM);
	qp_attr_p.ous_dst_rd_atom = 10; // TODO tune here
	
	// retry count
	QP_ATTR_MASK_SET(qp_attr_mask_p, QP_ATTR_RETRY_COUNT);
	qp_attr_p.retry_count = 255; // guessed

	ret = VAPI_modify_qp(	*hca_hndl_p,
									*qp_hndl_p,
									&qp_attr_p,
									&qp_attr_mask_p,
									&qp_cap_p	);
	IB_stat( ret, "VAPI_modify_qp() (RTR->RTS)" );

	return IB_OK;
}

static __inline__ int IB_Memlist_compare_entries(IB_Memlistel *a, IB_Memlistel *b,void *param) {

  /* two memory regions are defined as equal if they have some common
   * memory - more is not possible, because we have to ensure
   * reflexibility (a=b includes b=a) */
	
	if( (a->buf == b->buf) && (a->size == b->size) ) {
    return  0;
  }
	if ( (a->buf < b->buf)) {	
    return -1;
	}
	return +1;
}

static __inline__ void IB_Memlist_delete_key(IB_Memlistel *k) {
  /* do nothing because the key and the data element are identical :-) 
   * both (the single one :) is freed in IB_Memlist_memlist_delete() */
}

static __inline__ void IB_Memlist_memlist_delete(IB_Memlistel *entry) {
  /* free entry and deregister MR here ... */
}

static __inline__ int IB_Taglist_compare_entries(IB_Taglstel *a, IB_Taglstel *b,void *param) {

	if( a->tag == b->tag ) {
    return  0;
  }
	if( a->tag < b->tag ) {	
    return -1;
	}
	return +1;
}

static __inline__ void IB_Taglist_delete_key(IB_Taglstel *k) {
  /* do nothing because the key and the data element are identical :-) 
   * both (the single one :) is freed in IB_Memlist_memlist_delete() */
}

static __inline__ void IB_Taglist_delete(IB_Taglstel *entry) {
  /* free taglistentry */
  free(entry);
}
