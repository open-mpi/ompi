/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"
#include "gurt/hash.h"
#include <gurt/common.h>

static struct d_hash_table *coh_hash;
static struct d_hash_table *poh_hash;

enum {
    DAOS_POOL,
    DAOS_CONT,
};

static inline struct adio_daos_hdl *hdl_obj(d_list_t * rlink)
{
    return container_of(rlink, struct adio_daos_hdl, entry);
}

static bool
key_cmp(struct d_hash_table *htable, d_list_t * rlink, const void *key, unsigned int ksize)
{
    struct adio_daos_hdl *hdl = hdl_obj(rlink);

    return (uuid_compare(hdl->uuid, key) == 0);
}

static void rec_addref(struct d_hash_table *htable, d_list_t * rlink)
{
    hdl_obj(rlink)->ref++;
}

static bool rec_decref(struct d_hash_table *htable, d_list_t * rlink)
{
    struct adio_daos_hdl *hdl = hdl_obj(rlink);

    assert(hdl->ref > 0);
    hdl->ref--;
    return (hdl->ref == 0);
}

static void rec_free(struct d_hash_table *htable, d_list_t * rlink)
{
    struct adio_daos_hdl *hdl = hdl_obj(rlink);

    assert(d_hash_rec_unlinked(&hdl->entry));
    assert(hdl->ref == 0);

    if (hdl->type == DAOS_POOL)
        daos_pool_disconnect(hdl->open_hdl, NULL);
    else if (hdl->type == DAOS_CONT) {
        dfs_umount(hdl->dfs);
        daos_cont_close(hdl->open_hdl, NULL);
    } else
        assert(0);
    ADIOI_Free(hdl);
}

static d_hash_table_ops_t hdl_hash_ops = {
    .hop_key_cmp = key_cmp,
    .hop_rec_addref = rec_addref,
    .hop_rec_decref = rec_decref,
    .hop_rec_free = rec_free
};

int adio_daos_hash_init(void)
{
    int rc;

    rc = d_hash_table_create(0, 16, NULL, &hdl_hash_ops, &poh_hash);
    if (rc)
        return rc;

    return d_hash_table_create(0, 16, NULL, &hdl_hash_ops, &coh_hash);
}

void adio_daos_hash_finalize(void)
{
    d_hash_table_destroy(coh_hash, true /* force */);
    d_hash_table_destroy(poh_hash, true /* force */);
}

struct adio_daos_hdl *adio_daos_poh_lookup(const uuid_t uuid)
{
    d_list_t *rlink;

    rlink = d_hash_rec_find(poh_hash, uuid, sizeof(uuid_t));
    if (rlink == NULL)
        return NULL;

    return hdl_obj(rlink);
}

void adio_daos_poh_release(struct adio_daos_hdl *hdl)
{
    d_hash_rec_decref(poh_hash, &hdl->entry);
}

int adio_daos_poh_insert(uuid_t uuid, daos_handle_t poh, struct adio_daos_hdl **hdl)
{
    struct adio_daos_hdl *phdl;
    int rc;

    phdl = (struct adio_daos_hdl *) ADIOI_Calloc(1, sizeof(struct adio_daos_hdl));
    if (phdl == NULL)
        return -1;

    phdl->type = DAOS_POOL;
    uuid_copy(phdl->uuid, uuid);
    phdl->open_hdl.cookie = poh.cookie;

    rc = d_hash_rec_insert(poh_hash, phdl->uuid, sizeof(uuid_t), &phdl->entry, true);
    if (rc) {
        PRINT_MSG(stderr, "Failed to add phdl to hashtable (%d)\n", rc);
        goto free_hdl;
    }

    d_hash_rec_addref(poh_hash, &phdl->entry);
    *hdl = phdl;

    return 0;

  free_hdl:
    ADIOI_Free(phdl);
    return rc;
}

int adio_daos_poh_lookup_connect(uuid_t uuid, struct adio_daos_hdl **hdl)
{
    struct adio_daos_hdl *phdl;
    int rc;

    phdl = adio_daos_poh_lookup(uuid);
    if (phdl != NULL) {
        *hdl = phdl;
        return 0;
    }

    phdl = (struct adio_daos_hdl *) ADIOI_Calloc(1, sizeof(struct adio_daos_hdl));
    if (phdl == NULL)
        return -1;

    phdl->type = DAOS_POOL;
    uuid_copy(phdl->uuid, uuid);

    /** Get the SVCL and Server group from env variables. This is temp as those
     * won't be needed later */
    char *svcl_str = NULL;
    char *group = NULL;
    daos_pool_info_t pool_info;
    d_rank_list_t *svcl = NULL;

    svcl_str = getenv("DAOS_SVCL");
    if (svcl_str != NULL) {
        svcl = daos_rank_list_parse(svcl_str, ":");
        if (svcl == NULL) {
            PRINT_MSG(stderr, "Failed to parse SVC list env\n");
            rc = -1;
            goto free_hdl;
        }
    }
    group = getenv("DAOS_GROUP");

    rc = daos_pool_connect(uuid, group, svcl, DAOS_PC_RW, &phdl->open_hdl, &pool_info, NULL);
    d_rank_list_free(svcl);
    if (rc < 0) {
        PRINT_MSG(stderr, "Failed to connect to pool (%d)\n", rc);
        goto free_hdl;
    }

    rc = d_hash_rec_insert(poh_hash, phdl->uuid, sizeof(uuid_t), &phdl->entry, true);
    if (rc) {
        PRINT_MSG(stderr, "Failed to add phdl to hashtable (%d)\n", rc);
        goto err_pool;
    }

    d_hash_rec_addref(poh_hash, &phdl->entry);
    *hdl = phdl;

    return 0;

  err_pool:
    daos_pool_disconnect(phdl->open_hdl, NULL);
  free_hdl:
    ADIOI_Free(phdl);
    return rc;
}

struct adio_daos_hdl *adio_daos_coh_lookup(const uuid_t uuid)
{
    d_list_t *rlink;

    rlink = d_hash_rec_find(coh_hash, uuid, sizeof(uuid_t));
    if (rlink == NULL)
        return NULL;

    return hdl_obj(rlink);
}

void adio_daos_coh_release(struct adio_daos_hdl *hdl)
{
    d_hash_rec_decref(coh_hash, &hdl->entry);
}

int adio_daos_coh_insert(uuid_t uuid, daos_handle_t coh, struct adio_daos_hdl **hdl)
{
    struct adio_daos_hdl *co_hdl;
    int rc;

    co_hdl = (struct adio_daos_hdl *) ADIOI_Calloc(1, sizeof(struct adio_daos_hdl));
    if (co_hdl == NULL)
        return -1;

    co_hdl->type = DAOS_CONT;
    uuid_copy(co_hdl->uuid, uuid);
    co_hdl->open_hdl.cookie = coh.cookie;

    rc = d_hash_rec_insert(coh_hash, co_hdl->uuid, sizeof(uuid_t), &co_hdl->entry, true);
    if (rc) {
        PRINT_MSG(stderr, "Failed to add co_hdl to hashtable (%d)\n", rc);
        goto err_coh;
    }

    d_hash_rec_addref(coh_hash, &co_hdl->entry);
    *hdl = co_hdl;

    return 0;

  err_coh:
    ADIOI_Free(co_hdl);
    return rc;
}

int
adio_daos_coh_lookup_create(daos_handle_t poh, uuid_t uuid, int amode,
                            bool create, struct adio_daos_hdl **hdl)
{
    struct adio_daos_hdl *co_hdl;
    int rc;

    co_hdl = adio_daos_coh_lookup(uuid);
    if (co_hdl != NULL) {
        *hdl = co_hdl;
        return 0;
    }

    co_hdl = (struct adio_daos_hdl *) ADIOI_Calloc(1, sizeof(struct adio_daos_hdl));
    if (co_hdl == NULL)
        return -1;

    co_hdl->type = DAOS_CONT;
    uuid_copy(co_hdl->uuid, uuid);

    /* Try to open the DAOS container first (the parent directory) */
    rc = daos_cont_open(poh, uuid, DAOS_COO_RW, &co_hdl->open_hdl, NULL, NULL);
    /* If fails with NOEXIST we can create it then reopen if create mode */
    if (rc == -DER_NONEXIST && create) {
        rc = dfs_cont_create(poh, uuid, NULL, &co_hdl->open_hdl, &co_hdl->dfs);
        /** if someone got there first, re-open*/
        if (rc == EEXIST) {
            rc = daos_cont_open(poh, uuid, DAOS_COO_RW, &co_hdl->open_hdl, NULL, NULL);
            if (rc) {
                PRINT_MSG(stderr, "Failed to create DFS container (%d)\n", rc);
                goto free_coh;
            }
            rc = dfs_mount(poh, co_hdl->open_hdl, amode, &co_hdl->dfs);
            if (rc) {
                PRINT_MSG(stderr, "Failed to mount DFS namesapce (%d)\n", rc);
                goto err_cont;
            }
        } else if (rc) {
            PRINT_MSG(stderr, "Failed to create DFS container (%d)\n", rc);
            goto free_coh;
        }
    } else if (rc == 0) {
        /* Mount a DFS namespace on the container */
        rc = dfs_mount(poh, co_hdl->open_hdl, amode, &co_hdl->dfs);
        if (rc) {
            PRINT_MSG(stderr, "Failed to mount DFS namespace (%d)\n", rc);
            goto err_cont;
        }
    } else {
        goto free_coh;
    }

    rc = d_hash_rec_insert(coh_hash, co_hdl->uuid, sizeof(uuid_t), &co_hdl->entry, true);
    if (rc) {
        PRINT_MSG(stderr, "Failed to add co_hdl to hashtable (%d)\n", rc);
        goto err_dfs;
    }

    d_hash_rec_addref(coh_hash, &co_hdl->entry);
    *hdl = co_hdl;

    return 0;

  err_dfs:
    dfs_umount(co_hdl->dfs);
  err_cont:
    daos_cont_close(co_hdl->open_hdl, NULL);
  free_coh:
    ADIOI_Free(co_hdl);
    return rc;
}
