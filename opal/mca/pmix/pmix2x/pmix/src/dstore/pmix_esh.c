/*
 * Copyright (c) 2015-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

#include <src/include/pmix_config.h>
#include <pmix/pmix_common.h>
#include "src/include/pmix_globals.h"

#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/buffer_ops/types.h"
#include "src/util/pmix_environ.h"
#include "src/util/hash.h"
#include "src/util/error.h"
#include "src/sm/pmix_sm.h"

#include "pmix_dstore.h"
#include "pmix_esh.h"


static int _esh_init(void);
static int _esh_finalize(void);
static int _esh_store(const char *nspace, int rank, pmix_kval_t *kv);
static int _esh_fetch(const char *nspace, int rank, const char *key, pmix_value_t **kvs);

pmix_dstore_base_module_t pmix_dstore_esh_module = {
    "esh",
    _esh_init,
    _esh_finalize,
    _esh_store,
    _esh_fetch
};

#define ESH_REGION_EXTENSION        "EXTENSION_SLOT"
#define ESH_REGION_INVALIDATED      "INVALIDATED"
#define ESH_ENV_INITIAL_SEG_SIZE    "INITIAL_SEG_SIZE"
#define ESH_ENV_NS_META_SEG_SIZE    "NS_META_SEG_SIZE"
#define ESH_ENV_NS_DATA_SEG_SIZE    "NS_DATA_SEG_SIZE"
#define ESH_ENV_LINEAR              "SM_USE_LINEAR_SEARCH"

#define EXT_SLOT_SIZE (PMIX_MAX_KEYLEN + 1 + 2*sizeof(size_t)) /* in ext slot new offset will be stored in case if new data were added for the same process during next commit */
#define KVAL_SIZE(size) (PMIX_MAX_KEYLEN + 1 + sizeof(size_t) + size)

static int _store_data_for_rank(ns_track_elem_t *ns_info, int rank, pmix_buffer_t *buf);
static seg_desc_t *_create_new_segment(segment_type type, char *nsname, uint32_t id);
static seg_desc_t *_attach_new_segment(segment_type type, char *nsname, uint32_t id);
static int _update_ns_elem(ns_track_elem_t *ns_elem, ns_seg_info_t *info);
static int _put_ns_info_to_initial_segment(const char *nspace, pmix_sm_seg_t *metaseg, pmix_sm_seg_t *dataseg);
static ns_seg_info_t *_get_ns_info_from_initial_segment(const char *nspace);
static ns_track_elem_t *_get_track_elem_for_namespace(const char *nspace);
static rank_meta_info *_get_rank_meta_info(int rank, seg_desc_t *segdesc);
static uint8_t *_get_data_region_by_offset(seg_desc_t *segdesc, size_t offset);
static void _update_initial_segment_info(void);
static void _set_constants_from_env(void);
static void _delete_sm_desc(seg_desc_t *desc);
static int _pmix_getpagesize(void);
static inline uint32_t _get_univ_size(const char *nspace);

static seg_desc_t *_global_sm_seg_first;
static seg_desc_t *_global_sm_seg_last;
static pmix_list_t _namespace_info_list;
static char *_tmp_dir = NULL;
static size_t _initial_segment_size = 0;
static size_t _max_ns_num;
static size_t _meta_segment_size = 0;
static size_t _max_meta_elems;
static size_t _data_segment_size = 0;
static int _lockfd;
static char *_lockfile_name;

/* If _direct_mode is set, it means that we use linear search
 * along the array of rank meta info objects inside a meta segment
 * to find the requested rank. Otherwise,  we do a fast lookup
 * based on rank and directly compute offset.
 * This mode is called direct because it's effectively used in
 * sparse communication patterns when direct modex is usually used.
 */
static int _direct_mode = 0;

static void ncon(ns_track_elem_t *p) {
    p->meta_seg = NULL;
    p->data_seg = NULL;
    p->num_meta_seg = 0;
    p->num_data_seg = 0;
}

static void ndes(ns_track_elem_t *p) {
    _delete_sm_desc(p->meta_seg);
    _delete_sm_desc(p->data_seg);
}

PMIX_CLASS_INSTANCE(ns_track_elem_t,
                    pmix_list_item_t,
                    ncon, ndes);

static inline int _is_server(void)
{
    return (pmix_globals.server);
}

static inline const char *_unique_id(void)
{
    static const char *str = NULL;
    if (!str) {
        /* see: pmix_server.c initialize_server_base()
         * to get format of uri
         */
        if (_is_server()) {
            static char buf[100];
            snprintf(buf, sizeof(buf) - 1, "pmix-%d", getpid());
            str = buf;
        } else {
            str = getenv("PMIX_SERVER_URI");
            if (str) {
                str = strrchr(str, '/');
            }
            str = (str ? str + 1 : "$$$");
        }
    }
    return str;
}

int _esh_init(void)
{
    int rc;
    seg_desc_t *seg;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s", __FILE__, __LINE__, __func__));

    rc = pmix_sm_init();
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    PMIX_CONSTRUCT(&_namespace_info_list, pmix_list_t);
    _global_sm_seg_first = NULL;
    _global_sm_seg_last = NULL;
    _tmp_dir = strdup(pmix_tmp_directory());
    _set_constants_from_env();
    _max_ns_num = (_initial_segment_size - sizeof(size_t) - sizeof(int)) / sizeof(ns_seg_info_t);
    _max_meta_elems = (_meta_segment_size - sizeof(size_t)) / sizeof(rank_meta_info);

    /* create a lock file to prevent clients from reading while server is writing to the shared memory.
     * This situation is quite often, especially in case of direct modex when clients might ask for data
     * simultaneously.*/
    _lockfile_name = malloc(256);
    snprintf(_lockfile_name, 256, "%s/%s_dstore_sm.lock", _tmp_dir, _unique_id());
    if (_is_server()) {
        _lockfd = open(_lockfile_name, O_CREAT | O_RDWR | O_EXCL, 0600);
        /* if previous launch was crashed, the lockfile might not be deleted and unlocked,
         * so we delete it and create a new one. */
        if (-1 == _lockfd) {
            unlink(_lockfile_name);
            _lockfd = open(_lockfile_name, O_CREAT | O_RDWR, 0600);
        }
    } else {
        _lockfd = open(_lockfile_name, O_RDWR);
    }
    if (-1 == _lockfd) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        return PMIX_ERROR;
    }

    if (_is_server()) {
        seg = _create_new_segment(INITIAL_SEGMENT, NULL, 0);
    } else {
        seg = _attach_new_segment(INITIAL_SEGMENT, NULL, 0);
    }
    if (NULL != seg) {
        _global_sm_seg_first = seg;
        _global_sm_seg_last = _global_sm_seg_first;
        return PMIX_SUCCESS;
    }
    return PMIX_ERROR;
}

int _esh_finalize(void)
{
    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s", __FILE__, __LINE__, __func__));

    _delete_sm_desc(_global_sm_seg_first);
    PMIX_LIST_DESTRUCT(&_namespace_info_list);
    if (NULL != _tmp_dir) {
        free(_tmp_dir);
    }
    close(_lockfd);
    if (_is_server()) {
        unlink(_lockfile_name);
    }
    free(_lockfile_name);

    pmix_sm_finalize();

    return PMIX_SUCCESS;
}


int _esh_store(const char *nspace, int rank, pmix_kval_t *kv)
{
    int rc;
    ns_track_elem_t *elem;
    pmix_buffer_t pbkt, xfer;
    ns_seg_info_t ns_info;

    if (NULL == kv) {
        return PMIX_ERROR;
    }

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: for %s:%d",
                         __FILE__, __LINE__, __func__, nspace, rank));

    /* set exclusive lock */
    flock(_lockfd, LOCK_EX);

    /* First of all, we go through local track list (list of ns_track_elem_t structures)
     * and look for an element for the target namespace.
     * If it is there, then shared memory segments for it are created, so we take it.
     * Otherwise, create a new element, fill its fields, create corresponding meta
     * and data segments for this namespace, add it to the local track list,
     * and put this info (ns_seg_info_t) to the initial segment. If initial segment
     * if full, then extend it by creating a new one and mark previous one as full.
     * All this stuff is done inside _get_track_elem_for_namespace function.
     */

    elem = _get_track_elem_for_namespace(nspace);
    if (NULL == elem) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        /* unset lock */
        flock(_lockfd, LOCK_UN);
        return PMIX_ERROR;
    }

    /* If a new element was just created, we need to create corresponding meta and
     * data segments and update corresponding element's fields. */
    if (NULL == elem->meta_seg || NULL == elem->data_seg) {
        strncpy(ns_info.ns_name, nspace, PMIX_MAX_NSLEN+1);
        ns_info.num_meta_seg = 1;
        ns_info.num_data_seg = 1;
        rc = _update_ns_elem(elem, &ns_info);
        if (PMIX_SUCCESS != rc || NULL == elem->meta_seg || NULL == elem->data_seg) {
            PMIX_ERROR_LOG(rc);
            /* unset lock */
            flock(_lockfd, LOCK_UN);
            return PMIX_ERROR;
        }

        /* zero created shared memory segments for this namespace */
        memset(elem->meta_seg->seg_info.seg_base_addr, 0, _meta_segment_size);
        memset(elem->data_seg->seg_info.seg_base_addr, 0, _data_segment_size);

        /* put ns's shared segments info to the global meta segment. */
        rc = _put_ns_info_to_initial_segment(nspace, &elem->meta_seg->seg_info, &elem->data_seg->seg_info);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            /* unset lock */
            flock(_lockfd, LOCK_UN);
            return rc;
        }
    }

    /* Now we know info about meta segment for this namespace. If meta segment
     * is not empty, then we look for data for the target rank. If they present, replace it. */
    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
    PMIX_LOAD_BUFFER(&xfer, kv->value->data.bo.bytes, kv->value->data.bo.size);
    pmix_buffer_t *pxfer = &xfer;
    pmix_bfrop.pack(&pbkt, &pxfer, 1, PMIX_BUFFER);
    xfer.base_ptr = NULL;
    xfer.bytes_used = 0;

    rc = _store_data_for_rank(elem, rank, &pbkt);

    PMIX_DESTRUCT(&xfer);
    PMIX_DESTRUCT(&pbkt);

    /* unset lock */
    flock(_lockfd, LOCK_UN);
    return rc;
}

int _esh_fetch(const char *nspace, int rank, const char *key, pmix_value_t **kvs)
{
    ns_seg_info_t *ns_info = NULL;
    int rc;
    ns_track_elem_t *elem;
    rank_meta_info *rinfo = NULL;
    size_t kval_cnt;
    seg_desc_t *meta_seg, *data_seg;
    uint8_t *addr;
    pmix_buffer_t buffer;
    pmix_value_t val;
    uint32_t nprocs;
    int cur_rank;

    if (NULL == key) {
        PMIX_OUTPUT_VERBOSE((7, pmix_globals.debug_output,
                             "dstore: Does not support passed parameters"));
        return PMIX_ERROR;
    }

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: for %s:%d look for key %s",
                         __FILE__, __LINE__, __func__, nspace, rank, key));

    if (kvs) {
        *kvs = NULL;
    }

    if (PMIX_RANK_UNDEF == rank) {
        nprocs = _get_univ_size(nspace);
        cur_rank = -1;
    } else {
        nprocs = 1;
        cur_rank = rank;
    }

    /* set shared lock */
    flock(_lockfd, LOCK_SH);

    /* First of all, we go through all initial segments and look at their field.
     * If it’s 1, then generate name of next initial segment incrementing id by one and attach to it.
     * We need this step to synchronize initial shared segments with our local track list.
     * Then we look for the target namespace in all initial segments.
     * If it is found, we get numbers of meta & data segments and
     * compare these numbers with the number of trackable meta & data
     * segments for this namespace in the local track list.
     * If the first number exceeds the last, or the local track list
     * doesn't track current namespace yet, then we update it (attach
     * to additional segments).
     */

    /* first update local information about initial segments. they can be extended, so then we need to attach to new segments. */
    _update_initial_segment_info();

    /* get information about shared segments per this namespace from the initial segment. */
    ns_info = _get_ns_info_from_initial_segment(nspace);
    if (NULL == ns_info) {
        /* no data for this namespace is found in the shared memory. */
        PMIX_OUTPUT_VERBOSE((7, pmix_globals.debug_output,
                    "%s:%d:%s:  no data for ns %s is found in the shared memory.",
                    __FILE__, __LINE__, __func__, nspace));
        /* unset lock */
        flock(_lockfd, LOCK_UN);
        return PMIX_ERROR;
    }

    /* get ns_track_elem_t object for the target namespace from the local track list. */
    elem = _get_track_elem_for_namespace(nspace);
    if (NULL == elem) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        /* unset lock */
        flock(_lockfd, LOCK_UN);
        return PMIX_ERROR;
    }
    /* need to update tracker:
     * attach to shared memory regions for this namespace and store its info locally
     * to operate with address and detach/unlink afterwards. */
    rc = _update_ns_elem(elem, ns_info);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        /* unset lock */
        flock(_lockfd, LOCK_UN);
        return PMIX_ERROR;
    }

    /* now we know info about meta segment for this namespace. */
    meta_seg = elem->meta_seg;
    data_seg = elem->data_seg;

    while (nprocs--) {
        if (PMIX_RANK_UNDEF == rank) {
            cur_rank++;
        }
        /* Then we look for the rank meta info in the shared meta segment. */
        rinfo = _get_rank_meta_info(cur_rank, meta_seg);
        if (NULL == rinfo) {
            PMIX_OUTPUT_VERBOSE((7, pmix_globals.debug_output,
                        "%s:%d:%s:  no data for this rank is found in the shared memory. rank %d",
                        __FILE__, __LINE__, __func__, cur_rank));
            rc = PMIX_ERR_PROC_ENTRY_NOT_FOUND;
            continue;
        }
        addr = _get_data_region_by_offset(data_seg, rinfo->offset);
        if (NULL == addr) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            rc = PMIX_ERR_PROC_ENTRY_NOT_FOUND;
            continue;
        }
        kval_cnt = rinfo->count;
        /* TODO: probably PMIX_ERR_NOT_FOUND is a better way but
         * setting to one initiates wrong next logic for unknown reason */
        rc = PMIX_ERROR;

        while (0 < kval_cnt) {
            /* data is stored in the following format:
             * key[PMIX_MAX_KEYLEN+1]
             * size_t size
             * byte buffer containing pmix_value, should be loaded to pmix_buffer_t and unpacked.
             * next kval pair
             * .....
             * EXTENSION slot which has key = EXTENSION_SLOT and a size_t value for offset to next data address for this process.
             */
            if (0 == strncmp((const char *)addr, ESH_REGION_INVALIDATED, PMIX_MAX_KEYLEN+1)) {
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %s:%d, skip %s region",
                            __FILE__, __LINE__, __func__, nspace, cur_rank, ESH_REGION_INVALIDATED));
                /*skip it */
                size_t size = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1);
                /* go to next item, updating address */
                addr += KVAL_SIZE(size);
            } else if (0 == strncmp((const char *)addr, ESH_REGION_EXTENSION, PMIX_MAX_KEYLEN+1)) {
                size_t offset = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t));
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %s:%d, reached %s with %lu value",
                            __FILE__, __LINE__, __func__, nspace, cur_rank, ESH_REGION_EXTENSION, offset));
                if (0 < offset) {
                    /* go to next item, updating address */
                    addr = _get_data_region_by_offset(data_seg, offset);
                    if (NULL == addr) {
                        /* report problem and return */
                        PMIX_ERROR_LOG(PMIX_ERROR);
                        rc = PMIX_ERROR;
                        goto done;
                    }
                } else {
                    /* no more data for this rank */
                    PMIX_OUTPUT_VERBOSE((7, pmix_globals.debug_output,
                                "%s:%d:%s:  no more data for this rank is found in the shared memory. rank %d key %s not found",
                                __FILE__, __LINE__, __func__, cur_rank, key));
                    break;
                }
            } else if (0 == strncmp((const char *)addr, key, PMIX_MAX_KEYLEN+1)) {
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %s:%d, found target key %s",
                            __FILE__, __LINE__, __func__, nspace, cur_rank, key));
                /* target key is found, get value */
                size_t size = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1);
                addr += PMIX_MAX_KEYLEN + 1 + sizeof(size_t);
                PMIX_CONSTRUCT(&buffer, pmix_buffer_t);
                PMIX_LOAD_BUFFER(&buffer, addr, size);
                int cnt = 1;
                /* unpack value for this key from the buffer. */
                PMIX_VALUE_CONSTRUCT(&val);
                if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buffer, &val, &cnt, PMIX_VALUE))) {
                    PMIX_ERROR_LOG(rc);
                } else if (PMIX_SUCCESS != (rc = pmix_bfrop.copy((void**)kvs, &val, PMIX_VALUE))) {
                    PMIX_ERROR_LOG(rc);
                }
                PMIX_VALUE_DESTRUCT(&val);
                buffer.base_ptr = NULL;
                buffer.bytes_used = 0;
                PMIX_DESTRUCT(&buffer);
                goto done;
            } else {
                char ckey[PMIX_MAX_KEYLEN+1] = {0};
                strncpy(ckey, (const char *)addr, PMIX_MAX_KEYLEN+1);
                size_t size = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1);
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %s:%d, skip key %s look for key %s", __FILE__, __LINE__, __func__, nspace, cur_rank, ckey, key));
                /* go to next item, updating address */
                addr += KVAL_SIZE(size);
                kval_cnt--;
            }
        }
    }

done:
    /* unset lock */
    flock(_lockfd, LOCK_UN);
    return rc;
}

static void _set_constants_from_env()
{
    char *str;
    int page_size = _pmix_getpagesize();

    if( NULL != (str = getenv(ESH_ENV_INITIAL_SEG_SIZE)) ) {
        _initial_segment_size = strtoul(str, NULL, 10);
        if ((size_t)page_size > _initial_segment_size) {
            _initial_segment_size = (size_t)page_size;
        }
    }
    if (0 == _initial_segment_size) {
        _initial_segment_size = INITIAL_SEG_SIZE;
    }
    if( NULL != (str = getenv(ESH_ENV_NS_META_SEG_SIZE)) ) {
        _meta_segment_size = strtoul(str, NULL, 10);
        if ((size_t)page_size > _meta_segment_size) {
            _meta_segment_size = (size_t)page_size;
        }
    }
    if (0 == _meta_segment_size) {
        _meta_segment_size = NS_META_SEG_SIZE;
    }
    if( NULL != (str = getenv(ESH_ENV_NS_DATA_SEG_SIZE)) ) {
        _data_segment_size = strtoul(str, NULL, 10);
        if ((size_t)page_size > _data_segment_size) {
            _data_segment_size = (size_t)page_size;
        }
    }
    if (0 == _data_segment_size) {
        _data_segment_size = NS_DATA_SEG_SIZE;
    }
    if (NULL != (str = getenv(ESH_ENV_LINEAR))) {
        if (1 == strtoul(str, NULL, 10)) {
            _direct_mode = 1;
        }
    }
}

static void _delete_sm_desc(seg_desc_t *desc)
{
    seg_desc_t *tmp;

    /* free all global segments */
    while (NULL != desc) {
        tmp = desc->next;
        /* detach & unlink from current desc */
        if (desc->seg_info.seg_cpid == getpid()) {
            pmix_sm_segment_unlink(&desc->seg_info);
        }
        pmix_sm_segment_detach(&desc->seg_info);
        free(desc);
        desc = tmp;
    }
}

static int _pmix_getpagesize(void)
{
#if defined(_SC_PAGESIZE )
    return sysconf(_SC_PAGESIZE);
#elif defined(_SC_PAGE_SIZE)
    return sysconf(_SC_PAGE_SIZE);
#else
    return 65536; /* safer to overestimate than under */
#endif
}

static seg_desc_t *_create_new_segment(segment_type type, char *nsname, uint32_t id)
{
    int rc;
    char file_name[PMIX_PATH_MAX];
    size_t size;
    seg_desc_t *new_seg = NULL;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: segment type %d, nspace %s, id %u",
                         __FILE__, __LINE__, __func__, type, nsname, id));

    switch (type) {
        case INITIAL_SEGMENT:
            size = _initial_segment_size;
            snprintf(file_name, PMIX_PATH_MAX, "%s/%s_initial-pmix_shared-segment-%u", _tmp_dir, _unique_id(), id);
            break;
        case NS_META_SEGMENT:
            size = _meta_segment_size;
            snprintf(file_name, PMIX_PATH_MAX, "%s/%s_smseg-%s-%u", _tmp_dir, _unique_id(), nsname, id);
            break;
        case NS_DATA_SEGMENT:
            size = _data_segment_size;
            snprintf(file_name, PMIX_PATH_MAX, "%s/%s_smdataseg-%s-%d", _tmp_dir, _unique_id(), nsname, id);
            break;
        default:
            PMIX_ERROR_LOG(PMIX_ERROR);
            return NULL;
    }
    new_seg = (seg_desc_t*)malloc(sizeof(seg_desc_t));
    if (new_seg) {
        new_seg->id = id;
        new_seg->next = NULL;
        new_seg->type = type;
        rc = pmix_sm_segment_create(&new_seg->seg_info, file_name, size);
        if (PMIX_SUCCESS == rc) {
            memset(new_seg->seg_info.seg_base_addr, 0, size);
        } else {
            free(new_seg);
            new_seg = NULL;
            PMIX_ERROR_LOG(rc);
        }
    }
    return new_seg;
}

static seg_desc_t *_attach_new_segment(segment_type type, char *nsname, uint32_t id)
{
    int rc;
    seg_desc_t *new_seg = NULL;
    new_seg = (seg_desc_t*)malloc(sizeof(seg_desc_t));
    new_seg->id = id;
    new_seg->next = NULL;
    new_seg->type = type;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: segment type %d, nspace %s, id %u",
                         __FILE__, __LINE__, __func__, type, nsname, id));

    switch (type) {
        case INITIAL_SEGMENT:
            new_seg->seg_info.seg_size = _initial_segment_size;
            snprintf(new_seg->seg_info.seg_name, PMIX_PATH_MAX, "%s/%s_initial-pmix_shared-segment-%u", _tmp_dir, _unique_id(), id);
            break;
        case NS_META_SEGMENT:
            new_seg->seg_info.seg_size = _meta_segment_size;
            snprintf(new_seg->seg_info.seg_name, PMIX_PATH_MAX, "%s/%s_smseg-%s-%u", _tmp_dir, _unique_id(), nsname, id);
            break;
        case NS_DATA_SEGMENT:
            new_seg->seg_info.seg_size = _data_segment_size;
            snprintf(new_seg->seg_info.seg_name, PMIX_PATH_MAX, "%s/%s_smdataseg-%s-%d", _tmp_dir, _unique_id(), nsname, id);
            break;
        default:
            PMIX_ERROR_LOG(PMIX_ERROR);
            return NULL;
    }
    rc = pmix_sm_segment_attach(&new_seg->seg_info);
    if (PMIX_SUCCESS != rc) {
        free(new_seg);
        new_seg = NULL;
        PMIX_ERROR_LOG(rc);
    }
    return new_seg;
}

/* This function synchronizes the content of initial shared segment and the local track list. */
static int _update_ns_elem(ns_track_elem_t *ns_elem, ns_seg_info_t *info)
{
    seg_desc_t *seg, *tmp = NULL;
    size_t i, offs;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s",
                         __FILE__, __LINE__, __func__));

    tmp = ns_elem->meta_seg;
    if (NULL != tmp) {
        while(NULL != tmp->next) {
            tmp = tmp->next;
        }
    }

    /* synchronize number of meta segments for the target namespace. */
    for (i = ns_elem->num_meta_seg; i < info->num_meta_seg; i++) {
        if (_is_server()) {
            seg = _create_new_segment(NS_META_SEGMENT, info->ns_name, i);
        } else {
            seg = _attach_new_segment(NS_META_SEGMENT, info->ns_name, i);
        }
        if (NULL == seg) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        if (NULL == tmp) {
            ns_elem->meta_seg = seg;
        } else {
            tmp->next = seg;
        }
        tmp = seg;
        ns_elem->num_meta_seg++;
    }

    tmp = ns_elem->data_seg;
    if (NULL != tmp) {
        while(NULL != tmp->next) {
            tmp = tmp->next;
        }
    }
    /* synchronize number of data segments for the target namespace. */
    for (i = ns_elem->num_data_seg; i < info->num_data_seg; i++) {
        if (_is_server()) {
            seg = _create_new_segment(NS_DATA_SEGMENT, info->ns_name, i);
            if (seg) {
                offs = sizeof(size_t);//shift on offset field itself
                memcpy(seg->seg_info.seg_base_addr, &offs, sizeof(size_t));
            }
        } else {
            seg = _attach_new_segment(NS_DATA_SEGMENT, info->ns_name, i);
        }
        if (NULL == seg) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        if (NULL == tmp) {
            ns_elem->data_seg = seg;
        } else {
            tmp->next = seg;
        }
        tmp = seg;
        ns_elem->num_data_seg++;
    }

    return PMIX_SUCCESS;
}

static seg_desc_t *extend_segment(seg_desc_t *segdesc, char *nspace)
{
    seg_desc_t *tmp, *seg;

    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s",
                         __FILE__, __LINE__, __func__));
    /* find last segment */
    tmp = segdesc;
    while (NULL != tmp->next) {
        tmp = tmp->next;
    }
    /* create another segment, the old one is full. */
    seg = _create_new_segment(segdesc->type, nspace, tmp->id + 1);
    if (NULL == seg) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        return NULL;
    }

    tmp->next = seg;

    return seg;
}

static int _put_ns_info_to_initial_segment(const char *nspace, pmix_sm_seg_t *metaseg, pmix_sm_seg_t *dataseg)
{
    ns_seg_info_t elem;
    size_t num_elems;
    num_elems = *((size_t*)(_global_sm_seg_last->seg_info.seg_base_addr));
    seg_desc_t *last_seg = _global_sm_seg_last;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s", __FILE__, __LINE__, __func__));

    if (_max_ns_num == num_elems) {
        if (NULL == (last_seg = extend_segment(last_seg, NULL))) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        /* mark previous segment as full */
        int full = 1;
        memcpy((uint8_t*)(_global_sm_seg_last->seg_info.seg_base_addr + sizeof(size_t)), &full, sizeof(int));
        _global_sm_seg_last = last_seg;
        memset(_global_sm_seg_last->seg_info.seg_base_addr, 0, _initial_segment_size);
    }
    strncpy(elem.ns_name, nspace, PMIX_MAX_NSLEN+1);
    elem.num_meta_seg = 1;
    elem.num_data_seg = 1;
    memcpy((uint8_t*)(_global_sm_seg_last->seg_info.seg_base_addr) + sizeof(size_t) + sizeof(int) + num_elems * sizeof(ns_seg_info_t),
            &elem, sizeof(ns_seg_info_t));
    num_elems++;
    memcpy((uint8_t*)(_global_sm_seg_last->seg_info.seg_base_addr), &num_elems, sizeof(size_t));
    return PMIX_SUCCESS;
}

/* clients should sync local info with information from initial segment regularly */
static void _update_initial_segment_info(void)
{
    seg_desc_t *tmp;
    tmp = _global_sm_seg_first;

    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s", __FILE__, __LINE__, __func__));

    /* go through all global segments */
    do {
        /* check if current segment was marked as full but no more next segment is in the chain */
        if (NULL == tmp->next && 1 == *((int*)((uint8_t*)(tmp->seg_info.seg_base_addr) + sizeof(size_t)))) {
            tmp->next = _attach_new_segment(INITIAL_SEGMENT, NULL, tmp->id+1);
        }
        tmp = tmp->next;
    }
    while (NULL != tmp);
}

/* this function will be used by clients to get ns data from the initial segment and add them to the tracker list */
static ns_seg_info_t *_get_ns_info_from_initial_segment(const char *nspace)
{
    int rc;
    size_t i;
    seg_desc_t *tmp;
    ns_seg_info_t *elem, *cur_elem;
    elem = NULL;
    size_t num_elems;

    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s", __FILE__, __LINE__, __func__));

    tmp = _global_sm_seg_first;

    rc = 1;
    /* go through all global segments */
    do {
        num_elems = *((size_t*)(tmp->seg_info.seg_base_addr));
        for (i = 0; i < num_elems; i++) {
            cur_elem = (ns_seg_info_t*)((uint8_t*)(tmp->seg_info.seg_base_addr) + sizeof(size_t) + sizeof(int) + i * sizeof(ns_seg_info_t));
            if (0 == (rc = strncmp(cur_elem->ns_name, nspace, strlen(nspace)+1))) {
                break;
            }
        }
        if (0 == rc) {
            elem = cur_elem;
            break;
        }
        tmp = tmp->next;
    }
    while (NULL != tmp);
    return elem;
}

static ns_track_elem_t *_get_track_elem_for_namespace(const char *nspace)
{
    ns_track_elem_t *new_elem = NULL;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: nspace %s",
                         __FILE__, __LINE__, __func__, nspace));

    /* check if this namespace is already being tracked to avoid duplicating data. */
    PMIX_LIST_FOREACH(new_elem, &_namespace_info_list, ns_track_elem_t) {
        if (0 == strncmp(nspace, new_elem->ns_name, PMIX_MAX_NSLEN+1)) {
            /* data for this namespace should be already stored in shared memory region. */
            /* so go and just put new data. */
            PMIX_OUTPUT_VERBOSE((7, pmix_globals.debug_output,
                        "%s:%d:%s: found nspace %s in the track list",
                        __FILE__, __LINE__, __func__, nspace));
            return new_elem;
        }
    }

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: create new object for nspace %s",
                         __FILE__, __LINE__, __func__, nspace));
    /* create shared memory regions for this namespace and store its info locally
     * to operate with address and detach/unlink afterwards. */
    new_elem = PMIX_NEW(ns_track_elem_t);
    strncpy(new_elem->ns_name, nspace, PMIX_MAX_NSLEN+1);

    pmix_list_append(&_namespace_info_list, &new_elem->super);

    return new_elem;
}

static rank_meta_info *_get_rank_meta_info(int rank, seg_desc_t *segdesc)
{
    size_t i;
    rank_meta_info *elem = NULL;
    seg_desc_t *tmp = segdesc;
    size_t num_elems, rel_offset;
    int id;
    rank_meta_info *cur_elem;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s",
                         __FILE__, __LINE__, __func__));

    if (1 == _direct_mode) {
        /* do linear search to find the requested rank inside all meta segments
         * for this namespace. */
        /* go through all existing meta segments for this namespace */
        do {
            num_elems = *((size_t*)(tmp->seg_info.seg_base_addr));
            for (i = 0; i < num_elems; i++) {
                cur_elem = (rank_meta_info*)((uint8_t*)(tmp->seg_info.seg_base_addr) + sizeof(size_t) + i * sizeof(rank_meta_info));
                if (rank == (int)cur_elem->rank) {
                    elem = cur_elem;
                    break;
                }
            }
            tmp = tmp->next;
        }
        while (NULL != tmp && NULL == elem);
    } else {
        /* directly compute index of meta segment (id) and relative offset (rel_offset)
         * inside this segment for fast lookup a rank_meta_info object for the requested rank. */
        id = rank/_max_meta_elems;
        rel_offset = (rank%_max_meta_elems) * sizeof(rank_meta_info) + sizeof(size_t);
        /* go through all existing meta segments for this namespace.
         * Stop at id number if it exists. */
        while (NULL != tmp->next && 0 != id) {
            tmp = tmp->next;
            id--;
        }
        if (0 == id) {
            /* the segment is found, looking for data for the target rank. */
            elem = (rank_meta_info*)((uint8_t*)(tmp->seg_info.seg_base_addr) + rel_offset);
            if ( 0 == elem->offset) {
                /* offset can never be 0, it means that there is no data for this rank yet. */
                elem = NULL;
            }
        }
    }
    return elem;
}

static int set_rank_meta_info(ns_track_elem_t *ns_info, rank_meta_info *rinfo)
{
    /* it's claimed that there is still no meta info for this rank stored */
    seg_desc_t *tmp;
    size_t num_elems, rel_offset;
    int id, count;
    rank_meta_info *cur_elem;

    if (!ns_info || !rinfo) {
        PMIX_ERROR_LOG(PMIX_ERROR);
        return PMIX_ERROR;
    }

    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s: nspace %s, add rank %lu offset %lu count %lu meta info",
                         __FILE__, __LINE__, __func__,
                         ns_info->ns_name, rinfo->rank, rinfo->offset, rinfo->count));

    tmp = ns_info->meta_seg;
    if (1 == _direct_mode) {
        /* get the last meta segment to put new rank_meta_info at the end. */
        while (NULL != tmp->next) {
            tmp = tmp->next;
        }
        num_elems = *((size_t*)(tmp->seg_info.seg_base_addr));
        if (_max_meta_elems <= num_elems) {
            PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                        "%s:%d:%s: extend meta segment for nspace %s",
                        __FILE__, __LINE__, __func__, ns_info->ns_name));
            /* extend meta segment, so create a new one */
            tmp = extend_segment(tmp, ns_info->ns_name);
            if (NULL == tmp) {
                PMIX_ERROR_LOG(PMIX_ERROR);
                return PMIX_ERROR;
            }
            ns_info->num_meta_seg++;
            memset(tmp->seg_info.seg_base_addr, 0, sizeof(rank_meta_info));
            /* update number of meta segments for namespace in initial_segment */
            ns_seg_info_t *elem = _get_ns_info_from_initial_segment(ns_info->ns_name);
            if (NULL == elem) {
                PMIX_ERROR_LOG(PMIX_ERROR);
                return PMIX_ERROR;
            }
            if (ns_info->num_meta_seg != elem->num_meta_seg) {
                elem->num_meta_seg = ns_info->num_meta_seg;
            }
            num_elems = 0;
        }
        cur_elem = (rank_meta_info*)((uint8_t*)(tmp->seg_info.seg_base_addr) + sizeof(size_t) + num_elems * sizeof(rank_meta_info));
        memcpy(cur_elem, rinfo, sizeof(rank_meta_info));
        num_elems++;
        memcpy(tmp->seg_info.seg_base_addr, &num_elems, sizeof(size_t));
    } else {
        /* directly compute index of meta segment (id) and relative offset (rel_offset)
         * inside this segment for fast lookup a rank_meta_info object for the requested rank. */
        id = rinfo->rank/_max_meta_elems;
        rel_offset = (rinfo->rank % _max_meta_elems) * sizeof(rank_meta_info) + sizeof(size_t);
        count = id;
        /* go through all existing meta segments for this namespace.
         * Stop at id number if it exists. */
        while (NULL != tmp->next && 0 != count) {
            tmp = tmp->next;
            count--;
        }
        /* if there is no segment with this id, then create all missing segments till the id number. */
        if ((int)ns_info->num_meta_seg < (id+1)) {
            while ((int)ns_info->num_meta_seg != (id+1)) {
                /* extend meta segment, so create a new one */
                tmp = extend_segment(tmp, ns_info->ns_name);
                if (NULL == tmp) {
                    PMIX_ERROR_LOG(PMIX_ERROR);
                    return PMIX_ERROR;
                }
                memset(tmp->seg_info.seg_base_addr, 0, sizeof(rank_meta_info));
                ns_info->num_meta_seg++;
            }
            /* update number of meta segments for namespace in initial_segment */
            ns_seg_info_t *elem = _get_ns_info_from_initial_segment(ns_info->ns_name);
            if (NULL == elem) {
                PMIX_ERROR_LOG(PMIX_ERROR);
                return PMIX_ERROR;
            }
            if (ns_info->num_meta_seg != elem->num_meta_seg) {
                elem->num_meta_seg = ns_info->num_meta_seg;
            }
        }
        /* store rank_meta_info object by rel_offset. */
        cur_elem = (rank_meta_info*)((uint8_t*)(tmp->seg_info.seg_base_addr) + rel_offset);
        memcpy(cur_elem, rinfo, sizeof(rank_meta_info));
    }
    return PMIX_SUCCESS;
}

static uint8_t *_get_data_region_by_offset(seg_desc_t *segdesc, size_t offset)
{
    seg_desc_t *tmp = segdesc;
    size_t rel_offset = offset;
    uint8_t *dataaddr = NULL;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s",
                         __FILE__, __LINE__, __func__));

    /* go through all existing data segments for this namespace */
    do {
        if (rel_offset >= _data_segment_size) {
            rel_offset -= _data_segment_size;
        } else {
            dataaddr = tmp->seg_info.seg_base_addr + rel_offset;
        }
        tmp = tmp->next;
    }
    while (NULL != tmp && NULL == dataaddr);
    return dataaddr;
}

static size_t get_free_offset(seg_desc_t *data_seg)
{
    size_t offset;
    seg_desc_t *tmp;
    int id = 0;
    tmp = data_seg;
    /* first find the last data segment */
    while (NULL != tmp->next) {
        tmp = tmp->next;
        id++;
    }
    offset = *((size_t*)(tmp->seg_info.seg_base_addr));
    if (0 == offset) {
        /* this is the first created data segment, the first 8 bytes are used to place the free offset value itself */
        offset = sizeof(size_t);
    }
    return (id * _data_segment_size + offset);
}

static int put_empty_ext_slot(seg_desc_t *dataseg)
{
    size_t global_offset, rel_offset, data_ended, sz, val;
    uint8_t *addr;
    global_offset = get_free_offset(dataseg);
    rel_offset = global_offset % _data_segment_size;
    if (rel_offset + EXT_SLOT_SIZE > _data_segment_size) {
        return PMIX_ERROR;
    }
    addr = _get_data_region_by_offset(dataseg, global_offset);
    strncpy((char *)addr, ESH_REGION_EXTENSION, PMIX_MAX_KEYLEN+1);
    val = 0;
    sz = sizeof(size_t);
    memcpy(addr + PMIX_MAX_KEYLEN + 1, &sz, sz);
    memcpy(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t), &val, sz);

    /* update offset at the beginning of current segment */
    data_ended = rel_offset + EXT_SLOT_SIZE;
    addr = (uint8_t*)(addr - rel_offset);
    memcpy(addr, &data_ended, sizeof(size_t));
    return PMIX_SUCCESS;
}

static size_t put_data_to_the_end(ns_track_elem_t *ns_info, seg_desc_t *dataseg, char *key, void *buffer, size_t size)
{
    size_t offset;
    seg_desc_t *tmp;
    int id = 0;
    size_t global_offset, data_ended;
    uint8_t *addr;
    size_t sz;

    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s: key %s",
                         __FILE__, __LINE__, __func__, key));

    tmp = dataseg;
    while (NULL != tmp->next) {
        tmp = tmp->next;
        id++;
    }
    global_offset = get_free_offset(dataseg);
    offset = global_offset % _data_segment_size;

    /* We should provide additional space at the end of segment to place EXTENSION_SLOT to have an ability to enlarge data for this rank.*/
    if (sizeof(size_t) + KVAL_SIZE(size) + EXT_SLOT_SIZE > _data_segment_size) {
        /* this is an error case: segment is so small that cannot place evem a single key-value pair.
         * warn a user about it and fail. */
        offset = 0; /* offset cannot be 0 in normal case, so we use this value to indicate a problem. */
        pmix_output(0, "PLEASE set NS_DATA_SEG_SIZE to value which is larger when %lu.",
                sizeof(size_t) + PMIX_MAX_KEYLEN + 1 + sizeof(size_t) + size + EXT_SLOT_SIZE);
        return offset;
    }
    if (offset + KVAL_SIZE(size) + EXT_SLOT_SIZE > _data_segment_size)  {
        id++;
        /* create a new data segment. */
        tmp = extend_segment(tmp, ns_info->ns_name);
        if (NULL == tmp) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            offset = 0; /* offset cannot be 0 in normal case, so we use this value to indicate a problem. */
            return offset;
        }
        ns_info->num_data_seg++;
        /* update_ns_info_in_initial_segment */
        ns_seg_info_t *elem = _get_ns_info_from_initial_segment(ns_info->ns_name);
        if (NULL == elem) {
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        elem->num_data_seg++;

        offset = sizeof(size_t);
    }
    global_offset = offset + id * _data_segment_size;
    addr = (uint8_t*)(tmp->seg_info.seg_base_addr)+offset;
    strncpy((char *)addr, key, PMIX_MAX_KEYLEN+1);
    sz = size;
    memcpy(addr + PMIX_MAX_KEYLEN + 1, &sz, sizeof(size_t));
    memcpy(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t), buffer, size);

    /* update offset at the beginning of current segment */
    data_ended = offset + KVAL_SIZE(size);
    addr = (uint8_t*)(tmp->seg_info.seg_base_addr);
    memcpy(addr, &data_ended, sizeof(size_t));
    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s: key %s, rel start offset %lu, rel end offset %lu, abs shift %lu size %lu",
                         __FILE__, __LINE__, __func__, key, offset, data_ended, id * _data_segment_size, size));
    return global_offset;
}

static int pmix_sm_store(ns_track_elem_t *ns_info, int rank, pmix_kval_t *kval, rank_meta_info **rinfo, int data_exist)
{
    size_t offset, size, kval_cnt;
    pmix_buffer_t *buffer;
    int rc;
    seg_desc_t *datadesc;
    uint8_t *addr;

    PMIX_OUTPUT_VERBOSE((2, pmix_globals.debug_output,
                         "%s:%d:%s: for rank %d, replace flag %d",
                         __FILE__, __LINE__, __func__, rank, data_exist));

    datadesc = ns_info->data_seg;
    /* pack value to the buffer */
    buffer = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(buffer, kval->value, 1, PMIX_VALUE))) {
        PMIX_RELEASE(buffer);
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    size = buffer->bytes_used;

    if (0 == data_exist) {
        /* there is no data blob for this rank yet, so add it. */
        size_t free_offset;
        free_offset = get_free_offset(datadesc);
        offset = put_data_to_the_end(ns_info, datadesc, kval->key, buffer->base_ptr, size);
        if (0 == offset) {
            /* this is an error */
            PMIX_RELEASE(buffer);
            PMIX_ERROR_LOG(PMIX_ERROR);
            return PMIX_ERROR;
        }
        /* if it's the first time when we put data for this rank, then *rinfo == NULL,
         * and even if segment was extended, and data was put into the next segment,
         * we don't need to extension slot at the end of previous segment.
         * If we try, we might overwrite other segments memory,
         * because previous segment is already full. */
        if (free_offset != offset && NULL != *rinfo) {
            /* here we compare previous free offset with the offset where we just put data.
             * It should be equal in the normal case. It it's not true, then it means that
             * segment was extended, and we put data to the next segment, so we now need to
             * put extension slot at the end of previous segment with a "reference" to a new_offset */
            size_t sz = sizeof(size_t);
            addr = _get_data_region_by_offset(datadesc, free_offset);
            strncpy((char *)addr, ESH_REGION_EXTENSION, PMIX_MAX_KEYLEN+1);
            memcpy(addr + PMIX_MAX_KEYLEN + 1, &sz, sizeof(size_t));
            memcpy(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t), &offset, sizeof(size_t));
        }
        if (NULL == *rinfo) {
            *rinfo = (rank_meta_info*)malloc(sizeof(rank_meta_info));
            (*rinfo)->rank = rank;
            (*rinfo)->offset = offset;
            (*rinfo)->count = 0;
        }
        (*rinfo)->count++;
    } else if (NULL != *rinfo) {
        /* there is data blob for this rank */
        addr = _get_data_region_by_offset(datadesc, (*rinfo)->offset);
        if (NULL == addr) {
            PMIX_RELEASE(buffer);
            PMIX_ERROR_LOG(PMIX_ERROR);
            return rc;
        }
        /* go through previous data region and find key matches.
         * If one is found, then mark this kval as invalidated.
         * Then put a new empty offset to the next extension slot,
         * and add new kval by this offset.
         * no need to update meta info, it's still the same. */
        kval_cnt = (*rinfo)->count;
        int add_to_the_end = 1;
        while (0 < kval_cnt) {
            /* data is stored in the following format:
             * key[PMIX_MAX_KEYLEN+1]
             * size_t size
             * byte buffer containing pmix_value, should be loaded to pmix_buffer_t and unpacked.
             * next kval pair
             * .....
             * extension slot which has key = EXTENSION_SLOT and a size_t value for offset to next data address for this process.
             */
            if (0 == strncmp((const char *)addr, ESH_REGION_EXTENSION, PMIX_MAX_KEYLEN+1)) {
                offset = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t));
                if (0 < offset) {
                    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                                "%s:%d:%s: for rank %d, replace flag %d %s is filled with %lu value",
                                __FILE__, __LINE__, __func__, rank, data_exist, ESH_REGION_EXTENSION, offset));
                    /* go to next item, updating address */
                    addr = _get_data_region_by_offset(datadesc, offset);
                    if (NULL == addr) {
                        PMIX_RELEASE(buffer);
                        PMIX_ERROR_LOG(PMIX_ERROR);
                        return rc;
                    }
                } else {
                    /* should not be, we should be out of cycle when this happens */
                }
            } else if (0 == strncmp((const char *)addr, kval->key, PMIX_MAX_KEYLEN+1)) {
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %d, replace flag %d found target key %s",
                            __FILE__, __LINE__, __func__, rank, data_exist, kval->key));
                /* target key is found, compare value sizes */
                size_t cur_size = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1);
                if (cur_size != size) {
                //if (1) { /* if we want to test replacing values for existing keys. */
                    /* invalidate current value and store another one at the end of data region. */
                    strncpy((char *)addr, ESH_REGION_INVALIDATED, PMIX_MAX_KEYLEN+1);
                    /* decrementing count, it will be incremented back when we add a new value for this key at the end of region. */
                    (*rinfo)->count--;
                    kval_cnt--;
                    /* go to next item, updating address */
                    addr += KVAL_SIZE(cur_size);
                    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                                "%s:%d:%s: for rank %d, replace flag %d mark key %s regions as invalidated. put new data at the end.",
                                __FILE__, __LINE__, __func__, rank, data_exist, kval->key));
                } else {
                    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                                "%s:%d:%s: for rank %d, replace flag %d replace data for key %s type %d in place",
                                __FILE__, __LINE__, __func__, rank, data_exist, kval->key, kval->value->type));
                    /* replace old data with new one. */
                    addr += PMIX_MAX_KEYLEN + 1;
                    memcpy(addr, &size, sizeof(size_t));
                    addr += sizeof(size_t);
                    memset(addr, 0, cur_size);
                    memcpy(addr, buffer->base_ptr, size);
                    addr += cur_size;
                    add_to_the_end = 0;
                    break;
                }
            } else {
                char ckey[PMIX_MAX_KEYLEN+1] = {0};
                strncpy(ckey, (const char *)addr, PMIX_MAX_KEYLEN+1);
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %d, replace flag %d skip %s key, look for %s key",
                            __FILE__, __LINE__, __func__, rank, data_exist, ckey, kval->key));
                /* Skip it: key is "INVALIDATED" or key is valid but different from target one. */
                if (0 != strncmp(ESH_REGION_INVALIDATED, ckey, PMIX_MAX_KEYLEN+1)) {
                    /* count only valid items */
                    kval_cnt--;
                }
                size_t size = *(size_t *)(addr + PMIX_MAX_KEYLEN + 1);
                /* go to next item, updating address */
                addr += KVAL_SIZE(size);
            }
        }
        if (1 == add_to_the_end) {
            /* if we get here, it means that we want to add a new item for the target rank, or
             * we mark existing item with the same key as "invalidated" and want to add new item
             * for the same key. */
            (*rinfo)->count++;
            size_t free_offset;
            free_offset = get_free_offset(datadesc);
            /* add to the end */
            offset = put_data_to_the_end(ns_info, datadesc, kval->key, buffer->base_ptr, size);
            if (0 == offset) {
                PMIX_RELEASE(buffer);
                PMIX_ERROR_LOG(PMIX_ERROR);
                return PMIX_ERROR;
            }
            /* we just reached the end of data for the target rank, and there can be two cases:
             * (1) - we are in the middle of data segment; data for this rank is separated from
             * data for different ranks, and that's why next element is EXTENSION_SLOT.
             * We put new data to the end of data region and just update EXTENSION_SLOT value by new offset.
             */
            if (0 == strncmp((const char *)addr, ESH_REGION_EXTENSION, PMIX_MAX_KEYLEN+1)) {
                PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                            "%s:%d:%s: for rank %d, replace flag %d %s should be filled with offset %lu value",
                            __FILE__, __LINE__, __func__, rank, data_exist, ESH_REGION_EXTENSION, offset));
                memcpy(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t), &offset, sizeof(size_t));
            } else {
                /* (2) - we point to the first free offset, no more data is stored further in this segment.
                 * There is no EXTENSION_SLOT by this addr since we continue pushing data for the same rank,
                 * and there is no need to split it.
                 * But it's possible that we reached the end of current data region and just jumped to the new region
                 * to put new data, in that case free_offset != offset and we must put EXTENSION_SLOT by the current addr
                 * forcibly and store new offset in its value. */
                if (free_offset != offset) {
                    /* segment was extended, need to put extension slot by free_offset indicating new_offset */
                    size_t sz = sizeof(size_t);
                    strncpy((char *)addr, ESH_REGION_EXTENSION, PMIX_MAX_KEYLEN+1);
                    memcpy(addr + PMIX_MAX_KEYLEN + 1, &sz, sz);
                    memcpy(addr + PMIX_MAX_KEYLEN + 1 + sizeof(size_t), &offset, sz);
                }
            }
            PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                        "%s:%d:%s: for rank %d, replace flag %d item not found ext slot empty, put key %s to the end",
                        __FILE__, __LINE__, __func__, rank, data_exist, kval->key));
        }
    }
    buffer->base_ptr = NULL;
    buffer->bytes_used = 0;
    PMIX_RELEASE(buffer);
    return rc;
}

static int _store_data_for_rank(ns_track_elem_t *ns_info, int rank, pmix_buffer_t *buf)
{
    int rc;
    int32_t cnt;

    pmix_buffer_t *bptr;
    pmix_kval_t *kp;
    seg_desc_t *metadesc, *datadesc;

    rank_meta_info *rinfo = NULL;
    size_t num_elems, free_offset, new_free_offset;
    int data_exist;

    PMIX_OUTPUT_VERBOSE((10, pmix_globals.debug_output,
                         "%s:%d:%s: for rank %d", __FILE__, __LINE__, __func__, rank));

    metadesc = ns_info->meta_seg;
    datadesc = ns_info->data_seg;

    if (NULL == datadesc || NULL == metadesc) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERROR;
    }

    num_elems = *((size_t*)(metadesc->seg_info.seg_base_addr));
    data_exist = 0;
    /* when we don't use linear search (_direct_mode ==0 ) we don't use num_elems field,
     * so anyway try to get rank_meta_info first. */
    if (0 < num_elems || 0 == _direct_mode) {
        /* go through all elements in meta segment and look for target rank. */
        rinfo = _get_rank_meta_info(rank, metadesc);
        if (NULL != rinfo) {
            data_exist = 1;
        }
    }
    /* incoming buffer may contain several inner buffers for different scopes,
     * so unpack these buffers, and then unpack kvals from each modex buffer,
     * storing them in the shared memory dstore.
     */
    cnt = 1;
    free_offset = get_free_offset(datadesc);
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(buf, &bptr, &cnt, PMIX_BUFFER))) {
        cnt = 1;
        kp = PMIX_NEW(pmix_kval_t);
        while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(bptr, kp, &cnt, PMIX_KVAL))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix: unpacked key %s", kp->key);
            if (PMIX_SUCCESS != (rc = pmix_sm_store(ns_info, rank, kp, &rinfo, data_exist))) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_RELEASE(kp); // maintain acctg - hash_store does a retain
            cnt = 1;
            kp = PMIX_NEW(pmix_kval_t);
        }
        cnt = 1;
        PMIX_RELEASE(kp);
        PMIX_RELEASE(bptr);  // free's the data region
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            break;
        }
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
    } else {
        rc = PMIX_SUCCESS;
    }

    /* Check if new data was put at the end of data segment.
     * It's possible that old data just was replaced with new one,
     * in that case we don't reserve space for EXTENSION_SLOT, it's
     * already reserved.
     * */
    new_free_offset = get_free_offset(datadesc);
    if (new_free_offset != free_offset) {
        /* Reserve space for EXTENSION_SLOT at the end of data blob.
         * We need it to split data for one rank from data for different
         * ranks and to allow extending data further.
         * We also put EXTENSION_SLOT at the end of each data segment, and
         * its value points to the beginning of next data segment.
         * */
        rc = put_empty_ext_slot(ns_info->data_seg);
        if (PMIX_SUCCESS != rc) {
            if (NULL != rinfo) {
                free(rinfo);
            }
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if this is the first data posted for this rank, then
     * update meta info for it */
    if (0 == data_exist) {
        set_rank_meta_info(ns_info, rinfo);
        if (NULL != rinfo) {
            free(rinfo);
        }
    }

    return rc;
}

static inline uint32_t _get_univ_size(const char *nspace)
{
    pmix_value_t *val = NULL;
    uint32_t nprocs = 0;
    pmix_nspace_t *ns, *nptr;

    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }

    if (nptr && (PMIX_SUCCESS == pmix_hash_fetch(&nptr->internal, PMIX_RANK_WILDCARD, PMIX_UNIV_SIZE, &val))) {
        if (val->type == PMIX_UINT32) {
            nprocs = val->data.uint32;
        }
        PMIX_VALUE_RELEASE(val);
    }

    return nprocs;
}
