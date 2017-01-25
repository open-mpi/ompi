/*
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_prot.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"

#define COMM_METHOD_MXM_YALLA 1   /* mxm    (via yalla pml) */
#define COMM_METHOD_MXM_CM    2   /* mxm    (via cm pml) */
#define COMM_METHOD_OFI       3   /* ofi    (via cm pml) */
#define COMM_METHOD_PSM       4   /* psm    (via cm pml) */
#define COMM_METHOD_VADER     5   /* vader  (via ob1 pml) */
#define COMM_METHOD_SM        6   /* sm     (via ob1 pml) */
#define COMM_METHOD_TCP       7   /* tcp    (via ob1 pml) */
#define COMM_METHOD_USNIC     8   /* usnic  (via ob1 pml) */
#define COMM_METHOD_OPENIB    9   /* openib (via ob1 pml) */
#define COMM_METHOD_SELF      10  /* self   (via ob1 pml) */
#define COMM_METHOD_PAMI      11  /* pami   (via pami pml) */
#define COMM_METHOD_PSM2      12  /* psm2   (via cm pml) */
#define COMM_METHOD_MAX       12

static int comm_method(MPI_Comm comm, int rank);
static char * comm_method_string(int method);
static int icompar(const void *a, const void *b);
static void abbreviate_list_into_string(char *str, int max, int *list, int nlist);
static void ompi_report_prots(int mode);


void ompi_hook_prot_mpi_init_bottom(int argc, char **argv, int requested, int *provided)
{
    if( hook_prot_enable_mpi_init ) {
        ompi_report_prots( 1 );
    }
}

void ompi_hook_prot_mpi_finalize_top(void)
{
    if( hook_prot_enable_mpi_finalize ) {
        ompi_report_prots( 2 );
    }
}

// ----------------------------------------------------------------------------

static int
comm_method(MPI_Comm comm, int rank) {
    void *pml_fp;
    void *dlsym_pami_fp;
    void *dlsym_yalla_fp;
    void *dlsym_ob1_fp;
    void *dlsym_cm_fp;

    void (*mca_pml_ob1__lookup_btl_fns_fp)(ompi_communicator_t* comm, int rank,
      void **sendfn, void **putfn);
    void (*mca_pml_cm__lookup_mtl_fns_fp)(void **finfn);


// First figure out which PML (pami,yalla,ob1,cm,v) is used based on
// mca_pml.pml_enable being one of mca_pml_{pami,yalla,ob1,cm}_enable.
// In general there's a certain amount of fragility here because
// if these symbols are static ('t' in nm) dlsym won't see them.
    pml_fp          = (void*) mca_pml.pml_enable;
    dlsym_pami_fp  = dlsym(RTLD_DEFAULT, "mca_pml_pami_enable");
    dlsym_yalla_fp  = dlsym(RTLD_DEFAULT, "mca_pml_yalla_enable");
    dlsym_ob1_fp    = dlsym(RTLD_DEFAULT, "mca_pml_ob1_enable");
    dlsym_cm_fp     = dlsym(RTLD_DEFAULT, "mca_pml_cm_enable");

    if (pml_fp == dlsym_pami_fp) {
        return COMM_METHOD_PAMI;
    }
    else if (pml_fp == dlsym_yalla_fp) {
        return COMM_METHOD_MXM_YALLA;
    }
    else if (pml_fp == dlsym_ob1_fp) {
        void *btl_fp, *btl_fp_put;
        void *dlsym_vader_send;
        void *dlsym_sm_send;
        void *dlsym_tcp_send;
        void *dlsym_openib_send;
        void *dlsym_self_send;
        void *dlsym_usnic_put;

        mca_pml_ob1__lookup_btl_fns_fp =
          dlsym(RTLD_DEFAULT, "mca_pml_ob1__lookup_btl_fns");
        if (!mca_pml_ob1__lookup_btl_fns_fp) {
            return 0;
        }
        mca_pml_ob1__lookup_btl_fns_fp(comm, rank,
            &btl_fp, &btl_fp_put);
        if (!btl_fp) { return 0; }

        dlsym_vader_send  = dlsym(RTLD_DEFAULT, "mca_btl_vader_send");
        dlsym_sm_send     = dlsym(RTLD_DEFAULT, "mca_btl_sm_send");
        dlsym_tcp_send    = dlsym(RTLD_DEFAULT, "mca_btl_tcp_send");
        dlsym_openib_send = dlsym(RTLD_DEFAULT, "mca_btl_openib_send");
        dlsym_self_send   = dlsym(RTLD_DEFAULT, "mca_btl_self_send");
// This asymmetry for usnic is due to the fragility mentioned above.
// The btl_send for usnic is usnic_send which is a static so we can't
// see it. But usnic happens to use a global for its put operation.
        dlsym_usnic_put   = dlsym(RTLD_DEFAULT, "opal_btl_usnic_put");

        if (btl_fp == dlsym_vader_send)       { return COMM_METHOD_VADER; }
        else if (btl_fp == dlsym_sm_send)     { return COMM_METHOD_SM; }
        else if (btl_fp == dlsym_tcp_send)    { return COMM_METHOD_TCP; }
        else if (btl_fp == dlsym_openib_send) { return COMM_METHOD_OPENIB; }
        else if (btl_fp == dlsym_self_send)   { return COMM_METHOD_SELF; }
        else if (btl_fp_put == dlsym_usnic_put) { return COMM_METHOD_USNIC; }
        else                                  { return 0; }
    }
    else if (pml_fp == dlsym_cm_fp) {
        void *mtl_fp;
        void *dlsym_mxm_finalize;
        void *dlsym_ofi_finalize;
        void *dlsym_psm_finalize;
        void *dlsym_psm2_finalize;

        mca_pml_cm__lookup_mtl_fns_fp =
          dlsym(RTLD_DEFAULT, "mca_pml_cm__lookup_mtl_fns");
        if (!mca_pml_cm__lookup_mtl_fns_fp) {
            return 0;
        }
        mca_pml_cm__lookup_mtl_fns_fp(&mtl_fp);
        if (!mtl_fp) { return 0; }

        dlsym_mxm_finalize = dlsym(RTLD_DEFAULT, "ompi_mtl_mxm_finalize");
        dlsym_ofi_finalize = dlsym(RTLD_DEFAULT, "ompi_mtl_ofi_finalize");
        dlsym_psm_finalize = dlsym(RTLD_DEFAULT, "ompi_mtl_psm_finalize");
        dlsym_psm2_finalize = dlsym(RTLD_DEFAULT, "ompi_mtl_psm2_finalize");

        if (mtl_fp == dlsym_mxm_finalize)       { return COMM_METHOD_MXM_CM; }
        else if (mtl_fp == dlsym_ofi_finalize)  { return COMM_METHOD_OFI; }
        else if (mtl_fp == dlsym_psm_finalize)  { return COMM_METHOD_PSM; }
        else if (mtl_fp == dlsym_psm2_finalize) { return COMM_METHOD_PSM2; }
        else                                    { return 0; }
    }
    else {
        // unrecognized PML
        return 0;
    }

    return 0;
}

// return abbreviation string
static char *
comm_method_string(int method)
{
    char *use = "--";
    switch (method) {
    case 0                     : { use = "n/a"; break; } // unconn or unknown
    case COMM_METHOD_PAMI      : { use = "pami"; break; }
    case COMM_METHOD_MXM_YALLA : { use = "mxm"; break; }
    case COMM_METHOD_MXM_CM    : { use = "mxmc"; break; }
    case COMM_METHOD_OFI       : { use = "ofi"; break; }
    case COMM_METHOD_PSM       : { use = "psm"; break; }
    case COMM_METHOD_PSM2      : { use = "psm2"; break; }
    case COMM_METHOD_VADER     : { use = "shm"; break; }
    case COMM_METHOD_SM        : { use = "sm"; break; }
    case COMM_METHOD_TCP       : { use = "tcp"; break; }
    case COMM_METHOD_USNIC     : { use = "usnic"; break; }
    case COMM_METHOD_OPENIB    : { use = "ib"; break; }
    case COMM_METHOD_SELF      : { use = "self"; break; }
    default                    : { use = "--"; break; }
    }
    return use;
}

static int
icompar(const void *a, const void *b) {
  if (*(int*)a < *(int*)b) { return -1; }
  if (*(int*)a > *(int*)b) { return 1; }
  return 0;
}

// Input list[] is expected to be sorted
static void
abbreviate_list_into_string(char *str, int max, int *list, int nlist)
{
    int lo, hi;
    int i;
    int per, tmp;

/*
 *  How much space do we need in strings to store rank numbers.
 *  A 10000 rank run needs more digits to write the rank numbers in than
 *  a 4 rank job.
 */
    per = 1;
    tmp = list[nlist-1];
    while (tmp >= 10) { ++per; tmp /= 10; }

    str[0] = 0;
    lo = hi = -1;
    for (i=0; i<nlist; ++i) {
        if (lo==-1) {
            lo = list[i];
            hi = list[i];
        }
/*
 *  Use hi,lo to specify contiguous chunks.  But if the current i is
 *  the start of a new contiguous chunk, print the previous hi,lo chunk.
 *  In general we can tell if we're allowed to write more into the string
 *  based on whether the previous iteration wrote ".." onto the end.
 */
        if (list[i] == hi+1) {
            hi = list[i];
        } else if (list[i] > hi) {
            if (strlen(str)==0 || str[strlen(str)-1] != '.') {
                if (strlen(str) != 0) {
                    strcpy(&str[strlen(str)], ", ");
                }
                if (lo != hi) {
                    sprintf(&str[strlen(str)], "%d - %d", lo, hi);
                } else {
                    sprintf(&str[strlen(str)], "%d", lo);
                }
            }
/*
 *  If we've almost written to the end of the string, and we haven't
 *  already written ".." to indicate we're not writing amy more, then
 *  add the "..".  Also set hi=lo=i since the data we just wrote is
 *  for the previous contiguous chunk, and the current i is the start
 *  of the next chunk.
 */
            if (((int)strlen(str)) >= max - 5 - 2*per
                &&
                (strlen(str) == 0 || str[strlen(str)-1] != '.'))
            {
                strcpy(&str[strlen(str)], ", ..");
                break;
            }
            hi = lo = list[i];
        }
    }
    if (strlen(str)==0 || str[strlen(str)-1] != '.') {
        if (strlen(str)!=0) {
            strcpy(&str[strlen(str)], ", ");
        }
        if (lo != hi) {
            sprintf(&str[strlen(str)], "%d - %d", lo, hi);
        } else {
            sprintf(&str[strlen(str)], "%d", lo);
        }
    }
}

// Input argument tells where we're being called from:
// 1 for init, 2 for finalize.
// The other implicit input is an environment variable we look at.
// When activated from init: we establish connections before printing.
// When activated from finalize: we just print whatever info is available.
static void
ompi_report_prots(int mode) // 1 = from init, 2 = from finalize
{
    int numhosts, i, j, k;
    char *p;
    int max2Dprottable = 12;
    int max2D1Cprottable = 36;
    int hostidprotbrief = 0;
    char * max2Dprotptr = NULL;
    char * hostidprotptr = NULL;
    int printTable;
    int hpmp_myrank, hpmp_nprocs;
    int mylocalrank, nlocalranks, myleaderrank, nleaderranks;
    int ret;
    ompi_communicator_t *local_comm, *leader_comm;
    int *method;
    char *hoststring;
    char **allhoststrings;

// early return in the case of spawn
    // PMPI_Comm_get_parent(&parent);
    if (ompi_mpi_comm_parent != MPI_COMM_NULL) { return; }

    hpmp_myrank = ompi_comm_rank(MPI_COMM_WORLD);
    hpmp_nprocs = ompi_comm_size(MPI_COMM_WORLD);

    if (hpmp_myrank == 0) {
        max2Dprotptr = getenv("MPI_PROT_MAX");
        if (max2Dprotptr) {
            max2Dprottable = atoi(max2Dprotptr);
            max2D1Cprottable = 3 * max2Dprottable;
        }

        hostidprotptr = getenv("MPI_PROT_BRIEF");
        if (hostidprotptr) { hostidprotbrief = atoi(hostidprotptr); }
    }

// Gathering layout data the same way osc_rdma_component.c does
    ret = ompi_comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, NULL,
      &local_comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return;
    }
    mylocalrank = ompi_comm_rank(local_comm);
    nlocalranks = ompi_comm_size(local_comm);

    ret = ompi_comm_split(MPI_COMM_WORLD,
        (0 == mylocalrank) ? 0 : MPI_UNDEFINED,
        hpmp_myrank, &leader_comm, false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ompi_comm_free(&local_comm);
        return;
    }

// Non-host-leaders return early.
    if (mylocalrank != 0) {
        ompi_comm_free(&local_comm);
        return;
    }
// -------------------------------------------------
// Only host-leaders exist from this point on.
// -------------------------------------------------
    myleaderrank = ompi_comm_rank(leader_comm);
    nleaderranks = numhosts = ompi_comm_size(leader_comm);

/*
 *  Allocate space for each rank to store its communication method
 *  on a per-host basis.  But rank 0 gets enough space to store the
 *  data for all pairs of hosts.
 */
    method = malloc(numhosts * sizeof(int) * (hpmp_myrank?1:numhosts));
    if (!method) {
        ompi_comm_free(&local_comm);
        ompi_comm_free(&leader_comm);
        return;
    }

// Each host leader figures out a string of basic info for its host
// in hoststring.  (allocated at all host leaders, can be different sizes)

    {
        int len;
        int *ranklist; // comm-world ranks contained in local_comm
                       // sorted into comm-world order (although
                       // local_comm should already be constructed
                       // in that way)
        int *ranklist_in;

        ompi_group_t *local_group, *world_group;
        ompi_comm_group(local_comm, &local_group);
        ompi_comm_group(MPI_COMM_WORLD, &world_group);
        ranklist = malloc(nlocalranks * sizeof(int) * 2);
        ranklist_in = ranklist + nlocalranks;
        for (i=0; i<nlocalranks; ++i) { ranklist_in[i] = i; }
        ompi_group_translate_ranks(local_group, nlocalranks, ranklist_in,
            world_group, ranklist);
        qsort(ranklist, nlocalranks, sizeof(int), &icompar); // hope redundant
        ompi_group_free(&local_group);
        ompi_group_free(&world_group);

        len = strlen(opal_process_info.nodename) + 100;
        hoststring  = malloc(len + 1);
        sprintf(hoststring, "Host %d [%s] ranks ",
            myleaderrank, opal_process_info.nodename);

        abbreviate_list_into_string(&hoststring[strlen(hoststring)],
            len - strlen(hoststring), ranklist, nlocalranks);
        free(ranklist);
    }

// If we're running during init, establish connections between all peers
// (in leader_comm, which is all the ranks that are here at this point)
    if (mode == 1) {
        for (i=0; i<=nleaderranks/2; ++i) {
// (Examples to show why the loop is i<=nleaderranks/2)
// np4 : 0 1 2 3    i=0 0c0  i=1 0c0&1&3  i=2 0c0&1&3&2
// np5 : 0 1 2 3 4  i=0 0c0  i=1 0c0&1&4  i=2 0c0&1&4&2&3
            MPI_Request sreq, rreq;
            MPI_Status status;
            int sbuf, rbuf;
            int speer = (myleaderrank + 1) % nleaderranks;
            int rpeer = (myleaderrank - 1 + nleaderranks) % nleaderranks;

            sbuf = rbuf = 0;
            MCA_PML_CALL(isend(&sbuf, 1, MPI_INT, speer, 99,
                    MCA_PML_BASE_SEND_STANDARD,
                    leader_comm, &sreq));
            MCA_PML_CALL(irecv(&rbuf, 1, MPI_INT, rpeer, 99,
                    leader_comm, &rreq));
            ompi_request_wait(&sreq, &status);
            ompi_request_wait(&rreq, &status);
        }
    }

// Each host leader fills in a "numhosts" sized array method[] of
// how it communicates with each peer.
    for (i=0; i<nleaderranks; ++i) {
        method[i] = comm_method(leader_comm, i);

// For looking at our own local host though, we don't really want "self"
// unless there's only one rank and "self" is the best answer. So if
// there's more than one rank on our host, we get our local-host's
// communication method for a neighbor on this host.
        if (i == myleaderrank) {
            if (nlocalranks > 1) {
                method[i] = comm_method(local_comm, 1);
            }
        }
    }

// Gather the strings and the methods at rank 0.
// The gatherv of the strings takes a few steps since we have to get
// the sizes first and allocate the receiving string.
    {
        int len, *lens, *disps;

        len = strlen(hoststring) + 1;
        if (myleaderrank == 0) {
            lens = malloc(nleaderranks * sizeof(int));
            disps = malloc(nleaderranks * sizeof(int));
        }
        leader_comm->c_coll.coll_gather(
            &len, 1, MPI_INT,
            lens, 1, MPI_INT,
            0, leader_comm, leader_comm->c_coll.coll_gather_module);
        if (myleaderrank == 0) {
            int tlen = 0;
            char *p;
            for (i=0; i<nleaderranks; ++i) {
                disps[i] = tlen;
                tlen += lens[i];
            }
            allhoststrings = malloc(nleaderranks * sizeof(char*)  +  tlen);
            p = (char*) (allhoststrings + nleaderranks);
            for (i=0; i<nleaderranks; ++i) {
                allhoststrings[i] = p;
                p += lens[i];
            }
            leader_comm->c_coll.coll_gatherv(
                hoststring, strlen(hoststring) + 1, MPI_CHAR,
                &allhoststrings[0][0], lens, disps, MPI_CHAR,
                0, leader_comm, leader_comm->c_coll.coll_gatherv_module);
        } else {
            // matching above call from rank 0, just &allhoststrings[0][0]
            // isn't legal here, and those args aren't used at non-root anyway
            leader_comm->c_coll.coll_gatherv(
                hoststring, strlen(hoststring) + 1, MPI_CHAR,
                NULL, NULL, NULL, MPI_CHAR,
                0, leader_comm, leader_comm->c_coll.coll_gatherv_module);
        }
        if (myleaderrank == 0) {
            free(lens);
            free(disps);
        }
// and a simpler gather for the methods
        leader_comm->c_coll.coll_gather(
            method, nleaderranks, MPI_INT,
            method, nleaderranks, MPI_INT,
            0, leader_comm, leader_comm->c_coll.coll_gather_module);
    }
    ompi_comm_free(&local_comm);
    ompi_comm_free(&leader_comm);

// Interception for testing purposes. Let rank-0 meddle with all its method[]
// settings, this is only for testing, eg to make sure the printing comes out
// right.
    if (myleaderrank == 0) {
        p = getenv("MPI_PROT_FAKEFILE");
        if (p && *p) {
            FILE *fp;
            int setting;
            fp = fopen(p, "r");
            for (i=0; i<nleaderranks; ++i) {
                for (k=0; k<nleaderranks; ++k) {
                    fscanf(fp, "%d", &setting);
                    // let -1 mean "use existing (real) setting"
                    if (setting != -1) {
                        method[i * nleaderranks + k] = setting;
                    }
                }
                fscanf(fp, "\n");
            }
            fclose(fp);
        }
    }

// Print
// 1. the hoststring each host contributed
// 2. the 2d table in method[] if it isn't too big
// 3. summary of on/off host interconnect, and list the exceptions
    if (myleaderrank == 0) {
// 1: hoststring for each host
        for (i=0; i<nleaderranks; ++i) {
          printf("%s\n", allhoststrings[i]);
        }
        printf("\n");
// 2: 2d table
        if (nleaderranks <= max2Dprottable) {
            char *str, *p;
            int tmp, per;
            int strlens[COMM_METHOD_MAX + 1];

            // characters per entry in the 2d table, must be large enough
            // for the digits needed for host numbers, and for whatever is
            // the longest string used in the table, plus a space.
            for (i=0; i<=COMM_METHOD_MAX; ++i) {
                strlens[i] = strlen(comm_method_string(i));
            }
            per = 2;
            tmp = nleaderranks;
            while (tmp >= 10) { ++per; tmp /= 10; }
            for (i=0; i<nleaderranks; ++i) {
                for (k=0; k<nleaderranks; ++k) {
                    tmp = strlens[method[i * nleaderranks + k]];
                    if (tmp+1 > per) { per = tmp+1; }
                }
            }

            str = malloc(nleaderranks * per + 1);
            p = str;
            for (i=0; i<nleaderranks; ++i) {
                sprintf(p, "%d", i);
                for (j=(int)strlen(p); j<per; ++j) {
                    p[j] = ' ';
                }
                p[j] = 0;
                p += j;
            }
            tmp = (int)strlen(str);
            --p;
            while (p>=str && ((*p)==' ')) { *(p--)=0; }
            printf(" host | %s\n", str);
            memset(str, (int)'=', tmp);
            printf("======|=%s\n", str);

            for (i=0; i<nleaderranks; ++i) {
                str[0] = 0;
                p = str;
                for (k=0; k<nleaderranks; ++k) {
                    strcat(p, comm_method_string(method[i * nleaderranks + k]));
                    for (j=(int)strlen(p); j<per; ++j) {
                        p[j] = ' ';
                    }
                    p[j] = 0;
                    p += j;
                }
                --p;
                while (p>str && *p==' ') { *(p--)=0; }
                printf("%5d : %s\n", i, str);
            }
            printf("\n");
            free(str);
        }
        else if (nleaderranks <= max2D1Cprottable) {
            char *str, *p;
            int tmp, per, done;
            char char_code[COMM_METHOD_MAX + 1], next_char;
            int method_count[COMM_METHOD_MAX + 1];

            // characters for the number column in the 2d table,
            // must be large enough for the digits needed for host numbers
            per = 2;
            tmp = nleaderranks;
            while (tmp >= 10) { ++per; tmp /= 10; }

            // pick a character code for each comm method based on
            // how many times it's in the table, use 'A' for the least common
            for (i=0; i<=COMM_METHOD_MAX; ++i) {
                char_code[i] = 0;
                method_count[i] = 0;
            }
            for (i=0; i<nleaderranks; ++i) {
                for (k=0; k<nleaderranks; ++k) {
                    tmp = method[i * nleaderranks + k];
                    ++method_count[tmp];
                }
            }
            next_char = 'A';
            done = 0;
            while (!done) {
                int next_idx = -1;
                done = 1;
                for (i=0; i<=COMM_METHOD_MAX; ++i) {
                    if (!char_code[i] && method_count[i]) {
                        done = 0;
                        if ( (next_idx == -1) ||
                             (method_count[i] < method_count[next_idx]))
                        {
                            next_idx = i;
                        }
                    }
                }
                if (next_idx != -1) {
                    char_code[next_idx] = next_char;
                    ++next_char;
                }
            }

            str = malloc(per + 32 + nleaderranks * 2 + 1);
            p = str;
            sprintf(p, "0 1 2 3 ", i);
            p += 8;
            for (i=4; i<nleaderranks; i+=4) {
                sprintf(p, "%d", i);
                for (j=(int)strlen(p); j<8; ++j) {
                    p[j] = ' ';
                }
                p[j] = 0;
                p += j;
            }
            --p;
            while (p>=str && ((*p)==' ')) { *(p--)=0; }
            tmp = (int)strlen(str) + 2;
            printf(" host | %s\n", str);
            memset(str, (int)'=', tmp);
            printf("======|=%s\n", str);

            for (i=0; i<nleaderranks; ++i) {
                str[0] = 0;
                p = str;
                for (k=0; k<nleaderranks; ++k) {
                    p[0] = char_code[method[i * nleaderranks + k]];
                    p[1] = ' ';
                    p[2] = 0;
                    p += 2;
                }
                --p;
                while (p>str && *p==' ') { *(p--)=0; }
                printf("%5d : %s\n", i, str);
            }
            free(str);
            for (i=0; i<=COMM_METHOD_MAX; ++i) {
                for (k=0; k<=COMM_METHOD_MAX; ++k) {
                    if (char_code[k] == 'A' + i) {
                        printf("key: %c == %s\n", char_code[k],
                            comm_method_string(k));
                    }
                }
            }
            printf("\n");
        }
// 3: abbreviated summary of interconnect and outliers
// - check diagonal for uniformity + self, save majority method
// - check non-diagonal for uniformity, save majority method
// - print ranks with non-majority settings
        {
            int method_count[COMM_METHOD_MAX + 1];
            int majority_method_onhost;
            int majority_method_offhost;
            int uniformity_onhost;
            int uniformity_offhost;
            int any_self = 0;

            printf("Connection summary:\n");

            majority_method_onhost = -1;
            uniformity_onhost = 1;
            for (i=0; i<=COMM_METHOD_MAX; ++i) { method_count[i] = 0; }
            for (i=0; i<nleaderranks; ++i) {
                int this_method = method[i * nleaderranks  +  i];
                ++method_count[this_method];

                if (this_method == 0) { continue; }

                if (majority_method_onhost == -1 &&
                    this_method != COMM_METHOD_SELF)
                {
                    majority_method_onhost = this_method;
                }
                if (majority_method_onhost != -1 &&
                    this_method != majority_method_onhost &&
                    this_method != COMM_METHOD_SELF)
                {
                    uniformity_onhost = 0;
                }
            }
            // the above majority_method_onhost isn't yet correct
            majority_method_onhost = COMM_METHOD_SELF;
            for (i=0; i<=COMM_METHOD_MAX; ++i) {
                if (method_count[i] > 0 &&
                    majority_method_onhost == COMM_METHOD_SELF)
                {
                    majority_method_onhost = i;
                }
                if (method_count[i] > method_count[majority_method_onhost]) {
                    if (i != COMM_METHOD_SELF) {
                        majority_method_onhost = i;
                    }
                }
            }
            if (method_count[COMM_METHOD_SELF] > 0) { any_self = 1; }

            majority_method_offhost = -1;
            uniformity_offhost = 1;
            for (i=0; i<=COMM_METHOD_MAX; ++i) { method_count[i] = 0; }
            for (i=0; i<nleaderranks; ++i) {
                for (k=0; k<nleaderranks; ++k) {
                    if (k == i) continue; // this i/k loop is for all off-host

                    int this_method = method[i * nleaderranks  +  k];
                    ++method_count[this_method];

                    if (this_method == 0) { continue; }

                    if (majority_method_offhost == -1) {
                        majority_method_offhost = this_method;
                    }
                    if (majority_method_offhost != -1 &&
                        this_method != majority_method_offhost)
                    {
                        uniformity_offhost = 0;
                    }
                }
            }
            // the above majority_method_offhost isn't yet correct
            majority_method_offhost = 0;
            for (i=0; i<=COMM_METHOD_MAX; ++i) {
                if (method_count[i] > 0 && majority_method_offhost == 0) {
                    majority_method_offhost = i;
                }
                if (method_count[i] > method_count[majority_method_offhost]) {
                    majority_method_offhost = i;
                }
            }

            char *all_or_most = "all";
            char *or_self = "";
            if (!uniformity_onhost) {
                all_or_most = "most";
            }
            if ((majority_method_onhost != COMM_METHOD_SELF) && any_self) {
                or_self = " or self";
            }
            printf("  on-host:  %s connections are %s%s\n", all_or_most,
                comm_method_string(majority_method_onhost), or_self);

            all_or_most = "all";
            if (!uniformity_offhost) {
                all_or_most = "most";
            }
            printf("  off-host: %s connections are %s\n", all_or_most,
                comm_method_string(majority_method_offhost));

            if (!uniformity_onhost || !uniformity_offhost) {
                printf("Exceptions:\n");
                for (i=0; i<nleaderranks; ++i) {
                    int is_nonconformist = 0;
                    for (k=0; k<=COMM_METHOD_MAX; ++k) { method_count[k] = 0; }
                    // count how many non-majority methods show up
                    for (k=0; k<nleaderranks; ++k) {
                        int this_method = method[i * nleaderranks  +  k];

                        ++method_count[this_method];

                        if (k == i &&
                            this_method != majority_method_onhost &&
                            this_method != COMM_METHOD_SELF &&
                            this_method != 0)
                        {
                            is_nonconformist = 1;
                        }
                        if (k != i &&
                            this_method != majority_method_offhost &&
                            this_method != 0)
                        {
                            is_nonconformist = 1;
                        }
                    }
                    if (is_nonconformist) {
                        char *str = malloc(1024);
//                      int first = 1;
                        sprintf(str, "  host %d:", i);
                        for (k=0; k<=COMM_METHOD_MAX; ++k) {
                            if (method_count[k] > 0) {
//                              if (!first) {
//                                  strcat(str, " /");
//                              }
                                sprintf(&str[strlen(str)],
                                    " [%dx %s]",
                                    method_count[k],
                                    comm_method_string(k));
//                              first = 0;
                            }
                        }
                        printf("%s\n", str);
                        free(str);
                    }
                }
            }
            printf("\n");
        }
    }

    if (myleaderrank == 0) {
        free(allhoststrings);
    }
    free(method);
}
