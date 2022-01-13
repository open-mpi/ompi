/*
 * Copyright (c) 2016-2020 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "hook_comm_method.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/btl/btl.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/mtl/base/base.h"

// In regular strncpy up to n bytes are copied, so if the 'to' buffer
// was char string[16] and you called strncpy(string, , 16) you could
// get 16 bytes of chars without a null.  My preferred API is to let
// n be the size of the buffer, and to let n-1 chars be copied, and
// to guarantee null termination.
static void
mystrncpy(char *to, const char *from, int n) {
    strncpy(to, from, n-1);
    to[n-1] = 0;
}

// For converting comm_method strings to comm_method id# and back.
// This starts as our local set of strings, but gets Allreduced into
// a global mapping so all the strings at all the ranks are represented.
// If an MCA's name is more than 15 chars it gets truncated.
#define COMM_METHOD_STRING_SIZE 16
#define MAX_COMM_METHODS 50
typedef struct {
    int n;
    char str[MAX_COMM_METHODS][COMM_METHOD_STRING_SIZE];
} comm_method_string_conversion_t;

static comm_method_string_conversion_t comm_method_string_conversion;

#define MODE_IS_PML 1
#define MODE_IS_MTL 2
#define MODE_IS_BTL 3

// ----------------------------------------------------------------------------

// return the pml's module:component:name function pointer in fp
static char*
lookup_pml_name(void)
{
    return (char*) mca_pml_base_selected_component.pmlm_version.mca_component_name;
}
// return the mtl's module:component:name function pointer in fp
static char*
lookup_mtl_name(void)
{
    if (!ompi_mtl_base_selected_component) { return NULL; }
    return (char*) ompi_mtl_base_selected_component->mtl_version.mca_component_name;
}
// Find the send btl's module:component:name for the incoming comm,rank
static char*
lookup_btl_name_for_send(ompi_communicator_t* comm, int rank) {
    ompi_proc_t *dst_proc = ompi_group_peer_lookup(comm->c_remote_group, rank);

    mca_bml_base_endpoint_t* endpoint = mca_bml_base_get_endpoint(dst_proc);
    if (endpoint &&
        endpoint->btl_send.bml_btls &&
        endpoint->btl_send.bml_btls[0].btl)
    {
        return (char*)
            endpoint->btl_send.bml_btls[0].btl->btl_component->btl_version.mca_component_name;
    }
    return NULL;
}

// Use the above to lookup the mca_component_name for the rank's
// sending BTL/MTL/PML.  The extra 3rd argument returns whether
// MODE_IS_BTL / MTL / PML (can pass NULL if you don't want that info).
//
// And this one puts the result into malloced mem of size
//     COMM_METHOD_STRING_SIZE
// that the caller has to free.
static char *
comm_method_string(MPI_Comm comm, int rank, int *comm_mode) {
    char *p;
    char *string = malloc(COMM_METHOD_STRING_SIZE);

    if (!string) { return NULL; }

    p = lookup_pml_name();
    if (p && 0==strncmp("ob1", p, 4)) {      // BTL
        if (comm_mode) { *comm_mode = MODE_IS_BTL; }
        mystrncpy(string, lookup_btl_name_for_send(comm, rank), COMM_METHOD_STRING_SIZE);
    }
    else if (p && 0==strncmp("cm", p, 3)) {  // MTL
        if (comm_mode) { *comm_mode = MODE_IS_MTL; }
        mystrncpy(string, lookup_mtl_name(), COMM_METHOD_STRING_SIZE);
    } else {                        // PML
        if (comm_mode) { *comm_mode = MODE_IS_PML; }
        mystrncpy(string, p, COMM_METHOD_STRING_SIZE);
    }
    return string;
}

// ----------------------------------------------------------------------------

// Managing the comm_method_string_conversion structure
// and using it to convert strings to/from id numbers:

// The data should be
//     string 0 ==       "n/a" for unconnected / unknown
//     string 1,2,... == "tcp" "self", etc, sorted
// self is important enough we want to make sure it's always in the list
static void
init_string_to_conversion_struct(comm_method_string_conversion_t *data)
{
    data->n = 0;
    mystrncpy(data->str[data->n], "n/a", COMM_METHOD_STRING_SIZE);
    ++(data->n);
    mystrncpy(data->str[data->n], "self", COMM_METHOD_STRING_SIZE);
    ++(data->n);
}

static int
lookup_string_in_conversion_struct(comm_method_string_conversion_t *data, char *string)
{
    int i;
    for (i=0; i<data->n; ++i) {
        if (0==strncmp(data->str[i], string, COMM_METHOD_STRING_SIZE)) {
            return i;
        }
    }
    return 0;
}

// For qsort of the str[] array in a comm_method_string_conversion_t
static int mycompar(const void *a, const void *b) {
    return strcmp(a, b);
}

static void
add_string_to_conversion_struct(comm_method_string_conversion_t *data, char *string)
{
    int i;
    if (0 == strcmp(string, "n/a")) { return; }

    i = lookup_string_in_conversion_struct(data, string);
    if (i == 0) { // didn't find string in list, so add it
        if (data->n < MAX_COMM_METHODS) {
            mystrncpy(data->str[data->n], string, COMM_METHOD_STRING_SIZE);
            ++(data->n);
        }
    }
    qsort(&data->str[1], data->n - 1, COMM_METHOD_STRING_SIZE, &mycompar);
}

// For MPI_Allreduce of a comm_method_string_conversion_t
static void myfn(void* invec, void* inoutvec, int *len, MPI_Datatype *dt) {
    comm_method_string_conversion_t *a, *b;
    int i, j;

    for (i=0; i<*len; ++i) {
        b = &((comm_method_string_conversion_t*)invec)[i];
        a = &((comm_method_string_conversion_t*)inoutvec)[i];
        for (j=0; j<b->n; ++j) { // for each entry j in 'b', add it to 'a'
            add_string_to_conversion_struct(a, b->str[j]);
        }
        qsort(&a->str[1], a->n - 1, COMM_METHOD_STRING_SIZE, &mycompar);
    }
}

// ----------------------------------------------------------------------------

// Use the static global comm_method_string_conversion to convert
// between comm_method string and id number

// This might convert "pami" for example to 1, "yalla" to 2, etc.
static int
string_to_comm_method(char *str) {
    // default to "n/a" for any bad or unrecognized inputs
    if (!str || !str[0]) { return 0; }

    return lookup_string_in_conversion_struct(&comm_method_string_conversion, str);
}

static char *
comm_method_to_string(int id) {
    return comm_method_string_conversion.str[id];
}

static int
comm_method(MPI_Comm comm, int rank) {
    char *p = comm_method_string(comm, rank, NULL);
    int id = string_to_comm_method(p);
    free(p);
    return id;
}

#define COMM_METHOD_SELF (string_to_comm_method("self"))
#define NUM_COMM_METHODS (comm_method_string_conversion.n)

// ----------------------------------------------------------------------------

typedef void (*VoidFuncPtr)(void); // a function pointer to a function that takes no arguments and returns void.
static char* comm_method_string(MPI_Comm comm, int rank, int *comm_mode);
static int comm_method(MPI_Comm comm, int rank);
static char* comm_method_to_string(int method);
static int icompar(const void *a, const void *b);
static void abbreviate_list_into_string(char *str, int max, int *list, int nlist);
static void ompi_report_comm_methods(int called_from_location);
static void host_leader_printstring(char *hoststring,
    int myleaderrank, int nleaderranks, ompi_communicator_t *leader_comm,
    int cols, int indent);
static void host_local_get_affstring(char **affstring, int indentation,
    int mylocalrank, int nlocalranks, ompi_communicator_t *local_comm);

void ompi_hook_comm_method_mpi_init_bottom(int argc, char **argv, int requested, int *provided)
{
    if( mca_hook_comm_method_enable_mpi_init ) {
        ompi_report_comm_methods( 1 );
    }
}

void ompi_hook_comm_method_mpi_finalize_top(void)
{
    if( mca_hook_comm_method_enable_mpi_finalize ) {
        ompi_report_comm_methods( 2 );
    }
}

// ----------------------------------------------------------------------------

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
/*
 *  Note about string size: 'max' is the number of chars that
 *  can be printed inside str which was allocated as max+1 total space
 *  to allow 'max' chars plus a null termination.  So for snprintf()
 *  that translates to size of max+1.  Or when writing on the end
 *  of str, eg snprintfing into &str[strlen(str)] that would be
 *  max - strlen(str) space for printable chars so
 *  max - strlen(str) + 1 as the size argument in snprintf().
 */
        if (list[i] == hi+1) {
            hi = list[i];
        } else if (list[i] > hi) {
            if (strlen(str)==0 || str[strlen(str)-1] != '.') {
                if (strlen(str) != 0) {
                    mystrncpy(&str[strlen(str)], ", ", max - strlen(str) + 1);
                }
                if (lo != hi) {
                    snprintf(&str[strlen(str)], max - strlen(str) + 1,
                        "%d - %d", lo, hi);
                } else {
                    snprintf(&str[strlen(str)], max - strlen(str) + 1,
                        "%d", lo);
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
                mystrncpy(&str[strlen(str)], ", ..", max - strlen(str) + 1);
                break;
            }
            hi = lo = list[i];
        }
    }
    if (strlen(str)==0 || str[strlen(str)-1] != '.') {
        if (strlen(str)!=0) {
            mystrncpy(&str[strlen(str)], ", ", max - strlen(str) + 1);
        }
        if (lo != hi) {
            snprintf(&str[strlen(str)], max - strlen(str) + 1,
                "%d - %d", lo, hi);
        } else {
            snprintf(&str[strlen(str)], max - strlen(str) + 1,
                "%d", lo);
        }
    }
}

// Input argument tells where we're being called from:
// 1 for init, 2 for finalize.
// The other implicit input is an environment variable we look at.
// When activated from init: we establish connections before printing.
// When activated from finalize: we just print whatever info is available.
static void
ompi_report_comm_methods(int called_from_location) // 1 = from init, 2 = from finalize
{
    int i, j, k;
    int max2Dprottable = 12;
    int max2D1Cprottable = 36;
    int hpmp_myrank;
    int mylocalrank, nlocalranks, myleaderrank, nleaderranks;
    int ret;
    ompi_communicator_t *local_comm, *leader_comm;
    int *method;
    int *mymethod;
    char *hoststring;
    char *affstring;
    int comm_mode; // MODE_IS_BTL / MTL / PML

// early return in the case of spawn
    // PMPI_Comm_get_parent(&parent);
    if (ompi_mpi_comm_parent != MPI_COMM_NULL) { return; }

    hpmp_myrank = ompi_comm_rank(MPI_COMM_WORLD);
    // hpmp_nprocs = ompi_comm_size(MPI_COMM_WORLD);

    max2Dprottable = mca_hook_comm_method_max;
    max2D1Cprottable = 3 * max2Dprottable;
    if (mca_hook_comm_method_brief) {
        // force only the short summary output to be printed with no 2d table:
        max2Dprottable = 0;
        max2D1Cprottable = 0;
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

    char prefix_string[64];
    char *unprefixed_affstring;
    prefix_string[0] = 0;
    if (mylocalrank == 0) {
        myleaderrank = ompi_comm_rank(leader_comm);
        snprintf(prefix_string, 64, "H%d: ", myleaderrank);
    }
    int indentation = strlen(prefix_string);
    /* reject really small aff_columns settings*/
    if (mca_hook_comm_method_aff_columns < indentation + 16) {
        mca_hook_comm_method_aff_columns = indentation + 16;
    }
    host_local_get_affstring(&unprefixed_affstring, indentation,
        mylocalrank, nlocalranks, local_comm);
    if (mylocalrank == 0) {
        int affstring_sz = strlen(prefix_string) + strlen(unprefixed_affstring) + 16;
        affstring = malloc(affstring_sz);
        snprintf(affstring, affstring_sz, "%s%s", prefix_string, unprefixed_affstring);
        free(unprefixed_affstring);
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
    nleaderranks = ompi_comm_size(leader_comm);

/*
 *  Allocate space for each rank to store its communication method
 *  on a per-host basis.  But rank 0 gets enough space to store the
 *  data for all pairs of hosts.
 */
    mymethod = method = malloc(nleaderranks * sizeof(int));
    if (hpmp_myrank == 0) {
        method = malloc(nleaderranks * sizeof(int) * nleaderranks);
    }
    if (!method || !mymethod) {
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
        snprintf(hoststring, len + 1, "Host %d [%s] ranks ",
            myleaderrank, opal_process_info.nodename);

        abbreviate_list_into_string(&hoststring[strlen(hoststring)],
            len - strlen(hoststring), ranklist, nlocalranks);
        free(ranklist);
    }

// If we're running during init, establish connections between all peers
// (in leader_comm, which is all the ranks that are here at this point)
    if (called_from_location == 1) {
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

// Each host will have a list of comm_method strings, and in
// order to associate them with numbers we'll need to Allreduce
// to get a comprehensive list of strings across the ranks
    init_string_to_conversion_struct(&comm_method_string_conversion);
    for (i=0; i<nleaderranks; ++i) {
        char *p = comm_method_string(leader_comm, i, &comm_mode);
        add_string_to_conversion_struct(&comm_method_string_conversion, p);
        free(p);
    }
    if (nlocalranks > 1) {
        char *p = comm_method_string(local_comm, 1, NULL);
        add_string_to_conversion_struct(&comm_method_string_conversion, p);
        free(p);
    }

    MPI_Datatype mydt;
    MPI_Op myop;
    MPI_Type_contiguous(sizeof(comm_method_string_conversion_t), MPI_BYTE, &mydt);
    MPI_Type_commit(&mydt);
    MPI_Op_create(myfn, 1, &myop);
    leader_comm->c_coll->coll_allreduce(
        MPI_IN_PLACE, (void*)&comm_method_string_conversion, 1, mydt, myop, leader_comm,
            leader_comm->c_coll->coll_allreduce_module);
    MPI_Op_free(&myop);
    MPI_Type_free(&mydt);

// Each host leader fills in a "nleaderranks" sized array method[] of
// how it communicates with each peer.
    for (i=0; i<nleaderranks; ++i) {
        mymethod[i] = comm_method(leader_comm, i);

// For looking at our own local host though, we don't really want "self"
// unless there's only one rank and "self" is the best answer. So if
// there's more than one rank on our host, we get our local-host's
// communication method for a neighbor on this host.
        if (i == myleaderrank) {
            if (nlocalranks > 1) {
                mymethod[i] = comm_method(local_comm, 1);
            }
        }
    }

// Gather the methods at rank 0.
    leader_comm->c_coll->coll_gather(
        mymethod, nleaderranks, MPI_INT,
        method, nleaderranks, MPI_INT,
        0, leader_comm, leader_comm->c_coll->coll_gather_module);

// Interception for testing purposes. Let rank-0 meddle with all its method[]
// settings, this is only for testing, eg to make sure the printing comes out
// right.
    if (myleaderrank == 0) {
        if (mca_hook_comm_method_fakefile) {
            FILE *fp;
            int setting;
            fp = fopen(mca_hook_comm_method_fakefile, "r");
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

// 1: hoststring for each host
    host_leader_printstring(hoststring, myleaderrank, nleaderranks, leader_comm, -1, -1);
    if (myleaderrank == 0) {
        printf("\n");
    }

// 1b: affstring for each host
    if (mca_hook_comm_method_enable_show_aff) {
        if (myleaderrank == 0) {
            printf("Affinity per host: (with ompi_display_comm_aff_columns %d)\n",
                mca_hook_comm_method_aff_columns);
        }
        host_leader_printstring(affstring, myleaderrank, nleaderranks, leader_comm,
            mca_hook_comm_method_aff_columns, indentation + 2);
        if (myleaderrank == 0) {
            printf("\n");
        }
        free(affstring);
    }

    ompi_comm_free(&local_comm);
    ompi_comm_free(&leader_comm);

    if (myleaderrank == 0) {
// 2: 2d table
        if (nleaderranks <= max2Dprottable) {
            char *str, *p;
            int tmp, per;
            int strlens[NUM_COMM_METHODS];

            // characters per entry in the 2d table, must be large enough
            // for the digits needed for host numbers, and for whatever is
            // the longest string used in the table, plus a space.
            for (i=0; i<NUM_COMM_METHODS; ++i) {
                strlens[i] = strlen(comm_method_to_string(i));
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

            int str_sz = nleaderranks * per + 1;
            str = malloc(str_sz);
            p = str;
            for (i=0; i<nleaderranks; ++i) {
                snprintf(p, str_sz - (p - str), "%d", i);
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
            str[tmp] = 0;
            printf("======|=%s\n", str);

            for (i=0; i<nleaderranks; ++i) {
                str[0] = 0;
                p = str;
                for (k=0; k<nleaderranks; ++k) {
                    strcat(p, comm_method_to_string(method[i * nleaderranks + k]));
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
            char char_code[NUM_COMM_METHODS], next_char;
            int method_count[NUM_COMM_METHODS];

            // characters for the number column in the 2d table,
            // must be large enough for the digits needed for host numbers
            per = 2;
            tmp = nleaderranks;
            while (tmp >= 10) { ++per; tmp /= 10; }

            // pick a character code for each comm method based on
            // how many times it's in the table, use 'A' for the least common
            for (i=0; i<NUM_COMM_METHODS; ++i) {
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
                for (i=0; i<NUM_COMM_METHODS; ++i) {
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

            int str_sz = per + 32 + nleaderranks * 2 + 1;
            str = malloc(str_sz);
            p = str;
            snprintf(p, str_sz, "0 1 2 3 ");
            p += 8;
            for (i=4; i<nleaderranks; i+=4) {
                snprintf(p, str_sz - (p - str), "%d", i);
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
            str[tmp] = 0;
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
            for (i=0; i<NUM_COMM_METHODS; ++i) {
                for (k=0; k<NUM_COMM_METHODS; ++k) {
                    if (char_code[k] == 'A' + i) {
                        printf("key: %c == %s\n", char_code[k],
                            comm_method_to_string(k));
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
            int method_count[NUM_COMM_METHODS];
            int majority_method_onhost;
            int majority_method_offhost;
            int uniformity_onhost;
            int uniformity_offhost;
            int any_self = 0;

            char *btl_etc = "btl";
            if (comm_mode == MODE_IS_MTL) { btl_etc = "mtl"; }
            if (comm_mode == MODE_IS_PML) { btl_etc = "pml"; }
            printf("Connection summary: (%s)\n", btl_etc);

            majority_method_onhost = -1;
            uniformity_onhost = 1;
            for (i=0; i<NUM_COMM_METHODS; ++i) { method_count[i] = 0; }
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
            for (i=0; i<NUM_COMM_METHODS; ++i) {
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
            for (i=0; i<NUM_COMM_METHODS; ++i) { method_count[i] = 0; }
            for (i=0; i<nleaderranks; ++i) {
                for (k=0; k<nleaderranks; ++k) {
                    if (k == i) continue; // this i/k loop is for all off-host

                    int this_method = method[i * nleaderranks  +  k];
                    ++method_count[this_method];

                    if (this_method == 0) { continue; }

                    if (majority_method_offhost == -1) {
                        majority_method_offhost = this_method;
                    }
                    if (this_method != majority_method_offhost)
                    {
                        uniformity_offhost = 0;
                    }
                }
            }
            // the above majority_method_offhost isn't yet correct
            majority_method_offhost = 0;
            for (i=0; i<NUM_COMM_METHODS; ++i) {
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
                comm_method_to_string(majority_method_onhost), or_self);

            all_or_most = "all";
            if (!uniformity_offhost) {
                all_or_most = "most";
            }
            printf("  off-host: %s connections are %s\n", all_or_most,
                comm_method_to_string(majority_method_offhost));

            if (!uniformity_onhost || !uniformity_offhost) {
                printf("Exceptions:\n");
                for (i=0; i<nleaderranks; ++i) {
                    int is_nonconformist = 0;
                    for (k=0; k<NUM_COMM_METHODS; ++k) { method_count[k] = 0; }
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
                        snprintf(str, 1024, "  host %d:", i);
                        for (k=0; k<NUM_COMM_METHODS; ++k) {
                            if (method_count[k] > 0) {
//                              if (!first) {
//                                  strcat(str, " /");
//                              }
                                snprintf(&str[strlen(str)],
                                    1024 - strlen(str),
                                    " [%dx %s]",
                                    method_count[k],
                                    comm_method_to_string(k));
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

    free(mymethod);
    if (hpmp_myrank == 0) {
        free(method);
    }
}

/*
 *  Input is a multiline string containing \n but not ending with one.
 *  Ex:
 * "H1 aff: [......../......../......../........] Lrank 0-1\n
 *          [......../......../......../........] Lrank 2-3"
 */
static void
multiline_print(char *instr, int cols, int indentation) {
    if (!instr) { return; }
    if (instr[0] == 0) { return; }

    if (cols < 0) { cols = INT_MAX; }

    char *str = strdup(instr);
    int len = strlen(str);
    int i;
    char *p1 = str;
    char *p2;
    char save;
    int prev_line_completed = 1;
    while (p1 < str + len) {
        p2 = p1;
        if (prev_line_completed) {
            while (*p2 != 0 && *p2 != '\n' && p2-p1<cols) { ++p2; }
        } else {
            while (*p2 != 0 && *p2 != '\n' && p2-p1<cols-indentation) { ++p2; }
        }
        save = *p2;
        *p2 = 0;
        if (!prev_line_completed) {
            for (i=0; i<indentation; ++i) {
                printf(" ");
            }
        }
        printf("%s\n", p1);
        if (save == '\n') {
            prev_line_completed = 1;
        } else {
            prev_line_completed = 0;
        }

        p1 = p2 + 1;
    }
    free(str);
}

static void
host_leader_printstring(
    char *hoststring,
    int myleaderrank, int nleaderranks,
    ompi_communicator_t *leader_comm, int cols, int indentation)
{
// Gather strings at rank 0.
// The gatherv of the strings takes a few steps since we have to get
// the sizes first and allocate the receiving string.
    int len, *lens, *disps, i;
    char **allhoststrings;

    len = strlen(hoststring) + 1;
    if (myleaderrank == 0) {
        lens = malloc(nleaderranks * sizeof(int));
        disps = malloc(nleaderranks * sizeof(int));
    } else {
        lens = disps = NULL;
    }
    leader_comm->c_coll->coll_gather(
        &len, 1, MPI_INT,
        lens, 1, MPI_INT,
        0, leader_comm, leader_comm->c_coll->coll_gather_module);
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
        leader_comm->c_coll->coll_gatherv(
            hoststring, strlen(hoststring) + 1, MPI_CHAR,
            &allhoststrings[0][0], lens, disps, MPI_CHAR,
            0, leader_comm, leader_comm->c_coll->coll_gatherv_module);
    } else {
        // matching above call from rank 0, just &allhoststrings[0][0]
        // isn't legal here, and those args aren't used at non-root anyway
        leader_comm->c_coll->coll_gatherv(
            hoststring, strlen(hoststring) + 1, MPI_CHAR,
            NULL, NULL, NULL, MPI_CHAR,
            0, leader_comm, leader_comm->c_coll->coll_gatherv_module);
    }
    if (myleaderrank == 0) {
        free(lens);
        free(disps);
    }

    if (myleaderrank == 0) {
        for (i=0; i<nleaderranks; ++i) {
            multiline_print(allhoststrings[i], cols, indentation);
        }
        free(allhoststrings);
    }
}

#include "opal/mca/hwloc/base/base.h"
#define LENGTHEN(affstring, nextchar, affstring_cursz, amount) do { \
    if ((int)(nextchar + amount + 16) >= (int)(*affstring_cursz)) { \
        *affstring_cursz *= 1.25; \
        *affstring_cursz += (amount + 16); \
        *affstring = realloc(*affstring, *affstring_cursz); \
    } \
} while (0)

/*
 *  If the old --report-bindings output would have been this:
 *            [BB/../../..][../../../..]
 *            [../BB/../..][../../../..]
 *            [../../../..][BB/../../..]
 *            [../../../..][../BB/../..]
 *  this function returns a string
 *            [aa/bb/../..][cc/dd/../..]
 *
 *  This function decides how many ranks' bitmasks to process based on
 *  1. avoiding overlap, and
 *  2. only using 52 letters a-zA-Z
 *  The *position argument should come in as 0 for the first call, and
 *  it gets updated to point to the next host needing to be processed.
 *
 *  Each call to this function adds another line to an existing *affstring
 *  (realloc()ing if needed).  The second-and-later lines get a carriage
 *  return in front of them.  The last line does not end with a carriage
 *  return.
 */
static void
sprint_bitmaps(char **affstring, int *affstring_cursz,
    int indentation,
    hwloc_cpuset_t *cpus_forrank, int nranks,
    int *position)
{
    int i, r;
    int nchildren_done[32], depth, some_cores_printed_under_containing_obj = 0;
    int nextchar;
    hwloc_obj_t obj;
    hwloc_bitmap_t cpus;
    char *key = "abcdefghijklmnopqrstuvwxyzABCDEGHIJKLMNOPQRSTUVWXYZ";
    int rankA, rankB;

/* pre-computing how many ranks to use
 */
    cpus = hwloc_bitmap_alloc();
    r = *position;
    hwloc_bitmap_copy(cpus, cpus_forrank[r]);
    while (r < nranks-1) {
        if (!hwloc_bitmap_intersects(cpus, cpus_forrank[r+1]) &&
            (r - *position + 1) < (int)strlen(key))
        {
            ++r;
            hwloc_bitmap_or(cpus, cpus, cpus_forrank[r]);
        } else {
            break;
        }
    }
    hwloc_bitmap_free(cpus);
/* update position, save a rankA and rankB to note the range we'll use */
    rankA = *position;
    rankB = r;
    *position = *position + (rankB - rankA + 1);

    nextchar = strlen(*affstring);
    LENGTHEN(affstring, nextchar, affstring_cursz, indentation + 1);
    if (rankA != 0) {
        (*affstring)[nextchar++] = '\n';
        (*affstring)[nextchar] = 0;
        for (i=0; i<indentation; ++i) {
            (*affstring)[nextchar++] = ' ';
        }
        (*affstring)[nextchar] = 0;
    }

/* for bookkeeping:
 * let the top of the stack be the obj we're processing, and keep
 * nchildren_done in the stack for each obj
 */
    obj = hwloc_get_root_obj(opal_hwloc_topology);
    depth = 0;
    nchildren_done[depth] = 0;
    while (obj) {
        LENGTHEN(affstring, nextchar, affstring_cursz, 2);
        if (obj->type == HWLOC_OBJ_PACKAGE) {
            (*affstring)[nextchar++] = '[';
            (*affstring)[nextchar] = 0;
            some_cores_printed_under_containing_obj = 0;
        }
        if (obj->type == HWLOC_OBJ_CORE && some_cores_printed_under_containing_obj) {
            (*affstring)[nextchar++] = '/';
            (*affstring)[nextchar] = 0;
        }
        /* if this is a numa level it doesn't have .type NUMA, it just
         * has a .memory_arity on the side.
         * The below code prints the "<...>" for both interesting
         * and non-interesting numas.  Eg if each package is a numa
         * this might print "[<../..>]".  We could change this to only
         * print the intersting cases by adding .type==GROUP here,
         * because the GROUP .type is used when the numa level isn't a 1:1
         * match for some other obj in the tree.
         *
         * The iszero() check is for skipping numas that have no PUs under
         * them.  Without that check my PPC machines at least look like
         * "[<../..>][<../..>]<><><><><><>"
         */
#if HWLOC_API_VERSION >= 0x20000
        if (obj->memory_arity > 0 && !hwloc_bitmap_iszero(obj->memory_first_child->cpuset)) {
            (*affstring)[nextchar++] = '<';
            (*affstring)[nextchar] = 0;
            some_cores_printed_under_containing_obj = 0;
        }
#endif

        if (obj->type == HWLOC_OBJ_PU) {
            char c = '.';
            for (r=rankA; r<=rankB; ++r) {
                if (hwloc_bitmap_isset(cpus_forrank[r], obj->os_index)) {
                    c = key[r - rankA];
                    break;
                }
            }
            (*affstring)[nextchar++] = c;
            (*affstring)[nextchar] = 0;
            some_cores_printed_under_containing_obj = 1;
        }


/*
 * traverse up till we find an obj we haven't yet processed all the children for
 */
        i = nchildren_done[depth];
        while (depth >= 0 && i >= (int)obj->arity) {
            LENGTHEN(affstring, nextchar, affstring_cursz, 2);
#if HWLOC_API_VERSION >= 0x20000
            if (obj->memory_arity > 0 && !hwloc_bitmap_iszero(obj->memory_first_child->cpuset)) {
                (*affstring)[nextchar++] = '>';
                (*affstring)[nextchar] = 0;
                some_cores_printed_under_containing_obj = 0;
            }
#endif
            if (obj->type == HWLOC_OBJ_PACKAGE) {
                (*affstring)[nextchar++] = ']';
                (*affstring)[nextchar] = 0;
                some_cores_printed_under_containing_obj = 0;
            }

            obj = obj->parent;
            --depth;
            if (depth >= 0) { i = nchildren_done[depth]; }
        }
/*
 * we're either ready to go to the next child of this obj, or we've walked off
 * the top of the tree and are done
 */
        if (obj && i < (int)obj->arity) {
            obj = obj->children[i];
            ++nchildren_done[depth];
            ++depth;
            nchildren_done[depth] = 0;
        } else {
            obj = NULL;
        }
    }
}
static void
host_local_get_affstring(
    char **affstring, int indentation,
    int mylocalrank, int nlocalranks,
    ompi_communicator_t *local_comm)
{
    hwloc_cpuset_t mycpus;
    hwloc_cpuset_t *cpus_forrank;
    char *mybitmap_string;
    char **bitmap_strings;

    /* ensure we have the topology */
    if (OPAL_SUCCESS != opal_hwloc_base_get_topology()) {
        return;
    }

    mycpus = hwloc_bitmap_alloc();
    hwloc_get_cpubind(opal_hwloc_topology, mycpus, HWLOC_CPUBIND_PROCESS);
    hwloc_bitmap_list_asprintf(&mybitmap_string, mycpus);
    hwloc_bitmap_free(mycpus);

// Gather hwloc_bitmap_t strings at host-local rank 0.
    int len, *lens, *disps, i;

    len = strlen(mybitmap_string) + 1;
    if (mylocalrank == 0) {
        lens = malloc(nlocalranks * sizeof(int));
        disps = malloc(nlocalranks * sizeof(int));
    } else {
        lens = disps = NULL;
    }
    local_comm->c_coll->coll_gather(
        &len, 1, MPI_INT,
        lens, 1, MPI_INT,
        0, local_comm, local_comm->c_coll->coll_gather_module);
    if (mylocalrank == 0) {
        int tlen = 0;
        char *p;
        for (i=0; i<nlocalranks; ++i) {
            disps[i] = tlen;
            tlen += lens[i];
        }
        bitmap_strings = malloc(nlocalranks * sizeof(char*)  +  tlen);
        p = (char*) (bitmap_strings + nlocalranks);
        for (i=0; i<nlocalranks; ++i) {
            bitmap_strings[i] = p;
            p += lens[i];
        }
        local_comm->c_coll->coll_gatherv(
            mybitmap_string, len, MPI_CHAR,
            &bitmap_strings[0][0], lens, disps, MPI_CHAR,
            0, local_comm, local_comm->c_coll->coll_gatherv_module);
    } else {
        // matching above call from rank 0, just &bitmap_strings[0][0]
        // isn't legal here, and those args aren't used at non-root anyway
        local_comm->c_coll->coll_gatherv(
            mybitmap_string, len, MPI_CHAR,
            NULL, NULL, NULL, MPI_CHAR,
            0, local_comm, local_comm->c_coll->coll_gatherv_module);
    }

    free(mybitmap_string);
    if (mylocalrank != 0) { return; }

/* only the host leader runs the rest */
    free(lens);
    free(disps);

    cpus_forrank = malloc(nlocalranks * sizeof(hwloc_cpuset_t));
    for (i=0; i<nlocalranks; ++i) {
        cpus_forrank[i] = hwloc_bitmap_alloc();
        hwloc_bitmap_list_sscanf(cpus_forrank[i], bitmap_strings[i]);
    }
    free(bitmap_strings);

    int affstring_cursz = 512;
    int position = 0;
    *affstring = malloc(affstring_cursz);
    (*affstring)[0] = 0;
    int a, b;
    while (position < nlocalranks) {
        a = position;
        sprint_bitmaps(affstring, &affstring_cursz, indentation,
            cpus_forrank, nlocalranks, &position);
        b = position - 1;
        if (1 || position < nlocalranks) {
            int nextchar = strlen(*affstring);
            LENGTHEN(affstring, nextchar, &affstring_cursz, 32);
            if (a != b) {
                snprintf(&(*affstring)[strlen(*affstring)],
                    affstring_cursz - strlen(*affstring),
                    " Lranks %d-%d", a, b);
            } else {
                snprintf(&(*affstring)[strlen(*affstring)],
                    affstring_cursz - strlen(*affstring),
                    " Lrank %d", a);
            }
        }
    }

    for (i=0; i<nlocalranks; ++i) {
        hwloc_bitmap_free(cpus_forrank[i]);
    }
    free(cpus_forrank);
}

