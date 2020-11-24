/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* Always enable valgrind macros (if possible) in this file.  If these functions
 * are used, the caller is concerned about correctness, not performance. */
#define MPL_VG_ENABLED 1

/* style: allow:calloc:1 sig:0 */
/* style: allow:free:2 sig:0 */
/* style: allow:malloc:2 sig:0 */
/* style: allow:strdup:1 sig:0 */

#include "mpl.h"
#include <assert.h>

#ifdef malloc
/* Undefine these in case they were set to 'error' */
#undef malloc
#undef calloc
#undef free
#undef strdup
#undef mmap
#undef munmap
/* Some GNU implementations use __strdup for strdup */
#if defined(__strdup)
#define strdup(s) __strdup(s)
#endif
#endif

#ifdef MPL_HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#define TR_ALIGN_BYTES 8
#define TR_ALIGN_MASK  0x7
#define TR_FNAME_LEN   48

#define COOKIE_VALUE   0xf0e0d0c9
#define ALREADY_FREED  0x0f0e0d9c

enum TR_mem_type {
    TR_MALLOC_TYPE = 0,
    TR_MMAP_TYPE = 1,
};

typedef struct TRSPACE {
    enum TR_mem_type type;
    MPL_memory_class class;
    size_t size;
    int id;
    int lineno;
    int freed_lineno;
    char freed_fname[TR_FNAME_LEN];
    char fname[TR_FNAME_LEN];
    void *real_head;            /* Pointer we got from (libc) malloc */
    struct TRSPACE *volatile next, *prev;
    unsigned long cookie;       /* Cookie is always the last element
                                 * inorder to catch the off-by-one
                                 * errors */
} TRSPACE;
/* This union is used to ensure that the block passed to the user is
   aligned on a double boundary */
typedef union TrSPACE {
    TRSPACE sp;
    /* Ensure trSPACE header follows the alignment rules for all predefined types.
     * Because any internal buffer is allocated as (TrSPACE)header + (void*)buffer.*/
    MPL_mem_alignment_t alignment;
} TrSPACE;

/*
 * This package maintains some state about itself.  These globals hold
 * this information.
 */
#define TRHEAD_PRESENTINAL ((TRSPACE *)0xbacdef01)
#define TRHEAD_POSTSENTINAL ((TRSPACE *)0x10fedcba)
static int world_rank = -1;
static volatile size_t allocated = 0;
static volatile long frags = 0;
static TRSPACE *volatile TRhead[3] = { TRHEAD_PRESENTINAL, 0, TRHEAD_POSTSENTINAL };

static volatile int TRid = 0;
static volatile int TRidSet = 0;
static volatile int TRlevel = 0;
static unsigned char TRDefaultByte = 0xda;
static unsigned char TRFreedByte = 0xfc;
static int TRdebugLevel = 0;
static int TRSetBytes = 0;
#define TR_MALLOC 0x1
#define TR_FREE   0x2
#define TR_MMAP   0x4
#define TR_MUNMAP 0x8

/* Used to keep track of allocations */
static volatile size_t TRMaxMem = 0;
static volatile int TRMaxMemId = 0;
static volatile size_t TRCurOverhead = 0;
static volatile size_t TRMaxOverhead = 314572800;
/* Used to limit allocation */
static volatile size_t TRMaxMemAllow = 0;

static int TR_is_threaded = 0;

static int is_configured = 0;
static int classes_initialized = 0;

static MPL_memory_allocation_t allocation_classes[MPL_MAX_MEMORY_CLASS];

/* This list should match the enum in mpl_trmem.h */
static const char *allocation_class_strings[] = {
    "MPL_MEM_ADDRESS",
    "MPL_MEM_OBJECT",
    "MPL_MEM_COMM",
    "MPL_MEM_GROUP",
    "MPL_MEM_STRINGS",
    "MPL_MEM_RMA",
    "MPL_MEM_BUFFER",
    "MPL_MEM_SHM",
    "MPL_MEM_THREAD",
    "MPL_MEM_DYNAMIC",
    "MPL_MEM_IO",
    "MPL_MEM_GREQ",
    "MPL_MEM_DATATYPE",
    "MPL_MEM_MPIT",
    "MPL_MEM_DEBUG",
    "MPL_MEM_PM",
    "MPL_MEM_COLL",
    "MPL_MEM_USER",
    "MPL_MEM_OTHER"
};

#if MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE

static MPL_thread_mutex_t memalloc_mutex;

#define TR_THREAD_CS_ENTER                                              \
    do {                                                                \
        if (TR_is_threaded) {                                           \
            int err_;                                                   \
            MPL_thread_mutex_lock(&memalloc_mutex, &err_);              \
            if (err_)                                                   \
                MPL_error_printf("Error acquiring memalloc mutex lock\n"); \
        }                                                               \
    } while (0)

#define TR_THREAD_CS_EXIT                                               \
    do {                                                                \
        if (TR_is_threaded) {                                           \
            int err_;                                                   \
            MPL_thread_mutex_unlock(&memalloc_mutex, &err_);            \
            if (err_)                                                   \
                MPL_error_printf("Error releasing memalloc mutex lock\n"); \
        }                                                               \
    } while (0)

#else /* MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_NONE */

#define TR_THREAD_CS_ENTER
#define TR_THREAD_CS_EXIT

#endif /* MPL_THREAD_PACKAGE_NAME */

/*
 * Printing of addresses.
 *
 * This is particularly difficult because there isn't a C integer type that is
 * known in advance to be the same size as an address, so we need some
 * way to convert a pointer into the characters the represent the address in
 * hex.  We can't simply use %x or %lx since the size of an address might not
 * be an int or a long (e.g., it might be a long long).
 *
 * In order to handle this, we have our own routine to convert
 * an address to hex digits.  For thread safety, the character
 * string is allocated within the calling routine (rather than returning
 * a static string from the conversion routine).
 */

/* 8 bytes = 16 hex chars + 0x (2 chars) + the null is 19 */
#define MAX_ADDRESS_CHARS 19

static void addrToHex(void *addr, char string[MAX_ADDRESS_CHARS])
{
    int i;
    static char hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };
    /* The following construction is used to keep compilers from issuing
     * a warning message about casting a pointer to an integer type */
    intptr_t iaddr = (intptr_t) ((char *) addr - (char *) 0);

    /* Initial location */
    i = sizeof(void *) * 2;
    string[i + 2] = 0;
    while (i) {
        string[i + 1] = hex[iaddr & 0xF];
        iaddr >>= 4;
        i--;
    }
    string[0] = '0';
    string[1] = 'x';
}

static void init_classes()
{
    int i;

    for (i = 0; i < MPL_MAX_MEMORY_CLASS; i++) {
        allocation_classes[i] = (MPL_memory_allocation_t) {
        .max_allocated_mem = 0,.curr_allocated_mem = 0,.total_allocated_mem =
                0,.num_allocations = 0};
    }

    classes_initialized = 1;
}

/*+C
   MPL_trinit - Setup the space package.  Only needed for
   error messages and flags.
+*/
void MPL_trinit()
{
    char *s;

    /* FIXME: We should use generalized parameter handling here
     * to allow use of the command line as well as environment
     * variables */
    s = getenv("MPL_TRMEM_INIT");
    if (s && *s && (strcmp(s, "YES") == 0 || strcmp(s, "yes") == 0)) {
        TRSetBytes = 1;
    }
    s = getenv("MPL_TRMEM_VALIDATE");
    if (s && *s && (strcmp(s, "YES") == 0 || strcmp(s, "yes") == 0)) {
        TRdebugLevel = 1;
    }
    s = getenv("MPL_TRMEM_INITZERO");
    if (s && *s && (strcmp(s, "YES") == 0 || strcmp(s, "yes") == 0)) {
        TRDefaultByte = 0;
        TRFreedByte = 0;
    }
    s = getenv("MPL_TRMEM_TRACELEVEL");
    if (s && *s) {
        int l = atoi(s);
        TRlevel = l;
    }
    s = getenv("MPL_TRMEM_MAX_OVERHEAD");
    if (s && *s) {
        long l = atol(s);
        TRMaxOverhead = (size_t) l;
    }
}

void MPL_trconfig(int rank, int need_thread_safety)
{
    world_rank = rank;

    if (is_configured)
        return;

    /* If the upper layer asked for thread safety and there's no
     * threading package available, we need to return an error. */
#if MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_NONE
    if (need_thread_safety)
        MPL_error_printf("No thread package to provide thread-safe memory allocation\n");
#endif

#if MPL_THREAD_PACKAGE_NAME != MPL_THREAD_PACKAGE_NONE
    if (need_thread_safety) {
        int err;

        MPL_thread_mutex_create(&memalloc_mutex, &err);
        if (err) {
            MPL_error_printf("Error creating memalloc mutex\n");
        }

        TR_is_threaded = 1;
    }
#endif

    is_configured = 1;
}

/*
  Validate given alignment.
  Invoked only when memory tracing is enabled.
 */
MPL_STATIC_INLINE_PREFIX int is_valid_alignment(size_t a)
{
    /* No alignment constraints - okay */
    if (a == 0)
        return 1;

    /* Alignment should be multiple of sizeof(void *), as in posix_memalign(3) */
    if (a % sizeof(void *) != 0)
        return 0;

    /* Check if it's power of two */
    while (a > 1) {
        if (a % 2 == 1)
            return 0;   /* Don't allow non-power-of-two numbers */
        a /= 2;
    }

    return 1;
}

/*+C
    MPL_trmalloc - Malloc with tracing

Input Parameters:
+   a   - number of bytes to allocate
.   lineno - line number where used.  Use __LINE__ for this
-   fname  - file name where used.  Use __FILE__ for this

    Returns:
    double aligned pointer to requested storage, or null if not
    available.
 +*/
static void *trmalloc(size_t alignment, size_t a, MPL_memory_class class, int lineno,
                      const char fname[])
{
    TRSPACE *head;
    char *new = NULL;
    unsigned long *nend;
    size_t nsize, alloc_size, align_shift;
    int l;

    if (!is_valid_alignment(alignment))
        goto fn_exit;

    if (TRdebugLevel > 0) {
        if (MPL_trvalid2("Invalid MALLOC arena detected at line %d in %s\n", lineno, fname))
            goto fn_exit;
    }

    nsize = a;
    if (nsize & TR_ALIGN_MASK)
        nsize += (TR_ALIGN_BYTES - (nsize & TR_ALIGN_MASK));
    if ((allocated + nsize > TRMaxMemAllow) && TRMaxMemAllow) {
        /* Return a null when memory would be exhausted */
        /* This is only called when additional debugging is enabled,
         * so the fact that this does not go through the regular error
         * message system is not a problem. */
        MPL_error_printf("Exceeded allowed memory!\n");
        goto fn_exit;
    }

    /*
     * Memory layout:
     *  _______________________________________
     * | pad | TrSPACE | user space | cookie |
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * ^     |         |            |
     * real_head: pointer we got from underlying malloc (len(pad) == align_shift)
     *       ^         |            |
     *       head: our own metadata block for memory tracing
     *                 ^            |
     *                 Pointer returned to user (aligned if requested)
     *                              ^
     *                              Cookied at the tail (unsigned long)
     */

    alloc_size = alignment + sizeof(TrSPACE) + nsize + sizeof(unsigned long);

    new = (char *) malloc(alloc_size);
    if (!new)
        goto fn_exit;

    if (TRSetBytes)
        memset(new, TRDefaultByte, alloc_size);

    if (alignment > 0)
        align_shift = alignment - ((uintptr_t) new + sizeof(TrSPACE)) % alignment;
    else
        align_shift = 0;
    if (align_shift == alignment)
        align_shift = 0;        /* buffer was already aligned at desired boundary */
    /* Cast to (void*) to avoid false warnings about alignment issues */
    head = (TRSPACE *) (void *) (new + align_shift);
    head->real_head = new;      /* Record the pointer we got from malloc */
    new += sizeof(TrSPACE) + align_shift;
    assert(!alignment || (uintptr_t) new % alignment == 0);

    if (TRhead[0] != TRHEAD_PRESENTINAL || TRhead[2] != TRHEAD_POSTSENTINAL) {
        MPL_error_printf("TRhead corrupted - likely memory overwrite.\n");
        free(head->real_head);
        goto fn_exit;
    }
    if (TRhead[1]) {
        MPL_VG_MAKE_MEM_DEFINED(&TRhead[1]->prev, sizeof(TRhead[1]->prev));
        TRhead[1]->prev = head;
        MPL_VG_MAKE_MEM_NOACCESS(&TRhead[1]->prev, sizeof(TRhead[1]->prev));
    }
    head->next = TRhead[1];
    TRhead[1] = head;
    head->type = TR_MALLOC_TYPE;
    head->class = class;
    head->prev = 0;
    head->size = nsize;
    head->id = TRid;
    head->lineno = lineno;
    if ((l = (int) strlen(fname)) > TR_FNAME_LEN - 1)
        fname += (l - (TR_FNAME_LEN - 1));
    MPL_strncpy(head->fname, fname, TR_FNAME_LEN);
    head->fname[TR_FNAME_LEN - 1] = 0;
    head->cookie = COOKIE_VALUE;
    /* Cast to (void*) to avoid false warning about alignment */
    nend = (unsigned long *) (void *) (new + nsize);
    nend[0] = COOKIE_VALUE;

    if (!classes_initialized)
        init_classes();

    /* Add to the hash counters */
    allocation_classes[class].curr_allocated_mem += nsize;
    allocation_classes[class].total_allocated_mem += nsize;
    allocation_classes[class].num_allocations++;
    if (allocation_classes[class].curr_allocated_mem > allocation_classes[class].max_allocated_mem)
        allocation_classes[class].max_allocated_mem = allocation_classes[class].curr_allocated_mem;

    allocated += nsize;
    if (allocated > TRMaxMem) {
        TRMaxMem = allocated;
        TRMaxMemId = TRid;
    }
    frags++;

    if (TRlevel & TR_MALLOC) {
        /* Note that %08p (what we'd like to use) isn't accepted by
         * all compilers */
        MPL_error_printf("[%d] Allocating %ld(%ld) bytes at %8p in %s[%d]\n",
                         world_rank, (long) a, (long) nsize, new, fname, lineno);
    }

    /* Warn the user about tracing overhead if the total memory overhead for
     * tracing is larger than the threshold, TRMaxOverhead. */
    TRCurOverhead += sizeof(TrSPACE) + align_shift;
    if ((TRCurOverhead > TRMaxOverhead) && TRMaxOverhead) {
        MPL_error_printf("[%d] %.1lf MB was used for memory usage tracing!\n",
                         world_rank, (double) TRCurOverhead / 1024 / 1024);
        TRMaxOverhead = TRMaxOverhead * 2;
    }

    /* Without these macros valgrind actually catches far fewer errors when
     * using --enable-g=mem. Note that it would be nice to use
     * MPL_VG_MALLOCLIKE_BLOCK and friends, but they don't work when the
     * underlying source of the memory is malloc/free. */
    MPL_VG_MAKE_MEM_UNDEFINED(new, nsize);
    MPL_VG_MAKE_MEM_NOACCESS(head->real_head, sizeof(TrSPACE) + align_shift);
    MPL_VG_MAKE_MEM_NOACCESS(nend, sizeof(unsigned long));
  fn_exit:
    return (void *) new;
}

void *MPL_trmalloc(size_t a, MPL_memory_class class, int lineno, const char fname[])
{
    void *retval;

    TR_THREAD_CS_ENTER;
    retval = trmalloc(0, a, class, lineno, fname);
    TR_THREAD_CS_EXIT;

    return retval;
}

#ifdef MPL_DEFINE_ALIGNED_ALLOC
void *MPL_traligned_alloc(size_t alignment, size_t size, MPL_memory_class class, int lineno,
                          const char fname[])
{
    void *memptr;

    TR_THREAD_CS_ENTER;
    memptr = trmalloc(alignment, size, class, lineno, fname);
    TR_THREAD_CS_EXIT;

    return memptr;
}
#endif /* #ifdef MPL_DEFINE_ALIGNED_ALLOC */

/*+C
   MPL_trfree - Free with tracing

Input Parameters:
+  a    - pointer to a block allocated with trmalloc
.  line - line in file where called
-  file - Name of file where called
 +*/
static void trfree(void *a_ptr, int line, const char file[])
{
    TRSPACE *head;
    unsigned long *nend;
    size_t nset;
    int l;
    char hexstring[MAX_ADDRESS_CHARS];

/* Don't try to handle empty blocks */
    if (!a_ptr)
        return;

    if (TRdebugLevel > 0) {
        if (MPL_trvalid2("Invalid MALLOC arena detected by FREE at line %d in %s\n", line, file))
            return;
    }

    /* Alignment guaranteed by the way a_ptr was allocated.  Use
     * (void *) cast to suppress false warning about alignment issues */
    head = (TRSPACE *) (void *) (((char *) a_ptr) - sizeof(TrSPACE));

    /* We need to mark the memory as defined before performing our own error
     * checks or valgrind will flag the trfree function as erroneous.  The real
     * free() at the end of this function will mark the whole block as NOACCESS
     * again.  See the corresponding block in the trmalloc function for more
     * info. */
    MPL_VG_MAKE_MEM_DEFINED(head, sizeof(TrSPACE));

    if (head->cookie != COOKIE_VALUE) {
        /* Damaged header */
        /* Note that %08p (what we'd like to use) isn't accepted by
         * all compilers */
        addrToHex(a_ptr, hexstring);
        MPL_error_printf("[%d] Block at address %s is corrupted; cannot free;\n"
                         "may be block not allocated with MPL_trmalloc or MALLOC\n"
                         "called in %s at line %d\n", world_rank, hexstring, file, line);
        return;
    }
    /* Cast to (void*) to avoid false warning about alignment */
    nend = (unsigned long *) (void *) ((char *) a_ptr + head->size);
/* Check that nend is properly aligned */
    if ((sizeof(long) == 4 && ((long) nend & 0x3) != 0) ||
        (sizeof(long) == 8 && ((long) nend & 0x7) != 0)) {
        addrToHex(a_ptr, hexstring);
        MPL_error_printf
            ("[%d] Block at address %s is corrupted (invalid address or header)\n"
             "called in %s at line %d\n", world_rank, hexstring, file, line);
        return;
    }

    MPL_VG_MAKE_MEM_DEFINED(nend, sizeof(*nend));
    if (*nend != COOKIE_VALUE) {
        if (*nend == ALREADY_FREED) {
            addrToHex(a_ptr, hexstring);
            if (TRidSet) {
                MPL_error_printf
                    ("[%d] Block [id=%d(%lu)] at address %s was already freed\n", world_rank,
                     head->id, (unsigned long) head->size, hexstring);
            } else {
                MPL_error_printf("[%d] Block at address %s was already freed\n",
                                 world_rank, hexstring);
            }
            head->fname[TR_FNAME_LEN - 1] = 0;  /* Just in case */
            head->freed_fname[TR_FNAME_LEN - 1] = 0;    /* Just in case */
            MPL_error_printf("[%d] Block freed in %s[%d]\n",
                             world_rank, head->freed_fname, head->freed_lineno);
            MPL_error_printf("[%d] Block allocated at %s[%d]\n",
                             world_rank, head->fname, head->lineno);
            return;
        } else {
            /* Damaged tail */
            addrToHex(a_ptr, hexstring);
            if (TRidSet) {
                MPL_error_printf
                    ("[%d] Block [id=%d(%lu)] at address %s is corrupted (probably write past end)\n",
                     world_rank, head->id, (unsigned long) head->size, hexstring);
            } else {
                MPL_error_printf
                    ("[%d] Block at address %s is corrupted (probably write past end)\n",
                     world_rank, hexstring);
            }
            head->fname[TR_FNAME_LEN - 1] = 0;  /* Just in case */
            MPL_error_printf("[%d] Block being freed allocated in %s[%d]\n",
                             world_rank, head->fname, head->lineno);
            MPL_error_printf("[%d] Block cookie should be %lx but was %lx\n",
                             world_rank, (long) COOKIE_VALUE, *nend);
        }
    }
/* Mark the location freed */
    *nend = ALREADY_FREED;
    head->freed_lineno = line;
    if ((l = (int) strlen(file)) > TR_FNAME_LEN - 1)
        file += (l - (TR_FNAME_LEN - 1));
    MPL_strncpy(head->freed_fname, file, TR_FNAME_LEN);

    allocation_classes[head->class].curr_allocated_mem -= head->size;

    allocated -= head->size;
    frags--;
    if (head->prev) {
        MPL_VG_MAKE_MEM_DEFINED(&head->prev->next, sizeof(head->prev->next));
        head->prev->next = head->next;
        MPL_VG_MAKE_MEM_NOACCESS(&head->prev->next, sizeof(head->prev->next));
    } else {
        TRhead[1] = head->next;
    }

    if (head->next) {
        MPL_VG_MAKE_MEM_DEFINED(&head->next->prev, sizeof(head->next->prev));
        head->next->prev = head->prev;
        MPL_VG_MAKE_MEM_NOACCESS(&head->next->prev, sizeof(head->next->prev));
    }

    if (TRlevel & TR_FREE) {
        addrToHex(a_ptr, hexstring);
        MPL_error_printf("[%d] Freeing %lu bytes at %s in %s[%d]\n",
                         world_rank, (unsigned long) head->size, hexstring, file, line);
    }

    TRCurOverhead -= (uintptr_t) a_ptr - (uintptr_t) head->real_head;

    /*
     * Now, scrub the data (except possibly the first few ints) to
     * help catch access to already freed data
     */
    /* FIXME why do we skip the first few ints? [goodell@] */
    /* Answer lost in time.  Probably because in some case, the
     * first few bytes provided useful information in tracking down
     * a problem. */
    if (head->size > 2 * sizeof(int)) {
        /* Now that nset is size_t, it might be defined as unsigned,
         * so we can't compare nset - 2*sizeof(int) against zero */
        nset = head->size - 2 * sizeof(int);
        /* If an upper layer (like the handle allocation code) ever used the
         * MPL_VG_MAKE_MEM_NOACCESS macro on part/all of the data we gave
         * them then our memset will elicit "invalid write" errors from
         * valgrind.  Mark it as accessible but undefined here to prevent this. */
        MPL_VG_MAKE_MEM_UNDEFINED((char *) a_ptr + 2 * sizeof(int), nset);
        if (TRSetBytes)
            memset((char *) a_ptr + 2 * sizeof(int), TRFreedByte, nset);
    }
    free(head->real_head);
}

void MPL_trfree(void *a_ptr, int line, const char fname[])
{
    TR_THREAD_CS_ENTER;
    trfree(a_ptr, line, fname);
    TR_THREAD_CS_EXIT;
}

/*+C
   MPL_trvalid - test the allocated blocks for validity.  This can be used to
   check for memory overwrites.

Input Parameters:
.  str - string to write out only if an error is detected.

   Return value:
   The number of errors detected.

   Output Effect:
   Error messages are written to stdout.  These have the form of either

$   Block [id=%d(%d)] at address %lx is corrupted (probably write past end)
$   Block allocated in <filename>[<linenumber>]

   if the sentinal at the end of the block has been corrupted, and

$   Block at address %lx is corrupted

   if the sentinal at the begining of the block has been corrupted.

   The address is the actual address of the block.  The id is the
   value of TRID.

   No output is generated if there are no problems detected.
+*/
static int trvalid(const char str[])
{
    return MPL_trvalid2(str, -1, (const char *) 0);
}

int MPL_trvalid2(const char str[], int line, const char file[])
{
    TRSPACE *head;
    TRSPACE *next;
    char *a;
    unsigned long *nend;
    int errs = 0;
    char hexstring[MAX_ADDRESS_CHARS];

    if (TRhead[0] != TRHEAD_PRESENTINAL || TRhead[2] != TRHEAD_POSTSENTINAL) {
        MPL_error_printf("TRhead corrupted - likely memory overwrite.\n");
        errs++;
        goto fn_exit;
    }
    head = TRhead[1];
    while (head) {
        /* mark defined before accessing head contents */
        MPL_VG_MAKE_MEM_DEFINED(head, sizeof(*head));
        if (head->cookie != COOKIE_VALUE) {
            if (!errs) {
                if (line > 0)
                    MPL_error_printf(str, line, file);
                else
                    MPL_error_printf("%s\n", str);
            }
            errs++;
            addrToHex(head + 1, hexstring);
            MPL_error_printf
                ("[%d] Block at address %s is corrupted (invalid cookie in head)\n",
                 world_rank, hexstring);
            MPL_VG_MAKE_MEM_NOACCESS(head, sizeof(*head));
            /* Must stop because if head is invalid, then the data in the
             * head is probably also invalid, and using could lead to
             * SEGV or BUS  */
            goto fn_exit;
        }
        /* Get the address of the first byte of the memory, which begins
         * just after the end of the header.  We must use the full header
         * (TrSPACE) rather than the struct with the data (TRSPACE) because
         * the full header is padded to ensure correct byte alignment with
         * the data */
        a = (char *) ((TrSPACE *) head + 1);
        /* Cast to (void*) to avoid false warning about alignment */
        nend = (unsigned long *) (void *) (a + head->size);

        /* mark defined before accessing nend contents */
        MPL_VG_MAKE_MEM_DEFINED(nend, sizeof(*nend));

        if (nend[0] != COOKIE_VALUE) {
            if (!errs) {
                if (line > 0)
                    MPL_error_printf(str, line, file);
                else
                    MPL_error_printf("%s\n", str);
            }
            errs++;
            head->fname[TR_FNAME_LEN - 1] = 0;  /* Just in case */
            addrToHex(a, hexstring);
            if (TRidSet) {
                MPL_error_printf
                    ("[%d] Block [id=%d(%lu)] at address %s is corrupted (probably write past end)\n",
                     world_rank, head->id, (unsigned long) head->size, hexstring);
            } else {
                MPL_error_printf
                    ("[%d] Block at address %s is corrupted (probably write past end)\n",
                     world_rank, hexstring);
            }
            MPL_error_printf("[%d] Block allocated in %s[%d]\n",
                             world_rank, head->fname, head->lineno);
            MPL_error_printf("[%d] Block cookie should be %lx but was %lx\n",
                             world_rank, (long) COOKIE_VALUE, *nend);
        }

        /* set both regions back to NOACCESS */
        next = head->next;
        MPL_VG_MAKE_MEM_NOACCESS(head, sizeof(*head));
        MPL_VG_MAKE_MEM_NOACCESS(nend, sizeof(*nend));
        head = next;
    }
  fn_exit:
    return errs;
}

int MPL_trvalid(const char str[])
{
    int retval;
    TR_THREAD_CS_ENTER;
    retval = trvalid(str);
    TR_THREAD_CS_EXIT;
    return retval;
}

/*+C
  MPL_trdump - Dump the allocated memory blocks to a file

Input Parameters:
+  fp  - file pointer.  If fp is NULL, stderr is assumed.
-  minid - Only print allocated memory blocks whose id is at least 'minid'

 +*/
static void trdump(FILE * fp, int minid)
{
    TRSPACE *head;
#ifdef VALGRIND_MAKE_MEM_NOACCESS
    TRSPACE *old_head;
#endif
    char hexstring[MAX_ADDRESS_CHARS];

    if (fp == 0)
        fp = stderr;
    if (TRhead[0] != TRHEAD_PRESENTINAL || TRhead[2] != TRHEAD_POSTSENTINAL) {
        MPL_error_printf("TRhead corrupted - likely memory overwrite.\n");
        return;
    }
    head = TRhead[1];
    while (head) {
        /* these "rank and size" strings are supposed to be small: enough to
         * hold an mpi rank, a size, and a hexadecimal address. */
#define ADDRESS_STR_BUFLEN 256

        char address_str[ADDRESS_STR_BUFLEN];
        MPL_VG_MAKE_MEM_DEFINED(head, sizeof(*head));
        if (head->id >= minid) {
            addrToHex((char *) head + sizeof(TrSPACE), hexstring);
            address_str[ADDRESS_STR_BUFLEN - 1] = 0;
            snprintf(address_str, ADDRESS_STR_BUFLEN - 1, "[%d] %lu at [%s],", world_rank,
                     (unsigned long) head->size, hexstring);
            head->fname[TR_FNAME_LEN - 1] = 0;  /* Be extra careful */
            if (TRidSet) {
                /* For head->id >= 0, we can add code to map the id to
                 * the name of a package, rather than using a raw number */
                fprintf(fp, "%s id = %d %s[%d]\n", address_str, head->id, head->fname,
                        head->lineno);
            } else {
                fprintf(fp, "%s %s[%d]\n", address_str, head->fname, head->lineno);
            }
        }
#ifdef VALGRIND_MAKE_MEM_NOACCESS
        old_head = head;
#endif
        head = head->next;
        MPL_VG_MAKE_MEM_NOACCESS(old_head, sizeof(*old_head));
    }
/*
    msg_fprintf(fp, "# [%d] The maximum space allocated was %ld bytes [%ld]\n",
             world_rank, TRMaxMem, TRMaxMemId);
 */
}

void MPL_trdump(FILE * fp, int minid)
{
    TR_THREAD_CS_ENTER;
    trdump(fp, minid);
    TR_THREAD_CS_EXIT;
}

/*+C
    MPL_trcalloc - Calloc with tracing

Input Parameters:
.   nelem  - number of elements to allocate
.   elsize - size of each element
.   lineno - line number where used.  Use __LINE__ for this
.   fname  - file name where used.  Use __FILE__ for this

    Returns:
    Double aligned pointer to requested storage, or null if not
    available.
 +*/
static void *trcalloc(size_t nelem, size_t elsize, MPL_memory_class class, int lineno,
                      const char fname[])
{
    void *p;

    p = trmalloc(0, nelem * elsize, class, lineno, fname);
    if (p) {
        memset(p, 0, nelem * elsize);
    }
    return p;
}

void *MPL_trcalloc(size_t nelem, size_t elsize, MPL_memory_class class, int lineno,
                   const char fname[])
{
    void *retval;
    TR_THREAD_CS_ENTER;
    retval = trcalloc(nelem, elsize, class, lineno, fname);
    TR_THREAD_CS_EXIT;
    return retval;
}

/*+C
    MPL_trrealloc - Realloc with tracing

Input Parameters:
.   p      - pointer to old storage
.   size   - number of bytes to allocate
.   lineno - line number where used.  Use __LINE__ for this
.   fname  - file name where used.  Use __FILE__ for this

    Returns:
    Double aligned pointer to requested storage, or null if not
    available.  This implementation ALWAYS allocates new space and copies
    the contents into the new space.
 +*/
static void *trrealloc(void *p, size_t size, MPL_memory_class class, int lineno, const char fname[])
{
    void *pnew;
    size_t nsize;
    TRSPACE *head = 0;
    char hexstring[MAX_ADDRESS_CHARS];

    /* We should really use the size of the old block... */
    if (p) {
        head = (TRSPACE *) (void *) ((char *) p - sizeof(TrSPACE));
        MPL_VG_MAKE_MEM_DEFINED(head, sizeof(*head));   /* mark defined before accessing contents */
        if (head->cookie != COOKIE_VALUE) {
            /* Damaged header */
            addrToHex(p, hexstring);
            MPL_error_printf("[%d] Block at address %s is corrupted; cannot realloc;\n"
                             "may be block not allocated with MPL_trmalloc or MALLOC\n",
                             world_rank, hexstring);
            return 0;
        }
    }

    /* Per the POSIX Standard, realloc() with zero size has two possible
     * results.  In both cases the given pointer (p) is freed, and the function
     * will either return NULL or a unique value that can safely be passed to
     * free().  We return NULL here because that is more likely to catch
     * programming errors at higher levels. */
    if (!size) {
        trfree(p, lineno, fname);
        return NULL;
    }

    pnew = trmalloc(0, size, class, lineno, fname);

    if (p && pnew) {
        nsize = size;
        if (head->size < nsize)
            nsize = head->size;
        memcpy(pnew, p, nsize);
        trfree(p, lineno, fname);
    }

    /* Re-mark the head as NOACCESS before returning. */
    /* FIXME: Note head is no longer valid after MPL_trfree above */
    if (head) {
        MPL_VG_MAKE_MEM_NOACCESS(head, sizeof(*head));
    }

    /* If the MPL_trmalloc failed above pnew will be NULL, just like a
     * regular realloc failure. */
    return pnew;
}

void *MPL_trrealloc(void *p, size_t size, MPL_memory_class class, int lineno, const char fname[])
{
    void *retval;
    TR_THREAD_CS_ENTER;
    retval = trrealloc(p, size, class, lineno, fname);
    TR_THREAD_CS_EXIT;
    return retval;
}

static void *trmmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset,
                    MPL_memory_class class, int lineno, const char fname[])
{
    char *new = NULL;

    new = (char *) mmap(addr, length, prot, flags, fd, offset);
    if (new == MAP_FAILED)
        goto fn_exit;

    if (TRlevel & TR_MMAP) {
        MPL_error_printf("[%d] Mmapping %ld(%ld) bytes at %p in %s[%d]\n",
                         world_rank, (long) length, (long) length, new, fname, lineno);
    }

    if (!classes_initialized)
        init_classes();

    allocation_classes[class].curr_allocated_mem += length;
    allocation_classes[class].total_allocated_mem += length;
    allocation_classes[class].num_allocations++;
    if (allocation_classes[class].max_allocated_mem < allocation_classes[class].curr_allocated_mem)
        allocation_classes[class].max_allocated_mem = allocation_classes[class].curr_allocated_mem;

  fn_exit:
    return (void *) new;
}

void *MPL_trmmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset,
                 MPL_memory_class class, int lineno, const char fname[])
{
    void *retval;
    TR_THREAD_CS_ENTER;
    retval = trmmap(addr, length, prot, flags, fd, offset, class, lineno, fname);
    TR_THREAD_CS_EXIT;
    return retval;
}

static void trmunmap(void *addr, size_t length, MPL_memory_class class, int lineno,
                     const char fname[])
{
    allocation_classes[class].curr_allocated_mem -= length;

    munmap(addr, length);
}

void MPL_trmunmap(void *addr, size_t length, MPL_memory_class class, int lineno, const char fname[])
{
    TR_THREAD_CS_ENTER;
    trmunmap(addr, length, class, lineno, fname);
    TR_THREAD_CS_EXIT;
}

/*+C
    MPL_trstrdup - Strdup with tracing

Input Parameters:
.   str    - string to duplicate
.   lineno - line number where used.  Use __LINE__ for this
.   fname  - file name where used.  Use __FILE__ for this

    Returns:
    Pointer to copy of the input string.
 +*/
static void *trstrdup(const char *str, int lineno, const char fname[])
{
    void *p;
    size_t len = strlen(str) + 1;

    p = trmalloc(0, len, MPL_MEM_STRINGS, lineno, fname);
    if (p) {
        memcpy(p, str, len);
    }
    return p;
}

void *MPL_trstrdup(const char *str, int lineno, const char fname[])
{
    void *retval;
    TR_THREAD_CS_ENTER;
    retval = trstrdup(str, lineno, fname);
    TR_THREAD_CS_EXIT;
    return retval;
}

void MPL_trcategorydump(FILE * fp)
{
    int i;

    fprintf(fp, "%16s\t%16s\t%16s\t%16s\t%16s\n",
            "CLASS", "MAX ALLOCATED", "CURR ALLOCATED", "TOT ALLOCATIED", "NUM ALLOCATIONS");
    for (i = 0; i < MPL_MAX_MEMORY_CLASS; i++) {
        fprintf(fp, "%16s\t%16ld\t%16ld\t%16ld\t%16ld\n",
                allocation_class_strings[i],
                allocation_classes[i].max_allocated_mem,
                allocation_classes[i].curr_allocated_mem,
                allocation_classes[i].total_allocated_mem, allocation_classes[i].num_allocations);
    }
}
