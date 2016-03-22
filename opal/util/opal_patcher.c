/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * ******** ADD IBM COPYRIGHT HERE ******
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_patcher.h"

#include "opal/constants.h"
#include "opal/util/sys_limits.h"
#include "opal/util/output.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <assert.h>

#if OPAL_ENABLE_DLOPEN_SUPPORT && (defined(__i386__) || defined(__x86_64__) || defined(__ia64__))

static void flush_and_invalidate_cache (unsigned long  a)
{
#if defined(__i386__)
    /* does not work with AMD processors */
    __asm__ volatile("mfence;clflush %0;mfence" : :"m" (*(char*)a));
#elif defined(__x86_64__)
    __asm__ volatile("mfence;clflush %0;mfence" : :"m" (*(char*)a));
#elif defined(__ia64__)
    __asm__ volatile ("fc %0;; sync.i;; srlz.i;;" : : "r"(a) : "memory");
#endif
}

#if defined(__ia64__)

#define INSERT_BIT(d,p,v) do {                  \
        unsigned char c=*(d);                     \
        assert(((p) < 8) && ((p) >= 0));          \
        c&= ~(1<<(p));                            \
        c|= ((v)<<(p));                           \
        *(d) = c;                                 \
    } while (0)

static inline void
copy_instr_slot(unsigned char **dst, int *dst_bitpos, unsigned long instr_slot)
{
    for (int i = 40 ; i >= 0 ; --i) {
        INSERT_BIT(*dst, *dst_bitpos, (instr_slot>>i)&1);
        if (*dst_bitpos == 0) {
            ++*dst;
            *dst_bitpos = 7;
        } else {
            --*dst_bitpos;
        }
    }
}

static void make_ia64_bundle (unsigned char *dst,
                              unsigned long i2,
                              unsigned long i1,
                              unsigned long i0,
                              unsigned template)
{
/*
 * each instr is 41 bits, template is 5 bits
 *
 * generate the bit concatenation of i2:i1:i0:t, all in all 128 bits
 *
 */

    int dst_bitpos = 7;

    copy_instr_slot(&dst, &dst_bitpos, i2);
    copy_instr_slot(&dst, &dst_bitpos, i1);
    copy_instr_slot(&dst, &dst_bitpos, i0);

    assert(dst_bitpos == 4);

    for (int i = 4 ; i >= 0 ; --i) {
        INSERT_BIT(dst, dst_bitpos, (template>>i)&1);
        --dst_bitpos;
    }
}
#endif /* defined(__ia64__) */

static int patch_code (uintptr_t func_old_addr, unsigned long func_new_addr)
{
    long pg_sz = opal_getpagesize ();

    if (mprotect((void*)(func_old_addr&~(pg_sz-1)), pg_sz, PROT_EXEC|PROT_READ|PROT_WRITE)) {
        perror("mprotect failed\n");
    }

    {
#if defined(__i386__)
      *(unsigned char*)(func_old_addr+0) = 0xe9;
      *(unsigned int *)(func_old_addr+1) = (unsigned int)(func_new_addr - func_old_addr - 5);
#elif defined(__x86_64__)
      *(unsigned short*)(func_old_addr+ 0) = 0xbb49;
      *(unsigned long* )(func_old_addr+ 2) = (unsigned long) func_new_addr;
      *(unsigned char*) (func_old_addr+10) = 0x41;
      *(unsigned char*) (func_old_addr+11) = 0xff;
      *(unsigned char*) (func_old_addr+12) = 0xe3;
#elif defined(__ia64__)
      {
/*
 * target64 = IP + ((i << 59 | imm39 << 20 | imm20) << 4)
 * imm64 = i << 63 | imm41 << 22 | ic << 21 | imm5c << 16 | imm9d << 7 | imm7b
 */
         unsigned char buf[16];
         unsigned long long imm64 =  func_new_addr - func_old_addr - 16;
         register unsigned long long glb_ptr  __asm__("r1");
         unsigned long long nop =
            (0x0ULL<<37) | /* O     */
            (0x0ULL<<36) | /* i     */
            (0x0ULL<<33) | /* x3    */
            (0x1ULL<<27) | /* x6    */
            (0x0ULL<< 6) | /* imm20 */
            (0x0ULL<< 0);  /* qp    */
         unsigned long long brl =
            (0xcULL                   << 37) |
            (((imm64>>63)&0x1ULL)     << 36) |
            (0x0ULL                   << 35) |
            (0x0ULL                   << 33) |
            (((imm64>>4)&0xFFFFFULL)  << 13) |
            (0x0ULL                   <<  6) |
            (0x0ULL                   <<  0);
         unsigned long long movl =
            (0x6ULL                    << 37) |
            (((glb_ptr>>63)&0x1ULL)    << 36) |
            (((glb_ptr>> 7)&0x1FFULL)  << 27) |
            (((glb_ptr>>16)&0x1FULL)   << 22) |
            (((glb_ptr>>21)&0x1ULL)    << 21) |
            (0ULL                      << 20) |
            (((glb_ptr>> 0)&0x7FULL)   << 13) |
            (1ULL                      <<  6) |
            (0x0ULL                    <<  0);

         make_ia64_bundle(buf, movl, (glb_ptr>>22)&0x1FFFFFFFFFFULL, nop, 5);
         for (int i = 0 ; i < 16 ; ++i) {
            ((unsigned char *)func_old_addr)[16-i-1] = buf[i];
         }

         make_ia64_bundle(buf, brl, ((imm64>>24)&0x7FFFFFFFFFULL)<<2, nop, 5);
         for (int i = 0 ; i < 16 ; ++i) {
            ((unsigned char *)func_old_addr+16)[16-i-1] = buf[i];
         }
      }
#endif
    }

    flush_and_invalidate_cache(func_old_addr+ 0);
    flush_and_invalidate_cache(func_old_addr+16);

#if 1
    if (mprotect((void*)(func_old_addr&~(pg_sz-1)), pg_sz, PROT_EXEC)) {
        perror("mprotect failed\n");
    }
#endif

    return 0;
}

int opal_patch_symbol (const char *func_symbol_name, uintptr_t func_new_addr)
{
    void *sym_addr;
    char *error;
    uintptr_t func_old_addr;

    /* NTH: might want to update opal/mca/dl to handle lookups in the default
     * handle. */
    sym_addr = dlsym(RTLD_DEFAULT, func_symbol_name);
    if ( (sym_addr == NULL) && ((error = dlerror()) != NULL) )  {
        opal_output(0, "error locating symbol %s to patch. %s", func_symbol_name,
                    error);
        return -1;
    }
    func_old_addr = (unsigned long)sym_addr;

#if defined(__ia64__)
    /* On IA64 addresses are all indirect */
    func_new_addr = *(unsigned long *)func_new_addr;
    func_old_addr = *(unsigned long *)func_old_addr;
#endif

    patch_code(func_old_addr, func_new_addr);
    return OPAL_SUCCESS;
}

/* end of #if defined(__i386__) || defined(__x86_64__) || defined(__ia64__) */
// ------------------------------------------------- PPC equivalent:
#elif OPAL_ENABLE_DLOPEN_SUPPORT && defined(__PPC__)

static inline uintptr_t addr_text (uintptr_t addr) {
#if (defined(__PPC64__) || defined(__powerpc64__) || defined(__PPC__)) && _CALL_ELF != 2
    struct odp_t {
        uintptr_t text;
        uintptr_t toc;
    } *odp = (struct odp_t *) addr;
    return (odp)?odp->text:0;
#else
    return addr;
#endif
}

// modify protection of memory range
static void
ModifyMemoryProtection(uintptr_t addr, size_t length, int prot)
{
    long      page_size = opal_getpagesize ();
    uintptr_t page_addr = (addr & ~(page_size-1));
    do {
        if (mprotect((void *)page_addr, page_size, prot))
            perror("MemHook: mprotect failed");
        page_addr += page_size;
    } while (page_addr < addr + length);
}

// PowerPC instructions used in patching
// Reference: "PowerPC User Instruction Set Architecture"
unsigned int addis(unsigned int RT, unsigned int RS, unsigned int UI) {
    return (15<<26) + (RT<<21) + (RS<<16) + (UI&0xffff);
}
unsigned int ori(unsigned int RT, unsigned int RS, unsigned int UI) {
    return (24<<26) + (RS<<21) + (RT<<16) + (UI&0xffff);
}
unsigned int oris(unsigned int RT, unsigned int RS, unsigned int UI) {
    return (25<<26) + (RS<<21) + (RT<<16) + (UI&0xffff);
}
unsigned int mtspr(unsigned int SPR, unsigned int RS) {
    return (31<<26) + (RS<<21) + ((SPR&0x1f)<<16) + ((SPR>>5)<<11) + (467<<1);
}
unsigned int bcctr(unsigned int BO, unsigned int BI, unsigned int BH) {
    return (19<<26) + (BO<<21) + (BI<<16) + (BH<<11) + (528<<1);
}
unsigned int rldicr(unsigned int RT, unsigned int RS, unsigned int SH, unsigned int MB)
{
    return (30<<26) + (RS<<21) + (RT<<16) + ((SH&0x1f)<<11) + ((SH>>5)<<1)
        + ((MB&0x1f)<<6) + ((MB>>5)<<5) + (1<<2);
}

static int
PatchLoadImm(uintptr_t addr, unsigned int reg, size_t value)
{
#if defined(__PPC64__)
    *(unsigned int *) (addr + 0) = addis ( reg, 0,   (value >> 48));
    *(unsigned int *) (addr + 4) = ori   ( reg, reg, (value >> 32));
    *(unsigned int *) (addr + 8) = rldicr( reg, reg, 32, 31);
    *(unsigned int *) (addr +12) = oris  ( reg, reg, (value >> 16));
    *(unsigned int *) (addr +16) = ori   ( reg, reg, (value >>  0));
    return 20;
#else
    *(unsigned int *) (addr + 0) = addis ( reg, 0,   (value >> 16));
    *(unsigned int *) (addr + 4) = ori   ( reg, reg, (value >>  0));
    return 8;
#endif
}


static void
apply_patch(char *patch, void *addr, int length)
{
    if (length == 0) { return; }
    ModifyMemoryProtection((uintptr_t)addr, length,
        PROT_EXEC|PROT_READ|PROT_WRITE);
    memcpy((void *)addr, patch, length);
    ModifyMemoryProtection((uintptr_t)addr, length, PROT_EXEC|PROT_READ);
}

#define MAX_PATCH_SIZE 1024
int opal_patch_symbol (const char *sys_func, uintptr_t hook_addr)
{
    void *addr_to_patch_hook;
    int len_to_patch_hook = 0;
    char patch_hook[MAX_PATCH_SIZE];

    void *addr_to_patch_sys;
    int len_to_patch_sys = 0;
    char patch_sys[MAX_PATCH_SIZE];

    int offset;
#if (defined(__PPC64__) || defined(__powerpc64__) || defined(__PPC__))
    unsigned int *nop_addr;
    const unsigned int nop = 0x60000000;
#endif

    // get system function address
    uintptr_t sys_addr = add_text(dlsym(RTLD_NEXT, sys_func));
    if(sys_addr == 0) sys_addr = add_text(dlsym(RTLD_DEFAULT, sys_func));
    hook_addr = add_text(hook_addr);

// Patch for hook function:
#if (defined(__PPC64__) || defined(__powerpc64__) || defined(__PPC__))
    // locate reserved code space in hook function
    nop_addr = (unsigned int *)hook_addr;
    for (; ; nop_addr++)
        if (nop_addr[0] == nop && nop_addr[1] == nop && nop_addr[2] == nop
                && nop_addr[3] == nop && nop_addr[4] == nop)
            break;
    // generate code to restore TOC
    register unsigned long toc asm("r2");
    addr_to_patch_hook   = nop_addr;
    len_to_patch_hook = PatchLoadImm((uintptr_t)patch_hook, 2, toc);

    // save the original code
    assert(len_to_patch_hook <= MAX_PATCH_SIZE);
    //memcpy(save, (void *)addr_to_patch_hook, len_to_patch_hook); // meh
#endif

// Patch for system function:
#if (defined(__PPC64__) || defined(__powerpc64__) || defined(__PPC__)) && _CALL_ELF == 2
    sys_addr += 8;
    hook_addr += 8;
#endif /* _CALL_ELF == 2*/

    addr_to_patch_sys = (void*) sys_addr;
    // generate patch code
    // r11 is a volatile register according to PowerPC EABI
    const unsigned int gr = 11;
    offset = PatchLoadImm((uintptr_t)patch_sys, gr, hook_addr);
    *(unsigned int *) (patch_sys + offset + 0) = mtspr (9, gr);   // 9 = CTR
    *(unsigned int *) (patch_sys + offset + 4) = bcctr (20, 0, 0);// 20 = always
    len_to_patch_sys = offset + 8;

    assert(len_to_patch_sys <= MAX_PATCH_SIZE);
    //memcpy(save, (void *)addr_to_patch_sys, len_to_patch_sys);

    apply_patch(patch_hook, addr_to_patch_hook, len_to_patch_hook);
    apply_patch(patch_sys,  addr_to_patch_sys,  len_to_patch_sys);
    return OPAL_SUCCESS;
}

#else

int opal_patch_symbol (const char *sys_func, uintptr_t hook_addr) {
    return OPAL_ERR_NOT_SUPPORTED;
}

#endif
