/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "patcher_overwrite.h"

#include "opal/mca/patcher/base/base.h"

#include "opal/constants.h"
#include "opal/util/sys_limits.h"
#include "opal/util/output.h"
#include "opal/prefetch.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <assert.h>

#if (OPAL_ASSEMBLY_ARCH == OPAL_IA32) || (OPAL_ASSEMBLY_ARCH == OPAL_IA64) || (OPAL_ASSEMBLY_ARCH == OPAL_X86_64)

#if (OPAL_ASSEMBLY_ARCH == OPAL_IA64)

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

static int mca_patcher_overwrite_apply_patch (mca_patcher_base_patch_t *patch)
{
    uintptr_t func_new_addr = patch->patch_value;

    {
#if (OPAL_ASSEMBLY_ARCH == OPAL_IA32)
        patch->patch_data_size = 5;
        *(unsigned char *)(patch->patch_data+0) = 0xe9;
        *(unsigned int *) (patch->patch_data+1) = (unsigned int)(func_new_addr - patch->patch_orig - 5);
#elif (OPAL_ASSEMBLY_ARCH == OPAL_X86_64)
        patch->patch_data_size = 13;
        *(unsigned short*)(patch->patch_data + 0) = 0xbb49;
        *(unsigned long* )(patch->patch_data + 2) = (unsigned long) func_new_addr;
        *(unsigned char*) (patch->patch_data +10) = 0x41;
        *(unsigned char*) (patch->patch_data +11) = 0xff;
        *(unsigned char*) (patch->patch_data +12) = 0xe3;
#elif (OPAL_ASSEMBLY_ARCH == OPAL_IA64)
      {
/*
 * target64 = IP + ((i << 59 | imm39 << 20 | imm20) << 4)
 * imm64 = i << 63 | imm41 << 22 | ic << 21 | imm5c << 16 | imm9d << 7 | imm7b
 */
         unsigned char buf[16];
         unsigned long long imm64 =  func_new_addr - patch->patch_orig - 16;
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

         patch->patch_data_size = 32;

         make_ia64_bundle(buf, movl, (glb_ptr>>22)&0x1FFFFFFFFFFULL, nop, 5);
         for (int i = 0 ; i < 16 ; ++i) {
             patch->patch_data[16-i-1] = buf[i];
         }

         make_ia64_bundle(buf, brl, ((imm64>>24)&0x7FFFFFFFFFULL)<<2, nop, 5);
         for (int i = 0 ; i < 16 ; ++i) {
             patch->patch_data[32-i-1] = buf[i];
         }
      }
#endif
    }

    mca_base_patcher_patch_apply_binary (patch);

    return OPAL_SUCCESS;
}

/* end of #if defined(__i386__) || defined(__x86_64__) || defined(__ia64__) */
// ------------------------------------------------- PPC equivalent:
#elif (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC32) || (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC64)

// PowerPC instructions used in patching
// Reference: "PowerPC User Instruction Set Architecture"
static unsigned int addis(unsigned int RT, unsigned int RS, unsigned int UI) {
    return (15<<26) + (RT<<21) + (RS<<16) + (UI&0xffff);
}
static unsigned int ori(unsigned int RT, unsigned int RS, unsigned int UI) {
    return (24<<26) + (RS<<21) + (RT<<16) + (UI&0xffff);
}
static unsigned int oris(unsigned int RT, unsigned int RS, unsigned int UI) {
    return (25<<26) + (RS<<21) + (RT<<16) + (UI&0xffff);
}
static unsigned int mtspr(unsigned int SPR, unsigned int RS) {
    return (31<<26) + (RS<<21) + ((SPR&0x1f)<<16) + ((SPR>>5)<<11) + (467<<1);
}
static unsigned int bcctr(unsigned int BO, unsigned int BI, unsigned int BH) {
    return (19<<26) + (BO<<21) + (BI<<16) + (BH<<11) + (528<<1);
}
static unsigned int rldicr(unsigned int RT, unsigned int RS, unsigned int SH, unsigned int MB)
{
    return (30<<26) + (RS<<21) + (RT<<16) + ((SH&0x1f)<<11) + ((SH>>5)<<1)
        + ((MB&0x1f)<<6) + ((MB>>5)<<5) + (1<<2);
}

static int
PatchLoadImm(uintptr_t addr, unsigned int reg, size_t value)
{
#if (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC64)
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


static int mca_patcher_overwrite_apply_patch (mca_patcher_base_patch_t *patch)
{
    uintptr_t sys_addr, hook_addr;
    int offset, rc;

    // get system function address
    sys_addr = mca_patcher_base_addr_text(patch->patch_orig);
    hook_addr = mca_patcher_base_addr_text(patch->patch_value);

// Patch for hook function:
#if (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC64)
    rc = mca_patcher_base_patch_hook (&mca_patcher_overwrite_module, hook_addr);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

#if defined(_CALL_ELF) && (_CALL_ELF == 2)
    sys_addr += 8;
    hook_addr += 8;
#endif /* _CALL_ELF == 2*/
#endif

    // Patch for system function:
    // generate patch code
    // r11 is a volatile register according to PowerPC EABI
    const unsigned int gr = 11;
    offset = PatchLoadImm ((uintptr_t) patch->patch_data, gr, hook_addr);
    *(unsigned int *) (patch->patch_data + offset + 0) = mtspr (9, gr);   // 9 = CTR
    *(unsigned int *) (patch->patch_data + offset + 4) = bcctr (20, 0, 0);// 20 = always
    patch->patch_data_size = offset + 8;
    patch->patch_orig = sys_addr;

    mca_base_patcher_patch_apply_binary (patch);

    return OPAL_SUCCESS;
}

#elif defined(__aarch64__)

/**
 * @brief Generate a mov immediate instruction
 *
 * @param[in] reg   register number (0-31)
 * @param[in] shift shift amount (0-3) * 16-bits
 * @param[in] value immediate value
 */
static uint32_t mov (unsigned int reg, uint16_t shift, uint16_t value)
{
    return (0x1a5 << 23) + ((uint32_t) shift << 21) + ((uint32_t) value << 5) + reg;
}

/**
 * @brief Generate a mov immediate with keep instruction
 *
 * @param[in] reg   register number (0-31)
 * @param[in] shift shift amount (0-3) * 16-bits
 * @param[in] value immediate value
 */
static uint32_t movk (unsigned int reg, uint16_t shift, uint16_t value)
{
    return (0x1e5 << 23) + ((uint32_t) shift << 21) + ((uint32_t) value << 5) + reg;
}

static uint32_t br (unsigned int reg)
{
    return (0xd61f << 16) + (reg << 5);
}

static int
PatchLoadImm(uintptr_t addr, unsigned int reg, uint64_t value)
{
    *(uint32_t *) (addr +  0) = mov(reg, 3, value >> 48);
    *(uint32_t *) (addr +  4) = movk(reg, 2, value >> 32);
    *(uint32_t *) (addr +  8) = movk(reg, 1, value >> 16);
    *(uint32_t *) (addr + 12) = movk(reg, 0, value);
    return 16;
}

static int mca_patcher_overwrite_apply_patch (mca_patcher_base_patch_t *patch)
{
    uintptr_t sys_addr, hook_addr;
    int offset, rc;

    /* get system function address */
    sys_addr = mca_patcher_base_addr_text(patch->patch_orig);
    hook_addr = mca_patcher_base_addr_text(patch->patch_value);

    /* Patch for hook function: */
    rc = mca_patcher_base_patch_hook (&mca_patcher_overwrite_module, hook_addr);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* Patch for system function:
     * generate patch code
     * r15 is the highest numbered temporary register. I am assuming this one is safe
     * to use. */
    const unsigned int gr = 15;
    offset = PatchLoadImm ((uintptr_t) patch->patch_data, gr, hook_addr);
    *(uint32_t *) (patch->patch_data + offset) = br(gr);
    patch->patch_data_size = offset + 4;
    patch->patch_orig = sys_addr;

    mca_base_patcher_patch_apply_binary (patch);

    return OPAL_SUCCESS;
}

#endif

static void mca_patcher_unapply_all_patches(void)
{
    mca_patcher_base_patch_t *patch, *patch_next;

    opal_mutex_lock (&mca_patcher_overwrite_module.patch_list_mutex);
    
    OPAL_LIST_FOREACH_SAFE(patch, patch_next, &opal_patcher->patch_list, mca_patcher_base_patch_t) {
        patch->patch_restore (patch);
        opal_list_remove_item(&opal_patcher->patch_list, patch);
        OBJ_RELEASE(patch);
    }

    opal_mutex_unlock (&mca_patcher_overwrite_module.patch_list_mutex);
}

/*
 * The logic in this function for each platform is based on code from
 * mca_patcher_overwrite_apply_patch(). There are 2 general approaches:
 * 1: Directly check constant instructions (ignoring addresses as parameters)
 * 2: Generate a bit mask by passing min and max values to underlying helper
 *    functions and negate the XOR'ed results. These results can be used to
 *    mask off transient values (like addresess) and non-instruction values
 *    (like register contents). Once the masks are applied, the results are
 *    compared against the min values directly to check for equality. If equal,
 *    we consider the memory to be previously patched.
 */
static bool mca_patcher_is_function_patched(unsigned char *target)
{

#if (OPAL_ASSEMBLY_ARCH == OPAL_IA32)
    return (*(unsigned char *)target == 0xe9);
#elif (OPAL_ASSEMBLY_ARCH == OPAL_X86_64)
	return (
		(*(unsigned short*)(target + 0) == 0xbb49) &&
		(*(unsigned char* )(target +10) == 0x41  ) &&
		(*(unsigned char* )(target +11) == 0xff  ) &&
		(*(unsigned char* )(target +12) == 0xe3  )
	);
#elif (OPAL_ASSEMBLY_ARCH == OPAL_IA64)
    unsigned char buf_zeros[16], buf_ones[16];
    unsigned char patch_mask[32], instr_mask[32];
    bool all_masked;
    int i;
    /*
     * Set the imm64 and glb_ptr variables to 0 in this testing case, since we can't compare
     * against specific values (we'll just have to use this as a mask value)
     */
    unsigned long long imm64_zeros = 0;
    unsigned long long imm64_ones = 0xFFFFFFFFFFFFFFFF;
    unsigned long long glb_ptr_zeros = 0;
    unsigned long long glb_ptr_ones = 0xFFFFFFFFFFFFFFFF;
    unsigned long long nop =
        (0x0ULL<<37) | /* O     */
        (0x0ULL<<36) | /* i     */
        (0x0ULL<<33) | /* x3    */
        (0x1ULL<<27) | /* x6    */
        (0x0ULL<< 6) | /* imm20 */
        (0x0ULL<< 0);  /* qp    */
    unsigned long long brl_zeros =
        (0xcULL                         << 37) |
        (((imm64_zeros>>63)&0x1ULL)     << 36) |
        (0x0ULL                         << 35) |
        (0x0ULL                         << 33) |
        (((imm64_zeros>>4)&0xFFFFFULL)  << 13) |
        (0x0ULL                         <<  6) |
        (0x0ULL                         <<  0);
    unsigned long long brl_ones =
        (0xcULL                        << 37) |
        (((imm64_ones>>63)&0x1ULL)     << 36) |
        (0x0ULL                        << 35) |
        (0x0ULL                        << 33) |
        (((imm64_ones>>4)&0xFFFFFULL)  << 13) |
        (0x0ULL                        <<  6) |
        (0x0ULL                        <<  0);
    unsigned long long movl_zeros =
        (0x6ULL                          << 37) |
        (((glb_ptr_zeros>>63)&0x1ULL)    << 36) |
        (((glb_ptr_zeros>> 7)&0x1FFULL)  << 27) |
        (((glb_ptr_zeros>>16)&0x1FULL)   << 22) |
        (((glb_ptr_zeros>>21)&0x1ULL)    << 21) |
        (0ULL                            << 20) |
        (((glb_ptr_zeros>> 0)&0x7FULL)   << 13) |
        (1ULL                            <<  6) |
        (0x0ULL                          <<  0);
    unsigned long long movl_ones =
        (0x6ULL                         << 37) |
        (((glb_ptr_ones>>63)&0x1ULL)    << 36) |
        (((glb_ptr_ones>> 7)&0x1FFULL)  << 27) |
        (((glb_ptr_ones>>16)&0x1FULL)   << 22) |
        (((glb_ptr_ones>>21)&0x1ULL)    << 21) |
        (0ULL                           << 20) |
        (((glb_ptr_ones>> 0)&0x7FULL)   << 13) |
        (1ULL                           <<  6) |
        (0x0ULL                         <<  0);
    make_ia64_bundle(buf_zeros, movl_zeros,
        (glb_ptr_zeros >> 22)&0x1FFFFFFFFFFULL, nop, 5);
    make_ia64_bundle(buf_ones,  movl_ones,
        (glb_ptr_ones  >> 22)&0x1FFFFFFFFFFULL, nop, 5);
    for (int i = 0 ; i < 16 ; ++i) {
        patch_mask[16-i-1] = ~(buf_zeros[i] ^ buf_ones[i]);
        instr_mask[16-i-1] = buf_zeros[i];
    }

    make_ia64_bundle(buf_zeros, brl_zeros,
        ((imm64_zeros >> 24)&0x7FFFFFFFFFULL)<<2, nop, 5);
    make_ia64_bundle(buf_ones,  brl_ones,
        ((imm64_ones  >> 24)&0x7FFFFFFFFFULL)<<2, nop, 5);
    for (int i = 0 ; i < 16 ; ++i) {
        patch_mask[32-i-1] = ~(buf_zeros[i] ^ buf_ones[i]);
        instr_mask[32-i-1] = buf_zeros[i];
    }

    i=0;
    all_masked=true;
    while (all_masked && i<32) {
        if ((target[i] & patch_mask[i]) != instr_mask[i])
            all_masked=false;
        i++;
    }
    return all_masked;
#elif (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC32 ) || (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC64)
    const unsigned int gr_max = 0xF; //11 is used in our patching code, but is the max 4 or 5 bits?
    const unsigned int addr_max = 0xFFFF;
    unsigned int  addis_base = addis ( 0, 0, 0);
    unsigned int  addis_mask = ~(addis_base  ^  addis ( gr_max,      0,  addr_max));
    unsigned int    ori_base =   ori ( 0, 0, 0);
    unsigned int    ori_mask = ~(  ori_base  ^    ori ( gr_max, gr_max,  addr_max));
    unsigned int  mtspr_base = mtspr ( 9, 0);    // 9 = CTR
    unsigned int  mtspr_mask = ~(mtspr_base  ^  mtspr ( 9,      gr_max));
    unsigned int  bcctr_base = bcctr (20, 0, 0); // 20 = always
    unsigned int  bcctr_mask = ~(bcctr_base  ^  bcctr (20,           0,         0));
#if (OPAL_ASSEMBLY_ARCH == OPAL_POWERPC32)

    return (
        ((*(unsigned int *) (target + 0 )) &  addis_mask) ==  addis_base &&
        ((*(unsigned int *) (target + 4 )) &    ori_mask) ==    ori_base &&
        ((*(unsigned int *) (target + 8 )) &  mtspr_mask) ==  mtspr_base &&
        ((*(unsigned int *) (target + 12)) &  bcctr_mask) ==  bcctr_base
    );
#else
    unsigned int rldicr_base = rldicr ( 0, 0, 32, 31);
    unsigned int rldicr_mask = ~(rldicr_base ^ rldicr ( gr_max, gr_max, 32, 31));
    unsigned int   oris_base = oris ( 0, 0, 0);
    unsigned int   oris_mask = ~(oris_base ^     oris ( gr_max, gr_max,  addr_max));

    return (
        ((*(unsigned int *) (target + 0 )) &  addis_mask) ==  addis_base &&
        ((*(unsigned int *) (target + 4 )) &    ori_mask) ==    ori_base &&
        ((*(unsigned int *) (target + 8 )) & rldicr_mask) == rldicr_base &&
        ((*(unsigned int *) (target + 12)) &   oris_mask) ==   oris_base &&
        ((*(unsigned int *) (target + 16)) &    ori_mask) ==    ori_base &&
        ((*(unsigned int *) (target + 20)) &  mtspr_mask) ==  mtspr_base &&
        ((*(unsigned int *) (target + 24)) &  bcctr_mask) ==  bcctr_base
    );
#endif
#elif defined(__aarch64__)
    uint32_t mov_mask=~((0xFFFF << 5) | 0x1F);
    uint32_t br_mask=~(0x1F << 5);

    return (
        ((*(uint32_t *) (target +  0)) & mov_mask) == mov(0, 3, 0) &&
        ((*(uint32_t *) (target +  4)) & mov_mask) == movk(0, 2, 0) &&
        ((*(uint32_t *) (target +  8)) & mov_mask) == movk(0, 1, 0) &&
        ((*(uint32_t *) (target + 12)) & mov_mask) == movk(0, 0, 0) &&
        ((*(uint32_t *) (target + 16)) & br_mask) == br(0)
    );
#endif
}

static int mca_patcher_overwrite_patch_address (uintptr_t sys_addr, uintptr_t hook_addr)
{
    mca_patcher_base_patch_t *patch;
    int rc;

    patch = OBJ_NEW(mca_patcher_base_patch_t);
    if (OPAL_UNLIKELY(NULL == patch)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    patch->patch_orig = sys_addr;
    patch->patch_value = hook_addr;

    opal_mutex_lock (&mca_patcher_overwrite_module.patch_list_mutex);
    do {
        rc = mca_patcher_overwrite_apply_patch (patch);
        if (OPAL_SUCCESS != rc) {
            break;
        }

        opal_list_append (&mca_patcher_overwrite_module.patch_list, &patch->super);
    } while (0);

    opal_mutex_unlock (&mca_patcher_overwrite_module.patch_list_mutex);

    return OPAL_SUCCESS;
}

static int mca_patcher_overwrite_patch_symbol (const char *func_symbol_name, uintptr_t func_new_addr,
                                               uintptr_t *func_old_addr)
{
    void *sym_addr;
    char *error;
    uintptr_t old_addr;

    /* NTH: might want to update opal/mca/dl to handle lookups in the default
     * handle. */
    sym_addr = dlsym (RTLD_NEXT, func_symbol_name);
    if (NULL == sym_addr) {
        sym_addr = dlsym(RTLD_DEFAULT, func_symbol_name);
        if ( (sym_addr == NULL) && ((error = dlerror()) != NULL) )  {
            opal_output(0, "error locating symbol %s to patch. %s", func_symbol_name,
                        error);
            return OPAL_ERR_NOT_FOUND;
        }
    }

    old_addr = (unsigned long)sym_addr;

#if (OPAL_ASSEMBLY_ARCH == OPAL_IA64)
    /* On IA64 addresses are all indirect */
    func_new_addr = *(unsigned long *)func_new_addr;
    old_addr = *(unsigned long *) old_addr;
#endif

    if (func_old_addr) {
        /* we will be overwritting part of the original function. do not return
         * its address */
        *func_old_addr = 0;
    }

    if (mca_patcher_is_function_patched ((unsigned char*)old_addr)) {
        opal_output(0, "function %s is already patched; stopping further patching\n",
                func_symbol_name);
        return OPAL_ERR_RESOURCE_BUSY;
	} else {
        return mca_patcher_overwrite_patch_address (old_addr, func_new_addr);
	}
}

mca_patcher_base_module_t mca_patcher_overwrite_module = {
    .patch_symbol = mca_patcher_overwrite_patch_symbol,
    .patch_address = mca_patcher_overwrite_patch_address,
    .patch_restore_all = mca_patcher_unapply_all_patches,
};
