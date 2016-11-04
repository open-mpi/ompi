/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2015.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/*
 * Copied from OpenUCX
 */

#include "patcher_linux.h"

#include "opal/mca/patcher/base/base.h"

#include "opal/constants.h"
#include "opal/util/sys_limits.h"
#include "opal/util/output.h"
#include "opal/prefetch.h"

#if defined(HAVE_SYS_AUXV_H)
#include <sys/auxv.h>
#endif

#include <elf.h>

#include <sys/mman.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <link.h>

static void *mca_patcher_linux_dlopen(const char *filename, int flag);

typedef struct mca_patcher_linux_dl_iter_context {
    mca_patcher_linux_patch_t *patch;
    bool remove;
    int status;
} mca_patcher_linux_dl_iter_context_t;

OBJ_CLASS_INSTANCE(mca_patcher_linux_patch_got_t, opal_list_item_t, NULL, NULL);

static void mca_patcher_linux_patch_construct (mca_patcher_linux_patch_t *patch)
{
    OBJ_CONSTRUCT(&patch->patch_got_list, opal_list_t);
}

static void mca_patcher_linux_patch_destruct (mca_patcher_linux_patch_t *patch)
{
    OPAL_LIST_DESTRUCT(&patch->patch_got_list);
}

OBJ_CLASS_INSTANCE(mca_patcher_linux_patch_t, mca_patcher_base_patch_t, mca_patcher_linux_patch_construct,
                   mca_patcher_linux_patch_destruct);

/* List of patches to be applied to additional libraries */
static void *(*orig_dlopen) (const char *, int);

static const ElfW(Phdr) *
mca_patcher_linux_get_phdr_dynamic(const ElfW(Phdr) *phdr, uint16_t phnum, int phent)
{
    for (uint16_t i = 0 ; i < phnum ; ++i, phdr = (ElfW(Phdr)*)((intptr_t) phdr + phent)) {
        if (phdr->p_type == PT_DYNAMIC) {
            return phdr;
        }
    }

    return NULL;
}

static void *mca_patcher_linux_get_dynentry(ElfW(Addr) base, const ElfW(Phdr) *pdyn, ElfW(Sxword) type)
{
    for (ElfW(Dyn) *dyn = (ElfW(Dyn)*)(base + pdyn->p_vaddr); dyn->d_tag; ++dyn) {
        if (dyn->d_tag == type) {
            return (void *) (uintptr_t) dyn->d_un.d_val;
        }
    }

    return NULL;
}

static void * mca_patcher_linux_get_got_entry (ElfW(Addr) base, const ElfW(Phdr) *phdr, int16_t phnum,
                                               int phent, const char *symbol)
{
    const ElfW(Phdr) *dphdr;
    void *jmprel, *strtab;
    ElfW(Sym)  *symtab;
    size_t pltrelsz;

    dphdr = mca_patcher_linux_get_phdr_dynamic (phdr, phnum, phent);

    jmprel = mca_patcher_linux_get_dynentry (base, dphdr, DT_JMPREL);
    symtab = (ElfW(Sym) *) mca_patcher_linux_get_dynentry (base, dphdr, DT_SYMTAB);
    strtab = mca_patcher_linux_get_dynentry (base, dphdr, DT_STRTAB);
    pltrelsz = (size_t) (uintptr_t) mca_patcher_linux_get_dynentry (base, dphdr, DT_PLTRELSZ);

    for (ElfW(Rela) *reloc = jmprel; (intptr_t) reloc < (intptr_t) jmprel + pltrelsz; ++reloc) {
#if SIZEOF_VOID_P == 8
        uint32_t relsymidx = ELF64_R_SYM(reloc->r_info);
#else
        uint32_t relsymidx = ELF32_R_SYM(reloc->r_info);
#endif
        char *elf_sym = (char *) strtab + symtab[relsymidx].st_name;

        if (0 == strcmp (symbol, elf_sym)) {
            return (void *)(base + reloc->r_offset);
        }
    }

    return NULL;
}

static int mca_patcher_linux_get_aux_phent (void)
{
#if !defined(HAVE_SYS_AUXV_H)
#define MCA_PATCHER_LINUX_AUXV_BUF_LEN 16
    static const char *proc_auxv_filename = "/proc/self/auxv";
    static int phent = 0;
    ElfW(auxv_t) buffer[MCA_PATCHER_LINUX_AUXV_BUF_LEN];
    unsigned count;
    ssize_t nread;
    int fd;

    /* Can avoid lock here - worst case we'll read the file more than once */
    if (phent == 0) {
        fd = open(proc_auxv_filename, O_RDONLY);
        if (fd < 0) {
            opal_output_verbose (MCA_BASE_VERBOSE_ERROR, opal_patcher_base_framework.framework_output,
                                 "failed to open '%s' for reading: %s", proc_auxv_filename,
                                 strerror (errno));
            return OPAL_ERROR;
        }

        /* Use small buffer on the stack, avoid using malloc() */
        do {
            nread = read(fd, buffer, sizeof(buffer));
            if (nread < 0) {
                opal_output_verbose (MCA_BASE_VERBOSE_ERROR, opal_patcher_base_framework.framework_output,
                                     "failed to read %" PRIsize_t " bytes from %s (ret=%ld): %s", sizeof (buffer),
                                      proc_auxv_filename, nread, strerror (errno));
                break;
            }

            count = nread / sizeof(buffer[0]);
            for (unsigned i = 0 ; i < count && AT_NULL != buffer[i].a_type ; ++i) {
                if (AT_PHENT == buffer[i].a_type) {
                    phent = buffer[i].a_un.a_val;
                    opal_output_verbose (MCA_BASE_VERBOSE_ERROR, opal_patcher_base_framework.framework_output,
                                         "read phent from %s: %d", proc_auxv_filename, phent);
                    break;
                }
            }
        } while ((count > 0) && (phent == 0));

        close(fd);
    }

    return phent;
#else
    return getauxval (AT_PHENT);
#endif
}

static int
mca_patcher_linux_modify_got (ElfW(Addr) base, const ElfW(Phdr) *phdr, const char *phname,
                              int16_t phnum, int phent, mca_patcher_linux_dl_iter_context_t *ctx)
{
    long page_size = opal_getpagesize ();
    void **entry, *page;
    int ret;

    entry = mca_patcher_linux_get_got_entry (base, phdr, phnum, phent, ctx->patch->super.patch_symbol);
    if (entry == NULL) {
        return OPAL_SUCCESS;
    }

    page = (void *)((intptr_t)entry & ~(page_size - 1));
    ret = mprotect(page, page_size, PROT_READ|PROT_WRITE);
    if (ret < 0) {
        opal_output_verbose (MCA_BASE_VERBOSE_ERROR, opal_patcher_base_framework.framework_output,
                             "failed to modify GOT page %p to rw: %s", page, strerror (errno));
        return OPAL_ERR_NOT_SUPPORTED;
    }

    if (!ctx->remove) {
        if (*entry != (void *) ctx->patch->super.patch_value) {
            mca_patcher_linux_patch_got_t *patch_got = OBJ_NEW(mca_patcher_linux_patch_got_t);
            if (NULL == patch_got) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            opal_output_verbose (MCA_BASE_VERBOSE_TRACE, opal_patcher_base_framework.framework_output,
                                 "patch %p (%s): modifying got entry %p. original value %p. new value %p\n", (void *)ctx->patch,
                                 ctx->patch->super.patch_symbol, (void *) entry, *entry, (void *) ctx->patch->super.patch_value);

            patch_got->got_entry = entry;
            patch_got->got_orig = *entry;

            opal_list_append (&ctx->patch->patch_got_list, &patch_got->super);

            *entry = (void *) ctx->patch->super.patch_value;
        }
    } else {
        /* find the appropriate entry and restore the original value */
        mca_patcher_linux_patch_got_t *patch_got;
        OPAL_LIST_FOREACH_REV(patch_got, &ctx->patch->patch_got_list, mca_patcher_linux_patch_got_t) {
            if (patch_got->got_entry == entry) {
                opal_output_verbose (MCA_BASE_VERBOSE_TRACE, opal_patcher_base_framework.framework_output,
                                     "restoring got entry %p with original value %p\n", (void *) entry, patch_got->got_orig);
                if (*entry == (void *) ctx->patch->super.patch_value) {
                    *entry = patch_got->got_orig;
                }
                opal_list_remove_item (&ctx->patch->patch_got_list, &patch_got->super);
                OBJ_RELEASE(patch_got);
                break;
            }
        }
    }

    return OPAL_SUCCESS;
}

static int mca_patcher_linux_phdr_iterator(struct dl_phdr_info *info, size_t size, void *data)
{
    mca_patcher_linux_dl_iter_context_t *ctx = data;
    int phent;

    phent = mca_patcher_linux_get_aux_phent();
    if (phent <= 0) {
        opal_output_verbose (MCA_BASE_VERBOSE_ERROR, opal_patcher_base_framework.framework_output,
                             "failed to read phent size");
        ctx->status = OPAL_ERR_NOT_SUPPORTED;
        return -1;
    }

    ctx->status = mca_patcher_linux_modify_got (info->dlpi_addr, info->dlpi_phdr,
                                                info->dlpi_name, info->dlpi_phnum,
                                                phent, ctx);
    if (ctx->status == OPAL_SUCCESS) {
        return 0; /* continue iteration and patch all objects */
    } else {
        return -1; /* stop iteration if got a real error */
    }
}

/* called with lock held */
static int mca_patcher_linux_apply_patch (mca_patcher_linux_patch_t *patch)
{
    mca_patcher_linux_dl_iter_context_t ctx = {
        .patch    = patch,
        .remove   = false,
        .status   = OPAL_SUCCESS,
    };

    /* Avoid locks here because we don't modify ELF data structures.
     * Worst case the same symbol will be written more than once.
     */
    (void) dl_iterate_phdr(mca_patcher_linux_phdr_iterator, &ctx);

    return ctx.status;
}

static int mca_patcher_linux_remove_patch (mca_patcher_linux_patch_t *patch)
{
    mca_patcher_linux_dl_iter_context_t ctx = {
        .patch    = patch,
        .remove   = true,
        .status   = OPAL_SUCCESS,
    };

    /* Avoid locks here because we don't modify ELF data structures.
     * Worst case the same symbol will be written more than once.
     */
    (void) dl_iterate_phdr(mca_patcher_linux_phdr_iterator, &ctx);

    return ctx.status;
}

static void *mca_patcher_linux_dlopen(const char *filename, int flag)
{
    OPAL_PATCHER_BEGIN;
    mca_patcher_linux_patch_t *patch;
    void *handle;

    handle = orig_dlopen (filename, flag);
    if (handle != NULL) {
        /*
         * Every time a new object is loaded, we must update its relocations
         * with our list of patches (including dlopen itself). This code is less
         * efficient and will modify all existing objects every time, but good
         * enough.
         */
        opal_mutex_lock (&mca_patcher_linux_module.patch_list_mutex);
        OPAL_LIST_FOREACH(patch, &mca_patcher_linux_module.patch_list, mca_patcher_linux_patch_t) {
            if (!patch->super.patch_data_size) {
                opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_patcher_base_framework.framework_output,
                                     "in dlopen(), re-applying '%s' to %p", patch->super.patch_symbol, (void *) patch->super.patch_value);
                /* ignore hook binary patches */
                mca_patcher_linux_apply_patch (patch);
            }
        }
        opal_mutex_unlock (&mca_patcher_linux_module.patch_list_mutex);
    }

    OPAL_PATCHER_END;
    return handle;
}

static intptr_t mca_patcher_linux_get_orig (const char *symbol, void *replacement)
{
    const char *error;
    void *func_ptr;

    func_ptr = dlsym(RTLD_DEFAULT, symbol);
    if (func_ptr == replacement) {
        (void)dlerror();
        func_ptr = dlsym(RTLD_NEXT, symbol);
        if (func_ptr == NULL) {
            error = dlerror();
            opal_output_verbose (MCA_BASE_VERBOSE_ERROR, opal_patcher_base_framework.framework_output,
                                 "could not find address of original %s(): %s", symbol, error ? error : "Unknown error");
        }
    }

    opal_output_verbose (MCA_BASE_VERBOSE_INFO, opal_patcher_base_framework.framework_output,
                         "original %s() is at %p", symbol, func_ptr);

    return (intptr_t) func_ptr;
}

static int mca_patcher_linux_patch_symbol (const char *symbol_name, uintptr_t replacement, uintptr_t *orig)
{
    mca_patcher_linux_patch_t *patch = OBJ_NEW(mca_patcher_linux_patch_t);
    int rc;

    if (OPAL_UNLIKELY(NULL == patch)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    patch->super.patch_symbol = strdup (symbol_name);
    if (NULL == patch->super.patch_symbol) {
        OBJ_RELEASE(patch);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    patch->super.patch_value = mca_patcher_base_addr_text (replacement);
    patch->super.patch_restore = (mca_patcher_base_restore_fn_t) mca_patcher_linux_remove_patch;

    /* Take lock first to handle a possible race where dlopen() is called
     * from another thread and we may end up not patching it.
     */
    opal_mutex_lock (&mca_patcher_linux_module.patch_list_mutex);
    do {
        rc = mca_patcher_base_patch_hook (&mca_patcher_linux_module, patch->super.patch_value);
        if (OPAL_SUCCESS != rc) {
            OBJ_RELEASE(patch);
            break;
        }

        rc = mca_patcher_linux_apply_patch (patch);
        if (OPAL_SUCCESS != rc) {
            OBJ_RELEASE(patch);
            break;
        }

        *orig = mca_patcher_linux_get_orig (patch->super.patch_symbol, (void *) replacement);

        opal_list_append (&mca_patcher_linux_module.patch_list, &patch->super.super);
    } while (0);
    opal_mutex_unlock (&mca_patcher_linux_module.patch_list_mutex);

    return rc;
}

/* called with lock held */
static int mca_patcher_linux_install_dlopen (void)
{
    return mca_patcher_linux_patch_symbol ("dlopen", (uintptr_t) mca_patcher_linux_dlopen,
                                           (uintptr_t *) &orig_dlopen);
}

static int mca_patcher_linux_init (void)
{
    return mca_patcher_linux_install_dlopen ();
}

mca_patcher_base_module_t mca_patcher_linux_module = {
    .patch_init = mca_patcher_linux_init,
    .patch_symbol = mca_patcher_linux_patch_symbol,
};
