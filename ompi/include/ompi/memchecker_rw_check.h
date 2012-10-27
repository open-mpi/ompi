/*
 * Copyright (c) 2010      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MEMCHECKER_RW_CHECK_H
#define OMPI_MEMCHECKER_RW_CHECK_H

#include "ompi_config.h"
#include "opal/util/output.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/memchecker/memchecker.h"

/* better to include this file directly into ompi/include/ompi/memchecker.h
 * or merge it in opal/mca/memchecker/*
 */


/* phase 0 check memory state definition */
#define MEMCHECKER_WATCH_READ  0    /* check phase 0: mem not readable. check phase 1: watch mem read */
#define MEMCHECKER_WATCH_WRITE 1    /* check phase 0: mem not writable. check phase 1: watch mem write */
#define MEMCHECKER_WATCH_RW    2    /* check phase 0: mem not writable or readable. check phase 1: watch mem write and read operation */

/* phase 1 check memory state definition */
#define MEMCHECKER_MEM_NOT_READABLE   0
#define MEMCHECKER_MEM_NOT_WRITABLE   1
#define MEMCHECKER_MEM_NOT_ACCESSABLE 2

/* memchecker check phases */
#define MEMCHECKER_PRE_COMM_PHASE  0 /* Pre-communication check phase */
#define MEMCHECKER_POST_COMM_PHASE 1 /* Post-communication check phase */

/* definitions for printing callback in MemPin */
#define MEMPIN_CALLBACK_PRINT_SOURCE 0
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_1   1
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_2   2
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_3   3
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_4   4
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_5   5
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_ALL 100


/* bit operation macros */
#define BITS_PER_BYTE  8
#define BIT_SET1(bitmap, pos) (bitmap[(pos)/BITS_PER_BYTE] |=  (1<<((pos)%BITS_PER_BYTE)))
#define BIT_SET0(bitmap, pos) (bitmap[(pos)/BITS_PER_BYTE] &= ~(1<<((pos)%BITS_PER_BYTE)))
#define BIT_TEST(bitmap, pos) (bitmap[(pos)/BITS_PER_BYTE] &   (1<<((pos)%BITS_PER_BYTE)))
#define BIT_FLIP(bitmap, pos) (bitmap[(pos)/BITS_PER_BYTE] ^=  (1<<((pos)%BITS_PER_BYTE)))

static inline int memchecker_rw_check_show_state()
{
    size_t i, j;

    for( i = 0;i < memchecker_num_mem;i++ ) {
        char *shadow_bits;
        char tmp[3];

        shadow_bits = (char *)malloc(memchecker_rw_check[i].len*3);

        /* memset always gets seg fault under pin, so use a loop to initialize the bits. */
        for (j = 0; j < memchecker_rw_check[i].len*3; j++) {
            shadow_bits[j] = ' ';
        }
        shadow_bits[memchecker_rw_check[i].len*3] = '\0';
        
        for( j = 0; j < memchecker_rw_check[i].len; j++ ) {
            if(BIT_TEST(memchecker_rw_check[i].rw_flag, 2*j)) {
                shadow_bits[3*j] = '1';
            } else {
                shadow_bits[3*j] = '0';
            }
            if(BIT_TEST(memchecker_rw_check[i].rw_flag, 2*j+1)) {
                shadow_bits[3*j+1] = '1';
            } else {
                shadow_bits[3*j+1] = '0';
            }
        }
        
        opal_output(0, "---- memory state bits at %0x:", memchecker_rw_check[i].addr);
        opal_output(0,"%s", shadow_bits);
        
        free(shadow_bits);
    }

    return OMPI_SUCCESS;
}


/*
 * two phases of checks:
 *    pre-comm check phase : for non-blocking send/recv buffer accessibility
 *    post-comm check phase: watch on the received buffers, whether they are used correctly
 */
static int memchecker_rw_check_cb(void *addr, size_t size, int offset, int is_write, void *result)
{
    int ret = OMPI_SUCCESS;
    unsigned int i, pos, rd_err, wr_err;

    if(!memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    if( offset >= 0 ) {
        pos = 0;
    } else {
        pos = -offset;
    }

    /* opal_output(0, "checking phase:%d", memchecker_phase); */
    /* memchecker_rw_check_show_state(); */
    switch( memchecker_phase ) {
    case MEMCHECKER_PRE_COMM_PHASE:
        rd_err = 0;
        wr_err = 0;
        for( i = 0; i < size; i++ ) {
            if(is_write) {
                /* is it a write operation on a un-writable region? */
                if( BIT_TEST((*(memchecker_rw_check_t *)result).rw_flag, (pos+i)*2+1) ) {
                    wr_err++;
                }
            } else {
                /* is it a read operation on a un-readable region? */
                if( BIT_TEST((*(memchecker_rw_check_t *)result).rw_flag, (pos+i)*2) ) {
                    rd_err++;
                }
            }
        }

        if (rd_err > 0) {
            opal_output(0, "memchecker: invalid read of size %d at %0x.\n", rd_err, addr);
            /* return and print 5 levels callstack */
            ret = MEMPIN_CALLBACK_PRINT_CALLSTACK_5;
        }
        if (wr_err > 0) {
            opal_output(0, "memchecker: invalid write of size %d at %0x.\n", wr_err, addr);
            /* return and print 5 levels callstack */
            ret = MEMPIN_CALLBACK_PRINT_CALLSTACK_5;
        }

        break;
    case MEMCHECKER_POST_COMM_PHASE:
        /* opal_output(0, "<debug> [%d]: %0x\n", pos/4, (*(memchecker_rw_check_t *)result).rw_flag[pos/4]); */
        for( i = 0; i < size; i++ ) {
            /* opal_output(0, "<debug> offset: %d, pos: %d\n", offset, pos+i); */
            if(is_write) {
                if( !BIT_TEST((*(memchecker_rw_check_t *)result).rw_flag, (pos+i)*2) ) {
                    opal_output(0, "memchecker: write before read at %0x:%d\n", (size_t)addr, pos+i);

                    /* set error flag */
                    BIT_SET1((*(memchecker_rw_check_t *)result).rw_flag, (pos+i)*2+1);

                    /* return and print 5 levels callstack */
                    ret = MEMPIN_CALLBACK_PRINT_CALLSTACK_5;
                }
            } else {
                /* set read flag */
                BIT_SET1((*(memchecker_rw_check_t *)result).rw_flag, (pos+i)*2);
            }
        }
        break;
    case 2:/* for other checks */
    default:
        break;
    }
    /* opal_output(0, "<debug> [%d]: %0x\n", pos/4, (*(memchecker_rw_check_t *)result).rw_flag[pos/4]); */

    /* memchecker_rw_check_show_state(); */
    return ret;
}


static inline int memchecker_reg_mem_rw_check(void *addr, size_t count, ompi_datatype_t *datatype, int watch_type)
{
    if (!opal_memchecker_base_runindebugger() ||
        (addr == NULL) || (count == 0) ||
        !memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    unsigned int i;

    size_t len = count*(datatype->super.true_ub - datatype->super.true_lb);
    void *tmp = realloc(memchecker_rw_check, (memchecker_num_mem+1)*sizeof(memchecker_rw_check_t));

    if(!tmp) {
        /* error output */
        opal_output(0, "memchecker: memory allocation error. number of entries: %d\n", memchecker_num_mem);
    }
    memchecker_rw_check = (memchecker_rw_check_t *) tmp;

    (memchecker_rw_check[memchecker_num_mem]).addr     = addr;
    (memchecker_rw_check[memchecker_num_mem]).rw_flag  = (char *) malloc(len*2);
    (memchecker_rw_check[memchecker_num_mem]).datatype = datatype;
    (memchecker_rw_check[memchecker_num_mem]).len      = len;

    /* initialize the rw flags according to the watch_type*/
    memset((memchecker_rw_check[memchecker_num_mem]).rw_flag, 0, len*2);
    if (memchecker_phase == MEMCHECKER_PRE_COMM_PHASE) {
        if( watch_type == MEMCHECKER_WATCH_READ ) {
            for( i = 0; i < len; i++) {
                BIT_SET1((memchecker_rw_check[memchecker_num_mem]).rw_flag, i*2);
            }
        } else if( watch_type == MEMCHECKER_WATCH_WRITE ) {
            for( i = 0; i < len; i++) {
                BIT_SET1((memchecker_rw_check[memchecker_num_mem]).rw_flag, i*2+1);
            }
        } else if( watch_type == MEMCHECKER_WATCH_RW ) {
            for( i = 0; i < len; i++) {
                BIT_SET1((memchecker_rw_check[memchecker_num_mem]).rw_flag, i*2);
                BIT_SET1((memchecker_rw_check[memchecker_num_mem]).rw_flag, i*2+1);
            }
        } else {
            /* unknow watch type */
            opal_output(0, "memchecker: unknow watch type.\n");
        }
    }

    if ( OMPI_SUCCESS == 
         opal_memchecker_base_reg_mem_watch(addr, len, MEMCHECKER_WATCH_RW, (void *) &memchecker_rw_check_cb,
                                            (void *) &memchecker_rw_check[memchecker_num_mem]) ) {
        memchecker_num_mem++;
    }
/*     opal_output(0, "memchecker: number of mem: %d, read: %d; write: %d\n", memchecker_num_mem, read_cnt, write_cnt); */

    return OMPI_SUCCESS;
}


static inline int memchecker_unreg_mem_rw_check(void *addr, size_t count, ompi_datatype_t *datatype)
{
    if (!opal_memchecker_base_runindebugger() ||
        (addr == NULL) || (count == 0) ||
        !memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    size_t len = count*(datatype->super.true_ub - datatype->super.true_lb);
    if ( OMPI_SUCCESS == opal_memchecker_base_unreg_mem_watch(addr, len) && memchecker_num_mem > 0) {
        memchecker_num_mem--;
    }

/*     opal_output(0, "memchecker: number of mem: %d, read: %d; write: %d\n", memchecker_num_mem, read_cnt, write_cnt); */
    return OMPI_SUCCESS;
}


static inline int memchecker_rw_enable_check()
{
    memchecker_enable_check = true;

    return OMPI_SUCCESS;
}

static inline int memchecker_rw_disable_check()
{
    memchecker_enable_check = false;

    return OMPI_SUCCESS;
}



static inline int memchecker_check_phase(int phase)
{
    if (!opal_memchecker_base_runindebugger() ||
        !memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    /* when changing to phase 0, we don't explicitly reset the bits. */
    /* otherwise, communication is finished, reset all memory bits, */
    if(phase == 1) {
        unsigned int i;
        memchecker_phase = phase;
        if( memchecker_num_mem > 0 ) {
            for( i = 0;i < memchecker_num_mem; i++ ) {
                memset(memchecker_rw_check[i].rw_flag, 0, memchecker_rw_check[i].len*2);
            }
        }
    }

    return OMPI_SUCCESS;
}

static inline int memchecker_clear_mem_state(void *addr, size_t count, ompi_datatype_t *datatype)
{
    if (!opal_memchecker_base_runindebugger() ||
        (addr == NULL) || (count == 0) ||
        !memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    /* opal_output(0, "memchecker warning: before clear state"); */
    /* memchecker_rw_check_show_state(); */

    size_t len = count*(datatype->super.true_ub - datatype->super.true_lb);
    size_t index[10];
    unsigned int i;

    index[0]=-1;
    if ( OMPI_SUCCESS != opal_memchecker_base_search_mem_index(addr, len, index) ) {
        return -1;
    }

    if(index[0] == -1) {
/*         opal_output(0, "memchecker warning: <clear mem state>"); */
/*         opal_output(0, "memchecker warning: no memory entry found for address: %0x, with size %d.\n", addr, len); */
        return OMPI_SUCCESS;
    }

    for(i = 0; index[i] != -1; i++) {
        memset(memchecker_rw_check[i].rw_flag, 0, memchecker_rw_check[i].len*2);
    }

    /* opal_output(0, "memchecker warning: after clear state"); */
    /* memchecker_rw_check_show_state(); */
    return OMPI_SUCCESS;
}


/*
 * set memory state with:
 *   0:  memory not readable
 *   1:  memory not writable
 *   2:  memory not accessable
 */
static inline int memchecker_set_mem_state(void *addr, size_t count, ompi_datatype_t *datatype, int mem_state)
{
    if (!opal_memchecker_base_runindebugger() ||
        (addr == NULL) || (count == 0) ||
        !memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    size_t len = count*(datatype->super.true_ub - datatype->super.true_lb);
    size_t index[10];
    unsigned int i, j;

    /* opal_output(0, "memchecker warning: before set state"); */
    /* memchecker_rw_check_show_state(); */

    index[0]=-1;
    if ( OMPI_SUCCESS != opal_memchecker_base_search_mem_index(addr, len, index) ) {
        return -1;
    }

    if(index[0] == -1) {
/*         opal_output(0, "memchecker warning: <set mem state>"); */
/*         opal_output(0, "memchecker warning: no memory entry found for address: %0x, with size %d.\n", addr, len); */
        return OMPI_SUCCESS;
    }

    for(i = 0; index[i] != -1; i++) {
        for(j = 0; j < memchecker_rw_check[i].len; j++) {
            switch(mem_state) {
            case MEMCHECKER_MEM_NOT_READABLE:
                BIT_SET1(memchecker_rw_check[i].rw_flag, j*2);
                break;
            case MEMCHECKER_MEM_NOT_WRITABLE:
                BIT_SET1(memchecker_rw_check[i].rw_flag, j*2+1);
                break;
            case MEMCHECKER_MEM_NOT_ACCESSABLE:
                BIT_SET1(memchecker_rw_check[i].rw_flag, j*2);
                BIT_SET1(memchecker_rw_check[i].rw_flag, j*2+1);
                break;
            default:
                /* unknow state */
                break;
            }
        }
    }

    /* opal_output(0, "memchecker warning: after set state"); */
    /* memchecker_rw_check_show_state(); */

    return OMPI_SUCCESS;
}

static inline int memchecker_rw_check_fini()
{
    if (!opal_memchecker_base_runindebugger() ||
        !memchecker_enable_check) {
        return OMPI_SUCCESS;
    }

    unsigned int i, j, unused=0, wbr=0;

    opal_output(0, "memchecker : <error summary>");
    /* memchecker_rw_check_show_state(); */
    /* loop through the registerred memory entries, check their flags */
    for( i = 0;i < memchecker_num_mem;i++ ) {
        for( j = 0; j < memchecker_rw_check[i].len; j++ ) {
            /* check both bits */
            if( !BIT_TEST(memchecker_rw_check[i].rw_flag, 2*j) ) {
                /* performance issue: memory not used at all */
/*                 opal_output(0, "memchecker warning: buffer communicated but not used, sequence number: %d, offset: %d. %d\n", i, j, */
/*                             BIT_TEST(memchecker_rw_check[i].rw_flag, 2*j)); */
                unused++;
            }
            /* check the error bit */
            if( BIT_TEST(memchecker_rw_check[i].rw_flag, 2*j+1) ) {
                /* performance issue: memory not used at all */
/*                 opal_output(0, "memchecker warning: the communicated buffer got WBR error, sequence number: %d, offset: %d.\n", i, j); */
                wbr++;
            }
        }
    }

    opal_output(0, "memchecker : %d wbr errors, %d bytes communicated but not used.", wbr, unused);

    /* Finally, unregister all memory entries */
    opal_memchecker_base_unreg_all_mem_watch();
/*     opal_output(0, "memchecker: number of mem: %d, read: %d; write: %d\n", memchecker_num_mem, read_cnt, write_cnt); */

    return OMPI_SUCCESS;
}

#endif
