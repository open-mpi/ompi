/*
 * Copyright (c) 1999 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 */

/* Author: Bertrand Serlet, August 1999 */

/*
 * Changes from original file:
 * LAM/MPI team, June 2004
 *
 * - changed include files to allow building without Darwin internal
 *   source files
 * - Added hook into deallocation algorithm to call LAM/MPI
 *   deregistration function
 *
 * Changes marked with LAM/MPI comment in source code
 */

#import "scalable_malloc.h"

#if 0 /* LAM/MPI */
#import <pthread_internals.h>
#endif /* #if 0 */

#import <unistd.h>
#import <libc.h>
#include <mach/vm_statistics.h>

/* LAM/MPI - begin changes */
#include <mach/mach_init.h>
#include <machine/byte_order.h>
#include "pthread_spinlock.h"

#include "lam_config.h"
#include "malloc_wrapper.h"

/*
 * Need to convince the linker that we need this version of this file,
 * rather than the stock version.  This function is called by the function
 * we instruct the linker to pull in first (and is in the call stack from
 * MPI_Init().
 */
void lam_darwin_malloc_linker_hack()
{
  /*  need to do some work to keep us from getting optimized away */
}

#ifndef __PRETTY_FUNCTION__
#define __PRETTY_FUNCTION__ ""
#endif

/* LAM/MPI - end changes */

/*********************	DEFINITIONS	************************/

#define DEBUG_MALLOC	0	// set to one to debug malloc itself

#define DEBUG_CLIENT	0	// set to one to help debug a nasty memory smasher

#if DEBUG_MALLOC
#warning DEBUG_MALLOC ENABLED
#define INLINE
#define CHECK_LOCKED(szone, fun)	{	\
    if (__is_threaded && TRY_LOCK(szone->lock)) {			\
	malloc_printf("*** lock was not set %p in %s\n", szone->lock, fun);	\
    }						\
}
#else
#define INLINE	__inline__
#define CHECK_LOCKED(szone, fun)	{}
#endif

#define PAGE_SIZE_FIXED	1	// flip if the page size becomes variable, one day
#if PAGE_SIZE_FIXED
#define vm_page_shift	12
#else
static unsigned vm_page_shift = 0; // guaranteed to be intialized by zone creation
#endif

typedef unsigned short msize_t; // a size in multiples of SHIFT_SMALL_QUANTUM or SHIFT_TINY_QUANTUM

typedef struct {
    unsigned	checksum;
    void	*previous;
    void	*next;
} free_list_t;

typedef struct {
    unsigned 	address_and_num_pages;
    // this type represents both an address and a number of pages
    // the low bits are the number of pages; the high bits are the address
    // note that the exact number of bits used for depends on the page size
    // also, this cannot represent pointers larger than 1 << (vm_page_shift * 2)
} compact_range_t;

typedef unsigned char	grain_t;

#define CHECK_REGIONS			(1 << 31)

#define CHECKSUM_MAGIC			0x357B

#define MAX_RECORDER_BUFFER	256

/*********************	DEFINITIONS for tiny	************************/

#define	SHIFT_TINY_QUANTUM			4	// Required for AltiVec
#define	TINY_QUANTUM				(1 << SHIFT_TINY_QUANTUM)

#define FOLLOWING_TINY_PTR(ptr,msize)	(((char *)(ptr)) + ((msize) << SHIFT_TINY_QUANTUM))

#define NUM_TINY_SLOTS			32	// number of slots for free-lists

#define SHIFT_NUM_TINY_BLOCKS	16
#define NUM_TINY_BLOCKS		(1 << SHIFT_NUM_TINY_BLOCKS)
#define TINY_BLOCKS_ALIGN	(SHIFT_NUM_TINY_BLOCKS + SHIFT_TINY_QUANTUM)
#define TINY_REGION_SIZE	((NUM_TINY_BLOCKS * TINY_QUANTUM + (NUM_TINY_BLOCKS >> 2) + 8 + (1 << vm_page_shift) - 1) & ~ ((1 << vm_page_shift) - 1))	// enough room for the data, followed by the bit arrays (2-bits per block) plus 2 words of padding as our bitmap operators overflow, plus rounding to the nearest page

#define TINY_FREE_SIZE(ptr)	(((msize_t *)(ptr))[6])
// At the end of free blocks, we stick the size (for enabling coalescing)
#define TINY_PREVIOUS_MSIZE(ptr)		((msize_t *)(ptr))[-1]


#define TINY_REGION_ADDRESS(region)		((region) << TINY_BLOCKS_ALIGN)
#define TINY_REGION_END(region)		(TINY_REGION_ADDRESS(region)+(1 << TINY_BLOCKS_ALIGN))

typedef unsigned short tiny_region_t;

#define INITIAL_NUM_TINY_REGIONS	24	// must be even for szone to be aligned

#define TINY_CACHE	1	// This governs a last-free cache of 1 that bypasses the free-list

#if ! TINY_CACHE
#warning TINY_CACHE turned off
#endif

/*********************	DEFINITIONS for small	************************/

/* We store the meta bits on the side in two bytes, as follows:
- high order bit SMALL_IS_FREE is set when the block is avail (and starts here)
- when block size, expressed in SMALL_QUANTUM, is the other 15 bits
- else 0 signifies this block is in the middle of another block
*/

#define SMALL_IS_FREE	(1 << 15)

#define	SHIFT_SMALL_QUANTUM			(SHIFT_TINY_QUANTUM + 5)	// 9
#define	SMALL_QUANTUM				(1 << SHIFT_SMALL_QUANTUM) // 512 bytes

#define FOLLOWING_SMALL_PTR(ptr,msize)	(((char *)(ptr)) + ((msize) << SHIFT_SMALL_QUANTUM))

#define NUM_SMALL_SLOTS			32	// number of slots for free-lists

#define SHIFT_NUM_SMALL_BLOCKS		14	// we can only represent up to 1<<15 for msize; but we chose to stay even below that to avoid the convention msize=0 => msize = (1<<15)
#define NUM_SMALL_BLOCKS		(1 << SHIFT_NUM_SMALL_BLOCKS)
#define SMALL_BLOCKS_ALIGN		(SHIFT_NUM_SMALL_BLOCKS + SHIFT_SMALL_QUANTUM) // 23
#define SMALL_REGION_SIZE			(NUM_SMALL_BLOCKS * SMALL_QUANTUM + NUM_SMALL_BLOCKS * 2)	// data + meta data

#define SMALL_PREVIOUS_MSIZE(ptr)		((msize_t *)(ptr))[-1]

#define SMALL_REGION_ADDRESS(region)	(((unsigned)region) << SMALL_BLOCKS_ALIGN)
#define SMALL_REGION_END(region)	(SMALL_REGION_ADDRESS(region)+(1 << SMALL_BLOCKS_ALIGN))

typedef unsigned short small_region_t;

#define INITIAL_NUM_SMALL_REGIONS	6	// must be even for szone to be aligned

#define PROTECT_SMALL			0	// Should be 0: 1 is too slow for normal use

#define SMALL_CACHE	1
#if !SMALL_CACHE
#warning SMALL_CACHE turned off
#endif

/*********************	DEFINITIONS for large	************************/

#define LARGE_THRESHOLD			(15 * 1024) // at or above this use "large"

#if (LARGE_THRESHOLD > NUM_SMALL_SLOTS * SMALL_QUANTUM)
#error LARGE_THRESHOLD should always be less than NUM_SMALL_SLOTS * SMALL_QUANTUM
#endif

#define VM_COPY_THRESHOLD		(40 * 1024)
    // When all memory is touched after a copy, vm_copy() is always a lose
    // But if the memory is only read, vm_copy() wins over memmove() at 3 or 4 pages (on a G3/300MHz)
    // This must be larger than LARGE_THRESHOLD

#define LARGE_ENTRY_ADDRESS(entry)					\
    (((entry).address_and_num_pages >> vm_page_shift) << vm_page_shift)
#define LARGE_ENTRY_NUM_PAGES(entry)					\
    ((entry).address_and_num_pages & ((1 << vm_page_shift) - 1))
#define LARGE_ENTRY_SIZE(entry)						\
    (LARGE_ENTRY_NUM_PAGES(entry) << vm_page_shift)
#define LARGE_ENTRY_MATCHES(entry,ptr)					\
    (!(((entry).address_and_num_pages - (unsigned)(ptr)) >> vm_page_shift))
#define LARGE_ENTRY_IS_EMPTY(entry)	(!((entry).address_and_num_pages))

typedef compact_range_t large_entry_t;

/*********************	DEFINITIONS for huge	************************/

typedef vm_range_t huge_entry_t;

/*********************	zone itself	************************/

typedef struct {
    malloc_zone_t	basic_zone;
    pthread_lock_t	lock;
    unsigned		debug_flags;
    void		*log_address;

    /* Regions for tiny objects */
    unsigned		num_tiny_regions;
    tiny_region_t	*tiny_regions;
    void		*last_tiny_free; // low SHIFT_TINY_QUANTUM indicate the msize
    unsigned		tiny_bitmap; // cache of the 32 free lists
    free_list_t		*tiny_free_list[NUM_TINY_SLOTS]; // 31 free lists for 1*TINY_QUANTUM to 31*TINY_QUANTUM plus 1 for larger than 32*SMALL_QUANTUM
    size_t		tiny_bytes_free_at_end; // the last free region in the last block is treated as a big block in use that is not accounted for
    unsigned		num_tiny_objects;
    unsigned		num_bytes_in_tiny_objects;

    /* Regions for small objects */
    unsigned		num_small_regions;
    small_region_t	*small_regions;
    void		*last_small_free; // low SHIFT_SMALL_QUANTUM indicate the msize
    unsigned		small_bitmap; // cache of the free list
    free_list_t		*small_free_list[NUM_SMALL_SLOTS];
    size_t		small_bytes_free_at_end; // the last free region in the last block is treated as a big block in use that is not accounted for
    unsigned		num_small_objects;
    unsigned		num_bytes_in_small_objects;

    /* large objects: vm_page_shift <= log2(size) < 2 *vm_page_shift */
    unsigned		num_large_objects_in_use;
    unsigned		num_large_entries;
    large_entry_t	*large_entries; // hashed by location; null entries don't count
    unsigned		num_bytes_in_large_objects;
    
    /* huge objects: log2(size) >= 2 *vm_page_shift */
    unsigned char	num_huge_entries;
    huge_entry_t	*huge_entries;
    unsigned		num_bytes_in_huge_objects;

    /* Initial region list */
    tiny_region_t	initial_tiny_regions[INITIAL_NUM_TINY_REGIONS];
    small_region_t	initial_small_regions[INITIAL_NUM_SMALL_REGIONS];
} szone_t;

static void *szone_malloc(szone_t *szone, size_t size);
static INLINE void *szone_malloc_should_clear(szone_t *szone, size_t size, boolean_t cleared_requested);
static void szone_free(szone_t *szone, void *ptr);
static boolean_t szone_check_all(szone_t *szone, const char *function);
static void szone_print(szone_t *szone, boolean_t verbose);
static void *small_malloc_from_region_no_lock(szone_t *szone, msize_t msize);

#if DEBUG_MALLOC
#define LOG(szone,ptr)							\
    (szone->log_address && (((unsigned)szone->log_address == -1) || (szone->log_address == (void *)(ptr))))
#else
#define LOG(szone,ptr)		0
#endif

#define SZONE_LOCK(szone) 		{	\
    LOCK(szone->lock);				\
}

#define SZONE_UNLOCK(szone)		{	\
    UNLOCK(szone->lock);			\
}

#define LOCK_AND_NOTE_LOCKED(szone,locked)	{	\
    CHECK(szone, __PRETTY_FUNCTION__); 			\
    locked = 1; SZONE_LOCK(szone); 			\
}

#if DEBUG_MALLOC || DEBUG_CLIENT
#define CHECK(szone,fun)						\
    if ((szone)->debug_flags & CHECK_REGIONS) szone_check_all(szone, fun)
#else
#define CHECK(szone,fun)	{}
#endif

/*********************	VERY LOW LEVEL UTILITIES  ************************/

#if DEBUG_MALLOC || DEBUG_CLIENT
static void
szone_sleep(void) {
    if (getenv("MallocErrorSleep")) {
	malloc_printf("*** Sleeping to help debug\n");
	sleep(3600); // to help debug
    }
}
#endif

static void
szone_error(szone_t *szone, const char *msg, const void *ptr) {
    if (szone) SZONE_UNLOCK(szone);
    if (ptr) {
	malloc_printf("*** malloc[%d]: error for object %p: %s\n", getpid(), ptr, msg);
    } else {
	malloc_printf("*** malloc[%d]: error: %s\n", getpid(), msg);
    }
#if DEBUG_MALLOC
    szone_print(szone, 1);
    szone_sleep();
#endif
#if DEBUG_CLIENT
    szone_sleep();
#endif
}

static void
protect(szone_t *szone, vm_address_t address, vm_size_t size,
  unsigned protection, unsigned debug_flags) {
    kern_return_t	err;
    if (!(debug_flags & SCALABLE_MALLOC_DONT_PROTECT_PRELUDE)) {
	err = vm_protect(mach_task_self(), address - (1 << vm_page_shift), 1 << vm_page_shift,
	  0, protection);
	if (err) {
	    malloc_printf("*** malloc[%d]: Can't protect(%p) region for "
	      "prelude guard page at %p\n", getpid(), protection,
	      address - (1 << vm_page_shift));
	}
    }
    if (!(debug_flags & SCALABLE_MALLOC_DONT_PROTECT_POSTLUDE)) {
	err = vm_protect(mach_task_self(), (vm_address_t)(address + size), 1 << vm_page_shift, 0, protection);
	if (err) {
	    malloc_printf("*** malloc[%d]: Can't protect(%p) region for "
	      "postlude guard page at %p\n", getpid(), protection,
	      address + size);
	}
    }
}

static vm_address_t
allocate_pages(szone_t *szone, size_t size, unsigned char align, unsigned debug_flags, int vm_page_label) {
    // align specifies a desired alignment (as a log) or 0 if no alignment requested
    kern_return_t	err;
    vm_address_t	addr;
    boolean_t		add_guard_pages = debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES;
    size_t		allocation_size = round_page(size);
    if (align) add_guard_pages = 0; // too cumbersome to deal with that
    if (!allocation_size) allocation_size = 1 << vm_page_shift;
    if (add_guard_pages) allocation_size += 2 * (1 << vm_page_shift);
    if (align) allocation_size += 1 << align;
    err = vm_allocate(mach_task_self(), &addr, allocation_size, vm_page_label | 1);
    if (err) {
	malloc_printf("*** malloc: vm_allocate(size=%d) failed (error code=%d)\n", size, err);
	szone_error(szone, "Can't allocate region", NULL);
	return NULL;
    }
    if (align) {
	// malloc_printf("In allocate_pages(size=%d(%p), align=%d) -> %p\n", size, size, align, addr);
	vm_address_t	aligned_address = (addr + (1 << align) - 1) & ~ ((1 << align) - 1);
	if (aligned_address != addr) {
	    size_t	delta = aligned_address - addr;
	    err = vm_deallocate(mach_task_self(), addr, delta);
	    if (err) malloc_printf("*** malloc: freeing unaligned header failed with %d\n", err);
	    // malloc_printf("deallocated unaligned header %p length=%d(%p)\n", addr, delta, delta);
	    addr = aligned_address;
	    allocation_size -= delta;
	}
	if (allocation_size > size) {
	    err = vm_deallocate(mach_task_self(), addr+size, allocation_size - size);
	    if (err) malloc_printf("*** malloc: freeing unaligned footer failed with %d\n", err);
	}
    }
    if (add_guard_pages) {
	addr += 1 << vm_page_shift;
	protect(szone, addr, size, 0, debug_flags);
    }
    return addr;
}

static void
deallocate_pages(szone_t *szone, vm_address_t addr, size_t size, unsigned debug_flags) {
    kern_return_t	err;
    boolean_t		add_guard_pages = debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES;
    if (add_guard_pages) {
	addr -= 1 << vm_page_shift;
	size += 2 * (1 << vm_page_shift);
    }
    lam_handle_free((void*) addr, size);
    err = vm_deallocate(mach_task_self(), addr, size);
    if (err) {
	szone_error(szone, "Can't deallocate_pages region", (void *)addr);
    }
}

static kern_return_t
_szone_default_reader(task_t task, vm_address_t address, vm_size_t size, void **ptr) {
    *ptr = (void *)address;
    return 0;
}

static INLINE void
free_list_checksum(szone_t *szone, free_list_t *ptr, const char *msg) {
    // We always checksum, as testing whether to do it (based on szone->debug_flags) is as fast as doing it
    if (ptr->checksum != (((unsigned)ptr->previous) ^ ((unsigned)ptr->next) ^ CHECKSUM_MAGIC)) {
#if DEBUG_MALLOC
	malloc_printf("*** Incorrect checksum: %s\n", msg);
#endif
	szone_error(szone, "Incorrect checksum for freed object - object was probably modified after being freed; break at szone_error", ptr);
    }
}

static INLINE void
free_list_set_checksum(szone_t *szone, free_list_t *ptr) {
    // We always set checksum, as testing whether to do it (based on
    // szone->debug_flags) is slower than just doing it
    ptr->checksum = ((unsigned)ptr->previous) ^ ((unsigned)ptr->next) ^ CHECKSUM_MAGIC;
}

static unsigned
free_list_count(const free_list_t *ptr) {
    unsigned	count = 0;
    while (ptr) {
	count++;
//	malloc_printf("%p ", ptr);
	ptr = ptr->next;
    }
    return count;
}

#define BITMAP32_SET(bitmap,bit) 	(bitmap |= 1 << (bit))
#define BITMAP32_CLR(bitmap,bit)	(bitmap &= ~ (1 << (bit)))
#define BITMAP32_BIT(bitmap,bit) 	((bitmap >> (bit)) & 1)

#define BITMAP32_FFS(bitmap) (ffs(bitmap))
    // returns bit # of first bit that's one, starting at 1 (returns 0 if !bitmap)

/*********************	TINY FREE LIST UTILITIES	************************/

// We encode the meta-headers as follows:
// Each quantum has an associated set of 2 bits:
// block_header when 1 says this block is the beginning of a block
// in_use when 1 says this block is in use
// so a block in use of size 3 is 1-1 0-X 0-X
// for a free block TINY_FREE_SIZE(ptr) carries the size and the bits are 1-0 X-X X-X
// for a block middle the bits are 0-0

// Attention double evaluation for these
#define BITARRAY_SET(bits,index)	(bits[index>>3] |= (1 << (index & 7)))
#define BITARRAY_CLR(bits,index)	(bits[index>>3] &= ~(1 << (index & 7)))
#define BITARRAY_BIT(bits,index)	(((bits[index>>3]) >> (index & 7)) & 1)

// Following is for start<8 and end<=start+32
#define BITARRAY_MCLR_LESS_32(bits,start,end) {	\
    unsigned char	*_bits = (bits);	\
    unsigned	_end = (end);			\
    switch (_end >> 3) {			\
	case 4: _bits[4] &= ~ ((1 << (_end - 32)) - 1); _end = 32; 	\
	case 3: _bits[3] &= ~ ((1 << (_end - 24)) - 1); _end = 24;	\
	case 2: _bits[2] &= ~ ((1 << (_end - 16)) - 1); _end = 16; 	\
	case 1: _bits[1] &= ~ ((1 << (_end - 8)) - 1); _end = 8; 	\
	case 0: _bits[0] &= ~ ((1 << _end) - (1 << (start)));		\
    }	\
}

#if 0	// Simple but slow version
#warning Slow version in effect
#define BITARRAY_MCLR(bits,index,num)	{		\
    unsigned	_ctr = (num);			\
    unsigned	_cur = (index);			\
    while (_ctr--) {BITARRAY_CLR(bits,_cur); _cur++; }	\
}
#else

// Following is for num <= 32
#define BITARRAY_MCLR(bits,index,num)	{		\
    unsigned	_index = (index);			\
    unsigned char	*_rebased = (bits) + (_index >> 3);	\
    _index &= 7;					\
    BITARRAY_MCLR_LESS_32(_rebased, _index, _index + (num)); \
}
#endif

static INLINE msize_t
get_tiny_meta_header(const void *ptr, boolean_t *is_free) {
    // returns msize and is_free
    // may return 0 for the msize component (meaning 65536)
    unsigned short	shifted_base = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << TINY_BLOCKS_ALIGN;
    unsigned char	*block_header = (unsigned char *)headers_start;
    msize_t		index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
    unsigned		byte_index = index >> 3;
    block_header += byte_index;
    index &= 7;
    *is_free = 0;
    if (!BITMAP32_BIT(*block_header, index)) return 0;
    unsigned char	*in_use = block_header + (NUM_TINY_BLOCKS >> 3) + 4;
    if (!BITMAP32_BIT(*in_use, index)) {
	*is_free = 1;
	return TINY_FREE_SIZE(ptr);
    }
#if defined(__BIG_ENDIAN__)
    unsigned	*addr = (void *)((unsigned)block_header & ~3);
    unsigned 	word0 = OSReadSwapInt32(addr, 0);
    unsigned 	word1 = OSReadSwapInt32(addr, 4);
    unsigned	bits = index + (((unsigned)block_header & 3) * 8);
    unsigned	word = (word0 >> bits) | (word1 << (32 - bits));
    unsigned	result = ffs(word >> 1);
#if DEBUG_MALLOC
    if (result >= 32) {
	malloc_printf("*** get_tiny_meta_header() invariant broken %p %d\n", ptr, result);
	szone_sleep();
    }
#endif
    return result;
#else
    unsigned	cur = index + 1;
    while (!BITARRAY_BIT(block_header, cur)) cur++; // assumes padding at the zone end
#if DEBUG_MALLOC
    if (cur - index >= 32) {
	malloc_printf("*** get_tiny_meta_header() invariant broken %p %d %d\n", ptr, index, cur);
	szone_sleep();
    }
#endif
    return cur - index;
#endif
}

static INLINE void
set_tiny_meta_header_in_use(const void *ptr, msize_t msize) {
    unsigned short	shifted_base = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << TINY_BLOCKS_ALIGN;
    unsigned char	*block_header = (unsigned char *)headers_start;
    msize_t		index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
#if DEBUG_MALLOC
    if (msize >= 32) malloc_printf("*** set_tiny_meta_header_in_use() invariant broken %p %d\n", ptr, msize);
    if ((unsigned)index + (unsigned)msize > 0x10000) malloc_printf("*** set_tiny_meta_header_in_use() invariant broken (2) %p %d\n", ptr, msize);
#endif
    unsigned		byte_index = index >> 3;
    block_header += byte_index;
    index &= 7;
    BITMAP32_SET(*block_header, index); 
    unsigned char	*in_use = block_header + (NUM_TINY_BLOCKS >> 3) + 4;
    BITMAP32_SET(*in_use, index); 
    index++; 
    msize_t	clr_msize = msize-1;
    if (clr_msize) {
	byte_index = index >> 3;
	block_header += byte_index; in_use += byte_index;
	index &= 7;
	unsigned	end_bit = index + clr_msize;
	BITARRAY_MCLR_LESS_32(block_header, index, end_bit);
	BITARRAY_MCLR_LESS_32(in_use, index, end_bit);
    }
    BITARRAY_SET(block_header, index+clr_msize); // we set the block_header bit for the following block to reaffirm next block is a block
#if DEBUG_MALLOC
    boolean_t	ff;
    msize_t	mf = get_tiny_meta_header(ptr, &ff);
    if (msize != mf) {
	malloc_printf("*** setting header for tiny in_use %p : %d\n", ptr, msize);
	malloc_printf("reading header for tiny %p : %d %d\n", ptr, mf, ff);
    }
#endif
}

static INLINE void
set_tiny_meta_header_middle(const void *ptr) {
    // indicates this block is in the middle of an in use block
    unsigned short	shifted_base = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << TINY_BLOCKS_ALIGN;
    unsigned char	*block_header = (unsigned char *)headers_start;
    unsigned char	*in_use = (unsigned char *)(headers_start + (NUM_TINY_BLOCKS >> 3) + 4);
    msize_t		index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
    BITARRAY_CLR(block_header, index); BITARRAY_CLR(in_use, index); 
    TINY_FREE_SIZE(ptr) = 0;
}

static INLINE void
set_tiny_meta_header_free(const void *ptr, msize_t msize) {
    // !msize is acceptable and means 65536
    unsigned short	shifted_base = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << TINY_BLOCKS_ALIGN;
    unsigned char	*block_header = (unsigned char *)headers_start;
    unsigned char	*in_use = (unsigned char *)(headers_start + (NUM_TINY_BLOCKS >> 3) + 4);
    msize_t		index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
#if DEBUG_MALLOC
    if ((unsigned)index + (unsigned)msize > 0x10000) {
	malloc_printf("*** setting header for tiny free %p msize too large: %d\n", ptr, msize);
    }
#endif
    BITARRAY_SET(block_header, index); BITARRAY_CLR(in_use, index); 
    TINY_FREE_SIZE(ptr) = msize;
    // mark the end of this block
    if (msize) {	// msize==0 => the whole region is free
	void	*follower = FOLLOWING_TINY_PTR(ptr, msize);
	TINY_PREVIOUS_MSIZE(follower) = msize;
    }
#if DEBUG_MALLOC
    boolean_t	ff;
    msize_t	mf = get_tiny_meta_header(ptr, &ff);
    if ((msize != mf) || !ff) {
	malloc_printf("*** setting header for tiny free %p : %d\n", ptr, msize);
	malloc_printf("reading header for tiny %p : %d %d\n", ptr, mf, ff);
    }
#endif
}

static INLINE boolean_t
tiny_meta_header_is_free(const void *ptr) {
    // returns msize and is_free shifted by 16
    // may return 0 for the msize component (meaning 65536)
    unsigned short	shifted_base = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << TINY_BLOCKS_ALIGN;
    unsigned char	*block_header = (unsigned char *)headers_start;
    unsigned char	*in_use = (unsigned char *)(headers_start + (NUM_TINY_BLOCKS >> 3) + 4);
    msize_t		index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
    if (!BITARRAY_BIT(block_header, index)) return 0;
    return !BITARRAY_BIT(in_use, index);
}

static INLINE void *
tiny_previous_preceding_free(void *ptr, msize_t *prev_msize) {
    // returns the previous block, assuming and verifying it's free
    unsigned short	shifted_base = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << TINY_BLOCKS_ALIGN;
    unsigned char	*block_header = (unsigned char *)headers_start;
    unsigned char	*in_use = (unsigned char *)(headers_start + (NUM_TINY_BLOCKS >> 3) + 4);
    msize_t	index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
    if (!index) return NULL;
    msize_t	previous_msize = TINY_PREVIOUS_MSIZE(ptr);
    if (previous_msize > index) return NULL;
    msize_t	previous_index = index - previous_msize;
    void	*previous_ptr = (void *)((shifted_base << TINY_BLOCKS_ALIGN) + (previous_index << SHIFT_TINY_QUANTUM));
    if (TINY_FREE_SIZE(previous_ptr) != previous_msize) return NULL;
    if (!BITARRAY_BIT(block_header, previous_index)) return NULL;
    if (BITARRAY_BIT(in_use, previous_index)) return NULL;
    // conservative check did match true check
    *prev_msize = previous_msize;
    // malloc_printf("tiny_previous_preceding_free(%p) -> %p,%d\n", ptr, previous_ptr, previous_msize);
    return previous_ptr;
}

static INLINE void
tiny_free_list_add_ptr(szone_t *szone, void *ptr, msize_t msize) {
    // Adds an item to the proper free list
    // Also marks the meta-header of the block properly
    // Assumes szone has been locked
    grain_t	slot = (!msize || (msize >= NUM_TINY_SLOTS)) ? NUM_TINY_SLOTS - 1 : msize - 1;
    free_list_t	*free_ptr = ptr;
    free_list_t	*free_head = szone->tiny_free_list[slot];
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In tiny_free_list_add_ptr(), ptr=%p, msize=%d\n", ptr, msize);
    }
    if (((unsigned)ptr) & (TINY_QUANTUM - 1)) {
	szone_error(szone, "tiny_free_list_add_ptr: Unaligned ptr", ptr);
    }
#endif
    set_tiny_meta_header_free(ptr, msize);
    if (free_head) {
	free_list_checksum(szone, free_head, __PRETTY_FUNCTION__);
#if DEBUG_MALLOC
	if (free_head->previous) {
	    malloc_printf("ptr=%p slot=%d free_head=%p previous=%p\n", ptr, slot, free_head, free_head->previous);
	    szone_error(szone, "tiny_free_list_add_ptr: Internal invariant broken (free_head->previous)", ptr);
	}
	if (! tiny_meta_header_is_free(free_head)) {
	    malloc_printf("ptr=%p slot=%d free_head=%p\n", ptr, slot, free_head);
	    szone_error(szone, "tiny_free_list_add_ptr: Internal invariant broken (free_head is not a free pointer)", ptr);
	}
#endif
	free_head->previous = free_ptr;
	free_list_set_checksum(szone, free_head);
    } else {
	BITMAP32_SET(szone->tiny_bitmap, slot);
    }
    free_ptr->previous = NULL;
    free_ptr->next = free_head;
    free_list_set_checksum(szone, free_ptr);
    szone->tiny_free_list[slot] = free_ptr;
    // malloc_printf("Setting head of free list for slot=%d to %p\n", slot, free_ptr);
}

static INLINE void
tiny_free_list_remove_ptr(szone_t *szone, void *ptr, msize_t msize) {
    // Removes item in the proper free list
    // msize could be read, but all callers have it so we pass it in
    // Assumes szone has been locked
    grain_t	slot = (!msize || (msize >= NUM_TINY_SLOTS)) ? NUM_TINY_SLOTS - 1 : msize - 1;
    free_list_t	*free_ptr = ptr;
    free_list_t	*next = free_ptr->next;
    free_list_t	*previous = free_ptr->previous;
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In tiny_free_list_remove_ptr(), ptr=%p, msize=%d\n", ptr, msize);
    }
#endif
    free_list_checksum(szone, free_ptr, __PRETTY_FUNCTION__);
    if (!previous) {
	// The block to remove is the head of the free list
#if DEBUG_MALLOC
	if (szone->tiny_free_list[slot] != ptr) {
	    malloc_printf("ptr=%p slot=%d msize=%d szone->tiny_free_list[slot]=%p\n", ptr, slot, msize, szone->tiny_free_list[slot]);
	    szone_error(szone, "tiny_free_list_remove_ptr: Internal invariant broken (szone->tiny_free_list[slot])", ptr);
	    return;
	}
#endif
	szone->tiny_free_list[slot] = next;
	if (!next) BITMAP32_CLR(szone->tiny_bitmap, slot);
    } else {
	previous->next = next;
	free_list_set_checksum(szone, previous);
    }
    if (next) {
	next->previous = previous;
	free_list_set_checksum(szone, next);
    }
}

static INLINE tiny_region_t *
tiny_region_for_ptr_no_lock(szone_t *szone, const void *ptr) {
    tiny_region_t	*region = szone->tiny_regions;
    unsigned		num_regions = szone->num_tiny_regions;
    unsigned		ptr_shifted = ((unsigned)ptr) >> TINY_BLOCKS_ALIGN;
    while (num_regions--) {
	tiny_region_t	this = *region;
	if (ptr_shifted == this) return region;
	region++;
    }
    return NULL;
}

static INLINE void
tiny_free_no_lock(szone_t *szone, tiny_region_t *region, void *ptr, msize_t msize) {
    size_t	original_size = msize << SHIFT_TINY_QUANTUM;
    void	*next_block = ((char *)ptr + original_size);
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In tiny_free_no_lock(), ptr=%p, msize=%d\n", ptr, msize);
    }
    if (! msize) {
	malloc_printf("In tiny_free_no_lock(), ptr=%p, msize=%d\n", ptr, msize);
	szone_error(szone, "Trying to free tiny block that is too small", ptr);
    }
#endif
    // We try to coalesce this block with the preceeding one
    msize_t	previous_msize;
    void	*previous = tiny_previous_preceding_free(ptr, &previous_msize);
    if (previous) {
#if DEBUG_MALLOC
	if (LOG(szone, ptr) || LOG(szone,previous)) { 
	    malloc_printf("In tiny_free_no_lock(), coalesced backwards for %p previous=%p\n", ptr, previous);
	}
#endif
	tiny_free_list_remove_ptr(szone, previous, previous_msize);
	ptr = previous;
	msize += previous_msize;
    }
    // We try to coalesce with the next block
    if (((vm_address_t)next_block < TINY_REGION_END(*region)) && tiny_meta_header_is_free(next_block)) {
	// The next block is free, we coalesce
	msize_t	next_msize = TINY_FREE_SIZE(next_block);
#if DEBUG_MALLOC
	if (LOG(szone, ptr) || LOG(szone, next_block)) {
	    malloc_printf("In tiny_free_no_lock(), for ptr=%p, msize=%d coalesced forward=%p next_msize=%d\n", ptr, msize, next_block, next_msize);
	}
#endif
	if (next_msize >= NUM_TINY_SLOTS) {
	    // we take a short cut here to avoid removing next_block from the slot 31 freelist and then adding ptr back to slot 31
	    // malloc_printf("Replacing %p(msize=%d) with %p(msize=%d) in freelist\n", next_block, next_msize, ptr, msize+next_msize);
	    msize += next_msize;
	    free_list_t	*big_free_block = (free_list_t *)next_block;
	    free_list_t	*after_next_block = big_free_block->next;
	    free_list_t	*before_next_block = big_free_block->previous;
	    free_list_checksum(szone, big_free_block, __PRETTY_FUNCTION__);
	    if (!before_next_block) {
		szone->tiny_free_list[NUM_TINY_SLOTS-1] = ptr;
	    } else {
		before_next_block->next = ptr;
		free_list_set_checksum(szone, before_next_block);
	    }
	    if (after_next_block) {
		after_next_block->previous = ptr;
		free_list_set_checksum(szone, after_next_block);
	    }
	    ((free_list_t *)ptr)->previous = before_next_block;
	    ((free_list_t *)ptr)->next = after_next_block;
	    free_list_set_checksum(szone, ptr);
	    set_tiny_meta_header_free(ptr, msize);
	    set_tiny_meta_header_middle(big_free_block); // clear the meta_header to enable coalescing backwards
	    goto tiny_free_ending;
	}
	tiny_free_list_remove_ptr(szone, next_block, next_msize);
	set_tiny_meta_header_middle(next_block); // clear the meta_header to enable coalescing backwards
	msize += next_msize;
    }
    if ((szone->debug_flags & SCALABLE_MALLOC_DO_SCRIBBLE) && msize) {
	memset(ptr, 0x55, msize << SHIFT_TINY_QUANTUM);
    }
    tiny_free_list_add_ptr(szone, ptr, msize);
  tiny_free_ending:
    // When in proper debug mode we write on the memory to help debug memory smashers
    szone->num_tiny_objects--;
    szone->num_bytes_in_tiny_objects -= original_size; // we use original_size and not msize to avoid double counting the coalesced blocks
}

static void *
tiny_malloc_from_region_no_lock(szone_t *szone, msize_t msize) {
    // Allocates from the last region or a freshly allocated region
    // Before anything we transform the tiny_bytes_free_at_end - if any - to a regular free block
    if (szone->tiny_bytes_free_at_end) {
	tiny_region_t	last_region = szone-> tiny_regions[szone->num_tiny_regions-1];
	void	*last_block = (void *)(TINY_REGION_END(last_region) - szone->tiny_bytes_free_at_end);
	tiny_free_list_add_ptr(szone, last_block, szone->tiny_bytes_free_at_end >> SHIFT_TINY_QUANTUM);
	szone->tiny_bytes_free_at_end = 0;
    }
    void		*ptr;
    // time to create a new region
    vm_address_t	aligned_address = allocate_pages(szone, TINY_REGION_SIZE, TINY_BLOCKS_ALIGN, 0, VM_MAKE_TAG(VM_MEMORY_MALLOC_TINY));
    if (! aligned_address) {
	// out of memory!
	return NULL;
    }
    // malloc_printf("Allocated tiny region #%d: %p [%y]\n", szone->num_tiny_regions, aligned_address, TINY_REGION_SIZE);
    // We set the padding after block_header to be all 1
    ((unsigned *)(aligned_address + (1 << TINY_BLOCKS_ALIGN) + (NUM_TINY_BLOCKS >> 3)))[0] = ~0;
    if (szone->num_tiny_regions == INITIAL_NUM_TINY_REGIONS) {
	tiny_region_t	*new_regions;
	// malloc_printf("=== Growing tiny_regions (%d regions)\n", szone->num_tiny_regions);
	new_regions = small_malloc_from_region_no_lock(szone, 16); // 16 * 512 bytes is plenty of tiny regions (more than 4,000)
	if (!new_regions) return NULL;
	memcpy(new_regions, szone->tiny_regions, INITIAL_NUM_TINY_REGIONS * sizeof(tiny_region_t));
	szone->tiny_regions = new_regions; // we set the pointer after it's all ready to enable enumeration from another thread without locking
    }
    szone->tiny_regions[szone->num_tiny_regions] = aligned_address >> TINY_BLOCKS_ALIGN;
    szone->num_tiny_regions ++; // we set the number after the pointer is all ready to enable enumeration from another thread without taking the lock
    ptr = (void *)aligned_address; 
    set_tiny_meta_header_in_use(ptr, msize);
    szone->num_tiny_objects++;
    szone->num_bytes_in_tiny_objects += msize << SHIFT_TINY_QUANTUM;
    // We put a header on the last block so that it appears in use (for coalescing, etc...)
    set_tiny_meta_header_in_use(ptr + (msize << SHIFT_TINY_QUANTUM), 1);
    szone->tiny_bytes_free_at_end = (NUM_TINY_BLOCKS - msize) << SHIFT_TINY_QUANTUM;
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In tiny_malloc_from_region_no_lock(), ptr=%p, msize=%d\n", ptr, msize);
    }
#endif
    return ptr;
}

static INLINE boolean_t
try_realloc_tiny_in_place(szone_t *szone, void *ptr, size_t old_size, size_t new_size) {
    // returns 1 on success
    msize_t	index = (((unsigned)ptr) >> SHIFT_TINY_QUANTUM) & (NUM_TINY_BLOCKS - 1);
    msize_t	old_msize = old_size >> SHIFT_TINY_QUANTUM;
    unsigned	next_index = index + old_msize;
    // malloc_printf("try_realloc_tiny_in_place %p %d %d\n", ptr, old_size, new_size);
    if (next_index >= NUM_TINY_BLOCKS) {
	// malloc_printf("try_realloc_tiny_in_place can't take place at end %p %d %d %d\n", ptr, old_size, new_size, next_index);
	return 0;
    }
    void	*next_block = (char *)ptr + old_size;
    SZONE_LOCK(szone);
    boolean_t	is_free = tiny_meta_header_is_free(next_block);
    if (!is_free) {
	SZONE_UNLOCK(szone);
	return 0; // next_block is in use;
    }
    msize_t	next_msize = TINY_FREE_SIZE(next_block);
    if (old_size + (next_msize >> SHIFT_TINY_QUANTUM) < new_size) {
	// malloc_printf("try_realloc_tiny_in_place can't %p too small %d\n", next_block, next_msize);
	SZONE_UNLOCK(szone);
	return 0; // even with next block, not enough
    }
    tiny_free_list_remove_ptr(szone, next_block, next_msize);
    set_tiny_meta_header_middle(next_block); // clear the meta_header to enable coalescing backwards
    msize_t	coalesced_msize = (new_size - old_size + TINY_QUANTUM - 1) >> SHIFT_TINY_QUANTUM;
    msize_t	leftover_msize = next_msize - coalesced_msize;
    // malloc_printf("Realloc in place for %p;  current size=%d next_msize=%d wanted=%d\n", ptr, old_size, next_msize, new_size);
    if (leftover_msize) {
	void	*leftover = next_block + (coalesced_msize << SHIFT_TINY_QUANTUM);
	// malloc_printf("Leftover in realloc in place %p leftover_msize=%d\n", leftover, leftover_msize);
	tiny_free_list_add_ptr(szone, leftover, leftover_msize);
    }
    set_tiny_meta_header_in_use(ptr, old_msize + coalesced_msize);
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In try_realloc_tiny_in_place(), ptr=%p, msize=%d\n", ptr, old_msize + coalesced_msize);
    }
#endif
    szone->num_bytes_in_tiny_objects += coalesced_msize << SHIFT_TINY_QUANTUM;
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
    // malloc_printf("Extended ptr %p for realloc old=%d desired=%d new=%d leftover=%d\n", ptr, (unsigned)old_size, (unsigned)new_size, (unsigned)szone_size(szone, ptr), leftover_msize << SHIFT_TINY_QUANTUM);
    return 1;
}

static boolean_t
szone_check_tiny_region(szone_t *szone, tiny_region_t *region) {
    vm_address_t	start = TINY_REGION_ADDRESS(*region);
    void		*ptr = (void *)start;
    vm_address_t	region_end = TINY_REGION_END(*region);
    boolean_t		prev_free = 0;
    if (region == szone->tiny_regions + szone->num_tiny_regions - 1) region_end -= szone->tiny_bytes_free_at_end;
    // malloc_printf("szone_check_tiny_region: szone=%p region=%p start=%p ptr=%p region_end=%p\n", szone, region, start, ptr, region_end);
    while ((vm_address_t)ptr < region_end) {
	boolean_t	is_free;
	msize_t		msize = get_tiny_meta_header(ptr, &is_free);
	if (is_free && !msize && (ptr == (void *)start)) {
	    // the entire region is free
	    return 1;
	}
	// malloc_printf("tiny %p [%d %d]\n", ptr, msize, is_free);
	if (! msize) {
	    malloc_printf("*** malloc[%d]: invariant broken for tiny block %p this msize=%d - size is too small\n", getpid(), ptr, msize);
	    return 0;
	}
	if (!is_free) {
	    // this block is in use
	    prev_free = 0;
	    if (msize > 31*TINY_QUANTUM) {
		malloc_printf("*** malloc[%d]: invariant broken for %p this tiny msize=%d[%p] - size is too large\n", getpid(), ptr, msize, msize);
		return 0;
	    }
	    ptr += msize << SHIFT_TINY_QUANTUM;
	} else {
	    // free pointer
	    if (prev_free) {
		malloc_printf("*** malloc[%d]: invariant broken for free block %p this tiny msize=%d: two free blocks in a row\n", getpid(), ptr, msize);
		return 0;
	    }
	    prev_free = 1;
	    free_list_t	*free_head = ptr;
	    free_list_checksum(szone, free_head, __PRETTY_FUNCTION__);
	    if (free_head->previous && !tiny_meta_header_is_free(free_head->previous)) {
		malloc_printf("*** malloc[%d]: invariant broken for %p (previous %p is not a free pointer)\n", getpid(), ptr, free_head->previous);
		return 0;
	    }
	    if (free_head->next && !tiny_meta_header_is_free(free_head->next)) {
		malloc_printf("*** malloc[%d]: invariant broken for %p (next in free list %p is not a free pointer)\n", getpid(), ptr, free_head->next);
		return 0;
	    }
	    void	*follower = FOLLOWING_TINY_PTR(ptr, msize);
	    if ((follower != (void *)region_end) && (TINY_PREVIOUS_MSIZE(follower) != msize)) {
		malloc_printf("*** malloc[%d]: invariant broken for tiny free %p followed by %p in region [%p-%p] (end marker incorrect) should be %d; in fact %d\n", getpid(), ptr, follower, TINY_REGION_ADDRESS(*region), region_end, msize, TINY_PREVIOUS_MSIZE(follower));
		return 0;
	    }
	    ptr = follower;
	}
    }
    if (ptr != (void *)region_end) {
	malloc_printf("*** malloc[%d]: invariant broken for region end %p - %p\n", getpid(), ptr, region_end);
	return 0;
    }
    if (region == szone->tiny_regions + szone->num_tiny_regions - 1) {
	if (szone->tiny_bytes_free_at_end) {
	    boolean_t	is_free;
	    msize_t	msize = get_tiny_meta_header(ptr, &is_free);
	    if (is_free || (msize != 1)) {
		malloc_printf("*** malloc[%d]: invariant broken for blocker block %p - %d %d\n", getpid(), ptr, msize, is_free);
	    }
	}
    }
    return 1;
}

static kern_return_t
tiny_in_use_enumerator(task_t task, void *context, unsigned type_mask, vm_address_t region_address, unsigned short num_regions, size_t tiny_bytes_free_at_end, memory_reader_t reader, vm_range_recorder_t recorder) {
    tiny_region_t	*regions;
    unsigned		index = 0;
    vm_range_t		buffer[MAX_RECORDER_BUFFER];
    unsigned		count = 0;
    kern_return_t	err;
    err = reader(task, region_address, sizeof(tiny_region_t) * num_regions, (void **)&regions);
    if (err) return err;
    while (index < num_regions) {
	// unsigned		num_in_use = 0;
	// unsigned		num_free = 0;
	tiny_region_t	region = regions[index];
	vm_range_t	range = {TINY_REGION_ADDRESS(region), TINY_REGION_SIZE};
	// malloc_printf("Enumerating tiny ptrs for tiny region starting at %p\n", range.address);
	if (type_mask & MALLOC_ADMIN_REGION_RANGE_TYPE) {
	    vm_range_t	admin_range = {range.address + (1 << TINY_BLOCKS_ALIGN), range.size - (1 << TINY_BLOCKS_ALIGN)};
	    recorder(task, context, MALLOC_ADMIN_REGION_RANGE_TYPE, &admin_range, 1);
	}
	if (type_mask & (MALLOC_PTR_REGION_RANGE_TYPE | MALLOC_ADMIN_REGION_RANGE_TYPE)) {
	    vm_range_t	ptr_range = {range.address, 1 << TINY_BLOCKS_ALIGN};
	    recorder(task, context, MALLOC_PTR_REGION_RANGE_TYPE, &ptr_range, 1);
	}
	if (type_mask & MALLOC_PTR_IN_USE_RANGE_TYPE) {
	    unsigned char	*mapped_region;
	    err = reader(task, range.address, range.size, (void **)&mapped_region);
	    if (err) return err;
	    unsigned char	*block_header = (unsigned char *)(mapped_region + (1 << TINY_BLOCKS_ALIGN));
	    unsigned char	*in_use = block_header + (NUM_TINY_BLOCKS >> 3) + 4;
	    unsigned		block_index = 0;
	    unsigned		block_limit = NUM_TINY_BLOCKS;
	    if (index == num_regions - 1)
		block_limit -= (tiny_bytes_free_at_end >> SHIFT_TINY_QUANTUM);
	    while (block_index < block_limit) {
		boolean_t	is_free = ! BITARRAY_BIT(in_use, block_index);
		msize_t		msize;
		if (is_free) {
		    void	*mapped_ptr = mapped_region + (block_index << SHIFT_TINY_QUANTUM);
		    msize = TINY_FREE_SIZE(mapped_ptr);
		    // printf("free:  index=%x mapped=%p true_addr=%p msize=%d\n", block_index, mapped_ptr, (void *)range.address + (block_index << SHIFT_TINY_QUANTUM), msize);
		    // num_free++;
		    if (!msize) break;
		} else {
		    msize = 1; 
		    unsigned 	bit = block_index + 1;
		    while (! BITARRAY_BIT(block_header, bit)) { bit++; msize ++; }
		    // printf("in_use: index=%x true_addr=%p msize=%d\n", block_index, (void *)range.address + (block_index << SHIFT_TINY_QUANTUM), msize);
		    // num_in_use++;
		    buffer[count].address = range.address + (block_index << SHIFT_TINY_QUANTUM);
		    buffer[count].size = msize << SHIFT_TINY_QUANTUM;
		    count++;
		    if (count >= MAX_RECORDER_BUFFER) {
			recorder(task, context, MALLOC_PTR_IN_USE_RANGE_TYPE, buffer, count);
			count = 0;
		    }
		}
		block_index += msize;
	    }
	}
	// malloc_printf("Found in tiny region %d in_use and %d free\n", num_in_use, num_free);
	index++;
    }
    if (count) {
	recorder(task, context, MALLOC_PTR_IN_USE_RANGE_TYPE, buffer, count);
    }
    return 0;
}

static INLINE void *
tiny_malloc_from_free_list(szone_t *szone, msize_t msize) {
    // Assumes we've locked the region
    void	*ptr;
    msize_t	this_msize;
    grain_t	slot = msize-1;
    free_list_t	**free_list = szone->tiny_free_list;
    free_list_t	**the_slot = free_list + slot;
    ptr = *the_slot;
    if (ptr) {
	free_list_t	*next;
	next = ((free_list_t *)ptr)->next;
	if (next) {
	    next->previous = NULL;
	    free_list_set_checksum(szone, next);
	}
	*the_slot = next;
	this_msize = msize;
#if DEBUG_MALLOC
	if (LOG(szone,ptr)) {
	    malloc_printf("In tiny_malloc_from_free_list(), exact match ptr=%p, this_msize=%d\n", ptr, this_msize);
	}
#endif
	goto return_tiny_alloc;
    }
    // adjust slot based on bitmap
    unsigned	bitmap = szone->tiny_bitmap & ~ ((1 << slot) - 1);
    if (! bitmap) goto try_tiny_malloc_from_end;
    slot = BITMAP32_FFS(bitmap) - 1;
    free_list_t	**limit = free_list + NUM_TINY_SLOTS - 1;
    free_list += slot;
    while (free_list < limit) {
	// try bigger grains
	ptr = *free_list;
	if (ptr) {
	    free_list_t	*next;
	    next = ((free_list_t *)ptr)->next;
	    if (next) {
		next->previous = NULL;
		free_list_set_checksum(szone, next);
	    }
	    *free_list = next;
	    this_msize = TINY_FREE_SIZE(ptr);
#if DEBUG_MALLOC
	    if (LOG(szone,ptr)) {
		malloc_printf("In tiny_malloc_from_free_list(), bigger grain ptr=%p, msize=%d this_msize=%d\n", ptr, msize, this_msize);
	    }
#endif
	    goto add_leftover_and_proceed;
	}
	free_list++;
    }
    // we are now looking at the last slot (31)
    ptr = *limit;
    if (ptr) {
	free_list_t	*next;
	this_msize = TINY_FREE_SIZE(ptr);
	next = ((free_list_t *)ptr)->next;
	if (this_msize - msize >= NUM_TINY_SLOTS) {
	    // the leftover will go back to the free list, so we optimize by modifying the free list rather than removing the head and then adding back
	    // malloc_printf("Allocation from largest tiny slot %p optimized\n", ptr);
	    msize_t	leftover_msize = this_msize - msize;
	    void	*leftover_ptr = ptr + (msize << SHIFT_TINY_QUANTUM);
	    *limit = leftover_ptr;
	    if (next) {
		next->previous = leftover_ptr;
		free_list_set_checksum(szone, next);
	    }
	    ((free_list_t *)leftover_ptr)->next = next;
	    ((free_list_t *)leftover_ptr)->previous = NULL;
	    free_list_set_checksum(szone, leftover_ptr);
	    set_tiny_meta_header_free(leftover_ptr, leftover_msize);
#if DEBUG_MALLOC
	    if (LOG(szone,ptr)) {
		malloc_printf("In tiny_malloc_from_free_list(), last slot ptr=%p, msize=%d this_msize=%d\n", ptr, msize, this_msize);
	    }
#endif
	    this_msize = msize;
	    goto return_tiny_alloc;
	}
	*limit = next;
	if (next) {
	    next->previous = NULL;
	    free_list_set_checksum(szone, next);
	}
	goto add_leftover_and_proceed;
    }
try_tiny_malloc_from_end:
    // Let's see if we can use szone->tiny_bytes_free_at_end
    if (szone->tiny_bytes_free_at_end >= (msize << SHIFT_TINY_QUANTUM)) {
	ptr = (void *)(TINY_REGION_END(szone->tiny_regions[szone->num_tiny_regions-1]) - szone->tiny_bytes_free_at_end);
	szone->tiny_bytes_free_at_end -= msize << SHIFT_TINY_QUANTUM;
	if (szone->tiny_bytes_free_at_end) {
	    // let's add an in use block after ptr to serve as boundary
	    set_tiny_meta_header_in_use(ptr + (msize << SHIFT_TINY_QUANTUM), 1);
	}
	this_msize = msize;
#if DEBUG_MALLOC
	if (LOG(szone,ptr)) {
	    malloc_printf("In tiny_malloc_from_free_list(), from end ptr=%p, msize=%d\n", ptr, msize);
	}
#endif
	goto return_tiny_alloc;
    }
    return NULL;
add_leftover_and_proceed:
    // malloc_printf("For msize=%d found tiny in free_list (slot=%d) this_msize=%d\n", msize, free_list - szone->tiny_free_list, this_msize);
    if (!this_msize || (this_msize > msize)) {
	msize_t	leftover_msize = this_msize - msize;
	void	*leftover_ptr = ptr + (msize << SHIFT_TINY_QUANTUM);
#if DEBUG_MALLOC
	if (LOG(szone,ptr)) {
	    malloc_printf("In tiny_malloc_from_free_list(), adding leftover ptr=%p, this_msize=%d\n", ptr, this_msize);
	}
#endif
	tiny_free_list_add_ptr(szone, leftover_ptr, leftover_msize);
	this_msize = msize;
    }
return_tiny_alloc:
    szone->num_tiny_objects++;
    szone->num_bytes_in_tiny_objects += this_msize << SHIFT_TINY_QUANTUM;
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In tiny_malloc_from_free_list(), ptr=%p, this_msize=%d, msize=%d\n", ptr, this_msize, msize);
    }
#endif
    set_tiny_meta_header_in_use(ptr, this_msize);
    return ptr;
}

static INLINE void *
tiny_malloc_should_clear(szone_t *szone, msize_t msize, boolean_t cleared_requested) {
    boolean_t	locked = 0;
    void	*ptr;
#if DEBUG_MALLOC
    if (! msize) {
	szone_error(szone, "Invariant broken (!msize) in allocation (region)", NULL);
    }
#endif
#if TINY_CACHE
    ptr = (void *)szone->last_tiny_free;
    if ((((unsigned)ptr) & (TINY_QUANTUM - 1)) == msize) {
	// we have a candidate - let's lock to make sure
	LOCK_AND_NOTE_LOCKED(szone, locked);
	if (ptr == (void *)szone->last_tiny_free) {
	    szone->last_tiny_free = NULL;
	    // malloc_printf("using last_tiny_free\n");
	    SZONE_UNLOCK(szone);
	    CHECK(szone, __PRETTY_FUNCTION__);
	    ptr = (void *)((unsigned)ptr & ~ (TINY_QUANTUM - 1));
	    if (cleared_requested) {
		memset(ptr, 0, msize << SHIFT_TINY_QUANTUM);
	    }
#if DEBUG_MALLOC
	    if (LOG(szone,ptr)) {
		malloc_printf("In tiny_malloc_should_clear(), tiny cache ptr=%p, msize=%d\n", ptr, msize);
	    }
#endif
	    return ptr;
	}
	// malloc_printf("optimistic locking for last_tiny_free failed\n");
    }
#endif
    // Except in rare occasions where we need to add a new region, we are going to end up locking, so we might as well lock right away to avoid doing unnecessary optimistic probes
    if (!locked) LOCK_AND_NOTE_LOCKED(szone, locked);
    ptr = tiny_malloc_from_free_list(szone, msize);
    // malloc_printf("tiny_malloc_from_free_list(%d) returned %p\n", msize, ptr);
    if (ptr) {
	SZONE_UNLOCK(szone);
	CHECK(szone, __PRETTY_FUNCTION__);
	if (cleared_requested) {
	    memset(ptr, 0, msize << SHIFT_TINY_QUANTUM);
	}
	return ptr;
    }
    ptr = tiny_malloc_from_region_no_lock(szone, msize);
    // malloc_printf("tiny_malloc_from_region_no_lock returned %p for msize=%d\n", ptr, msize);
    // we don't clear because this freshly allocated space is pristine
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
    return ptr;
}

static INLINE void
free_tiny(szone_t *szone, void *ptr, tiny_region_t *tiny_region) {
    // ptr is known to be in tiny_region
    SZONE_LOCK(szone);
#if TINY_CACHE
    void *ptr2 = szone->last_tiny_free;
    if (ptr == (void *)((unsigned)ptr2 & ~ (TINY_QUANTUM - 1))) {
	szone_error(szone, "Double free", ptr);
	return;
    }
#endif /* TINY_CACHE */
    boolean_t	is_free;
    msize_t	msize = get_tiny_meta_header(ptr, &is_free);
    if (is_free) {
	szone_error(szone, "Double free", ptr);
	return;
    }
    // malloc_printf("%p[%x]\n", ptr, msize);
#if DEBUG_MALLOC
    if (!msize) {
	malloc_printf("*** szone_free() block in use is too large: %p\n", ptr);
    }
#endif
#if TINY_CACHE
    if (msize < TINY_QUANTUM) {	// to see if the bits fit in the last 4 bits
	szone->last_tiny_free = (void *)(((unsigned)ptr) | msize);
	if (!ptr2) {
	    // malloc_printf("stuffing last_tiny_free\n");
	    SZONE_UNLOCK(szone);
	    CHECK(szone, __PRETTY_FUNCTION__);
	    return;
	}
	// malloc_printf("replacing previous last_tiny_free %p with %p\n", ptr2, szone->last_tiny_free);
	msize = (unsigned)ptr2 & (TINY_QUANTUM - 1);
	ptr = (void *)(((unsigned)ptr2) & ~ (TINY_QUANTUM - 1));
	tiny_region = tiny_region_for_ptr_no_lock(szone, ptr);
	if (!tiny_region) {
	    szone_error(szone, "Double free (tiny cache)", ptr);
	}
    }
#endif
    tiny_free_no_lock(szone, tiny_region, ptr, msize);
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
}

static void
print_tiny_free_list(szone_t *szone) {
    grain_t		slot = 0;
    malloc_printf("Tiny free sizes: ");
    while (slot < NUM_TINY_SLOTS) {
	free_list_t	*ptr = szone->tiny_free_list[slot];
	if (ptr) {
	    malloc_printf("%s%y[%d]; ", (slot == NUM_TINY_SLOTS-1) ? ">=" : "", (slot+1)*TINY_QUANTUM, free_list_count(ptr));
	}
	slot++;
    }
    malloc_printf("\n");
}

static void
print_tiny_region(boolean_t verbose, tiny_region_t region, size_t bytes_at_end) {
    unsigned	counts[1024];
    unsigned	in_use = 0;
    vm_address_t	start = TINY_REGION_ADDRESS(region);
    vm_address_t	current = start;
    vm_address_t	limit = TINY_REGION_END(region) - bytes_at_end;
    memset(counts, 0, 1024 * sizeof(unsigned));
    while (current < limit) {
	boolean_t	is_free;
	msize_t		msize = get_tiny_meta_header((void *)current, &is_free);
	// malloc_printf("%p [%d %d]; ", current, msize, is_free);
	if (is_free & !msize && (current == start)) {
	    // first block is all free
	    break;
	}
	if (!msize) {
	    malloc_printf("*** Error with %p: msize=%d\n", current, msize);
	    break;
	}
	if (! is_free) {
	    // block in use
	    if (msize > 32) malloc_printf("*** Error at %p msize for in_use is %d\n", current, msize);
	    if (msize < 1024) counts[msize]++;
	    in_use++;
	}
	current += msize << SHIFT_TINY_QUANTUM;
    }
    malloc_printf("Tiny region [%p-%p, %y]\t", start, TINY_REGION_END(region), (int)TINY_REGION_SIZE);
    malloc_printf("In_use=%d ", in_use);
    if (bytes_at_end) malloc_printf("Untouched=%y ", bytes_at_end);
    if (verbose && in_use) {
	unsigned	ci = 0;
	malloc_printf("\n\tSizes in use: "); 
	while (ci < 1024) {
	    if (counts[ci]) {
		malloc_printf("%d[%d] ", ci << SHIFT_TINY_QUANTUM, counts[ci]);
	    }
	    ci++;
	}
    }
    malloc_printf("\n");
}

static boolean_t
tiny_free_list_check(szone_t *szone, grain_t slot) {
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    unsigned	count = 0;
    free_list_t	*ptr = szone->tiny_free_list[slot];
    free_list_t	*previous = NULL;
    while (ptr) {
	free_list_checksum(szone, ptr, __PRETTY_FUNCTION__);
	boolean_t	is_free = tiny_meta_header_is_free(ptr);
	if (! is_free) {
	    malloc_printf("*** malloc[%d]: In-use ptr in free list slot=%d count=%d ptr=%p\n", getpid(), slot, count, ptr);
	    return 0;
	}
	if (((unsigned)ptr) & (TINY_QUANTUM - 1)) {
	    malloc_printf("*** malloc[%d]: Unaligned ptr in free list slot=%d  count=%d ptr=%p\n", getpid(), slot, count, ptr);
	    return 0;
	}
	if (!tiny_region_for_ptr_no_lock(szone, ptr)) {
	    malloc_printf("*** malloc[%d]: Ptr not in szone slot=%d  count=%d ptr=%p\n", getpid(), slot, count, ptr);
	    return 0;
	}
	if (ptr->previous != previous) {
	    malloc_printf("*** malloc[%d]: Previous incorrectly set slot=%d  count=%d ptr=%p\n", getpid(), slot, count, ptr);
	    return 0;
	}
	previous = ptr;
	ptr = ptr->next;
	count++;
    }
    // malloc_printf("tiny_free_list_check passed\n");
    return 1;
}

/*********************	SMALL FREE LIST UTILITIES	************************/

static INLINE msize_t *
small_meta_headers(const void *ptr) {
    // returns address of meta info
    unsigned short	shifted_base = ((unsigned)ptr) >> SMALL_BLOCKS_ALIGN;
    unsigned		headers_start = (shifted_base + 1) << SMALL_BLOCKS_ALIGN;
    return (msize_t *)headers_start;
}

static INLINE msize_t
small_meta_index(const void *ptr) {
    // returns address of meta info
    return (((unsigned)ptr) >> SHIFT_SMALL_QUANTUM) & (NUM_SMALL_BLOCKS - 1);
}

static INLINE msize_t *
small_meta_header(const void *ptr) {
    // returns address of meta info
    msize_t	*meta_headers = small_meta_headers(ptr);
    msize_t	index = small_meta_index(ptr);
    return meta_headers + index;
}

static INLINE void
small_meta_header_set_is_free(msize_t *meta_headers, msize_t index, msize_t msize) {
    // Indicates that the meta_header for index says 'is free'
    meta_headers[index] = msize | SMALL_IS_FREE;
}

static INLINE void
small_meta_header_set_in_use(msize_t *meta_headers, msize_t index, msize_t msize) {
    // Indicates that the meta_header for index says 'in use'
    meta_headers[index] = msize;
}

static INLINE void
small_meta_header_set_middle(msize_t *meta_headers, msize_t index) {
    // Indicates that the meta_header for index says 'in the middle of a block'
    meta_headers[index] = 0;
}

static void
small_free_list_add_ptr(szone_t *szone, void *ptr, msize_t msize) {
    // Adds an item to the proper free list
    // Also marks the header of the block properly
    // Assumes szone has been locked
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    grain_t	grain = (msize <= NUM_SMALL_SLOTS) ? msize - 1 : NUM_SMALL_SLOTS - 1;
    free_list_t	*free_ptr = ptr;
    free_list_t	*free_head = szone->small_free_list[grain];
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In small_free_list_add_ptr(), ptr=%p, msize=%d\n", ptr, msize);
    }
    if (((unsigned)ptr) & (SMALL_QUANTUM - 1)) {
	szone_error(szone, "small_free_list_add_ptr: Unaligned ptr", ptr);
    }
#endif
    if (free_head) {
	free_list_checksum(szone, free_head, __PRETTY_FUNCTION__);
#if DEBUG_MALLOC
	if (free_head->previous) {
	    malloc_printf("ptr=%p grain=%d free_head=%p previous=%p\n", ptr, grain, free_head, free_head->previous);
	    szone_error(szone, "small_free_list_add_ptr: Internal invariant broken (free_head->previous)", ptr);
	}
	if (!(small_meta_header(free_head)[0] & SMALL_IS_FREE)) {
	    malloc_printf("ptr=%p grain=%d free_head=%p\n", ptr, grain, free_head);
	    szone_error(szone, "small_free_list_add_ptr: Internal invariant broken (free_head is not a free pointer)", ptr);
	}
#endif
	free_head->previous = free_ptr;
	free_list_set_checksum(szone, free_head);
    } else {
	BITMAP32_SET(szone->small_bitmap, grain);
    }
    free_ptr->previous = NULL;
    free_ptr->next = free_head;
    free_list_set_checksum(szone, free_ptr);
    szone->small_free_list[grain] = free_ptr;
    void	*follower = ptr + (msize << SHIFT_SMALL_QUANTUM);
    SMALL_PREVIOUS_MSIZE(follower) = msize;
}

static void
small_free_list_remove_ptr(szone_t *szone, void *ptr, msize_t msize) {
    // Removes item in the proper free list
    // msize could be read, but all callers have it so we pass it in
    // Assumes szone has been locked
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    grain_t	grain = (msize <= NUM_SMALL_SLOTS) ? msize - 1 : NUM_SMALL_SLOTS - 1;
    free_list_t	*free_ptr = ptr;
    free_list_t	*next = free_ptr->next;
    free_list_t	*previous = free_ptr->previous;
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In small_free_list_remove_ptr(), ptr=%p, msize=%d\n", ptr, msize);
    }
#endif
    free_list_checksum(szone, free_ptr, __PRETTY_FUNCTION__);
    if (!previous) {
#if DEBUG_MALLOC
	if (szone->small_free_list[grain] != ptr) {
	    malloc_printf("ptr=%p grain=%d msize=%d szone->small_free_list[grain]=%p\n", ptr, grain, msize, szone->small_free_list[grain]);
	    szone_error(szone, "small_free_list_remove_ptr: Internal invariant broken (szone->small_free_list[grain])", ptr);
	    return;
	}
#endif
	szone->small_free_list[grain] = next;
	if (!next) BITMAP32_CLR(szone->small_bitmap, grain);
    } else {
	previous->next = next;
	free_list_set_checksum(szone, previous);
    }
    if (next) {
	next->previous = previous;
	free_list_set_checksum(szone, next);
    }
}

static INLINE small_region_t *
small_region_for_ptr_no_lock(szone_t *szone, const void *ptr) {
    small_region_t	*region = szone->small_regions;
    unsigned		num_regions = szone->num_small_regions;
    unsigned		ptr_shifted = ((unsigned)ptr) >> SMALL_BLOCKS_ALIGN;
    while (num_regions--) {
	small_region_t	this = *region;
	if (ptr_shifted == this) return region;
	region++;
    }
    return NULL;
}

static INLINE void
small_free_no_lock(szone_t *szone, small_region_t *region, void *ptr, msize_t msize) {
    // Assumes locked
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    msize_t	*meta_headers = small_meta_headers(ptr);
    msize_t	index = small_meta_index(ptr);
    size_t	original_size = msize << SHIFT_SMALL_QUANTUM;
    void	*next_block = ((char *)ptr + original_size);
    msize_t	next_index = index + msize;
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In small_free_no_lock(), ptr=%p, msize=%d\n", ptr, msize);
    }
    if (! msize) {
	malloc_printf("In small_free_no_lock(), ptr=%p, msize=%d\n", ptr, msize);
	szone_error(szone, "Trying to free small block that is too small", ptr);
    }
    // printf("In small_free_no_lock %p - msize=%d\n", ptr, msize);
#endif
    // We try to coalesce this block with the preceeding one
    if (index && (SMALL_PREVIOUS_MSIZE(ptr) <= index)) {
	msize_t	previous_msize = SMALL_PREVIOUS_MSIZE(ptr);
	if (meta_headers[index - previous_msize] == (previous_msize | SMALL_IS_FREE)) {
	    void	*previous = ptr - (previous_msize << SHIFT_SMALL_QUANTUM);
	    // previous is really to be coalesced
#if DEBUG_MALLOC
	    if (LOG(szone, ptr) || LOG(szone,previous)) { 
		malloc_printf("In small_free_no_lock(), coalesced backwards for %p previous=%p\n", ptr, previous);
	    }
#endif
	    // malloc_printf("In small_free_no_lock(), coalesced backwards for %p previous=%p\n", ptr, previous);
	    small_free_list_remove_ptr(szone, previous, previous_msize);
	    small_meta_header_set_middle(meta_headers, index);
	    ptr = previous;
	    msize += previous_msize;
	    index -= previous_msize;
	}
    }
    // We try to coalesce with the next block
    if (((vm_address_t)next_block < SMALL_REGION_END(*region)) && (meta_headers[next_index] & SMALL_IS_FREE)) {
	// next block is free, we coalesce
	msize_t	next_msize = meta_headers[next_index] & ~ SMALL_IS_FREE;
#if DEBUG_MALLOC
	if (LOG(szone,ptr)) malloc_printf("In small_free_no_lock(), for ptr=%p, msize=%d coalesced next block=%p next_msize=%d\n", ptr, msize, next_block, next_msize);
#endif
	// malloc_printf("In small_free_no_lock(), for ptr=%p, msize=%d coalesced next block=%p next_msize=%d\n", ptr, msize, next_block, next_msize);
	small_free_list_remove_ptr(szone, next_block, next_msize);
	small_meta_header_set_middle(meta_headers, next_index);
	msize += next_msize;
    }
    if (szone->debug_flags & SCALABLE_MALLOC_DO_SCRIBBLE) {
	if (!msize) {
	    szone_error(szone, "Incorrect size information - block header was damaged", ptr);
	} else {
	    memset(ptr, 0x55, (msize << SHIFT_SMALL_QUANTUM));
	}
    }
    small_free_list_add_ptr(szone, ptr, msize);
    small_meta_header_set_is_free(meta_headers, index, msize);
    szone->num_small_objects--;
    szone->num_bytes_in_small_objects -= original_size; // we use original_size and not msize to avoid double counting the coalesced blocks
}

static void *
small_malloc_from_region_no_lock(szone_t *szone, msize_t msize) {
    // Allocates from the last region or a freshly allocated region
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    // Before anything we transform the small_bytes_free_at_end - if any - to a regular free block
    if (szone->small_bytes_free_at_end) {
	small_region_t	last_region = szone->small_regions[szone->num_small_regions - 1];
	void	*last_block = (void *)(SMALL_REGION_END(last_region) - szone->small_bytes_free_at_end);
	small_free_list_add_ptr(szone, last_block, szone->small_bytes_free_at_end >> SHIFT_SMALL_QUANTUM);
	small_meta_header(last_block)[0] = (szone->small_bytes_free_at_end >> SHIFT_SMALL_QUANTUM) | SMALL_IS_FREE;
	szone->small_bytes_free_at_end = 0;
    }
    void		*ptr;
    // time to create a new region
    vm_address_t	new_address = allocate_pages(szone, SMALL_REGION_SIZE, SMALL_BLOCKS_ALIGN, 0, VM_MAKE_TAG(VM_MEMORY_MALLOC_SMALL));
    if (!new_address) {
	// out of memory!
	return NULL;
    }
    ptr = (void *)new_address;
    msize_t	*meta_headers = small_meta_headers(ptr);
    msize_t	index = 0;
    // malloc_printf("Allocated small region #%d: %p [%y]\n", szone->num_small_regions, new_address, SMALL_REGION_SIZE);
    if (szone->num_small_regions == INITIAL_NUM_SMALL_REGIONS) {
	// time to grow the number of regions
	unsigned	region_capacity = (1 << (32 - SMALL_BLOCKS_ALIGN)) - 20; // that is for sure the maximum number of small regions we can have
	msize_t		new_msize = (region_capacity * sizeof(small_region_t) + SMALL_QUANTUM - 1) / SMALL_QUANTUM;
	small_region_t	*new_regions = ptr;
	// malloc_printf("Now %d small_regions growing regions %p to %d msize=%d\n", szone->num_small_regions + 1, szone->small_regions, region_capacity, new_msize);
	small_meta_header_set_in_use(meta_headers, index, new_msize);
	szone->num_small_objects++;
	szone->num_bytes_in_small_objects += new_msize << SHIFT_SMALL_QUANTUM;
	memcpy(new_regions, szone->small_regions, INITIAL_NUM_SMALL_REGIONS * sizeof(small_region_t));
	// We intentionally leak the previous regions pointer to avoid multi-threading crashes if another thread was reading it (unlocked) while we are changing it.  
	szone->small_regions = new_regions; // note we set this pointer after it's all set
	ptr += new_msize << SHIFT_SMALL_QUANTUM;
	index = new_msize;
	// malloc_printf("Regions is now %p next ptr is %p\n", szone->small_regions, ptr);
    }
    szone->small_regions[szone->num_small_regions] = new_address >> SMALL_BLOCKS_ALIGN;
    szone->num_small_regions++; // we bump the number of regions AFTER we have changes the regions pointer to enable finding a small region without taking the lock
    // malloc_printf("Now %d small regions\n", szone->num_small_regions);
    small_meta_header_set_in_use(meta_headers, index, msize);
    msize_t		msize_left = NUM_SMALL_BLOCKS - index;
    szone->num_small_objects++;
    szone->num_bytes_in_small_objects += msize << SHIFT_SMALL_QUANTUM;
    // add a big free block
    index += msize; msize_left -= msize;
    meta_headers[index] = msize_left;
    szone->small_bytes_free_at_end = msize_left << SHIFT_SMALL_QUANTUM;
    // malloc_printf("small_bytes_free_at_end set to %d\n", szone-> small_bytes_free_at_end);
    return ptr;
}

static boolean_t
szone_check_small_region(szone_t *szone, small_region_t *region) {
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    void		*ptr = (void *)SMALL_REGION_ADDRESS(*region);
    msize_t		*meta_headers = small_meta_headers(ptr);
    vm_address_t	region_end = SMALL_REGION_END(*region);
    msize_t		prev_free = 0;
    if (region == szone->small_regions + szone->num_small_regions - 1) region_end -= szone->small_bytes_free_at_end;
    while ((vm_address_t)ptr < region_end) {
	msize_t		index = small_meta_index(ptr);
	msize_t		msize_and_free = meta_headers[index];
	if (! (msize_and_free & SMALL_IS_FREE)) {
	    // block is in use
	    msize_t	msize = msize_and_free;
	    if (!msize) {
		malloc_printf("*** malloc[%d]: invariant broken: null msize ptr=%p region#=%d num_small_regions=%d end=%p\n", getpid(), ptr, region - szone->small_regions, szone->num_small_regions, (void *)region_end);
		return 0;
	    }
	    if (msize > (LARGE_THRESHOLD / SMALL_QUANTUM)) {
		malloc_printf("*** malloc[%d]: invariant broken for %p this small msize=%d - size is too large\n", getpid(), ptr, msize_and_free);
		return 0;
	    }
	    ptr += msize << SHIFT_SMALL_QUANTUM;
	    prev_free = 0;
	} else {
	    // free pointer
	    msize_t	msize = msize_and_free & ~ SMALL_IS_FREE;
	    free_list_t	*free_head = ptr;
	    msize_t	*follower = (void *)FOLLOWING_SMALL_PTR(ptr, msize);
	    if (! msize) {
		malloc_printf("*** malloc[%d]: invariant broken for free block %p this msize=%d\n", getpid(), ptr, msize);
		return 0;
	    }
	    if (prev_free) {
		malloc_printf("*** malloc[%d]: invariant broken for %p (2 free in a row)\n", getpid(), ptr);
		return 0;
	    }
	    free_list_checksum(szone, free_head, __PRETTY_FUNCTION__);
	    if (free_head->previous && !(small_meta_header(free_head->previous)[0] & SMALL_IS_FREE)) {
		malloc_printf("*** malloc[%d]: invariant broken for %p (previous %p is not a free pointer)\n", getpid(), ptr, free_head->previous);
		return 0;
	    }
	    if (free_head->next && !(small_meta_header(free_head->next)[0] & SMALL_IS_FREE)) {
		malloc_printf("*** malloc[%d]: invariant broken for %p (next is not a free pointer)\n", getpid(), ptr);
		return 0;
	    }
	    if (SMALL_PREVIOUS_MSIZE(follower) != msize) {
		malloc_printf("*** malloc[%d]: invariant broken for small free %p followed by %p in region [%p-%p] (end marker incorrect) should be %d; in fact %d\n", getpid(), ptr, follower, SMALL_REGION_ADDRESS(*region), region_end, msize, SMALL_PREVIOUS_MSIZE(follower));
		return 0;
	    }
	    ptr = follower;
	    prev_free = SMALL_IS_FREE;
	}
    }
    return 1;
}

static kern_return_t
small_in_use_enumerator(task_t task, void *context, unsigned type_mask, vm_address_t region_address, unsigned short num_regions, size_t small_bytes_free_at_end, memory_reader_t reader, vm_range_recorder_t recorder) {
    small_region_t	*regions;
    unsigned		index = 0;
    vm_range_t		buffer[MAX_RECORDER_BUFFER];
    unsigned		count = 0;
    kern_return_t	err;
    err = reader(task, region_address, sizeof(small_region_t) * num_regions, (void **)&regions);
    if (err) return err;
    while (index < num_regions) {
	small_region_t	region = regions[index];
	vm_range_t	range = {SMALL_REGION_ADDRESS(region), SMALL_REGION_SIZE};
	// malloc_printf("Enumerating small ptrs for Region starting at %p\n", range.address);
	if (type_mask & MALLOC_ADMIN_REGION_RANGE_TYPE) {
	    vm_range_t	admin_range = {range.address + (1 << SMALL_BLOCKS_ALIGN), range.size - (1 << SMALL_BLOCKS_ALIGN)};
	    recorder(task, context, MALLOC_ADMIN_REGION_RANGE_TYPE, &admin_range, 1);
	}
	if (type_mask & (MALLOC_PTR_REGION_RANGE_TYPE | MALLOC_ADMIN_REGION_RANGE_TYPE)) {
	    vm_range_t	ptr_range = {range.address, 1 << SMALL_BLOCKS_ALIGN};
	    recorder(task, context, MALLOC_PTR_REGION_RANGE_TYPE, &ptr_range, 1);
	}
	if (type_mask & MALLOC_PTR_IN_USE_RANGE_TYPE) {
	    unsigned char	*mapped_region;
	    err = reader(task, range.address, range.size, (void **)&mapped_region);
	    if (err) return err;
	    msize_t	*block_header = (msize_t *)(mapped_region + (1 << SMALL_BLOCKS_ALIGN));
	    unsigned		block_index = 0;
	    unsigned		block_limit = NUM_SMALL_BLOCKS;
	    if (index == num_regions - 1)
		block_limit -= (small_bytes_free_at_end >> SHIFT_SMALL_QUANTUM);
	    while (block_index < block_limit) {
		msize_t	msize_and_free = block_header[block_index];
		msize_t	msize = msize_and_free & ~ SMALL_IS_FREE;
		if (! (msize_and_free & SMALL_IS_FREE)) {
		    // Block in use
		    buffer[count].address = range.address + (block_index << SHIFT_SMALL_QUANTUM);
		    buffer[count].size = msize << SHIFT_SMALL_QUANTUM;
		    count++;
		    if (count >= MAX_RECORDER_BUFFER) {
			recorder(task, context, MALLOC_PTR_IN_USE_RANGE_TYPE, buffer, count);
			count = 0;
		    }
		}
		block_index += msize;
	    }
	    // malloc_printf("End small region - count=%d\n", count);
	}
	index++;
    }
    if (count) {
	recorder(task, context, MALLOC_PTR_IN_USE_RANGE_TYPE, buffer, count);
    }
    return 0;
}

static INLINE void *
small_malloc_from_free_list(szone_t *szone, msize_t msize) {
    // Assumes locked
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    grain_t	grain = (msize <= NUM_SMALL_SLOTS) ? msize - 1 : NUM_SMALL_SLOTS - 1;
    unsigned	bitmap = szone->small_bitmap & ~ ((1 << grain) - 1);
    void	*ptr;
    msize_t	this_msize;
    if (!bitmap) goto try_small_from_end;
    grain = BITMAP32_FFS(bitmap) - 1;
    // first try the small grains
    free_list_t	**free_list;
    free_list_t	**limit = szone->small_free_list + NUM_SMALL_SLOTS - 1;
    free_list = szone->small_free_list + grain;
    while (free_list < limit) {
	// try bigger grains
	ptr = *free_list;
	if (ptr) {
	    free_list_t	*next;
	    next = ((free_list_t *)ptr)->next;
	    if (next) {
		next->previous = NULL;
		free_list_set_checksum(szone, next);
	    }
	    *free_list = next;
	    this_msize = small_meta_header(ptr)[0] & ~ SMALL_IS_FREE;
	    // malloc_printf("small_malloc_from_free_list: allocated from free list\n");
	    goto add_leftover_and_proceed;
	}
	free_list++;
    }
    // We now check the large grains for one that is big enough
    ptr = *free_list;
    while (ptr) {
	this_msize = small_meta_header(ptr)[0] & ~ SMALL_IS_FREE;
	if (this_msize >= msize) {
	    // malloc_printf("small_malloc_from_free_list: allocated from last free list\n");
	    small_free_list_remove_ptr(szone, ptr, this_msize);
	    goto add_leftover_and_proceed;
	}
	ptr = ((free_list_t *)ptr)->next;
    }
try_small_from_end:
    // Let's see if we can use szone->small_bytes_free_at_end
    // malloc_printf("Found nothing in free list small_bytes_free_at_end=%y\n", szone-> small_bytes_free_at_end);
    if (szone->small_bytes_free_at_end >= (msize << SHIFT_SMALL_QUANTUM)) {
	ptr = (void *)(SMALL_REGION_END(szone->small_regions[szone->num_small_regions-1]) - szone->small_bytes_free_at_end);
	szone->small_bytes_free_at_end -= msize << SHIFT_SMALL_QUANTUM;
	if (szone->small_bytes_free_at_end) {
	    // let's mark this block as in use to serve as boundary
	    small_meta_header(ptr + (msize << SHIFT_SMALL_QUANTUM))[0] = szone->small_bytes_free_at_end >> SHIFT_SMALL_QUANTUM;
	}
	this_msize = msize;
	goto return_small_alloc;
    }
    return NULL;
add_leftover_and_proceed:
    if (this_msize > msize) {
	msize_t	leftover_msize = this_msize - msize;
	void	*leftover_ptr = ptr + (msize << SHIFT_SMALL_QUANTUM);
#if DEBUG_MALLOC
	if (LOG(szone,ptr)) {
	    malloc_printf("In small_malloc_from_free_list(), adding leftover ptr=%p, this_msize=%d\n", ptr, this_msize);
	}
#endif
	small_free_list_add_ptr(szone, leftover_ptr, leftover_msize);
	msize_t	*meta_headers = small_meta_headers(leftover_ptr);
	msize_t	leftover_index = small_meta_index(leftover_ptr);
	small_meta_header_set_is_free(meta_headers, leftover_index, leftover_msize);
	this_msize = msize;
    }
return_small_alloc:
    szone->num_small_objects++;
    szone->num_bytes_in_small_objects += this_msize << SHIFT_SMALL_QUANTUM;
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In small_malloc_from_free_list(), ptr=%p, this_msize=%d, msize=%d\n", ptr, this_msize, msize);
    }
#endif
    small_meta_header(ptr)[0] = this_msize;
    return ptr;
}

static INLINE void *
small_malloc_should_clear(szone_t *szone, msize_t msize, boolean_t cleared_requested) {
    boolean_t	locked = 0;
    void	*ptr;
#if SMALL_CACHE
    ptr = (void *)szone->last_small_free;
    if ((((unsigned)ptr) & (SMALL_QUANTUM - 1)) == msize) {
	// we have a candidate - let's lock to make sure
	LOCK_AND_NOTE_LOCKED(szone, locked);
	if (ptr == (void *)szone->last_small_free) {
	    szone->last_small_free = NULL;
	    // malloc_printf("using last_small_free\n");
	    SZONE_UNLOCK(szone);
	    CHECK(szone, __PRETTY_FUNCTION__);
	    ptr = (void *)((unsigned)ptr & ~ (SMALL_QUANTUM - 1));
	    if (cleared_requested) {
		memset(ptr, 0, msize << SHIFT_SMALL_QUANTUM);
	    }
	    return ptr;
	}
	// malloc_printf("optimistic locking for last_small_free failed\n");
    }
#endif
    // Except in rare occasions where we need to add a new region, we are going to end up locking, so we might as well lock right away to avoid doing unnecessary optimistic probes
    if (!locked) LOCK_AND_NOTE_LOCKED(szone, locked);
    ptr = small_malloc_from_free_list(szone, msize);
    if (ptr) {
	SZONE_UNLOCK(szone);
	CHECK(szone, __PRETTY_FUNCTION__);
	if (cleared_requested) {
	    memset(ptr, 0, msize << SHIFT_SMALL_QUANTUM);
	}
	return ptr;
    }
    ptr = small_malloc_from_region_no_lock(szone, msize);
    // we don't clear because this freshly allocated space is pristine
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
    return ptr;
}

static INLINE void *
small_malloc_cleared_no_lock(szone_t *szone, msize_t msize) {
    // tries to allocate a small, cleared block
    // Assumes already locked
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    void	*ptr;
    ptr = small_malloc_from_free_list(szone, msize);
    if (ptr) {
	memset(ptr, 0, msize << SHIFT_SMALL_QUANTUM);
	return ptr;
    } else {
	ptr = small_malloc_from_region_no_lock(szone, msize);
	// we don't clear because this freshly allocated space is pristine
    }
    return ptr;
}

static INLINE void
free_small(szone_t *szone, void *ptr, small_region_t *small_region) {
    // ptr is known to be in small_region
    msize_t		msize_and_free;
    msize_and_free = small_meta_header(ptr)[0];
    if (msize_and_free & SMALL_IS_FREE) {
	szone_error(szone, "Object already freed being freed", ptr);
	return;
    }
    CHECK(szone, __PRETTY_FUNCTION__);
    // malloc_printf("%p[%x]\n", ptr, msize_and_free);
    SZONE_LOCK(szone);
#if SMALL_CACHE
    void	*ptr2 = szone->last_small_free;
    szone->last_small_free = (void *)(((unsigned)ptr) | msize_and_free);
    if (!ptr2) {
	// malloc_printf("stuffing last_small_free\n");
	SZONE_UNLOCK(szone);
	CHECK(szone, __PRETTY_FUNCTION__);
	return;
    }
    // malloc_printf("replacing previous last_small_free %p with %p\n", ptr2, szone->last_small_free);
    msize_and_free = (unsigned)ptr2 & (SMALL_QUANTUM - 1);
    ptr = (void *)(((unsigned)ptr2) & ~ (SMALL_QUANTUM - 1));
    small_region = small_region_for_ptr_no_lock(szone, ptr);
    if (!small_region) {
	szone_error(szone, "Double free (small cache)", ptr);
    }
#endif
    small_free_no_lock(szone, small_region, ptr, msize_and_free);
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
}

static void
print_small_free_list(szone_t *szone) {
    grain_t		grain = 0;
    malloc_printf("Small free sizes: ");
    while (grain < NUM_SMALL_SLOTS) {
	free_list_t	*ptr = szone->small_free_list[grain];
	if (ptr) {
	    malloc_printf("%s%y[%d]; ", (grain == NUM_SMALL_SLOTS-1) ? ">=" : "", (grain + 1) * SMALL_QUANTUM, free_list_count(ptr));
	}
	grain++;
    }
    malloc_printf("\n");
}

static void
print_small_region(szone_t *szone, boolean_t verbose, small_region_t *region, size_t bytes_at_end) {
    unsigned	counts[1024];
    unsigned	in_use = 0;
    vm_address_t	start = SMALL_REGION_ADDRESS(*region);
    vm_address_t	limit = SMALL_REGION_END(*region) - bytes_at_end;
    memset(counts, 0, 1024 * sizeof(unsigned));
    while (start < limit) {
	msize_t	msize_and_free = small_meta_header((void *)start)[0];
	msize_t	msize = msize_and_free & ~ SMALL_IS_FREE;
	if (!(msize_and_free & SMALL_IS_FREE)) {
	    // block in use
	    if (msize < 1024) counts[msize]++;
	    in_use++;
	}
	start += msize << SHIFT_SMALL_QUANTUM;
    }
    malloc_printf("Small region [%p-%p, %y]\tIn_use=%d ", SMALL_REGION_ADDRESS(*region), SMALL_REGION_END(*region), (int)SMALL_REGION_SIZE, in_use);
    if (bytes_at_end) malloc_printf("Untouched=%y ", bytes_at_end);
    if (verbose && in_use) {
	unsigned	ci = 0;
	malloc_printf("\n\tSizes in use: "); 
	while (ci < 1024) {
	    if (counts[ci]) {
		malloc_printf("%d[%d] ", ci << SHIFT_SMALL_QUANTUM, counts[ci]);
	    }
	    ci++;
	}
    }
    malloc_printf("\n");
}

static boolean_t
small_free_list_check(szone_t *szone, grain_t grain) {
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    unsigned	count = 0;
    free_list_t	*ptr = szone->small_free_list[grain];
    free_list_t	*previous = NULL;
    while (ptr) {
	msize_t	msize_and_free = small_meta_header(ptr)[0];
	count++;
	if (!(msize_and_free & SMALL_IS_FREE)) {
	    malloc_printf("*** malloc[%d]: In-use ptr in free list grain=%d count=%d ptr=%p\n", getpid(), grain, count, ptr);
	    return 0;
	}
	if (((unsigned)ptr) & (SMALL_QUANTUM - 1)) {
	    malloc_printf("*** malloc[%d]: Unaligned ptr in free list grain=%d  count=%d ptr=%p\n", getpid(), grain, count, ptr);
	    return 0;
	}
	if (!small_region_for_ptr_no_lock(szone, ptr)) {
	    malloc_printf("*** malloc[%d]: Ptr not in szone grain=%d  count=%d ptr=%p\n", getpid(), grain, count, ptr);
	    return 0;
	}
	free_list_checksum(szone, ptr, __PRETTY_FUNCTION__);
	if (ptr->previous != previous) {
	    malloc_printf("*** malloc[%d]: Previous incorrectly set grain=%d  count=%d ptr=%p\n", getpid(), grain, count, ptr);
	    return 0;
	}
	previous = ptr;
	ptr = ptr->next;
    }
    return 1;
}

/*********************	LARGE ENTRY UTILITIES	************************/

#if DEBUG_MALLOC

static void
large_debug_print(szone_t *szone) {
    unsigned	num_large_entries = szone->num_large_entries;
    unsigned	index = num_large_entries;
    while (index--) {
	large_entry_t	*range = szone->large_entries + index;
	large_entry_t	entry = *range;
	if (!LARGE_ENTRY_IS_EMPTY(entry)) {
	    malloc_printf("%d: %p(%y);  ", index, LARGE_ENTRY_ADDRESS(entry), LARGE_ENTRY_SIZE(entry));
	}
    }
    malloc_printf("\n");
}
#endif

static large_entry_t *
large_entry_for_pointer_no_lock(szone_t *szone,
  const void *ptr) {
    // result only valid during a lock
    unsigned	num_large_entries = szone->num_large_entries;
    unsigned	hash_index;
    unsigned	index;
    if (!num_large_entries) return NULL;
    hash_index = ((unsigned)ptr >> vm_page_shift) % num_large_entries;
    index = hash_index;
    do {
	large_entry_t	*range = szone->large_entries + index;
	large_entry_t	entry = *range;
	if (LARGE_ENTRY_MATCHES(entry, ptr)) return range;
	if (LARGE_ENTRY_IS_EMPTY(entry)) return NULL; // end of chain
	index++; if (index == num_large_entries) index = 0;
    } while (index != hash_index);
    return NULL;
}

static void
large_entry_insert_no_lock(szone_t *szone, large_entry_t range) {
    unsigned	num_large_entries = szone->num_large_entries;
    unsigned	hash_index = (range.address_and_num_pages >> vm_page_shift)
      % num_large_entries;
    unsigned	index = hash_index;
//    malloc_printf("Before insertion of %p\n", LARGE_ENTRY_ADDRESS(range));
    do {
	large_entry_t	*entry = szone->large_entries + index;
	if (LARGE_ENTRY_IS_EMPTY(*entry)) {
	    *entry = range;
	    return; // end of chain
	}
	index++; if (index == num_large_entries) index = 0;
    } while (index != hash_index);
}

static INLINE void
large_entries_rehash_after_entry_no_lock(szone_t *szone,
  large_entry_t *entry) {
    unsigned	num_large_entries = szone->num_large_entries;
    unsigned	hash_index = entry - szone->large_entries;
    unsigned	index = hash_index;
    do {
	large_entry_t	range;
	index++; if (index == num_large_entries) index = 0;
	range = szone->large_entries[index];
	if (LARGE_ENTRY_IS_EMPTY(range)) return;
	szone->large_entries[index].address_and_num_pages = 0;
	large_entry_insert_no_lock(szone, range); // this will reinsert in the
						  // proper place
    } while (index != hash_index);
}

static INLINE large_entry_t *
large_entries_alloc_no_lock(szone_t *szone,
  unsigned num) {
    size_t	size = num * sizeof(large_entry_t);
    boolean_t	is_vm_allocation = size >= LARGE_THRESHOLD;
    if (is_vm_allocation) {
	// Note that we allocate memory (via a system call) under a spin lock
	// That is certainly evil, however it's very rare in the lifetime of a process
	// The alternative would slow down the normal case
	return (void *)allocate_pages(szone, round_page(size), 0, 0, VM_MAKE_TAG(VM_MEMORY_MALLOC_LARGE));
    } else {
	return small_malloc_cleared_no_lock(szone, (size + SMALL_QUANTUM - 1) >> SHIFT_SMALL_QUANTUM);
    }
}

static void
large_entries_free_no_lock(szone_t *szone, large_entry_t *entries, unsigned num, vm_range_t *range_to_deallocate) {
    // returns range to deallocate
    size_t	size = num * sizeof(large_entry_t);
    boolean_t	is_vm_allocation = size >= LARGE_THRESHOLD;
    // malloc_printf("In large_entries_free_no_lock %d %d\n", num, is_vm_allocation);
    if (is_vm_allocation) {
	range_to_deallocate->address = (vm_address_t)entries;
	range_to_deallocate->size = round_page(size);
    } else {
	range_to_deallocate->size = 0;
	small_region_t	*region = small_region_for_ptr_no_lock(szone, entries);
	msize_t		msize_and_free = small_meta_header(entries)[0];
	if (msize_and_free & SMALL_IS_FREE) {
	    szone_error(szone, "Object already freed being freed", entries);
	    return;
	}
	small_free_no_lock(szone, region, entries, msize_and_free);
    }
}

static void
large_entries_grow_no_lock(szone_t *szone, vm_range_t *range_to_deallocate) {
    // sets range_to_deallocate
    unsigned		old_num_entries = szone->num_large_entries;
    large_entry_t	*old_entries = szone->large_entries;
    unsigned		new_num_entries = (old_num_entries) ? old_num_entries
      * 2 + 1 : 63; // always an odd number for good hashing
    large_entry_t	*new_entries = large_entries_alloc_no_lock(szone, new_num_entries);
    unsigned		index = old_num_entries;
    szone->num_large_entries = new_num_entries;
    szone->large_entries = new_entries;
    // malloc_printf("_grow_large_entries old_num_entries=%d new_num_entries=%d %p\n", old_num_entries, new_num_entries, old_entries);
    while (index--) {
	large_entry_t	oldRange = old_entries[index];
	if (!LARGE_ENTRY_IS_EMPTY(oldRange)) {
	    large_entry_insert_no_lock(szone, oldRange);
	}
    }
    if (old_entries) {
	large_entries_free_no_lock(szone, old_entries, old_num_entries, range_to_deallocate);
    } else {
	range_to_deallocate->size = 0;
    }
}

static vm_range_t
large_free_no_lock(szone_t *szone, large_entry_t *entry) {
    // frees the specific entry in the size table
    // returns a range to truly deallocate
    vm_range_t		range;
    range.address = LARGE_ENTRY_ADDRESS(*entry);
    range.size = LARGE_ENTRY_SIZE(*entry);
    szone->num_large_objects_in_use --;
    szone->num_bytes_in_large_objects -= range.size;
    if (szone->debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES) {
	protect(szone, range.address, range.size, VM_PROT_READ | VM_PROT_WRITE,
	  szone->debug_flags);
	range.address -= 1 << vm_page_shift;
	range.size += 2 * (1 << vm_page_shift);
    }
//    malloc_printf("Entry is %p=%d; cache is %p ; found=%p\n", entry,
//      entry-szone->large_entries, szone->large_entries,
//      large_entry_for_pointer_no_lock(szone, (void *)range.address));
    entry->address_and_num_pages = 0;
    large_entries_rehash_after_entry_no_lock(szone, entry);
#if DEBUG_MALLOC
    if (large_entry_for_pointer_no_lock(szone, (void *)range.address)) {
	malloc_printf("*** malloc[%d]: Freed entry %p still in use; "
	  "num_large_entries=%d\n", getpid(), range.address,
	  szone->num_large_entries);
	large_debug_print(szone);
	szone_sleep();
    }
#endif
    return range;
}

static INLINE boolean_t
try_realloc_small_in_place(szone_t *szone, void *ptr, size_t old_size, size_t new_size) {
    // returns 1 on success
    msize_t	*meta_headers = small_meta_headers(ptr);
    msize_t	index = small_meta_index(ptr);
    msize_t	old_msize = old_size >> SHIFT_SMALL_QUANTUM;
    msize_t	new_msize = (new_size + SMALL_QUANTUM - 1) >> SHIFT_SMALL_QUANTUM;
    void	*next_block = (char *)ptr + old_size;
    msize_t	next_index = index + old_msize;
    if (next_index >= NUM_SMALL_BLOCKS) {
	// malloc_printf("try_realloc_small_in_place can't take place at end %p %d %d %d\n", ptr, old_size, new_size, next_index);
	return 0;
    }
#if DEBUG_MALLOC
    if ((vm_address_t)next_block & (SMALL_QUANTUM - 1)) {
	szone_error(szone, "Internal invariant broken in realloc(next_block)", next_block);
    }
    if (meta_headers[index] != old_msize) malloc_printf("*** try_realloc_small_in_place incorrect old %d %d\n", meta_headers[index], old_msize);
#endif
    SZONE_LOCK(szone);
    // If the next block is free, we coalesce
    msize_t	next_msize_and_free;
    msize_t	next_msize;
    next_msize_and_free = meta_headers[next_index];
    next_msize = next_msize_and_free & ~ SMALL_IS_FREE;
    if (!(next_msize_and_free & SMALL_IS_FREE) || (old_msize + next_msize < new_msize)) {
	SZONE_UNLOCK(szone);
	return 0;
    }
    // malloc_printf("Small realloc in place for %p;  current msize=%db(%d) next=%p next_msize=%d wanted=%db(%d)\n", ptr, old_size, meta_headers[index], next_block, next_msize, new_size, new_msize);
    small_free_list_remove_ptr(szone, next_block, next_msize);
    small_meta_header_set_middle(meta_headers, next_index);
    msize_t	leftover_msize = old_msize + next_msize - new_msize;
    if (leftover_msize) {
	void	*leftover = ptr + (new_msize << SHIFT_SMALL_QUANTUM);
	// malloc_printf("Leftover in realloc in place %p msize=%d\n", leftover, leftover_msize);
	small_free_list_add_ptr(szone, leftover, leftover_msize);
	msize_t	leftover_index = index + new_msize;
	small_meta_header_set_is_free(meta_headers, leftover_index, leftover_msize);
    }
#if DEBUG_MALLOC
    if ((new_msize << SHIFT_SMALL_QUANTUM) >= LARGE_THRESHOLD) {
	malloc_printf("*** Realloc in place for %p exceeded msize=%d\n", new_msize);
    }
#endif
    small_meta_header_set_in_use(meta_headers, index, new_msize);
#if DEBUG_MALLOC
    if (LOG(szone,ptr)) {
	malloc_printf("In szone_realloc(), ptr=%p, msize=%d\n", ptr, small_meta_header(ptr)[0]);
    }
#endif
    szone->num_bytes_in_small_objects += (new_msize - old_msize) << SHIFT_SMALL_QUANTUM;
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
//    malloc_printf("Extended ptr %p for realloc old=%d desired=%d new=%d "
//      "leftover=%d\n", ptr, (unsigned)old_size, (unsigned)new_size,
//      (unsigned)szone_size(szone, ptr), leftover_msize << SHIFT_SMALL_QUANTUM);
    return 1;
}

static kern_return_t
large_in_use_enumerator(task_t task, void *context,
  unsigned type_mask, vm_address_t large_entries_address, unsigned num_entries,
  memory_reader_t reader, vm_range_recorder_t recorder) {
    unsigned		index = 0;
    vm_range_t		buffer[MAX_RECORDER_BUFFER];
    unsigned		count = 0;
    large_entry_t	*entries;
    kern_return_t	err;
    err = reader(task, large_entries_address,
      sizeof(large_entry_t) * num_entries, (void **)&entries);
    if (err) return err;
    index = num_entries;
    if ((type_mask & MALLOC_ADMIN_REGION_RANGE_TYPE)
      && (num_entries * sizeof(large_entry_t) >= LARGE_THRESHOLD)) {
	vm_range_t	range;
	range.address = large_entries_address;
	range.size = round_page(num_entries * sizeof(large_entry_t));
	recorder(task, context, MALLOC_ADMIN_REGION_RANGE_TYPE, &range, 1);
    }
    if (type_mask & (MALLOC_PTR_IN_USE_RANGE_TYPE
      | MALLOC_PTR_REGION_RANGE_TYPE))
      while (index--) {
	large_entry_t	entry = entries[index];
	if (!LARGE_ENTRY_IS_EMPTY(entry)) {
	    vm_range_t	range;
	    range.address = LARGE_ENTRY_ADDRESS(entry);
	    range.size = LARGE_ENTRY_SIZE(entry);
	    buffer[count++] = range;
	    if (count >= MAX_RECORDER_BUFFER) {
		recorder(task, context, MALLOC_PTR_IN_USE_RANGE_TYPE
		  | MALLOC_PTR_REGION_RANGE_TYPE, buffer, count);
		count = 0;
	    }
	}
    }
    if (count) {
	recorder(task, context, MALLOC_PTR_IN_USE_RANGE_TYPE
	  | MALLOC_PTR_REGION_RANGE_TYPE, buffer, count);
    }
    return 0;
}

/*********************	HUGE ENTRY UTILITIES	************************/

static huge_entry_t *
huge_entry_for_pointer_no_lock(szone_t *szone,
  const void *ptr) {
    unsigned	index = szone->num_huge_entries;
    while (index--) {
	huge_entry_t	*huge = szone->huge_entries + index;
	if (huge->address == (vm_address_t)ptr) return huge;
    }
    return NULL;
}

static boolean_t
huge_entry_append(szone_t *szone, huge_entry_t huge) {
    // We do a little dance with locking because doing allocation (even in the
    // default szone) may cause something to get freed in this szone, with a
    // deadlock
    // Returns 1 on success
    huge_entry_t	*new_huge_entries = NULL;
    SZONE_LOCK(szone);
    while (1) {
	unsigned	num_huge_entries;
	num_huge_entries = szone->num_huge_entries;
	SZONE_UNLOCK(szone);
//	malloc_printf("In huge_entry_append currentEntries=%d\n", num_huge_entries);
	if (new_huge_entries) szone_free(szone, new_huge_entries);
	new_huge_entries = szone_malloc(szone, (num_huge_entries + 1) * sizeof(huge_entry_t));
	if (new_huge_entries == NULL) return 0;
	SZONE_LOCK(szone);
	if (num_huge_entries == szone->num_huge_entries) {
	    // No change - our malloc still applies
	    huge_entry_t	*old_huge_entries = szone->huge_entries;
	    if (num_huge_entries) {
		memcpy(new_huge_entries, old_huge_entries, num_huge_entries * sizeof(huge_entry_t));
	    }
	    new_huge_entries[szone->num_huge_entries++] = huge;
	    szone->huge_entries = new_huge_entries;
	    SZONE_UNLOCK(szone);
	    szone_free(szone, old_huge_entries);
//	    malloc_printf("Done huge_entry_append now=%d\n", szone->num_huge_entries);
	    return 1;
	}
	// try again!
    }
}

static kern_return_t
huge_in_use_enumerator(task_t task, void *context,
  unsigned type_mask, vm_address_t huge_entries_address, unsigned num_entries,
  memory_reader_t reader, vm_range_recorder_t recorder) {
    huge_entry_t	*entries;
    kern_return_t	err;
    err = reader(task, huge_entries_address, sizeof(huge_entry_t) * num_entries,
      (void **)&entries);
    if (err) return err;
    if (num_entries) {
	recorder(task, context,
	  MALLOC_PTR_IN_USE_RANGE_TYPE | MALLOC_PTR_REGION_RANGE_TYPE, entries,
	  num_entries);
    }
    return 0;
}

static void *
large_and_huge_malloc(szone_t *szone, unsigned num_pages) {
    vm_address_t	addr = 0;
    vm_range_t		range_to_deallocate;
    if (!num_pages) num_pages = 1; // minimal allocation size for this szone
//    malloc_printf("In large_and_huge_malloc for %y\n", num_pages * (1 << vm_page_shift));
    range_to_deallocate.size = 0;
    if (num_pages >= (1 << vm_page_shift)) {
	huge_entry_t	huge;
	huge.size = num_pages << vm_page_shift;
	addr = allocate_pages(szone, huge.size, 0, szone->debug_flags, VM_MAKE_TAG(VM_MEMORY_MALLOC_HUGE));
	if (!addr) return NULL;
	huge.address = addr;
	if (! huge_entry_append(szone, huge)) return NULL;
	SZONE_LOCK(szone);
	szone->num_bytes_in_huge_objects += huge.size;
    } else {
	vm_size_t		size = num_pages << vm_page_shift;
	large_entry_t		entry;
	addr = allocate_pages(szone, size, 0, szone->debug_flags, VM_MAKE_TAG(VM_MEMORY_MALLOC_LARGE));
#if DEBUG_MALLOC
	if (LOG(szone, addr)) malloc_printf("In szone_malloc true large allocation at %p for %y\n", (void *)addr, size);
#endif
	SZONE_LOCK(szone);
	if (!addr) {
	    SZONE_UNLOCK(szone);
	    return NULL;
	}
#if DEBUG_MALLOC
	if (large_entry_for_pointer_no_lock(szone, (void *)addr)) {
	    malloc_printf("Freshly allocated is already in use: %p\n", addr);
	    large_debug_print(szone);
	    szone_sleep();
	}
#endif
	if ((szone->num_large_objects_in_use + 1) * 4 > szone->num_large_entries) {
	    // density of hash table too high; grow table
	    // we do that under lock to avoid a race
	    // malloc_printf("In szone_malloc growing hash table current=%d\n", szone->num_large_entries);
	    large_entries_grow_no_lock(szone, &range_to_deallocate);
	}
//	malloc_printf("Inserting large entry (%p, %y)\n", addr, num_pages * (1 << vm_page_shift));
	entry.address_and_num_pages = addr | num_pages;
#if DEBUG_MALLOC
	if (large_entry_for_pointer_no_lock(szone, (void *)addr)) {
	    malloc_printf("Entry about to be added already in use: %p\n",
	      addr);
	    large_debug_print(szone);
	    szone_sleep();
	}
#endif
	large_entry_insert_no_lock(szone, entry);
#if DEBUG_MALLOC
	if (!large_entry_for_pointer_no_lock(szone, (void *)addr)) {
	    malloc_printf("Can't find entry just added\n");
	    large_debug_print(szone);
	    szone_sleep();
	}
#endif
//	malloc_printf("Inserted large entry (%p, %d pages)\n", addr,
//	  num_pages);
	szone->num_large_objects_in_use ++;
	szone->num_bytes_in_large_objects += size;
    }
    SZONE_UNLOCK(szone);
    if (range_to_deallocate.size) {
	deallocate_pages(szone, range_to_deallocate.address, range_to_deallocate.size, 0); // we deallocate outside the lock
	// malloc_printf("Deallocated large entries %d\n", range_to_deallocate.size);
    }
    return (void *)addr;
}

static INLINE void
free_large_or_huge(szone_t *szone, void *ptr) {
    // We have established ptr is page-aligned and not tiny nor small
    large_entry_t	*entry;
    vm_range_t		vm_range_to_deallocate;
    huge_entry_t	*huge;
    SZONE_LOCK(szone);
    entry = large_entry_for_pointer_no_lock(szone, ptr);
    if (entry) {
//	malloc_printf("Ready for deallocation [%p-%y]\n", LARGE_ENTRY_ADDRESS(*entry), LARGE_ENTRY_SIZE(*entry));
	vm_range_to_deallocate = large_free_no_lock(szone, entry);
#if DEBUG_MALLOC
	if (large_entry_for_pointer_no_lock(szone, ptr)) {
	    malloc_printf("*** malloc[%d]: Just after freeing %p still in use num_large_entries=%d\n", getpid(), ptr, szone->num_large_entries);
	    large_debug_print(szone);
	    szone_sleep();
	}
#endif
    } else if ((huge = huge_entry_for_pointer_no_lock(szone, ptr))) {
	vm_range_to_deallocate = *huge;
	*huge = szone->huge_entries[--szone->num_huge_entries]; // last entry fills that spot
	szone->num_bytes_in_huge_objects -= vm_range_to_deallocate.size;
    } else {
#if DEBUG_MALLOC
	large_debug_print(szone);
#endif
	szone_error(szone, "Pointer being freed was not allocated", ptr);
	return;
    }
    SZONE_UNLOCK(szone); // we release the lock asap
    CHECK(szone, __PRETTY_FUNCTION__);
    // we deallocate_pages, including guard pages
    if (vm_range_to_deallocate.address) {
//	malloc_printf("About to deallocate %p size %y\n", vm_range_to_deallocate.address, vm_range_to_deallocate.size);
#if DEBUG_MALLOC
	if (large_entry_for_pointer_no_lock(szone,
	  (void *)vm_range_to_deallocate.address)) {
	    malloc_printf("*** malloc[%d]: Invariant broken: %p still in use num_large_entries=%d\n", getpid(), vm_range_to_deallocate.address, szone->num_large_entries);
	    large_debug_print(szone);
	    szone_sleep();
	}
#endif
	deallocate_pages(szone, vm_range_to_deallocate.address, vm_range_to_deallocate.size, 0);
    }
}

static INLINE int
try_realloc_large_or_huge_in_place(szone_t *szone, void *ptr, size_t old_size, size_t new_size) {
    vm_address_t	addr = (vm_address_t)ptr + old_size;
    large_entry_t	*entry;
    kern_return_t	err;
#if DEBUG_MALLOC
    if (old_size != ((old_size >> vm_page_shift) << vm_page_shift)) {
	malloc_printf("*** old_size is %d\n", old_size);
    }
#endif
//    malloc_printf("=== Trying (1) to extend %p from %d to %d\n", ptr, old_size, new_size);
    SZONE_LOCK(szone);
    entry = large_entry_for_pointer_no_lock(szone, (void *)addr);
    SZONE_UNLOCK(szone);
    if (entry) {
	return 0; // large pointer already exist in table - extension is not going to work
    }
    new_size = round_page(new_size);
//    malloc_printf("=== Trying (2) to extend %p from %d to %d\n", ptr, old_size, new_size);
    /*
     * Ask for allocation at a specific address, and mark as realloc
     * to request coalescing with previous realloc'ed extensions.
     */
    err = vm_allocate(mach_task_self(), &addr, new_size - old_size, VM_MAKE_TAG(VM_MEMORY_REALLOC));
    if (err != KERN_SUCCESS) {
	return 0;
    }
    SZONE_LOCK(szone);
    /*
     * If the new size is still under the large/huge threshold, we can just
     * extend the existing large block.
     *
     * Note: this logic is predicated on the understanding that an allocated
     * block can never really shrink, so that the new size will always be 
     * larger than the old size.
     */
    if ((new_size >> vm_page_shift) < (1 << vm_page_shift)) {
	/* extend existing large entry */
	entry = large_entry_for_pointer_no_lock(szone, ptr);
	if (!entry) {
	    szone_error(szone, "large entry reallocated is not properly in table", ptr);
	    /* XXX will cause fault on next reference to entry */
	}
	entry->address_and_num_pages = (vm_address_t)ptr | (new_size >> vm_page_shift);
	szone->num_bytes_in_large_objects += new_size - old_size;
    } else if ((old_size >> vm_page_shift) >= (1 << vm_page_shift)) {
	/* extend existing huge entry */
	huge_entry_t	*huge_entry = huge_entry_for_pointer_no_lock(szone, ptr);
	if (!huge_entry) {
	    szone_error(szone, "huge entry reallocated is not properly in table", ptr);
	    /* XXX will cause fault on next reference to huge_entry */
	}
	huge_entry->size = new_size;
	szone->num_bytes_in_huge_objects += new_size - old_size;
    } else {
	/* need to convert large entry to huge entry */
	huge_entry_t huge;

	/* release large entry, note we still have the VM allocation */
	entry = large_entry_for_pointer_no_lock(szone, ptr);
	large_entry_t	saved_entry = *entry; // in case we need to put it back
	large_free_no_lock(szone, entry);
	szone->num_bytes_in_large_objects -= old_size;

	/* and get a huge entry */
	huge.address = (vm_address_t)ptr;
	huge.size = new_size;	/* fix up size */
	SZONE_UNLOCK(szone);
	if (huge_entry_append(szone, huge)) {
	    szone->num_bytes_in_huge_objects += new_size;
	    return 1; // success!
	}
	SZONE_LOCK(szone);
	// we leak memory (the extra space appended) but data structures are correct
	large_entry_insert_no_lock(szone, saved_entry); // this will reinsert the large entry
    }
//    malloc_printf("=== Successfully reallocated at end of %p from %d to %d\n", ptr, old_size, new_size);
    SZONE_UNLOCK(szone); // we release the lock asap
    return 1;
}

/*********************	Zone call backs	************************/

static void
szone_free(szone_t *szone, void *ptr) {
    // malloc_printf("szone_free(%p)\n", ptr);
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) malloc_printf("In szone_free with %p\n", ptr);
#endif
    if (!ptr) return;
    if ((vm_address_t)ptr & (TINY_QUANTUM - 1)) {
	szone_error(szone, "Non-aligned pointer being freed", ptr);
	return;
    }
    // try a tiny pointer
    tiny_region_t	*tiny_region = tiny_region_for_ptr_no_lock(szone, ptr);
    if (tiny_region) {
	free_tiny(szone, ptr, tiny_region);
	return;
    }
    if ((vm_address_t)ptr & (SMALL_QUANTUM - 1)) {
	szone_error(szone, "Non-aligned pointer being freed (2)", ptr);
	return;
    }
    // try a small pointer
    small_region_t		*small_region = small_region_for_ptr_no_lock(szone, ptr);
    if (small_region) {
	free_small(szone, ptr, small_region);
	return;
    }
    if (((unsigned)ptr) & ((1 << vm_page_shift) - 1)) {
	szone_error(szone, "Non-page-aligned, non-allocated pointer being freed", ptr);
	return;
    }
    free_large_or_huge(szone, ptr);
}

static INLINE void *
szone_malloc_should_clear(szone_t *szone, size_t size, boolean_t cleared_requested) {
    void	*ptr;
    if (size <= 31*TINY_QUANTUM) {
	// think tiny
	msize_t		msize = (size + TINY_QUANTUM - 1) >> SHIFT_TINY_QUANTUM;
	if (! msize) msize = 1;
	ptr = tiny_malloc_should_clear(szone, msize, cleared_requested);
    } else if (!((szone->debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES) && PROTECT_SMALL) && (size < LARGE_THRESHOLD)) {
	// think small
	msize_t		msize = (size + SMALL_QUANTUM - 1) >> SHIFT_SMALL_QUANTUM;
	if (! msize) msize = 1;
	ptr = small_malloc_should_clear(szone, msize, cleared_requested);
    } else {
	unsigned		num_pages;
	num_pages = round_page(size) >> vm_page_shift;
	ptr = large_and_huge_malloc(szone, num_pages);
    }
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) malloc_printf("szone_malloc returned %p\n", ptr);
#endif
    return ptr;
}

static void *
szone_malloc(szone_t *szone, size_t size) {
    // malloc_printf("szone_malloc(%d)\n", size);
    void *ptr = szone_malloc_should_clear(szone, size, 0);
    // malloc_printf("szone_malloc(%d) -> %p %d\n", size, ptr, malloc_size(ptr));
    return ptr;
}

static void *
szone_calloc(szone_t *szone, size_t num_items, size_t size) {
    // malloc_printf("szone_calloc(%d,%d)\n", num_items, size);
    void	*ptr = szone_malloc_should_clear(szone, num_items * size, 1);
    // malloc_printf("szone_calloc(%d,%d) -> %p\n", num_items, size, ptr);
    return ptr;
}

static void *
szone_valloc(szone_t *szone, size_t size) {
    void	*ptr;
    unsigned	num_pages;
    num_pages = round_page(size) >> vm_page_shift;
    ptr = large_and_huge_malloc(szone, num_pages);
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) malloc_printf("szone_valloc returned %p\n", ptr);
#endif
    return ptr;
}

static size_t
szone_size(szone_t *szone, const void *ptr) {
    size_t		size = 0;
    large_entry_t	*entry;
    huge_entry_t	*huge;
    // malloc_printf("szone_size(%p)\n", ptr);
    if (!ptr) return 0;
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) {
	malloc_printf("In szone_size for %p (szone=%p)\n", ptr, szone);
    }
#endif
    if ((vm_address_t)ptr & (TINY_QUANTUM - 1)) return 0;
    // Try tiny
    tiny_region_t	*tiny_region = tiny_region_for_ptr_no_lock(szone, ptr);
    if (tiny_region) {
	// this is indeed a valid pointer
	boolean_t	is_free;
	msize_t		msize = get_tiny_meta_header(ptr, &is_free);
	return (is_free) ? 0 : msize << SHIFT_TINY_QUANTUM;
    }
    if ((vm_address_t)ptr & (SMALL_QUANTUM - 1)) return 0;
    // Try a small
    small_region_t		*small_region = small_region_for_ptr_no_lock(szone, ptr);
    if (small_region) {
	// this is indeed a valid pointer
	msize_t		msize_and_free = small_meta_header(ptr)[0];
	return (msize_and_free & SMALL_IS_FREE) ? 0 : msize_and_free << SHIFT_SMALL_QUANTUM;
    }
    if (((unsigned)ptr) & ((1 << vm_page_shift) - 1)) {
	// malloc_printf("Object %p not found in szone_size\n", ptr);
	return 0;
    }
    SZONE_LOCK(szone);
    entry = large_entry_for_pointer_no_lock(szone, ptr);
    if (entry) {
	size = LARGE_ENTRY_SIZE(*entry);
    } else if ((huge = huge_entry_for_pointer_no_lock(szone, ptr))) {
	size = huge->size;
    }
    SZONE_UNLOCK(szone); 
    // malloc_printf("szone_size for large/huge %p returned %d\n", ptr, (unsigned)size);
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) {
	malloc_printf("szone_size for %p returned %d\n", ptr, (unsigned)size);
    }
#endif
    return size;
}

static void *
szone_realloc(szone_t *szone, void *ptr, size_t new_size) {
    size_t		old_size = 0;
    void		*new_ptr;
    // malloc_printf("szone_realloc(%p,%d)\n", ptr, new_size);
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) {
	malloc_printf("In szone_realloc for %p, %d\n", ptr, (unsigned)new_size);
    }
#endif
    if (!ptr) {
	ptr = szone_malloc(szone, new_size);
	// malloc_printf("szone_realloc(%p,%d) -> %p\n", ptr, new_size, ptr);
	return ptr;
    }
    old_size = szone_size(szone, ptr);
    if (!old_size) {
	szone_error(szone, "Pointer being reallocated was not allocated", ptr);
	return NULL;
    }
    /* we never shrink an allocation */
    if (old_size >= new_size) return ptr;
    if ((new_size + TINY_QUANTUM - 1) <= 31 * TINY_QUANTUM) {
	// We now try to realloc in place
	if (try_realloc_tiny_in_place(szone, ptr, old_size, new_size)) {
	    // malloc_printf("szone_realloc(%p,%d) -> %p\n", ptr, new_size, ptr);
	    return ptr;
	}
    } else if (!((szone->debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES) && PROTECT_SMALL) && ((new_size + SMALL_QUANTUM - 1) < LARGE_THRESHOLD) && (old_size > 31 * TINY_QUANTUM)) {
	// We now try to realloc in place
	if (try_realloc_small_in_place(szone, ptr, old_size, new_size)) {
	    // malloc_printf("szone_realloc(%p,%d) small in place -> %p\n", ptr, new_size, ptr);
	    return ptr;
	}
    } else if (!((szone->debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES) && PROTECT_SMALL) && (old_size > LARGE_THRESHOLD)) {
	if (try_realloc_large_or_huge_in_place(szone, ptr, old_size, new_size)) {
	    return ptr;
	}
    }
    new_ptr = szone_malloc(szone, new_size);
    if (new_ptr == NULL) return NULL;
    if ((old_size > VM_COPY_THRESHOLD) && (new_size > VM_COPY_THRESHOLD)) {
	// we know everything is page-aligned try vm_copy
	kern_return_t	err = 0;
	err = vm_copy(mach_task_self(), (vm_address_t)ptr, old_size, (vm_address_t)new_ptr);
	if (err) {
	    szone_error(szone, "Can't vm_copy region", ptr);
	}
    } else {
	memcpy(new_ptr, ptr, old_size);
    }
    szone_free(szone, ptr);
#if DEBUG_MALLOC
    if (LOG(szone, ptr)) {
	malloc_printf("szone_realloc returned %p for %d\n", new_ptr, (unsigned)new_size);
    }
#endif
    // malloc_printf("szone_realloc(%p,%d) -> %p\n", ptr, new_size, new_ptr);
    return new_ptr;
}

unsigned
szone_batch_malloc(szone_t *szone, size_t size, void **results, unsigned count) {
    // given a size, returns pointers capable of holding that size
    // returns the number of pointers allocated
    // may return 0 - this function will do best attempts, but just that
    // malloc_printf("In szone_batch_malloc(%d, %d)\n", size, count);
    if (size > 31*TINY_QUANTUM) return 0; // only bother implementing this for tiny
    msize_t		msize = (size + TINY_QUANTUM - 1) >> SHIFT_TINY_QUANTUM;
    if (! msize) msize = 1;
    size_t		chunk_size = msize << SHIFT_TINY_QUANTUM;
    unsigned	found = 0;
    CHECK(szone, __PRETTY_FUNCTION__);
    SZONE_LOCK(szone); // might as well lock right here to avoid concurrency issues
    free_list_t	**free_list = szone->tiny_free_list + msize - 1;
    free_list_t	*ptr = *free_list;
    while (found < count) {
	if (!ptr) break;
	*results++ = ptr; found++;
	set_tiny_meta_header_in_use(ptr, msize);
	ptr = ((free_list_t *)ptr)->next;
    }
    if (ptr) {
	((free_list_t *)ptr)->previous = NULL;
	free_list_set_checksum(szone, (free_list_t *)ptr);
    }
    *free_list = (void *)ptr;
    // Note that we could allocate from the free lists for larger msize
    // But that may un-necessarily fragment - so we might as well let the client do that
    // We could also allocate from szone->tiny_bytes_free_at_end
    // But that means we'll "eat-up" the untouched area faster, increasing the working set
    // So we just return what we have and just that
    szone->num_tiny_objects += found;
    szone->num_bytes_in_tiny_objects += chunk_size * found;
    SZONE_UNLOCK(szone);
    // malloc_printf("In szone_batch_malloc(%d, %d) -> %d\n", size, count, found);
    return found;
}

void
szone_batch_free(szone_t *szone, void **to_be_freed, unsigned count) {
    // frees all the pointers in to_be_freed
    // note that to_be_freed may be overwritten during the process
    if (!count) return;
    // malloc_printf("Freeing %d items\n", count);
    unsigned	cc = 0;
    CHECK(szone, __PRETTY_FUNCTION__);
    SZONE_LOCK(szone);
    while (cc < count) {
	void	*ptr = to_be_freed[cc];
	tiny_region_t	*tiny_region = tiny_region_for_ptr_no_lock(szone, ptr);
	if (tiny_region) {
	    // this is a tiny pointer
	    boolean_t	is_free;
	    msize_t	msize = get_tiny_meta_header(ptr, &is_free);
	    if (is_free) break; // a double free; let the standard free deal with it
	    tiny_free_no_lock(szone, tiny_region, ptr, msize);
	    to_be_freed[cc] = NULL;
	}
	cc++;
    }
    SZONE_UNLOCK(szone);
    CHECK(szone, __PRETTY_FUNCTION__);
    while (count--) {
	void	*ptr = to_be_freed[count];
	// malloc_printf("Freeing item at %d: %p\n", count, ptr);
	if (ptr) szone_free(szone, ptr);
    }
}

static void
szone_destroy(szone_t *szone) {
    unsigned	index;
    small_region_t	pended_region = 0;
    index = szone->num_large_entries;
    while (index--) {
	large_entry_t	*entry = szone->large_entries + index;
	if (!LARGE_ENTRY_IS_EMPTY(*entry)) {
	    large_entry_t	range;
	    range = *entry;
	    // we deallocate_pages, including guard pages
	    deallocate_pages(szone, LARGE_ENTRY_ADDRESS(range), LARGE_ENTRY_SIZE(range), szone->debug_flags);
	}
    }
    if (szone->num_large_entries * sizeof(large_entry_t) >= LARGE_THRESHOLD) {
	vm_range_t	range_to_deallocate;
	large_entries_free_no_lock(szone, szone->large_entries, szone->num_large_entries, &range_to_deallocate); // we do not free in the small chunk case
	if (range_to_deallocate.size) deallocate_pages(szone, range_to_deallocate.address, range_to_deallocate.size, 0);

    }
    index = szone->num_huge_entries;
    while (index--) {
	huge_entry_t	*huge = szone->huge_entries + index;
	deallocate_pages(szone, huge->address, huge->size, szone->debug_flags);
    }
    // the tiny regions
    index = szone->num_tiny_regions;
    while (index--) {
        tiny_region_t	tiny_region = szone->tiny_regions[index];
	vm_size_t	size_allocated = ((TINY_REGION_SIZE + (1 << vm_page_shift) - 1) >> vm_page_shift) << vm_page_shift;
	deallocate_pages(szone, TINY_REGION_ADDRESS(tiny_region), size_allocated, 0);
    }
    // and now we free regions, with regions[0] as the last one (the final harakiri)
    index = szone->num_small_regions;
    while (index--) {
        small_region_t	region = szone->small_regions[index];
	if (index > 0
	    && (void *)szone->small_regions >= (void *)(SMALL_REGION_ADDRESS(region))
	    && (void *)szone->small_regions < (void *)(SMALL_REGION_END(region))) {
		// Pend deallocation of this region, since the region
		// bookkeeping array is in it.
		pended_region = region;
	} else {
		deallocate_pages(szone, SMALL_REGION_ADDRESS(region), SMALL_REGION_SIZE, 0);
	}
    }
    if (pended_region) {
        deallocate_pages(szone, SMALL_REGION_ADDRESS(pended_region), SMALL_REGION_SIZE, 0);
    }
}

static size_t
szone_good_size(szone_t *szone, size_t size) {
    if (size <= 31 * TINY_QUANTUM) {
	// think tiny
	msize_t	msize = (size + TINY_QUANTUM - 1) >> SHIFT_TINY_QUANTUM;
	if (! msize) msize = 1;
	return msize << SHIFT_TINY_QUANTUM;
    }
    if (!((szone->debug_flags & SCALABLE_MALLOC_ADD_GUARD_PAGES) && PROTECT_SMALL) && (size < LARGE_THRESHOLD)) {
	// think small
	msize_t	msize = (size + SMALL_QUANTUM - 1) >> SHIFT_SMALL_QUANTUM;
	if (! msize) msize = 1;
	return msize << SHIFT_SMALL_QUANTUM;
    } else {
	unsigned		num_pages;
	num_pages = round_page(size) >> vm_page_shift;
	if (!num_pages) num_pages = 1; // minimal allocation size for this
	return num_pages << vm_page_shift;
    }
}

unsigned szone_check_counter = 0;
unsigned szone_check_start = 0;
unsigned szone_check_modulo = 1;

static boolean_t
szone_check_all(szone_t *szone, const char *function) {
    unsigned	index = 0;
    SZONE_LOCK(szone);
    CHECK_LOCKED(szone, __PRETTY_FUNCTION__);
    while (index < szone->num_tiny_regions) {
	tiny_region_t	*region = szone->tiny_regions + index++;
	if (! szone_check_tiny_region(szone, region)) {
	    SZONE_UNLOCK(szone);
	    szone->debug_flags &= ~ CHECK_REGIONS;
	    malloc_printf("*** malloc[%d]: Tiny region %d incorrect szone_check_all(%s) counter=%d\n", getpid(), index-1, function, szone_check_counter);
	    szone_error(szone, "Check: tiny region incorrect", NULL);
	    return 0;
	}
    }

    index = 0;
    while (index < NUM_TINY_SLOTS) {
	if (! tiny_free_list_check(szone, index)) {
	    SZONE_UNLOCK(szone);
	    szone->debug_flags &= ~ CHECK_REGIONS;
	    malloc_printf("*** malloc[%d]: Tiny free list incorrect (slot=%d) szone_check_all(%s) counter=%d\n", getpid(), index, function, szone_check_counter);
	    szone_error(szone, "Check: tiny free list incorrect", NULL);
	    return 0;
	}
	index++;
    }

    index = 0; while (index < szone->num_small_regions) {
	small_region_t	*region = szone->small_regions + index++;
	if (! szone_check_small_region(szone, region)) {
	    SZONE_UNLOCK(szone);
	    szone->debug_flags &= ~ CHECK_REGIONS;
	    malloc_printf("*** malloc[%d]: Small region %d incorrect szone_check_all(%s) counter=%d\n", getpid(), index-1, function, szone_check_counter);
	    szone_error(szone, "Check: small region incorrect", NULL);
	    return 0;
	}
    }
    index = 0;
    while (index < NUM_SMALL_SLOTS) {
	if (! small_free_list_check(szone, index)) {
	    SZONE_UNLOCK(szone);
	    szone->debug_flags &= ~ CHECK_REGIONS;
	    malloc_printf("*** malloc[%d]: Small free list incorrect (grain=%d) szone_check_all(%s) counter=%d\n", getpid(), index, function, szone_check_counter);
	    szone_error(szone, "Check: small free list incorrect", NULL);
	    return 0;
	}
	index++;
    }
    SZONE_UNLOCK(szone);
    // szone_print(szone, 1);
    return 1;
}

static boolean_t
szone_check(szone_t *szone) {
    if (! (++szone_check_counter % 10000)) {
	malloc_printf("At szone_check counter=%d\n", szone_check_counter);
    }
    if (szone_check_counter < szone_check_start) return 1;
    if (szone_check_counter % szone_check_modulo) return 1;
    return szone_check_all(szone, "");
}

static kern_return_t
szone_ptr_in_use_enumerator(task_t task, void *context,
  unsigned type_mask, vm_address_t zone_address, memory_reader_t reader,
  vm_range_recorder_t recorder) {
    szone_t		*szone;
    kern_return_t	err;
    if (!reader) reader = _szone_default_reader;
//    malloc_printf("Enumerator for zone %p\n", zone_address);
    err = reader(task, zone_address, sizeof(szone_t), (void **)&szone);
    if (err) return err;
//    malloc_printf("Tiny ptrs enumeration for zone %p\n", zone_address);
    err = tiny_in_use_enumerator(task, context, type_mask,
      (vm_address_t)szone->tiny_regions, szone->num_tiny_regions, szone->tiny_bytes_free_at_end , reader, recorder);
    if (err) return err;
//    malloc_printf("Small ptrs enumeration for zone %p\n", zone_address);
    err = small_in_use_enumerator(task, context, type_mask,
      (vm_address_t)szone->small_regions, szone->num_small_regions, szone->small_bytes_free_at_end , reader, recorder);
    if (err) return err;
//    malloc_printf("Large ptrs enumeration for zone %p\n", zone_address);
    err = large_in_use_enumerator(task, context, type_mask,
      (vm_address_t)szone->large_entries, szone->num_large_entries, reader,
      recorder);
    if (err) return err;
//    malloc_printf("Huge ptrs enumeration for zone %p\n", zone_address);
    err = huge_in_use_enumerator(task, context, type_mask,
      (vm_address_t)szone->huge_entries, szone->num_huge_entries, reader,
      recorder);
    return err;
}

// Following method is deprecated:  use scalable_zone_statistics instead
void
scalable_zone_info(malloc_zone_t *zone, unsigned *info_to_fill, unsigned count) {
    szone_t	*szone = (void *)zone;
    unsigned	info[13];
    // We do not lock to facilitate debug
    info[4] = szone->num_tiny_objects;
    info[5] = szone->num_bytes_in_tiny_objects;
    info[6] = szone->num_small_objects;
    info[7] = szone->num_bytes_in_small_objects;
    info[8] = szone->num_large_objects_in_use;
    info[9] = szone->num_bytes_in_large_objects;
    info[10] = szone->num_huge_entries;
    info[11] = szone->num_bytes_in_huge_objects;
    info[12] = szone->debug_flags;
    info[0] = info[4] + info[6] + info[8] + info[10];
    info[1] = info[5] + info[7] + info[9] + info[11];
    info[3] = szone->num_tiny_regions * TINY_REGION_SIZE + szone->num_small_regions * SMALL_REGION_SIZE + info[9] + info[11];
    info[2] = info[3] - szone->tiny_bytes_free_at_end - szone->small_bytes_free_at_end;
    memcpy(info_to_fill, info, sizeof(unsigned)*count);
}

static void
szone_print(szone_t *szone, boolean_t verbose) {
    unsigned	info[13];
    unsigned	index = 0;
    SZONE_LOCK(szone);
    scalable_zone_info((void *)szone, info, 13);
    malloc_printf("Scalable zone %p: inUse=%d(%y) touched=%y allocated=%y flags=%d\n", szone, info[0], info[1], info[2], info[3], info[12]);
    malloc_printf("\ttiny=%d(%y) small=%d(%y) large=%d(%y) huge=%d(%y)\n", info[4], info[5], info[6], info[7], info[8], info[9], info[10], info[11]);
    // tiny
    malloc_printf("%d tiny regions: \n", szone->num_tiny_regions);
    while (index < szone->num_tiny_regions) {
	tiny_region_t	*region = szone->tiny_regions + index;
	print_tiny_region(verbose, *region, (index == szone->num_tiny_regions - 1) ? szone->tiny_bytes_free_at_end : 0);
	index++;
    }
    if (verbose) print_tiny_free_list(szone);
    // small
    malloc_printf("%d small regions: \n", szone->num_small_regions);
    index = 0; 
    while (index < szone->num_small_regions) {
	small_region_t	*region = szone->small_regions + index;
	print_small_region(szone, verbose, region, (index == szone->num_small_regions - 1) ? szone->small_bytes_free_at_end : 0);
	index++;
    }
    if (verbose) print_small_free_list(szone);
    SZONE_UNLOCK(szone);
}

static void
szone_log(malloc_zone_t *zone, void *log_address) {
    szone_t	*szone = (void *)zone;
    szone->log_address = log_address;
}

static void
szone_force_lock(szone_t *szone) {
//    malloc_printf("szone_force_lock\n");
    SZONE_LOCK(szone);
}

static void
szone_force_unlock(szone_t *szone) {
//    malloc_printf("szone_force_unlock\n");
    SZONE_UNLOCK(szone);
}

boolean_t
scalable_zone_statistics(malloc_zone_t *zone, malloc_statistics_t *stats, unsigned subzone) {
    szone_t *szone = (void *)zone;
    switch (subzone) {
	case 0:	
	    stats->blocks_in_use = szone->num_tiny_objects;
	    stats->size_in_use = szone->num_bytes_in_tiny_objects;
	    stats->size_allocated = szone->num_tiny_regions * TINY_REGION_SIZE;
	    stats->max_size_in_use = stats->size_allocated - szone->tiny_bytes_free_at_end;
	    return 1;
	case 1:	
	    stats->blocks_in_use = szone->num_small_objects;
	    stats->size_in_use = szone->num_bytes_in_small_objects;
	    stats->size_allocated = szone->num_small_regions * SMALL_REGION_SIZE;
	    stats->max_size_in_use = stats->size_allocated - szone->small_bytes_free_at_end;
	    return 1;
	case 2:
	    stats->blocks_in_use = szone->num_large_objects_in_use;
	    stats->size_in_use = szone->num_bytes_in_large_objects;
	    stats->max_size_in_use = stats->size_allocated = stats->size_in_use;
	    return 1;
	case 3:
	    stats->blocks_in_use = szone->num_huge_entries;
	    stats->size_in_use = szone->num_bytes_in_huge_objects;
	    stats->max_size_in_use = stats->size_allocated = stats->size_in_use;
	    return 1;
    }
    return 0;
}

static void
szone_statistics(szone_t *szone, malloc_statistics_t *stats) {
    stats->blocks_in_use = szone->num_tiny_objects + szone->num_small_objects + szone->num_large_objects_in_use + szone->num_huge_entries;
    size_t	big_and_huge = szone->num_bytes_in_large_objects + szone->num_bytes_in_huge_objects;
    stats->size_in_use = szone->num_bytes_in_tiny_objects + szone->num_bytes_in_small_objects + big_and_huge;
    stats->max_size_in_use = stats->size_allocated = szone->num_tiny_regions * TINY_REGION_SIZE + szone->num_small_regions * SMALL_REGION_SIZE + big_and_huge ; 
    // Now we account for the untouched areas
    stats->max_size_in_use -= szone->tiny_bytes_free_at_end;
    stats->max_size_in_use -= szone->small_bytes_free_at_end;
}

static const struct malloc_introspection_t szone_introspect = {
    (void *)szone_ptr_in_use_enumerator,
    (void *)szone_good_size,
    (void *)szone_check,
    (void *)szone_print,
    szone_log,
    (void *)szone_force_lock,
    (void *)szone_force_unlock,
    (void *)szone_statistics
}; // marked as const to spare the DATA section

malloc_zone_t *
create_scalable_zone(size_t initial_size, unsigned debug_flags) {
    szone_t		*szone;
    vm_address_t	addr;
    size_t		msize;
    size_t		msize_used = 0;
    // malloc_printf("=== create_scalable_zone(%d,%d) - %s\n", initial_size, debug_flags, (DEBUG_MALLOC) ? "**** DEBUG" : "");
#if PAGE_SIZE_FIXED
    if ((1 << vm_page_shift) == vm_page_size) {
	// malloc_printf("vm_page_shift validated to be %d\n", vm_page_shift);
    } else {
	malloc_printf("*** vm_page_shift incorrectly set to %d\n", vm_page_shift);
	exit(-1);
    }
#else
    if (!vm_page_shift) {
	unsigned	page;
	vm_page_shift = 12; // the minimal for page sizes
	page = 1 << vm_page_shift;
	while (page != vm_page_size) { page += page; vm_page_shift++;};
    }
#endif
    addr = allocate_pages(NULL, SMALL_REGION_SIZE, SMALL_BLOCKS_ALIGN, 0, VM_MAKE_TAG(VM_MEMORY_MALLOC));
    if (!addr) return NULL;
    szone = (void *)addr;
    msize = (sizeof(szone_t) + SMALL_QUANTUM - 1) >> SHIFT_SMALL_QUANTUM;
    // malloc_printf("sizeof(szone_t)=%d msize for 1st block=%d; wasted %d bytes\n", sizeof(szone_t), msize, (msize << SHIFT_SMALL_QUANTUM) - sizeof(szone_t));
    small_meta_header(szone)[0] = msize;
    szone->tiny_regions = szone->initial_tiny_regions;
    szone->small_regions = szone->initial_small_regions;
    msize_used += msize; szone->num_small_objects++;
    szone->basic_zone.version = 3;
    szone->basic_zone.size = (void *)szone_size;
    szone->basic_zone.malloc = (void *)szone_malloc;
    szone->basic_zone.calloc = (void *)szone_calloc;
    szone->basic_zone.valloc = (void *)szone_valloc;
    szone->basic_zone.free = (void *)szone_free;
    szone->basic_zone.realloc = (void *)szone_realloc;
    szone->basic_zone.destroy = (void *)szone_destroy;
    szone->basic_zone.batch_malloc = (void *)szone_batch_malloc;
    szone->basic_zone.batch_free = (void *)szone_batch_free;
    szone->basic_zone.introspect = (struct malloc_introspection_t *)&szone_introspect;
    LOCK_INIT(szone->lock);
#if 0
#warning CHECK_REGIONS enabled
    debug_flags |= CHECK_REGIONS;
#endif
#if 0
#warning LOG enabled
    szone->log_address = ~0;
#endif
    szone->debug_flags = debug_flags;
    szone->small_regions[0] = addr >> SMALL_BLOCKS_ALIGN;
    szone->num_small_regions = 1;
    msize_t	free_msize = NUM_SMALL_BLOCKS - msize;
    small_meta_header(szone)[msize] = free_msize;
    szone->small_bytes_free_at_end = free_msize << SHIFT_SMALL_QUANTUM;
    CHECK(szone, __PRETTY_FUNCTION__);
#if 0
    write(1, "Malloc szone created\n", 23);
#endif
    return (malloc_zone_t *)szone;
}

/********* Support code for emacs unexec ************/

/* History of freezedry version numbers:
 *
 * 1) Old malloc (before the scalable malloc implementation in this file
 *    existed).
 * 2) Original freezedrying code for scalable malloc.  This code was apparently
 *    based on the old freezedrying code and was fundamentally flawed in its
 *    assumption that tracking allocated memory regions was adequate to fake
 *    operations on freezedried memory.  This doesn't work, since scalable
 *    malloc does not store flags in front of large page-aligned allocations.
 * 3) Original szone-based freezedrying code.
 * 4) Fresher malloc with tiny zone
 *
 * No version backward compatibility is provided, but the version number does
 * make it possible for malloc_jumpstart() to return an error if the application
 * was freezedried with an older version of malloc.
 */
#define MALLOC_FREEZEDRY_VERSION 4

typedef struct {
    unsigned version;
    unsigned nszones;
    szone_t *szones;
} malloc_frozen;

static void *
frozen_malloc(szone_t *zone, size_t new_size) {
    return malloc(new_size);
}

static void *
frozen_calloc(szone_t *zone, size_t num_items, size_t size) {
    return calloc(num_items, size);
}

static void *
frozen_valloc(szone_t *zone, size_t new_size) {
    return valloc(new_size);
}

static void *
frozen_realloc(szone_t *zone, void *ptr, size_t new_size) {
    size_t	old_size = szone_size(zone, ptr);
    void	*new_ptr;
    if (new_size <= old_size) {
	return ptr;
    }
    new_ptr = malloc(new_size);
    if (old_size > 0) {
	memcpy(new_ptr, ptr, old_size);
    }
    return new_ptr;
}

static void
frozen_free(szone_t *zone, void *ptr) {
}

static void
frozen_destroy(szone_t *zone) {
}

/********* Pseudo-private API for emacs unexec ************/

/*
 * malloc_freezedry() records all of the szones in use, so that they can be
 * partially reconstituted by malloc_jumpstart().  Due to the differences
 * between reconstituted memory regions and those created by the szone code,
 * care is taken not to reallocate from the freezedried memory, except in the
 * case of a non-growing realloc().
 *
 * Due to the flexibility provided by the zone registration mechanism, it is
 * impossible to implement generic freezedrying for any zone type.  This code
 * only handles applications that use the szone allocator, so malloc_freezedry()
 * returns 0 (error) if any non-szone zones are encountered.
 */

int
malloc_freezedry(void) {
    extern unsigned malloc_num_zones;
    extern malloc_zone_t **malloc_zones;
    malloc_frozen *data;
    unsigned i;

    /* Allocate space in which to store the freezedry state. */
    data = (malloc_frozen *) malloc(sizeof(malloc_frozen));

    /* Set freezedry version number so that malloc_jumpstart() can check for compatibility. */
    data->version = MALLOC_FREEZEDRY_VERSION;

    /* Allocate the array of szone pointers. */
    data->nszones = malloc_num_zones;
    data->szones = (szone_t *) calloc(malloc_num_zones, sizeof(szone_t));

    /* Fill in the array of szone structures.  They are copied rather than
     * referenced, since the originals are likely to be clobbered during malloc
     * initialization. */
    for (i = 0; i < malloc_num_zones; i++) {
	if (strcmp(malloc_zones[i]->zone_name, "DefaultMallocZone")) {
	    /* Unknown zone type. */
	    free(data->szones);
	    free(data);
	    return 0;
	}
	memcpy(&data->szones[i], malloc_zones[i], sizeof(szone_t));
    }

    return (int) data;
}

int
malloc_jumpstart(int cookie) {
    malloc_frozen *data = (malloc_frozen *) cookie;
    unsigned i;

    if (data->version != MALLOC_FREEZEDRY_VERSION) {
	/* Unsupported freezedry version. */
	return 1;
    }

    for (i = 0; i < data->nszones; i++) {
	/* Set function pointers.  Even the functions that stay the same must be
	 * set, since there are no guarantees that they will be mapped to the
	 * same addresses. */
	data->szones[i].basic_zone.size = (void *) szone_size;
	data->szones[i].basic_zone.malloc = (void *) frozen_malloc;
	data->szones[i].basic_zone.calloc = (void *) frozen_calloc;
	data->szones[i].basic_zone.valloc = (void *) frozen_valloc;
	data->szones[i].basic_zone.free = (void *) frozen_free;
	data->szones[i].basic_zone.realloc = (void *) frozen_realloc;
	data->szones[i].basic_zone.destroy = (void *) frozen_destroy;
	data->szones[i].basic_zone.introspect = (struct malloc_introspection_t *)&szone_introspect;

	/* Register the freezedried zone. */
	malloc_zone_register(&data->szones[i].basic_zone);
    }

    return 0;
}
