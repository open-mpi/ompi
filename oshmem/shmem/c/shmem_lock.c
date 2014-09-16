/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/runtime/params.h"
#include "oshmem/runtime/runtime.h"
#include <stdlib.h>
#include <memory.h>

#include "oshmem/shmem/shmem_api_logger.h"
#include "oshmem/shmem/shmem_lock.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/atomic/atomic.h"

#define OPAL_BITWISE_SIZEOF_LONG (SIZEOF_LONG * 8)

struct oshmem_lock_counter {
    void *lock;
    int counter;
    struct oshmem_lock_counter *next;
    struct oshmem_lock_counter *prev;
};
typedef struct oshmem_lock_counter oshmem_lock_counter_t;

struct oshmem_lock_prev_pe_container {
    void *lock;
    int prev_pe;
    struct oshmem_lock_prev_pe_container *next;
    struct oshmem_lock_prev_pe_container *prev;
};
typedef struct oshmem_lock_prev_pe_container oshmem_lock_prev_pe_container_t;

oshmem_lock_counter_t *lock_counter_head;
oshmem_lock_prev_pe_container_t *lock_prev_pe_container_head;
static int *lock_turn;
static int *lock_inform;
static int *lock_last_ticket;

static int lock_save_prev_pe(void *lock, int prev_pe);
static int lock_restore_prev_pe(void *lock, int *prev_pe);

static int shmem_lock_try_inform_server(void *lock, int lock_size);
static void shmem_get_wrapper(uint64_t *target,
                              const void *source,
                              int source_size,
                              size_t nelems,
                              int pe);
static void shmem_wait_wrapper(void *target, int target_size, uint64_t value);

static int shmem_lock_extract_pe_next(void *lock, int lock_size, int *pe_next);
static int shmem_lock_pack_pe_next_pe_last(void *lock,
                                           int lock_size,
                                           const int *pe_next,
                                           const int *pe_last);
static int shmem_lock_pack_pe_next(void *lock,
                                   int lock_size,
                                   const int *pe_next);

static void shmem_lock_increment_counter(void *lock, int lock_size);
static int shmem_lock_decrement_counter(void *lock, int lock_size);

static int shmem_lock_get_server(void *lock);
static int shmem_lock_is_mine(void *lock, int lock_size);

static int shmem_lock_get_ticket(void *lock);
static int shmem_lock_wait_for_ticket(void *lock,
                                      int lock_size,
                                      int ticket,
                                      int *pe_last);
static int shmem_lock_subscribe_for_informing(void *lock,
                                              int lock_size,
                                              int pe_last);
static int shmem_lock_wait_for_informing(void *lock, int lock_size);
static int shmem_lock_inform_next(void *lock, int lock_size, int pe_next);

/***************************************************************************/
/**************************Init/Finalize************************************/
/***************************************************************************/

int shmem_lock_init()
{
    void* ptr = 0;

#if (OPAL_BITWISE_SIZEOF_LONG == 32)
    int number_of_pes = shmem_n_pes();
    if (number_of_pes >= 65534)
    {
        SHMEM_API_ERROR("SHMEM distributed locking implementation does not support total number of PEs greater than 65534 if sizeof(long) = 4");
        return OSHMEM_ERROR;
    }
#endif

#if ((OPAL_BITWISE_SIZEOF_LONG != 64) && (OPAL_BITWISE_SIZEOF_LONG != 32))
    SHMEM_API_ERROR("SHMEM distributed locking implementation does not support sizeof(long) = %i",
                    sizeof(long));
    return OSHMEM_ERROR;
#endif

    ptr = (void *) lock_turn;
    MCA_MEMHEAP_CALL(private_alloc(sizeof(int), &ptr));
    lock_turn = (int *) ptr;
    *lock_turn = 0;
    ptr = (void *) lock_last_ticket;
    MCA_MEMHEAP_CALL(private_alloc(sizeof(int), &ptr));
    lock_last_ticket = (int *) ptr;
    *lock_last_ticket = 0;
    ptr = (void *) lock_inform;
    MCA_MEMHEAP_CALL(private_alloc(sizeof(int), &ptr));
    lock_inform = (int *) ptr;
    *lock_inform = 0;

    lock_counter_head = 0;
    lock_prev_pe_container_head = 0;

    return OSHMEM_SUCCESS;
}

int shmem_lock_finalize()
{
    oshmem_lock_counter_t *current_counter = lock_counter_head;
    oshmem_lock_prev_pe_container_t *current_pe_container =
            lock_prev_pe_container_head;

    if (0 != lock_turn) {
        MCA_MEMHEAP_CALL(private_free(lock_turn));
    }
    if (0 != lock_last_ticket) {
        MCA_MEMHEAP_CALL(private_free(lock_last_ticket));
    }
    if (0 != lock_inform) {
        MCA_MEMHEAP_CALL(private_free(lock_inform));
    }

    lock_turn = 0;
    lock_last_ticket = 0;
    lock_inform = 0;

    while (0 != current_counter) {
        oshmem_lock_counter_t *counter_to_free = current_counter;
        current_counter = current_counter->next;
        free(counter_to_free);
    }
    lock_counter_head = 0;

    while (0 != current_pe_container) {
        oshmem_lock_prev_pe_container_t *container_to_free =
                current_pe_container;
        current_pe_container = current_pe_container->next;
        free(container_to_free);
    }
    lock_prev_pe_container_head = 0;

    return OSHMEM_SUCCESS;
}

static int shmem_lock_get_server(void *lock)
{
    uint64_t offset =  MCA_MEMHEAP_CALL(find_offset(shmem_my_pe(), 0, lock, lock));
    return (offset / 8) % shmem_n_pes();
}

static uint64_t get_lock_value(const void *lock, int lock_size)
{
    uint64_t lock_value = 0;

    if (lock_size == 4) {
        lock_value = *(uint32_t *) lock;
    } else if (lock_size == 8) {
        lock_value = *(uint64_t *) lock;
    }

    return lock_value;
}

static void set_lock_value(void *lock, int lock_size, uint64_t lock_value)
{
    if (lock_size == 8) {
        memcpy(lock, &lock_value, 8);
    } else if (lock_size == 4) {
        uint32_t lock_value_32 = (uint32_t) lock_value;
        memcpy(lock, &lock_value_32, 4);
    }
}

/***************************************************************************/
/**************************Pack/Extract*************************************/
/***************************************************************************/

static int extract_2_words(const void *lock, int lock_size, int *one, int *two)
{
    int lock_bitwise_size = lock_size * 8;
    uint64_t lock_value = get_lock_value(lock, lock_size);

    if (lock == 0 || one == 0 || two == 0) {
        return OSHMEM_ERROR;
    }

    *one = (int) (lock_value >> (lock_bitwise_size / 2));
    if (lock_size == 8) {
        *two = (int) ((lock_value << 32) >> 32);
    } else if (lock_size == 4) {
        *two = (int) ((lock_value << 48) >> 48);
    }

    return OSHMEM_SUCCESS;
}

static int pack_2_words(void *lock,
                        int lock_size,
                        const int *one,
                        const int *two)
{
    uint64_t lock_value = 0;
    int lock_bitwise_size = lock_size * 8;

    if (lock == 0 || one == 0 || two == 0) {
        return OSHMEM_ERROR;
    }

    lock_value = (uint64_t) *two
            | (((uint64_t) *one) << (lock_bitwise_size / 2));
    set_lock_value(lock, lock_size, lock_value);

    return OSHMEM_SUCCESS;
}

static int extract_first_word(void *lock, int lock_size, int *one)
{
    int two = 0;
    return extract_2_words(lock, lock_size, one, &two);
}

static int extract_second_word(void *lock, int lock_size, int *two)
{
    int one = 0;
    return extract_2_words(lock, lock_size, &one, two);
}

static uint64_t shmem_cswap(void *target,
                            int target_size,
                            uint64_t cond,
                            uint64_t value,
                            int pe)
{
    uint64_t prev_value = 0;

    if (target_size == 8) {
        MCA_ATOMIC_CALL(cswap( target, (void*)&prev_value, (const void*)&cond, (const void*)&value, target_size, pe));
    } else if (target_size == 4) {
        uint32_t prev_value_32 = 0;
        uint32_t cond32 = (uint32_t) cond;
        uint32_t value32 = (uint32_t) value;

        MCA_ATOMIC_CALL(cswap( target, (void*)&prev_value_32, (const void*)&cond32, (const void*)&value32, target_size, pe));

        prev_value = prev_value_32;
    }

    return prev_value;
}

static uint64_t shmem_fadd(void *target,
                           int target_size,
                           uint64_t value,
                           int pe)
{
    uint64_t prev_value = 0;

    if (target_size == sizeof(int)) {
        prev_value = (uint64_t) shmem_int_fadd((int *) target, (int) value, pe);
    } else if (target_size == sizeof(long)) {
        prev_value = (uint64_t) shmem_long_fadd((long *) target,
                                                (long) value,
                                                pe);
    } else if (target_size == sizeof(long long)) {
        prev_value = (uint64_t) shmem_longlong_fadd((long long *) target,
                                                    (long long) value,
                                                    pe);
    }

    return prev_value;
}

static int pack_first_word(void *lock,
                           int lock_size,
                           const int *one,
                           int use_atomic)
{
    int my_pe = shmem_my_pe();
    uint64_t lock_value = 0;
    uint64_t new_long_value = 0;
    uint64_t temp = 0;
    int two = 0;

    if (lock == 0 || one == 0) {
        return OSHMEM_ERROR;
    }

    if (use_atomic) {
        lock_value = get_lock_value(lock, lock_size);
        extract_second_word(&lock_value, lock_size, &two);
        pack_2_words(&new_long_value, lock_size, one, &two);
        while (lock_value
                != (temp = shmem_cswap(lock,
                                       lock_size,
                                       lock_value,
                                       new_long_value,
                                       my_pe))) {
            lock_value = temp;
            extract_second_word(&lock_value, lock_size, &two);
            pack_2_words(&new_long_value, lock_size, one, &two);
        }
    } else {
        uint64_t zero_mask = 0xFFFFFFFF;
        if (lock_size == 4) {
            zero_mask = 0xFFFF;
        }

        int zero = 0;
        int written_one = 0;

        pack_2_words(&new_long_value, lock_size, one, &zero);
        do {
            lock_value = get_lock_value(lock, lock_size);
            lock_value &= zero_mask;
            lock_value |= new_long_value;
            set_lock_value(lock, lock_size, lock_value);
            extract_first_word(lock, lock_size, &written_one);
        } while (written_one != *one);
    }

    return OSHMEM_SUCCESS;
}

static int pack_second_word(void *lock,
                            int lock_size,
                            const int *two,
                            int use_atomic)
{
    int my_pe = shmem_my_pe();
    uint64_t lock_value = 0;
    uint64_t new_long_value = 0;
    uint64_t temp = 0;
    int one = 0;

    if (lock == 0 || two == 0) {
        return OSHMEM_ERROR;
    }

    if (use_atomic) {
        lock_value = get_lock_value(lock, lock_size);
        extract_first_word(&lock_value, lock_size, &one);
        pack_2_words(&new_long_value, lock_size, &one, two);
        while (lock_value
                != (temp = shmem_cswap(lock,
                                       lock_size,
                                       lock_value,
                                       new_long_value,
                                       my_pe))) {
            lock_value = temp;
            extract_first_word(&lock_value, lock_size, &one);
            pack_2_words(&new_long_value, lock_size, &one, two);
        }
    } else {
        uint64_t zero_mask = 0xFFFFFFFF00000000;
        if (lock_size == 4) {
            zero_mask = 0xFFFF0000;
        }

        int zero = 0;
        int written_two = 0;

        pack_2_words(&new_long_value, lock_size, &zero, two);
        do {
            lock_value = get_lock_value(lock, lock_size);
            lock_value &= zero_mask;
            lock_value |= new_long_value;
            set_lock_value(lock, lock_size, lock_value);
            extract_second_word(lock, lock_size, &written_two);
        } while (written_two != *two);
    }

    return OSHMEM_SUCCESS;
}

static int lock_extract_pe_next_counter(void *lock,
                                        int lock_size,
                                        int *pe_next,
                                        int *counter)
{
    int status = extract_2_words(lock, lock_size, counter, pe_next);

    /* make sure counter does not have the last bit - infoming bit */
    *counter &= ~(((unsigned int) 1) << (SIZEOF_INT * 8 - 1));
    if (*pe_next >= 0) {
        *pe_next -= 1;
    }

    return status;
}

static int shmem_lock_extract_pe_next(void *lock, int lock_size, int *pe_next)
{
    int status = extract_second_word(lock, lock_size, pe_next);
    if (*pe_next >= 0) {
        *pe_next -= 1;
    }

    return status;
}

static int lock_extract_pe_last(void *lock, int lock_size, int *pe_last)
{
    int status = extract_first_word(lock, lock_size, pe_last);
    if (*pe_last >= 0) {
        *pe_last -= 1;
    }

    return status;
}

static int lock_extract_counter(void *lock, int lock_size, int *count)
{
    int status = extract_first_word(lock, lock_size, count);

    /* make sure counter does not have the last bit - infoming bit */
    *count &= ~(((unsigned int) 1) << (SIZEOF_INT * 8 - 1));
    return status;
}

static int shmem_lock_pack_pe_next_pe_last(void *lock,
                                           int lock_size,
                                           const int *pe_next,
                                           const int *pe_last)
{
    int pe_next_plus_one = *pe_next + 1;
    int pe_last_plus_one = *pe_last + 1;

    return pack_2_words(lock, lock_size, &pe_last_plus_one, &pe_next_plus_one);
}

static int lock_pack_pe_next_counter(void *lock,
                                     int lock_size,
                                     const int *pe_next,
                                     const int *counter)
{
    int pe_next_plus_one = *pe_next + 1;

    return pack_2_words(lock, lock_size, counter, &pe_next_plus_one);
}

static int shmem_lock_pack_pe_next(void *lock,
                                   int lock_size,
                                   const int *pe_next)
{
    int pe_next_plus_one = *pe_next + 1;

    return pack_second_word(lock, lock_size, &pe_next_plus_one, 1);
}

static int lock_pack_counter(void *lock,
                             int lock_size,
                             const int *counter,
                             int use_atomic)
{
    return pack_first_word(lock, lock_size, counter, use_atomic);
}

static int lock_pack_pe_last(void *lock,
                             int lock_size,
                             const int *pe_last,
                             int use_atomic)
{
    int pe_last_plus_one = *pe_last + 1;

    return pack_first_word(lock, lock_size, &pe_last_plus_one, use_atomic);
}

/***************************************************************************/
/**************************Lock counters************************************/
/***************************************************************************/

static oshmem_lock_counter_t *lock_find_counter(void *lock)
{
    oshmem_lock_counter_t *current_counter = lock_counter_head;

    if (0 == lock_counter_head) {
        return 0;
    }

    while (0 != current_counter) {
        if (current_counter->lock == lock) {
            return current_counter;
        }

        current_counter = current_counter->next;
    }

    return 0;
}

static int shmem_lock_insert_counter(void *lock)
{
    oshmem_lock_counter_t *counter = lock_find_counter(lock);

    if (counter) {
        counter->counter += 1;
    } else if (lock_counter_head) {
        counter = malloc(sizeof(oshmem_lock_counter_t));
        counter->lock = lock;
        counter->counter = 1;
        counter->next = lock_counter_head;
        counter->prev = lock_counter_head->prev;
        lock_counter_head->prev = counter;
        lock_counter_head = counter;
    } else {
        lock_counter_head = malloc(sizeof(oshmem_lock_counter_t));
        lock_counter_head->lock = lock;
        lock_counter_head->counter = 1;
        lock_counter_head->next = 0;
        lock_counter_head->prev = 0;
    }

    return OSHMEM_SUCCESS;
}

static int shmem_lock_remove_counter(void *lock)
{
    oshmem_lock_counter_t *counter = lock_find_counter(lock);

    if (counter) {
        oshmem_lock_counter_t *prev = counter->prev;
        oshmem_lock_counter_t *next = counter->next;

        if (next) {
            next->prev = prev;
        }

        if (prev) {
            prev->next = next;
        }

        if (lock_counter_head == counter) {
            lock_counter_head = next;
        }

        free(counter);
    }

    return OSHMEM_SUCCESS;
}

static void shmem_lock_increment_counter(void *lock, int lock_size)
{
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);

    if (my_pe == server_pe) {
        shmem_lock_insert_counter(lock);
    } else {
        int counter = 0;
        lock_extract_counter(lock, lock_size, &counter);
        counter++;
        lock_pack_counter(lock, lock_size, &counter, 1);
    }
}

static int shmem_lock_decrement_counter(void *lock, int lock_size)
{
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);
    int current_lock_counter = -1;

    if (my_pe == server_pe) {
        oshmem_lock_counter_t *counter = lock_find_counter(lock);

        if (counter) {
            if (oshmem_shmem_lock_recursive) {
                counter->counter -= 1;
            } else {
                counter->counter = 0;
            }

            if ((current_lock_counter = counter->counter) <= 0) {
                shmem_lock_remove_counter(lock);
                current_lock_counter = 0;
            }
        }
    } else {
        int pe_next = 0, counter = 0;
        lock_extract_pe_next_counter(lock, lock_size, &pe_next, &counter);
        if (counter > 0) {
            if (oshmem_shmem_lock_recursive) {
                current_lock_counter = counter - 1;
            } else {
                current_lock_counter = 0;
            }

            lock_pack_counter(lock, lock_size, &current_lock_counter, 1);
        }
    }

    return current_lock_counter;
}

static int lock_get_count(void *lock, int lock_size)
{
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);
    int count = 0;

    if (my_pe == server_pe) {
        oshmem_lock_counter_t *counter = lock_find_counter(lock);

        if (counter) {
            count = counter->counter;
        }
    } else {
        lock_extract_counter(lock, lock_size, &count);
    }

    return count;
}

static int shmem_lock_is_mine(void *lock, int lock_size)
{
    return (lock_get_count(lock, lock_size) > 0);
}

/***************************************************************************/
/**************************Ticket utilities*********************************/
/***************************************************************************/
static int shmem_lock_get_ticket(void *lock)
{
    int server_pe = shmem_lock_get_server(lock);
    int my_ticket = shmem_int_finc(lock_last_ticket, server_pe);

    return my_ticket;
}

static void shmem_get_wrapper(uint64_t *target,
                              const void *source,
                              int source_size,
                              size_t nelems,
                              int pe)
{
    if (source_size == 8) {
        shmem_get64(target, source, nelems, pe);
    } else if (source_size == 4) {
        uint32_t temp32 = 0;
        shmem_get32(&temp32, source, nelems, pe);
        *target = temp32;
    }
}

static int shmem_lock_wait_for_ticket(void *lock,
                                      int lock_size,
                                      int ticket,
                                      int *pe_last)
{
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);
    int remote_new_pe_last = 0;
    int remote_turn = 0;
    uint64_t server_lock = 0;
    uint64_t new_server_lock = 0;
    uint64_t temp = 0;

    do {
        shmem_int_get(&remote_turn, lock_turn, 1, server_pe);
    } while (remote_turn != ticket);

    shmem_get_wrapper(&temp, lock, lock_size, 1, server_pe);
    do {
        /* Another process has ignored the queue, possibly due to shmem_test_lock */
        new_server_lock = server_lock = temp;
        lock_pack_pe_last(&new_server_lock, lock_size, &my_pe, 0);
    } while (server_lock
            != (temp = shmem_cswap(lock,
                                   lock_size,
                                   server_lock,
                                   new_server_lock,
                                   server_pe)));
    lock_extract_pe_last(&server_lock, lock_size, pe_last);
    if (*pe_last == -1) {
        /* we are first in queue for the lock */
        *pe_last = my_pe;
    }

    /* Since quiet is too slow in ikrit then check directly
     * shmem_quiet();
     */
    do {
        shmem_get_wrapper(&new_server_lock, lock, lock_size, 1, server_pe);
        lock_extract_pe_last(&new_server_lock, lock_size, &remote_new_pe_last);
    } while (remote_new_pe_last != my_pe);

    shmem_int_finc(lock_turn, server_pe);

    return OSHMEM_SUCCESS;
}

static int shmem_lock_subscribe_for_informing(void *lock,
                                              int lock_size,
                                              int pe_last)
{
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);
    int remote_prev_pe_next = 0;
    uint64_t prev_remote_value = 1;

    prev_remote_value = shmem_fadd(lock, lock_size, my_pe + 1, pe_last);
    if (my_pe == server_pe) {
        lock_save_prev_pe(lock, pe_last);
    }

    /* Check the previous value of pe_next is -1
     * if not 0 report a bug in distributed locking implementation
     */
    shmem_lock_extract_pe_next(&prev_remote_value,
                               lock_size,
                               &remote_prev_pe_next);
    if (remote_prev_pe_next != -1) {
        int remote_counter = 0;
        uint64_t new_remote_value = 0;
        uint64_t temp_value = 0;
        SHMEM_API_ERROR("PE #%i noticed incorrect pe_next value=%i on PE#%i",
                        my_pe, remote_prev_pe_next, pe_last);

        /* Trying to restore */
        lock_extract_counter(&prev_remote_value, lock_size, &remote_counter);
        lock_pack_pe_next_counter(&new_remote_value,
                                  lock_size,
                                  &my_pe,
                                  &remote_counter);
        prev_remote_value += my_pe + 1;

        while (prev_remote_value
                != (temp_value = shmem_cswap(lock,
                                             lock_size,
                                             prev_remote_value,
                                             new_remote_value,
                                             pe_last))) {
            prev_remote_value = temp_value;
            lock_extract_counter(&prev_remote_value,
                                 lock_size,
                                 &remote_counter);
            lock_pack_pe_next_counter(&new_remote_value,
                                      lock_size,
                                      &my_pe,
                                      &remote_counter);
        }
    }

    return OSHMEM_SUCCESS;
}

static void shmem_wait_wrapper(void *target, int target_size, uint64_t value)
{
    if (target_size == sizeof(int)) {
        shmem_int_wait((int *) target, (int) value);
    } else if (target_size == sizeof(long)) {
        shmem_long_wait((long *) target, (long) value);
    } else if (target_size == sizeof(long long)) {
        shmem_longlong_wait((long long *) target, (long long) value);
    }
}

/***************************************************************************/
/********************Release lock informing functions***********************/
/***************************************************************************/

static int shmem_lock_wait_for_informing(void *lock, int lock_size)
{
    int lock_bitwise_size = lock_size * 8;
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);

    if (my_pe != server_pe) {
        int original_counter = 1;
        uint64_t prev_value = get_lock_value(lock, lock_size);
        int informed = (prev_value >> (lock_bitwise_size - 1));

        lock_extract_counter(&prev_value, lock_size, &original_counter);
        while (!informed) {
            shmem_wait_wrapper(lock, lock_size, prev_value);
            prev_value = get_lock_value(lock, lock_size);
            informed = (prev_value >> (lock_bitwise_size - 1));
        }

        lock_pack_counter(lock, lock_size, &original_counter, 1);
    } else {
        int prev_value = *lock_inform;
        int prev_pe = -1;
        int remote_pe_next = 0;
        int remote_counter = 1;

        if (OSHMEM_SUCCESS != lock_restore_prev_pe(lock, &prev_pe)) {
            SHMEM_API_ERROR("Unable to restore prev_pe on server PE#%i", my_pe);
            oshmem_shmem_abort(-1);

        }

        if (prev_pe == server_pe) {
            SHMEM_API_ERROR("prev_pe (%i) is me", prev_pe);
        }

        do {
            uint64_t remote_lock = 0;
            shmem_get_wrapper(&remote_lock, lock, lock_size, 1, prev_pe);
            lock_extract_pe_next_counter(&remote_lock,
                                         lock_size,
                                         &remote_pe_next,
                                         &remote_counter);
            if ((remote_counter > 0) && (remote_pe_next == my_pe)) {
                shmem_int_wait(lock_inform, prev_value);
                prev_value = *lock_inform;
            }
        } while ((remote_counter > 0) && (remote_pe_next == my_pe));
    }

    return OSHMEM_SUCCESS;
}

static int shmem_lock_inform_next(void *lock, int lock_size, int pe_next)
{
    int lock_bitwise_size = lock_size * 8;
    int server_pe = shmem_lock_get_server(lock);

    if (server_pe != pe_next) {
        uint64_t temp_value = 0, remote_value = 0;
        shmem_get_wrapper(&remote_value, lock, lock_size, 1, pe_next);
        uint64_t new_remote_value = remote_value
                | (((uint64_t) 1) << (lock_bitwise_size - 1));

        while (remote_value
                != (temp_value = shmem_cswap(lock,
                                             lock_size,
                                             remote_value,
                                             new_remote_value,
                                             pe_next))) {
            remote_value = temp_value;
            new_remote_value = remote_value
                    | (((uint64_t) 1) << (lock_bitwise_size - 1));
        }
    } else {
        shmem_int_inc(lock_inform, pe_next);
    }

    return OSHMEM_SUCCESS;
}

static int lock_save_prev_pe(void *lock, int prev_pe)
{
    oshmem_lock_prev_pe_container_t *container = lock_prev_pe_container_head;

    while (container != 0) {
        if (container->lock == lock) {
            break;
        }
        container = container->next;
    }

    if (container) {
        container->prev_pe = prev_pe;
    } else {
        container = malloc(sizeof(oshmem_lock_prev_pe_container_t));
        container->lock = lock;
        container->prev_pe = prev_pe;
        container->next = lock_prev_pe_container_head;
        container->prev = 0;
        if (lock_prev_pe_container_head) {
            lock_prev_pe_container_head->prev = container;
        }
        lock_prev_pe_container_head = container;
    }

    return OSHMEM_SUCCESS;
}

static int lock_restore_prev_pe(void *lock, int *prev_pe)
{
    oshmem_lock_prev_pe_container_t *container = lock_prev_pe_container_head;
    while (container != 0) {
        if (container->lock == lock) {
            break;
        }
        container = container->next;
    }

    if (container) {
        oshmem_lock_prev_pe_container_t *next = container->next;
        oshmem_lock_prev_pe_container_t *prev = container->prev;
        *prev_pe = container->prev_pe;

        if (prev) {
            prev->next = next;
        }
        if (next) {
            next->prev = prev;
        }
        if (lock_prev_pe_container_head == container) {
            lock_prev_pe_container_head = next;
        }
        free(container);
        return OSHMEM_SUCCESS;
    } else {
        *prev_pe = -1;
        return OSHMEM_ERROR;
    }
}

static int shmem_lock_try_inform_server(void *lock, int lock_size)
{
    int my_pe = shmem_my_pe();
    int server_pe = shmem_lock_get_server(lock);
    int zero = 0;
    int incorrect_pe = -1;
    uint64_t remote_value = 0;

    shmem_lock_pack_pe_next_pe_last(&remote_value,
                                    lock_size,
                                    &incorrect_pe,
                                    &my_pe);
    return !(remote_value
            == shmem_cswap(lock, lock_size, remote_value, zero, server_pe));
}

/***************************************************************************/
/**************************API wrappers*************************************/
/***************************************************************************/

void _shmem_set_lock(void *lock, int lock_size)
{
    int my_pe = shmem_my_pe();

    if (!shmem_lock_is_mine(lock, lock_size)) {
        int has_lock = !_shmem_test_lock(lock, lock_size);

        if (!has_lock) {
            int pe_last = -1;
            int ticket = shmem_lock_get_ticket(lock);

            shmem_lock_increment_counter(lock, lock_size);
            shmem_lock_wait_for_ticket(lock, lock_size, ticket, &pe_last);
            if (pe_last != my_pe) {
                shmem_lock_subscribe_for_informing(lock, lock_size, pe_last);
                shmem_lock_wait_for_informing(lock, lock_size);
            }
        }
    } else {
        shmem_lock_increment_counter(lock, lock_size);
    }
}

int _shmem_test_lock(void *lock, int lock_size)
{
    int status = 1;
    int server_pe = shmem_lock_get_server(lock);
    int incorrect_value = -1;
    int my_pe = shmem_my_pe();
    uint64_t new_lock_value = 0;
    uint64_t prev_lock_value = 1;
    int my_lock = shmem_lock_is_mine(lock, lock_size);

    shmem_lock_increment_counter(lock, lock_size);
    if (!my_lock) {
        if (shmem_lock_pack_pe_next_pe_last(&new_lock_value,
                                            lock_size,
                                            &incorrect_value,
                                            &my_pe)) {
            goto FreeMemory;
        }

        prev_lock_value = shmem_cswap(lock,
                                      lock_size,
                                      0,
                                      new_lock_value,
                                      server_pe);
    }

    if (0 == prev_lock_value || my_lock) {
        status = 0;
    } else {
        shmem_lock_decrement_counter(lock, lock_size);
    }

    FreeMemory: return status;
}

void _shmem_clear_lock(void *lock, int lock_size)
{
    int current_lock_counter = shmem_lock_decrement_counter(lock, lock_size);

    if (0 == current_lock_counter) {
        int next_informed = 0;
        int pe_next = 0;

        while (!next_informed) {
            shmem_lock_extract_pe_next(lock, lock_size, &pe_next);
            if (pe_next >= 0) {
                shmem_lock_inform_next(lock, lock_size, pe_next);
                next_informed = 1;
            } else {
                /* It seems I'm the last in queue */
                if (!shmem_lock_try_inform_server(lock, lock_size)) {
                    next_informed = 1;
                }
            }
        }

        pe_next = -1;
        shmem_lock_pack_pe_next(lock, lock_size, &pe_next);
    }
}
