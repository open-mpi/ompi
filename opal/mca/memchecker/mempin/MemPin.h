#ifndef __MEMPIN_H__
#define __MEMPIN_H__

#include <stddef.h>


/* mem watch types */
#define MEMPIN_WATCH_READ  0
#define MEMPIN_WATCH_WRITE 1
#define MEMPIN_WATCH_RW    2

/* callback return value operation */
#define MEMPIN_CALLBACK_NULL                0
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_1   1
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_2   2
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_3   3
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_4   4
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_5   5
#define MEMPIN_CALLBACK_PRINT_CALLSTACK_ALL 100
/* XXX define more return callback options */

/* XXX this should be replaced with a proper check or configure/cmake-based */
#ifdef __GNUC__
#define mp_unused __attribute__((unused)) /* get rid of compiler warnings */
#define mp_weak   __attribute__((weak))   /* try to have multiple local versions being replaced */
#else
#define mp_unused
#define mp_weak
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* Format of reg/unreg functions:
 *    mempin_reg_mem(addr, size, watch_type, cb_func, cb_args);
 *    MEMPIN_REG_MEM_WATCH(void* addr,
 *                         int size,
 *                         int op,
 *                         void* cb_func,  -- of type mempin_reg_cb_t
 *                         void* cb_args);
 *    mempin_unreg_mem(addr, size);
 * !!!! These can NOT be used in callbacks from MemPin !!!!
 *
 * Format of the callback functions is:
 *     int MEMPIN_REG_CB_T(
 *        void * addr,
 *        size_t size,
 *        int offset,
 *        int is_write,
 *        void * cb_info,
 *        void * ip);
 * Return value:
 *   The return value selects the action in the MemPin Tool:
 *     MEMPIN_CALLBACK_NULL
 *     MEMPIN_CALLBACK_PRINT_CALLSTACK_1
 *     and so on (see above)
 */
typedef int (*MEMPIN_REG_CB_T)(void*, size_t, int, int, void*, void*);

volatile static int MEMPIN_hardcode = 42;


void MEMPIN_RUNNING_WITH_PIN(int *pin_alive) mp_unused mp_weak;
void MEMPIN_RUNNING_WITH_PIN(int *pin_alive)
{if (NULL != pin_alive) *pin_alive = MEMPIN_hardcode;}

void MEMPIN_REG_MEM_WATCH(const void* addr mp_unused, int size mp_unused, int op mp_unused, MEMPIN_REG_CB_T cb_func mp_unused, void * cb_args mp_unused) mp_unused mp_weak;
void MEMPIN_REG_MEM_WATCH(const void* addr, int size, int op, MEMPIN_REG_CB_T cb_func, void* cb_args)
{}

void MEMPIN_UNREG_MEM_WATCH(const void* addr mp_unused, int size mp_unused) mp_unused mp_weak;
void MEMPIN_UNREG_MEM_WATCH(const void* addr, int size)
{}

void MEMPIN_UNREG_ALL_MEM_WATCH(void) mp_unused mp_weak;
void MEMPIN_UNREG_ALL_MEM_WATCH(void)
{}

void MEMPIN_SEARCH_MEM_INDEX(const void* addr mp_unused, int size mp_unused, size_t *index mp_unused) mp_unused mp_weak;
void MEMPIN_SEARCH_MEM_INDEX(const void* addr, int size, size_t *index)
{}

int MEMPIN_MEM_WATCH_COUNT(void) mp_unused mp_weak;
int MEMPIN_MEM_WATCH_COUNT(void)
{return MEMPIN_hardcode;}

void MEMPIN_PRINT_CALLSTACK(void) mp_unused mp_weak;
void MEMPIN_PRINT_CALLSTACK(void)
{}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* __MEMPIN_H__ */

