/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_BASE_ERROR_H
#define MCA_BTL_BASE_ERROR_H


#if defined(ACCEPT_C99) && __STDC_VERSION__ >= 199901L
#   define BTL_ERROR(fmt, ...) {                                           \
    opal_output(0, "[%s:%d:%d] my_name: [%lu,%lu,%lu] " fmt, __FILE__, __LINE__, __func__, \
               ORTE_NAME_ARGS(orte_process_info.my_name), __VA_ARGS__); \
    }
#else
# if defined(__GNUC__) && !defined(__STDC__) 
#define BTL_ERROR(fmt, args...) {                                           \
    opal_output(0, "[%s:%d:%d] my_name: [%lu,%lu,%lu]" fmt, __FILE__, __LINE__, __func__,\
               ORTE_NAME_ARGS(orte_process_info.my_name), ##args); \
    }    
#else 
static inline void BTL_ERROR(char *fmt, ... ) 
{ 
    va_list list; 
    va_start(list, fmt); 
    opal_output(0, "[%s:%d:%d] my_name: [%lu,%lu,%lu]", fmt, __FILE__, __LINE__, __func__,
                ORTE_NAME_ARGS(orte_process_info.my_name), list); 
    va_end(list); 
}
#define BTL_ERROR printf 
#endif 
#endif 
#ifdef BTL_DEBUG_OUT 
#if defined(ACCEPT_C99) && __STDC_VERSION__ >= 199901L
#   define BTL_DEBUG_OUT(fmt, ...) {                                           \
    opal_output(0, "[%s:%d:%d " fmt, __FILE__, __LINE__, __func__, __VA_ARGS__); \
    }
#else
# if defined(__GNUC__) && !defined(__STDC__) 
#define BTL_DEBUG_OUT(fmt, args...) {                                           \
    opal_output(0, "[%s:%d:%d " fmt, __FILE__, __LINE__, __func__, ##args); \
    }    
#else 
static inline void BTL_DEBUG_OUT(char *fmt, ... ) 
{ 
    va_list list; 
    va_start(list, fmt); 
    opal_output(0, "[%s:%d:%d ", fmt, __FILE__, __LINE__, __func__, list);
    va_end(list); 
}
#define BTL_DEBUG_OUT printf 
#endif 
#endif 
#else 
#if defined(ACCEPT_C99) && __STDC_VERSION__ >= 199901L
#   define BTL_DEBUG_OUT(fmt, ...) 
#else
# if defined(__GNUC__) && !defined(__STDC__) 
#define BTL_DEBUG_OUT(fmt, args...) 
#else 
static inline void BTL_DEBUG_OUT(char *fmt, ... ) 
{ 
}
#define BTL_DEBUG_OUT printf 
#endif 
#endif 
#endif 

#endif
