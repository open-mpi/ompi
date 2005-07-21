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


#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#   define BTL_ERROR(fmt, ...) {                                           \
    opal_output(0, "[%s:%d:%s] my_name: [%lu,%lu,%lu] " fmt "\n", __FILE__, __LINE__, __func__, \
               ORTE_NAME_ARGS(orte_process_info.my_name), __VA_ARGS__); \
    }
#else
# if defined(__GNUC__) && !defined(__STDC__) 
#define BTL_ERROR(fmt, args...) {                                           \
    opal_output(0, "[%s:%d:%s] my_name: [%lu,%lu,%lu]" fmt "\n", __FILE__, __LINE__, __func__,\
               ORTE_NAME_ARGS(orte_process_info.my_name), ##args); \
    }    
#else 
static inline void BTL_ERROR(char *fmt, ... ) 
{ 
    va_list list; 
    va_start(list, fmt); 
    fprintf(stderr,"[%s:%d:%s] my_name: [%lu,%lu,%lu]", 
            __FILE__, __LINE__, __func__,
            ORTE_NAME_ARGS(orte_process_info.my_name)); 
    
    vfprintf(stderr, fmt, list);
    fprintf(stderr, "\n"); 
    va_end(list); 
}
#endif 
#endif 
#if 0 
 #if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901)L
  #   define BTL_DEBUG_OUT(fmt, ...) {                                           \
       opal_output(0, "[%s:%d:%s] " fmt "\n", __FILE__, __LINE__, __func__, __VA_ARGS__); \
       }
  #else
   # if defined(__GNUC__) && !defined(__STDC__) 
     #define BTL_DEBUG_OUT(fmt, args...) {                                           \
       opal_output(0, "[%s:%d:%s] " fmt "\n", __FILE__, __LINE__, __func__, ##args); \
      }    
   #else 
    static inline void BTL_DEBUG_OUT(char *fmt, ... ) 
    { 
      va_list list; 
      va_start(list, fmt); 
      fprintf(stderr, "[%s:%d:%s]",  __FILE__, __LINE__, __func__, list);
      vfprintf(stderr, fmt, list);
      vfpritnf(stderr, "\n"); 
      va_end(list); 
    }
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
  #endif 
 #endif 
#endif 

#endif
