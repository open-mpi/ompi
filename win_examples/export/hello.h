#ifndef HELLO_H
#define HELLO_H

#if !(defined(__cplusplus) || defined(c_plusplus))
typedef enum {false, true} bool;
#endif

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

typedef int (*one_fn)(void);
typedef int (*two_fn)(void);


struct hello_t {
    one_fn i; 
    two_fn j;
};
typedef struct hello_t hello_t;


#ifdef EXPORTING
    __declspec(dllexport) extern hello_t anju;
    __declspec(dllexport) extern bool aaa;
#else
    __declspec(dllimport) extern hello_t anju;
    __declspec(dllimport) extern bool aaa;
#endif

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /*HELLO_H*/
