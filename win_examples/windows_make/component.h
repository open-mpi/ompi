#ifndef COMPONENT_H
#define COMPONENT_H

struct component;
struct module;

typedef struct module *(*component_query_fn_t)(int i);

struct component {
    component_query_fn_t component_query_fn;
};

typedef void (*module_query_fn_t)(int i);

struct module {
    module_query_fn_t module_query_fn;
};

#endif
