#include <stdio.h>

#include "component.h"

static struct module *component_query(int i);
static void module_query(int i);


/* This is the component struct, and is exported */
//__declspec(dllexport) 
__declspec(dllexport) struct component component_instance = {
    component_query
};

/* This is the module struct, and is static */
static struct module module_instance = {
    module_query
};

static struct module *component_query(int i)
{
    printf("this is the component query: I got value %d\n", i);
    return &module_instance;
}

static void module_query(int i)
{
    printf("this is the module query: I got value %d\n", i);
}
