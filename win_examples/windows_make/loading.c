/* Prabhanjan Kambadur: Open Systems Lab
   Example documenting the usage of functions from a dynamic library in windows
   September 24 2004 */
 
#include <stdio.h> 
#include <windows.h> 
#include "component.h"
  
void main(void) 
{ 
    HINSTANCE hinstLib; 
    struct component *component;
    struct module *module;
                    
    /* Get a handle to the DLL */
    
    hinstLib = LoadLibrary("component"); 
                              
    /* If the handle is valid, try to get the function address */
                                   
    if (hinstLib != NULL) {
        component = (struct component *) GetProcAddress(hinstLib,
                                                        "component_instance"); 
         
         /* If the function address is valid, call the function */
                                                                   
         if (NULL != component) {
             module = component->component_query_fn(37);
             module->module_query_fn(17);
         } else {
             printf("Was not able to find component_instance\n");
         }
    } else {
        printf("Was not able to LoadLibrary\n");
    }
}
