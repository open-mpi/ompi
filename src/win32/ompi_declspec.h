#ifndef OMPI_DECLSPEC_H
#define OMPI_DECLSPEC_H

#ifdef OMPI_BUILDING_LIBRARY
#define OMPI_DECLSPEC __declspec(dllexport)
#else
#define OMPI_DECLSPEC __declspec(dllimport)
#endif
#define OMPI_COMP_EXPORT __declspec(dllexport)
#define OMPI_WINDOWS
#endif
