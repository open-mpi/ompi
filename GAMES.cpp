# If necessary, use the RELATIVE flag,otherwise each source file may be listed
# with full pathname. RELATIVEmay makes it easier to extract an excutable name
# automatically
file( GLOB APP_SOURCES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.c})
#file( GLOB APP_SOURCES ${CMAKE_SOURCE_DIR}/*.c})
#AUX_SOURCE_DIRECTORY(${CMAKE_SOURCE_DIR} APP_SOURCE)

foreach( testsourcefile ${APP_SOURCES} )
   string( REPLACE ".c" "" testname ${testsourcefile} )
   string(  REPLACE ".c" "" testname ${testname} )
    string(  REPLACE "" "_" testname ${testname} )
    
    add_executable( ${testname} ${testsource} )
    
    if(openMP_C_FOUND)
           target_link_libraries(${testname} OpenMP::OpenMP_C)
    endif()
    
    if(MATH_LIBRARY)
           target_link_libraries(${testname} ${MATH_LIBRARY})
    endif()
	install(TARGETS ${testname} DESTINATION "bin/games")
	
endforeach( testsourcefile ${APP_SOURCES} )
