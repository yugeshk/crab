add_executable(crab crab.cc)

set (CRAB_LIBS 
   OS
   Term
   Debug
   Stats
   )

target_link_libraries (crab ${CRAB_LIBS} 
  ${LDD_LIBRARY} 
  ${APRON_LIBRARY}
  ${MPFR_LIBRARIES} 
  ${GMPXX_LIB} 
  ${GMP_LIB} 
  ${RT_LIB}
  ### begin Python
  ${PYTHON_LIBRARIES}
  ##  XXX: linker is happy
  ${PYTHON_DEPS}
  ## XXX: linker is not happy
  # ${DL_LIB} 
  # ${Z_LIB} 
  # ${UTIL_LIB} 
  # ${PTHREAD_LIB}
  ### end Python
  ${Boost_PYTHON_LIBRARY} 
  ${Boost_SYSTEM_LIBRARY} 
  ${Boost_FILESYSTEM_LIBRARY} 
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  )

install(TARGETS crab RUNTIME DESTINATION bin)

find_program(PYTHON "python")
option(USE_PY_SETUP "Use python distutils to install python modules" OFF)

if (PYTHON AND NOT USE_PY_SETUP)
  install(PROGRAMS crabParser.py DESTINATION bin)
  install(PROGRAMS crabTypes.py DESTINATION bin)
endif()

# if (PYTHON AND USE_PY_SETUP)
#   set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in")
#   set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
#   set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp")

#   set(DEPS crabParser.py ${SETUP_PY_IN})
    
#   configure_file(${SETUP_PY_IN} ${SETUP_PY})

#   add_custom_command(OUTPUT ${OUTPUT}
#     COMMAND ${PYTHON} ${SETUP_PY} build
#     COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
#     DEPENDS ${DEPS})

#   add_custom_target(python ALL DEPENDS ${OUTPUT})

#   install(CODE "execute_process(COMMAND ${PYTHON} ${SETUP_PY} install --install-lib ${CMAKE_INSTALL_PREFIX}/bin --install-scripts ${CMAKE_INSTALL_PREFIX}/bin)")

# endif()




