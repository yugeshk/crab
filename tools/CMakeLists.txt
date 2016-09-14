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





