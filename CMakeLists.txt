cmake_minimum_required(VERSION 3.5.1)
project(libgit2-el LANGUAGES C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake")
set(CMAKE_POSITION_INDEPENDENT_CODE TRUE CACHE BOOL "pic" FORCE)
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DEGIT_DEBUG")

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR} CACHE PATH "package-user-dir" FORCE)
endif()

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()

include_directories($ENV{HOME}/.local/include)
link_directories($ENV{HOME}/.local/lib)

add_subdirectory(src)
install(TARGETS git2-el)

if(BUILD_TESTS)
	enable_testing()
	# NOTE: These test names must be replicated in .appveyor-script.ps1
	# for the Windows CI. Fixing this is still TODO.
	set(EGIT_TESTS
	  annotated-commit
	  blame
	  blob
	  branch
	  checkout
	  cherrypick
	  commit
	  config
	  describe
	  diff
	  graph
	  ignore
	  index
	  merge
	  message
	  pathspec
	  reference
	  reflog
	  remote
	  repository
	  reset
	  revert
	  revparse
	  revwalk
	  signature
	  status
	  submodule
	  tag
	  tree)

	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
	  list(APPEND EGIT_TESTS refcount)
	endif(CMAKE_BUILD_TYPE STREQUAL "Debug")

	foreach(test ${EGIT_TESTS})
	  add_test(NAME libgit2-el_${test} COMMAND
	    emacs --batch
		  -L "${CMAKE_CURRENT_BINARY_DIR}"
		  -L "${CMAKE_CURRENT_SOURCE_DIR}/test"
		  -l libgit2-el
		  -l test-helper
		  -l ${test}-test
		  -f ert-run-tests-batch-and-exit)
	endforeach(test)
endif()
