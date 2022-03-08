file(GLOB GIT2_EL_SRCS *.c)

add_library(git2-el SHARED ${GIT2_EL_SRCS})
set_target_properties(git2-el PROPERTIES C_STANDARD 99)

# Emacs looks for .so on linux and OSX.
# By default CMake builds .dylib on OSX, so override that.
if(APPLE)
  set_target_properties(git2-el PROPERTIES SUFFIX .so)
endif(APPLE)

if(WIN32)
  set_target_properties(git2-el PROPERTIES PREFIX lib)
endif(WIN32)

target_link_libraries(git2-el git2)

if(CMAKE_COMPILER_IS_GNUCC)
  target_compile_options(git2-el PRIVATE -Wall -Wextra)
endif(CMAKE_COMPILER_IS_GNUCC)

add_custom_command(TARGET git2-el POST_BUILD COMMAND
  ${CMAKE_COMMAND} -E copy $<TARGET_FILE:git2-el> "${CMAKE_CURRENT_BINARY_DIR}/..")
