
cmake_minimum_required (VERSION 2.6)
project (xyto)

MACRO(priinclude subdir)
        set (old_pwd ${pwd})
        set (pwd "${subdir}")
        include("${subdir}/include.cmake")
        set (pwd ${old_pwd})
ENDMACRO(priinclude)

set(sources)
priinclude(../)



if (CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
        add_definitions (-Wall -Wextra -D__STDC_LIMIT_MACROS)
endif()

include_directories (
        ../                                # auxiliary
        ../../../renderer/redshift/include/   # redshift
)

add_executable (xyto  EXCLUDE_FROM_ALL  ${sources} ../main.cpp)
add_library (libxyto  ${sources})
