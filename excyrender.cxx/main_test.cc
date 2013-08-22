#define CATCH_CONFIG_RUNNER
#include "catch.hpp"


int unit_tests(int argc, char *argv[]) {
    // see also https://github.com/philsquared/Catch/blob/master/docs/own-main.md
    return Catch::Session().run(argc, argv);
}
