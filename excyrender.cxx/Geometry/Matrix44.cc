// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include "Matrix44.hh"
#include <cstring>


//- Tests ------------------------------------------------------------------------------------------
#include "catch.hpp"
TEST_CASE( "Geometry/Matrix44", "Matrix 4x4" ) {
    using namespace excyrender::Geometry;

    REQUIRE(Matrix44() == Matrix44(1,0,0,0,
                                   0,1,0,0,
                                   0,0,1,0,
                                   0,0,0,1));

    REQUIRE(Matrix44() != Matrix44(1,0,0,0,
                                   0,1,0,0,
                                   0,0,1,0,
                                   1,0,0,1));

    REQUIRE(Matrix44() == inverse(Matrix44()));

    REQUIRE(Matrix44() == (Matrix44() * inverse(Matrix44())));

    REQUIRE(Matrix44( 1,  2,  3,  4,
                      5,  6,  7,  8,
                      9, 10, 11, 12,
                     13, 14, 15, 16)
            == transpose(Matrix44( 1,  5,  9, 13,
                                   2,  6, 10, 14,
                                   3,  7, 11, 15,
                                   4,  8, 12, 16)));
}
//--------------------------------------------------------------------------------------------------



namespace excyrender { namespace Geometry {

Matrix44 inverse(Matrix44 const &m) {
    // Ported from PBRT:transform.cpp

    int indxc[4], indxr[4];
    int ipiv[4] = { 0, 0, 0, 0 };
    Matrix44 minv;
    memcpy(minv.m, m.m, 4*4*sizeof(real));
    for (int i = 0; i < 4; i++) {
        int irow = -1, icol = -1;
        real big = 0.;
        // Choose pivot
        for (int j = 0; j < 4; j++) {
            if (ipiv[j] != 1) {
                for (int k = 0; k < 4; k++) {
                    if (ipiv[k] == 0) {
                        if (fabs(minv.m[j*4+k]) >= big) {
                            big = real(fabsf(minv.m[j*4+k]));
                            irow = j;
                            icol = k;
                        }
                    }
                    else if (ipiv[k] > 1)
                        throw std::runtime_error("Singular matrix in MatrixInvert");
                }
            }
        }
        ++ipiv[icol];
        // Swap rows _irow_ and _icol_ for pivot
        if (irow != icol) {
            for (int k = 0; k < 4; ++k)
                swap(minv.m[irow*4+k], minv.m[icol*4+k]);
        }
        indxr[i] = irow;
        indxc[i] = icol;
        if (minv.m[icol*4+icol] == 0.)
            throw std::runtime_error("Singular matrix in MatrixInvert");

        // Set $m[icol][icol]$ to one by scaling row _icol_ appropriately
        real pivinv = 1.f / minv.m[icol*4+icol];
        minv.m[icol*4+icol] = 1.f;
        for (int j = 0; j < 4; j++)
            minv.m[icol*4+j] *= pivinv;

        // Subtract this row from others to zero out their columns
        for (int j = 0; j < 4; j++) {
            if (j != icol) {
                real save = minv.m[j*4+icol];
                minv.m[j*4+icol] = 0;
                for (int k = 0; k < 4; k++)
                    minv.m[j*4+k] -= minv.m[icol*4+k]*save;
            }
        }
    }
    // Swap columns to reflect permutation
    for (int j = 3; j >= 0; j--) {
        if (indxr[j] != indxc[j]) {
            for (int k = 0; k < 4; k++)
                swap(minv.m[k*4+indxr[j]], minv.m[k*4+indxc[j]]);
        }
    }
    return minv;
}

} }

