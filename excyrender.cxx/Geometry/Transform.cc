// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#include "Transform.hh"


//- Tests ------------------------------------------------------------------------------------------
#include "catch.hpp"
TEST_CASE( "Geometry/Transform", "Transformations" ) {
    using namespace excyrender::Geometry;
}
//--------------------------------------------------------------------------------------------------


namespace excyrender { namespace Geometry {

Transform Transform::Translate(const Vector &delta) {
    Matrix44 m(1, 0, 0, delta.x,
                0, 1, 0, delta.y,
                0, 0, 1, delta.z,
                0, 0, 0,       1);
    Matrix44 minv(1, 0, 0, -delta.x,
                   0, 1, 0, -delta.y,
                   0, 0, 1, -delta.z,
                   0, 0, 0,        1);
    return Transform(m, minv);
}


Transform Transform::Scale(real x, real y, real z) {
    Matrix44 m(x, 0, 0, 0,
                0, y, 0, 0,
                0, 0, z, 0,
                0, 0, 0, 1);
    Matrix44 minv(1.f/x,     0,     0,     0,
                   0,     1.f/y,     0,     0,
                   0,         0,     1.f/z, 0,
                   0,         0,     0,     1);
    return Transform(m, minv);
}


Transform Transform::RotateX(Angle angle) {
    real sin_t = sin(real(Radians(angle)));
    real cos_t = cos(real(Radians(angle)));
    Matrix44 m(1,     0,      0, 0,
                0, cos_t, -sin_t, 0,
                0, sin_t,  cos_t, 0,
                0,     0,      0, 1);
    return Transform(m, transpose(m));
}


Transform Transform::RotateY(Angle angle) {
    real sin_t = sin(real(Radians(angle)));
    real cos_t = cos(real(Radians(angle)));
    Matrix44 m( cos_t,  0,  sin_t, 0,
                 0,     1,      0, 0,
                -sin_t, 0,  cos_t, 0,
                 0,     0,   0,    1);
    return Transform(m, transpose(m));
}



Transform Transform::RotateZ(Angle angle) {
    real sin_t = sin(real(Radians(angle)));
    real cos_t = cos(real(Radians(angle)));
    Matrix44 m(cos_t, -sin_t, 0, 0,
                sin_t,  cos_t, 0, 0,
                0,      0, 1, 0,
                0,      0, 0, 1);
    return Transform(m, transpose(m));
}


Transform Transform::Rotate(Angle angle, const Vector &axis) {
    Vector a = normalize(axis);
    real s = sin(real(Radians(angle)));
    real c = cos(real(Radians(angle)));

    Matrix44 mat(a.x * a.x + (1 - a.x * a.x) * c,
                 a.x * a.y * (1 - c) - a.z * s,
                 a.x * a.z * (1 - c) + a.y * s,
                 0,

                 a.x * a.y * (1 - c) + a.z * s,
                 a.y * a.y + (1 - a.y * a.y) * c,
                 a.y * a.z * (1 - c) - a.x * s,
                 0,

                 a.x * a.z * (1 - c) - a.y * s,
                 a.y * a.z * (1 - c) + a.x * s,
                 a.z * a.z + (1 - a.z * a.z) * c,
                 0,

                 0,
                 0,
                 0,
                 1);

    return Transform(mat, transpose(mat));
}


Transform Transform::LookAt(const Point &pos, const Point &look, const Vector &up) {

    Vector dir = normalize(look - pos);
    Vector left = normalize(cross(normalize(up), dir));
    Vector newUp = cross(dir, left);

    Matrix44 camToWorld(left.x, newUp.x, dir.x, pos.x,
                        left.y, newUp.y, dir.y, pos.y,
                        left.z, newUp.z, dir.z, pos.z,
                        0,      0,       0,     1      );
    return Transform(inverse(camToWorld), camToWorld);
}


Transform Transform::Orthographic(real znear, real zfar) {
    return Transform::Scale(1, 1, 1 / (zfar-znear)) *
           Transform::Translate(Vector(0, 0, -znear));
}


Transform Transform::Perspective(Angle fov, real n, real f) {
    // Perform projective divide
    Matrix44 persp {1, 0,       0,            0,
                    0, 1,       0,            0,
                    0, 0, f/(f-n), -f*n / (f-n),
                    0, 0,       1,            0};

    // Scale to canonical viewing volume
    real invTanAng = 1 / tan((real)Radians(fov) / 2);
    return Transform::Scale(invTanAng, invTanAng, 1) * Transform(persp);
}


} }

