//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Copyright (C) 2010  Sebastian Mach (*1983)
// * mail: phresnel/at/gmail/dot/com
// * http://phresnel.org
// * http://picogen.org
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "GLDisplayListMesh.hh"
#include <QImage>
#include <QColor>

using namespace xyto;

namespace {
        GLuint loadTexture (const char* filename) {
                GLuint id;
                glGenTextures(1, &id);
                glBindTexture(GL_TEXTURE_2D, id);

                QImage img (filename);

                // make it 2^n
                int w=1, h=1;
                while (img.width()>w)
                        w *= 2;
                while (img.width()>h)
                        h *= 2;
                img = img.scaled(w,h,Qt::IgnoreAspectRatio, Qt::SmoothTransformation);

                unsigned char *foo = new unsigned char [img.width()*img.height()*4];
                for (int y=0; y<img.height(); ++y) {
                        for (int x=0; x<img.width(); ++x) {
                                QRgb const & col = img.pixel(x, y);

                                unsigned char * const p = &foo[(x+img.width()*y)*4];
                                p[0] = qRed(col);
                                p[1] = qGreen(col);
                                p[2] = qBlue(col);
                                p[3] = qAlpha(col);
                        }
                }
                gluBuild2DMipmaps(GL_TEXTURE_2D, 4, img.width(), img.height(),
                                  GL_RGBA, GL_UNSIGNED_BYTE, foo);
                delete [] foo;

                glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
                glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                                 GL_LINEAR_MIPMAP_LINEAR);
                glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
                glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
                glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

                glTexEnvf(GL_TEXTURE_FILTER_CONTROL, GL_TEXTURE_LOD_BIAS, -4);
                return id;
        }
}



GLDisplayListMesh::GLDisplayListMesh () {
        textures_.push_back(loadTexture("bark.jpg"));
        textures_.push_back(loadTexture("leaf.png"));

        displayList_ = glGenLists(1);
        glNewList(displayList_, GL_COMPILE);
        glBindTexture(GL_TEXTURE_2D, textures_[0]);
        glEnable(GL_CULL_FACE);
}



GLDisplayListMesh::~GLDisplayListMesh () {
        glEndList();
}



void GLDisplayListMesh::moveTo (Turtle state) {
        this->state = state;
}



void GLDisplayListMesh::drawTo (Turtle newState) {
        using std::fabs;
        using std::acos;

        const double pi = 3.14159, pi2 = pi*2;
        int count = 7;


        const double fdot = dot (state.rotation.forward(),
                                 newState.rotation.forward());
        if (fabs(fdot) < 0.999) {
                const TurtleVector axis =
                                normalize(cross(
                                        state.rotation.forward(),
                                        newState.rotation.forward()));
                const double angle = acos(fdot);
                newState.rotation = state.rotation
                                    *
                                    TurtleMatrix::Rotate(angle, axis)
                                    ;
        } else {
                newState.rotation = state.rotation;
        }

        const double oldTexV = state.pathLength    * 0.01,
                     newTexV = newState.pathLength * 0.01;

        glBegin(GL_QUAD_STRIP);
        for (int i=0; i<=count; ++i) {
                const double f = i / (float)count;
                const double phi = f * pi2;
                const TurtleVector oldV = state.disk(phi) * state.scale;
                const TurtleVector newV = newState.disk(phi) * newState.scale;
                const TurtleVector oldN = state.normal(phi);
                const TurtleVector newN = newState.normal(phi);

                glTexCoord2f(f, newTexV);
                glNormal3f(newN.x, newN.y, newN.z);
                glVertex3f(newV.x, newV.y, newV.z);

                glTexCoord2f(f, oldTexV);
                glNormal3f(oldN.x, oldN.y, oldN.z);
                glVertex3f(oldV.x, oldV.y, oldV.z);
        }
        glEnd();

        state = newState;
}



void GLDisplayListMesh::leaf() {
        Turtle state = this->state;
        state.rollToVertical();
        state.rollLeft(0.3*((rand()/(double)RAND_MAX)-0.5));
        state.up(0.1*((rand()/(double)RAND_MAX)-0.5));

        TurtleVector head = state.heading();
        /*head.y = 0;
        head = normalize(head);*/
        //TurtleVector right (head.z, 0, -head.x);
        TurtleVector right = state.right();

        const double s = 40;
        const TurtleVector
                        A = (state.position + right*-s) * state.scale,
                        B = (state.position + right*-s + head*s) * state.scale,
                        C = (state.position + right*s + head*s) * state.scale,
                        D = (state.position + right*s) * state.scale;

        glEnable (GL_BLEND);
        glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        glDisable(GL_CULL_FACE);
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GREATER, 0.1);
        glBindTexture(GL_TEXTURE_2D, textures_[1]);

        glBegin(GL_QUADS);
        glNormal3d(0,1,0);
        glTexCoord2d(0, 1);
        glVertex3f(A.x, A.y, A.z);
        glTexCoord2d(0, 0);
        glVertex3f(B.x, B.y, B.z);
        glTexCoord2d(1, 0);
        glVertex3f(C.x, C.y, C.z);
        glTexCoord2d(1, 1);
        glVertex3f(D.x, D.y, D.z);
        glEnd();

        glBindTexture(GL_TEXTURE_2D, textures_[0]);
        glDisable(GL_ALPHA_TEST);
        glEnable(GL_CULL_FACE);
}



GLuint GLDisplayListMesh::displayList() const {
        return displayList_;
}



std::vector<GLuint> GLDisplayListMesh::textures() const {
        return textures_;
}



void GLDisplayListMesh::pushState() {
        stateStack.push(state);
}



void GLDisplayListMesh::popState() {
        state = stateStack.top();
        stateStack.pop();
}
