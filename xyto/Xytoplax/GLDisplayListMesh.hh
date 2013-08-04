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

#ifndef GLDISPLAYLISTMESH_HH
#define GLDISPLAYLISTMESH_HH

#include <stack>
#include <vector>

#include "../turtle.hh"

#include "GL/gl.h"
#include "GL/glu.h"

#ifndef GL_TEXTURE_FILTER_CONTROL
#define GL_TEXTURE_FILTER_CONTROL         0x8500
#endif
#ifndef GL_TEXTURE_LOD_BIAS
#define GL_TEXTURE_LOD_BIAS               0x8501
#endif


class GLDisplayListMesh {
public:
        GLDisplayListMesh ();
        ~GLDisplayListMesh ();
        void moveTo (xyto::Turtle state);
        void drawTo (xyto::Turtle newState);
        void leaf();
        GLuint displayList() const;
        std::vector<GLuint> textures() const;
        void pushState();
        void popState();

private:
        xyto::Turtle state;
        std::stack<xyto::Turtle> stateStack;
        GLuint displayList_;
        std::vector<GLuint> textures_;
};


#endif // GLDISPLAYLISTMESH_HH
