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

#ifndef DRAW_HH_INCLUDED_20100805
#define DRAW_HH_INCLUDED_20100805

#include "lsystem.hh"
#include "turtle.hh"

namespace xyto { 

template <typename T>
inline void draw (LSystem lsys, Pattern pat, Turtle turtle, T &mesh) {
        turtle.scale = 1;
        if (lsys.constantExists("#scale"))
                turtle.scale = lsys.constant("#scale").toReal();
        if (lsys.constantExists("#diascale"))
                turtle.diameterScale = lsys.constant("#diascale").toReal();
        bool first = true;
        return draw_(pat, turtle, mesh, first);
}

template <typename T>
inline void draw_ (Pattern pat, Turtle turtle, T &mesh, bool &first) {
        typedef Pattern::const_iterator It;

        bool lastWasBranch = false;

        for (It it = pat.begin(); it!=pat.end(); ++it) {
                Segment seg = *it;
                if (seg.type() == Segment::Branch) {
                        mesh.pushState();
                        draw_ (seg.branch(), turtle, mesh, first);
                        mesh.popState();
                        lastWasBranch = true;
                } else if (seg.type() == Segment::Letter) {
                        if (seg.name() == "left") {
                                if (!seg.parameterList().empty()) {
                                        turtle.turnLeft(seg.parameterList()[0].toReal() * 0.0174532925);
                                } else {
                                        turtle.turnLeft(0.5);
                                }
                        } else if (seg.name() == "right") {
                                if (!seg.parameterList().empty())
                                        turtle.turnRight(seg.parameterList()[0].toReal() * 0.0174532925);
                                else
                                        turtle.turnRight(0.5);
                        } else if (seg.name() == "up") {
                                if (!seg.parameterList().empty()) {
                                        turtle.pitchUp(seg.parameterList()[0].toReal() * 0.0174532925);
                                } else {
                                        turtle.pitchUp(0.5);
                                }
                        } else if (seg.name() == "down") {
                                if (!seg.parameterList().empty())
                                        turtle.pitchDown(seg.parameterList()[0].toReal() * 0.0174532925);
                                else
                                        turtle.pitchDown(0.5);
                        } else if (seg.name() == "rollleft") {
                                if (!seg.parameterList().empty()) {
                                        turtle.rollLeft(seg.parameterList()[0].toReal() * 0.0174532925);
                                } else {
                                        turtle.rollLeft(0.5);
                                }
                        } else if (seg.name() == "rollright") {
                                if (!seg.parameterList().empty())
                                        turtle.rollRight(seg.parameterList()[0].toReal() * 0.0174532925);
                                else
                                        turtle.rollRight(0.5);
                        } else if (seg.name() == "vert") {
                                turtle.rollToVertical();
                        } else if (seg.name() == "dia") {
                                if (!seg.parameterList().empty())
                                        turtle.decrementDiameter(seg.parameterList()[0].toReal());
                        } else if (seg.name() == "f"){
                                const Turtle oldBoy = turtle;

                                if (!seg.parameterList().empty()) {
                                        turtle.forward(seg.parameterList()[0].toReal());
                                } else {
                                        turtle.forward(1);
                                }

                                if (first) {
                                        mesh.moveTo(oldBoy);
                                        first = false;
                                }
                                mesh.drawTo (turtle);
                                lastWasBranch = false;
                        }
                }
        }
        if (!lastWasBranch)
                mesh.leaf();
}

}

#endif // DRAW_HH_INCLUDED_20100805
