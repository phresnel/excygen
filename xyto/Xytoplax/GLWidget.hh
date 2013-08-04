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

#ifndef GLWIDGET_HH
#define GLWIDGET_HH

#include <QtOpenGL>
#include <QGLWidget>

namespace xyto {
    class Pattern;
    class LSystem;
}

class GLWidget : public QGLWidget
{
        Q_OBJECT
public:
        GLWidget(QWidget *parent = 0);
        ~GLWidget();

        QSize minimumSizeHint() const;
        QSize sizeHint() const;

        void updateData (xyto::LSystem const &lsys, xyto::Pattern const &pat);
        void setWireframe (bool enable);
public slots:
        void setXRotation(int angle);
        void setYRotation(int angle);
        void setZRotation(int angle);
protected:
        void initializeGL();
        void paintGL();
        void resizeGL(int width, int height);
        void mousePressEvent(QMouseEvent *event);
        void mouseMoveEvent(QMouseEvent *event);
        void wheelEvent (QWheelEvent *);
private:
        int xRot;
        int yRot;
        int zRot;
        double xPos, yPos, zPos;
        QPoint lastPos;
        QColor qtGreen;
        QColor qtPurple;

        GLuint displayList;
        std::vector<GLuint> textures;
};


#endif // GLWIDGET_HH
