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

#ifndef SIMPLE_HH
#define SIMPLE_HH

#include <QWidget>
#include "../lsystem.hh"

namespace Ui {
    class Simple;
}

class Simple : public QWidget
{
        Q_OBJECT

public:
        explicit Simple(QWidget *parent = 0);
        ~Simple();

private:
        Ui::Simple *ui;
        xyto::Pattern pat;
        xyto::LSystem lsys;

        void resizeEvent(QResizeEvent *);

private slots:
        void on_wireframe_toggled(bool checked);
        void on_rotationY_valueChanged(int value);
        void on_write_clicked();
        void on_draw_clicked();
};

#endif // SIMPLE_HH
