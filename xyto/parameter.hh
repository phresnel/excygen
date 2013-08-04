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

#ifndef PARAMETER_HH_INCLUDED_20100726
#define PARAMETER_HH_INCLUDED_20100726

#include <string>
#include <iostream>
#include "special_ptr/soft_value.hh"
#include "constant.hh"

namespace xyto {

class Parameter {
public:
        enum Type {
                Identifier,
                Integer,
                Real,
                ParameterIndex, // <-- never parsed, used by compiler

                // arithmetic
                Negate,
                Multiplication, Division,
                Addition, Subtraction,

                // relational
                LessThan, LessEqual,
                GreaterThan, GreaterEqual,

                // logical
                LogicalAnd, LogicalOr, LogicalXor
        };

        Type type() const;
        void setType (Type type);

        void setInteger (int v);
        void setReal (double v);
        void setIdentifier (std::string v);

        int integer () const;
        double real () const;
        std::string identifier () const;

        double toReal () const ;

        void toParameterIndex (int index);
        int parameterIndex() const;

        void toConstant (xyto::Constant c);
        xyto::Constant constant() const;

        Parameter lhs () const;
        void setLhs (Parameter const &);

        Parameter rhs () const;
        void setRhs (Parameter const &);

        Parameter unaryParameter () const;
        void setUnaryParameter (Parameter const &);

        void swap (Parameter&);
private:
        Type type_;
        int intval;
        int index;
        double realval;
        std::string idval;

        special_ptr::soft_value<Parameter> lhs_, rhs_, unary_;
};

}


#endif // PARAMETER_HH_INCLUDED_20100726
