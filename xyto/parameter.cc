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

#include <iostream>
#include <stdexcept>
#include "parameter.hh"

namespace xyto {

Parameter::Type Parameter::type() const {
        return type_;
}



double Parameter::toReal () const {
        if (type_ == Real)
                return realval;
        if (type_ == Integer)
                return intval;
        std::cout << "runtime error: Parameter::toReal() "
                "called for non-number\n";
        return 0;
}



void Parameter::setType (Parameter::Type type) {
        type_ = type;
}



void Parameter::setInteger (int v) {
        if (type_ != Integer)
                std::cout << "runtime error: Parameter::setInteger() "
                        "called for non-integer\n";
        intval = v;
}



void Parameter::setReal (double v) {
        if (type_ != Real)
                std::cout << "runtime error: Parameter::setReal() "
                        "called for non-real\n";
        realval = v;
}



void Parameter::setIdentifier (std::string v) {
        if (type_ != Identifier)
                std::cout << "runtime error: "
                        "Parameter::setIdentifier() called for "
                        "non-identifier\n";
        idval = v;
}



int Parameter::integer () const {
        if (type_ != Integer)
                std::cout << "runtime error: Parameter::integer() "
                        "called for non-integer\n";
        return intval;
}



double Parameter::real () const {
        if (type_ != Real)
                std::cout << "runtime error: Parameter::real() "
                        "called for non-real\n";
        return realval;
}



std::string Parameter::identifier () const {
        if (type_ != Identifier)
                std::cout << "runtime error: Parameter::identifier() "
                        "called for non-identifier\n";
        return idval;
}



Parameter Parameter::lhs () const {
        if (!lhs_) {
                std::cerr << "Parameter::lhs() called, but "
                             "it is NULL (type=" << type_ << ")" << std::endl;
        }
        return *lhs_;
}



void Parameter::setLhs (Parameter const &val) {
        lhs_ = special_ptr::soft_value<Parameter>(val);
}



Parameter Parameter::rhs () const {
        if (!rhs_) {
                std::cerr << "Parameter::rhs() called, but "
                             "it is NULL (type=" << type_ << ")" << std::endl;
        }
        return *rhs_;
}



void Parameter::setRhs (Parameter const &val) {
        rhs_ = special_ptr::soft_value<Parameter>(val);
}



Parameter Parameter::unaryParameter () const {
        if (!unary_) {
                std::cerr << "Parameter::unaryParameter() called, but "
                             "it is NULL (type=" << type_ << ")" << std::endl;
        }
        return *unary_;
}



void Parameter::setUnaryParameter (Parameter const &val) {
        unary_ = special_ptr::soft_value<Parameter>(val);
}



void Parameter::toParameterIndex (int index) {
        if (type_ != Identifier)
                std::cout << "internal runtime error: Parameter::"
                        "toParameterIndex() called for non-"
                        "identifier\n";
        type_ = ParameterIndex;
        this->index = index;
}



int Parameter::parameterIndex() const {
        return index;
}



void Parameter::toConstant (xyto::Constant c) {
        if (type_ != Identifier)
                std::cout << "internal runtime error: Parameter::"
                        "toConstant() called for non-"
                        "identifier\n";
        switch (c.type()) {
        case xyto::Constant::Real:
                type_ = Real;
                realval = c.toReal();
                break;
        case xyto::Constant::Integer:
                type_ = Integer;
                intval = c.toInteger();
                break;
        case xyto::Constant::String:
                throw std::runtime_error(
                        "in function xyto->Paramater::toConstant(), c is of "
                        "type String");
        }
}

}

/*Constant Parameter::constant() const {
        return constant_;
}*/
