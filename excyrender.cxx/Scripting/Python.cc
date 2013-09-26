// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.

#include <Python.h>
#include <iostream>
#include <stdexcept>
#include "Python.hh"


namespace excyrender { namespace Scripting { namespace Python {


PyFun::PyFun (std::string const &module,
              std::string const &function)
{
    char *py_argv[] = {const_cast<char*>(module.c_str())}; // TODO: validate that this is proper
    const int py_argc = sizeof (py_argv) / sizeof(py_argv[0]);
    PySys_SetArgv(py_argc, py_argv);

    // Build the name object
    if (!(pName = PyString_FromString(module.c_str()))) {
        PyErr_Print();
        throw std::runtime_error("failed to load python module '" + module + "'");
    }

    // Load the module object
    if (!(pModule = PyImport_Import(pName)))  {
        PyErr_Print();
        throw std::runtime_error("failed to load python function '" + function +
                                 "' from module '" + module + "'");
    }

    // pDict is a borrowed reference
    if (!(pDict = PyModule_GetDict(pModule))) {
        PyErr_Print();
        throw std::runtime_error("failed to GetDict from module '" + module + "'");
    }

    // pFunc is also a borrowed reference
    if (!(pFunc = PyDict_GetItemString(pDict, function.c_str()))) {
        PyErr_Print();
        throw std::runtime_error("failed to GetItemString for function '" + function +
                                 "' from module '" + module + "'");
    }


    if (!PyCallable_Check(pFunc)) {
        throw std::runtime_error("function '" + function + "' from module '" + module + "' "
                                 "is not callable");
    }
}



PyObject* PyFun::operator() (PyObject *args) const {
    return PyObject_CallObject(pFunc, args);
}



PyFun::~PyFun() {
    // Clean up
    Py_DECREF(pModule);
    Py_DECREF(pName);
}






PyHeightFun::PyHeightFun(std::string const &module) : fun(new PyFun(module, "height"))
{
}



float PyHeightFun::operator() (float u, float v) const {
    auto args = PyTuple_New(2);
    if (NULL == args) {
        throw std::runtime_error("PyTuple_New allocation error");
    }
    PyTuple_SetItem(args, 0, PyFloat_FromDouble(u));
    PyTuple_SetItem(args, 1, PyFloat_FromDouble(v));
    float f = PyFloat_AsDouble ((*fun)(args));
    Py_DECREF(args);
    return f;
}


} } }
