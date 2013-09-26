// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef PYTHON_HH_INCLUDED_20130826
#define PYTHON_HH_INCLUDED_20130826

#include <string>
#include <memory>

struct _object;
typedef _object PyObject;

namespace excyrender { namespace Scripting { namespace Python {

    class PyFun final
    {
    public:
        PyFun& operator= (PyFun const &) = delete;
        PyFun (PyFun const &) = delete;

        PyFun (std::string const &module, std::string const &function);
        PyObject* operator() (PyObject *args) const;
        virtual ~PyFun();

    private:
        PyObject *pName, *pModule, *pDict, *pFunc, *pValue;
    };



    class PyHeightFun final
    {
    public:
        PyHeightFun(std::string const &module_name);
        float operator() (float u, float v) const;

    private:
        std::shared_ptr<PyFun> fun;
    };

} } }


/*
int main(int argc, char *argv[])
{
    Py_SetProgramName(argv[0]);
    Py_Initialize();

    try {
        PyHeightFun fun("py");
        std::cout << "h: " << fun(0.15, 0.6) << std::endl;
    } catch (std::exception &e) {
        std::cerr << "failure: " << e.what() << "\n";
    }

    Py_Finalize();
    return 0;
}
*/

#endif // PYTHON_HH_INCLUDED_20130826
