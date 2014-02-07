#include <boost/python.hpp>
#include "testC.h"
using namespace boost::python;

/*
  Virtual methods
     any base class that has a virtual function that's going to be overridden in Python and
     called polymorphically from C++, it needs to be wrapped with a boost::python::wrapper<...>

  To extend class in python:
    >>> base = Base()
    >>> class Derived(Base):
    ...     def f(self):
    ...         return 42
    ...
    >>> derived = Derived()
*/

// inheriting pure virtual functions 
struct PureBase {
    PureBase() {}
    virtual ~PureBase() {}
    virtual int f() = 0;
};
struct testA {
    testA(testC c) {}
    int testing() { return 1; }
};
struct PureBaseWrap : PureBase, wrapper<PureBase>
{
    int f()
    {
        return this->get_override("f")();
    }
};
// inheriting virtual functions with default definitions
struct DefaultBase
{
    virtual ~DefaultBase() {}
    virtual int f() { return 0; }
};
struct DefaultBaseWrap : DefaultBase, wrapper<DefaultBase>
{
    int f()
    {
        if (override f = this->get_override("f"))
            return f(); // *note*
        return DefaultBase::f();
    }

    int default_f() { return this->DefaultBase::f(); }
};

// Derived can inherit from PureBase or from DefaultBase
struct Derived : DefaultBase /*PureBase*/ {
    // multiple constructors
    Derived() {}
    Derived(std::string msg): msg(msg) {} 
    Derived(double, double) {}
    // getters and setters 
    void set(std::string msg) { this->msg = msg; }
    std::string get() { return msg; }
    // attributes
    std::string msg;
    std::string const name;
    int value;
    // overridden method
    virtual int f() { return value; }
};

// polymorphic function args (pass C++ defined-class to functions)
void b(DefaultBase* /* PureBase* */) {}
void d(Derived*) {}
DefaultBase* /* PureBase* */factory() { return new Derived("factory world"); }

// expose classes/functions to python
BOOST_PYTHON_MODULE(python_example)
{
    /* 
      class definitions:
          bases<...> identifies the superclass(s)
            - World will inherit all of PureBase's methods
            - if PureBase is polymorphic: World instances passed to python as PureBase ptr/refs 
              can be used where ptr/refs of World are expected
          init<...>() exposes the constructor 
          pure_virtual indicates which methods are pure virtual
    */
    class_<testC>("testC")
    ;
    class_<testA>("testA", init<testC>())
        .def("testing", &testA::testing)
    ;
    class_<PureBaseWrap, boost::noncopyable>("PureBase")
        .def("f", pure_virtual(&PureBase::f))
        ;                                                                              
    class_<DefaultBaseWrap, boost::noncopyable>("DefaultBase")
        .def("f", &DefaultBase::f, &DefaultBaseWrap::default_f)
        ;
    class_<Derived, bases<DefaultBase /* PureBase */> >("Derived", init<std::string>())
        // alternative constructor:
        .def(init<double, double>())
        // member functions:
        .def("get", &Derived::get)
        .def("set", &Derived::set)
        // member variables:
        .def_readonly("name", &Derived::name) 
        .def_readwrite("value", &Derived::value) 
        // properties (i.e. getters/setters):
        .add_property("rovalue", &Derived::get) 
        .add_property("rwvalue", &Derived::get, &Derived::set);
  
    /* 
      external functions: polymorphism!
            both b and d can take in factory() as an argument
    */
    def("b", b);
    def("d", d);
    // tell Python to take ownership of factory's result
    def("factory", factory,
        return_value_policy<manage_new_object>());
                                                                              
}
