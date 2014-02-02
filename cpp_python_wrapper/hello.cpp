#include <boost/python.hpp>

char const* greet()
{
   return "hello, world YAY";
}

BOOST_PYTHON_MODULE(hello)
{
    using namespace boost::python;
    def("greet", greet);
}
