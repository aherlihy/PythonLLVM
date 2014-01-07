import llvm.core

class vec(object):
    """
    <4 x float>
    """

    def __init__(self, *args):

        v = []

        if len(args) == 4:
            # vec(1.0, 2.0, 3.0, 4.0)

            for a in args:
                assert isinstance(a, float), ("Arg must be a float type, but %s(%s) is given" % (a, type(a)))
                
                v.append(a)
                
        elif len(args) == 1:
            if isinstance(args[0], list):
                # vec([1.0, 2.0, 3.0, 4.0])
                v = args[0]

            elif isinstance(args[0], float):
                # vec(1.0)
                v.append(args[0])
                v.append(args[0])
                v.append(args[0])
                v.append(args[0])

            else:
                raise Exception("Unsupported input for vec():", args)

        elif len(args) == 0:
            # vec()
            v = [0.0, 0.0, 0.0, 0.0]
           
        else: 
            raise Exception("Unsupported input for vec():", args)

        self.value = v


    def __str__(self):

        return str(self.value)

         
    def __add__(self, b):

        if not isinstance(b, vec):
            raise Exception("RHS is not a type of vec")

        tmp = vec([x + y for x, y in zip(self.value, b.value)])

        return tmp


    def __sub__(self, b):

        if not isinstance(b, vec):
            raise Exception("RHS is not a type of vec")

        tmp = vec([x - y for x, y in zip(self.value, b.value)])

        return tmp


    def swizzle(self, arg):
        pass


def GetVecTypeDic():

    d = {
        'vec' : vec
        }

    return d
    

