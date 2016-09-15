# Classes for Crab cfg statements: 

# Types
class Types:
    Int = 'int'
    ## XXX: extend later
    #Real = 'real'
    #def elems(cls): return cls.Int
        
class TypeException(Exception):
    pass

## Typed variables
class Var:
    def __init__(self, name, ty):
        #assert ty in Types.elems ()
        self.name = name
        self.ty = ty

    def __str__(self):
        return self.name + ':' + self.ty

## Integer variable
def IntVar(name):
    return Var(name, Types.Int)

## Statements
class Statement:
    pass

class BinaryOperation(Statement):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'
    
    def __init__(self, lhs, op, op1, op2):

        assert isinstance(lhs, Var)
        assert isinstance(op1, Var)
        assert op in (BinaryOperation.ADD, BinaryOperation.SUB, BinaryOperation.MUL, BinaryOperation.DIV)
        assert isinstance(op2, Var) or isinstance (op2, (int, long))

        self.lhs = lhs
        self.op = op
        self.op1 = op1
        self.op2 = op2

    def __str__(self):
        if isinstance (self.op2, Var):
            return self.lhs.name + ":=" + self.op1.name + self.op + self.op2.name
        else: 
            return self.lhs.name + ":=" + self.op1.name + self.op + self.op2


def Add(lhs, op1, op2):
    return BinaryOperation(lhs, BinaryOperation.ADD, op1, op2)


def Sub(lhs, op1, op2):
    return BinaryOperation(lhs, BinaryOperation.SUB, op1, op2)


def Mul(lhs, op1, op2):
    return BinaryOperation(lhs, BinaryOperation.MUL, op1, op2)


def Div(lhs, op1, op2):
    return BinaryOperation(lhs, BinaryOperation.DIV, op1, op2)


class Assign (Statement):
    def __init__(self, lhs, rhs):
        assert isinstance(lhs, Var)
        assert isinstance(rhs, Var) or isinstance (rhs, (int, long))
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        if isinstance (self.rhs, Var):
            return self.lhs.name + ":=" + self.rhs.name 
        else: 
            return self.lhs.name + ":=" + self.rhs

class Havoc (Statement):
    def __init__(self, var):
        assert isinstance(var, Var)
        self.var = var

    def __str__(self):
        return "havoc(" + self.var.name + ")"

# linear term is c*x
class LinearTerm:
    def __init__(self, factor, var):
        assert isinstance (factor, (int, long))
        assert isinstance (var, Var)
        self.factor = factor
        self.var = var

    def __str__(self):
        if self.factor == 0: return "0"
        elif self.factor == 1: return self.var.name
        elif self.factor == -1: return "-" + self.var.name
        else: return str(self.factor) + "*" + self.var.name
            
# linear expression is of the form c1*x1 + ... + cn*xn + k
class LinearExpression:
    def __init__(self):
        self.terms = list ()
        self.cst = 0

    def add_term(self, t):
        assert isinstance (t, LinearTerm)
        self.terms.append (t)

    def add_cst(self, c):
        assert isinstance (c, (int, long))
        self.cst = self.cst + c

    def __str__(self):
        if len(terms) == 0:
            return str(self.cst)
        s = ''
        for t in self.terms:
            if s: s += " + "
            s += str(t)

        if self.cst > 0:
            s += " + "    
            s += str(self.cst)
        elif self.cst < 0:
            s += " - "    
            s += str(abs(self.cst))

        return s    

class LinearConstraint :
    GT = '>'
    GEQ = '>='
    LT = '<'
    LEQ = '<='
    EQ = '='
    DIS = '!='

    def __init__(self, exp, op, cst):
        assert isinstance(exp, LinearExpression)
        assert op in (LinearConstraint.GT, LinearConstraint.GEQ, LinearConstraint.LT, LinearConstraint.LEQ, 
                      LinearConstraint.EQ, LinearConstraint.DIS)
        self.exp = exp
        self.op = op
        self.cst = cst
    
    def __str__(self):
        ## We could normalize constants since they can appear on lhs
        ## and rhs of the constraints
        if len(self.exp.terms) == 0:
            return str(self.exp.cst) + self.op + str(self.cst)

        s = ''
        for t in self.exp.terms:
            if s: s += " + "
            s += str(t)

        s += self.op    
        s += str(self.cst)
        return s    
        
class Assert(Statement):
    def __init__(self, constraint):
        assert isinstance(constraint, LinearConstraint)
        self.constraint = constraint

    def __str__(self):
        return "assert (" + str(self.constraint) + ")" 

class Assume(Statement):
    def __init__(self, constraint):
        assert isinstance(constraint, LinearConstraint)
        self.constraint = constraint

    def __str__(self):
        return "assume (" + str(self.constraint) + ")" 


if __name__ == "__main__":
    # Tests
    x1 = Var ("x1", Types.Int)
    x2 = Var ("x2", Types.Int)
    x3 = Var ("x3", Types.Int)

    s1 = BinaryOperation (x1, BinaryOperation.ADD, x2, x3)
    s2 = Havoc (x1)
    s3 = Assign (x1, x2)

    t1 = LinearTerm (2, x1)
    t2 = LinearTerm (1, x2)
    e1 = LinearExpression ()
    e1.add_term (t1)
    e1.add_term (t2)
    e1.add_cst (-1)
    c1 = LinearConstraint (e1, LinearConstraint.LEQ, 0)
    s4 = Assume (c1)

    print s1
    print s2
    print s3
    print s4
