from pyparsing import *

check_errors = False
debug = False

class CrabParserException(Exception):
    pass

class CrabParser(object):
    def __init__(self):
        self.CRAB_PROG = {"abs_domain" : "",
                     "decl" : {},
                     "basic_blocks" : {},
                     "edges": {},
        }
        self.Vars = list()
        self.BB = list()
        
        ## The final AST
        self.ast = dict ()
        return


    def _decl(self, decl):
        for d in decl:
            var = d[0]
            self.Vars.append(var)
            typ = d[1]
            decl =self.CRAB_PROG["decl"]
            decl.update({d[0]:d[1]})
        return

    def _checkVars(self, action, expr_dict):
        vars = list()
        for exp_type, exp in expr_dict.iteritems():
            if exp_type == "mexp":
                for e in exp:
                    vars.append(e[1])                
            else:
                vars.append(exp[1])
        if check_errors and not set(vars).issubset(set(self.Vars)):
            v = " ,".join(x for x in set(vars) - set(self.Vars))
            msg = "In %s: %s is not declared" % (action,v)
            raise CrabParserException(msg)
        else:
            if debug: print "OK vars"

    def _mkLinearCst(self, cst):
        cst_out = dict()
        for c, e in cst.iteritems():
            if c == "bop":
                if type(e) is list:
                    cst_out.update({'bop':e[0]})
                else:
                    cst_out.update({'bop':e})
            elif c == "l_exp":
                cst_out.update({'l_exp':e.values()[0]})
            elif c == "num":
                cst_out.update({c:e})
        return cst_out
                
        
    def _basicBlocks(self, bbs):
        bb_dict = dict()
        for bb in bbs:
            print str(bb)
            bb_name = bb.bb_name[0]
            self.BB.append(bb_name)
            bb_dict.update({bb_name:{}})
            inst_n = 0
            instr_dict = dict()
            tbd_dict = bb.instr.asDict()
            for inst_name, inst in tbd_dict.iteritems():
                if inst_name == "assignment":
                    assignment = [inst['rhs'][0], inst['lhs'][0]]
                    instr_dict.update({inst_n:{"assignment":assignment}})
                elif inst_name == "assume" or inst_name == "assertion":
                    cst = inst['cst']
                    self._checkVars(inst_name, cst['l_exp'])
                    linear_cst = self._mkLinearCst(cst)
                    instr_dict.update({inst_n:{inst_name:linear_cst}})
                elif inst_name in ["add", "sub", "mul", "div"]:
                    if check_errors and not set(inst).issubset(set(self.Vars)):
                        v = " ,".join(x for x in set(inst) - set(self.Vars))
                        msg = "In %s: %s is not declared" % (inst_name, v)
                        raise CrabParserException(msg)
                    instr_dict.update({inst_n:{inst_name:inst}})
                elif inst_name is "havoc":
                    if check_errors and inst[0] not in self.Vars:
                        msg = "In Havoc %s is not declared" % h_var[0]
                        raise CrabParserException(msg)
                    instr_dict.update({inst_n:{"havoc":inst}})
                else:
                    msg = "%s unknown instruction" % inst_name
                    raise CrabParserException(msg)
                inst_n +=1
            bb_dict.update({bb_name:instr_dict})
        (self.CRAB_PROG["basic_blocks"]).update(bb_dict)
        return


    def _edges(self, edges):
        out = dict()
        n = 0
        for edge in edges:
            edge = edge.asList()
            if check_errors and not set(edge).issubset(set(self.BB)):
                v = " ,".join(x for x in set(edge) - set(self.BB))
                msg = "\t %s not present"% v
                raise CrabParserException(msg)
            out.update({n:edge})
            n+=1
        (self.CRAB_PROG["edges"]).update(out)
        return
        
            
        
    def _crab_prog(self, s,l,c):
        error = False
        for g in c.abs_domain:
            self.CRAB_PROG.update({"abs_domain": g})
        try:
            self._decl(c.decl)
        except Exception as e:
            print "Error: Handling variable declartions\n\t %s" % e
            error = True
        try:
            self._basicBlocks(c.bbs)
        except Exception as e:
            print "Error: Handling basic blocks\n\t %s" % e
            error = True
        try:
            self._edges(c.edges)
        except Exception as e:
            print "Error: Handling Links\n\t %s" % e
            error = True
        if not error:
            return self.CRAB_PROG
        else:
            return {}

    def _globals(self, s, l, c):
        print c


    COMMENTS = Group(Literal("-- ") + restOfLine).setResultsName("comments")
    a1 = printables.replace(";", " ")
    a2 = a1.replace(":", " ")
    a2 = a2.replace("[", " ")
    a2 = a2.replace("]", " ")
    a2 = a2.replace("{", " ")
    a2 = a2.replace("}", " ")
    a2 = a2.replace("+", " ")
    a2 = a2.replace("=", " ")
    a2 = a2.replace("-", " ")
    a2 = a2.replace("*", " ")
    a2 = a2.replace("/", " ")
    a2 = a2.replace(">", " ")
    a2 = a2.replace("!", " ")
    a2 = a2.replace("<", " ")
    a2 = a2.replace("(", " ")
    a2 = a2.replace(")", " ")
    ID = Word(alphanums + "_")
    VARS = OneOrMore(ID | Word(a2))
    NUM = Word('-0123456789')

    #VARS = Group(ID)
    TYPES = Literal("int") | Literal("real")
    ASSIGN2 = ZeroOrMore(Group(VARS.setResultsName("var_name") + Literal(":").suppress() + TYPES.setResultsName("type") + Literal(";").suppress())).setResultsName("assign")


    DECL_VARS = Literal("decl").suppress() + Literal(":").suppress() + Literal("[").suppress()\
                      + ASSIGN2 +  Literal("]").suppress() + Literal(";").suppress()

    EXP = Group(Literal("(").suppress()\
                + NUM + Literal("*").suppress()\
                + VARS + Literal(")").suppress())
    
    LINEAR_EXP = OneOrMore(Group(EXP + Literal("+").suppress() + EXP).setResultsName("mexp") | EXP.setResultsName("sexp"))
    
    BOP = Literal("<=") | Literal(">=") | Literal("=") | Literal("!=") | Literal("<") | Literal(">")
    
    ###  XXX: these rules do not compile >=, <=, != operators
    # BOP = Literal("<") | Literal(">") | Literal("=")\
    #       | Group(Literal("!=")) | Group(Literal(">="))\
    #       | Group(Literal(">="))

    LINEAR_CST = Group(LINEAR_EXP.setResultsName("l_exp") + BOP.setResultsName("bop") + NUM.setResultsName("num"))

    ASSIGNMENT = Group(VARS.setResultsName("lhs") + Literal(":=").suppress()\
                       + VARS.setResultsName("rhs") + Literal(";").suppress())
    ASSUME = Group(Literal("assume").suppress() + LINEAR_CST.setResultsName("cst") + Literal(";").suppress())
    ASSERT = Group(Literal("assert").suppress() + LINEAR_CST.setResultsName("cst") + Literal(";").suppress())
    HAVOC = Group(Literal("havoc").suppress() + VARS + Literal(";").suppress())
    ADD = Group(VARS + Literal(":=").suppress() + VARS + Literal("+").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal(":=").suppress() + VARS + Literal("+").suppress() + NUM+ Literal(";").suppress())
    SUB = Group(VARS + Literal(":=").suppress() + VARS + Literal("-").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal(":=").suppress() + VARS + Literal("-").suppress() + NUM+ Literal(";").suppress())
    MUL = Group(VARS + Literal(":=").suppress() + VARS + Literal("*").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal(":=").suppress() + VARS + Literal("*").suppress() + NUM+ Literal(";").suppress())
    DIV = Group(VARS + Literal(":=").suppress() + VARS + Literal("/").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal(":=").suppress() + VARS + Literal("/").suppress() + NUM+ Literal(";").suppress())


    INSTR = ASSIGNMENT.setResultsName("assignment")\
            | ASSUME.setResultsName("assume")\
            | ASSERT.setResultsName("assertion")\
            | ADD.setResultsName("add")\
            | SUB.setResultsName("sub")\
            | MUL.setResultsName("mul")\
            | DIV.setResultsName("div")\
            | HAVOC.setResultsName("havoc")


    BB = Group(VARS.setResultsName("bb_name")\
         + Literal("[").suppress()\
         + Group(ZeroOrMore(INSTR)).setResultsName("instr")\
         + Literal("]").suppress())

    BBS = Literal("blocks").suppress() + Literal("{").suppress()\
          + OneOrMore(BB).setResultsName("bblock")\
          + Literal("}").suppress()

    EDGE = Group(VARS.setResultsName("bb_left")\
         + Group(Literal("->")).suppress()\
         + VARS.setResultsName("bb_right")\
         + Literal(";").suppress())

    EDGES = Literal("edges").suppress() + Literal("{").suppress()\
            + OneOrMore(EDGE).setResultsName("edge")\
            + Literal("}").suppress()

    def _multiNode (self):

        M_COMMENTS = nestedExpr("(*",")")

        ABS_DOMAIN = Literal("abs_domain").suppress() + Literal(":").suppress()\
                     + Word(self.a1) + Literal(";").suppress()


        ALL_PROGRAM = Optional(ABS_DOMAIN).setResultsName("abs_domain")\
                      + Optional(self.DECL_VARS).setResultsName("decl")\
                      + Optional(self.BBS).setResultsName("bbs")\
                      + Optional(self.EDGES).setResultsName("edges")

        PROGRAM = Forward()
        PROGRAM << (ALL_PROGRAM).setParseAction(lambda s, l, c: self._crab_prog(s, l, c))
        return PROGRAM

    def parse(self, lustreFile):
        try:
            PROGRAM = self._multiNode()
            nodes = PROGRAM.parseString(lustreFile, parseAll=True)
            self._dictify(nodes)
            return
        except ParseException, err:
            raise err

    def _dictify(self, conf_list):
        for conf in conf_list:
            self.ast.update(conf)

    def ppAST(self):
        pp = "\n"
        for k,v in self.ast.iteritems():
            bb = ""
            if k == "basic_blocks":
                for bb_name, bb_expr in v.iteritems():
                    
                    bb += "\t" + bb_name + " => " + str(bb_expr) + "\n"
                pp += k + " => \n " + bb + "\n"
            else:
                pp += k + " => " + str(v) + "\n\n"
        return pp

    ##############
    ### API needed by C++ to access to each element in the AST
    ##############

    # TODO: I don't know how to iterate over ast from C++
    #
    # The solution is to create python classes for each kind of
    # statement (inheriting from a base class statement), variable,
    # linear expression and linear_constraint. In addition, we need
    # methods such as getBasicBlocks, getInstructions, etc. In that
    # way, C++ can then call their methods and access to their
    # components.

    def getAbsDom (self):
        return self.ast ["abs_domain"]

    def getBasicBlocks (self):
        bbs = list()
        for k,v in self.ast.iteritems():
            if k == "basic_blocks":
                for name, instrs in v.iteritems():
                    bbs.append(name)

        bbs_sorted = list (reversed (bbs))
        #print "Basic blocks=" + str(bbs_sorted)
        return bbs_sorted

    def getEdges (self):
        #print str(self.ast)
        edges = list ()
        for k,v in self.ast.iteritems():
            if k == "edges":
                for _,pairlist in v.iteritems():
                    if (len(pairlist) != 2):
                        raise CrabParserException("edges must be lists of length two")
                    edges.append((pairlist[0],pairlist[1]))
                    #print "Edge " + "(" + str(pairlist[0]) + "," + str(pairlist[1]) + ")"
        return edges 

    def getInstructions (self, bb_name):
        ## TODO
        instrs = list () ## should be a list of Statements
        return instrs

## This test should succeed without type-checking
test_0 = """
abs_domain : interval;
decl : [i:int; j:int; i:int; ];
blocks { helle[h:=k; teme:=hola;]
     two[]
     d [havoc b; havoc c;]
     a [assume (2 *g) + (3 * g) <0;]
     b [assert (3 *h) != 0;]
     c [v1 := v2 + v3; v1:= v2 - v3; v1 := v2*v3; v1 := v2/v3;]
}

edges { f->j; a->b; b->a;}
"""
### This test will fail: e.g., tokens like add are not recognized.
test_1="""
abs_domain: [interval];
decl: int i;
blocks: {
     entry[assign(i,0)];
     loop_entry[];
     loop_body[assume(i<10),
               add(i,i,1)]
     loop_exit[assume(i>=10)]
   }

edges: {
        entry -> loop_header;
        loop_header -> loop_body;
        loop_body -> loop_header;
	loop_header -> loop_exit
     }
"""

### This test should succeed without type-checking (u and v are not declared)
test_2 = """
abs_domain : interval;
decl : [h:int; k:int; v1:int; v2:int; v3:int; v4:int;];
blocks {
   bb1[h:=k; u:=k; h:=v; assert (-3*h)+(4*v2)!=0;]
   bb2[havoc h;]
   bb3[]
   bb4[assume (2*h)>0;]
   bb5 [v1 := v2 + v3; v1 := v2 - v3; havoc v3;]
   }
edges {bb1 -> bb2; bb2 -> bb3; bb4->bb5; bb6 -> bb7;}
"""

### This test should succeed
test_3 = """
abs_domain : zones;
decl : [x:int; y:int;];
blocks {
   bb1[x:=0; y:=0;]
   bb2[]
   bb3[assume (1*x)<10;x := x + 1; y := y + 1;]
   bb4[assume (1*x)>=10;]
   }
edges {bb1 ->bb2; bb2 -> bb3; bb3-> bb2; bb2->bb4;}
"""

if __name__ == "__main__":
  p = CrabParser()
  # p.parse(test_0)
  # p.parse(test_1)
  # p.parse(test_2)
  p.parse(test_3)
  print p.ppAST()
