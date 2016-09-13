from pyparsing import *
from LogManager import LoggingManager




class CrabParser(object):

    def __init__(self):
        self.CRAB_PROG = {"abs_domain" : "",
                     "decl" : {},
                     "basic_blocks" : {},
                     "edges": {},
        }
        return


    def _decl(self, decl):
        for d in decl:
            var = d[0]
            typ = d[1]
            decl =self.CRAB_PROG["decl"]
            decl.update({d[0]:d[1]})
        return

    def _basicBlocks(self, bbs):
        bb_dict = dict()
        for bb in bbs:
            bb_name = bb.bb_name[0]
            bb_instrs = bb.instrs
            if bb.assignment:
                assignment = dict()
                for ass in bb.instrs:
                    assignment.update({ass[0]:ass[1]})
                bb_dict.update({bb_name:{"assignment":assignment}})
        (self.CRAB_PROG["basic_blocks"]).update(bb_dict)
        return


    def _crab_prog(self, s,l,c):

        for g in c.abs_domain:
            self.CRAB_PROG.update({"abs_domain": g})
        try:
            self._decl(c.decl)
        except Exception as e:
            print "Getting variable declartion"
        try:
            self._basicBlocks(c.bbs)
        except Exception as e:
            print e
            print "Handling basic blocks"
        for e in c.edges:
            print e

        # for node in c.nodes:
        #     input_vars = self._decls(node.input_vars)
        #     output_vars = self._decls(node.output_vars)
        #     local_vars = self._decls(node.local_vars,"L") if node.local_vars else {}
        #     streams = self._defs(node.defs.streams)
        #     props = self._props(node.defs.props)
        #     is_main = node.defs.main_node == "--%MAIN"
        #     asserts = self._props(node.defs.asserts)
        #     outSpecs = self._Outspecs(node.mOutSpec, node.sOutSpec)
        #     inSpecs = self._Inspecs(node.defs.inSpec)
        #     s_prop = self._props(node.defs.s_prop)
        #     NODE  = {"node_name" : node.node_name,
        #           "input_vars" : input_vars,
        #           "output_vars" : output_vars,
        #           "local_vars": local_vars,
        #           "streams":streams,
        #           "asserts":asserts,
        #           "outSpecs":outSpecs,
        #           "s_prop": s_prop,
        #           "inSpecs":inSpecs}
        #     NODES.update({node.node_name : NODE})
        # NODES.update({"glob":GLOB})
        return self.CRAB_PROG

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
    NUM = Word('0123456789')

    #VARS = Group(ID)
    TYPES = Literal("int") | Literal("real")
    ASSIGN2 = ZeroOrMore(Group(VARS.setResultsName("var_name") + Literal(":").suppress() + TYPES.setResultsName("type") + Literal(";").suppress())).setResultsName("assign")


    DECL_VARS = Literal("decl").suppress() + Literal(":").suppress() + Literal("[").suppress()\
                      + ASSIGN2 +  Literal("]").suppress() + Literal(";").suppress()

    EXP = Group(Literal("(").suppress()\
                + NUM + Literal(",").suppress()\
                + VARS + Literal(")").suppress()).setResultsName("")
    LINEAR_EXP = OneOrMore(EXP).setResultsName("exp")
    BOP = Literal("<") | Literal(">") | Literal("=")\
          | Group(Literal("!=")) | Group(Literal(">="))\
          | Group(Literal(">="))

    LINEAR_CST = Group(LINEAR_EXP.setResultsName("cst") + BOP + NUM)

    ASSIGNMENT = Group(VARS + Literal(":=").suppress() + VARS + Literal(";").suppress())
    ASSUME = Group(Literal("assume").suppress() + LINEAR_CST.setResultsName("const") + Literal(";").suppress())
    ASSERT = Group(Literal("assert").suppress() + LINEAR_CST.setResultsName("const") + Literal(";").suppress())
    HAVOC = Group(Literal("havoc").suppress() + VARS+ Literal(";").suppress())
    ADD = Group(VARS + Literal("=").suppress() + VARS + Literal("+").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal("=").suppress() + VARS + Literal("+").suppress() + NUM+ Literal(";").suppress())
    SUB = Group(VARS + Literal("=").suppress() + VARS + Literal("-").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal("=").suppress() + VARS + Literal("-").suppress() + NUM+ Literal(";").suppress())
    MUL = Group(VARS + Literal("=").suppress() + VARS + Literal("*").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal("=").suppress() + VARS + Literal("*").suppress() + NUM+ Literal(";").suppress())
    DIV = Group(VARS + Literal("=").suppress() + VARS + Literal("/").suppress() + VARS + Literal(";").suppress())\
          | Group(VARS + Literal("=").suppress() + VARS + Literal("/").suppress() + NUM+ Literal(";").suppress())


    INSTR = ASSIGNMENT.setResultsName("assignment")\
            | ASSUME.setResultsName("assume")\
            | ASSERT.setResultsName("assert")\
            | ADD.setResultsName("add")\
            | SUB.setResultsName("sub")\
            | MUL.setResultsName("mul")\
            | DIV.setResultsName("div")\
            | HAVOC.setResultsName("havoc")


    BB = Group(VARS.setResultsName("bb_name")\
         + Literal("[").suppress()\
         + ZeroOrMore(INSTR).setResultsName("instrs")\
         + Literal("]").suppress()).setResultsName("bb")

    BBS = Literal("bb").suppress() + Literal("{").suppress()\
          + OneOrMore(BB)\
          + Literal("}").suppress()

    EDGE = Group(VARS.setResultsName("bb_name")\
         + Group(Literal(">>") | Literal("<<>>").setResultsName("bi")).suppress()\
         + VARS.setResultsName("bb_name")\
         + Literal(";").suppress()).setResultsName("edge")

    EDGES = Literal("link").suppress() + Literal("{").suppress()\
            + OneOrMore(EDGE)\
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
            ast = self._dictify(nodes)
            return ast
        except ParseException, err:
            raise err

    def _dictify(self, conf_list):
        conf_dict = {}
        for conf in conf_list:
            conf_dict.update(conf)
        return conf_dict

    def ppAST(self, ast):
        pp = "\n"
        for n,k in ast.iteritems():
            pp += n + " => " + str(k) + "\n\n"
        return pp

test_0 = """
abs_domain : interval;
decl : [i:int; j:int; i:int; ];
bb { helle[h:=k; teme:=hola;]
     two[]
     d [havoc b;]
     a [assume (2,g) (3,g) <0;]
     b [assert (3,h) != 0;]
     c [v1 = v2 + v3; v1= v2 - v3; v1 = v2*v3; v1 = v2/v3;]
}

link { f>>j; a<<>>b;}
"""
test_1="""
abs_domain: [interval];
decl: int i;
bb: {
     entry[assign(i,0)];
     loop_entry[];
     loop_body[assume(i<10),
               add(i,i,1)]
     loop_exit[assume(i>=10)]
   }

edges: {
        entry >> loop_header;
        loop_header <<>> loop_body;
	loop_header >> loop_exit
     }
"""

test = """
abs_domain : interval;
decl : [h:int; k:int;];
bb { bb1[h:=k;]
}
"""

if __name__ == "__main__":
  p = CrabParser()
  ast = p.parse(test)
  print ast
