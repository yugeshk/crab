from pyparsing import *
from LogManager import LoggingManager
import pprint



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
            bb_instr = bb.instr
            bb_dict.update({bb_name:{}})
            inst_n = 0
            instr_dict = dict()
            for inst in bb_instr:
                if inst.getName() is "assignment":
                    instr_dict.update({inst_n:{"assignment":inst.asList()}})
                elif inst.getName() is "assume":
                    assume_dict = inst.asDict()['cst']
                    if type(assume_dict['bop']) is list:
                        assume_dict.update({'bop':(assume_dict['bop'])[0]})
                    instr_dict.update({inst_n:{"assume":assume_dict}})
                elif inst.getName() is "assert":
                    ass_dict = inst.asDict()['cst']
                    if type(ass_dict['bop']) is list:
                        ass_dict.update({'bop':(ass_dict['bop'])[0]})
                    instr_dict.update({inst_n:{"assert":ass_dict}})
                elif inst.getName() in ["add", "sub", "mul", "div"]:
                    instr_dict.update({inst_n:{str(inst.getName()):inst.asList()}})
                elif inst.getName() is "havoc":
                    instr_dict.update({inst_n:{"havoc":inst.asList()}})
                else:
                    assert False, 'unknown instruction'
                inst_n +=1
            bb_dict.update({bb_name:instr_dict})
        (self.CRAB_PROG["basic_blocks"]).update(bb_dict)
        return


    def _edges(self, edges):
        out = dict()
        n = 0
        for edge in edges:
            edge = edge.asList()
            out.update({n:edge})
            n+=1
        (self.CRAB_PROG["edges"]).update(out)
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
        try:
            self._edges(c.edges)
        except Exception as e:
            print e
            print "Handling edges"
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

    LINEAR_CST = Group(LINEAR_EXP.setResultsName("exp") + BOP.setResultsName("bop") + NUM.setResultsName("num"))

    ASSIGNMENT = Group(VARS.setResultsName("lhs") + Literal(":=").suppress()\
                       + VARS.setResultsName("rhs") + Literal(";").suppress())
    ASSUME = Group(Literal("assume").suppress() + LINEAR_CST.setResultsName("cst") + Literal(";").suppress())
    ASSERT = Group(Literal("assert").suppress() + LINEAR_CST.setResultsName("cst") + Literal(";").suppress())
    HAVOC = Group(Literal("havoc").suppress() + VARS + Literal(";").suppress())
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
         + Group(ZeroOrMore(INSTR)).setResultsName("instr")\
         + Literal("]").suppress())

    BBS = Literal("bb").suppress() + Literal("{").suppress()\
          + OneOrMore(BB).setResultsName("bblock")\
          + Literal("}").suppress()

    EDGE = Group(VARS.setResultsName("bb_left")\
         + Group(Literal(">>")).suppress()\
         + VARS.setResultsName("bb_right")\
         + Literal(";").suppress())

    EDGES = Literal("link").suppress() + Literal("{").suppress()\
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
            bb = ""
            if n == "basic_blocks":
                for bb_name, bb_expr in k.iteritems():
                    bb += "\t" + bb_name + " => " + str(bb_expr) + "\n"
                pp += n + " => \n " + bb + "\n"
            else:
                pp += n + " => " + str(k) + "\n\n"
        return pp

test_0 = """
abs_domain : interval;
decl : [i:int; j:int; i:int; ];
bb { helle[h:=k; teme:=hola;]
     two[]
     d [havoc b; havoc c;]
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
bb {
   bb1[h:=k; u:=k; h:=v; assert (3,h) (4,h)!=0;]
   bb2[havoc h;]
   bb3[]
   bb4[assume (2,h)=0;]
   bb5 [v1 = v2 + v3; v1 = v2 - v4;]
   }
link {bb1 >> bb2; bb2 >> bb3; bb4>>bb5;}
"""

if __name__ == "__main__":
  p = CrabParser()
  ast = p.parse(test)
  print p.ppAST(ast)
