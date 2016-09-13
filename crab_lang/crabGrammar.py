from pyparsing import *
from LogManager import LoggingManager




class CrabParser(object):
    CRAB_PROG = {"node_name" : "",
        "input_vars" : {},
        "output_vars" : {},
        "local_vars": {},
        "streams":{},
        "asserts":{},
        "s_prop":{},
        "is_main": False
        }

    def __init__(self):
        return

    def _decls(self, vars_type, l="I_O"):
        io_list = []
        local_dict = {}
        if l != "I_O":
            for all_local in vars_type.l_var:
                for var in all_local[0]:
                    local_dict.update({var:all_local[2]})
            return local_dict
        else:
            for all_vars in vars_type:
                for var in all_vars[0]:
                    io_list.append((var,all_vars[2]))
            return io_list

    def _defs(self, streams):
        streams_dict = {}
        for st in streams:
            st_defs= " ".join(x for x in st.rhs)
            st_name= " ".join(x for x in st.lhs.vars)
            streams_dict.update({st_name:st_defs})
        return streams_dict

    def _props(self, props):
        props_dict = {}
        i = 0
        for p in props:
            props_dict.update({i:(" ".join(x for x in p))})
            i += 1
        return props_dict

    def _Outspecs(self, mSpec, sSpec):
        spec_dict = {}
        spec = []
        if (sSpec and mSpec):
         spec = (sSpec + mSpec)
        elif (sSpec):
         spec = sSpec
        else:
          spec = mSpec
        req = []
        en = []
        ob = []
        for sp in spec:
          if sp[0]=="requires":
            req.append(sp[1])
            spec_dict.update({"requires": req})
          elif sp[0]=="ensures":
            en.append(sp[1])
            spec_dict.update({"ensures": en})
          elif sp[0]=="observer":
            ob.append(sp[1])
            spec_dict.update({"observer": ob})
        return spec_dict

    def _Inspecs(self, spec):
        spec_dict = {}
        inv = []
        main = []
        ob = []
        for sp in spec:
          if sp[0]=="invariant":
            inv.append(sp[1])
            spec_dict.update({"invariant": inv})
          elif sp[0]=="main":
            main.append(sp[1])
            spec_dict.update({"main": main})
          elif sp[0]=="observer":
            ob.append(sp[1])
            spec_dict.update({"observer": ob})
          else:
            spec_dict.update({sp[0]:[sp[1]]})
        return spec_dict


    def _node(self, s,l,c):
        NODES = {}
        GLOB = {}
        g_list = []
        for g in c.globals:
            if g[0] == "const":
              g_list.append(g[1])
              GLOB.update({"const":g_list})
            else:
              GLOB.update({g[0]:g[1]})
        for node in c.nodes:
            input_vars = self._decls(node.input_vars)
            output_vars = self._decls(node.output_vars)
            local_vars = self._decls(node.local_vars,"L") if node.local_vars else {}
            streams = self._defs(node.defs.streams)
            props = self._props(node.defs.props)
            is_main = node.defs.main_node == "--%MAIN"
            asserts = self._props(node.defs.asserts)
            outSpecs = self._Outspecs(node.mOutSpec, node.sOutSpec)
            inSpecs = self._Inspecs(node.defs.inSpec)
            s_prop = self._props(node.defs.s_prop)
            NODE  = {"node_name" : node.node_name,
                  "input_vars" : input_vars,
                  "output_vars" : output_vars,
                  "local_vars": local_vars,
                  "streams":streams,
                  "asserts":asserts,
                  "outSpecs":outSpecs,
                  "s_prop": s_prop,
                  "inSpecs":inSpecs}
            NODES.update({node.node_name : NODE})
        NODES.update({"glob":GLOB})
        return NODES

    def _globals(self, s, l, c):
        print c

    COMMENTS = Group(Literal("-- ") + restOfLine).setResultsName("comments")
    a1 = printables.replace(";", " ")
    a2 = a1.replace(":", " ")
    ID = Word(alphanums + "_")

    SUBRANGE = Literal("subrange") + Literal("[") + Word(nums) + Literal(",") + Word(nums) + Literal("]") + Literal("of") + Literal("int")
    VARS = Group(OneOrMore(Word(alphanums+"_") + Optional(Literal(",")).suppress())).setResultsName("vars")
    TYPES = Literal("int") | Literal("bool") | Literal("real") | Group(SUBRANGE)
    ASSIGN = Group(VARS + Literal(":") + TYPES.setResultsName("type"))
    ASSIGN2 = ZeroOrMore(Group(VARS + Literal(":") + TYPES.setResultsName("type") + Optional(Literal(";")).suppress()))
    INPUT_VARS = Group(Literal("(").suppress() + ASSIGN2.setResultsName("assign") + Literal(")").suppress()).setResultsName("input_vars")
    OUTPUT_VARS = Group(Literal("(").suppress() + ASSIGN2.setResultsName("assign") + Literal(")").suppress()).setResultsName("output_vars")
    L_VAR = ASSIGN.setResultsName("assign") +  Literal(";").suppress()
    L_VAR_COMMENTS = OneOrMore(L_VAR | COMMENTS.suppress())
    LOCAL_VARS = Group(Literal("var") + L_VAR_COMMENTS.setResultsName("l_var")).setResultsName("local_vars")


    # RHS
    I_VALUE = OneOrMore(Word(a2))
    I_KEY = OneOrMore(ID | Word(a2))
    KEY_VALUE = Group(I_KEY + Literal(":").suppress() + I_VALUE + Literal(";").suppress())

    REAL_EXPR = Word(a1);

    RHS = (OneOrMore(REAL_EXPR)).setResultsName("real_expr")

    # Expressions
    EX = (Group(RHS + Literal(";").suppress())).setResultsName("rhs")
    EX_NAME = (Group(Optional(Literal("(")).suppress() + VARS + Optional(Literal(")")).suppress())).setResultsName("lhs")
    EXPR= (Group(EX_NAME + Literal("=").suppress() + EX )).setResultsName("expr")
    MULTI_EXPR = Forward()
    MULTI_EXPR << (OneOrMore(EXPR + Optional(COMMENTS.suppress()))).setResultsName("mExpr")

    PROPS = OneOrMore(ID | Word(a1))
    S_PROPERTY = OneOrMore(Group(Literal("--!PROPERTY").suppress() + PROPS + Literal(";").suppress()))

    ASSERT = OneOrMore(Word(a1))
    ASSERTION = OneOrMore(Group(Literal("assert").suppress() + ASSERT + Literal(";").suppress()))


    M_I_SPEC = OneOrMore(Literal("(*! ").suppress()\
                              + KEY_VALUE + Literal(";").suppress()\
                              + Literal("*)").suppress())
    S_I_SPEC = Literal("--!").suppress() + KEY_VALUE

    INSIDE_SPEC = OneOrMore(S_I_SPEC)

    MAIN_NODE = Literal("--!MAIN")

    EQ = MULTI_EXPR.setResultsName("streams")\
        | S_PROPERTY.setResultsName("s_prop")\
        | ASSERTION.setResultsName("asserts")\
        | COMMENTS.suppress()\
        | MAIN_NODE.setResultsName("main_node").suppress()\
        | INSIDE_SPEC.setResultsName("inSpec")


    EQUATIONS = (Group(ZeroOrMore(EQ))).setResultsName("defs")


    def _multiNode (self):
        NODE = Group(Optional(self.SPEC)\
                      + Literal("node") + self.ID.setResultsName("node_name") + self.INPUT_VARS + Literal("returns") + self.OUTPUT_VARS + Literal(";")\
                      + Optional(self.LOCAL_VARS)\
                      + Literal("let")\
                      + self.EQUATIONS\
                      + Literal("tel") + Optional(Literal(";") | Literal("."))
                  )


        M_COMMENTS = nestedExpr("(*",")")

        GLOBAL_DECL = Literal("abs_domain") + Literal(":") + Word(self.a1) + Literal(";").suppress()

        ALL_PROGRAM = Optional(ABS_DOMAIN).setResultsName("abs_domain")\
                      + (OneOrMore((NODE).setResultsName("cprog")\
                      | self.COMMENTS.suppress()\
                      | M_COMMENTS.suppress())
                      ).setResultsName("cprogs")\

        PROGRAM = Forward()
        PROGRAM << (ALL_PROGRAM).setParseAction(lambda s, l, c: self._node(s, l, c))
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

test_1="""
abs_domain: interval;
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


if __name__ == "__main__":
  p = LParser()
  ast = p.parse(test_1)
  print ast
