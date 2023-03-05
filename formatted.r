Inlined(
  None, 
  Nil, 
  Apply(
    TypeApply(
      Select(
        Apply(
          TypeApply(
            Select(
              Apply(
                TypeApply(
                  Select(
                    Apply(
                      TypeApply(Select(Ident("Some"), "apply"), List(Inferred())), 
                      List(Literal(IntConstant(1)))
                    ), 
                    "zip"
                  ), 
                  List(Inferred(), Inferred())
                ), 
                List(
                  Apply(
                    TypeApply(Select(Ident("Some"), "apply"), List(Inferred())), 
                    List(Literal(IntConstant(2)))
                  )
                )
              ), 
              "zip"
            ), 
            List(Inferred(), Inferred())
          ), 
          List(
            Apply(
              TypeApply(Select(Ident("Some"), "apply"), List(Inferred())), 
              List(Literal(IntConstant(3)))
            )
          )
        ), 
        "map"
      ), 
      List(Inferred())
    ), 
    List(
      Block(
        List(
          DefDef(
            "$anonfun", 
            List(TermParamClause(List(ValDef("x$1", Inferred(), None)))), 
            Inferred(), 
            Some(
              Match(
                Ident("x$1"), 
                List(
                  CaseDef(
                    Unapply(
                      TypeApply(Select(Ident("Tuple2"), "unapply"), List(Inferred(), Inferred())), 
                      Nil, 
                      List(
                        Unapply(
                          TypeApply(Select(Ident("Tuple2"), "unapply"), List(Inferred(), Inferred())), 
                          Nil, 
                          List(Bind("one", Wildcard()), Bind("two", Wildcard()))
                        ), 
                        Bind("three", Wildcard())
                      )
                    ), 
                    None, 
                    Block(
                      Nil, 
                      Apply(
                        TypeApply(Select(Ident("List"), "apply"), List(Inferred())), 
                        List(
                          Typed(
                            Repeated(List(Ident("one"), Ident("two"), Ident("three")), Inferred()), 
                            Inferred()
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ), 
        Closure(Ident("$anonfun"), None)
      )
    )
  )
)
