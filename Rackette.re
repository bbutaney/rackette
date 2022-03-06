open CS17SetupRackette;
open Read.Reader;
open Types;

/* Input: a list of values, alov
   Output: a value representing the sum of the list of values */
let builtinPlus: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => NumV(int1 + int2)
  | _ => failwith("+ expects two integers")
  };
checkExpect(builtinPlus([NumV(4), NumV(6)]), NumV(10), "builtinPlus: 1");
checkExpect(builtinPlus([NumV(0), NumV(23)]), NumV(23), "builtinPlus: 2");

/* Input: a list of values, alov
   Output: a value representing the first value in alov minus the second*/
let builtinSub: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => NumV(int1 - int2)
  | _ => failwith("- expects two integers")
  };
checkExpect(builtinSub([NumV(10), NumV(6)]), NumV(4), "builtinSub: 1");
checkExpect(builtinSub([NumV(23), NumV(0)]), NumV(23), "builtinSub: 2");

/* Input: a list of values, alov
   Output: a value representing the produt of the values in alov*/ 
let builtinMult: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => NumV(int1 * int2)
  | _ => failwith("* expects two integers")
  };
checkExpect(builtinMult([NumV(10), NumV(6)]), NumV(60), "builtinMult: 1");
checkExpect(builtinMult([NumV(23), NumV(0)]), NumV(0), "builtinMult: 2");

/* Input: a list of values, alov
   Output: a value representing the first value in alov divided by the second*/ 
let builtinDivide: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => NumV(int1 / int2)
  | _ => failwith("/ expects two integers")
  };
checkExpect(builtinDivide([NumV(10), NumV(5)]), NumV(2), "builtinDivide: 1");
checkExpect(builtinDivide([NumV(23), NumV(1)]), NumV(23), "builtinDivide: 2");

/* Input: a list of values, alov
   Output: a value representing the remainder of dividing the first
   value in alov by the second 
   
   Recursion Diagram: 
  OI: (NumV(10), NumV(3))
  RI: (NumV(7), NumV(3))
  Ideation: subtract the second value from the first until thefhe first is
  less than the second, then return second.
  RO: (NumV(4), NumV(3))
  OO: 1

  Recursion Diagram 2:
  OI: (NumV(0), NumV(6))
  RI: N/A
  Ideation: first value is already less than second, so return second
  RO: N/A
  OO: 0
   */ 
let rec builtinRem: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] when int1 < int2 => NumV(int1)
  | [NumV(int1), NumV(int2)] when int1 >= int2 => 
      builtinRem([NumV(int1 - int2), NumV(int2)])
  | _ => failwith("remainder expects two integers")
  };
checkExpect(builtinRem([NumV(9), NumV(10)]), NumV(9), "builtinRem: 1");
checkExpect(builtinRem([NumV(5), NumV(3)]), NumV(2), "builtinRem: 2");

/* Input: a list of values, alov
   Output: a value representing the boolean of whether the two values
   in alov are equal*/ 
let builtinEqualSign: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => BoolV(int1 == int2)
  | _ => failwith("= expects two integers")
  };
checkExpect(builtinEqualSign([NumV(9), NumV(9)]), 
            BoolV(true), 
            "builtinEqualSign: 1");
checkExpect(builtinEqualSign([NumV(5), NumV(3)]), 
            BoolV(false), 
            "builtinEqualSign: 2");

/* Input: a list of values, alov
   Output: a value representing the boolean of whether the first value
   in alov is less than the second*/ 
let builtinLesser: list(value) => value = alov => 
  switch(alov){
  | [NumV(int1), NumV(int2)] => BoolV(int1 < int2)
  | _ => failwith("< expects two integers")
  };
checkExpect(builtinLesser([NumV(9), NumV(10)]), 
            BoolV(true), 
            "builtinLesser: 1");
checkExpect(builtinLesser([NumV(5), NumV(3)]), 
            BoolV(false), 
            "builtinLesser: 2");

/* Input: a list of values, alov
   Output: a value representing the boolean of whether the first value
   in alov is greater than the second*/ 
let builtinGreater: list(value) => value = alov => 
  switch(alov){
  | [NumV(int1), NumV(int2)] => BoolV(int1 > int2)
  | _ => failwith("> expects two integers")
  };
checkExpect(builtinGreater([NumV(9), NumV(10)]), 
            BoolV(false), 
            "builtinGreater: 1");
checkExpect(builtinGreater([NumV(5), NumV(3)]), 
            BoolV(true), 
            "builtinGreater: 2");

/* Input: a list of values, alov
   Output: a value representing a boolean of whether the first value in
   alov is less than or equaL to the second */
let builtinLessEq: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => BoolV(int1 <= int2)
  | _ => failwith("<= expects two integers")
  };
checkExpect(builtinLessEq([NumV(11), NumV(10)]), 
            BoolV(false), 
            "builtinLessEq: 1");
checkExpect(builtinLessEq([NumV(5), NumV(5)]), 
            BoolV(true), 
            "builtinLessEq: 2");

/* Input: a list of values, alov
   Output: a value representing a boolean of whether the first value
   in alov is greater than or equal to the second*/ 
let builtinGreatEq: list(value) => value = alov =>
  switch(alov){
  | [NumV(int1), NumV(int2)] => BoolV(int1 >= int2)
  | _ => failwith(">= expects two integers")
  };
checkExpect(builtinGreatEq([NumV(9), NumV(10)]), 
            BoolV(false), 
            "builtinGreatEq: 1");
checkExpect(builtinGreatEq([NumV(5), NumV(5)]), 
            BoolV(true), 
            "builtinGreatEq: 2");

/* Input: a list of values, alov
   Output: a value representing a boolean of whether the two values in
   alov are equal*/ 
let builtinEqual: list(value) => value = alov =>
  switch(alov){
  | [val1, val2] => BoolV(val1 == val2)
  | _ => failwith("equal? expects two arguments")
  };
checkExpect(builtinEqual([BoolV(true), BoolV(true)]),
            BoolV(true),
            "builtinEqual: 1");
checkExpect(builtinEqual([NumV(3), BoolV(true)]),
            BoolV(false),
            "builtinEqual: 1");

/* Input: a list of a single value, alov
   Output: a value representing whether the value in alov is a number*/
let builtinNumber: list(value) => value = alov =>
  switch(alov){
  | [NumV(_int)] => BoolV(true)
  | [_] => BoolV(false)
  | _ => failwith("number? expects one argument")
  };
checkExpect(builtinNumber([NumV(8)]), BoolV(true), "builtinNumber: 1");
checkExpect(builtinNumber([BoolV(true)]), BoolV(false), "builtinNumber: 2");

/* Input: a list of a single value, alov
   Output: a value representing a boolean of whether the value in alov is
   zero*/ 
let builtinZero: list(value) => value = alov => 
  switch(alov){
  | [NumV(int)] => BoolV(int == 0)
  | _ => failwith("zero? expects one integer")
  };
checkExpect(builtinZero([NumV(0)]), BoolV(true), "builtinZero: 1");
checkExpect(builtinZero([NumV(5)]), BoolV(false), "builtinZero: 2");

/* Input: a list of values, alov
   Output: a value representing a ListV of the values in alov*/ 
let builtinCons: list(value) => value = alov =>
  switch(alov){
  | [val1, ListV(lst)] => ListV(List.cons(val1, lst))
  | _ => failwith("cons expects an argument of type 'a and an argument" ++ 
                  " of type list('a)")
  };
checkExpect(builtinCons([NumV(3), ListV([NumV(4), NumV(5)])]),
            ListV([NumV(3), NumV(4), NumV(5)]),
            "builtinCons: 1");
checkExpect(builtinCons([BoolV(true), ListV([BoolV(false), BoolV(true)])]),
            ListV([BoolV(true), BoolV(false), BoolV(true)]),
            "builtinCons: 2");

/* Input: a list of values, alov
   Output: a value representing the first value in alov*/ 
let builtinFirst: list(value) => value = alov =>
  switch(alov){
  | [ListV(lst)] => switch(lst){
                    | [hd, ..._] => hd 
                    | [] => failwith("first expects a non-empty list")
                    };
  | _ => failwith("first expects one argument of type list") 
  };
checkExpect(builtinFirst([ListV([NumV(3), NumV(4), NumV(5)])]),
            NumV(3),
            "builtinFirst: 1");
checkExpect(builtinFirst([ListV([BoolV(true), BoolV(false), BoolV(true)])]),
            BoolV(true),
            "builtinFirst: 2");

/* Input: a list of values, alov
   Output: a ListV of every item in alov other than the first*/
let builtinRest: list(value) => value = alov =>
  switch(alov){
  | [ListV(lst)] => switch(lst){
                    | [_hd, ...tl] => ListV(tl)
                    | [] => failwith("rest expects a non-empyt list")
                    };
  | _ => failwith("rest expects one argument of type list")
  };
checkExpect(builtinRest([ListV([NumV(3), NumV(4), NumV(5)])]),
            ListV([NumV(4), NumV(5)]),
            "builtinRest: 1");
checkExpect(builtinRest([ListV([BoolV(true), BoolV(false), BoolV(true)])]),
            ListV([BoolV(false), BoolV(true)]),
            "builtinRest: 2");

/* Input: a one element list of ListV, alov
   Output: a value representing whether the ListV in alov is empty*/ 
let builtinEmpty: list(value) => value = alov =>
  switch(alov){
  | [ListV(lst)] => switch(lst){
                    | [] => BoolV(true)
                    | [_, ..._] => BoolV(false)
                    };
  | _ => failwith("empty? expects one argument of type list")
  };
checkExpect(builtinEmpty([ListV([])]), BoolV(true), "builtinEmpty: 1");
checkExpect(builtinEmpty([ListV([NumV(5)])]), BoolV(false), "builtinEmpty: 2");

/* Input: a list of one element ListV, alov
   Output: a value representing whether the element in alov is cons*/ 
let builtinConsP: list(value) => value = alov => 
  switch(alov){
  | [ListV(lst)] => switch(lst){
                    | [] => BoolV(false)
                    | [_, ..._] => BoolV(true)
                    };
  | _ => failwith("cons? expects one argument of type list")
  };
checkExpect(builtinConsP([ListV([])]), BoolV(false), "builtinConsP: 1");
checkExpect(builtinConsP([ListV([NumV(5)])]), BoolV(true), "builtinConsP: 2");

/* Input: a on element list of BoolV, alov
   Output: BoolV(true) if alov is a false BoolV, true otherwise*/
let builtinNot: list(value) => value = alov =>
  switch(alov){
  | [BoolV(bol)] => BoolV(!bol)
  | _ => failwith("not expects one argument of type bool")
  };
checkExpect(builtinNot([BoolV(true)]), BoolV(false), "builtinNot: 1");
checkExpect(builtinNot([BoolV(false)]), BoolV(true), "builtinNot: 2");

/* Defining our top level environment */
let initialTle: environment = 
  [(Name("+"), BuiltinV({bName: "builtinPlus", bProc: builtinPlus,})),
   (Name("-"), BuiltinV({bName: "builtinSub", bProc: builtinSub,})),
   (Name("*"), BuiltinV({bName: "builtinMult", bProc: builtinMult,})),
   (Name("/"), BuiltinV({bName: "builtinDivide", bProc: builtinDivide,})),
   (Name("remainder"), BuiltinV({bName: "builtinRem", bProc: builtinRem,})),
   (Name("="), BuiltinV({bName: "builtinEqualSign", bProc: builtinEqualSign,})),
   (Name("<"), BuiltinV({bName: "builtinLesser", bProc: builtinLesser,})),
   (Name(">"), BuiltinV({bName: "builtinGreater", bProc: builtinGreater,})),
   (Name("<="), BuiltinV({bName: "builtinLessEq", bProc: builtinLessEq,})),
   (Name(">="), BuiltinV({bName: "builtinGreatEq", bProc: builtinGreatEq,})),
   (Name("equal?"), BuiltinV({bName: "builtinEqual", bProc: builtinEqual,})),
   (Name("number?"), BuiltinV({bName: "builtinNumber", bProc: builtinNumber,})),
   (Name("zero?"), BuiltinV({bName: "builtinZero", bProc: builtinZero,})),
   (Name("cons"), BuiltinV({bName: "builtinCons", bProc: builtinCons,})),
   (Name("first"), BuiltinV({bName: "builtinFirst", bProc: builtinFirst,})),
   (Name("rest"), BuiltinV({bName: "builtinRest", bProc: builtinRest,})),
   (Name("empty?"), BuiltinV({bName: "builtinEmpty", bProc: builtinEmpty,})),
   (Name("cons?"), BuiltinV({bName: "builtinConsP", bProc: builtinConsP,})),
   (Name("not"), BuiltinV({bName: "builtinNot", bProc: builtinNot,}))];

/* OI: ListC([SymbolC("and"), 
             ListC([SymbolC(">"), NumberC("8"), NumberC("3")]),
             ListC([SymbolC("<"), NumberC("3"), NumberC("9")])])
 *  RI: ListC([SymbolC(">"), NumberC("8"), NumberC("3")])
 *  RO: ApplicationE([NameE(Name(">")), NumE(8), NumE(3)])
 *  RI: ListC([SymbolC("<"), NumberC("3"), NumberC("9")])
 *  RO: ApplicationE([NameE(Name("<")), NumE(3), NumE(9)])
 * Ideation space: wrap both ROs in an AndE
 * OO: AndE(ApplicationE([NameE(Name(">")), NumE(8), NumE(3)]),
 *          ApplicationE([NameE(Name("<")), NumE(3), NumE(9)]))
 * ------------------
 * OI: ListC([SymbolC("or"), 
             ListC([SymbolC(">"), NumberC("3"), NumberC("7")]),
             ListC([SymbolC("<"), NumberC("3"), NumberC("9")])])
 *  RI: ListC([SymbolC(">"), NumberC("3"), NumberC("7")])
 *  RO: ApplicationE([NameE(Name(">")), NumE(3), NumE(7)])
 *  RI: ListC([SymbolC("<"), NumberC("3"), NumberC("9")])
 *  RO: ApplicationE([NameE(Name("<")), NumE(3), NumE(9)])
 * Ideation space: wrap both ROs in OrE
 * OO: OrE(ApplicationE([NameE(Name(">")), NumE(3), NumE(7)]),
 *         ApplicationE([NameE(Name("<")), NumE(3), NumE(9)]))
 */
/* Input: a concreteProgramPiece, cpp, of type NumberC(int), SymbolC(string), or
 * ListC(list(concreteProgramPiece) where the list(concreteProgramPiece) does
 * not begin with SymbolC("define")
 * Output: the result of converting cpp to an expression
 */
let rec parseExpression: concreteProgramPiece => expression = input => 
  switch(input){
  | NumberC(int) => NumE(int)
  | SymbolC(str) => 
      switch(str){
      | "true" => BoolE(true)
      | "false" => BoolE(false)
      | "empty" => EmptyE
      | _ => NameE(Name(str))
      };
  | ListC(lst) => 
      switch(lst){
      | [SymbolC("and"), expr1, expr2] => AndE(parseExpression(expr1),
                                               parseExpression(expr2))
      | [SymbolC("or"), expr1, expr2] => OrE(parseExpression(expr1),
                                             parseExpression(expr2))
      | [SymbolC("if"), expr1, expr2, expr3] => 
          IfE({boolExpr: parseExpression(expr1),
               trueExpr: parseExpression(expr2),
               falseExpr: parseExpression(expr3),})
      | [SymbolC("cond"), ...tl] => CondE(List.map(getCondData, tl))
      | [SymbolC("lambda"), expr1, expr2] => 
          LambdaE({nameList: getNameList(expr1),
                   lambdaBody: parseExpression(expr2),})
      | [SymbolC("let"), ListC(lst), expr] => 
          LetE({letPairs: getLetPairs(lst),
                letBody: parseExpression(expr),})
      | [SymbolC("true"), ..._]
      | [SymbolC("false"), ..._]
      | [NumberC(_), ..._] => 
          failwith("expected a procedure or keyword after open parentheses")
      | [SymbolC(str), ...tl] => 
          ApplicationE(List.cons(NameE(Name(str)), 
                                 List.map(parseExpression, tl)))
      | [ListC([SymbolC("lambda"), expr1, expr2]), ...tl] => 
          ApplicationE(List.cons(LambdaE({nameList: getNameList(expr1),
                                 lambdaBody: parseExpression(expr2),}), 
                                 List.map(parseExpression, tl)))
      | _ => failwith("syntax error (parse)")
      };
  }
  and 
  /* Input: a concreteProgramPiece, cp
   Output: the condData of cp */ 
  getCondData: concreteProgramPiece => condData = input =>
    switch(input){
    | ListC([expr1, expr2]) => {conditionExpr: parseExpression(expr1),
                                resultExpr: parseExpression(expr2),}
    | _ => failwith("getCondData expects a concreteProgramPeice of type" ++
                    " ListC([expr1, expr2]")
  }
  and
  /* Input: a concreteProgramPiece, input, of type ListC
   Output: a list of names from the data in the ListC of input
   Recursion Diagram 1: 
   OI: ListC([SymbolC("x"), SymbolC("y"), SymbolC("z")])
   RI: ListC([SymbolC("y"), SymbolC("z")])
   Ideation: if the first element in the ListC is a SymbolC, make a
   Name of its string
   RO: ListC([SymbolC("z")])
   OO: [Name("x"), Name("y"), Name("z")] 

   Recursion Diagram 2: 
   OI: ListC([])
   RI: N/A
   Ideation: when given an input in the form of ListC([]), return []
   RO: N/A
   OO: []
   */  
  getNameList: concreteProgramPiece => list(name) = input => 
    switch(input){
    | ListC([]) => []
    | ListC([SymbolC(str), ...tl]) => List.cons(Name(str), 
                                                getNameList(ListC(tl)))
    | _ => failwith("returnNameList expects a concreteProgramPiece of type" ++
                    " ListC([]) or ListC([SymbolC(str), ...tl])")
  }
  and
  /* Input: a list of concreteProgramPiece, cp, of type ListC
   Output: a list of letPairs frmom th edata in ListC

  Recursion Diagram 1: 
   OI: [ListC([SymbolC("x"), NumE(1)]),  ListC([SymbolC("y"), NumE(2)])]
   RI: [ListC([SymbolC("y"), NumE(2)])]
   Ideation: return a list of tuples binding the SymbolC string values
    to the expressions
   RO: []
   OO: [{pairName: "x", pairExpr: NumE(1)}, {pairName: "y", pairExpr: NumE(2)}] 

   Recursion Diagram 2: 
   OI: []
   RI: N/A
   Ideation: when given an input in the form of [], return []
   RO: N/A
   OO: []
   */   
  getLetPairs: list(concreteProgramPiece) => list(letPair) = input =>
    switch(input){
    | [] => []
    | [ListC([SymbolC(str), expr]), ...tl] => 
      List.cons({pairName: Name(str), pairExpr: parseExpression(expr),}, 
                getLetPairs(tl))
    | _ => failwith("getLetPairs expects a list(concreteProgramPiece of type" ++
                   " [] or [ListC([SymbolC(str), expr]), ...tl]")
  };

/* test cases for getCondData */
checkExpect(getCondData(ListC([ListC([SymbolC("not"), SymbolC("true")]),
                              NumberC(5)])),
            {conditionExpr: ApplicationE([NameE(Name("not")),
                                          BoolE(true)]),
             resultExpr: NumE(5),},
             "getCondData: 1");
checkExpect(getCondData(ListC([ListC([SymbolC(">"), NumberC(5), NumberC(2)]),
                              NumberC(5)])),
            {conditionExpr: ApplicationE([NameE(Name(">")),
                                          NumE(5),
                                          NumE(2)]),
             resultExpr: NumE(5),},
             "getCondData: 2");

/* test cases for getNameList */
checkExpect(getNameList(ListC([])), [], "getNameList: 1");
checkExpect(getNameList(ListC([SymbolC("giraffe"),
                               SymbolC("seal")])),
            [Name("giraffe"), Name("seal")],
            "getNameList: 2");

/* test cases for getLetPairs */
checkExpect(getLetPairs([]), [], "getLetPairs: 1");
checkExpect(getLetPairs([ListC([SymbolC("x"), NumberC(6)]),
                         ListC([SymbolC("y"), NumberC(9)])]),
            [{pairName: Name("x"), pairExpr: NumE(6)}, 
             {pairName: Name("y"), pairExpr: NumE(9)}],
             "getLetPairs: 2");

/* test cases for parseExpression */
checkExpectExpression(parseExpression(NumberC(9)), 
                      NumE(9), 
                      "parseExpression: With one NumberC(int)");
checkExpectExpression(parseExpression(SymbolC("true")),
                      BoolE(true),
                      "parseExpression: With SymbolC(str), str is bool");
checkExpectExpression(parseExpression(ListC([SymbolC("and"),
                                             SymbolC("true"),
                                             SymbolC("false")])),
                      AndE(BoolE(true), BoolE(false)),
                      "parseExpression: returning AndE");
checkExpectExpression(parseExpression(ListC([SymbolC("or"),
                                             SymbolC("false"),
                                             SymbolC("true")])),
                      OrE(BoolE(false), BoolE(true)),
                      "parseExpression: returning OrE");
checkExpectExpression(parseExpression(ListC([SymbolC("if"),
                                             SymbolC("true"),
                                             NumberC(3),
                                             NumberC(7)])),
                      IfE({boolExpr: BoolE(true),
                           trueExpr: NumE(3),
                           falseExpr: NumE(7),}),
                      "parseExpression: returning IfE");
checkExpectExpression(parseExpression(ListC([SymbolC("cond"),
                                            ListC([ListC([SymbolC("empty?"),
                                                          SymbolC("input")]),
                                                  SymbolC("empty")]),
                                            ListC([ListC([SymbolC("cons?"),
                                                          SymbolC("input")]),
                                                  NumberC(6)])])),
                    CondE([{conditionExpr: ApplicationE([NameE(Name("empty?")),
                                                         NameE(Name("input"))]),
                            resultExpr: EmptyE,}, 
                           {conditionExpr: ApplicationE([NameE(Name("cons?")),
                                                        NameE(Name("input"))]),
                              resultExpr: NumE(6),}]),
                      "parseExpression: returning CondE");
checkExpectExpression(parseExpression(ListC([SymbolC("lambda"),
                                             ListC([SymbolC("x"),
                                                    SymbolC("y")]),
                                             ListC([SymbolC("+"),
                                                    SymbolC("x"),
                                                    SymbolC("y")])])),
                      LambdaE({nameList: [Name("x"), Name("y")],
                               lambdaBody: ApplicationE([NameE(Name("+")),
                                                         NameE(Name("x")),
                                                         NameE(Name("y"))]),}),
                      "parseExpression: returning LambdaE");
checkExpectExpression(parseExpression(ListC([SymbolC("let"),
                                             ListC([ListC([SymbolC("x"),
                                                           NumberC(5)]),
                                                    ListC([SymbolC("y"),
                                                           NumberC(3)])]),
                                             ListC([SymbolC("+"),
                                                    SymbolC("x"),
                                                    SymbolC("y")])])),
                    LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(5),}, 
                                     {pairName: Name("y"), pairExpr: NumE(3),}],
                            letBody: ApplicationE([NameE(Name("+")),
                                                   NameE(Name("x")),
                                                   NameE(Name("y"))]),}),
                      "parseExpression: returning LetE");

/* Input: a concreteProgramPiece, cpp, of type ListC(list(concreteProgramPiece))
 * where list(concreteProgramPeice) begins with SymbolC("define")
 * Output: the result of converting cpp to a definition
 */
let parseDefinition: concreteProgramPiece => definition = input => 
  switch(input){
  | ListC([SymbolC("define"), SymbolC(str), expr]) => 
      (Name(str), parseExpression(expr))
  | _ => failwith("parseDefintion expects a concreteProgramPiece in the form" ++
                   " ListC([SymbolC(define), ..._]) (note: define should be" ++ 
                   " a string")
  };
/* test cases for parseDefinition */
checkExpectDefinition(parseDefinition(ListC([SymbolC("define"),
                                             SymbolC("plus"),
                                             ListC([SymbolC("lambda"),
                                                    ListC([SymbolC("x"),
                                                           SymbolC("y")]),
                                                    ListC([SymbolC("+"),
                                                           SymbolC("x"),
                                                           SymbolC("y")])])])),
                      (Name("plus"), 
                       LambdaE({nameList: [Name("x"), Name("y")],
                                lambdaBody: ApplicationE([NameE(Name("+")),
                                                         NameE(Name("x")),
                                                         NameE(Name("y"))]),})), 
                      "parseDefinition: with lambda");

/* Input: a concreteProgramPeice, input
 * Output: the result of converting input to an abstractProgramPeice
 */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* Input: a concreteProgram, input
 * Output: the result of converting input to an abstractProgram
 */
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* test cases for parse */
checkExpectAbstractProgram(parse(readAll("(define z 
                                            (lambda (x y) (> x y))) (z 7 8)")),
                           [Definition((Name("z"), 
                            LambdaE({nameList: [Name("x"), Name("y")],
                                    lambdaBody: ApplicationE([NameE(Name(">")),
                                                              NameE(Name("x")),
                                                       NameE(Name("y"))]),}))),  
                            Expression(ApplicationE([NameE(Name("z")),
                                                     NumE(7),
                                                     NumE(8)]))],
                           "parse: with one definition and one expression");

/* Input:  a list of names, nameList, and a list of values, valList
 * Output: an environment in which the bindings are the first of nameList witb
 * the first of valList, the second of nameList with the second of valList, etc.
 */
let rec makeEnv: (list(name), list(value)) => environment = 
  (nameList, valList) =>
    makeEnvHelp(nameList, valList, [])
and
/* Input: a list of names, nameList, a list of values, valList, and
  an environment, env
  Output: an environment that contains the bindings from env, as well as
  new bindings made from tuples of the entries in nameList and valList

  Recursion Diagram 1: 
   OI: [Name("x"), Name("y")], [NumV(1), NumV(2)], []
   RI: [Name("y")], [NumV(2)], [(Name("x"), NumV(1))]
   Ideation: make a tuple out of the first of nameList and valList, add to env
   RO: [], [], [(Name("x"), NumV(1)), (Name("y"), NumV(2))]
   OO: [(Name("x"), NumV(1)), (Name("y"), NumV(2))]

   Recursion Diagram 2: 
   OI: [[], [], [(Name("v"), NumV(85))]
   RI: N/A
   Ideation: when nameList is of the form [], return env
   RO: N/A
   OO: [(Name("v"), NumV(85))]
*/
makeEnvHelp: (list(name), list(value), environment) => environment = 
  (nameList, valList, env) => 
    if (List.length(nameList) == List.length(valList))
    {switch(nameList){
    | [] => env
    | [hd, ..._tl] => 
        List.cons((hd, List.hd(valList)), 
                   makeEnvHelp(List.tl(nameList), List.tl(valList), env))
    };
    } else {
    failwith("number of formal arguments does not match number of actuals")
    };

/* test cases for makeEnvHelp */
checkExpect(makeEnvHelp([], [], []), [], "makeEnvHelp: 1");
checkExpect(makeEnvHelp([Name("deer"), Name("beaver")],
                        [NumV(5), BoolV(true)],
                        []),
            [(Name("deer"), NumV(5)), (Name("beaver"), BoolV(true))],
            "makeEnvHelp: 2");

/* test cases for makeEnv */
checkExpect(makeEnv([], []), [], "makeEnv: empty nameList");
checkExpect(makeEnv([Name("otter"), Name("beaver")], [NumV(4), BoolV(false)]),
            [(Name("otter"), NumV(4)), (Name("beaver"), BoolV(false))],
            "makeEnv: two element nameList and valList");

/* OI: ([(Name("otter"), NumV(4)), (Name("beaver"), BoolV(false))],
         Name("beaver"))
 *  RI: ([(Name("beaver"), BoolV(false))], Name("beaver"))
 *  RO: Some(BoolV(false))
 * Ideation space: The recursive output should be the same as the overall output
 * OO: Some(BoolV(false))
 * ---------------------------
 * OI: ([], Name("squirrel"))
 *  RI: n/a
 *  RO: n/a
 * Ideation space: Recursion is not necessary in this case. lookup called on an
 * empty environment should alsways produce None. 
 * OO: None
 */
/* Input: an environment, env, and a name, n
 * Output: an option indicating whether n is bound to a value in env. If so the 
 * value n is bound to is returned in the option
 */                
let rec lookup: (environment, name) => option(value) = (env, n) => 
    switch(env){
    | [] => None
    | [(name, value), ...tl] => if (name == n) 
                                  {Some(value)} 
                                  else {lookup(tl, n)}
    }; 
/* test cases for lookup */
checkExpect(lookup(initialTle, Name("zebra")), None, "lookup: None");
checkExpect(lookup(initialTle, Name("+")), 
            Some(BuiltinV({bName: "builtinPlus", bProc: builtinPlus,})),
            "lookup: Some(BuiltinV)");

/* Input: two environments, env1 and env2
 * Output: an environment formed by appending env2, env1
 */
let extendEnv: (environment, environment) => environment =
  (env1, env2) => List.append(env2, env1);
/* test cases for extendEvn */
checkExpect(extendEnv([(Name("otter"), NumV(4)), 
                       (Name("beaver"), BoolV(false))],
                      [(Name("dolphin"), NumV(9))]),
            [(Name("dolphin"), NumV(9)), 
             (Name("otter"), NumV(4)), 
             (Name("beaver"), BoolV(false))],
            "extendEnv: 2 element and 1 element environment");
checkExpect(extendEnv([], [(Name("koala"), BoolV(true))]),
            [(Name("koala"), BoolV(true))],
            "extendEnv: empty env and 1 element environment");

/* Input: two environments, tle and env, and and expression, expr.
 * Output: the result of converting expr to a value. the name in any instances
 * of NameE(name) in expr are looked up first in env and then in tle. 
 * Recursion Diagram 1: 
   OI: [(Name("v"), NumV(85))], [], 
      OrE(
        BoolE(builtinEqualSign([NumV(12), Name("v")])), 
        BoolE(builtinEqualSign([NumV(85), Name("v")])))
        )]
   RI: [(Name("v"), NumV(85))], [], 
        BoolE(builtinEqualSign([NumV(85), Name("v")]))))]
   Ideation: evaluate the second BoolE if the first one evaluates to 
  BoolE(false)
   RO: BoolE(true)
   OO: BoolE(true)

   Recursion Diagram 2: 
   OI: [[], [],
     AndE(
        BoolE(builtinEqualSign([NumV(12), Name("v")])), 
        BoolE(builtinEqualSign([NumV(85), Name("v")])))
        )]
   RI: N/A
   Ideation: since the first BoolE evaluates to BoolE(false), return BoolE(false)
   RO: N/A
   OO: BoolE(false)
 */ 
let rec eval: (environment, environment, expression) => value = 
  (tle, env, expr) =>
    switch(expr){
    | NumE(int) => NumV(int)
    | BoolE(bool) => BoolV(bool)
    | EmptyE => ListV([])
    | NameE(Name(str)) => switch(lookup(extendEnv(tle, env), Name(str))){
                          | None => 
                              failwith("a given name " ++ str ++ " is unbound")
                          | Some(value) => value
                          };
    | AndE(expr1, expr2) => 
        switch(eval(tle, env, expr1)){
        | BoolV(false) => BoolV(false)
        | BoolV(true) => 
            switch(eval(tle, env, expr2)){
            | BoolV(true) => BoolV(true)
            | BoolV(false) => BoolV(false)
            | _ => failwith("the second expression in the and expression" ++
                            " does not evaluate to a boolean")
            };
        | _ => failwith("the first expression in the and expression does not" ++
                        " evaluate to a boolean")
        };
    | OrE(expr1, expr2) => 
        switch(eval(tle, env, expr1)){
        | BoolV(true) => BoolV(true)
        | BoolV(false) => 
            switch(eval(tle, env, expr2)){
            | BoolV(true) => BoolV(true)
            | BoolV(false) => BoolV(false)
            | _ => failwith("the second expression in the or expression does" ++
                            " not evaluate to a boolean")
            };
        | _ => failwith("the first expression in the or expression does not" ++
                        " evaluate to a boolean")
        };
    | IfE({boolExpr: expr1, trueExpr: expr2, falseExpr: expr3}) =>
        switch(eval(tle, env, expr1)){
        | BoolV(true) => eval(tle, env, expr2)
        | BoolV(false) => eval(tle, env, expr3)
        | _ => failwith("the test expression in the if expression does not" ++ 
                        " evaluate to a boolean")
        };
    | CondE(listOfCondData) => evalCondData(tle, env, listOfCondData);
    | LambdaE({nameList: nl, lambdaBody: lb}) => 
        ClosureV({cNameList: nl, 
                  cExpr: lb, 
                  cEnv: extendEnv(tle, env)})
    | LetE({letPairs: lop, letBody: expr,}) => 
        eval(tle, 
             extendEnv(env, makeEnv(getNames(lop), getVals(tle, env, lop))), 
             expr)
    | ApplicationE([nameExpr, ...tl]) => 
        switch(eval(tle, env, nameExpr)){
        | BuiltinV(builtin) => builtin.bProc(mapEval(tle, env, tl))
        | ClosureV(closure) => 
            eval(extendEnv(tle, closure.cEnv), 
                 makeEnv(closure.cNameList, mapEval(tle, env, tl)), 
                 closure.cExpr)
        | _ => failwith("expected a procedure after open parentheses")
        };
    | _ => failwith("syntax error (eval)")
    }
and
/* Input: a top level environment, tle, a local environment, env, and a list
  of cond data, cdl
  Output: a value representing the evaluation of cdl given tle and env

Recursion Diagram 1: 
   OI: (Name("v"), NumV(85))], [], 
        [{conditionExpr: BoolE(false), resultExpr: NumE(1)}, 
        {conditionExpr: BoolE(true), resultExpr: NumE(0)}]
   RI: (Name("v"), NumV(85))], [], 
        [{conditionExpr: BoolE(true), resultExpr: NumE(0)}]
   Ideation: once aa conditionExpr evaluates to BoolE(true), return resulExpr
   RO: NumE(0)
   OO: NumE(0)

   Recursion Diagram 2: 
   OI: [], [], []
   RI: N/A
   Ideation: since the condData list is empty, throw error
   RO: N/A
   OO: "all conditions were false"
*/
evalCondData:(environment, environment, list(condData)) => value = 
  (tle, env, cdl) =>
    switch(cdl){
    | [] => failwith("all conditions were false")
    | [hd, ...tl] => 
      switch(eval(tle, env, hd.conditionExpr)){
      | BoolV(false) => evalCondData(tle, env, tl)
      | BoolV(true) => eval(tle, env, hd.resultExpr)
      | _ => failwith("condition expression does not evaluate to a boolean")
      };
  } 
and 
/* Input: two environments, env1 and env2, and a list of expressions, exprList
  Output: a list of values representing the mapping of eval on exprList

Recursion Diagram 1: 
   OI: [(Name("v"), NumV(85))], [], 
        [NumE(1), NumE(2)]
   RI: [(Name("v"), NumV(85))], [], 
        [NumE(2)]
   Ideation: recur through the list of expressions, applying eval to each item
   RO: [(Name("v"), NumV(85))], [], []]
   OO: [(Name("v"), NumV(85))], [], [NumV(1), NumV(2)]]

   Recursion Diagram 2: 
   OI: [], [], []
   RI: N/A
   Ideation: since the list of expressions is empty, return []
   RO: N/A
   OO: []
*/
mapEval: (environment, environment, list(expression)) => list(value) = 
  (env1, env2, exprLst) => 
    switch(exprLst){
    | [] => []
    | [hd, ...tl] => List.cons(eval(env1, env2, hd), mapEval(env1, env2, tl))
    }
and 
/* Input: a list of letPairs, lop
  Output: a list of names representing all of the names in the letPairs of lop

Recursion Diagram 1: 
   OI: [{pairName: "x", pairExpr: NumE(10)}, {pairName: "y", pairExpr: NumE(12)}]
   RI: [{pairName: "y", pairExpr: NumE(12)}]
   Ideation: recur through the letPairs and extract the pairNames
   RO: []
   OO: [Name("x"), Name("y")]

   Recursion Diagram 2: 
   OI: []
   RI: N/A
   Ideation: since the list of letPairs is empty, return []
   RO: N/A
   OO: []
*/
getNames: list(letPair) => list(name) = lop => 
  switch(lop){
  | [] => []
  | [hd, ...tl] => 
      if (List.mem(hd.pairName, getNames(tl)))
      {failwith("the name is already bound in let expression")
      } else {
      List.cons(hd.pairName, getNames(tl))
      }
  }
and 
/* Input: a top level environment, tle, a local environment, env, and a 
  list of letPairs, lop
  Ouptut: a list of values that contains the values from the letPairs in lop

Recursion Diagram 1: 
   OI: [{pairName: "x", pairExpr: NumE(10)}, {pairName: "y", pairExpr: NumE(12)}]
   RI: [{pairName: "y", pairExpr: NumE(12)}]
   Ideation: recur through the letPairs and extract the values
   RO: []
   OO: [NumE(10) NumE(12)]

   Recursion Diagram 2: 
   OI: []
   RI: N/A
   Ideation: since the list of letPairs is empty, return []
   RO: N/A
   OO: []
*/
getVals: (environment, environment, list(letPair)) => list(value) = 
  (tle, env, lop) =>
    switch(lop){
    | [] => []
    | [hd, ...tl] => List.cons(eval(tle, env, hd.pairExpr), 
                               getVals(tle, env, tl))
    };

/* test cases for evalCondData */
checkExpect(evalCondData([(Name(">"), BuiltinV({bName: "builtinGreater", 
                                                bProc: builtinGreater,})),
                          (Name("not"), BuiltinV({bName: "builtinNot", 
                                                  bProc: builtinNot,}))],
                          [],
                          [{conditionExpr: ApplicationE([NameE(Name("not")),
                                          BoolE(true)]),
                             resultExpr: NumE(7),},
                           {conditionExpr: ApplicationE([NameE(Name(">")),
                                          NumE(5),
                                          NumE(2)]),
                             resultExpr: NumE(5),}]),
            NumV(5),
            "evalCondData: 1");

/* test cases for mapEval */
checkExpect(mapEval(initialTle, [], []),
            [],
            "mapEval: 1");
checkExpect(mapEval(initialTle, [], [NumE(8), NumE(3)]),
            [NumV(8), NumV(3)],
            "mapEval: 2");

/* test cases for getNames */
checkExpect(getNames([]), [], "getNames: 1");
checkExpect(getNames([{pairName: Name("x"), pairExpr: NumE(6)}, 
                      {pairName: Name("y"), pairExpr: NumE(9)}]),
            [Name("x"), Name("y")],
            "getNames: 2");

/* test cases for getVals */
checkExpect(getVals(initialTle, [], []), [], "getVals: 1");
checkExpect(getVals(initialTle, 
                    [], 
                    [{pairName: Name("x"), pairExpr: NumE(6)}, 
                     {pairName: Name("y"), pairExpr: NumE(9)}]),
            [NumV(6), NumV(9)],
            "getVals: 2");

/* test cases for eval */
checkExpect(eval(initialTle, [], NumE(9)), NumV(9), "eval: NumE(int)");
checkExpect(eval(initialTle, [], BoolE(false)), BoolV(false), "eval: BoolE");
checkExpect(eval(initialTle, [], NameE(Name("+"))), 
            BuiltinV({bName: "builtinPlus", bProc: builtinPlus,}),
            "eval: NameE(name)");
checkExpect(eval(initialTle, [], ApplicationE([NameE(Name("+")),
                                               NumE(10),
                                               NumE(7)])),
            NumV(17),
            "eval: ApplicationE(addition-procedure)");
checkExpect(eval(initialTle, [], AndE(ApplicationE([NameE(Name(">")),
                                                    NumE(9),
                                                    NumE(2)]), 
                                      ApplicationE([NameE(Name("<")),
                                                    NumE(10),
                                                    NumE(3)]))),
            BoolV(false),
            "eval: AndE");
checkExpect(eval(initialTle, [], OrE(ApplicationE([NameE(Name(">")),
                                                    NumE(9),
                                                    NumE(2)]), 
                                      ApplicationE([NameE(Name("<")),
                                                    NumE(10),
                                                    NumE(3)]))),
            BoolV(true),
            "eval: OrE");
checkExpect(eval(initialTle, 
            [], 
            CondE([{conditionExpr: ApplicationE([NameE(Name(">")),
                                                 NumE(3),
                                                 NumE(100)]),
                    resultExpr: NumE(3),},
                   {conditionExpr: ApplicationE([NameE(Name("<")),
                                                 NumE(3),
                                                 NumE(100)]),
                    resultExpr: NumE(100),}])),
            NumV(100), 
            "eval: CondE");

/* Input: an environment, env, and a definition represented as (id, expr)
 * Output: a new environment with all of the bindings from the env and the 
 * new binding (id, epxr) as well. If id is already defined in env, 
 * addDefinition will return an error. 
 */
let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) => 
    switch(lookup(env, id)){
    | None => [(id, eval(env, [], expr)), ...env]
    | Some(_) => failwith("the given name is already defined in the given" ++
                           " environment")
    };
/* test cases for addDefinition */
checkExpect(addDefinition([(Name("+"), BuiltinV({bName: "builtinPlus", 
                                                 bProc: builtinPlus,})),
                           (Name("-"), BuiltinV({bName: "builtinSub", 
                                                 bProc: builtinSub,}))], 
                           (Name("flamingo"), NumE(7))),
            [(Name("flamingo"), NumV(7)),
             (Name("+"), BuiltinV({bName: "builtinPlus", bProc: builtinPlus,})),
             (Name("-"), BuiltinV({bName: "builtinSub", bProc: builtinSub,}))],
             "addDefinition: 1");
  
/* Input: a value, aValue
 * Output: the result of converting aValue to a string
 * 
 * Recursion Diagram 1:
 * OI: ListV([NumV(1), NumV(2), NumV(3)])
 * RI: ListV([Numv(2), NumV(3)])
 * Ideation: use ++ to join the head with the tail
 * RO: "(cons 1, " ++ stringOfValue(ListV([NumV(2), NumV(3)]))
 * OO: (cons 1 (cons 2 (cons 3 empty)))
 * 
 * Recursion Diagram 2:
 * OI: ListV([])
 * RI: N/A
 * Ideation: return empty list
 * RO: N/A
 * OO: ListV([empty])
 */
let rec stringOfValue: value => string = aValue => 
  switch(aValue){
  | NumV(int) => string_of_int(int)
  | BoolV(bool) => string_of_bool(bool)
  | ListV(alov) => 
      switch(alov){
      | [] => "empty"
      | [hd, ...tl] => 
          ("(cons " ++ 
           stringOfValue(hd) ++ 
           " " ++ 
           stringOfValue(ListV(tl)) ++ ")")
      }
  | BuiltinV(builtin) => builtin.bName 
  | ClosureV(_closure) => "User defined proc"
  };
/* test cases for stringOfValue */
checkExpect(stringOfValue(ListV([NumV(4)])), 
            "(cons 4 empty)", 
            "stringOfValue: one element list");
checkExpect(stringOfValue(ListV([BoolV(true), BoolV(false), BoolV(true)])),
            "(cons true (cons false (cons true empty)))",
            "stringOfValue: three element bool list");
checkExpect(stringOfValue(BuiltinV({bName: "builtinPlus", 
                                    bProc: builtinPlus,})),
           "builtinPlus",
           "stringOfValue: builtin");
checkExpect(stringOfValue(ClosureV({cNameList: [Name("val1"), Name("val2")],
             cExpr: IfE({boolExpr: ApplicationE([NameE(Name(">")), 
                                                NameE(Name("val1")), 
                                                NameE(Name("val2"))]),
                         trueExpr: NameE(Name("val1")),
                         falseExpr: NameE(Name("val2")),}),
              cEnv: initialTle})),
           "User defined proc",
           "stringOfValue: closure")

/* Input: an abstractProgram, pieces
 * Output: a list of values representing the result of converting every 
 * Expression(expression) in pieces to a value. Any abstractProgramPiece's in
 * pieces that are definitions are added to the top-level environment
 */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

/* test cases for process */
checkExpect(process(parse([ListC([SymbolC("define"), 
                                  SymbolC("x"), 
                                  NumberC(9)]), 
                           SymbolC("x")])),
            [NumV(9)],
            "process: with one definition and one expression");
checkExpect(process(parse(readAll("(let
                                       ((x 8)
                                        (y 2))
                                    (* x y))"))),
            [NumV(16)],
            "process: with one let expression");

/* TODO: write the header comment parts required by the Design Recipe */
/* Input: a rawProgram, program
 * Output: the result of reading, parsing, and processing rawprogram, and then
 * converting the resulting values into strings.
 */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* test cases for rackette */
checkExpect(rackette("(+ (* 5 7) (/ 8 4))"), ["37"], "rackette: 3 proc-apps");
checkExpect(rackette("(and (or (zero? 0) (num? 5)) (> 6 7))"), 
            ["false"], 
            "rackette: with and and or exprs");
checkExpect(rackette("(define x 9) x"), ["9"], "rackette: one definition");
checkExpect(rackette("(define x 9) (define y 8) (* x y)"),
            ["72"],
            "rackette: with two name-defs and a proc-app");
checkExpect(rackette("(define f (lambda (x y) (- x y))) (f 8 3)"),
            ["5"],
            "rackette: with one proc-definition and one expr");
checkExpect(rackette("(define g (lambda (x y z) (* (+ x y) z))) 
                      (g 1 2 3) 
                      (g 4 5 6)"),
            ["9", "54"],
            "rackette: with one proc-deff and two exprs");
checkExpect(rackette("(define return-nums (lambda (x) (cond
                                                        ((< x 100) (- x 100))
                                                        ((= x 100) 100)
                                                        ((> x 100) (+ x 100)))))
                      (return-nums 5)
                      (return-nums 100)
                      (return-nums 198)"),
            ["-95", "100", "298"],
            "rackette: with one proc-def and three exprs");
checkExpect(rackette("(define z 6)
                      (define w 3)
                      (let
                          ((x 4)
                           (y 9))
                        (* x y))
                      (* z w) "),
            ["36", "18"],
            "rackette: two name defs, one let-expr, one proc-app");
checkExpect(rackette("(define x 6)
                      (define y 3)
                      (define sample-proc
                        (lambda (z)
                          (let
                              ((x 8)
                               (y 2))
                            (+ (* x y) z))))
                      (sample-proc 5)
                      (+ x y)"),
            ["21", "9"],
            "rackette: two name defs, one proc-def with let, two proc-apps");
checkExpect(rackette("(define my-list (cons 2 (cons 3 empty)))
                      (cons 1 my-list)"),
                     ["(cons 1 (cons 2 (cons 3 empty)))"],
                     "rackette: one name-def (list) one proc-app");
checkExpect(rackette("(define add-one-to-first
                        (lambda (alon)
                          (cond
                           ((empty? alon) empty)
                           ((cons? alon)
                            (cons (+ 1 (first alon)) (rest alon))))))
                      (add-one-to-first (cons 1 (cons 2 (cons 3 empty))))
                      (add-one-to-first empty)"),
            ["(cons 2 (cons 2 (cons 3 empty)))", "empty"],
            "rackette: one proc-def and two list output proc-defs");
checkExpect(rackette("(define silly-proc
                        (lambda (num1 num2)
                          (if (>= num1 num2)
                              (* num1 num2)
                              (+ num1 num2))))
                      (silly-proc 5 5)
                      (silly-proc 3 9)"),
            ["25", "12"],
            "rackette: one proc-def with if and two proc-app-exprs");
checkExpect(rackette("(define c 9)
                      (define d 10)
                      (let
                          ((c 4)
                           (d 3))
                        (let
                            ((bear (lambda (e) (+ (* c d) e))))
                          (bear 7)))
                      (remainder c d)"),
            ["19", "9"],
            "rackette: nested let exprs");
checkExpect(rackette("((lambda (x) (/ 8 x)) 4)"),
            ["2"],
            "rackette: one proc-app with lambda");
checkExpect(rackette("(define c 9)
                      (define d 10)
                      (let
                          ((c 4)
                           (d 3))
                        ((lambda (e) (+ (* c d) e)) 7))
                      (remainder c d)"),
            ["19", "9"],
            "rackette: two name-defs, one let w lambda, one proc-app");
checkExpect(rackette("(define add-one
                        (lambda (alon)
                          (cond
                            ((empty? alon) empty)
                            ((cons? alon)
                             (cons (+ 1 (first alon)) 
                                   (add-one (rest alon)))))))
                     (add-one empty)
                     (add-one (cons 3 (cons 5 (cons 8 (cons 10 empty)))))"),
            ["empty", "(cons 4 (cons 6 (cons 9 (cons 11 empty))))"],
            "rackette: one proc-def and one expr. output type list");
checkExpect(rackette("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"),
            ["34"],
            "rackette: practice on paper 1");
checkExpect(rackette("((lambda (x y) ((lambda (x) (+ x y)) x)) 17 18)"),
            ["35"],
            "rackette: practice on paper 2");
checkExpect(rackette("((lambda (x y) ((lambda (x) (+ x y)) y)) 17 18)"),
            ["36"],
            "rackette: practice on paper 3");
checkExpect(rackette("(let
                          ((x 0))
                        (let (( f (lambda (a) (* x a ))))
                          (let
                              ((x 1 ))
                            (f 5))))"),
            ["0"],
            "rackette: practice on paper 4");
checkExpect(rackette("(let
                          ((x 0)
                           (y 18))
                        (let
                            ((f (lambda (a b) (+ x b)))
                             (x 17))
                          (f y x)))"),
            ["17"],
            "rackette: practice on paper 5");
checkExpect(rackette("(define fact
                        (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))
                      (fact 3)"),
            ["6"],
            "rackette: practice on paper 7");
checkExpect(rackette("(define map
                        (lambda (proc alod)
                          (cond
                            ((empty? alod) empty)
                            ((cons? alod)
                             (cons (proc (first alod)) 
                                   (map proc (rest alod)))))))
                      (define foldr
                        (lambda (proc init alod)
                          (cond
                            ((empty? alod) init)
                            ((cons? alod)
                             (proc (first alod) 
                                   (foldr proc init (rest alod)))))))
                      (define append
                        (lambda (alod1 alod2) (foldr cons alod2 alod1)))
                      (define subset?
                        (lambda (inset outset)
                          (foldr (lambda (item rres) 
                                   (and rres (member? item outset))) 
                                 true 
                                 inset)))
                      (define subsets
                        (lambda (alod)
                          (cond
                            ((empty? alod) (cons empty empty))
                            ((cons? alod)
                             (append (map (lambda (x) (cons (first alod) x)) 
                                          (subsets (rest alod)))
                                     (subsets (rest alod)))))))
                      (subsets (cons 8 (cons 2 (cons 5 empty))))"),
            ["(cons (cons 8 (cons 2 (cons 5 empty)))" ++
             " (cons (cons 8 (cons 2 empty)) (cons (cons 8 (cons 5 empty))" ++
             " (cons (cons 8 empty) (cons (cons 2 (cons 5 empty))" ++
             " (cons (cons 2 empty) (cons (cons 5 empty)" ++
             " (cons empty empty))))))))"],
            "rackette: subsets");

/* Error Checking */
checkError(() => rackette("(+ 3 false)"), 
           "+ expects two integers");
checkError(() => rackette("(3 4 5)"), 
           "expected a procedure or keyword after open parentheses");
checkError(() => rackette("(true false true)"), 
           "expected a procedure or keyword after open parentheses");
checkError(() => rackette("(+ (* 3 5) (- 4 8)"), 
           "wrong number of parentheses");
checkError(() => rackette("(if (+ 2 4) 7 8)"), 
           "the test expression in the if expression does not evaluate to a" ++
            " boolean");
checkError(() => rackette("(* 3 2 5)"), "* expects two integers");
checkError(() => rackette("(define fact
                             (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))
                           (fact 3 2)"),
          "number of formal arguments does not match number of actuals");
checkError(() => rackette("(let 
                               ((x 8)
                                (y 3)
                                (x 2))
                              (+ x y))"),
          "the name is already bound in let expression");
checkError(() => rackette("(add 3 5)"), "a given name add is unbound");
checkError(() => rackette("(and (< 3 5) (- 4 3))"), 
          "the second expression in the and expression does not evaluate to" ++
          " a boolean");
checkError(() => rackette("(or (> 3 5) (cons 4 empty))"), 
          "the second expression in the or expression does not evaluate to" ++
          " a boolean");
checkError(() => rackette("(cond
                             ((> 3 4) 5)
                             ((+ 2 3) 8))"),
          "condition expression does not evaluate to a boolean");
checkError(() => rackette("(define x 3)
                           (define x 2)"),
          "the given name is already defined in the given environment");

/* TODO: Test Cases (we have included a few sample check-expects) */
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(parseExpression(SymbolC("empty")), EmptyE,
  "parse empty expression");
// sample test: parseExpression with read
checkExpectExpression(parseExpression(read("empty")), EmptyE,
  "read and parse empty expression"); 

