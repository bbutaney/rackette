
/* Data Definition:
  a rawProgram is a program, represented as a string written in 
  Racket/Rackette syntax
*/
type rawProgram = string;
/* Example Data
*  "Hello", "1", "[2, 3]"
*/

/* Data Definition:
 a concreteProgramPiece is a component of a concreteProgram and either
  a NumberC(int),
  a SymbolC(string),
  or a ListC(list(concreteProgramPiece))
*/
type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));
/* Example Data
*  NumberC(1), SymbolC("H"), ListC(list(NumberC(1)))
*/

/* Data Definition:
  a concreteProgram is a list of concreteProgramPieces
*/
type concreteProgram = list(concreteProgramPiece);
/* Example Data:
 * [NumberC(98), NumberC(84))]
 * [ListC([SymbolC("+"), NumberC(98), NumberC(35)]),
 *  ListC([SymbolC("-"), NumberC(100), NumberC(10)])]
 */

/* Data Definition:
  a name is a Name(string) representng a Rackette name
*/
type name =
  | Name(string);
/* Example Data
* Name("donkey"), Name("builtinPlus") 
*/

/* Data Definition:
  an expression represents a Rackette expression in the form of either
  a NumE(int),
  a BoolE(bool),
  an EmptyE,
  a NameE(name),
  an AndE(expression, expression),
  an OrE(expression, expression),
  an IfE(ifData),
  a CondE(list(condData)),
  a LambdaE(lambdaData),
  a LetE(letData),
  or an ApplicationE(list(expression))
*/
type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData)) 
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))
/* Data Definition:
    an ifData is a record type that represents the data inside an if expression
   in rackette, with its data stored as
   {boolExpr: expression, trueExpr: expression, falseExpr: expression}
*/
  and ifData = {
    boolExpr: expression,
    trueExpr: expression,
    falseExpr: expression,
  }
/* Data Definition:
  a condData is a record type that represents the data within a cond expression
  in rackette, with its data stored as
  {conditionExpr: expression, resultExpr: expression}
*/
  and condData = { 
    conditionExpr: expression, 
    resultExpr: expression,
  }
/* Data Definition:
  a lambdaData is a record type that represents the data within a lambda expression
  in rackette, with its data stored as
  {nameList: list(name), lambdaBody: expression} */
  and lambdaData = {
    nameList: list(name),
    lambdaBody: expression,
  } 
/* Data Definition:
  a letPair is a record type that represents a let definition in a letData
  in rackette, with its data stored as
  {pairName: name, pairExpr: expression} */
  and letPair = {
    pairName: name, 
    pairExpr: expression,  
  }
/* Data Definition:
  a letData is a record type that the data within a let expression
  in rackette, with its data stored as
  {letPairs: list(letPair), letBody: expression} */
  and letData = {
    letPairs: list(letPair),
    letBody: expression,
  }
/* Example Data
* expression: 
* NumE(1) 
* BoolE(true) 
* OrE((ApplicationE([NameE("equal?"),
                     NumE(1),
                     NumE(2)])),
      (ApplicationE([NameE("zero?"),
                     NumE(0)])))
* IfE({testExpr: ApplicationE([NameE(Name(">")), NameE(Name("val1")), NameE(Name("val2"))]),
*      trueExpr: NameE(Name("val1")),
*      falseExpr: NameE(Name("val2")),})
* condE({conditionExpr: ApplicationE([NameE(Name(">")), 
                                     NameE(Name("val1")), 
                                     NameE(Name("val2"))]), 
         resultExpr: NumE(1),})
* LambdaE({nameList: [Name("x"), Name("y"),
           lambdaBody: ApplicationE([Name("+"), Name("x"), Name("y")]),}) 
* LetE({letPairs: [{pairname: Name("x"), pairExpr: NumE(5),},
                   {pairname: Name("y"), pairExpr: NumE(9),}],
        letBody: ApplicationE([Name("*"), Name("x"), Name("y")]),})
*/

/* Data Definition:
  a definition is a Rackette definition binds a name to an expression as a tuple*/
type definition = (name, expression);
/* Example Data
 *  (Name("one"), (NumE(1)) 
 *  (Name("T"), BoolE(true))
 */

/* Data Definition:
  an abstractProgramPiece is a piece of Rackette that can be processed:
  either a definition or an expression */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);
/* Example Data
* Definition(Name("foo"), NumE(1))
*/

/* Data Definition:
  and abstractProgram is a representation of a Rackette program -
  a list of any number of pieces */
type abstractProgram = list(abstractProgramPiece);
/* Example Data
* [Definition(Name("one"), NumE(1)), Definition(Name("T"), BoolE(true))]
*/

/* a Rackette value: the result of evaluating a Rackette expression */
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
/* Data Definition:
   a builtinData is a record type of the form
  {bName: string, bProc: list(value) => value}
*/
  and builtinData = { 
    bName: string,
    bProc: list(value) => value,
  }
/* Data Definition:
  a closureData is a record type representing the data in
  a ClosureV of the form
  {cNameList: list(name), cExpr: expression, cEnv: environment} */
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }
/* Example data:
   * value:
   * NumV(37)
   * NumV(89)
   * BoolV(true)
   * BoolV(false)
   * ListV([NumV(90), NumV(34)])
   * ListV([BoolV(true), BoolV(false), BoolV(true)])
   * BuiltinV({bName: "builtinPlus", bProc: builtinPlus,})
   * BuiltinV({bName: "builtinCons", bProc: builtinCons,})
   * ClosureV({cNameList: [Name("n1"), Name("n2")],
   *           cExpr: ApplicationE([NameE(Name("+")), NameE(Name("n1")), NameE(Name("n2"))])
   *           cEnv: [(Name("+"), BuiltinV({bName: "builtinPlus", bProc: builtinPlus,})]})
   * ClosureV({cNameList: [Name("val1"), Name("val2")],
   *           cExpr: IfE({boolExpr: ApplicationE([NameE(Name(">")), NameE(Name("val1")), NameE(Name("val2"))]),
   *                       trueExpr: NameE(Name("val1")),
   *                       falseExpr: NameE(Name("val2")),}),
   *           cEnv: [(Name(">"), BuiltinV({bName: "builtinGreater", bProc: builtinGreater,})]})
   */

  /* Environments and bindings aren't values
     But we use "and" here so bindings have access to values
     and closures have access to environments 
     Data Definition:
    an environment is a list of bindings
     */
  and environment = (list(binding))
  /* Data Definition:
     a binding is a tuple of a name and a value bound to said name */
  and binding = (name, value);
  /* Example data: 
   * binding:
   * (Name("x"), NumV(5))
   * (Name("+"), BuiltinV({bName: "builtinPlus", bProc: builtinPlus,}))
   * environment:
   * [(Name("x"), NumV(5)), 
   *  (Name("+"), BuiltinV({bName: "builtinPlus", bProc: builtinPlus,}))]
   * [(Name("seventeen"), NumV(17)),
   *  (Name("sequential list"), ListV([NumV(1), NumV(2), NumV(3)]))]
   */