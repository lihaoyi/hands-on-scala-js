package torimatomeru

import language.implicitConversions
import syntax._
import org.parboiled2._

class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals {

  type B = Boolean
  
  def Whitespace = rule { zeroOrMore(WhitespaceChar | Comment) }
  def WhiteLines = rule{ zeroOrMore(WhitespaceChar | Comment | Newline) }
  def White(G: B = true) =
    if (G) WhiteLines
    else Whitespace

  /**
   * Every token handles space at the end.
   * Don't let it propagate to mixins
   */
  implicit private[this] def wspStr(s: String): Rule0 = rule {
    str(s) ~ WhiteLines
  }
  def wspStrG(s: String, G: B): Rule0 = rule {
    str(s) ~ White(G)
  }

  implicit private[this] def wspChar(s: Char): Rule0 = rule {
    ch(s) ~ WhiteLines
  }

  def pos = cursor -> cursorChar

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  //////////////////////////////////////////////////
  // Override rules from dependencies
  // in order to handle white spaces
  // Note: when you add your AST, make sure to
  // only capture super.rule and not the whitespace
  //////////////////////////////////////////////////

  def IdS(G: B = true) = rule { super.Id ~ White(G)}
  def VarIdS(G: B = true) = rule { super.VarId ~ White(G) }
  def LiteralS(G: B = true) = rule { super.Literal ~ White(G) }
  def SemiS = rule { super.Semi ~ WhiteLines }
  def NewlineS = rule { super.Newline ~ WhiteLines }

  ///////////////////////////////////////////
  // Qualifiers and Ids
  ///////////////////////////////////////////

  def QualId = rule { oneOrMore(IdS()) separatedBy '.' }
  def Ids = rule { oneOrMore(IdS()) separatedBy ',' }

  //path and stableId were refactored (wrt spec) to avoid recursiveness and be more specific 
  def Path(G: B = true): Rule0 = rule { zeroOrMore(IdS(G) ~ '.') ~ "this" ~ zeroOrMore(IdS(G)).separatedBy('.') | StableId(G) }
  def StableId(G: B = true): Rule0 = rule {
    zeroOrMore(IdS() ~ '.') ~ ("this" | "super" ~ optional(ClassQualifier)) ~ '.' ~ oneOrMore(IdS(G)).separatedBy('.') |
      IdS(G) ~ zeroOrMore('.' ~ IdS(G))
  }
//    def StableId: Rule0 = rule { zeroOrMore(Id ~ '.') ~ optional("this" | "super" ~ optional(ClassQualifier)) ~ oneOrMore(Id).separatedBy('.') }
  def ClassQualifier = rule { '[' ~ IdS() ~ ']' }

  ///////////////////////////////////////////
  // Types and more Types
  ///////////////////////////////////////////

  def Type: Rule0 = rule { FunctionArgTypes ~ "=>" ~ Type | InfixType ~ optional(ExistentialClause) }
  def FunctionArgTypes = rule { InfixType | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')' }

  def ExistentialClause = rule { "forSome" ~ '{' ~ oneOrMore(ExistentialDcl).separatedBy(SemiS) }
  def ExistentialDcl = rule { "type" ~ TypeDcl | "val" ~ ValDcl }

  def InfixType = rule { CompoundType ~ zeroOrMore(IdS() ~ optional(NewlineS) ~ CompoundType) }
  def CompoundType = rule { oneOrMore(AnnotType).separatedBy("with") ~ optional(Refinement) }
  def AnnotType = rule { SimpleType ~ zeroOrMore(Annotation) }
  def SimpleType: Rule0 = rule {
    BasicType ~ optional('#' ~ IdS()) ~ optional(TypeArgs)
  }
  def BasicType: Rule0 = rule {
    '(' ~ Types ~ ')' |
      Path() ~ '.' ~ "type" |
      StableId()
  }
  def TypeArgs = rule { '[' ~ Types ~ ']' }
  def Types = rule { oneOrMore(Type).separatedBy(',') }
  def Refinement = rule { optional(NewlineS) ~ '{' ~ oneOrMore(RefineStat).separatedBy(SemiS) ~ '}' }
  def RefineStat = rule { "type" ~ TypeDef | Dcl | MATCH }
  def TypePat = rule { Type }
  def Ascription = rule { ":" ~ (InfixType | oneOrMore(Annotation) | "_" ~ "*") }

  def ParamType = rule { "=>" ~ Type | Type ~ "*" | Type }

  /////////////////////////////////////////////////
  // Declarations, Expressions and Pattern Matching
  /////////////////////////////////////////////////

  def Expr(G: B = true): Rule0 = rule { (Bindings | optional("implicit") ~ IdS() | "_") ~ "=>" ~ Expr(G) | Expr1(G) }
  def Expr1(G: B = true): Rule0 = rule {
    IfCFlow(G) |
    WhileCFlow(G) |
    TryCFlow(G) |
    DoWhileCFlow(G) |
    ForCFlow(G) |
    "throw" ~ Expr(G) |
    "return" ~ optional(Expr(G)) |
    SimpleExpr() ~ ArgumentExprs() ~ '=' ~ Expr(G) |
    optional(SimpleExpr() ~ '.') ~ IdS() ~ '=' ~ Expr(G) |
    PostfixExpr(G) ~ optional("match" ~ '{' ~ CaseClauses ~ '}' | Ascription)
  }

  def IfCFlow(G: B = true) = rule { "if" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(NewlineS) ~ Expr(G) ~ optional(optional(SemiS) ~ "else" ~ Expr(G)) }
  def WhileCFlow(G: B = true) = rule { "while" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(NewlineS) ~ Expr(G) }
  def TryCFlow(G: B = true) = rule { "try" ~ '{' ~ Block ~ wspStrG("}", G) ~ optional("catch" ~ '{' ~ CaseClauses ~ wspStrG("}", G)) ~ optional("finally" ~ Expr(G)) }
  def DoWhileCFlow(G: B = true) = rule { "do" ~ Expr() ~ optional(SemiS) ~ "while" ~ '(' ~ Expr() ~ wspStrG(")", G) }
  def ForCFlow(G: B = true) = rule { "for" ~ ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}') ~ zeroOrMore(NewlineS) ~ optional("yield") ~ Expr(G) }
  def PostfixExpr(G: B = true): Rule0 = rule { InfixExpr(G) ~ optional(IdS() ~ optional(NewlineS)) }
  def InfixExpr(G: B = true): Rule0 = rule { PrefixExpr(G) ~ zeroOrMore(IdS() ~ optional(NewlineS) ~ PrefixExpr(G)) }
  def PrefixExpr(G: B = true) = rule { optional(anyOf("-+~!")) ~ SimpleExpr(G) }

  def SimpleExpr(G: B = true): Rule0 = rule {
    SimpleExpr1(G) ~
    zeroOrMore('.' ~ IdS() | TypeArgs | ArgumentExprs(G)) ~
    optional('_')
  }

  def SimpleExpr1(G: B = true) = rule{
    "new" ~ (ClassTemplate(G) | TemplateBody(G)) |
    BlockExpr(G) |
    LiteralS(G) ~ drop[String] |
    Path(G) |
    '_' |
    '(' ~ optional(Exprs) ~ wspStrG(")", G)
  }


  def Exprs: Rule0 = rule { oneOrMore(Expr()).separatedBy(',') }
  def ArgumentExprs(G: B = true): Rule0 = rule {
    '(' ~ (optional(Exprs ~ ',') ~ PostfixExpr() ~ ':' ~ '_' ~ '*' | optional(Exprs)) ~ wspStrG(")", G) |
    optional(NewlineS) ~ BlockExpr(G)
  }

  def BlockExpr(G: B = true): Rule0 = rule { '{' ~ (CaseClauses | Block) ~ wspStrG("}", G) }
  def Block: Rule0 = rule {
    zeroOrMore(BlockStat ~ SemiS) ~ optional(ResultExpr())
  }

  def BlockStat: Rule0 = rule {
    SemiS |
    Import(false) |
    zeroOrMore(Annotation) ~ (optional("implicit" | "lazy") ~ Def(false) | zeroOrMore(LocalModifier) ~ TmplDef(false)) |
    Expr1(false)
  }
  def ResultExpr(G: B = true): Rule0 = rule { (Bindings | optional("implicit") ~ IdS() | "_") ~ "=>" ~ Block | Expr1(true) }
  def Enumerators: Rule0 = rule { Generator ~ zeroOrMore(SemiS ~ Enumerator) }
  def Enumerator: Rule0 = rule { Generator | Guard | Pattern1 ~ '=' ~ Expr() }
  def Generator: Rule0 = rule { Pattern1 ~ "<-" ~ Expr() ~ optional(Guard) }
  def CaseClauses: Rule0 = rule { oneOrMore(CaseClause) }
  def CaseClause: Rule0 = rule { "case" ~ Pattern ~ optional(Guard) ~ "=>" ~ Block }
  def Guard: Rule0 = rule { "if" ~ PostfixExpr() }
  def Pattern: Rule0 = rule { oneOrMore(Pattern1) separatedBy '|' }
  def Pattern1: Rule0 = rule { '_' ~ ':' ~ TypePat | VarIdS() ~ ':' ~ TypePat | Pattern2 }
  def Pattern2: Rule0 = rule { VarIdS() ~ optional("@" ~ Pattern3) | Pattern3 }
  def Pattern3: Rule0 = rule { SimplePattern ~ zeroOrMore(IdS() ~ optional(NewlineS) ~ SimplePattern) } // this pattern doesn't make sense to me...
  def SimplePattern: Rule0 = rule {
    '_' |
    LiteralS() ~ drop[String] | //literal currently captures, so it can be used outside. but since all our rules lack AST, we drop its value in order to be able to compose them
    '(' ~ optional(Patterns) ~ ')' |
    StableId() ~ '(' ~ (optional(Patterns ~ ',') ~ optional(VarIdS() ~ '@') ~ '_' ~ '*' | optional(Patterns)) ~ ')' |
    VarIdS() /*|
    XmlPattern*/
  }
  def Patterns: Rule0 = rule { '_' ~ '*' | oneOrMore(Pattern).separatedBy(',') }

  def TypeParamClause: Rule0 = rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  def FunTypeParamClause: Rule0 = rule { '[' ~ oneOrMore(TypeParam).separatedBy(',') ~ ']' }
  def VariantTypeParam: Rule0 = rule { zeroOrMore(Annotation) ~ optional(anyOf("+-")) ~ TypeParam }
  def TypeParam: Rule0 = rule { (IdS() | '_') ~ optional(TypeParamClause) ~ optional(">:" ~ Type) ~ optional("<:" ~ Type) ~ zeroOrMore("<%" ~ Type) ~ zeroOrMore(':' ~ Type) }
  def ParamClauses: Rule0 = rule { zeroOrMore(ParamClause) ~ optional(optional(NewlineS) ~ '(' ~ "implicit" ~ Params ~ ')') }
  def ParamClause: Rule0 = rule { optional(NewlineS) ~ '(' ~ optional(Params) ~ ')' }
  def Params: Rule0 = rule { zeroOrMore(Param).separatedBy(',') }
  def Param: Rule0 = rule { zeroOrMore(Annotation) ~ IdS() ~ optional(':' ~ ParamType) ~ optional('=' ~ Expr()) }
  def ClassParamClauses(G: B = true): Rule0 = rule { zeroOrMore(ClassParamClause(G)) ~ optional(optional(NewlineS) ~ '(' ~ "implicit" ~ ClassParam ~ wspStrG(")", G)) }
  def ClassParamClause(G: B = true): Rule0 = rule { optional(NewlineS) ~ '(' ~ optional(ClassParams) ~ wspStrG(")", G) }
  def ClassParams: Rule0 = rule { oneOrMore(ClassParam).separatedBy(',') }
  def ClassParam: Rule0 = rule { zeroOrMore(Annotation) ~ optional(zeroOrMore(Modifier) ~ ("val" | "var")) ~ IdS() ~ ":" ~ ParamType ~ optional("=" ~ Expr()) }

  def Bindings: Rule0 = rule { '(' ~ oneOrMore(Binding).separatedBy(',') ~ ')' }
  def Binding: Rule0 = rule { (IdS() | '_') ~ optional(':' ~ Type) }

  def Modifier: Rule0 = rule { LocalModifier | AccessModifier | "override" }
  def LocalModifier: Rule0 = rule { "abstract" | "final" | "sealed" | "implicit" | "lazy" }
  def AccessModifier: Rule0 = rule { ("private" | "protected") ~ optional(AccessQualifier) }
  def AccessQualifier: Rule0 = rule { '[' ~ ("this" ~ IdS()) ~ ']' }

  def Annotation: Rule0 = rule { '@' ~ SimpleType ~ zeroOrMore(ArgumentExprs()) }
  def ConstrAnnotation: Rule0 = rule { '@' ~ SimpleType ~ ArgumentExprs() }

  def TemplateBody(G: B = true): Rule0 = rule {
    WhiteLines ~
    '{' ~
    optional(SelfType) ~
    TemplateStat(false) ~
    zeroOrMore(SemiS ~ TemplateStat(false)) ~
    WhiteLines ~
    wspStrG("}", G)
  }
  def TemplateStat(G: B = true): Rule0 = rule {
    Import(false) |
    zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ (Def(G) | Dcl) |
    Expr(false) |
    MATCH
  }

  def SelfType: Rule0 = rule { "this" ~ ':' ~ Type ~ "=>" | IdS() ~ optional(':' ~ Type) ~ "=>" }

  def Import(G: B = true): Rule0 = rule { "import" ~ oneOrMore(ImportExpr(G)).separatedBy(',') }

  //ImportExpr is slightly changed wrt spec because StableId always consumes all the Ids possible, so there is no need to one at the end
  def ImportExpr(G: B = true): Rule0 = rule { StableId(G) ~ optional('.' ~ (wspStrG("_", G) | ImportSelectors(G))) }
  def ImportSelectors(G: B = true): Rule0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | '_') ~ wspStrG("}", G) }
  def ImportSelector: Rule0 = rule { IdS() ~ optional("=>" ~ (IdS() | '_')) }

  def Dcl: Rule0 = rule {
    "val" ~ ValDcl |
    "var" ~ VarDcl |
    "def" ~ FunDcl |
    "type" ~ zeroOrMore(NewlineS) ~ TypeDcl
  }
  def ValDcl: Rule0 = rule { Ids ~ ':' ~ Type }
  def VarDcl: Rule0 = rule { Ids ~ ':' ~ Type }
  def FunDcl: Rule0 = rule { FunSig ~ optional(':' ~ Type) }
  def FunSig: Rule0 = rule { IdS() ~ optional(FunTypeParamClause) ~ ParamClauses }
  def TypeDcl: Rule0 = rule { IdS() ~ optional(TypeParamClause) ~ optional(">:" ~ Type) ~ optional("<:" ~ Type) }

  def PatVarDef(G: B = true): Rule0 = rule { "val" ~ PatDef(G) | "var" ~ VarDef(G) }
  def Def(G: B = true): Rule0 = rule { "def" ~ FunDef(G) | "type" ~ zeroOrMore(NewlineS) ~ TypeDef | PatVarDef(G) | TmplDef(G) }
  def PatDef(G: B = true): Rule0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(':' ~ Type) ~ '=' ~ Expr(G) }
  def VarDef(G: B = true): Rule0 = rule { Ids ~ ':' ~ Type ~ '=' ~ '_' | PatDef(G) }
  def FunDef(G: B = true): Rule0 = rule {
    "this" ~ ParamClause ~ ParamClauses ~ ('=' ~ ConstrExpr | optional(NewlineS) ~ ConstrBlock) |
      FunSig ~ (optional(':' ~ Type) ~ '=' ~ optional("macro") ~ Expr(G) | optional(NewlineS) ~ '{' ~ Block ~ '}')
  }
  def TypeDef: Rule0 = rule { IdS() ~ optional(TypeParamClause) ~ '=' ~ Type }

  def TmplDef(G: B = true): Rule0 = rule { "trait" ~ TraitDef(G) | optional("case") ~ ("class" ~ ClassDef(G) | "object" ~ ObjectDef(G)) }
  def ClassDef(G: B = true): Rule0 = rule {
    IdS() ~
    optional(TypeParamClause) ~
    zeroOrMore(ConstrAnnotation) ~
    optional(AccessModifier) ~
    ClassParamClauses(G) ~
    ClassTemplateOpt(G)
  }
  def TraitDef(G: B = true): Rule0 = rule { IdS() ~ optional(TypeParamClause) ~ TraitTemplateOpt(G) }
  def ObjectDef(G: B = true): Rule0 = rule { IdS() ~ ClassTemplateOpt(G) }
  def ClassTemplateOpt(G: B = true): Rule0 = rule {
    "extends" ~ ClassTemplate(G) |
    optional(optional("extends") ~ TemplateBody(G))
  }
  def TraitTemplateOpt(G: B = true): Rule0 = rule { "extends" ~ TraitTemplate(G) | optional(optional("extends") ~ TemplateBody(G)) }
  def ClassTemplate(G: B = true): Rule0 = rule {
    optional(EarlyDefs) ~
    ClassParents ~
    optional(TemplateBody(G))
  }

  def TraitTemplate(G: B = true): Rule0 = rule { optional(EarlyDefs) ~ TraitParents ~ optional(TemplateBody(G)) }
  def ClassParents: Rule0 = rule { Constr ~ zeroOrMore("with" ~ AnnotType) }
  def TraitParents: Rule0 = rule { AnnotType ~ zeroOrMore("with" ~ AnnotType) }
  def Constr: Rule0 = rule { AnnotType ~ zeroOrMore(ArgumentExprs()) }
  def EarlyDefs: Rule0 = rule {
    '{' ~ optional(oneOrMore(EarlyDef).separatedBy(SemiS)) ~ '}' ~ "with"
  }
  def EarlyDef: Rule0 = rule { zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ PatVarDef(false) }
  def ConstrExpr: Rule0 = rule { ConstrBlock | SelfInvocation }
  def ConstrBlock: Rule0 = rule { '{' ~ SelfInvocation ~ zeroOrMore(SemiS ~ BlockStat) ~ '}' }
  def SelfInvocation: Rule0 = rule { "this" ~ oneOrMore(ArgumentExprs()) }

  def TopStatSeq: Rule0 = rule { zeroOrMore(TopStat).separatedBy(SemiS) }
  def TopStat: Rule0 = rule { Packaging | PackageObject(false) | Import(false) | zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ TmplDef(false) | MATCH }
  def Packaging: Rule0 = rule { "package" ~ QualId ~ optional(NewlineS) ~ '{' ~ TopStatSeq ~ '}' }
  def PackageObject(G: B = true): Rule0 = rule { "package" ~ "object" ~ ObjectDef(G) }
  def CompilationUnit: Rule1[String] = rule { capture(zeroOrMore(SemiS) ~ zeroOrMore("package" ~ QualId).separatedBy(SemiS) ~ TopStatSeq ~ EOI) }
}
