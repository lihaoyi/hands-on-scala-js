package torimatomeru

import language.implicitConversions
import syntax._
import org.parboiled2._

class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals {

  def Whitespace = rule { zeroOrMore(WhitespaceChar | Comment) }
  def WhiteLines = rule{ zeroOrMore(WhitespaceChar | Comment | Newline) }
  def White(greedy: Boolean = true) =
    if (greedy) WhiteLines
    else Whitespace

  /**
   * Every token handles space at the end.
   * Don't let it propagate to mixins
   */
  implicit private[this] def wspStr(s: String): Rule0 = rule {
    str(s) ~ WhiteLines
  }
  def wspStrG(s: String, greedy: Boolean): Rule0 = rule {
    str(s) ~ White(greedy)
  }

  implicit private[this] def wspChar(s: Char): Rule0 = rule {
    ch(s) ~ WhiteLines
  }

  def pos = cursor -> cursorChar

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(print(s)) }

  //////////////////////////////////////////////////
  // Override rules from dependencies
  // in order to handle white spaces
  // Note: when you add your AST, make sure to
  // only capture super.rule and not the whitespace
  //////////////////////////////////////////////////

  def IdS(greedy: Boolean = true) = rule { super.Id ~ White(greedy)}
  def VarIdS(greedy: Boolean = true) = rule { super.VarId ~ White(greedy) }
  def LiteralS(greedy: Boolean = true) = rule { super.Literal ~ White(greedy) }
  def SemiS = rule { super.Semi ~ WhiteLines }
  def NewlineS = rule { super.Newline ~ WhiteLines }

  ///////////////////////////////////////////
  // Qualifiers and Ids
  ///////////////////////////////////////////

  def QualId = rule { oneOrMore(IdS()) separatedBy '.' }
  def Ids = rule { oneOrMore(IdS()) separatedBy ',' }

  //path and stableId were refactored (wrt spec) to avoid recursiveness and be more specific 
  def Path: Rule0 = rule { zeroOrMore(IdS() ~ '.') ~ "this" ~ zeroOrMore(IdS()).separatedBy('.') | StableId }
  def StableId: Rule0 = rule {
    zeroOrMore(IdS() ~ '.') ~ ("this" | "super" ~ optional(ClassQualifier)) ~ '.' ~ oneOrMore(IdS()).separatedBy('.') |
      IdS() ~ zeroOrMore('.' ~ IdS())
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
      Path ~ '.' ~ "type" |
      StableId
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

  def Expr(greedy: Boolean = true): Rule0 = rule { (Bindings | optional("implicit") ~ IdS() | "_") ~ "=>" ~ Expr(greedy) | Expr1(greedy) }
  def Expr1(greedy: Boolean = true): Rule0 = rule {
    IfCFlow(greedy) |
    WhileCFlow(greedy) |
    TryCFlow(greedy) |
    DoWhileCFlow(greedy) |
    ForCFlow(greedy) |
    "throw" ~ Expr(greedy) |
    "return" ~ optional(Expr(greedy)) |
    SimpleExpr() ~ ArgumentExprs() ~ '=' ~ Expr(greedy) |
    optional(SimpleExpr() ~ '.') ~ IdS() ~ '=' ~ Expr(greedy) |
    PostfixExpr(greedy) ~ optional("match" ~ '{' ~ CaseClauses ~ '}' | Ascription)
  }

  def IfCFlow(greedy: Boolean = true) = rule { "if" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(NewlineS) ~ Expr(greedy) ~ optional(optional(SemiS) ~ "else" ~ Expr(greedy)) }
  def WhileCFlow(greedy: Boolean = true) = rule { "while" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(NewlineS) ~ Expr(greedy) }
  def TryCFlow(greedy: Boolean = true) = rule { "try" ~ '{' ~ Block ~ wspStrG("}", greedy) ~ optional("catch" ~ '{' ~ CaseClauses ~ wspStrG("}", greedy)) ~ optional("finally" ~ Expr(greedy)) }
  def DoWhileCFlow(greedy: Boolean = true) = rule { "do" ~ Expr() ~ optional(SemiS) ~ "while" ~ '(' ~ Expr() ~ wspStrG(")", greedy) }
  def ForCFlow(greedy: Boolean = true) = rule { "for" ~ ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}') ~ zeroOrMore(NewlineS) ~ optional("yield") ~ Expr(greedy) }
  def PostfixExpr(greedy: Boolean = true): Rule0 = rule { InfixExpr(greedy) ~ optional(IdS() ~ optional(NewlineS)) }
  def InfixExpr(greedy: Boolean = true): Rule0 = rule { PrefixExpr(greedy) ~ zeroOrMore(IdS() ~ optional(NewlineS) ~ PrefixExpr(greedy)) }
  def PrefixExpr(greedy: Boolean = true) = rule { optional(anyOf("-+~!")) ~ SimpleExpr(greedy) }

  def SimpleExpr(greedy: Boolean = true): Rule0 = rule {
    SimpleExpr1(greedy) ~ zeroOrMore('.' ~ IdS() | TypeArgs | ArgumentExprs(greedy)) ~ optional('_')
  }

  def SimpleExpr1(greedy: Boolean = true) = rule{
    "new" ~ (ClassTemplate | TemplateBody) |
    BlockExpr(greedy) |
    LiteralS() ~ drop[String] |
    Path |
    '_' |
    '(' ~ optional(Exprs) ~ wspStrG(")", greedy)
  }


  def Exprs: Rule0 = rule { oneOrMore(Expr()).separatedBy(',') }
  def ArgumentExprs(greedy: Boolean = true): Rule0 = rule {
    '(' ~ (optional(Exprs ~ ',') ~ PostfixExpr() ~ ':' ~ '_' ~ '*' | optional(Exprs)) ~ ')' |
      optional(NewlineS) ~ BlockExpr(greedy)
  }
  def BlockExpr(greedy: Boolean = true): Rule0 = rule { '{' ~ (CaseClauses | Block) ~ wspStrG("}", greedy) }
  def Block: Rule0 = rule { zeroOrMore(BlockStat ~ SemiS) ~ optional(ResultExpr()) }
  def BlockStat: Rule0 = rule {
    &(SemiS) ~ MATCH | //shortcircuit when Semi is found
    Import |
    zeroOrMore(Annotation) ~ (optional("implicit" | "lazy") ~ Def(false) | zeroOrMore(LocalModifier) ~ TmplDef) |
    Expr1(false)
  }
  def ResultExpr(greedy: Boolean = true): Rule0 = rule { (Bindings | optional("implicit") ~ IdS() | "_") ~ "=>" ~ Block | Expr1(true) }
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
      StableId ~ '(' ~ (optional(Patterns ~ ',') ~ optional(VarIdS() ~ '@') ~ '_' ~ '*' | optional(Patterns)) ~ ')' |
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
  def ClassParamClauses: Rule0 = rule { zeroOrMore(ClassParamClause) ~ optional(optional(NewlineS) ~ '(' ~ "implicit" ~ ClassParam ~ ')') }
  def ClassParamClause: Rule0 = rule { optional(NewlineS) ~ '(' ~ optional(ClassParams) ~ ')' }
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

  def TemplateBody: Rule0 = rule { optional(NewlineS) ~ '{' ~ optional(SelfType) ~ TemplateStat(false) ~ zeroOrMore(SemiS ~ TemplateStat(false)) ~ '}' }
  def TemplateStat(greedy: Boolean = true): Rule0 = rule {
    Import |
    zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ (Def(greedy) | Dcl) |
    Expr(false) |
    MATCH
  }

  def SelfType: Rule0 = rule { "this" ~ ':' ~ Type ~ "=>" | IdS() ~ optional(':' ~ Type) ~ "=>" }

  def Import: Rule0 = rule { "import" ~ oneOrMore(ImportExpr).separatedBy(',') }

  //ImportExpr is slightly changed wrt spec because StableId always consumes all the Ids possible, so there is no need to one at the end
  def ImportExpr: Rule0 = rule { StableId ~ optional('.' ~ ('_' | ImportSelectors)) }
  def ImportSelectors: Rule0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | '_') ~ '}' }
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

  def PatVarDef(greedy: Boolean = true): Rule0 = rule { "val" ~ PatDef(greedy) | "var" ~ VarDef(greedy) }
  def Def(greedy: Boolean = true): Rule0 = rule { "def" ~ FunDef(greedy) | "type" ~ zeroOrMore(NewlineS) ~ TypeDef | PatVarDef(greedy) | TmplDef }
  def PatDef(greedy: Boolean = true): Rule0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(':' ~ Type) ~ '=' ~ Expr(greedy) }
  def VarDef(greedy: Boolean = true): Rule0 = rule { Ids ~ ':' ~ Type ~ '=' ~ '_' | PatDef(greedy) }
  def FunDef(greedy: Boolean = true): Rule0 = rule {
    "this" ~ ParamClause ~ ParamClauses ~ ('=' ~ ConstrExpr | optional(NewlineS) ~ ConstrBlock) |
      FunSig ~ (optional(':' ~ Type) ~ '=' ~ Expr(greedy) | optional(NewlineS) ~ '{' ~ Block ~ '}')
  }
  def TypeDef: Rule0 = rule { IdS() ~ optional(TypeParamClause) ~ '=' ~ Type }

  def TmplDef: Rule0 = rule { "trait" ~ TraitDef | optional("case") ~ ("class" ~ ClassDef | "object" ~ ObjectDef) }
  def ClassDef: Rule0 = rule { IdS() ~ optional(TypeParamClause) ~ zeroOrMore(ConstrAnnotation) ~ optional(AccessModifier) ~ ClassParamClauses ~ ClassTemplateOpt }
  def TraitDef: Rule0 = rule { IdS() ~ optional(TypeParamClause) ~ TraitTemplateOpt }
  def ObjectDef: Rule0 = rule { IdS() ~ ClassTemplateOpt }
  def ClassTemplateOpt: Rule0 = rule { "extends" ~ ClassTemplate | optional(optional("extends") ~ TemplateBody) }
  def TraitTemplateOpt: Rule0 = rule { "extends" ~ TraitTemplate | optional(optional("extends") ~ TemplateBody) }
  def ClassTemplate: Rule0 = rule { optional(EarlyDefs) ~ ClassParents ~ optional(TemplateBody) }
  def TraitTemplate: Rule0 = rule { optional(EarlyDefs) ~ TraitParents ~ optional(TemplateBody) }
  def ClassParents: Rule0 = rule { Constr ~ zeroOrMore("with" ~ AnnotType) }
  def TraitParents: Rule0 = rule { AnnotType ~ zeroOrMore("with" ~ AnnotType) }
  def Constr: Rule0 = rule { AnnotType ~ zeroOrMore(ArgumentExprs()) }
  def EarlyDefs: Rule0 = rule { '{' ~ optional(oneOrMore(EarlyDef).separatedBy(SemiS)) ~ '}' ~ "with" }
  def EarlyDef: Rule0 = rule { zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ PatVarDef(false) }
  def ConstrExpr: Rule0 = rule { ConstrBlock | SelfInvocation }
  def ConstrBlock: Rule0 = rule { '{' ~ SelfInvocation ~ zeroOrMore(SemiS ~ BlockStat) ~ '}' }
  def SelfInvocation: Rule0 = rule { "this" ~ oneOrMore(ArgumentExprs()) }

  def TopStatSeq: Rule0 = rule { oneOrMore(TopStat).separatedBy(SemiS) }
  def TopStat: Rule0 = rule { Packaging | PackageObject | Import | zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ TmplDef | MATCH }
  def Packaging: Rule0 = rule { "package" ~ QualId ~ optional(NewlineS) ~ '{' ~ TopStatSeq ~ '}' }
  def PackageObject: Rule0 = rule { "package" ~ "object" ~ ObjectDef }
  def CompilationUnit: Rule0 = rule { zeroOrMore("package" ~ QualId ~ SemiS) ~ TopStatSeq }
}
