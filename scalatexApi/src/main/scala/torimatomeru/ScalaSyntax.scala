package torimatomeru

import language.implicitConversions
import syntax._
import org.parboiled2._

class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals {
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.
  type B = Boolean
  val t = true
  type R0 = Rule0
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule { zeroOrMore(WhitespaceChar | Comment) }

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule{ zeroOrMore(WhitespaceChar | Comment | Newline) }

  /**
   * Whitespace which captures or doesn't-capture
   * newlines depending on the G that gets passed in
   */
  def W(G: B = t) =
    if (G) WL
    else WS

  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately after the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule { str(s) ~ WL }
  implicit private[this] def wspChar(s: Char): R0 = rule { ch(s) ~ WL }

  /**
   * Occasionally, you want to decide whether or not to
   * capture newlines based on the context, so use this
   * and pass in G manually.
   */
  def StrW(s: String, G: B): R0 = rule { str(s) ~ W(G) }



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

  def IdS(G: B = t) = rule { capture(super.Id ~ W(G)) ~> ((x: String) => println("IdS: |" + x + "|")) }
  def VarIdS(G: B = t) = rule { super.VarId ~ W(G) }
  def LiteralS(G: B = t) = rule { super.Literal ~ W(G) }
  def SemiS = rule { super.Semi ~ WL }
  def NewlineS = rule { super.Newline ~ WL }

  ///////////////////////////////////////////
  // Qualifiers and Ids
  ///////////////////////////////////////////

  def QualId(G: B = t) = rule { oneOrMore(IdS(false)) separatedBy '.' ~ W(G) }
  def Ids = rule { oneOrMore(IdS()) separatedBy ',' }

  def Path(G: B = t): R0 = rule {
    zeroOrMore(IdS(G) ~ '.') ~ "this" ~ zeroOrMore(IdS(G)).separatedBy('.') |
    StableId(G)
  }
  def StableId(G: B = t): R0 = rule {
    zeroOrMore(IdS() ~ '.') ~ ("this" | "super" ~ optional(ClassQualifier)) ~ '.' ~ oneOrMore(IdS(G)).separatedBy('.') |
    pr("StableId 1") ~ IdS(false) ~ pr("StableId 2") ~ zeroOrMore(WL ~ '.' ~ WL ~ IdS(false)) ~ pr("StableId 3") ~ W(G) ~ pr("StableId 4")
  }

  def ClassQualifier = rule { '[' ~ IdS() ~ ']' }

  ///////////////////////////////////////////
  // Types and more Types
  ///////////////////////////////////////////

  def Type: R0 = rule { FunctionArgTypes ~ "=>" ~ Type | InfixType ~ optional(ExistentialClause) }
  def FunctionArgTypes = rule { InfixType | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')' }

  def ExistentialClause = rule { "forSome" ~ '{' ~ oneOrMore(ExistentialDcl).separatedBy(SemiS) }
  def ExistentialDcl = rule { "type" ~ TypeDcl | "val" ~ ValDcl }

  def InfixType = rule { CompoundType ~ zeroOrMore(IdS() ~ optional(NewlineS) ~ CompoundType) }
  def CompoundType = rule { oneOrMore(AnnotType(false)).separatedBy(WL ~ "with") ~ optional(Refinement) }
  def AnnotType(G: B = t) = rule {
    SimpleType(false) ~ zeroOrMore(WL ~ Annotation) ~ W(G)
  }
  def SimpleType(G: B = t): R0 = rule {
    BasicType(false) ~
    optional(WL ~
    '#' ~ IdS(false)) ~
    optional(WL ~
    TypeArgs(false)) ~
    W(G)
  }
  def BasicType(G: B = t): R0 = rule {
    '(' ~ Types ~ ')' |
      Path() ~ '.' ~ "type" |
      StableId(G)
  }
  def TypeArgs(G: B = t) = rule { '[' ~ Types ~ StrW("]", G) }
  def Types = rule { oneOrMore(Type).separatedBy(',') }
  def Refinement = rule { optional(NewlineS) ~ '{' ~ oneOrMore(RefineStat).separatedBy(SemiS) ~ '}' }
  def RefineStat = rule { "type" ~ TypeDef | Dcl | MATCH }
  def TypePat = rule { Type }
  def Ascription = rule { ":" ~ (InfixType | oneOrMore(Annotation) | "_" ~ "*") }

  def ParamType = rule { "=>" ~ Type | Type ~ "*" | Type }

  /////////////////////////////////////////////////
  // Declarations, Expressions and Pattern Matching
  /////////////////////////////////////////////////

  def Expr(G: B = t): R0 = rule { (Bindings | optional("implicit") ~ IdS() | "_") ~ "=>" ~ Expr(G) | Expr1(G) }
  def Expr1(G: B = t): R0 = rule {
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

  def IfCFlow(G: B = t) = rule { "if" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(NewlineS) ~ Expr(G) ~ optional(optional(SemiS) ~ "else" ~ Expr(G)) }
  def WhileCFlow(G: B = t) = rule { "while" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(NewlineS) ~ Expr(G) }
  def TryCFlow(G: B = t) = rule { "try" ~ '{' ~ Block ~ StrW("}", G) ~ optional("catch" ~ '{' ~ CaseClauses ~ StrW("}", G)) ~ optional("finally" ~ Expr(G)) }
  def DoWhileCFlow(G: B = t) = rule { "do" ~ Expr() ~ optional(SemiS) ~ "while" ~ '(' ~ Expr() ~ StrW(")", G) }
  def ForCFlow(G: B = t) = rule { "for" ~ ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}') ~ zeroOrMore(NewlineS) ~ optional("yield") ~ Expr(G) }
  def PostfixExpr(G: B = t): R0 = rule { InfixExpr(G) ~ optional(IdS() ~ optional(NewlineS)) }
  def InfixExpr(G: B = t): R0 = rule { PrefixExpr(G) ~ zeroOrMore(IdS() ~ optional(NewlineS) ~ PrefixExpr(G)) }
  def PrefixExpr(G: B = t) = rule { optional(anyOf("-+~!")) ~ SimpleExpr(G) }

  def SimpleExpr(G: B = t): R0 = rule {
    SimpleExpr1(false) ~
    zeroOrMore(
      WL ~ ('.' ~ IdS(false) | TypeArgs(false) | ArgumentExprs(false))
    ) ~
    optional(WL ~ StrW("_", false)) ~
    W(G)
  }

  def SimpleExpr1(G: B = t) = rule{
    "new" ~ (ClassTemplate(G) | TemplateBody(G)) |
    BlockExpr(G) |
    LiteralS(G) ~ drop[String] |
    Path(G) |
    '_' |
    '(' ~ optional(Exprs) ~ StrW(")", G)
  }


  def Exprs: R0 = rule { oneOrMore(Expr()).separatedBy(',') }
  def ArgumentExprs(G: B = t): R0 = rule {
    '(' ~ (optional(Exprs ~ ',') ~ PostfixExpr() ~ ':' ~ '_' ~ '*' | optional(Exprs)) ~ StrW(")", G) |
    optional(NewlineS) ~ BlockExpr(G)
  }

  def BlockExpr(G: B = t): R0 = rule { '{' ~ (CaseClauses | Block) ~ StrW("}", G) }
  def Block: R0 = rule {
    zeroOrMore(BlockStat ~ SemiS) ~ optional(ResultExpr())
  }

  def BlockStat: R0 = rule {
    SemiS |
    Import(false) |
    zeroOrMore(Annotation) ~ (optional("implicit" | "lazy") ~ Def(false) | zeroOrMore(LocalModifier) ~ TmplDef(false)) |
    Expr1(false)
  }
  def ResultExpr(G: B = t): R0 = rule { (Bindings | optional("implicit") ~ IdS() | "_") ~ "=>" ~ Block | Expr1(t) }
  def Enumerators: R0 = rule { Generator ~ zeroOrMore(SemiS ~ Enumerator) }
  def Enumerator: R0 = rule { Generator | Guard | Pattern1 ~ '=' ~ Expr() }
  def Generator: R0 = rule { Pattern1 ~ "<-" ~ Expr() ~ optional(Guard) }
  def CaseClauses: R0 = rule { oneOrMore(CaseClause) }
  def CaseClause: R0 = rule { "case" ~ Pattern ~ optional(Guard) ~ "=>" ~ Block }
  def Guard: R0 = rule { "if" ~ PostfixExpr() }
  def Pattern: R0 = rule {
    pr("Pattern!") ~ oneOrMore(pr("Pattern! oom") ~ Pattern1 ~ pr("Pattern! oom2") ).separatedBy('|')
  }
  def Pattern1: R0 = rule { '_' ~ ':' ~ TypePat | VarIdS() ~ ':' ~ TypePat | Pattern2 }
  def Pattern2: R0 = rule { VarIdS() ~ optional("@" ~ Pattern3) | Pattern3 }
  def Pattern3: R0 = rule {
    SimplePattern ~ zeroOrMore(IdS() ~ SimplePattern)
  }
  def SimplePattern: R0 = rule { 
    '_' |
    LiteralS() ~ drop[String] |
    '(' ~ optional(Patterns) ~ ')' |
    StableId() ~ optional('(' ~ (optional(Patterns ~ ',') ~ optional(VarIdS() ~ '@') ~ '_' ~ '*' | optional(Patterns)) ~ ')') |
    VarIdS() /*|
    XmlPattern*/
  }
  def Patterns: R0 = rule { '_' ~ '*' | oneOrMore(Pattern).separatedBy(',') }

  def TypeParamClause: R0 = rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  def FunTypeParamClause: R0 = rule { '[' ~ oneOrMore(TypeParam).separatedBy(',') ~ ']' }
  def VariantTypeParam: R0 = rule { zeroOrMore(Annotation) ~ optional(anyOf("+-")) ~ TypeParam }
  def TypeParam: R0 = rule { (IdS() | '_') ~ optional(TypeParamClause) ~ optional(">:" ~ Type) ~ optional("<:" ~ Type) ~ zeroOrMore("<%" ~ Type) ~ zeroOrMore(':' ~ Type) }
  def ParamClauses: R0 = rule { zeroOrMore(ParamClause) ~ optional(optional(NewlineS) ~ '(' ~ "implicit" ~ Params ~ ')') }
  def ParamClause: R0 = rule { optional(NewlineS) ~ '(' ~ optional(Params) ~ ')' }
  def Params: R0 = rule { zeroOrMore(Param).separatedBy(',') }
  def Param: R0 = rule { zeroOrMore(Annotation) ~ IdS() ~ optional(':' ~ ParamType) ~ optional('=' ~ Expr()) }
  def ClassParamClauses(G: B = t): R0 = rule { zeroOrMore(ClassParamClause(G)) ~ optional(optional(NewlineS) ~ '(' ~ "implicit" ~ ClassParam ~ StrW(")", G)) }
  def ClassParamClause(G: B = t): R0 = rule { optional(NewlineS) ~ '(' ~ optional(ClassParams) ~ StrW(")", G) }
  def ClassParams: R0 = rule { oneOrMore(ClassParam).separatedBy(',') }
  def ClassParam: R0 = rule { zeroOrMore(Annotation) ~ optional(zeroOrMore(Modifier) ~ ("val" | "var")) ~ IdS() ~ ":" ~ ParamType ~ optional("=" ~ Expr()) }

  def Bindings: R0 = rule { '(' ~ oneOrMore(Binding).separatedBy(',') ~ ')' }
  def Binding: R0 = rule { (IdS() | '_') ~ optional(':' ~ Type) }

  def Modifier: R0 = rule { LocalModifier | AccessModifier | "override" }
  def LocalModifier: R0 = rule { "abstract" | "final" | "sealed" | "implicit" | "lazy" }
  def AccessModifier: R0 = rule { ("private" | "protected") ~ optional(AccessQualifier) }
  def AccessQualifier: R0 = rule { '[' ~ ("this" | IdS()) ~ ']' }

  def Annotation: R0 = rule { '@' ~ SimpleType(false) ~ zeroOrMore(WL ~ ArgumentExprs()) }
  def ConstrAnnotation: R0 = rule { '@' ~ SimpleType() ~ ArgumentExprs() }

  def TemplateBody(G: B = t): R0 = rule {
    WL ~
    '{' ~
    optional(SelfType) ~
    TemplateStat ~
    zeroOrMore(SemiS ~ TemplateStat) ~
    WL ~
    StrW("}", G)
  }
  def TemplateStat: R0 = rule {
    Import(false) |
    zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ (Def(false) | Dcl) |
    Expr(false) |
    MATCH
  }

  def SelfType: R0 = rule { "this" ~ ':' ~ Type ~ "=>" | IdS() ~ optional(':' ~ Type) ~ "=>" }

  def Import(G: B = t): R0 = rule { "import" ~ oneOrMore(ImportExpr(G)).separatedBy(',') }

  //ImportExpr is slightly changed wrt spec because StableId always consumes all the Ids possible, so there is no need to one at the end
  def ImportExpr(G: B = t): R0 = rule { StableId(G) ~ optional('.' ~ (StrW("_", G) | ImportSelectors(G))) }
  def ImportSelectors(G: B = t): R0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | '_') ~ StrW("}", G) }
  def ImportSelector: R0 = rule { IdS() ~ optional("=>" ~ (IdS() | '_')) }

  def Dcl: R0 = rule {
    "val" ~ ValDcl |
    "var" ~ VarDcl |
    "def" ~ FunDcl |
    "type" ~ zeroOrMore(NewlineS) ~ TypeDcl
  }
  def ValDcl: R0 = rule { Ids ~ ':' ~ Type }
  def VarDcl: R0 = rule { Ids ~ ':' ~ Type }
  def FunDcl: R0 = rule { FunSig ~ optional(':' ~ Type) }
  def FunSig: R0 = rule { IdS() ~ optional(FunTypeParamClause) ~ ParamClauses }
  def TypeDcl: R0 = rule { IdS() ~ optional(TypeParamClause) ~ optional(">:" ~ Type) ~ optional("<:" ~ Type) }

  def PatVarDef(G: B = t): R0 = rule { "val" ~ PatDef(G) | "var" ~ VarDef(G) }
  def Def(G: B = t): R0 = rule { "def" ~ FunDef(G) | "type" ~ zeroOrMore(NewlineS) ~ TypeDef | PatVarDef(G) | TmplDef(G) }
  def PatDef(G: B = t): R0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(':' ~ Type) ~ '=' ~ Expr(G) }
  def VarDef(G: B = t): R0 = rule { Ids ~ ':' ~ Type ~ '=' ~ '_' | PatDef(G) }
  def FunDef(G: B = t): R0 = rule {
    "this" ~ ParamClause ~ ParamClauses ~ ('=' ~ ConstrExpr | optional(NewlineS) ~ ConstrBlock) |
      FunSig ~ (optional(':' ~ Type) ~ '=' ~ optional("macro") ~ Expr(G) | optional(NewlineS) ~ '{' ~ Block ~ '}')
  }
  def TypeDef: R0 = rule { IdS() ~ optional(TypeParamClause) ~ '=' ~ Type }

  def TmplDef(G: B = t): R0 = rule {
    "trait" ~ TraitDef(G) |
    optional("case") ~ ("class" ~ ClassDef(G) |
    "object" ~ ObjectDef(G))
  }
  def ClassDef(G: B = t): R0 = rule {
    IdS() ~
    optional(TypeParamClause) ~
    zeroOrMore(ConstrAnnotation) ~
    optional(AccessModifier) ~
    ClassParamClauses(false) ~
    ClassTemplateOpt(false) ~
    W(G)

  }
  def TraitDef(G: B = t): R0 = rule { IdS() ~ optional(TypeParamClause) ~ TraitTemplateOpt(G) }
  def ObjectDef(G: B = t): R0 = rule { IdS() ~ ClassTemplateOpt(G) }
  def ClassTemplateOpt(G: B = t): R0 = rule {
    WL ~ "extends" ~ ClassTemplate(G) |
    optional(WL ~ optional("extends") ~ TemplateBody(G))
  }
  def TraitTemplateOpt(G: B = t): R0 = rule { "extends" ~ TraitTemplate(G) | optional(optional("extends") ~ TemplateBody(G)) }
  def ClassTemplate(G: B = t): R0 = rule {
    optional(EarlyDefs) ~
    ClassParents(false) ~
    optional(WL ~ TemplateBody(false)) ~
    W(G)
  }

  def TraitTemplate(G: B = t): R0 = rule {
    optional(EarlyDefs) ~ TraitParents(false) ~ optional(TemplateBody(false)) ~ W(G)
  }
  def ClassParents(G: B = t): R0 = rule {
    Constr(false) ~ zeroOrMore(WL ~ "with" ~ AnnotType(G)) ~ W(G)
  }
  def TraitParents(G: B = t): R0 = rule {
    AnnotType(false) ~ zeroOrMore(WL ~ "with" ~ AnnotType(false)) ~ W(G)
  }
  def Constr(G: B = t): R0 = rule {
    AnnotType(false) ~ zeroOrMore(WL ~ ArgumentExprs(false)) ~
    W(G)
  }
  def EarlyDefs: R0 = rule {
    '{' ~ optional(oneOrMore(EarlyDef).separatedBy(SemiS)) ~ '}' ~ "with"
  }
  def EarlyDef: R0 = rule {
    zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ PatVarDef(false)
  }
  def ConstrExpr: R0 = rule { ConstrBlock | SelfInvocation }
  def ConstrBlock: R0 = rule { '{' ~ SelfInvocation ~ zeroOrMore(SemiS ~ BlockStat) ~ '}' }
  def SelfInvocation: R0 = rule { "this" ~ oneOrMore(ArgumentExprs()) }

  def TopStatSeq: R0 = rule { zeroOrMore(TopStat).separatedBy(SemiS) }
  def TopStat: R0 = rule { Packaging | PackageObject(false) | Import(false) | zeroOrMore(Annotation ~ optional(NewlineS)) ~ zeroOrMore(Modifier) ~ TmplDef(false) | MATCH }
  def Packaging: R0 = rule { "package" ~ QualId() ~ '{' ~ TopStatSeq ~ '}' }
  def PackageObject(G: B = t): R0 = rule { "package" ~ "object" ~ ObjectDef(G) }
  def CompilationUnit: Rule1[String] = rule {
    capture(
      zeroOrMore(SemiS) ~
      zeroOrMore("package" ~ QualId(false)).separatedBy(SemiS) ~
      TopStatSeq ~
      EOI
    )
  }
}
