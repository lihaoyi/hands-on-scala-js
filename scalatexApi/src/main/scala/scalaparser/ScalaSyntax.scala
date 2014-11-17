package scalaparser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._

/**
 * Parser for Scala syntax.
 *
 * The `G` parameter that gets passed in to each rule stands for
 * "Greedy", and determines whether or not that rule is to consume
 * newlines after the last terminal in that rule. We need to pass it
 * everywhere so it can go all the way to the last terminal deep
 * inside the parse tree, which can then decide whether or not to
 * consume whitespace.
 *
 * The vast majority of terminals will consume newlines; only rules
 * which occur in {} blocks won't have their terminals consume newlines,
 * and only the *last* terminal in the rule will be affected.
 * That's why the parser does terminals-consume-newlines-by-default,
 * and leaves it up to the dev to thread the `G` variable where-ever
 * we want the opposite behavior.
 */
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
  def WS = rule { zeroOrMore(Basic.WhitespaceChar | Literals.Comment) }

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule{ zeroOrMore(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) }

  def Gap = rule{ oneOrMore(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) }

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

  def TypeColon = rule{ str(":") ~ !Basic.OperatorChar ~ WL}

  def pos = cursor -> cursorChar

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def Id(G: B = t) = rule { Identifiers.Id ~ W(G) }
  def VarId(G: B = t) = rule { Identifiers.VarId ~ W(G) }
  def Literal(G: B = t) = rule { Literals.Literal ~ W(G) }
  def Semi = rule { Basic.Semi ~ WL }
  def Newline = rule { Basic.Newline ~ WL }

  def QualId(G: B = t) = rule { oneOrMore(Id(false)) separatedBy '.' ~ W(G) }
  def Ids = rule { oneOrMore(Id()) separatedBy ',' }

  def Path(G: B = t): R0 = rule {
    zeroOrMore(Id(G) ~ '.') ~ "this" ~ zeroOrMore(Id(G)).separatedBy('.') |
    StableId(G)
  }
  def StableId(G: B = t): R0 = rule {
    zeroOrMore(Id() ~ '.') ~ ("this" | "super" ~ optional(ClassQualifier)) ~ '.' ~ oneOrMore(Id(G)).separatedBy('.') |
    Id(false) ~ zeroOrMore(WL ~ '.' ~ WL ~ Id(false)) ~ W(G)
  }

  def ClassQualifier = rule { '[' ~ Id() ~ ']' }

  def Type(G: B = t): R0 = rule {
    FunctionArgTypes ~ "=>" ~ Type(G) | InfixType(false) ~ optional(WL ~ ExistentialClause) ~ W(G)
  }
  def FunctionArgTypes = rule {
    InfixType() | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')'
  }

  def ExistentialClause = rule { "forSome" ~ '{' ~ oneOrMore(ExistentialDcl(false)).separatedBy(Semi) }
  def ExistentialDcl(G: B = t) = rule { "type" ~ TypeDcl(G) | "val" ~ ValDcl(G) }

  def InfixType(G: B = t) = rule {
    CompoundType(false) ~ zeroOrMore(WL ~ Id() ~ optional(Newline) ~ CompoundType(false)) ~ W(G)
  }
  def CompoundType(G: B = t) = rule {
    oneOrMore(AnnotType(false)).separatedBy(WL ~ "with") ~ optional(Refinement(false)) ~ W(G)
  }
  def AnnotType(G: B = t) = rule {
    SimpleType(false) ~ zeroOrMore(WL ~ Annotation(false)) ~ W(G)
  }
  def SimpleType(G: B = t): R0 = rule {
    BasicType(false) ~
    optional(WL ~ '#' ~ Id(false)) ~
    optional(WL ~ TypeArgs(false)) ~
    W(G)
  }
  def BasicType(G: B = t): R0 = rule {
    '(' ~ Types ~ ')' |
    Path() ~ '.' ~ "type" |
    StableId(G)
  }
  def TypeArgs(G: B = t) = rule { '[' ~ Types ~ StrW("]", G) }
  def Types = rule { oneOrMore(Type()).separatedBy(',') }
  def Refinement(G: B = t) = rule {
    optional(Newline) ~ '{' ~ oneOrMore(RefineStat).separatedBy(Semi) ~ StrW("}", G)
  }
  def RefineStat = rule { "type" ~ TypeDef(false) | Dcl(false) | MATCH }
  def TypePat = rule { CompoundType() }
  def Ascription(G: B = t) = rule {
    ":" ~ ("_" ~ StrW("*", G) | InfixType(G) | oneOrMore(Annotation(G)))
  }

  def ParamType = rule { "=>" ~ Type() | Type() ~ "*" | Type() }

  def Expr(G: B = t): R0 = rule { (Bindings | optional("implicit") ~ Id() | "_") ~ "=>" ~ Expr(G) | Expr1(G) }
  def Expr1(G: B = t): R0 = rule {
    IfCFlow(G) |
    WhileCFlow(G) |
    TryCFlow(G) |
    DoWhileCFlow(G) |
    ForCFlow(G) |
    "throw" ~ Expr(G) |
    "return" ~ optional(Expr(G)) |
    SimpleExpr() ~ '=' ~ !Basic.OperatorChar ~ Expr(G) |
    PostfixExpr(false) ~ optional("match" ~ '{' ~ CaseClauses ~ StrW("}", false) | Ascription(false)) ~ W(G)
  }
  def IfCFlow(G: B = t) = rule { "if" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(Newline) ~ Expr(G) ~ optional(optional(Semi) ~ "else" ~ Expr(G)) }
  def WhileCFlow(G: B = t) = rule { "while" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(Newline) ~ Expr(G) }
  def TryCFlow(G: B = t) = rule {
    "try" ~ Expr(false) ~
    optional(WL ~ "catch" ~ Expr(false)) ~
    optional(WL ~ "finally" ~ Expr(false)) ~
    W(G)
  }

  def DoWhileCFlow(G: B = t) = rule { "do" ~ Expr() ~ optional(Semi) ~ "while" ~ '(' ~ Expr() ~ StrW(")", G) }
  def ForCFlow(G: B = t) = rule {
    "for" ~
    ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}') ~
    zeroOrMore(Newline) ~
    optional("yield") ~
    Expr(G) }
  def PostfixExpr(G: B = t): R0 = rule { InfixExpr(G) ~ optional(Id() ~ optional(Newline)) }
  def InfixExpr(G: B = t): R0 = rule { PrefixExpr(G) ~ zeroOrMore(Id() ~ optional(Newline) ~ PrefixExpr(G)) }
  def PrefixExpr(G: B = t) = rule { optional(anyOf("-+~!")) ~ SimpleExpr(G) }

  def SimpleExpr(G: B = t): R0 = rule {
    SimpleExpr1(false) ~
    zeroOrMore(WL ~ ('.' ~ Id(false) | TypeArgs(false) | ArgumentExprs(false))) ~
    optional(WL ~ StrW("_", false)) ~
    W(G)
  }

  def SimpleExpr1(G: B = t) = rule{
    "new" ~ (ClassTemplate(G) | TemplateBody(G)) |
    BlockExpr(G) |
    Literal(G) ~ drop[String] |
    Path(G) |
    '_' |
    '(' ~ optional(Exprs) ~ StrW(")", G)
  }



  def Exprs: R0 = rule { oneOrMore(Expr()).separatedBy(',') }
  def ArgumentExprs(G: B = t): R0 = rule {
    '(' ~ optional(Exprs ~ optional(':' ~ '_' ~ '*')) ~ StrW(")", G) |
    optional(Newline) ~ BlockExpr(G)
  }

  def BlockExpr(G: B = t): R0 = rule { '{' ~ (CaseClauses | Block) ~ StrW("}", G) }
  def Block: R0 = rule {
    zeroOrMore(BlockStat ~ Semi) ~ optional(ResultExpr())
  }

  def BlockStat: R0 = rule {
    Semi |
    Import(false) |
    zeroOrMore(Annotation(false)) ~ (optional("implicit" | "lazy") ~ Def(false) | zeroOrMore(LocalModifier) ~ TmplDef(false)) |
    Expr1(false)
  }
  def ResultExpr(G: B = t): R0 = rule { (Bindings | optional("implicit") ~ Id() | "_") ~ "=>" ~ Block | Expr1(t) }
  def Enumerators: R0 = rule { Generator(false) ~ zeroOrMore(Semi ~ Enumerator(false)) ~ WL }
  def Enumerator(G: B = t): R0 = rule { Generator(G) | Guard(G) | Pattern1 ~ '=' ~ Expr(G) }
  def Generator(G: B = t): R0 = rule { Pattern1 ~ "<-" ~ Expr(false) ~ optional(WL ~ Guard(false)) ~ W(G) }
  def CaseClauses: R0 = rule { oneOrMore(CaseClause) }
  def CaseClause: R0 = rule { "case" ~ Pattern ~ optional(Guard(true)) ~ "=>" ~ Block }
  def Guard(G: B = t): R0 = rule { "if" ~ PostfixExpr(G) }
  def Pattern: R0 = rule {
    oneOrMore(Pattern1).separatedBy('|')
  }
  def Pattern1: R0 = rule {
    '_' ~ TypeColon ~ TypePat | VarId() ~ TypeColon ~ TypePat | Pattern2
  }
  def Pattern2: R0 = rule {
    VarId() ~ "@" ~ Pattern3 | Pattern3 | VarId()
  }
  def Pattern3: R0 = rule {
    SimplePattern ~ zeroOrMore(Id() ~ SimplePattern)
  }
  def SimplePattern: R0 = rule {
    '_' |
    Literal() ~ drop[String] |
    '(' ~ optional(Patterns) ~ ')' |
    (
      StableId() ~
      optional(
        '(' ~
        (optional(Patterns ~ ',') ~ optional(VarId() ~ '@') ~ '_' ~ '*' | optional(Patterns)) ~
        ')'
      )
    ) |
    VarId()
  }
  def Patterns: R0 = rule { '_' ~ '*' | oneOrMore(Pattern).separatedBy(',') }

  def TypeParamClause: R0 = rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  def FunTypeParamClause: R0 = rule { '[' ~ oneOrMore(TypeParam).separatedBy(',') ~ ']' }
  def VariantTypeParam: R0 = rule { zeroOrMore(Annotation()) ~ optional(anyOf("+-")) ~ TypeParam }
  def TypeParam: R0 = rule {
    (Id() | '_') ~
    optional(TypeParamClause) ~
    optional(">:" ~ Type()) ~
    optional("<:" ~ Type()) ~
    zeroOrMore("<%" ~ Type()) ~
    zeroOrMore(TypeColon ~ Type())
  }
  def ParamClauses: R0 = rule { zeroOrMore(ParamClause) ~ optional(optional(Newline) ~ '(' ~ "implicit" ~ Params ~ ')') }
  def ParamClause: R0 = rule { optional(Newline) ~ '(' ~ optional(Params) ~ ')' }
  def Params: R0 = rule { zeroOrMore(Param).separatedBy(',') }
  def Param: R0 = rule { zeroOrMore(Annotation()) ~ Id() ~ optional(TypeColon ~ ParamType) ~ optional('=' ~ Expr()) }
  def ClassParamClauses(G: B = t): R0 = rule { zeroOrMore(ClassParamClause(G)) ~ optional(optional(Newline) ~ '(' ~ "implicit" ~ ClassParam ~ StrW(")", G)) }
  def ClassParamClause(G: B = t): R0 = rule { optional(Newline) ~ '(' ~ optional(ClassParams) ~ StrW(")", G) }
  def ClassParams: R0 = rule { oneOrMore(ClassParam).separatedBy(',') }
  def ClassParam: R0 = rule { zeroOrMore(Annotation()) ~ optional(zeroOrMore(Modifier) ~ ("val" | "var")) ~ Id() ~ TypeColon ~ ParamType ~ optional("=" ~ Expr()) }

  def Bindings: R0 = rule { '(' ~ oneOrMore(Binding).separatedBy(',') ~ ')' }
  def Binding: R0 = rule { (Id() | '_') ~ optional(TypeColon ~ Type()) }

  def Modifier: R0 = rule { LocalModifier | AccessModifier | "override" }
  def LocalModifier: R0 = rule { "abstract" | "final" | "sealed" | "implicit" | "lazy" }
  def AccessModifier: R0 = rule { ("private" | "protected") ~ optional(AccessQualifier) }
  def AccessQualifier: R0 = rule { '[' ~ ("this" | Id()) ~ ']' }

  def Annotation(G: B = t): R0 = rule { '@' ~ SimpleType(false) ~ zeroOrMore(WL ~ ArgumentExprs(false)) ~ W(G) }
  def ConstrAnnotation: R0 = rule { '@' ~ SimpleType() ~ ArgumentExprs() }

  def TemplateBody(G: B = t): R0 = rule {
    WL ~
    '{' ~
    optional(SelfType) ~
    TemplateStat ~
    zeroOrMore(Semi ~ TemplateStat) ~
    WL ~
    StrW("}", G)
  }
  def TemplateStat: R0 = rule {
    Import(false) |
    zeroOrMore(Annotation() ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ (Def(false) | Dcl(false)) |
    Expr(false) |
    MATCH
  }

  def SelfType: R0 = rule { "this" ~ TypeColon ~ Type() ~ "=>" | Id() ~ optional(TypeColon ~ Type()) ~ "=>" }

  def Import(G: B = t): R0 = rule { "import" ~ oneOrMore(ImportExpr(G)).separatedBy(',') }

  def ImportExpr(G: B = t): R0 = rule { StableId(G) ~ optional('.' ~ (StrW("_", G) | ImportSelectors(G))) }
  def ImportSelectors(G: B = t): R0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | '_') ~ StrW("}", G) }
  def ImportSelector: R0 = rule { Id() ~ optional("=>" ~ (Id() | '_')) }

  def Dcl(G: B = t): R0 = rule {
    "val" ~ ValDcl(G) |
    "var" ~ VarDcl(G) |
    "def" ~ FunDcl(G) |
    "type" ~ zeroOrMore(Newline) ~ TypeDcl(G)
  }
  def ValDcl(G: B = t): R0 = rule { Ids ~ TypeColon ~ Type(G) }
  def VarDcl(G: B = t): R0 = rule { Ids ~ TypeColon ~ Type(G) }
  def FunDcl(G: B = t): R0 = rule { FunSig(false) ~ optional(WL ~ TypeColon ~ Type(G)) }
  def FunSig(G: B = t): R0 = rule { Id() ~ optional(FunTypeParamClause) ~ ParamClauses }
  def TypeDcl(G: B = t): R0 = rule {
    Id(false) ~
    optional(WL ~ TypeParamClause) ~
    optional(WL ~ ">:" ~ Type(false)) ~
    optional(WL ~ "<:" ~ Type(false)) ~
    W(G)
  }

  def PatVarDef(G: B = t): R0 = rule { "val" ~ PatDef(G) | "var" ~ VarDef(G) }
  def Def(G: B = t): R0 = rule { "def" ~ FunDef(G) | "type" ~ zeroOrMore(Newline) ~ TypeDef(G) | PatVarDef(G) | TmplDef(G) }
  def PatDef(G: B = t): R0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(TypeColon ~ Type()) ~ '=' ~ Expr(G) }
  def VarDef(G: B = t): R0 = rule { Ids ~ TypeColon ~ Type() ~ '=' ~ '_' | PatDef(G) }
  def FunDef(G: B = t): R0 = rule {
    "this" ~ ParamClause ~ ParamClauses ~ ('=' ~ ConstrExpr | optional(Newline) ~ ConstrBlock) |
    FunSig() ~
    (
      optional(TypeColon ~ Type()) ~ '=' ~ optional("macro") ~ Expr(G) |
      optional(Newline) ~ '{' ~ Block ~ StrW("}", G)
    )
  }
  def TypeDef(G: B = t): R0 = rule { Id() ~ optional(TypeParamClause) ~ '=' ~ Type(G) }

  def TmplDef(G: B = t): R0 = rule {
    "trait" ~ TraitDef(G) |
    optional("case") ~ ("class" ~ ClassDef(G) |
    "object" ~ ObjectDef(G))
  }
  def ClassDef(G: B = t): R0 = rule {
    Id() ~
    optional(TypeParamClause) ~
    zeroOrMore(ConstrAnnotation) ~
    optional(AccessModifier) ~
    ClassParamClauses(false) ~
    ClassTemplateOpt(false) ~
    W(G)

  }
  def TraitDef(G: B = t): R0 = rule { Id() ~ optional(TypeParamClause) ~ TraitTemplateOpt(G) }
  def ObjectDef(G: B = t): R0 = rule { Id() ~ ClassTemplateOpt(G) }
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
    '{' ~ optional(oneOrMore(EarlyDef).separatedBy(Semi)) ~ '}' ~ "with"
  }
  def EarlyDef: R0 = rule {
    zeroOrMore(Annotation() ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ PatVarDef(false)
  }
  def ConstrExpr: R0 = rule { ConstrBlock | SelfInvocation }
  def ConstrBlock: R0 = rule { '{' ~ SelfInvocation ~ zeroOrMore(Semi ~ BlockStat) ~ '}' }
  def SelfInvocation: R0 = rule { "this" ~ oneOrMore(ArgumentExprs()) }

  def TopStatSeq: R0 = rule { zeroOrMore(TopStat).separatedBy(Semi) }
  def TopStat: R0 = rule {
    Packaging |
    PackageObject(false) |
    Import(false) |
    zeroOrMore(Annotation(false) ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ TmplDef(false) |
    MATCH
  }
  def Packaging: R0 = rule { "package" ~ QualId() ~ '{' ~ TopStatSeq ~ '}' }
  def PackageObject(G: B = t): R0 = rule { "package" ~ "object" ~ ObjectDef(G) }
  def CompilationUnit: Rule1[String] = rule {
    capture(
      WL ~
      zeroOrMore(Semi) ~
      zeroOrMore("package" ~ QualId(false)).separatedBy(Semi) ~
      TopStatSeq ~
      EOI
    )
  }
}
