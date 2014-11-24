package scalaParser
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
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object K {
    def W(s: String) = rule {
      Key.W(s) ~ WL
    }

    def O(s: String) = rule {
      Key.O(s) ~ WL
    }
  }
  
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

  def Id(G: B = t) = rule { Identifiers.Id ~ W(G) }
  def VarId(G: B = t) = rule { Identifiers.VarId ~ W(G) }
  def Literal(G: B = t) = rule { Literals.Literal ~ W(G) }
  def Semi = rule { Basic.Semi ~ WL }
  def Newline = rule { Basic.Newline ~ WL }

  def QualId(G: B = t) = rule { oneOrMore(Id(false)) separatedBy '.' ~ W(G) }
  def Ids = rule { oneOrMore(Id()) separatedBy ',' }

  def Path(G: B = t): R0 = rule {
    zeroOrMore(Id(G) ~ '.') ~ K.W("this") ~ zeroOrMore(Id(G)).separatedBy('.') |
    StableId(G)
  }
  def StableId(G: B = t): R0 = rule {
    zeroOrMore(Id() ~ '.') ~ (K.W("this") | K.W("super") ~ optional(ClassQualifier)) ~ '.' ~ oneOrMore(Id(G)).separatedBy('.') |
    Id(false) ~ zeroOrMore(WL ~ '.' ~ WL ~ Id(false)) ~ W(G)
  }

  def ClassQualifier = rule { '[' ~ Id() ~ ']' }

  def Type(G: B = t): R0 = rule {
    FunctionArgTypes ~ K.O("=>") ~ Type(G) | InfixType(false) ~ optional(WL ~ ExistentialClause) ~ W(G)
  }
  def FunctionArgTypes = rule {
    InfixType() | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')'
  }

  def ExistentialClause = rule { "forSome" ~ '{' ~ oneOrMore(ExistentialDcl(false)).separatedBy(Semi) }
  def ExistentialDcl(G: B = t) = rule { K.W("type") ~ TypeDcl(G) | K.W("val") ~ ValDcl(G) }

  def InfixType(G: B = t) = rule {
    CompoundType(false) ~ zeroOrMore(WL ~ Id() ~ optional(Newline) ~ CompoundType(false)) ~ W(G)
  }
  def CompoundType(G: B = t) = rule {
    oneOrMore(AnnotType(false)).separatedBy(WL ~ K.W("with")) ~ optional(Refinement(false)) ~ W(G)
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
    Path() ~ '.' ~ K.W("type") |
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

  def ParamType = rule { K.O("=>") ~ Type() | Type() ~ "*" | Type() }

  def Expr(G: B = t): R0 = rule { (Bindings | optional(K.W("implicit")) ~ Id() | "_") ~ K.O("=>") ~ Expr(G) | Expr1(G) }
  def Expr1(G: B = t): R0 = rule {
    IfCFlow(G) |
    WhileCFlow(G) |
    TryCFlow(G) |
    DoWhileCFlow(G) |
    ForCFlow(G) |
    K.W("throw") ~ Expr(G) |
    K.W("return") ~ optional(Expr(G)) |
    SimpleExpr() ~ K.O("=") ~ Expr(G) |
    PostfixExpr(false) ~ optional("match" ~ '{' ~ CaseClauses ~ StrW("}", false) | Ascription(false)) ~ W(G)
  }
  def IfCFlow(G: B = t) = rule { "if" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(Newline) ~ Expr(G) ~ optional(optional(Semi) ~ K.W("else") ~ Expr(G)) }
  def WhileCFlow(G: B = t) = rule { "while" ~ '(' ~ Expr() ~ ')' ~ zeroOrMore(Newline) ~ Expr(G) }
  def TryCFlow(G: B = t) = rule {
    K.W("try") ~ Expr(false) ~
    optional(WL ~ K.W("catch") ~ Expr(false)) ~
    optional(WL ~ K.W("finally") ~ Expr(false)) ~
    W(G)
  }

  def DoWhileCFlow(G: B = t) = rule { K.W("do") ~ Expr() ~ optional(Semi) ~ "while" ~ '(' ~ Expr() ~ StrW(")", G) }
  def ForCFlow(G: B = t) = rule {
    "for" ~
    ('(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}') ~
    zeroOrMore(Newline) ~
    optional(K.W("yield")) ~
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
    K.W("new") ~ (ClassTemplate(G) | TemplateBody(G)) |
    BlockExpr(G) |
    Literal(G) ~ drop[String] |
    Path(G) |
    K.W("_") |
    '(' ~ optional(Exprs) ~ StrW(")", G)
  }



  def Exprs: R0 = rule { oneOrMore(Expr()).separatedBy(',') }
  def ArgumentExprs(G: B = t): R0 = rule {
    '(' ~ optional(Exprs ~ optional(K.O(":") ~ K.W("_") ~ '*')) ~ StrW(")", G) |
    optional(Newline) ~ BlockExpr(G)
  }

  def BlockExpr(G: B = t): R0 = rule { '{' ~ (CaseClauses | Block) ~ StrW("}", G) }
  def Block: R0 = rule {
    zeroOrMore(BlockStat ~ Semi) ~ optional(ResultExpr())
  }

  def BlockStat: R0 = rule {
    Semi |
    Import(false) |
    zeroOrMore(Annotation(false)) ~ (optional(K.W("implicit") | K.W("lazy")) ~ Def(false) | zeroOrMore(LocalModifier) ~ TmplDef(false)) |
    Expr1(false)
  }
  def ResultExpr(G: B = t): R0 = rule { (Bindings | optional(K.W("implicit")) ~ Id() | "_") ~ K.W("=>") ~ Block | Expr1(t) }
  def Enumerators: R0 = rule { Generator(false) ~ zeroOrMore(Semi ~ Enumerator(false)) ~ WL }
  def Enumerator(G: B = t): R0 = rule { Generator(G) | Guard(G) | Pattern1 ~ K.O("=") ~ Expr(G) }
  def Generator(G: B = t): R0 = rule { Pattern1 ~ K.O("<-") ~ Expr(false) ~ optional(WL ~ Guard(false)) ~ W(G) }
  def CaseClauses: R0 = rule { oneOrMore(CaseClause) }
  def CaseClause: R0 = rule { K.W("case") ~ Pattern ~ optional(Guard(true)) ~ K.O("=>") ~ Block }
  def Guard(G: B = t): R0 = rule { K.W("if") ~ PostfixExpr(G) }
  def Pattern: R0 = rule {
    oneOrMore(Pattern1).separatedBy('|')
  }
  def Pattern1: R0 = rule {
    K.W("_") ~ K.O(":") ~ TypePat | VarId() ~ K.O(":") ~ TypePat | Pattern2
  }
  def Pattern2: R0 = rule {
    VarId() ~ "@" ~ Pattern3 | Pattern3 | VarId()
  }
  def Pattern3: R0 = rule {
    SimplePattern ~ zeroOrMore(Id() ~ SimplePattern)
  }
  def SimplePattern: R0 = rule {
    K.W("_") |
    Literal() ~ drop[String] |
    '(' ~ optional(Patterns) ~ ')' |
    (
      StableId() ~
      optional(
        '(' ~
        (optional(Patterns ~ ',') ~ optional(VarId() ~ '@') ~ K.W("_") ~ '*' | optional(Patterns)) ~
        ')'
      )
    ) |
    VarId()
  }
  def Patterns: R0 = rule { K.W("_") ~ '*' | oneOrMore(Pattern).separatedBy(',') }

  def TypeParamClause: R0 = rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  def FunTypeParamClause: R0 = rule { '[' ~ oneOrMore(TypeParam).separatedBy(',') ~ ']' }
  def VariantTypeParam: R0 = rule { zeroOrMore(Annotation()) ~ optional(anyOf("+-")) ~ TypeParam }
  def TypeParam: R0 = rule {
    (Id() | K.W("_")) ~
    optional(TypeParamClause) ~
    optional(K.O(">:") ~ Type()) ~
    optional(K.O("<:") ~ Type()) ~
    zeroOrMore(K.O("<%") ~ Type()) ~
    zeroOrMore(K.O(":") ~ Type())
  }
  def ParamClauses: R0 = rule { zeroOrMore(ParamClause) ~ optional(optional(Newline) ~ '(' ~ K.W("implicit") ~ Params ~ ')') }
  def ParamClause: R0 = rule { optional(Newline) ~ '(' ~ optional(Params) ~ ')' }
  def Params: R0 = rule { zeroOrMore(Param).separatedBy(',') }
  def Param: R0 = rule { zeroOrMore(Annotation()) ~ Id() ~ optional(K.O(":") ~ ParamType) ~ optional(K.O("=") ~ Expr()) }
  def ClassParamClauses(G: B = t): R0 = rule { zeroOrMore(ClassParamClause(G)) ~ optional(optional(Newline) ~ '(' ~ K.W("implicit") ~ ClassParam ~ StrW(")", G)) }
  def ClassParamClause(G: B = t): R0 = rule { optional(Newline) ~ '(' ~ optional(ClassParams) ~ StrW(")", G) }
  def ClassParams: R0 = rule { oneOrMore(ClassParam).separatedBy(',') }
  def ClassParam: R0 = rule { zeroOrMore(Annotation()) ~ optional(zeroOrMore(Modifier) ~ (K.W("val") | K.W("var"))) ~ Id() ~ K.O(":") ~ ParamType ~ optional(K.O("=") ~ Expr()) }

  def Bindings: R0 = rule { '(' ~ oneOrMore(Binding).separatedBy(',') ~ ')' }
  def Binding: R0 = rule { (Id() | K.W("_")) ~ optional(K.O(":") ~ Type()) }

  def Modifier: R0 = rule { LocalModifier | AccessModifier | K.W("override") }
  def LocalModifier: R0 = rule { K.W("abstract") | K.W("final") | K.W("sealed") | K.W("implicit") | K.W("lazy") }
  def AccessModifier: R0 = rule { (K.W("private") | K.W("protected")) ~ optional(AccessQualifier) }
  def AccessQualifier: R0 = rule { '[' ~ (K.W("this") | Id()) ~ ']' }

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

  def SelfType: R0 = rule { K.W("this") ~ K.O(":") ~ Type() ~ K.O("=>") | Id() ~ optional(K.O(":") ~ Type()) ~ K.O("=>") }

  def Import(G: B = t): R0 = rule { K.W("import") ~ oneOrMore(ImportExpr(G)).separatedBy(',') }

  def ImportExpr(G: B = t): R0 = rule { StableId(G) ~ optional('.' ~ (StrW("_", G) | ImportSelectors(G))) }
  def ImportSelectors(G: B = t): R0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | K.W("_")) ~ StrW("}", G) }
  def ImportSelector: R0 = rule { Id() ~ optional(K.O("=>") ~ (Id() | K.W("_"))) }

  def Dcl(G: B = t): R0 = rule {
    K.W("val") ~ ValDcl(G) |
    K.W("var") ~ VarDcl(G) |
    K.W("def") ~ FunDcl(G) |
    K.W("type") ~ zeroOrMore(Newline) ~ TypeDcl(G)
  }
  def ValDcl(G: B = t): R0 = rule { Ids ~ K.O(":") ~ Type(G) }
  def VarDcl(G: B = t): R0 = rule { Ids ~ K.O(":") ~ Type(G) }
  def FunDcl(G: B = t): R0 = rule { FunSig(false) ~ optional(WL ~ K.O(":") ~ Type(G)) }
  def FunSig(G: B = t): R0 = rule { Id() ~ optional(FunTypeParamClause) ~ ParamClauses }
  def TypeDcl(G: B = t): R0 = rule {
    Id(false) ~
    optional(WL ~ TypeParamClause) ~
    optional(WL ~ K.O(">:") ~ Type(false)) ~
    optional(WL ~ K.O("<:") ~ Type(false)) ~
    W(G)
  }

  def PatVarDef(G: B = t): R0 = rule { K.W("val") ~ PatDef(G) | K.W("var") ~ VarDef(G) }
  def Def(G: B = t): R0 = rule { K.W("def") ~ FunDef(G) | K.W("type") ~ zeroOrMore(Newline) ~ TypeDef(G) | PatVarDef(G) | TmplDef(G) }
  def PatDef(G: B = t): R0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(K.O(":") ~ Type()) ~ K.O("=") ~ Expr(G) }
  def VarDef(G: B = t): R0 = rule { Ids ~ K.O(":") ~ Type() ~ K.O("=") ~ K.W("_") | PatDef(G) }
  def FunDef(G: B = t): R0 = rule {
    K.W("this") ~ ParamClause ~ ParamClauses ~ (K.O("=") ~ ConstrExpr | optional(Newline) ~ ConstrBlock) |
    FunSig() ~
    (
      optional(K.O(":") ~ Type()) ~ K.O("=") ~ optional(K.W("macro")) ~ Expr(G) |
      optional(Newline) ~ '{' ~ Block ~ StrW("}", G)
    )
  }
  def TypeDef(G: B = t): R0 = rule { Id() ~ optional(TypeParamClause) ~ K.O("=") ~ Type(G) }

  def TmplDef(G: B = t): R0 = rule {
    K.W("trait") ~ TraitDef(G) |
    optional(K.W("case")) ~ (K.W("class") ~ ClassDef(G) |
    K.W("object") ~ ObjectDef(G))
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
    WL ~ K.W("extends") ~ ClassTemplate(G) |
    optional(WL ~ optional(K.W("extends")) ~ TemplateBody(G))
  }
  def TraitTemplateOpt(G: B = t): R0 = rule { K.W("extends") ~ TraitTemplate(G) | optional(optional(K.W("extends")) ~ TemplateBody(G)) }
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
    Constr(false) ~ zeroOrMore(WL ~ K.W("with") ~ AnnotType(G)) ~ W(G)
  }
  def TraitParents(G: B = t): R0 = rule {
    AnnotType(false) ~ zeroOrMore(WL ~ K.W("with") ~ AnnotType(false)) ~ W(G)
  }
  def Constr(G: B = t): R0 = rule {
    AnnotType(false) ~ zeroOrMore(WL ~ ArgumentExprs(false)) ~
    W(G)
  }
  def EarlyDefs: R0 = rule {
    '{' ~ optional(oneOrMore(EarlyDef).separatedBy(Semi)) ~ '}' ~ K.W("with")
  }
  def EarlyDef: R0 = rule {
    zeroOrMore(Annotation() ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ PatVarDef(false)
  }
  def ConstrExpr: R0 = rule { ConstrBlock | SelfInvocation }
  def ConstrBlock: R0 = rule { '{' ~ SelfInvocation ~ zeroOrMore(Semi ~ BlockStat) ~ '}' }
  def SelfInvocation: R0 = rule { K.W("this") ~ oneOrMore(ArgumentExprs()) }

  def TopStatSeq: R0 = rule { zeroOrMore(TopStat).separatedBy(Semi) }
  def TopStat: R0 = rule {
    Packaging |
    PackageObject(false) |
    Import(false) |
    zeroOrMore(Annotation(false) ~ optional(Newline)) ~ zeroOrMore(Modifier) ~ TmplDef(false) |
    MATCH
  }
  def Packaging: R0 = rule { K.W("package") ~ QualId() ~ '{' ~ TopStatSeq ~ '}' }
  def PackageObject(G: B = t): R0 = rule { K.W("package") ~ K.W("object") ~ ObjectDef(G) }
  def CompilationUnit: Rule1[String] = rule {
    capture(
      WL ~
      zeroOrMore(Semi) ~
      zeroOrMore(K.W("package") ~ QualId(false)).separatedBy(Semi) ~
      TopStatSeq ~
      EOI
    )
  }
}
