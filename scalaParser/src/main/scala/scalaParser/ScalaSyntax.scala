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
   * By default, all strings and characters greedily
   * capture all whitespace immediately after the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule { WL ~ str(s)  }
  implicit private[this] def wspChar(s: Char): R0 = rule { WL ~ ch(s) }

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object K {
    def W(s: String) = rule{ WL ~ Key.W(s) }

    def O(s: String) = rule{ WL ~ Key.O(s) }
  }

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def Id = rule { WL ~ Identifiers.Id }
  def VarId = rule { WL ~ Identifiers.VarId }
  def Literal = rule { WL ~ Literals.Literal }
  def Semi = rule { WS ~ Basic.Semi }
  def Semis = rule { oneOrMore(Semi) }
  def Newline = rule { WL ~ Basic.Newline }

  def QualId = rule { WL ~ oneOrMore(Id).separatedBy('.') }
  def Ids = rule { oneOrMore(Id) separatedBy ',' }

  def NotNewline: R0 = rule{ &( WS ~ noneOf("\n") )}
  def OneNewlineMax: R0 = rule{ WS ~ optional(Basic.Newline) ~ NotNewline}
  def StableId: R0 = {
    def ClassQualifier = rule { '[' ~ Id ~ ']' }
    rule {
      zeroOrMore(Id ~ '.') ~ (K.W("this") | K.W("super") ~ optional(ClassQualifier)) ~ zeroOrMore('.' ~ Id) |
      Id ~ zeroOrMore('.' ~ Id)
    }
  }
  def ExistentialDcl = rule { K.W("type") ~ TypeDcl | K.W("val") ~ ValDcl }
  def ExistentialClause = rule {
    "forSome" ~ '{' ~ oneOrMore(ExistentialDcl).separatedBy(Semi) ~ '}'
  }
  def Type: R0 = {
    def WildcardType: R0 = rule{ K.W("_") }

    def FunctionArgTypes = rule {
      InfixType | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')'
    }
    rule {
      (WildcardType |
      FunctionArgTypes ~ K.O("=>") ~ Type |
      InfixType ~ optional(ExistentialClause)) ~ TypeBounds
    }
  }

  def InfixType = rule {
    CompoundType ~ zeroOrMore(Id ~ OneNewlineMax ~ CompoundType)
  }
  def CompoundType = {
    def RefineStat = rule { "type" ~ TypeDef | Dcl | MATCH }
    def Refinement = rule {
      OneNewlineMax ~ '{' ~ oneOrMore(RefineStat).separatedBy(Semi) ~ "}"
    }
    rule {
      oneOrMore(AnnotType).separatedBy(K.W("with")) ~ optional(Refinement) |
      Refinement
    }
  }
  def AnnotType = rule {
    SimpleType ~ optional(NotNewline ~ oneOrMore(Annotation))
  }
  def SimpleType: R0 = {
    def BasicType: R0 = rule {
      '(' ~ Types ~ ')'  |
        StableId ~ '.' ~ K.W("type") |
        StableId
    }
    rule {
      BasicType ~
        optional('#' ~ Id) ~
        optional(TypeArgs)
    }
  }

  def TypeArgs = rule { '[' ~ Types ~ "]" }
  def Types = rule { oneOrMore(Type).separatedBy(',') }


  def TypePat = rule { CompoundType }
  def FunctionArgTypes = rule {
    InfixType | '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')'
  }
  def Ascription = rule {
     ":" ~ ("_" ~ "*" |  InfixType | oneOrMore(Annotation))
  }

  def ParamType = rule { K.O("=>") ~ Type | Type ~ "*" | Type }

  def LambdaHead: R0 = {
    def Bindings: R0 = {
      def Binding: R0 = rule { (Id | K.W("_")) ~ optional(K.O(":") ~ Type) }
      rule { '(' ~ zeroOrMore(Binding).separatedBy(',') ~ ')' }
    }
    rule{
      (
        Bindings |
        optional(K.W("implicit")) ~ Id ~ optional(Ascription) |
        K.W("_") ~ optional(Ascription)
      ) ~
      K.O("=>")
    }
  }
  def Enumerators(G: Boolean = false): R0 = {
    def Generator: R0 = rule { Pattern1 ~ K.O("<-") ~ Expr0(G) ~ optional(Guard(G))  }
    def Enumerator: R0 = rule { Generator | Guard(G) | Pattern1 ~ K.O("=") ~ Expr0(G) }
    rule { Generator ~ zeroOrMore(Semis ~ Enumerator) ~ WL }
  }
  def Expr = Expr0()
  def ExprSensitive = Expr0(true)
  def Expr0(G: Boolean = false): R0 = {
    def IfCFlow = rule { "if" ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ optional(optional(Semi) ~ K.W("else") ~ Expr0(G)) }
    def WhileCFlow = rule { "while" ~ '(' ~ Expr ~ ')' ~ Expr0(G) }
    def TryCFlow = rule {
      K.W("try") ~ Expr0(G) ~
        optional(K.W("catch") ~ Expr0(G)) ~
        optional(K.W("finally") ~ Expr0(G))
    }

    def DoWhileCFlow = rule { K.W("do") ~ Expr0(G) ~ optional(Semi) ~ "while" ~ '(' ~ Expr ~ ")" }
    def ForCFlow = {

      rule {
        "for" ~
        ('(' ~ Enumerators() ~ ')' | '{' ~ Enumerators(G = true) ~ '}') ~
        optional(K.W("yield")) ~
        Expr0(G)
      }
    }
    rule {
      zeroOrMore(LambdaHead) ~ (
        IfCFlow |
        WhileCFlow |
        TryCFlow |
        DoWhileCFlow |
        ForCFlow |
        K.W("throw") ~ Expr0(G) |
        K.W("return") ~ optional(Expr0(G)) |
        SimpleExpr ~ K.O("=") ~ Expr0(G) |
        PostfixExpr(G) ~ optional("match" ~ '{' ~ CaseClauses ~ "}" | Ascription)
      )
    }
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def PrefixExpr = rule { optional(WL ~ anyOf("-+~!")) ~ SimpleExpr }
    def Check = if (G) OneNewlineMax else MATCH
    def Check0 = if (G) NotNewline else MATCH
    def InfixExpr: R0 = rule {
      PrefixExpr ~
      zeroOrMore(
        Check0 ~
        Id ~
        Check ~
        PrefixExpr
      )
    }
    rule { InfixExpr ~ optional(NotNewline ~ Id ~ optional(Newline)) }
  }

  def SimpleExpr: R0 = {
    def Path: R0 = rule {
      zeroOrMore(Id ~ '.') ~ K.W("this") ~ zeroOrMore('.' ~ Id) |
      StableId
    }
    def SimpleExpr1 = rule{
      K.W("new") ~ (ClassTemplate | TemplateBody) |
      BlockExpr |
      Literal |
      Path |
      K.W("_") |
      '(' ~ optional(Exprs) ~ ")"
    }
    rule {
      SimpleExpr1 ~
      zeroOrMore('.' ~ Id | TypeArgs | ArgumentExprs) ~
      optional(K.W("_"))
    }
  }

  def Exprs: R0 = rule { oneOrMore(Expr).separatedBy(',') }
  def ArgumentExprs: R0 = rule {
    '(' ~ optional(Exprs ~ optional(K.O(":") ~ K.W("_") ~ '*')) ~ ")" |
      OneNewlineMax ~ BlockExpr
  }

  def BlockExpr: R0 = rule { '{' ~ (CaseClauses | Block) ~ "}" }

  def BlockStats: R0 = {

    def BlockStat: R0 = rule {
      Import |
      zeroOrMore(Annotation) ~ (optional(K.W("implicit") | K.W("lazy")) ~ Def | zeroOrMore(LocalModifier) ~ TmplDef) |
      Expr0(true)
    }
    rule{ oneOrMore(BlockStat).separatedBy(Semis) }
  }

  def Block: R0 = {
    def BlockEnd: R0 = rule{ optional(Semis) ~ &("}" | K.W("case")) }
    def ResultExpr: R0 = Expr0(true)
    rule {
      zeroOrMore(LambdaHead) ~
      optional(Semis) ~
      (
        ResultExpr ~ BlockEnd |
        BlockStats ~ optional(Semis ~ ResultExpr) ~ BlockEnd |
        MATCH ~ BlockEnd
      )
    }
  }



  def CaseClauses: R0 = {
    def CaseClause: R0 = rule { K.W("case") ~ Pattern ~ optional(Guard()) ~ K.O("=>") ~ Block }
    rule { oneOrMore(CaseClause) }
  }

  def Guard(G: Boolean = false): R0 = rule { K.W("if") ~ PostfixExpr(G) }
  def Pattern: R0 = rule {
    oneOrMore(Pattern1).separatedBy('|')
  }
  def Pattern1: R0 = rule {
    K.W("_") ~ K.O(":") ~ TypePat | VarId ~ K.O(":") ~ TypePat | Pattern2
  }
  def Pattern2: R0 = {
    def Pattern3: R0 = rule {
      SimplePattern ~ zeroOrMore(Id ~ SimplePattern)
    }
    rule{ VarId ~ "@" ~ Pattern3 | Pattern3 | VarId }
  }

  def SimplePattern: R0 = {
    def Patterns: R0 = rule { K.W("_") ~ '*' | oneOrMore(Pattern).separatedBy(',') }
    rule {
      K.W("_") |
        Literal |
        '(' ~ optional(Patterns) ~ ')' |
        (
          StableId ~
          optional(
            '(' ~
            (optional(Patterns ~ ',') ~ optional(VarId ~ '@') ~ K.W("_") ~ '*' | optional(Patterns)) ~
            ')'
          )
        ) |
        VarId
    }
  }


  def TypeParamClause: R0 = {
    def VariantTypeParam: R0 = rule { zeroOrMore(Annotation) ~ optional(WL ~ anyOf("+-")) ~ TypeParam }
    rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  }
  def FunTypeParamClause: R0 = rule {
    '[' ~ oneOrMore(zeroOrMore(Annotation) ~ TypeParam).separatedBy(',') ~ ']' }
  def TypeBounds: R0 = rule{
    optional(K.O(">:") ~ Type) ~
    optional(K.O("<:") ~ Type)
  }
  def TypeParam: R0 = rule {
    (Id | K.W("_")) ~
    optional(TypeParamClause) ~
    TypeBounds ~
    zeroOrMore(K.O("<%") ~ Type) ~
    zeroOrMore(K.O(":") ~ Type)
  }
  def ParamClauses: R0 = rule {
    zeroOrMore(ParamClause) ~ optional(OneNewlineMax ~ '(' ~ K.W("implicit") ~ Params ~ ')')
  }
  def ParamClause: R0 = rule { OneNewlineMax ~ '(' ~ optional(Params) ~ ')' }
  def Params: R0 = {
    def Param: R0 = rule { zeroOrMore(Annotation) ~ Id ~ optional(K.O(":") ~ ParamType) ~ optional(K.O("=") ~ Expr) }
    rule { zeroOrMore(Param).separatedBy(',') }
  }

  def ClassParam: R0 = rule {
    zeroOrMore(Annotation) ~
    optional(zeroOrMore(Modifier) ~ (K.W("val") | K.W("var"))) ~
    Id ~
    K.O(":") ~
    ParamType ~
    optional(K.O("=") ~ Expr)
  }

  def Modifier: R0 = rule { LocalModifier | AccessModifier | K.W("override") }
  def LocalModifier: R0 = rule { K.W("abstract") | K.W("final") | K.W("sealed") | K.W("implicit") | K.W("lazy") }
  def AccessModifier: R0 = {
    def AccessQualifier: R0 = rule { '[' ~ (K.W("this") | Id) ~ ']' }
    rule { (K.W("private") | K.W("protected")) ~ optional(AccessQualifier) }
  }

  def Annotation: R0 = rule {  '@' ~  SimpleType ~  zeroOrMore(ArgumentExprs)  }

  def TemplateBody: R0 = rule {
    '{' ~
    optional(SelfType) ~
    zeroOrMore(TemplateStat).separatedBy(Semis) ~
    '}'
  }
  def TemplateStat: R0 = rule {
    Import |
    zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ (Def | Dcl) |
    Expr0(true)
  }

  def SelfType: R0 = rule { K.W("this") ~ K.O(":") ~ Type ~ K.O("=>") | Id ~ optional(K.O(":") ~ Type) ~ K.O("=>") }

  def Import: R0 = {
    def ImportExpr: R0 = rule {
      StableId ~ optional('.' ~ (K.W("_") | ImportSelectors))
    }
    def ImportSelectors: R0 = rule { '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | K.W("_")) ~ "}" }
    def ImportSelector: R0 = rule { Id ~ optional(K.O("=>") ~ (Id | K.W("_"))) }
    rule { K.W("import") ~ oneOrMore(ImportExpr).separatedBy(',') }
  }

  def Dcl: R0 = {

    def VarDcl: R0 = rule { Ids ~ K.O(":") ~ Type }
    def FunDcl: R0 = rule { FunSig ~ optional(K.O(":") ~ Type) }

    rule {
      K.W("val") ~ ValDcl |
      K.W("var") ~ VarDcl |
      K.W("def") ~ FunDcl |
      K.W("type") ~ TypeDcl
    }
  }
  def FunSig: R0 = rule { Id ~ optional(FunTypeParamClause) ~ ParamClauses }
  def ValDcl: R0 = rule { Ids ~ K.O(":") ~ Type }
  def TypeDcl: R0 = rule {
    Id ~
    optional(TypeParamClause) ~
    TypeBounds
  }

  def PatVarDef: R0 = {
    def PatDef: R0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(K.O(":") ~ Type) ~ K.O("=") ~ Expr0(true) }
    def VarDef: R0 = rule { Ids ~ K.O(":") ~ Type ~ K.O("=") ~ K.W("_") | PatDef }
    rule { K.W("val") ~ PatDef | K.W("var") ~ VarDef }
  }
  def Def: R0 = {
    def ConstrExpr: R0 = rule { ConstrBlock | SelfInvocation }
    def FunDef: R0 = rule {
      K.W("this") ~ ParamClause ~ ParamClauses ~ (K.O("=") ~ ConstrExpr | OneNewlineMax ~ ConstrBlock) |
        FunSig ~
          (
            optional(K.O(":") ~ Type) ~ K.O("=") ~ optional(K.W("macro")) ~ Expr |
              OneNewlineMax ~ '{' ~ Block ~ "}"
            )
    }
    rule { K.W("def") ~ FunDef | K.W("type") ~ TypeDef | PatVarDef | TmplDef }
  }

  def TypeDef: R0 = rule { Id ~ optional(TypeParamClause) ~ K.O("=") ~ Type }

  def TmplDef: R0 = {
    def TraitTemplate: R0 = {
      def TraitParents: R0 = rule {
        AnnotType ~ zeroOrMore(K.W("with") ~ AnnotType)
      }
      rule{ optional(EarlyDefs) ~ TraitParents ~ optional(TemplateBody) }
    }
    def ClassParamClauses: R0 = {
      def Implicit: R0 = rule{
        OneNewlineMax ~
        '(' ~
        K.W("implicit") ~
        ClassParam ~
        ")"
      }

      def ClassParamClause: R0 = {
        def ClassParams: R0 = rule { oneOrMore(ClassParam).separatedBy(',') }
        rule { OneNewlineMax ~'(' ~ optional(ClassParams) ~ ")" }
      }
      rule {
        oneOrMore(ClassParamClause) ~ optional(Implicit) | Implicit
      }
    }
    def ConstrPrelude: R0 = {
      def Annot: R0 = rule { '@' ~ SimpleType ~ ArgumentExprs }
      rule{
        NotNewline ~ (
          oneOrMore(Annot) ~ optional(AccessModifier) |
          zeroOrMore(Annot) ~ AccessModifier
        )
      }
    }
    def ClassDef: R0 = rule {
      Id ~
      optional(TypeParamClause) ~
      optional(ConstrPrelude) ~
      optional(ClassParamClauses) ~
      ClassTemplateOpt
    }
    def TraitTemplateOpt: R0 = rule {
      K.W("extends") ~ TraitTemplate | optional(optional(K.W("extends")) ~ TemplateBody)
    }
    def TraitDef: R0 = rule { Id ~ optional(TypeParamClause) ~ TraitTemplateOpt }
    rule {
      K.W("trait") ~ TraitDef |
      optional(K.W("case")) ~ (
        K.W("class") ~ ClassDef |
        K.W("object") ~ ObjectDef
      )
    }
  }


  def ObjectDef: R0 = rule { Id ~ ClassTemplateOpt }
  def ClassTemplateOpt: R0 = rule {
    K.W("extends") ~ ClassTemplate |
      optional(optional(K.W("extends")) ~ TemplateBody)
  }

  def ClassTemplate: R0 = {
    def ClassParents: R0 = {
      def Constr: R0 = rule{ AnnotType ~ zeroOrMore(ArgumentExprs) }
      rule{ Constr ~ zeroOrMore(K.W("with") ~ AnnotType) }
    }
    rule{ optional(EarlyDefs) ~ ClassParents ~ optional(TemplateBody) }
  }

  def EarlyDefs: R0 = {
    def EarlyDef: R0 = rule {
      zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ PatVarDef
    }
    rule{ '{' ~ optional(oneOrMore(EarlyDef).separatedBy(Semis)) ~ '}' ~ K.W("with") }
  }

  def ConstrBlock: R0 = rule { '{' ~ SelfInvocation ~ optional(Semis ~ BlockStats) ~ '}' }
  def SelfInvocation: R0 = rule { K.W("this") ~ oneOrMore(ArgumentExprs) }

  def TopStatSeq: R0 = {
    def PackageObject: R0 = rule { K.W("package") ~ K.W("object") ~ ObjectDef }
    def Packaging: R0 = rule { K.W("package") ~ QualId ~ '{' ~ TopStatSeq ~ '}' }
    def TopStat: R0 = rule {
      Packaging |
      PackageObject |
      Import |
      zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ TmplDef
    }
    rule { oneOrMore(TopStat).separatedBy(Semis) }
  }

  def CompilationUnit: Rule1[String] = {
    def TopPackageSeq: R0 = rule{
      oneOrMore(K.W("package") ~ QualId).separatedBy(Semis)
    }
    rule {
      capture(
        optional(Semis) ~
        (TopPackageSeq ~ optional(Semis ~ TopStatSeq) | TopStatSeq) ~
        optional(Semis) ~
        WL
      )
    }
  }
}
