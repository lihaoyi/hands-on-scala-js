Twist
=====

Twist is a lightweight, whitespace-delimited, markdown-like relative of the [Twirl](https://github.com/playframework/twirl) templating engine. It allows you to write 

```
@(titleString: String)(sidebar: Html)(content: Html)

@html
  @head
    @title @titleString
  @body
    @section(cls:="sidebar") @sidebar
    @section(cls:="content") @content
```

Instead of the more verbose Twirl-template

```html
@(title: String)(sidebar: Html)(content: Html)
<!DOCTYPE html>
<html>
  <head>
    <title>@title</title>
  </head>
  <body>
    <section class="sidebar">@sidebar</section>
    <section class="content">@content</section>
  </body>
</html>
```

Apart from the syntactic difference, Twirlite templates are just as type-safe as Twirl templates. Furthermore, they use the [Scalatags](https://github.com/lihaoyi/scalatags) HTML-generation library under the hood, making them render around [4x faster](https://github.com/lihaoyi/scalatags#performance) than the equivalent Twirl template. Like Twirl templates, you can use arbitrary function calls, control-flow structures like `if`/`for`, and other Scala expressions within your templates.

Why Twirlite?
-------------

Twirlite emerged out of the deficiencies of other languages used for marking up text.

###[Markdown](http://en.wikipedia.org/wiki/Markdown) 

is nice to use but too inflexible: it is impossible to define abstractions in the language, and if you want any sort of automation, e.g. substituting in sections from somewhere else, or re-usable blocks of title+paragraph, you're left with performing hacky string-replacements on the markdown source. With Twirlite, tags are simply functions, and you can define additional tags yourself to abstract away arbitrary patterns within your markup. 

###[Scalatags](https://github.com/lihaoyi/scalatags) 

is perfect for forming heavily structured markup: since in Scalatags the tags/structure is "raw" while the text is quoted, it facilitates structure-heavy markup at the cost of making text-heavy markup very cumbersome to write. With Twirlite, the situation is reversed: text is left raw, while the tags are quoted (using `@`), lending itself to easily marking-up text-heavy documents. As Twirlite is built on Scalatags, we still get all the other advantages of speed and composability. 

###[Twirl](https://github.com/playframework/twirl) 

is almost what I want for a markup language, as it doesn't suffer from the same problems Scalatags or Markdown does. However, it has a rather noisy syntax for a langauge meant for marking up text: you need to surround blocks of text in curly braces `{...}` to pass them into functions/tags. Furthermore, Twirl by default uses Scala.XML both in syntax and in implementation, resulting in an odd mix of <XML>-tags and @twirl-tags. Twirlite solves the first by using whitespace as a delimiter, and solves the second by using Scalatags to provide the HTML structure, making all tags uniformly @twirl-tags.
 
----------

Twirlite also has some other design decisions which are unique, for better or for worse:

###Twirlite as a Macro

Twirlite is usable as a macro right inside your code; that means if you have a text-heavy section of your Scalatags code, you can simply drop into a `twl(...)` macro and start writing long-form text, while still using `@` to interpolate tags as necessary, or splicing in variables in the enclosing scope.

###Direct Text Generation

Twirlite works as a direct-text-generator rather than as a markup-language that is interpreted to generate text. This has advantages in speed and simplicity-of-abstraction, since creating re-usable tags is as simple as defining a function `f(inner: Frag*): Frag` and making it available in scope. 

However, it makes it more difficult to do certain kinds of whole-program-analysis, since there never really is a syntax-tree available that you can analyze. Twirlite compiles to Scala source code, which when evaluated spits out HTML `String`s, with nothing in between. Furthermore, since Twirlite can run arbitrary Scala code, it makes it much more difficult to sandbox than Markdown or similar languages. 