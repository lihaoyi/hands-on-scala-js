Hands-On Scala.js

	- Some Scala experience
	- Some Javascript/Web experience

	Intro to Scala.js
		What
		Why
		Where

	Tutorial
		A HTML5 Canvas Application
			Accessing DOM APIs
				Using js.Dynamic
				Using scala-js-dom for type-safety

			Input, Output
			Publishing
			Looking through the generated code

		Interactive Web Pages
			Managing HTML using the DOM
			Managing HTML using Scalatags
			Wiring up DOM events and interactions
			Publishing

		Building Cross-platform Libraries
			Shared code organization
			Shared tests using uTest
			Publishing 

		Client-Server Integration
			Hello World client/server project
				Serving the client javascript from the server

			Sharing code
				Using shared libraries
				Writing shared application logic

			Ajax calls
				Using the DOM APIs
				Using uPickle for serialization
				Using Autowire for routing

			Deployment

	Reference
		Javascript Interop
			Calling Javascript from Scala.js
			Writing your own FFI facades
			Calling Scala.js from Javascript
			Mapping of Types
		
		Library Dependencies

		Differences from Scala/JVM

		Compilation Pipeline
			Optimization Phases



Intro to Intro to Scala.js

	Scala.js is a compiler that compiles Scala source code to equivalent Javascript code. That lets you write Scala code that you can run in a web browser, or other environments (Chrome plugins, Node.js, etc.) where Javascript is supported.

	This book is targeted at people who have some experience in both Scala and Javascript. You do not need to be an expert in both, but I will skim over basic concepts in both languages to cut to the Scala.js specific points.

	Scala.js on its own allows you to develop web applications with the safety and toolability that comes with a statically typed language. 
	
	- Typo-safety due to its compiler which catches many silly errors before the code is run
	- In-editor support for autocomplete, error-highlighting, refactors, and intelligent navigation
	- Very small compiled executables, in the 170-400kb range
	- Source-maps for ease of debugging

	In general, the development experience is on