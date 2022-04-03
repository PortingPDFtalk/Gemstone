# Created 3. April 2022 um 18:54:04 by Gemstone Transform(1.4.0.7,chaider)
FileFormat UTF8
IfErr 1 list dictionaries
IfErr 2 stk
IfErr 3 display oops
IfErr 4 omit classoops
IfErr 5 stack
IfErr 6 Exit
DoIt
	| package |
	UserGlobals at: #FileInStartingTimestamp put: DateAndTime now.
	(UserGlobals includesKey: #FileInSymbolDictionary) ifTrue: [
		nil error: 'Previous file-in did not complete'].
	(GsSession currentSession resolveSymbol: #Values) ifNil: [
		package := GsPackageLibrary createPackageNamed: #Values.
		package initialize.
		GsPackageLibrary installPackage: package].
%
DoIt
	UserGlobals at: #FileInSymbolDictionary put: Values.
%
DoIt
	FileInSymbolDictionary at: #codeComponents put: Dictionary new.	"Add root of pundle structure"
	FileInSymbolDictionary at: #namespacePathsAtClasses put: Dictionary new.	"Add registry for namespace paths of classes"
%
DoIt
	| dict components |
	dict := SymbolDictionary new.
	dict name: #Values.
	dict at: #comment put: 'Values are simple immutable literal objects.

Runtime support for Values.

Defines the root class Value and provides
- printing instances
- equality
- an example.

Specifies the responsibilities of subclasses to define the #localSpecification'.
	dict at: #notice put: 'The MIT License

Copyright © 2009-2018 Christian Haider

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.'.
	dict at: #padded put: true.
	dict at: #parcelName put: 'Values'.
	dict at: #storeVersion put: '3.0.1.0'.
	components := (GsPackageLibrary packageNamed: #Values) symbolDict at: #codeComponents.
	components at: dict name put: dict.
%

# VisualWorks stub classes

DoIt
DateAndTime
	subclass: 'Timestamp'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
Timestamp category: 'Values VW stub class'.
Timestamp comment: 'VisualWorks class'.
%
DoIt
Color
	subclass: 'ColorValue'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
ColorValue category: 'Values VW stub class'.
ColorValue comment: 'VisualWorks class'.
%
# Define class Value
DoIt
Object
	subclass: 'Value'
	instVarNames: #()
	classVars: #(NamedValuesRegistry)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Value category: 'Values'.
	Value comment: 'Value is the abstract root object of values (literal objects).
All Value classes which have subclasses are considered abstract. Only leaf classes can have instances!
Literal objects are created immutable with constructors.
The instance variables are object constants.

Subclasses must implement the following messages:
	class specification
		localSpecification'.
%
# Define class Printvalue
DoIt
Value
	subclass: 'Printvalue'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Printvalue category: 'Values'.
	Printvalue comment: 'A Printvalue holds a representation of a Value to be printed.
It consists of
	- classname 	<String>										properly resolved against a target class (#name or #fullName)
	- arguments 	<SequenceableCollection of: Printargument>		the list of arguments with constructor variable name and a Printvalue

Printvalues are created when printing the source for a Value in the first pass. The second pass takes the Printvalue and produces a nicely indented soure string for the value.'.
%
# Define class ArrayPrintvalue
DoIt
Printvalue
	subclass: 'ArrayPrintvalue'
	instVarNames: #(arguments)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	ArrayPrintvalue category: 'Values'.
	ArrayPrintvalue comment: 'ArrayPrintvalue is a special Printvalue for sequentiable collections of Values'.
%
# Define class Emitter
DoIt
Object
	subclass: 'Emitter'
	instVarNames: #(printer stream level)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Emitter category: 'Values'.
	Emitter comment: 'Emitter outputs a Printvalue as formatted source code to a stream.
Subclasses write source or text.
This implements the second pass of the source generation for Values.

Instance Variables
	printer	<ValuePrinter>					the printer to resolve namespace names
	level	<Integer>						the indention level
	stream	<WriteStream | TextStream>		the output stream
'.
%
# Define class SourceEmitter
DoIt
Emitter
	subclass: 'SourceEmitter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	SourceEmitter category: 'Values'.
	SourceEmitter comment: 'SourceEmitter outputs source code.'.
%
# Define class Blockemitter
DoIt
SourceEmitter
	subclass: 'Blockemitter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Blockemitter category: 'Values'.
	Blockemitter comment: 'Emits source as indented block'.
%
# Define class TextEmitter
DoIt
Emitter
	subclass: 'TextEmitter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	TextEmitter category: 'Values'.
	TextEmitter comment: 'TextEmitter outputs a descriptive text.'.
%
# Define class TextLineemitter
DoIt
TextEmitter
	subclass: 'TextLineemitter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	TextLineemitter category: 'Values'.
	TextLineemitter comment: 'TextLineEmitter emits text as one line'.
%
# Define class Printargument
DoIt
Value
	subclass: 'Printargument'
	instVarNames: #(name value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Printargument category: 'Values'.
	Printargument comment: 'A Printargument holds the data to print one instance variable (constant) and its value.

	- name 	<String> 		the name of the argument
	- value 	<Printvalue>	the print value of the value

With Printvalue it represents the source for a Value'.
%
# Define class DictionaryPrintargument
DoIt
Printargument
	subclass: 'DictionaryPrintargument'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	DictionaryPrintargument category: 'Values'.
	DictionaryPrintargument comment: 'DictionaryPrintargument holds the Printvalues for key and value of an entry in a dictionary.
Both key and value can be Values'.
%
# Define class Valuemap
DoIt
Dictionary
	indexableSubclass: 'Valuemap'
	instVarNames: #(order)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Valuemap category: 'Values'.
	Valuemap comment: 'A Valuemap is an ordered dictionary preserving the order in which entries are added.

Instances are also used as ordered volatile dictionary, not just as value.

In VisualWorks, Valuemap is subclassed from Dictionary to inherit the dictionary API.

Instance Variables:
	order	<SequenceableCollection of: Object>		the ordered keys


The class used to be named OrderedDictionary, but was renamed for dialect compatibility with Pharo, which has a class named OrderedDictionary with different semantics.'.
%
# Define class LiteralPrintvalue
DoIt
Printvalue
	subclass: 'LiteralPrintvalue'
	instVarNames: #(string)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	LiteralPrintvalue category: 'Values'.
	LiteralPrintvalue comment: 'LiteralPrintvalue is a Printvalue for literals.

Instance Variables:
	string	<String>	the representation as a string'.
%
# Define class DictionaryPrintvalue
DoIt
Printvalue
	subclass: 'DictionaryPrintvalue'
	instVarNames: #(arguments isOrdered)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	DictionaryPrintvalue category: 'Values'.
	DictionaryPrintvalue comment: 'DictionaryPrintvalue is used for printing Dictionaries

Instance Variables:
	arguments	<SequenceableCollection of DictionaryPrintargument>	the Printarguments for each entry'.
%
# Define class ValuePrinter
DoIt
Object
	subclass: 'ValuePrinter'
	instVarNames: #(target)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	ValuePrinter category: 'Values'.
	ValuePrinter comment: 'ValuePrinter creates the source for a Value.
For each level, a new ValuePrinter is created.

Instance Variables:
	target	<Value class>	defines the scoping for the created source - how class names are printed
	level	<Integer>		the indent level
	stream	<WriteStream>	a writestream passed around to collect the sources

'.
%
# Define class ValuePrintvalue
DoIt
Printvalue
	subclass: 'ValuePrintvalue'
	instVarNames: #(classname arguments)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	ValuePrintvalue category: 'Values'.
	ValuePrintvalue comment: 'ValuePrintvalue holds the data to print a Value.

Instance Variables:
	arguments	<SequenceableCollection of: (Printargument | EmptyArgument)>	the arguments
	classname	<String>														the namespace aware name of the class

'.
%
# Define class TextBlockemitter
DoIt
TextEmitter
	subclass: 'TextBlockemitter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	TextBlockemitter category: 'Values'.
	TextBlockemitter comment: 'TextBlockEmitter emits text as indented block'.
%
# Define class Lineemitter
DoIt
SourceEmitter
	subclass: 'Lineemitter'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values
%
DoIt
	Lineemitter category: 'Values'.
	Lineemitter comment: 'Emits source as one line'.
%
category: 'instance creation'
classmethod: ArrayPrintvalue
arguments: argumentsArray
	| inst |
	inst := self new.
	inst initializeArguments: argumentsArray.
	^inst
%
category: 'specification'
classmethod: ArrayPrintvalue
localSpecification
	<sequence: #arguments>
%
category: 'accessing'
method: ArrayPrintvalue
arguments
	"<Array>"

	^arguments ifNil: [#()]
%
method: ArrayPrintvalue
sourceWith: emitter
	emitter emitArraySourceFor: self arguments
%
category: 'initialize-release'
method: ArrayPrintvalue
initializeArguments: argumentsArray
	(argumentsArray notNil and: [
	argumentsArray notEmpty]) ifTrue: [
			arguments := (Array withAll: argumentsArray) beImmutable].
	self beImmutable
%
category: 'printing'
method: ArrayPrintvalue
printvalueWith: printer
	| args |
	args := OrderedCollection new.
	args add: (printer array: 'arguments' value: self arguments).
	^printer printvalue: self arguments: args
%
category: 'testing'
method: ArrayPrintvalue
isLine
	^self arguments size = 1 or: [
	self arguments size < 5 and: [
	self arguments allSatisfy: [:arg | arg isSimple]]]
%
category: 'initialize-release'
method: Blockemitter
initializePrinter: aPrinter stream: aWriteStream level: anInteger
	printer := aPrinter.
	stream := aWriteStream.
	level := anInteger + 1
%
category: 'writing'
method: Blockemitter
emitSeparator
	self stream cr.
	self level + 1 timesRepeat: [self stream tab]
%
category: 'specification'
classmethod: DictionaryPrintargument
localSpecification
	<constant: #name class: #(#Printvalue)>
	<constant: #value class: #(#Printvalue)>
%
category: 'instance creation'
classmethod: DictionaryPrintvalue
arguments: argumentsArray isOrdered: isOrderedBoolean
	| inst |
	inst := self new.
	inst initializeArguments: argumentsArray isOrdered: isOrderedBoolean.
	^inst
%
category: 'instance creation optional'
classmethod: DictionaryPrintvalue
arguments: argumentsArray
	^self arguments: argumentsArray isOrdered: nil
%
classmethod: DictionaryPrintvalue
isOrdered: isOrderedBoolean
	^self arguments: nil isOrdered: isOrderedBoolean
%
category: 'specification'
classmethod: DictionaryPrintvalue
localSpecification
	<sequence: #arguments>
	<optional: #isOrdered class: #(#Boolean) default: 'false'>
%
category: 'accessing'
method: DictionaryPrintvalue
arguments
	"<Array>"

	^arguments ifNil: [#()]
%
method: DictionaryPrintvalue
isOrdered
	"<Boolean>"

	^isOrdered ifNil: [false]
%
method: DictionaryPrintvalue
sourceWith: emitter
	self isEmpty ifTrue: [
		^emitter emit: self targetClassname , ' new'].
	emitter emitDictionarySourceFor: self
%
method: DictionaryPrintvalue
targetClass
	^self isOrdered
		ifTrue: [Valuemap]
		ifFalse: [Dictionary]
%
method: DictionaryPrintvalue
targetClassname
	^self targetClass name asString
%
category: 'initialize-release'
method: DictionaryPrintvalue
initializeArguments: argumentsArray isOrdered: isOrderedBoolean
	(argumentsArray notNil and: [
	argumentsArray notEmpty]) ifTrue: [
			arguments := (Array withAll: argumentsArray) beImmutable].
	(isOrderedBoolean notNil and: [
	self isOrdered ~= isOrderedBoolean]) ifTrue: [
			isOrdered := isOrderedBoolean].
	self beImmutable
%
category: 'printing'
method: DictionaryPrintvalue
printvalueWith: printer
	| args |
	args := OrderedCollection new.
	args add: (printer array: 'arguments' value: self arguments).
	args add: (printer optional: 'isOrdered' value: isOrdered).
	^printer printvalue: self arguments: args
%
category: 'testing'
method: DictionaryPrintvalue
isEmpty
	^self arguments isEmpty
%
method: DictionaryPrintvalue
isLine
	^self isEmpty
%
category: 'instance creation'
classmethod: Emitter
for: aPrinter
	| inst |
	inst := self new.
	inst initializePrinter: aPrinter stream: nil level: -1.
	^inst
%
classmethod: Emitter
for: aPrinter on: aWriteStream at: anInteger
	| inst |
	inst := self new.
	inst initializePrinter: aPrinter stream: aWriteStream level: anInteger.
	^inst
%
category: 'accessing'
method: Emitter
emitterClassFor: printvalue
	"<Emitter class>"

	^self subclassResponsibility
%
method: Emitter
emitterFor: printvalue
	"<Emitter>"

	^(self emitterClassFor: printvalue) for: self printer on: self stream at: self level
%
method: Emitter
level
	"<Integer>
	the indent level"

	^level ifNil: [0]
%
method: Emitter
printer
	"<ValuePrinter>"

	^printer
%
method: Emitter
stream
	"<WriteStream | TextStream>
	the output stream"

	^stream ifNil: [stream := self streamClass on: (String new: 100)]
%
method: Emitter
streamClass
	"<WriteStream class | TextStream class>"

	^self subclassResponsibility
%
category: 'initialize-release'
method: Emitter
initializePrinter: aPrinter stream: aWriteStream level: anInteger
	printer := aPrinter.
	stream := aWriteStream.
	level := anInteger
%
category: 'writing'
method: Emitter
emit: aString
	self stream nextPutAll: aString
%
method: Emitter
emitArgument: aPrintargument
	self emit: aPrintargument name , ': '.
	aPrintargument value sourceFor: self
%
method: Emitter
emitArraySourceFor: anArray
	self inBracketsDo: [
		anArray size > 4
			ifTrue: [self emitLongCollection: anArray]
			ifFalse: [
			self emitSmallCollection: anArray]]
%
method: Emitter
emitClass: aString
	self emit: aString
%
method: Emitter
emitDictionarySourceFor: aDictionaryPrintvalue
	self inBracketsDo: [
		aDictionaryPrintvalue arguments size > 4
			ifTrue: [
			self emitLongDictionary: aDictionaryPrintvalue]
			ifFalse: [
			self emitSmallDictionary: aDictionaryPrintvalue]]
%
method: Emitter
emitEmpty: aValuePrintvalue
	"write the constructor for an empty instance"

	self emitClass: aValuePrintvalue classname.
	self emit: ' new'
%
method: Emitter
emitLongCollection: aSequenceableCollection
	self emit: '(', (OrderedCollection nameRelativeTo: self printer target) , ' new: ' , aSequenceableCollection size printString, ')'.
	aSequenceableCollection do: [:value |
		self emitSeparator.
		self emit: 'add: '.
		value sourceFor: self.
		self stream nextPut: $;].
	self emitSeparator.
	self emit: 'yourself'
%
method: Emitter
emitLongDictionary: aDictionaryPrintvalue
	self emit: '(', (aDictionaryPrintvalue targetClass nameRelativeTo: self printer target), ' new: ' , aDictionaryPrintvalue arguments size printString, ')'.
	aDictionaryPrintvalue arguments do: [:argument |
		self emitSeparator.
		self emit: 'add: '.
		argument name sourceFor: self.
		self emit: ' -> '.
		argument value sourceFor: self.
		self stream nextPut: $;].
	self emitSeparator.
	self emit: 'yourself'
%
method: Emitter
emitSeparator
	self subclassResponsibility
%
method: Emitter
emitSmallCollection: aSequenceableCollection
	self emit: (Array nameRelativeTo: self printer target).
	aSequenceableCollection do: [:value |
		self emitSeparator.
		self emit: 'with: '.
		value sourceFor: self]
%
method: Emitter
emitSmallDictionary: aDictionaryPrintvalue
	self emit: (aDictionaryPrintvalue targetClass nameRelativeTo: self printer target).
	aDictionaryPrintvalue arguments do: [:argument |
		self emitSeparator.
		self emit: 'with: '.
		argument name sourceFor: self.
		self emit: ' -> '.
		argument value sourceFor: self]
%
method: Emitter
emitValue: aString
	self emit: aString
%
method: Emitter
inBracketsDo: aZeroArgumentBlock
	self stream nextPut: $(.
	aZeroArgumentBlock ensure: [self stream nextPut: $)]
%
category: 'writing'
method: Lineemitter
emitSeparator
	self stream space
%
category: 'instance creation'
classmethod: LiteralPrintvalue
string: stringString
	| inst |
	inst := self new.
	inst initializeString: stringString.
	^inst
%
category: 'specification'
classmethod: LiteralPrintvalue
localSpecification
	<constant: #string class: #(#String)>
%
category: 'accessing'
method: LiteralPrintvalue
sourceWith: emitter
	emitter emitValue: self string
%
method: LiteralPrintvalue
string
	"<String>"

	^string
%
category: 'initialize-release'
method: LiteralPrintvalue
initializeString: stringString
	string := stringString.
	self beImmutable
%
category: 'printing'
method: LiteralPrintvalue
printvalueWith: printer
	| args |
	args := OrderedCollection new.
	args add: (printer constant: 'string' value: self string).
	^printer printvalue: self arguments: args
%
category: 'testing'
method: LiteralPrintvalue
isLine
	^true
%
method: LiteralPrintvalue
isSimple
	^(self string first = $() not
%
category: 'instance creation'
classmethod: Printargument
name: nameString value: valuePrintvalue
	| inst |
	inst := self new.
	inst initializeName: nameString value: valuePrintvalue.
	^inst
%
category: 'specification'
classmethod: Printargument
localSpecification
	<constant: #name class: #(#String)>
	<constant: #value class: #(#Printvalue)>
%
category: 'accessing'
method: Printargument
name
	"<String>"

	^name
%
method: Printargument
sourceWith: emitter
	emitter emitArgument: self
%
method: Printargument
value
	"<Printvalue>"

	^value
%
category: 'initialize-release'
method: Printargument
initializeName: nameString value: valuePrintvalue
	name := nameString.
	value := valuePrintvalue.
	self beImmutable
%
category: 'printing'
method: Printargument
printvalueWith: printer
	| args |
	args := OrderedCollection new.
	args add: (printer constant: 'name' value: self name).
	args add: (printer constant: 'value' value: self value).
	^printer printvalue: self arguments: args
%
category: 'testing'
method: Printargument
isSimple
	^self value isSimple
%
category: 'accessing'
method: Printvalue
description
	"<Text>"

	| emitter |
	emitter := self textemitterClass new.
	self sourceWith: emitter.
	^emitter stream contents
%
method: Printvalue
emitterClass
	"<SourceEmitter class>"

	^self isLine
		ifTrue: [Lineemitter]
		ifFalse: [Blockemitter]
%
method: Printvalue
sourceFor: outerEmitter
	"<String>"

	^self sourceWith: (outerEmitter emitterFor: self)
%
method: Printvalue
sourceWith: emitter
	"write yourself with the emitter"

	^self subclassResponsibility
%
method: Printvalue
sourceWithPrinter: aPrinter
	"<String>"

	| emitter |
	emitter := self emitterClass for: aPrinter.
	self sourceWith: emitter.
	^emitter stream contents
%
method: Printvalue
textemitterClass
	"<SourceEmitter class>"

	^self isLine
		ifTrue: [TextLineemitter]
		ifFalse: [TextBlockemitter]
%
category: 'testing'
method: Printvalue
isLine
	^false
%
method: Printvalue
isSimple
	"does it have to be printed in brackets?
	only literals are simple"

	^false
%
category: 'accessing'
method: SourceEmitter
emitterClassFor: printvalue
	^printvalue isLine
		ifTrue: [Lineemitter]
		ifFalse: [Blockemitter]
%
method: SourceEmitter
streamClass
	^WriteStream
%
category: 'initialize-release'
method: TextBlockemitter
initializePrinter: aPrinter stream: aWriteStream level: anInteger
	printer := aPrinter.
	stream := aWriteStream.
	level := anInteger + 1
%
category: 'writing'
method: TextBlockemitter
emitSeparator
	self stream cr.
	self level + 1 timesRepeat: [self stream tab]
%
method: TextBlockemitter
inBracketsDo: aZeroArgumentBlock
	aZeroArgumentBlock value
%
category: 'accessing'
method: TextEmitter
emitterClassFor: printvalue
	^printvalue isLine
		ifTrue: [TextLineemitter]
		ifFalse: [TextBlockemitter]
%
method: TextEmitter
streamClass
	^TextStream
%
category: 'writing'
method: TextEmitter
emitArraySourceFor: anArray
	self emitSmallCollection: anArray
%
method: TextEmitter
emitBold: aString
	self stream withAttributes: (Array with: TextEmphasis bold) do: [
		self stream nextPutAll: aString]
%
method: TextEmitter
emitClass: aString
	self emitBold: aString
%
method: TextEmitter
emitDictionarySourceFor: aDictionaryPrintvalue
	self emitSmallDictionary: aDictionaryPrintvalue
%
method: TextEmitter
emitSmallCollection: aSequenceableCollection
	aSequenceableCollection do: [:value |
		self emitSeparator.
		value sourceFor: self]
%
method: TextEmitter
emitSmallDictionary: aDictionaryPrintvalue
	aDictionaryPrintvalue arguments do: [:argument |
		self emitSeparator.
		argument name sourceFor: self.
		self emit: ': '.
		argument value sourceFor: self]
%
method: TextEmitter
emitValue: aString
	self emitBold: aString
%
category: 'writing'
method: TextLineemitter
emitSeparator
	self stream space
%
category: 'class initialization'
classmethod: Value
obsolete
	self allInstancesDo: #beMutable.
	super obsolete
%
category: 'initialize-release'
classmethod: Value
primeRuntime
	"reset and load all class variables to fill the caches to avoid lazy initialization at runtime.
	This should be executed when deploying after all application fonts and extensions are loaded"
	"self primeRuntime"

	self resetNamedValuesRegistry.
	Object namesByValues
%
classmethod: Value
resetNamedValuesRegistry
	"self resetNamedValuesRegistry"

	NamedValuesRegistry := nil
%
category: 'named values'
classmethod: Value
namedValuesAt: aClass
	"<Dictionary key: Value value: Symbol>
	the named instances of aClass"

	^self namedValuesRegistry at: aClass ifAbsent: [Dictionary new]
%
classmethod: Value
namedValuesAt: aClass ifAbsentPut: aBlock
	"<Dictionary key: Value value: Symbol>
	the named instances of aClass"

	^self namedValuesRegistry at: aClass ifAbsentPut: aBlock
%
classmethod: Value
namedValuesRegistry
	"<Dictionary key: Class value: (Dictionary key: Value value: Symbol)>
	the named instances of classes.
	This is a light weight implementation to hold all named instances of all classes, instead of storing them with the class"
	
	^NamedValuesRegistry ifNil: [NamedValuesRegistry := Dictionary new]
%
category: 'pragmas'
classmethod: Value
specificationPragmas
	<pragmas: #class>
	^#(
		#constant:class:
		#constant:class:comment:
		
		#optional:class:default:
		#optional:class:default:comment:
		
		#sequence:
		#sequence:comment:
		
		#map:
		#map:comment:
	)
%
category: 'specification'
classmethod: Value
localSpecification
	"specification of the constants with pragmas.
	Only for the instvars defined in this class - access the full specification with #specification"

	^self subclassResponsibility
%
category: 'comparing'
method: Value
= anObject
	"all instvars must be equal"

	^self isEqualValue: anObject
%
method: Value
hash
	^(1 to: self class instSize) inject: 0 into: [:hash :i | hash bitXor: (self instVarAt: i) hash]
%
method: Value
isEqualValue: anObject
	self == anObject ifTrue: [
		^true].
	self class = anObject class ifFalse: [
		^false].
	1 to: self class instSize do: [:i |
		((self instVarAt: i) isEqualValue: (anObject instVarAt: i)) ifFalse: [
			^false]].
	^true
%
category: 'copying'
method: Value
postCopy
	super postCopy.
	self beImmutable
%
category: 'marshaling'
method: Value
passMode
	"for OpenTalk StSt (Smalltalk to Smalltalk)"

	^#value
%
category: 'printing'
method: Value
printOn: stream
	stream nextPutAll: self asSource
%
method: Value
printvalueWith: printer
	^printer printvalue: self arguments: #()
%
category: 'testing'
method: Value
isEmpty
	^self = self class new
%
method: Value
notEmpty
	^self isEmpty not
%
category: 'accessing'
method: Valuemap
atIndex: index
	"<Association>"

	^self associationAt: (self order at: index)
%
method: Valuemap
first
	"<Association>"

	^self associationAt: self keys first
%
method: Valuemap
indexOf: aKey
	"<Integer>"

	^self order indexOf: aKey
%
method: Valuemap
keys
	"<SequenceableCollection of: Object>"

	^self order copy
%
method: Valuemap
last
	"<Association>"

	^self associationAt: self keys last
%
method: Valuemap
order
	"<SequenceableCollection of: Object>
	the order of the keys"

	^order ifNil: [order := OrderedCollection new]
%
category: 'comparing'
method: Valuemap
= otherOrderedDictionary
	^self class == otherOrderedDictionary class and: [
	self size = otherOrderedDictionary size and: [
	(1 to: self size) allSatisfy: [:i |
		(self atIndex: i) = (otherOrderedDictionary atIndex: i)]]]
%
method: Valuemap
hash
	^(1 to: self size) inject: self class hash into: [:hash :index |
		| assoc |
		assoc := self atIndex: index.
		hash bitXor: (assoc key hash bitXor: assoc value hash)]
%
category: 'copying'
method: Valuemap
copyWith: anAssociation 
	"Answer a copy of the receiver with anAssociation added"

	| copy |
	copy := self copy.
	copy add: anAssociation.
	^copy
%
method: Valuemap
copyWithAll: aDictionary
	"Answer a copy of the receiver with all associations from aDictionary added"

	| copy |
	copy := self copy.
	copy addAll: aDictionary.
	^copy
%
method: Valuemap
postCopy

	super postCopy.
	order := self order copy
%
category: 'dictionary enumerating'
method: Valuemap
associationsDo: aBlock
	"Note: do not use the keys to access anything.
	This method is used by #changeCapacityTo: and #rehash where key access is not working"

	| assocs |
	assocs := Array new: self order size.
	super associationsDo: [:assoc | assocs at: (self order indexOf: assoc key) put: assoc].
	assocs do: aBlock
%
method: Valuemap
removeKey: aKey otherwise: defaultValue
	| return |
	return := super removeKey: aKey otherwise: defaultValue.
	self order remove: aKey ifAbsent: nil.
	^return
%
method: Valuemap
removeAssociation: anAssociation otherwise: defaultValue
	| return |
	return := super removeAssociation: anAssociation otherwise: defaultValue.
	self order remove: anAssociation key ifAbsent: nil.
	^return
%
category: 'enumerating'
method: Valuemap
collect: oneArgumentBlock
	| newCollection |
	newCollection := self species new: self size.
	self keysAndValuesDo: [:eachKey :eachValue | 
		newCollection at: eachKey put: (oneArgumentBlock value: eachValue)].
	^newCollection
%
method: Valuemap
do: oneArgumentBlock
	self order do: [:key |
		oneArgumentBlock value: (self at: key)]
%
method: Valuemap
keysAndValuesDo: oneArgumentBlock
	self order do: [:key |
		oneArgumentBlock value: key value: (self at: key)]
%
method: Valuemap
keysDo: aBlock
	self keys do: aBlock
%
method: Valuemap
valuesDo: aBlock
	self keysDo: [:key | aBlock value: (self at: key)]
%
category: 'printing'
method: Valuemap
keysForPrinting
	^self keys
%
method: Valuemap
printvalueWith: printer
	^DictionaryPrintvalue arguments: (self printargumentsWith: printer) isOrdered: true
%
category: 'private'
method: Valuemap
addNewAssociation: anAssociation hash: aHashVal
	self order addLast: anAssociation key.
	^super addNewAssociation: anAssociation hash: aHashVal
%
method: Valuemap
rebuild
	| original return |
	original := self order copy.
	return := super rebuild.
	order := original.
	^return
%
category: 'instance creation'
classmethod: ValuePrinter
newFor: targetClass
	| inst |
	inst := self new.
	inst initializeTarget: targetClass.
	^inst
%
category: 'accessing'
method: ValuePrinter
array: selectorname value: object
	"<Printargument | nil>"

	object isEmpty ifTrue: [
		^nil].
	^Printargument name: selectorname value: (object printvalueWith: self)
%
method: ValuePrinter
classnameOf: value
	^value class nameRelativeTo: self target
%
method: ValuePrinter
constant: selectorname value: object
	"<Printargument>"

	^Printargument name: selectorname value: (object printvalueWith: self)
%
method: ValuePrinter
dictionary: selectorname value: object
	"<Printargument | nil>"

	object isEmpty ifTrue: [
		^nil].
	^Printargument name: selectorname value: (object printvalueWith: self)
%
method: ValuePrinter
optional: selectorname value: object
	"<Printargument | nil>"

	^object ifNotNil: [
		Printargument name: selectorname value: (object printvalueWith: self)]
%
method: ValuePrinter
printvalue: aValue arguments: arguments
	aValue class new = aValue ifTrue: [
		^ValuePrintvalue classname: (self classnameOf: aValue)].
	^ValuePrintvalue
		classname: (self classnameOf: aValue)
		arguments: (arguments select: [:arg |
			arg notNil])
%
method: ValuePrinter
target
	"<Value class>
	the class for which the object is printed.
	Takes the visibility of classes into account for short class name generation"

	^target
%
category: 'initialize-release'
method: ValuePrinter
initializeTarget: targetClass
	target := targetClass
%
category: 'instance creation'
classmethod: ValuePrintvalue
classname: classnameString arguments: argumentsArray
	| inst |
	inst := self new.
	inst initializeClassname: classnameString arguments: argumentsArray.
	^inst
%
category: 'instance creation optional'
classmethod: ValuePrintvalue
classname: classnameString
	^self classname: classnameString arguments: nil
%
category: 'specification'
classmethod: ValuePrintvalue
localSpecification
	<constant: #classname class: #(#String)>
	<sequence: #arguments>
%
category: 'accessing'
method: ValuePrintvalue
arguments
	"<Array>"

	^arguments ifNil: [#()]
%
method: ValuePrintvalue
classname
	"<String>"

	^classname
%
method: ValuePrintvalue
sourceWith: emitter
	self arguments isEmpty ifTrue: [
		^emitter emitEmpty: self].
	emitter inBracketsDo: [
		emitter emitClass: self classname.
		self arguments do: [:arg |
			emitter emitSeparator.
			emitter emitArgument: arg]]
%
category: 'initialize-release'
method: ValuePrintvalue
initializeClassname: classnameString arguments: argumentsArray
	classname := classnameString.
	(argumentsArray notNil and: [
	argumentsArray notEmpty]) ifTrue: [
			arguments := (Array withAll: argumentsArray) beImmutable].
	self beImmutable
%
category: 'printing'
method: ValuePrintvalue
printvalueWith: printer
	| args |
	args := OrderedCollection new.
	args add: (printer constant: 'classname' value: self classname).
	args add: (printer array: 'arguments' value: self arguments).
	^printer printvalue: self arguments: args
%
category: 'testing'
method: ValuePrintvalue
isLine
	self arguments size = 1 ifTrue: [
		^self arguments first value isLine].
	^self arguments size < 5 and: [
	self arguments allSatisfy: [:arg | arg isSimple]]
%
category: '*Values-instance creation'
classmethod: ColorValue
fromByte: anInteger
	^self fromBytes: (ByteArray new: 3 withAll: anInteger)
%
classmethod: ColorValue
fromBytes: threeBytes
	^self fromBytesRed: threeBytes first green: (threeBytes at: 2) blue: threeBytes last
%
classmethod: ColorValue
fromBytesRed: redByte green: greenByte blue: blueByte
	^self red: redByte / 255 green: greenByte / 255 blue: blueByte / 255
%
classmethod: ColorValue
fromColor: aColor
	^self r: aColor red g: aColor green b: aColor blue
%
classmethod: ColorValue
red: r green: g blue: b
	^self r: r g: g b: b
%
category: '*Values-named values'
classmethod: ColorValue
namedValueNames
	"The selection is the common set of color names with identical colors in VW and Squeak/Pharo
	The colors are all variations of min and max of the 3 components"
	"self namesByValues"
	
	^#(#black #white
	#red #green #blue
	#cyan #magenta #yellow)
%
classmethod: ColorValue
new
	^self basicNew
%
classmethod: ColorValue
black
	^super black asColorValue
%
classmethod: ColorValue
white
	^super white asColorValue
%
classmethod: ColorValue
red
	^super red asColorValue
%
classmethod: ColorValue
green
	^super green asColorValue
%
classmethod: ColorValue
blue
	^super blue asColorValue
%
classmethod: ColorValue
cyan
	^super cyan asColorValue
%
classmethod: ColorValue
magenta
	^super magenta asColorValue
%
classmethod: ColorValue
yellow
	^super yellow asColorValue
%
category: '*Values-converting'
method: ColorValue
asByteArray
	^ByteArray
		with: (self red * 255) truncated
		with: (self green * 255) truncated
		with: (self blue * 255) truncated
%
method: ColorValue
asGrayValue
	"<Number>
	between 0.0 (Black) and 1.0 (White) suitable for PDF DeviceGray"

	^self brightness
%
method: ColorValue
rgbIndex
	"<Integer>
	Three 8 bit RGB numbers interpreted as number for ordering"

	^self asByteArray inject: 0 into: [:num :byte | (num bitShift: 8) + byte]
%
method: ColorValue
asColorValue
	^self
%
category: '*Values-printing'
method: ColorValue
printvalueWith: printer
	| args |
	(self class nameOrNilFor: self) ifNotNil: [:symbol |
		^LiteralPrintvalue string: (printer classnameOf: self) , ' ' , symbol asString].
	args := OrderedCollection new.
	args add: (printer constant: 'fromBytesRed' value: (self red * 255) rounded).
	args add: (printer constant: 'green' value: (self green * 255) rounded).
	args add: (printer constant: 'blue' value: (self blue * 255) rounded).
	^printer printvalue: self arguments: args
%
method: ColorValue
printOn: aStream
	aStream nextPutAll: self asSource
%
category: '*Values-testing'
method: ColorValue
isEmpty
	^self privateRGB isNil
%
category: '*Values-accessing'
method: ColorValue
cyan
	"<Number[0..1]>
	the cyan part of the receiver"

	^1.0 - self red
%
method: ColorValue
magenta
	"<Number[0..1]>
	the magenta part of the receiver"

	^1.0 - self green
%
method: ColorValue
yellow
	"<Number[0..1]>
	the yellow part of the receiver"

	^1.0 - self blue
%
category: '*Values-instance creation'
classmethod: Timestamp
d: dayInteger m: monthInteger y: yearInteger
	^self d: dayInteger m: monthInteger y: yearInteger h: 0 m: 0 s: 0
%
classmethod: Timestamp
d: dayInteger m: monthInteger y: yearInteger h: hoursInteger
	^self d: dayInteger m: monthInteger y: yearInteger h: hoursInteger m: 0 s: 0
%
classmethod: Timestamp
d: dayInteger m: monthInteger y: yearInteger h: hoursInteger m: minutesInteger
	^self d: dayInteger m: monthInteger y: yearInteger h: hoursInteger m: minutesInteger s: 0
%
classmethod: Timestamp
d: dayInteger m: monthInteger y: yearInteger h: hoursInteger m: minutesInteger s: secondsInteger
	^self
		year: yearInteger
		month: monthInteger
		day: dayInteger
		hour: hoursInteger
		minute: minutesInteger
		second: secondsInteger
		offset: Duration zero
%
classmethod: Timestamp
fromDate: aDate andTime: aTime
	^self date: aDate time: aTime offset: Duration zero
%
classmethod: Timestamp
epoch
	^self d: 1 m: 1 y: 1901
%
category: '*Values-printing'
method: Timestamp
printvalueWith: printer
	| wst |
	wst := WriteStream on: (String new: 50).
	wst
		nextPut: $(;
		nextPutAll: (printer classnameOf: self);
		nextPutAll: ' d: ';
		nextPutAll: self dayOfMonth printString;
		nextPutAll: ' m: ';
		nextPutAll: self month printString;
		nextPutAll: ' y: ';
		nextPutAll: self year printString.
	(self hour isZero and: [
	self minute isZero and: [
	self second isZero]]) ifTrue: [
			wst nextPut: $).
			^LiteralPrintvalue string: wst contents].
	wst
		nextPutAll: ' h: ';
		nextPutAll: self hour printString.
	(self minute isZero and: [
	self second isZero]) ifTrue: [
			wst nextPut: $).
			^LiteralPrintvalue string: wst contents].
	wst
		nextPutAll: ' m: ';
		nextPutAll: self minute printString.
	self second isZero ifTrue: [
		wst nextPut: $).
		^LiteralPrintvalue string: wst contents].
	wst
		nextPutAll: ' s: ';
		nextPutAll: self second printString;
		nextPut: $).
	^LiteralPrintvalue string: wst contents
%
DoIt
	| package policy |
	package := GsPackageLibrary createPackageNamed: #SessionMethods.
	GsPackageLibrary installPackage: package.
	policy := GsPackagePolicy current.
	policy homeSymbolDict: FileInSymbolDictionary.
	policy externalSymbolList: (GsSession currentSession symbolList asArray copyWithout: FileInSymbolDictionary).
	policy enable.
%
category: '*Values-printing'
method: Symbol
literalString
	self asString isValidIdentifier ifTrue: [
		^'#' , self asString].
	^super literalString
%
category: '*Values-testing'
method: Array
isLiteral
	^(self isMemberOf: Array) and: [
	self allSatisfy: #isLiteral]
%
category: '*Values-printing'
method: Array
literalString
	| wst |
	wst := String new writeStream.
	wst nextPutAll: '#('.
	self do: [:elem | wst nextPutAll: elem asSource] separatedBy: [wst space].
	wst nextPut: $).
	^wst contents
%
category: '*Values-durations'
method: Number
nanoseconds
	^Duration nanoSeconds: self
%
category: '*Values-printing'
method: Boolean
literalString
	^self printString
%
category: '*Values-printing'
method: CharacterCollection
literalString
	^self printString
%
category: '*Values-printing'
method: SmallInteger
literalString
	^self printString
%
category: '*Values-printing'
method: LargeInteger
literalString
	^self printString
%
category: '*Values-printing'
method: Character
literalString
	^self printString
%
category: '*Values-printing'
method: UndefinedObject
literalString
	^self printString
%
category: '*Values-printing'
method: BinaryFloat
literalString
	^self asStringLocaleC
%
category: '*Values-printing'
method: DecimalFloat
literalString
	^self asStringLocaleC
%
category: '*Values-printing'
method: FixedPoint
literalString
	^self asStringLocaleC
%
category: '*Values-printing'
method: ScaledDecimal
literalString
	^self asStringLocaleC
%
category: '*Values-converting'
method: Color
asColorValue
	^ColorValue fromColor: self
%
category: '*Values-accessing'
method: Class
nameRelativeTo: targetClass
	"<String>"
	"self nameRelativeTo: Object"

	(targetClass isInScope: self) ifTrue: [
		^self name asString].
	^self nameAccessExpression: self namespacePath
%
method: Class
definitionDictionary
	"<Dictionary | nil>
	symbol dictionary in which the receiver is defined or nil if the receiver is in the scope of the ProfileUsers symbolDict"

	| symlist |
	self namespacePath isEmpty ifTrue: [
		^nil].
	symlist := GsSession currentSession.
	self namespacePath do: [:symbol |
		symlist := (symlist resolveSymbol: symbol)
			ifNotNil: [:assoc |
			SymbolList with: assoc value]
			ifNil: [
			^nil]].
	^symlist
%
method: Class
nameAccessExpression: aPath
	"<String>"

	aPath isEmpty ifTrue: [
		^self name asString].
	aPath size = 1 ifTrue: [
		^aPath first asString].
	^'(' , (self nameAccessExpression: (aPath allButLast: 1)) , ' at: ' , aPath last printString , ')'
%
method: Class
namespacePath
	"<Array of: Symbol>
	the list of symbol dictionary names where the receiver is defined"

	^self namespacePathsAtClasses at: self ifAbsent: [#()]
%
method: Class
namespacePath: anArrayOfSymbols
	self namespacePathsAtClasses at: self put: anArrayOfSymbols
%
category: '*Values-testing'
method: Class
isInScope: aClass
	"<Boolean>
	true if aClass is visible by the receiver - aClass can be used in the source without namespace qualifier.
	false when aClass cannot be seen by the receiver - use the fullName"

	self definitionDictionary ifNotNil: [:symlist |
		(symlist resolveSymbol: aClass name) ifNotNil: [:entity |
			^entity value == aClass]].
	^(GsSession currentSession resolveSymbol: aClass name) notNil
%
category: '*Values-instance creation'
classmethod: Date
d: dayInteger m: monthInteger y: yearInteger
	^self newDay: dayInteger monthNumber: monthInteger year: yearInteger
%
category: '*Values-accessing'
classmethod: Date
nameRelativeTo: targetClass
	"for all technical subclasses.
	Save, because Date is global"

	^'Date'
%
category: '*Values-printing'
method: Date
printvalueWith: printer
	| wst |
	wst := WriteStream on: (String new: 25).
	wst
		nextPut: $(;
		nextPutAll: (printer classnameOf: self);
		nextPutAll: ' d: ';
		nextPutAll: self dayOfMonth printString;
		nextPutAll: ' m: ';
		nextPutAll: self monthIndex printString;
		nextPutAll: ' y: ';
		nextPutAll: self year printString;
		nextPut: $).
	^LiteralPrintvalue string: wst contents
%
category: '*Values-printing'
method: Dictionary
keysForPrinting
	^[self keys asSortedCollection] on: Error do: [:ex | ex return: self keys]
%
method: Dictionary
printargumentsWith: aPrinter
	^self keysForPrinting collect: [:key |
		DictionaryPrintargument
			name: (key printvalueWith: aPrinter)
			value: ((self at: key) printvalueWith: aPrinter)]
%
method: Dictionary
printvalueWith: printer
	^DictionaryPrintvalue arguments: (self printargumentsWith: printer)
%
category: '*Values-instance creation'
classmethod: Duration
nanoseconds: aNumber
	^self nanoSeconds: aNumber
%
classmethod: Duration
nanoseconds: aNumber
	^self nanoSeconds: aNumber
%
category: '*Values-printing'
method: Duration
printvalueWith: printer
	| wst |
	wst := WriteStream on: (String new: 25).
	wst
		nextPut: $(;
		nextPutAll: (printer classnameOf: self);
		nextPutAll: ' nanoseconds: ';
		nextPutAll: self asNanoseconds printString;
		nextPut: $).
	^LiteralPrintvalue string: wst contents
%
category: '*Values-converting'
method: Duration
asNanoseconds
	^self asNanoSeconds
%
category: '*Values-named values'
classmethod: Object
namedValueNames
	"<Array of: Symbol>
	names to print for special known values"
	
	^#()
%
classmethod: Object
nameOrNilFor: aValue
	"<Symbol | nil>"

	^self namesByValues at: aValue ifAbsent: [nil]
%
classmethod: Object
namesByValues
	"<Dictionary key: Value value: Symbol>"

	^Value namedValuesAt: self ifAbsentPut: [self newNamesByValues]
%
classmethod: Object
newNamesByValues
	"<Dictionary key: Value value: Symbol>"

	| dict |
	dict := Valuemap new: self namedValueNames size.
	self namedValueNames do: [:symbol | dict at: (self perform: symbol) put: symbol].
	^dict
%
category: '*Values-comparing'
method: Object
isEqualValue: anObject
	^self = anObject
%
category: '*Values-printing'
method: Object
asDescription
	"<Text>
	produces a text equivalent to the soure with bold leaf values"

	^self asDescriptionFor: self class
%
method: Object
asDescriptionFor: targetClass
	"<Text>
	produces a text equivalent to the source"

	| printer printvalue |
	printer := ValuePrinter newFor: targetClass.
	printvalue := self printvalueWith: printer.
	^printvalue description
%
method: Object
asSource
	"<String>
	produces a string which can be evaluated in the context of the receiver class to an object equivalent to the receiver"

	^self asSourceFor: self class
%
method: Object
asSourceFor: targetClass
	"<String>
	produces a string which can be evaluated in the context of targetClass to an object equivalent to the receiver"

	| printer printvalue |
	printer := ValuePrinter newFor: targetClass.
	printvalue := self printvalueWith: printer.
	^printvalue sourceWithPrinter: printer
%
method: Object
printvalueWith: printer
	^LiteralPrintvalue string: (self literalString ifNil: [self printString])
%
method: Object
literalString
	"<String | nil>
	answers a string iff the receiver can be printed so that the compiler reconstructs the receiver from it"

	^nil
%
category: '*Values-accessing'
method: Object
namespacePathsAtClasses
	^PDFtalkLibrary at: #namespacePathsAtClasses
%
category: '*Values-testing'
method: Object
isLiteral
	^self literalString notNil
%
category: '*Values-actions'
method: Object
beImmutable
	^self immediateInvariant
%
method: Object
beMutable
	"do nothing on Gemstone"

%
category: '*Values-printing'
method: Point
printvalueWith: printer
	| string |
	string := (self x asSourceFor: printer target) , ' @ ' , (self y asSourceFor: printer target).
	^LiteralPrintvalue string: string
%
category: '*Values-printing'
method: Rectangle
printvalueWith: printer
	| wst |
	wst := WriteStream on: (String new: 25).
	wst
		nextPut: $(;
		nextPutAll: (self origin asSourceFor: printer target);
		nextPutAll: ' corner: ';
		nextPutAll: (self corner asSourceFor: printer target);
		nextPut: $).
	^LiteralPrintvalue string: wst contents
%
category: '*Values-printing'
method: SequenceableCollection
printvalueWith: printer
	self isLiteral ifTrue: [
		^LiteralPrintvalue string: self literalString].
	^ArrayPrintvalue arguments: (self collect: [:item | item printvalueWith: printer])
%
category: '*Values-instance creation'
classmethod: Time
h: hoursInteger
	^self h: hoursInteger m: 0 s: 0
%
classmethod: Time
h: hoursInteger m: minutesInteger
	^self h: hoursInteger m: minutesInteger s: 0
%
classmethod: Time
h: hoursInteger m: minutesInteger s: secondsInteger
	^self fromSeconds: hoursInteger * 3600 + (minutesInteger * 60) + secondsInteger
%
classmethod: Time
zero
	^self h: 0
%
category: '*Values-accessing'
classmethod: Time
nameRelativeTo: targetClass
	"for all technical subclasses.
	Save, because Time is global"

	^'Time'
%
category: '*Values-printing'
method: Time
printvalueWith: printer
	| wst |
	wst := WriteStream on: (String new: 25).
	(self hours isZero and: [
	self minutes isZero and: [
	self seconds isZero]]) ifTrue: [
			wst
				nextPutAll: (printer classnameOf: self);
				nextPutAll: ' zero'.
			^LiteralPrintvalue string: wst contents].
	wst
		nextPut: $(;
		nextPutAll: (printer classnameOf: self);
		nextPutAll: ' h: ';
		nextPutAll: self hours printString.
	(self minutes isZero and: [
	self seconds isZero]) ifTrue: [
			wst nextPut: $).
			^LiteralPrintvalue string: wst contents].
	wst
		nextPutAll: ' m: ';
		nextPutAll: self minutes printString.
	self seconds isZero ifTrue: [
		wst nextPut: $).
		^LiteralPrintvalue string: wst contents].
	wst
		nextPutAll: ' s: ';
		nextPutAll: self seconds printString;
		nextPut: $).
	^LiteralPrintvalue string: wst contents
%
DoIt
	| dict policy |
	dict := SymbolDictionary new.
	dict at: #UNWRITABLE put: dict.
	dict immediateInvariant.
	policy := GsPackagePolicy current.
	policy homeSymbolDict: dict.
	policy externalSymbolList: Array new.
	policy enable.
%
DoIt
	| start |
	UserGlobals removeKey: #FileInSymbolDictionary.
	start := UserGlobals removeKey: #FileInStartingTimestamp ifAbsent: [
		^'No starting timestamp; no duration available'].
	'Run duration: ' , (DateAndTime now - start) printString.
%
IfErr_clear
Commit
# 
# Finished file-in successfully
# 
