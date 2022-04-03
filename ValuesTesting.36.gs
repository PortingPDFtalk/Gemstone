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
	(GsSession currentSession resolveSymbol: #'Values Testing') ifNil: [
		package := GsPackageLibrary createPackageNamed: #'Values Testing'.
		package initialize.
		GsPackageLibrary installPackage: package].
%
DoIt
	UserGlobals at: #FileInSymbolDictionary put: Values Testing.
%
DoIt
	FileInSymbolDictionary at: #codeComponents put: Dictionary new.	"Add root of pundle structure"
	FileInSymbolDictionary at: #namespacePathsAtClasses put: Dictionary new.	"Add registry for namespace paths of classes"
%
DoIt
	| dict components |
	dict := SymbolDictionary new.
	dict name: #'Values Testing'.
	dict at: #comment put: 'Values are simple immutable literal objects.

Tests for Values.

Includes conformance tests for all leaf classes of Value.'.
	dict at: #developmentPrerequisites put: #(#(#any 'SUnitToo' '')).
	dict at: #notice put: ''.
	dict at: #packageName put: 'Values Testing'.
	dict at: #padded put: true.
	dict at: #prerequisiteDescriptions put: #(#(#name 'SUnitToo' #componentType #package)).
	dict at: #prerequisiteParcels put: #(#('SUnitToo' '')).
	dict at: #storeVersion put: '3.0.1.0'.
	components := (GsPackageLibrary packageNamed: #'Values Testing') symbolDict at: #codeComponents.
	components at: dict name put: dict.
%
# Define class ValuemapTests
DoIt
TestCase
	subclass: 'ValuemapTests'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values Testing
%
DoIt
	ValuemapTests category: 'Values Testing'.
	ValuemapTests comment: 'Tests for the behavior of OrderedDictionary as Dictionary and as Value'.
%
# Define class ValuePrinterTests
DoIt
TestCase
	subclass: 'ValuePrinterTests'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values Testing
%
DoIt
	ValuePrinterTests category: 'Values Testing'.
	ValuePrinterTests comment: 'Tests for the source strings of Values'.
%
# Define class Testvalue
DoIt
Value
	subclass: 'Testvalue'
	instVarNames: #(constant optional array dictionary)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Values Testing
%
DoIt
	Testvalue category: 'Values Testing'.
	Testvalue comment: 'Testvalue is a resource class for testing values'.
%
category: 'instance creation'
classmethod: Testvalue
constant: constantSymbol optional: optionalSymbol array: arrayArray dictionary: dictionaryDictionary
	| inst |
	inst := self new.
	inst
		initializeConstant: constantSymbol
		optional: optionalSymbol
		array: arrayArray
		dictionary: dictionaryDictionary.
	^inst
%
category: 'instance creation optional'
classmethod: Testvalue
constant: constantSymbol
	| inst |
	inst := self new.
	inst initializeConstant: constantSymbol optional: nil array: nil dictionary: nil.
	^inst
%
classmethod: Testvalue
constant: constantSymbol array: arrayArray
	| inst |
	inst := self new.
	inst initializeConstant: constantSymbol optional: nil array: arrayArray dictionary: nil.
	^inst
%
classmethod: Testvalue
constant: constantSymbol array: arrayArray dictionary: dictionaryDictionary
	| inst |
	inst := self new.
	inst
		initializeConstant: constantSymbol
		optional: nil
		array: arrayArray
		dictionary: dictionaryDictionary.
	^inst
%
classmethod: Testvalue
constant: constantSymbol dictionary: dictionaryDictionary
	| inst |
	inst := self new.
	inst
		initializeConstant: constantSymbol
		optional: nil
		array: nil
		dictionary: dictionaryDictionary.
	^inst
%
classmethod: Testvalue
constant: constantSymbol optional: optionalSymbol
	| inst |
	inst := self new.
	inst initializeConstant: constantSymbol optional: optionalSymbol array: nil dictionary: nil.
	^inst
%
classmethod: Testvalue
constant: constantSymbol optional: optionalSymbol array: arrayArray
	| inst |
	inst := self new.
	inst
		initializeConstant: constantSymbol
		optional: optionalSymbol
		array: arrayArray
		dictionary: nil.
	^inst
%
classmethod: Testvalue
constant: constantSymbol optional: optionalSymbol dictionary: dictionaryDictionary
	| inst |
	inst := self new.
	inst
		initializeConstant: constantSymbol
		optional: optionalSymbol
		array: nil
		dictionary: dictionaryDictionary.
	^inst
%
category: 'specification'
classmethod: Testvalue
localSpecification
	<constant: #constant class: #(#Symbol)>
	<optional: #optional class: #(#Symbol) default: '#none'>
	<sequence: #array>
	<map: #dictionary>
%
category: 'test instances'
classmethod: Testvalue
example
	^Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x 'name')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> 'string')
%
category: 'accessing'
method: Testvalue
array
	"<Array>"

	^array ifNil: [#()]
%
method: Testvalue
constant
	"<Symbol>"

	^constant
%
method: Testvalue
dictionary
	"<Dictionary>"

	^dictionary ifNil: [Dictionary new beImmutable]
%
method: Testvalue
optional
	"<Symbol>"

	^optional ifNil: [#none]
%
category: 'initialize-release'
method: Testvalue
initializeConstant: constantSymbol optional: optionalSymbol array: arrayArray dictionary: dictionaryDictionary
	constant := constantSymbol.
	(optionalSymbol notNil and: [
	self optional ~= optionalSymbol]) ifTrue: [
			optional := optionalSymbol].
	(arrayArray notNil and: [
	arrayArray notEmpty]) ifTrue: [
			array := (Array withAll: arrayArray) beImmutable].
	(dictionaryDictionary notNil and: [
	dictionaryDictionary notEmpty]) ifTrue: [
			dictionary := (Valuemap withAll: dictionaryDictionary) beImmutable].
	self beImmutable
%
category: 'marshaling'
method: Testvalue
passInstVars
	"for OpenTalk StSt"

	^#(#default #default #default #value)
%
category: 'printing'
method: Testvalue
printvalueWith: printer
	| args |
	args := OrderedCollection new.
	args add: (printer constant: 'constant' value: self constant).
	args add: (printer optional: 'optional' value: optional).
	args add: (printer array: 'array' value: self array).
	args add: (printer dictionary: 'dictionary' value: self dictionary).
	^printer printvalue: self arguments: args
%
category: 'Testing'
method: ValuemapTests
testAdding
	| od od1 |
	od := Valuemap new.
	self assert: od isEmpty.
	od add: #foo -> 'bar'.
	self assert: od size = 1.
	self assert: (od atIndex: 1) = (#foo -> 'bar').
	self assert: (od at: #foo) = 'bar'.
	od at: #baz put: 'xxx'.
	self assert: od size = 2.
	self assert: (od atIndex: 2) = (#baz -> 'xxx').
	self assert: (od at: #baz) = 'xxx'.
	od1 := Valuemap new.
	od1 add: #foo -> 'baz'.
	od1 at: #baz put: 'yyy'.
	od addAll: od1.
	self assert: od size = 2.
	self assert: (od at: #foo) = 'baz'.
	self assert: (od at: #baz) = 'yyy'.
%
method: ValuemapTests
testAddingWithGrowth
	"add iteratively elements, so that the dictionary must grow"

	| letters od od1 |
	letters := #(#a #b #c #d #e #f #g #h #i #j #k #l #m #n #o #p #q #r #s #t #u #v #w #x #y #z).
	od := Valuemap new.
	self shouldnt: [letters do: [:symbol | od add: symbol -> symbol asString]] raise: Error.
	self assert: od keys asArray = #(#a #b #c #d #e #f #g #h #i #j #k #l #m #n #o #p #q #r #s #t #u #v #w #x #y #z).
	od1 := Valuemap new.
	self shouldnt: [letters do: [:symbol | od1 at: symbol put: symbol asString]] raise: Error.
	self assert: od1 keys asArray = #(#a #b #c #d #e #f #g #h #i #j #k #l #m #n #o #p #q #r #s #t #u #v #w #x #y #z).
%
method: ValuemapTests
testComparing
	self assert: (Valuemap with: #foo -> 'bar') = (Valuemap with: #foo -> 'bar').
	self deny: (Valuemap with: #foo -> 'bar') == (Valuemap with: #foo -> 'bar').
	self deny: (Valuemap with: #foo -> 'bar') = (Dictionary with: #foo -> 'bar').
	self deny: (Dictionary with: #foo -> 'bar') = (Valuemap with: #foo -> 'bar').
	self assert: (Valuemap with: #foo -> 'bar' with: $a -> 123) = (Valuemap with: #foo -> 'bar' with: $a -> 123).
	self deny: (Valuemap with: #foo -> 'bar' with: $a -> 123) = (Valuemap with: $a -> 123 with: #foo -> 'bar' ).
	self deny: (Valuemap with: #foo -> 'bar' with: $a -> 123) = (Valuemap with: #foo -> 123 with: $a -> 'bar' ).
	self deny: (Valuemap with: #foo -> 'bar' with: $a -> 123) = (Valuemap with: #foo -> 'bar' with: $a -> 1234).
	self deny: (Valuemap with: #foo -> 'bar' with: $a -> 123) = (Valuemap with: #foo -> 'bars' with: $a -> 123).
	self deny: (Valuemap with: #foo -> 'bar' with: $a -> 123) = (Valuemap with: #foo -> 'bar' with: $a -> 123 with: '' -> 0).
%
method: ValuemapTests
testDescribeAndReadBackNew
	| inst |
	inst := Valuemap new.
	self assert: inst asDescription = inst asSource evaluate asDescription
%
method: ValuemapTests
testEnumeration
	| od keys |
	od := Valuemap new.
	od add: #foo -> 'bar'.
	od at: #baz put: 'xxx'.
	od associationsDo: [:assoc |
		self assert: (assoc isKindOf: Association).
		self assert: assoc key isSymbol.
		self assert: assoc value isString].
	self assert: od keys asArray = #(#foo #baz).
	self assert: od values asArray = #('bar' 'xxx').
	keys := OrderedCollection new.
	od keysDo: [:k | keys add: k].
	self assert: keys asArray = #(#foo #baz).
%
method: ValuemapTests
testExampleSource
	self assert: Valuemap example asSource = '((Valuemap new: 17)
	add: #title -> ''Valuemap class example with examples of all basic values'';
	add: #boolean -> true;
	add: #character -> $A;
	add: #symbol -> #mySymbol;
	add: #string -> ''with umlauts and euro: äöüß€'';
	add: #schluessel -> nil;
	add: #integer -> 42;
	add: #number -> 3.14;
	add: #date -> (Date d: 27 m: 3 y: 2022);
	add: #time -> (Time h: 12 m: 18);
	add: #timestamp -> (Timestamp d: 27 m: 3 y: 2022 h: 12 m: 18);
	add: #duration -> (Duration nanoseconds: 300000000000);
	add: #color -> (ColorValue fromBytesRed: 127 green: 0 blue: 0);
	add: #knownColor -> ColorValue red;
	add: #array -> #(#one ''two'' 3);
	add: #bytearray -> #[1 2 3];
	add: #valuemap -> (Valuemap
		with: #test -> true
		with: #two -> 2
		with: #none -> nil);
	yourself)'
%
method: ValuemapTests
testIndexedAccess
	| od |
	od := Valuemap new.
	od add: #foo -> 'bar'.
	self assert: od size = 1.
	self assert: (od atIndex: 1) = (#foo -> 'bar').
	self should: [od atIndex: 0] raise: OffsetError.
	self should: [od atIndex: 2] raise: OffsetError.
	self should: [od atIndex: #key] raise: ArgumentError.
	self should: [od atIndex: '1'] raise: ArgumentError.
%
method: ValuemapTests
testIndexOf
	| od |
	od := Valuemap new.
	self assert: od isEmpty.
	self assert: (od indexOf: #foo) isZero.
	od add: #foo -> 'bar'.
	self assert: od size = 1.
	self assert: (od indexOf: #foo) = 1.
	self assert: (od indexOf: #baz) isZero.
	od at: #baz put: 'xxx'.
	self assert: od size = 2.
	self assert: (od indexOf: #baz) = 2.
%
method: ValuemapTests
testKeyedAccess
	| od |
	od := Valuemap new.
	od add: #foo -> 'bar'.
	od at: #baz put: 'xxx'.
	self assert: od size = 2.
	self assert: (od at: #foo) = 'bar'.
	self assert: (od at: #baz) = 'xxx'.
	self should: [od at: #bar] raise: LookupError.
	self assert: (od at: #bar ifAbsent: [nil]) isNil.
%
method: ValuemapTests
testReadBackAllTestInstances
	(Valuemap class organization listAtCategoryNamed: (Valuemap class whichCategoryIncludesSelector: #example)) do: [:sel |
		| inst readInst |
		inst := Valuemap perform: sel.
		self shouldnt: [readInst := inst asSource evaluate] raise: Error.
		self assert: inst asSource = readInst asSource]
%
method: ValuemapTests
testRemoving
	| od |
	od := Valuemap new.
	self assert: od isEmpty.
	self should: [od removeKey: #foo] raise: LookupError.
	self shouldnt: [od removeKey: #foo ifAbsent: nil] raise: LookupError.
	self assert: (od removeKey: #foo ifAbsent: nil) isNil.
	od add: #foo -> 'bar'.
	self assert: od size = 1.
	self assert: od keys asArray = #(#foo).
	self assert: (od removeKey: #foo) = 'bar'.
	self assert: od size = 0.
	self assert: od keys asArray = #().
	od add: #foo -> 'bar'.
	od at: #baz put: 'xxx'.
	self assert: od size = 2.
	self assert: od keys asArray = #(#foo #baz).
	self should: [od removeKey: #zzz] raise: LookupError.
	self shouldnt: [od removeKey: #zzz ifAbsent: nil] raise: LookupError.
	self assert: od size = 2.
	self assert: od keys asArray = #(#foo #baz).
	self assert: (od removeKey: #foo) = 'bar'.
	self assert: od size = 1.
	self assert: od keys asArray = #(#baz)
%
category: 'Testing'
method: ValuePrinterTests
testArray
	self assert: Array new asSource = '#()'.
	self assert: #(1 2 3 4) asSource = '#(1 2 3 4)'.
	self assert: #(1 2 3 4 5 6 7 8 9 10) asSource = '#(1 2 3 4 5 6 7 8 9 10)'.
	self assert: (Array with: 1 with: 2 with: 3 with: 4) asSource = '#(1 2 3 4)'.
	self assert: (Array with: 1 with: (Date d: 7 m: 7 y: 2007) with: ColorValue red) asSource = '(Array
	with: 1
	with: (Date d: 7 m: 7 y: 2007)
	with: ColorValue red)'.
	self assert: (#(1 2 3 4) , (Array with: (Date d: 7 m: 7 y: 2007))) asSource = '((OrderedCollection new: 5)
	add: 1;
	add: 2;
	add: 3;
	add: 4;
	add: (Date d: 7 m: 7 y: 2007);
	yourself)'.
	self assert: (Array with: Testvalue example with: (Date d: 7 m: 7 y: 2007) with: ColorValue red) asSource = '(Array
	with: (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x ''name'')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> ''string''))
	with: (Date d: 7 m: 7 y: 2007)
	with: ColorValue red)'.
%
method: ValuePrinterTests
testArraySource
	self assert: (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x 'name')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> 'string')) asSource = '(Testvalue
	constant: #constantSymbol
	optional: #optionalSymbol
	array: #(1 #a $x ''name'')
	dictionary: (Valuemap
		with: 1 -> #symbol
		with: #key -> ''string''))'.
%
method: ValuePrinterTests
testBoolean
	self assert: true asSource = 'true'.
	self assert: false asSource = 'false'.
%
method: ValuePrinterTests
testColorValue
	self assert: ColorValue new isEmpty.
	self assert: ColorValue red asSource = 'ColorValue red'.
	self assert: (ColorValue fromBytesRed: 255 green: 0 blue: 0) asSource = 'ColorValue red'.
	self assert: (ColorValue fromBytesRed: 255 green: 0 blue: 10) asSource = '(ColorValue fromBytesRed: 255 green: 0 blue: 10)'
%
method: ValuePrinterTests
testDate
	self assert: (Date d: 7 m: 7 y: 2007) asSource = '(Date d: 7 m: 7 y: 2007)'
%
method: ValuePrinterTests
testDictionary
	| dict |
	self assert: Valuemap new asSource = 'Valuemap new'.
	dict := Valuemap
		with: 1 -> 75
		with: #b -> $x.
	self assert: dict asSource = '(Valuemap
	with: 1 -> 75
	with: #b -> $x)'
%
method: ValuePrinterTests
testDictionarySource
	self assert: (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x 'name')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> 'string')) asSource = '(Testvalue
	constant: #constantSymbol
	optional: #optionalSymbol
	array: #(1 #a $x ''name'')
	dictionary: (Valuemap
		with: 1 -> #symbol
		with: #key -> ''string''))'.
	self assert: (Testvalue constant: #Haider) asSource = '(Testvalue constant: #Haider)'
%
method: ValuePrinterTests
testDictionaryWithValues
	| dict |
	dict := Valuemap
		with: 1 -> Testvalue example
		with: Testvalue example -> $x.
	self assert: dict asSource = '(Valuemap
	with: 1 -> (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x ''name'')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> ''string''))
	with: (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x ''name'')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> ''string'')) -> $x)'
%
method: ValuePrinterTests
testDuration
	self assert: 5 minutes asSource = '(Duration nanoseconds: 300000000000)'
%
method: ValuePrinterTests
testLiteralArrays
	self assert: #(1 2 10 20 100) asSource = '#(1 2 10 20 100)'
%
method: ValuePrinterTests
testLongDictionary
	| dict |
	dict := Valuemap new.
	dict at: 1 put: 75.
	dict at: #b put: $x.
	dict at: 42 put: 'Hello'.
	dict at: 'abc' put: 32.
	dict at: 2 put: #(#String).
	dict at: true put: #hi.
	self assert: dict asSource = '((Valuemap new: 6)
	add: 1 -> 75;
	add: #b -> $x;
	add: 42 -> ''Hello'';
	add: ''abc'' -> 32;
	add: 2 -> #(#String);
	add: true -> #hi;
	yourself)'
%
method: ValuePrinterTests
testNumbers
	self assert: 42 asSource = '42'.
	self assert: 429999999939333 asSource = '429999999939333'.
	self assert: 2.5 asSource = '2.5'.
	"only VW, not Gemstone
	self assert: 2.5e7 asSource = '2.5e7'.
	self assert: 2.5e-7 asSource = '2.5e-7'.
	self assert: 2.5d137 asSource = '2.5d137'.
	self assert: 2.5d-137 asSource = '2.5d-137'.
	self assert: 25s7 asSource = '25.0000000s'.
	self assert: (2 / 5) asSource = '(2 / 5)'.
	"
%
method: ValuePrinterTests
testOptionalConstantSource
	self assert: (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x 'name')
		dictionary: (Valuemap
			with: 1 -> #symbol
			with: #key -> 'string')) asSource = '(Testvalue
	constant: #constantSymbol
	optional: #optionalSymbol
	array: #(1 #a $x ''name'')
	dictionary: (Valuemap
		with: 1 -> #symbol
		with: #key -> ''string''))'.
	self assert: (Testvalue
		constant: #constantSymbol
		optional: #optionalSymbol
		array: #(1 #a $x 'name')) asSource = '(Testvalue constant: #constantSymbol optional: #optionalSymbol array: #(1 #a $x ''name''))'.
	self assert: (Testvalue constant: #constantSymbol optional: #optionalSymbol) asSource = '(Testvalue constant: #constantSymbol optional: #optionalSymbol)'.
	self assert: (Testvalue constant: #constantSymbol) asSource = '(Testvalue constant: #constantSymbol)'
%
method: ValuePrinterTests
testPoint
	self assert: (1 @ 2) asSource = '1 @ 2'.
	self assert: (1 @ (Date d: 7 m: 7 y: 2007)) asSource = '1 @ (Date d: 7 m: 7 y: 2007)'
%
method: ValuePrinterTests
testTime
	self assert: (Time h: 7 m: 27 s: 13) asSource = '(Time h: 7 m: 27 s: 13)'.
	self assert: (Time h: 7 m: 27 s: 0) asSource = '(Time h: 7 m: 27)'.
	self assert: (Time h: 7 m: 27) asSource = '(Time h: 7 m: 27)'.
	self assert: (Time h: 7 m: 0 s: 0) asSource = '(Time h: 7)'.
	self assert: (Time h: 7 m: 0) asSource = '(Time h: 7)'.
	self assert: (Time h: 7) asSource = '(Time h: 7)'.
	self assert: (Time h: 0) asSource = 'Time zero'.
	self assert: Time zero asSource = 'Time zero'
%
method: ValuePrinterTests
testTimestamp
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 7 m: 27 s: 13) asSource = '(Timestamp d: 15 m: 7 y: 2007 h: 7 m: 27 s: 13)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 7 m: 27 s: 0) asSource = '(Timestamp d: 15 m: 7 y: 2007 h: 7 m: 27)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 7 m: 27) asSource = '(Timestamp d: 15 m: 7 y: 2007 h: 7 m: 27)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 7 m: 0 s: 0) asSource = '(Timestamp d: 15 m: 7 y: 2007 h: 7)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 7 m: 0) asSource = '(Timestamp d: 15 m: 7 y: 2007 h: 7)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 7) asSource = '(Timestamp d: 15 m: 7 y: 2007 h: 7)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007 h: 0) asSource = '(Timestamp d: 15 m: 7 y: 2007)'.
	self assert: (Timestamp d: 15 m: 7 y: 2007) asSource = '(Timestamp d: 15 m: 7 y: 2007)'
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
category: '*Values Testing-test instances'
classmethod: Valuemap
example
	"self example"

	| map |
	map := Valuemap new.
	map at: #title put: self class name , ' example with examples of all basic values'.
	map at: #boolean put: true.
	map at: #character put: $A.
	map at: #symbol put: #mySymbol.
	map at: #string put: 'with umlauts and euro: äöüß€'.
	map at: #schluessel put: nil.
	map at: #integer put: 42.
	map at: #number put: 3.14.
	map at: #date put: (Date d: 27 m: 3 y: 2022).
	map at: #time put: (Time h: 12 m: 18).
	map at: #timestamp put: (Timestamp d: 27 m: 3 y: 2022 h: 12 m: 18).
	map at: #duration put: 5 minutes.
	map at: #color put: (ColorValue fromBytesRed: 127 green: 0 blue: 0).
	map at: #knownColor put: ColorValue red.
	map at: #array put: #(#one 'two' 3).
	map at: #bytearray put: #[1 2 3].
	map
		at: #valuemap
		put: (Valuemap
			with: #test -> true
			with: #two -> 2
			with: #none -> nil).
	^map
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
