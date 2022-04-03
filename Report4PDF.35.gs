# Created 3. April 2022 um 18:57:25 by Gemstone Transform(1.4.0.7,chaider)
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
	(GsSession currentSession resolveSymbol: #Report4PDFLibrary) ifNil: [
		package := GsPackageLibrary createPackageNamed: #Report4PDFLibrary.
		package initialize.
		package addPrereq: PDFtalkLibrary.
		GsPackageLibrary installPackage: package].
%
DoIt
	UserGlobals at: #FileInSymbolDictionary put: Report4PDFLibrary.
%
DoIt
	FileInSymbolDictionary at: #codeComponents put: Dictionary new.	"Add root of pundle structure"
	FileInSymbolDictionary at: #namespacePathsAtClasses put: Dictionary new.	"Add registry for namespace paths of classes"
%
DoIt
	| dict components |
	dict := SymbolDictionary new.
	dict name: #Report4PDF.
	dict at: #comment put: 'PDF4Smalltalk  is an implementation of the PDF specification (ISO standard PDF 32000-1:2008) in VisualWorks Smalltalk. Report4PDF generates output for PDF4Smalltalk  using layout and content information, coding in a style influenced by Seaside. This is intended to be a programmer''s tool for building PDF content; it is not an end user report tool.

See R4PReport class>>buildUserGuide for user guide. 
SUnit tests and examples are found in Report4PDF-test 

Developer mailing list: http://www.freelists.org/archive/pdf4st
'.
	dict at: #developmentPrerequisites put: #(#(#any 'PDFtalk' '') #(#any 'ImageReaders' '')).
	dict at: #disregardedPrerequisites put: #().
	dict at: #notice put: 'The MIT License

Copyright © 2012-2018 Bob Nemec

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
	dict at: #packageName put: 'Report4PDF'.
	dict at: #parcelName put: 'Report4PDF'.
	dict at: #prerequisiteDescriptions put: #(#(#name 'PDFtalk' #componentType #bundle) #(#name 'ImageReaders' #componentType #package)).
	dict at: #prerequisiteParcels put: #(#('PDFtalk' '') #('ImageReaders' '')).
	dict at: #storeVersion put: '2.5.0.0'.
	components := (GsPackageLibrary packageNamed: #Report4PDFLibrary) symbolDict at: #codeComponents.
	components at: dict name put: dict.
%
# Define namespace Report4PDF
DoIt
	| newDictionary |
	newDictionary := SymbolDictionary new.
	newDictionary at: #Report4PDF put: newDictionary.
	Report4PDFLibrary at: #Report4PDF put: newDictionary
%
DoIt
System myUserProfile insertDictionary: Report4PDF at: 1.
%
# Define class R4PObject
DoIt
Object
	subclass: 'R4PObject'
	instVarNames: #(properties)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PObject category: 'Report4PDF'.
	R4PObject namespacePath: #(#Report4PDF).
%
# Define class R4POutput
DoIt
R4PObject
	subclass: 'R4POutput'
	instVarNames: #(color colorName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4POutput category: 'Report4PDF'.
	R4POutput namespacePath: #(#Report4PDF).
%
# Define class R4PLayout
DoIt
R4PObject
	subclass: 'R4PLayout'
	instVarNames: #(parent widthPercent font fontSize align verticalAlign margin padding spacing border sections include exclude fixedBox layoutTop boxBottomLimit noWrapSet truncated built boxHeightUsed background foreground newPageBlock fitOnPage)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PLayout category: 'Report4PDF'.
	R4PLayout namespacePath: #(#Report4PDF).
%
# Define class R4PTable
DoIt
R4PLayout
	subclass: 'R4PTable'
	instVarNames: #(headerRow columns cellPadding cellSpacing cellBorder repeatHeadingSet)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PTable category: 'Report4PDF'.
	R4PTable namespacePath: #(#Report4PDF).
%
# Define class R4PText
DoIt
R4PLayout
	subclass: 'R4PText'
	instVarNames: #(flip leading maxFontHeight containsTruncatedText)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PText category: 'Report4PDF'.
	R4PText namespacePath: #(#Report4PDF).
%
# Define class R4POutputPage
DoIt
R4POutput
	subclass: 'R4POutputPage'
	instVarNames: #(renderer output reportPage nextPage previousPage currentY minimumY maximumY pageWidth pageHeight outputPageIndex nextPageNumber)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4POutputPage category: 'Report4PDF'.
	R4POutputPage namespacePath: #(#Report4PDF).
%
# Define class R4POutputBackground
DoIt
R4POutput
	subclass: 'R4POutputBackground'
	instVarNames: #(top left bottom right width height)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4POutputBackground category: 'Report4PDF'.
	R4POutputBackground namespacePath: #(#Report4PDF).
%
# Define class R4PImage
DoIt
R4PLayout
	subclass: 'R4PImage'
	instVarNames: #(image filename scale)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PImage category: 'Report4PDF'.
	R4PImage namespacePath: #(#Report4PDF).
%
# Define class R4POutputImage
DoIt
R4POutput
	subclass: 'R4POutputImage'
	instVarNames: #(matrix xImage)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4POutputImage category: 'Report4PDF'.
	R4POutputImage namespacePath: #(#Report4PDF).
%
# Define class R4PBullet
DoIt
R4PLayout
	subclass: 'R4PBullet'
	instVarNames: #(bulletText)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PBullet category: 'Report4PDF'.
	R4PBullet namespacePath: #(#Report4PDF).
%
# Define class R4POutputString
DoIt
R4POutput
	subclass: 'R4POutputString'
	instVarNames: #(matrix string font fontSize align originalWidth)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4POutputString category: 'Report4PDF'.
	R4POutputString namespacePath: #(#Report4PDF).
%
# Define class R4PCell
DoIt
R4PLayout
	subclass: 'R4PCell'
	instVarNames: #(column columnSpan rowSpan cellHeight)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PCell category: 'Report4PDF'.
	R4PCell namespacePath: #(#Report4PDF).
%
# Define class R4PLine
DoIt
R4PLayout
	subclass: 'R4PLine'
	instVarNames: #(lineWidth)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PLine category: 'Report4PDF'.
	R4PLine namespacePath: #(#Report4PDF).
%
# Define class R4PSection
DoIt
R4PLayout
	subclass: 'R4PSection'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PSection category: 'Report4PDF'.
	R4PSection namespacePath: #(#Report4PDF).
%
# Define class R4PReport
DoIt
R4PObject
	subclass: 'R4PReport'
	instVarNames: #(pageHeight pageWidth pages font fontSize margin pageNumberPattern pageTotalPattern reportTotalPattern builder traceOption traceFileStream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PReport category: 'Report4PDF'.
	R4PReport namespacePath: #(#Report4PDF).
%
# Define class R4PError
DoIt
Exception
	subclass: 'R4PError'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PError category: 'Report4PDF'.
	R4PError namespacePath: #(#Report4PDF).
%
# Define class R4PColumn
DoIt
R4PLayout
	subclass: 'R4PColumn'
	instVarNames: #(cells columnWidth)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PColumn category: 'Report4PDF'.
	R4PColumn namespacePath: #(#Report4PDF).
%
# Define class R4PBuilder
DoIt
R4PObject
	subclass: 'R4PBuilder'
	instVarNames: #(report reportPage outputPages currentPage currentFont)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PBuilder category: 'Report4PDF'.
	R4PBuilder namespacePath: #(#Report4PDF).
%
# Define class R4POutputLine
DoIt
R4POutput
	subclass: 'R4POutputLine'
	instVarNames: #(startPoint endPoint width)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4POutputLine category: 'Report4PDF'.
	R4POutputLine namespacePath: #(#Report4PDF).
%
# Define class R4PPage
DoIt
R4PLayout
	subclass: 'R4PPage'
	instVarNames: #(pageWidth pageHeight header footer nextPageNumber watermark outputPages)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PPage category: 'Report4PDF'.
	R4PPage namespacePath: #(#Report4PDF).
%
# Define class R4PString
DoIt
R4PLayout
	subclass: 'R4PString'
	instVarNames: #(lineBreak endWithLineBreak string positionX)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PString category: 'Report4PDF'.
	R4PString namespacePath: #(#Report4PDF).
%
# Define class R4PRow
DoIt
R4PLayout
	subclass: 'R4PRow'
	instVarNames: #(cellPadding cellSpacing cellBorder maxOutputPage)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Report4PDF
%
DoIt
	R4PRow category: 'Report4PDF'.
	R4PRow namespacePath: #(#Report4PDF).
%
DoIt
System myUserProfile removeDictionaryAt: 1.
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
category: '*Report4PDF-converting'
method: CharacterCollection
asReportString
	"replaces #greaseString"

	^self asString
%
category: '*Report4PDF-Report4PDF'
method: Array
asLayoutArray

	^self
%
category: '*Report4PDF-Report4PDF'
method: Character
asReportString

	^String with: self
%
category: '*Report4PDF-Report4PDF'
method: Number
asLayoutArray
	"top right bottom left"

	^Array 
		with: self 
		with: self 
		with: self 
		with: self
%
category: '*Report4PDF-Report4PDF'
method: Object
asReportString

	^self printString
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
System myUserProfile insertDictionary: Report4PDF at: 1.
%
category: 'accessing'
method: R4PBuilder
currentFont
	^currentFont
%
method: R4PBuilder
currentFont: anObject
	currentFont := anObject
%
method: R4PBuilder
layoutBounds

	^self reportPage layoutBounds
%
method: R4PBuilder
outputPages
	^outputPages
%
method: R4PBuilder
outputPages: anObject
	outputPages := anObject
%
method: R4PBuilder
report
	^report
%
method: R4PBuilder
report: anObject
	report := anObject
%
method: R4PBuilder
reportPage
	^reportPage
%
method: R4PBuilder
reportPage: aPage
	reportPage := aPage
%
category: 'accessing - output'
method: R4PBuilder
clearCurrentPage
	"use the last outputPage"

	self currentPage: nil
%
method: R4PBuilder
currentPage
	"currentPage can be set to a previous page for table output; normally it is nil"

	^currentPage ifNil: [self outputPages last]
%
method: R4PBuilder
currentPage: aPage

	currentPage := aPage
%
method: R4PBuilder
currentPageNumber

	^self outputPages indexOf: self currentPage
%
method: R4PBuilder
currentY
	^self currentPage currentY
%
method: R4PBuilder
currentY: aNumber
	"'currentY:' echo: aNumber."
	self currentPage currentY: aNumber
%
method: R4PBuilder
maximumY
	"page height - footer height"

	^self currentPage maximumY
%
method: R4PBuilder
maximumY: aNumber
	
	self currentPage maximumY: aNumber
%
method: R4PBuilder
remainingY

	^self maximumY - self currentY
%
category: 'actions'
method: R4PBuilder
addOutput: anOutput

	self currentPage outputAdd: anOutput
%
method: R4PBuilder
addOutputAll: aCollection

	aCollection do: [:each | 
		self currentPage outputAdd: each]
%
method: R4PBuilder
checkFitImage: aLayoutImage

	aLayoutImage outputOnNewPage ifFalse: [
		(aLayoutImage canBuildAt: self currentY limit: self maximumY) ifTrue: [^self]].

	(aLayoutImage layoutHeight > aLayoutImage pageHeight) ifTrue: [
		^self reportError: 'Image cannot fit on page'].

	self pageBreak.
	self incrementY: aLayoutImage spacingTop.
	aLayoutImage layoutTop: self currentY.

	aLayoutImage parent buildNewPageParent: self.
	aLayoutImage parent buildNextPageParent: self.
%
method: R4PBuilder
checkFitLine: aLayoutLine

	aLayoutLine outputOnNewPage ifFalse: [
		(aLayoutLine canBuildAt: self currentY limit: self maximumY) ifTrue: [^self]].

	self pageBreak.
	self incrementY: aLayoutLine spacingTop.
	aLayoutLine layoutTop: self currentY.

	aLayoutLine parent buildNewPageParent: self.
	aLayoutLine parent buildNextPageParent: self.
%
method: R4PBuilder
checkFitRowBottom: aLayoutRow
	"a row's cells will create new output pages as required; row's bottom spacing may trigger a page break.
	cell's leave current Y unchanged, so the row has to do the final increment"

	(self findPageToFitRowBottom: aLayoutRow) ifTrue: [
		^self incrementY: aLayoutRow layoutHeight].

	self pageBreak.
	self incrementY: aLayoutRow spacingTop.
%
method: R4PBuilder
checkFitRowTop: aLayoutRow
	"we have cases where = failed on floats that should be equal"

	aLayoutRow outputOnNewPage ifFalse: [
		(self findPageToFitRowTop: aLayoutRow) ifTrue: [^self]].

	self pageBreak.
	aLayoutRow layoutTop: self currentY.
	aLayoutRow parent buildNewPageParent: self.
	aLayoutRow parent buildNextPageParent: self.
%
method: R4PBuilder
checkFitSpacingBottom: aLayoutText

	aLayoutText isFixedLayout ifTrue: [^self].

	(aLayoutText 
		canEndAtY: self currentY 
		limit: self maximumY) 
			ifTrue: [self incrementY: aLayoutText spacingBottom]
			ifFalse: [self pageBreak]
%
method: R4PBuilder
checkFitSpacingTop: aLayout
	"make sure there is room from top spacing; each line of text will check to see if it fits and may trigger a page break"

	aLayout outputOnNewPage ifFalse: [
		(self findPageToFitLayout: aLayout) ifTrue: [^true]].

	self pageBreak.
	self incrementY: aLayout spacingTop.
	aLayout layoutTop: self currentY.
	aLayout parent buildNewPageParent: self.
	aLayout parent buildNextPageParent: self.
	^false
%
method: R4PBuilder
checkFitString: aLayoutString
	"check fit again in case next page also does not fit.
	parent border may need to be done again if this is a row cell"

	aLayoutString outputOnNewPage ifFalse: [
		(self findPageToFitString: aLayoutString) ifTrue: [^self]].

	self pageBreak.

	self setLayout: aLayoutString.
	aLayoutString parent buildNewPageParent: self.
	aLayoutString parent buildNextPageParent: self.
%
method: R4PBuilder
findPageToFitLayout: aLayout

	(aLayout canBuildAt: self currentY limit: self maximumY) ifTrue: [^true].

	[self currentPage nextPage notNil] whileTrue: [
		self currentPage: self currentPage nextPage.
		(aLayout canBuildAt: self currentY limit: self maximumY) ifTrue: [^true]].

	^false
%
method: R4PBuilder
findPageToFitRowBottom: aLayoutRow
	"need to increment Y after all the cells are done"

	(aLayoutRow canEndAtY: self currentY limit: self maximumY) ifTrue: [^true].

	[self currentPage nextPage notNil] whileTrue: [
		self currentPage: self currentPage nextPage.
		(aLayoutRow canEndAtY: self currentY limit: self maximumY) ifTrue: [^true]].

	^false
%
method: R4PBuilder
findPageToFitRowTop: aLayoutRow

	(aLayoutRow canBuildAt: self currentY limit: self maximumY) ifTrue: [^true].

	[self currentPage nextPage notNil] whileTrue: [
		self currentPage: self currentPage nextPage.
		(aLayoutRow canBuildAt: self currentY limit: self maximumY) ifTrue: [^true]].

	^false
%
method: R4PBuilder
findPageToFitString: aLayoutString

	(aLayoutString canBuildAt: self currentY limit: self maximumY) ifTrue: [^true].

	[self currentPage nextPage notNil] whileTrue: [
		self currentPage resetCurrentY.
		self currentPage: self currentPage nextPage.
		(aLayoutString canBuildAt: self currentY limit: self maximumY) ifTrue: [
			aLayoutString parent buildNextPageParent: self.
			^true]].

	^false
%
method: R4PBuilder
incrementY: aNumber
	
	| newY | 

	newY := self currentY + aNumber.
	(newY roundTo: 0.01) > (self maximumY roundTo: 0.01) ifTrue: [
		^self reportError: 'PRwBuilder>>incrementY: ', aNumber printString, ' exceeded maximumY: ', self maximumY printString].

	self currentY: newY
%
method: R4PBuilder
newOutputPage

	self outputPagesAdd: R4POutputPage new.
%
method: R4PBuilder
outputPagesAdd: aPage

	self outputPages add: aPage.
	aPage outputPageIndex: (self outputPages indexOf: aPage).
	self reportPage outputPagesAdd: aPage.
%
method: R4PBuilder
pageBreak
	"A 2nd page break can be triggered from a table cell if it's neighbour already triggered one.
	aBlock is only intended for 'new' pages, not when revisiting 'next' pages"

	| nextPage | 

	nextPage := self currentPage createNextPageOutput. 
	self currentPage resetCurrentY.
	self clearCurrentPage.
	self outputPagesAdd: nextPage.
	self buildPageSetup.
%
method: R4PBuilder
resetCurrentYAfter: aBlock

	| previousY previousPage | 

	previousY := self currentY. 
	previousPage := self currentPage.

	aBlock value.

	self currentPage resetCurrentY.
	self currentPage: previousPage.
	self currentY: previousY.
%
method: R4PBuilder
setCurrentPage: anOutputPage
	"a nested table may need to set the currentPage to a page that is not the 'largest' 
	if outputPages is emtpy at this point, we have an error"

	self outputPages last == anOutputPage ifTrue: [	
		self clearCurrentPage].

	self currentPage: anOutputPage.
%
method: R4PBuilder
setLayout: aLayout

	aLayout layoutTop: self currentY.
	aLayout setBoxBottomLimit: self maximumY.
	self incrementY: aLayout spacingTop.
%
method: R4PBuilder
updateOutputPages
	"replace strings if total pages pattern is used"
	"need to fix root cause of extra output pages with tables that have page breaks. 
	See exampleTextColumnsPageBreaks2"

	| count | 

	self outputPages: (self outputPages reject: [:each | each output isEmpty]).

	count := 0.
	self outputPages do: [:eachPage |
		count := count + 1.
		eachPage nextPageNumber notNil ifTrue: [
			count := eachPage nextPageNumber].
		eachPage 
			replaceString: self report pageNumberPattern 
			with: count printString.
		eachPage
			replaceString: self report pageTotalPattern 
			with: eachPage outputPagesSize printString.
		eachPage
			replaceString: self report reportTotalPattern 
			with: self outputPages size printString].
%
category: 'build'
method: R4PBuilder
buildPage

	self clearCurrentPage. 
	self newOutputPage.
	self buildPageSetup.
	self buildPageSections.
%
method: R4PBuilder
buildPages
	"for debugging, send #buildPagesX"

	[self buildPagesX]
		on: R4PError
		do: [:ex | self currentPage outputErrorMessage: '*** ERROR building PDF document: ', ex displayString].
%
method: R4PBuilder
buildPageSections

	self reportPage layoutSections do: [:each | each buildOutput: self].
%
method: R4PBuilder
buildPageSetup

	self currentPage 
			pageWidth: self report pageWidth; 
			pageHeight: self report pageHeight.

	self currentY: self reportPage layoutTop.
	self maximumY: self reportPage layoutBottom.
	self currentPage previousPage isNil ifTrue: [ "only done by first output page"
		self currentPage nextPageNumber: self reportPage nextPageNumber]. 

	self reportPage buildWatermark: self.
	self buildFooter.
	self buildHeader.
%
method: R4PBuilder
buildPagesX

	self report pages do: [:eachPage | 
		self reportPage: eachPage.
		self buildPage]
%
method: R4PBuilder
buildReport: aReport

	| document  | 

	[	
		aReport start.
	 	self report: aReport.
		aReport builder: self.
	
		self buildPages.
		self updateOutputPages.
		aReport traceOutput:  aReport printOutput.
		aReport traceOutput:  'output pages: ', self outputPages size printString
	] ensure: [
		aReport end].

	document := (PDFtalk at: #Document) new.
	self outputPages do: [:each | 
		document root addPage: each renderPage].
	^document
%
category: 'build - layout'
method: R4PBuilder
buildBackground: aLayout

	aLayout hasBackground ifFalse: [^self].
	self addOutput: (aLayout outputBackgroundAt: self currentPageNumber)
%
method: R4PBuilder
buildBorder: aLayout

	aLayout hasBorder ifFalse: [^self].
	self currentPage outputBorder: aLayout at: self currentPageNumber.
%
method: R4PBuilder
buildBullet: aBulletLayout  

	self checkFitSpacingTop: aBulletLayout.

	self resetCurrentYAfter: [
		self setLayout: aBulletLayout.
		self buildBackground: aBulletLayout bulletText.
		self buildBorder: aBulletLayout bulletText.
		self incrementY:  aBulletLayout bulletTextYAdjustment.
		self buildText: aBulletLayout bulletText].

	self buildSection: aBulletLayout.
%
method: R4PBuilder
buildCell: aLayoutCell
	"reset currentY because the cell contents may be multi-line ... contains truncated text if rowSpan > 1 and height > total row height"

	self resetCurrentYAfter: [
		self checkFitSpacingTop: aLayoutCell.
		self setLayout: aLayoutCell.
		self buildBackground: aLayoutCell.
		self buildBorder: aLayoutCell.
		self buildSections: aLayoutCell.
		self addOutputAll: aLayoutCell outputTruncated.
		aLayoutCell outputPage: self currentPage; built: true].
%
method: R4PBuilder
buildFixedImage: aLayoutImage

	self resetCurrentYAfter: [
		self currentY: aLayoutImage fixedTop.
		self incrementY: aLayoutImage spacingTop.
		self addOutput: aLayoutImage outputImage.
		self buildBorder: aLayoutImage.
		self buildSections: aLayoutImage]
%
method: R4PBuilder
buildFixedSection: aSection

	self resetCurrentYAfter: [
		self currentY: aSection fixedTop.
		self incrementY: aSection spacingTop.
		self buildBackground: aSection.
		self buildBorder: aSection.
		self buildSections: aSection].
%
method: R4PBuilder
buildFooter

	| previousY newMaximumY | 

	self reportPage footer layoutSections isEmpty ifTrue: [^self].

	newMaximumY :=  (self maximumY) - (self reportPage footer layoutHeight).
	previousY := self currentY. 
	self currentY: newMaximumY.
	self buildSection: self reportPage footer.
	self maximumY: newMaximumY.
	self currentY: previousY.
%
method: R4PBuilder
buildHeader
	
	(self reportPage header layoutHeight) > (self report pageHeight) ifTrue: [
		self error: 'header height is greate than page height: ', 
				((self reportPage header layoutHeight) -> (self report pageHeight)) printString].

	self buildSection: self reportPage header.
	self currentPage setMinimumY.
%
method: R4PBuilder
buildImage: aLayoutImage
	"border is done after image in case padding is not set; border will overlay the image edge, othewise it would be invisible"

	aLayoutImage isFixedLayout ifTrue: [
		^self buildFixedImage: aLayoutImage].

	self checkFitImage: aLayoutImage.

	self setLayout: aLayoutImage.
	self addOutput: aLayoutImage outputImage.
	self buildBorder: aLayoutImage.
	self incrementY: aLayoutImage layoutHeight.
	self buildSections: aLayoutImage.

	aLayoutImage built: true.
%
method: R4PBuilder
buildLine: aLayoutLine

	aLayoutLine isFixedLayout ifTrue: [
		^self addOutput: aLayoutLine fixedOutputLine].

	self checkFitLine: aLayoutLine. 

	self setLayout: aLayoutLine.
	self addOutput: aLayoutLine outputLine.
	aLayoutLine built: true.

	self incrementY: aLayoutLine nestedLayoutHeight + aLayoutLine spacingBottom.
%
method: R4PBuilder
buildRow: aLayoutRow  

	self checkFitRowTop: aLayoutRow. 
	self setLayout: aLayoutRow.
	self buildBackground: aLayoutRow.
	self buildBorder: aLayoutRow.

	aLayoutRow setCellHeight. 
	self buildSections: aLayoutRow. 
	self setCurrentPage: aLayoutRow maxOutputPage.

	self checkFitRowBottom: aLayoutRow.
	aLayoutRow built: true.
%
method: R4PBuilder
buildSection: aSection

	aSection isFixedLayout ifTrue: [
		^self buildFixedSection: aSection].

	self checkFitSpacingTop: aSection.
	self setLayout: aSection.
	self buildBackground: aSection.
	self buildBorder: aSection.
	self buildSections: aSection.
	aSection built: true.
%
method: R4PBuilder
buildSections: aLayout

	aLayout layoutSections do: [:each | each buildOutput: self].
%
method: R4PBuilder
buildString: aLayoutString 
	"fit is checked for a string prior to adding it, so there should not be a page break after it is added.
	Sender increments Y ... different for footer vs. regular text"

	self checkFitString: aLayoutString.
	aLayoutString layoutTop: self currentY.
	self addOutputAll: aLayoutString outputContent.

	aLayoutString lineBreak ifTrue: [
		self incrementY: aLayoutString layoutHeight].

	aLayoutString built: true.
%
method: R4PBuilder
buildTable: aLayoutTable

	self buildSection: aLayoutTable.
	self checkFitSpacingBottom: aLayoutTable.
%
method: R4PBuilder
buildTableHeaderRow: aLayoutTable
	"sent when a table with repeating headings has nested cells that trigger a page break.
	set minimum Y so that the top of the next columns line up, since they don't trigger the header rendering"

	self checkFitSpacingTop: aLayoutTable.
	self setLayout: aLayoutTable.

	aLayoutTable headerRow notNil ifTrue: [
		aLayoutTable headerRow setBuiltToFalse. 
		self buildRow: aLayoutTable headerRow].

	self currentPage setMinimumY.
%
method: R4PBuilder
buildText: aLayoutText  

	self buildSection: aLayoutText.
	self checkFitSpacingBottom: aLayoutText.
%
category: 'initialize-release'
method: R4PBuilder
initialize

	super initialize.

	outputPages := OrderedCollection new.

	^self
%
category: 'printing'
method: R4PBuilder
printOn: aStream

	super printOn: aStream.
	aStream
		cr; tab; nextPutAll: 'currentY: '; nextPutAll: self currentY printString; 
		cr; tab; nextPutAll: 'maximumY: '; nextPutAll: self maximumY printString; 
		cr; tab; nextPutAll: 'currentPage: '; nextPutAll: self currentPageNumber printString; 
			nextPutAll: ' of: '; nextPutAll: self outputPages size printString; 
		cr; tab; nextPutAll: 'minimumY: '; nextPutAll: self currentPage minimumY printString; 
		yourself
%
category: 'accessing'
method: R4PBullet
bulletFont: aSymbol

	self bulletText font: aSymbol
%
method: R4PBullet
bulletFontSize: aNumber

	self bulletText fontSize: aNumber
%
method: R4PBullet
bulletText
	^bulletText
%
method: R4PBullet
bulletText: anObject
	bulletText := anObject
%
method: R4PBullet
indent: aNumber
	
	self bulletText marginLeft: aNumber.
%
category: 'accessing - keywords'
method: R4PBullet
circle

	self bulletText replaceString: 'l'
%
method: R4PBullet
diamond

	self bulletText replaceString: 'u'
%
method: R4PBullet
large

	self bulletFontSize: 8
%
method: R4PBullet
small

	self bulletFontSize: 4
%
method: R4PBullet
square

	self bulletText replaceString: 'n'
%
method: R4PBullet
veryLarge

	self bulletFontSize: 10
%
method: R4PBullet
verySmall

	self bulletFontSize: 2
%
category: 'actions'
method: R4PBullet
buildOutput: aBuilder 

	aBuilder buildBullet: self
%
method: R4PBullet
bulletTextYAdjustment
	"center the bullet based on the text font, so add 1/2 of the difference between the bullet font and text font"

	| bulletHeight textHeight | 
	
	self layoutSections isEmpty ifTrue: [^0].
	bulletHeight := self bulletText fontHeight.
	textHeight := self sections first fontHeight.

	bulletHeight >= textHeight ifTrue: [^0].

	^(textHeight - bulletHeight) / 2.0
%
category: 'actions - add'
method: R4PBullet
character: aCharacter

	self bulletText 
		font: #Helvetica; 
		fontSize: 10; 
		replaceString: aCharacter asReportString
%
method: R4PBullet
number: aNumber

	self character: aNumber
%
category: 'calculate'
method: R4PBullet
calculateLayout

	self bulletText calculateLayout.
	self sections do: [:each | 
		each marginLeft: self bulletText initialStringWidth 
							+ self bulletText fontBoxWidth
							+ self bulletText marginLeft].
	super calculateLayout.
%
category: 'initialize-release'
method: R4PBullet
initialize

	super initialize.

	self initializeBulletText. 

	^self
%
method: R4PBullet
initializeBulletText

	self bulletText: (R4PText newForParent: self).
	self bulletText noWrap.
	"self bulletText border: 0.1 . "
	self bulletFont: #ZapfDingbats.
	self bulletFontSize: 6.
	self circle.
%
category: 'printing'
method: R4PBullet
printLayoutOn: aStream

	super printLayoutOn: aStream.

	aStream cr; tab; nextPutAll: 'bullet: '; nextPutAll: self bulletText displayString.
%
category: 'testing'
method: R4PBullet
canBuildAt: aNumber limit: aMaximumY

	(((aNumber + self bulletTextYAdjustment) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)) ifFalse: [^false].

	self sections isEmpty ifTrue: [^true].
	^self sections first 
			canBuildAt: aNumber + self spacingTop
			limit: aMaximumY
%
category: 'accessing'
method: R4PCell
cellHeight
	"first: calculate the layout height of a cell using its nested layouts. next: set the cell height to the row height, which is max of all cell heights"

	^cellHeight
%
method: R4PCell
cellHeight: anObject
	cellHeight := anObject
%
method: R4PCell
column
	^column
%
method: R4PCell
column: anObject
	column := anObject
%
method: R4PCell
columnSpan
	^columnSpan
%
method: R4PCell
columnSpan: anObject
	columnSpan := anObject
%
method: R4PCell
fixedBottom

	^self parent layoutBottom
%
method: R4PCell
fixedTop

	^self basicLayoutTop
%
method: R4PCell
lastRow

	^self parent lastRowOf: self
%
method: R4PCell
rowSpan
	^rowSpan
%
method: R4PCell
rowSpan: anObject
	rowSpan := anObject
%
method: R4PCell
width: aNumber
	"for cells only a fixed width is allowed; no offset from a fixedLeft ... notice the other 'fixed' accessors for cell "	

	self fixedRight: aNumber
%
category: 'build - layout'
method: R4PCell
buildOutput: aBuilder

	aBuilder buildCell: self.
%
category: 'calculate'
method: R4PCell
boxHeightRemainingAt: anIndex
	"No box calculation for repeating headers. They are the same on each page."

	self isHeaderRowCell ifTrue: [^self boxHeight].
	^super boxHeightRemainingAt: anIndex
%
method: R4PCell
calculateRowSpan

	self spansRows ifFalse: [^self].
	self parent calculateRowSpanCell: self
%
method: R4PCell
columnWidth
	
	self spansColumns ifTrue: [
		^(self layoutRight - self layoutLeft)].

	^self column columnWidth
%
method: R4PCell
columnWidthFor: aColumn
	"treat width percent as fixed width ... can be set by any cell in the rows (last one wins)"
	
	^self widthPercent notNil
		ifTrue: [
			aColumn widthPercent: self widthPercent. "used here as a flag to lock column width"
			self cellWidth]
		ifFalse: [
			aColumn columnWidth max: self cellWidth]
%
category: 'initialize-release'
method: R4PCell
initialize

	super initialize.

	columnSpan := 1.
	rowSpan := 1.
	cellHeight := nil.
	^self
%
category: 'layout'
method: R4PCell
boxBottom
	
	self spansRows ifTrue: [
		^self boxBottomRowSpan].

	^self parent boxBottom
		- self parent paddingBottom
		- self marginBottom
%
method: R4PCell
boxBottomAt: aPageNumber
	"bypass 'self boxTop + (self nestedUnbuiltLayoutHeight) + self spacingBottom' for table cells ... see super class method"

	| bottom | 

	self isFixedLayout ifTrue: [^self fixedBottom].
	bottom := self layoutBottom - self marginBottom.
	bottom := bottom min: self boxBottomLimit.

	^bottom
%
method: R4PCell
boxBottomRowSpan

	| lastRowBottom |

	lastRowBottom := self parent lastRowBottomOf: self.
	^lastRowBottom
		- self marginBottom
%
method: R4PCell
boxRight

	^self layoutRight 
		- self marginRight
%
method: R4PCell
boxTop
	"All cells in a row have the same box top"

	^self parent boxTop
		+ self parent paddingTop
		+ self marginTop
%
method: R4PCell
cellWidth
	"only used for single column cells"

	self widthPercent notNil ifTrue: [
		^(self tableWidth) * (self widthPercent / 100.0)].

	self isFixedLayout ifTrue: [
		^self fixedRight].

	^self initialStringWidth 
		+ self spacingLeft 
		+ self spacingRight
%
method: R4PCell
contentHeight

	^self cellHeight ifNil: [super contentHeight]
%
method: R4PCell
contentLeft

	^self layoutLeft
		+ self spacingLeft
%
method: R4PCell
contentRight

	^self layoutRight
		- self spacingRight
%
method: R4PCell
contentWidth

	^self columnWidth
		- self spacingLeft
		- self spacingRight
%
method: R4PCell
fixedLeft

	^self layoutLeft
%
method: R4PCell
layoutHeight
	"cell height is set by the row after each cell's height is calculated, 
	prior to that, cell height is calculated, even for fixed width columns"

	self isSingleRow ifFalse: [^self totalRowHeight].
	^self cellHeight ifNil: [self layoutHeightCalculated]
%
method: R4PCell
layoutLeft

	^self parent contentLeftOf: self
%
method: R4PCell
layoutNoWrap

	^super layoutNoWrap or: [
		self sections anySatisfy: [:each | each noWrapSet == true]]
%
method: R4PCell
layoutRight

	self spansColumns ifTrue: [
		^self parent layoutRightOf: self].

	^self layoutLeft 
		+ self columnWidth
%
method: R4PCell
layoutTruncated

	^super layoutTruncated or: [self column layoutTruncated]
%
method: R4PCell
tableWidth

	^self parent tableWidth
%
method: R4PCell
totalRowHeight
	"for rowSpan"

	^self parent totalRowHeightOf: self
%
category: 'output'
method: R4PCell
outputPage: anOutputPage

	self parent setMaxOutputPage: anOutputPage.
%
method: R4PCell
outputTruncated

	^self containsTruncatedText 
		ifTrue: [Array with: self truncatedImage]
		ifFalse: [Array new]
%
category: 'printing'
method: R4PCell
displayColumnIndex

	self column isNil ifTrue: [^'<no column>'].
	^self column displayColumnIndex
%
method: R4PCell
displayRowIndex

	self parent isNil ifTrue: [^'<no row>'].
	^self parent displayRowIndex
%
method: R4PCell
displayString

	self sections isEmpty ifTrue: [^'<none>'].
	^self sections first displayString
%
method: R4PCell
printLayoutOn: aStream

	super printLayoutOn: aStream.

	aStream cr; tab; nextPutAll: 'row: '; nextPutAll: self displayRowIndex. 
	aStream cr; tab; nextPutAll: 'column: '; nextPutAll: self displayColumnIndex. 
	self spansColumns ifTrue: [aStream cr; tab; nextPutAll: 'columnSpan: '; nextPutAll: self columnSpan printString].
	self spansRows ifTrue: [aStream cr; tab; nextPutAll: 'rowSpan: '; nextPutAll: self rowSpan printString].

	self sections isEmpty ifTrue: [^self].
	aStream cr; tab; nextPutAll: self sections first displayString printString
%
category: 'testing'
method: R4PCell
canBuildAt: aNumber limit: aMaximumY
	"is there room for the first section?"

	((aNumber + self spacingTop) roundTo: 0.01) >  (aMaximumY roundTo: 0.01) ifTrue: [^false].
	self layoutSections isEmpty ifTrue: [^true].
	(self canFitOnPageAt: aNumber limit: aMaximumY) ifFalse: [^false].

	^self sections first 
			canBuildAt: aNumber + self spacingTop
			limit: aMaximumY
%
method: R4PCell
containsTruncatedText

	self isSingleRow ifTrue: [^false].
	self layoutHeight > self totalRowHeight ifTrue: [^true].

	^false
%
method: R4PCell
isHeaderRowCell

	^self parent isHeaderRow
%
method: R4PCell
isSingleColumn

	^self columnSpan = 1
%
method: R4PCell
isSingleRow

	^self rowSpan = 1
%
method: R4PCell
spansColumns

	^self columnSpan > 1
%
method: R4PCell
spansRows

	^self rowSpan > 1
%
category: 'accessing'
method: R4PColumn
cells
	^cells
%
method: R4PColumn
cells: anObject
	cells := anObject
%
method: R4PColumn
columnWidth
	^columnWidth
%
method: R4PColumn
columnWidth: aNumber
	columnWidth := aNumber
%
category: 'actions'
method: R4PColumn
setCells: anArray

	self cells: anArray.
	anArray do: [:eachCell | 
		eachCell column: self]
%
category: 'calculate'
method: R4PColumn
calculateLayout
	"if a cell spans rows, we want word wrap to be calculated across the rows"

	self columnWidth: 0.
	self cells do: [:eachCell | 
		self widthPercent isNil ifTrue: [  "if a cell in the column has a width percent it sets this value; ignore the remaining cells"
			eachCell isSingleColumn ifTrue: [
				eachCell isSingleRow ifTrue: [
					self columnWidth: (eachCell columnWidthFor: self)]]]].

	super calculateLayout
%
category: 'initialize-release'
method: R4PColumn
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	cells := Array new.

	^self
%
category: 'layout'
method: R4PColumn
contentLeft

	^self layoutLeft
%
method: R4PColumn
contentRight

	^self layoutRight
%
method: R4PColumn
layoutLeft

	^self parent contentLeftOf: self
%
method: R4PColumn
layoutNoWrap

	^self cells anySatisfy: [:each | each layoutNoWrap]
%
method: R4PColumn
layoutRight 

	^self layoutLeft 
		+ self columnWidth
%
category: 'printing'
method: R4PColumn
displayColumnIndex 

	self parent isNil ifTrue: [^'<no table>'].

	^(self parent columns indexOf: self) printString
%
method: R4PColumn
printLayoutOn: aStream

	aStream cr; tab; nextPutAll: 'columnWidth: '; nextPutAll: self columnWidth printString
%
method: R4PColumn
printOn: aStream

	super printOn: aStream.
	
	aStream nextPutAll: ' column: '; nextPutAll: self displayColumnIndex. 
	self cells isEmpty ifTrue: [^aStream nextPutAll: ' <empty>'].
	aStream space; nextPutAll: self cells first displayString
%
category: 'testing'
method: R4PColumn
isFixedLayout

	^self cells anySatisfy: [:each | each isFixedLayout]
%
category: 'initialize-release'
method: R4PError
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Replace this comment with the appropriate initialization code *** "
	^self
%
category: 'printing'
method: R4PError
displayString

	^self messageText asString
%
category: 'instance creation'
classmethod: R4PImage
newForFilename: aFilename parent: aParent

	^self new
		parent: aParent; 
		filename: aFilename; 
		yourself
%
classmethod: R4PImage
newForImage: anImage parent: aParent

	^self new
		parent: aParent; 
		image: anImage; 
		yourself
%
classmethod: R4PImage
newTruncatedOutputForParent: aLayoutString

	| image | 

	image := self newForParent: aLayoutString.
	^image buildTruncatedOutputFor: aLayoutString
%
category: 'accessing'
method: R4PImage
filename
	^filename
%
method: R4PImage
filename: anObject
	filename := anObject.
	self buildImage.
%
method: R4PImage
image
	^image
%
method: R4PImage
image: anObject
	image := anObject.
%
method: R4PImage
scale
	
	^scale ifNil: [self image height * (72 / 96.0)]
%
method: R4PImage
scale: anObject
	scale := anObject
%
method: R4PImage
truncatedGif
	^(ByteArray fromPackedString: 'Q4%FNC%!C@@L@O\@@FA XHB@ HNC ;>?/</K2<;N3,?O3=KR4/[6=/3<?O???0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@B0@@@@@C@@L@@@HQ @UJC! PJAA PTG@@@08JA@@@XVK"2HT@ED"QLUQKR(PBGF@@,9Z,P(TZPB@"PA&EP  JQHAAE]*$1PHJUJ%P8]W+P)LR@@N0@a')
%
method: R4PImage
truncatedXImage
	"<ImageXObject>
	created with:
		| xImage |
		xImage := (GIFImageReader new from: (ReadStream on: R4PImage new truncatedGif)) image asPDF.
		xImage asMethod: #truncatedXImage in: #accessing package: 'Report4PDF'
		
	this creates a method on the class side of ImageXObject. Moved by hand to here and fixed namespaces."

	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 12;
			add: #Height -> 12;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceGray
				number: 255
				bytes: (ByteArray fromASCII85String: '@"<mEbKeG^pAFpjzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz~>'));
			add: #Length -> 62;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: #[120 218 77 140 65 14 0 33 12 2 113 85 84 254 255 96 219 37 26 231 52 5 82 105 80 134 170 64 181 131 8 248 167 246 56 8 33 55 65 65 186 220 216 213 30 215 119 125 242 230 171 191 251 3 206 151 100 3 201 78 2 120]
%
method: R4PImage
widthRatio
	"scale is used for image height, so width has to be adjusted"

	^(self image width) / (self image height)
%
category: 'actions'
method: R4PImage
buildOutput: aBuilder 

	aBuilder buildImage: self
%
method: R4PImage
setTruncatedImageFor: aLayout
	self image: self truncatedXImage.
	self left.
	self fixedTop: aLayout layoutBottom - 14.
	self fixedBottom: aLayout layoutBottom - 2.
	self fixedLeft: aLayout contentRight - 14.
	self fixedRight: aLayout contentRight - 2.
%
method: R4PImage
xImage

	self image isNil ifTrue: [^self reportError: 'No image created from file ', self filename printString].
	^self image asPDF
%
category: 'calculate'
method: R4PImage
calculateLayout
	"scale down to fit page width"

	| ratio | 

	self imageWidth <= self layoutWidth ifTrue: [^self].
	ratio := self layoutWidth / self imageWidth. 
	self scale: self scale * ratio.
	
	super calculateLayout.
%
category: 'initialize-release'
method: R4PImage
initialize

	super initialize.

	image := nil.
	scale := nil.
	^self
%
category: 'layout'
method: R4PImage
boxRight

	self isFixedLayout ifTrue: [^self fixedRight].

	^self boxLeft
		+ self spacingLeft 
		+ self imageWidth
		+ self spacingRight
%
method: R4PImage
contentHeight

	^self scale
%
method: R4PImage
contentLeft
	"scale is used as a linear multiplier, so we'll need to adjust for that here"

	self isLeftJustified ifTrue: [^super contentLeft].
	self isCentered ifTrue: [^super contentLeft + ((self contentWidth - self image width) / 2.0)].
	self isRightJustified ifTrue: [^super contentLeft + (self contentWidth - self image width)].

	self reportError: 'PRwImage>>contentLeft ... invalid layoutAlign: ', self layoutAlign printString
%
method: R4PImage
imageMatrix
	"adjust for scale calculation ... see #scale"

	self isFixedLayout ifTrue: [^self imageMatrixFixed].

	^(Array new: 6)	
		at: 1 put: self imageWidth ; 
		at: 2 put: 0; 
		at: 3 put: 0;
		at: 4 put: self scale negated; 
		at: 5 put: self contentLeft; 
		at: 6 put: (self contentTop + self scale);
		yourself
%
method: R4PImage
imageMatrixFixed

	^(Array new: 6)	
		at: 1 put: self fixedWidth ; 
		at: 2 put: 0; 
		at: 3 put: 0;
		at: 4 put: self fixedHeight negated; 
		at: 5 put: self fixedLeft; 
		at: 6 put: self fixedBottom;
		yourself
%
method: R4PImage
imageWidth 

	^self scale * self widthRatio
%
method: R4PImage
nestedLayoutHeight

	^self contentHeight
%
category: 'output'
method: R4PImage
buildTruncatedOutputFor: aLayout

	self setTruncatedImageFor: aLayout. 

	^R4POutputImage 
		newForMatrix: self imageMatrix 
		xImage: self xImage
%
method: R4PImage
outputImage

	^R4POutputImage 
		newForMatrix: self imageMatrix 
		xImage: self xImage
%
category: 'testing'
method: R4PImage
canBuildAt: aNumber limit: aMaximumY

	^((aNumber + self layoutHeight) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
category: 'instance creation'
classmethod: R4PLayout
newForParent: aParent

	^self new
		parent: aParent; 
		yourself
%
category: 'accessing'
method: R4PLayout
align
	^align
%
method: R4PLayout
align: aSymbol

	| value | 

	value := aSymbol asSymbol.
	value = #centre ifTrue: [^align := #center].
	align := value
%
method: R4PLayout
background
	^background
%
method: R4PLayout
background: aSymbol
	background := aSymbol
%
method: R4PLayout
border
	^border
%
method: R4PLayout
border: anObject

	border := anObject asLayoutArray
%
method: R4PLayout
borderBottom

	^self border at: 3
%
method: R4PLayout
borderBottom: anInteger

	self border at: 3 put: anInteger.
%
method: R4PLayout
borderLeft

	^self border at: 4
%
method: R4PLayout
borderLeft: anInteger

	self border at: 4 put: anInteger.
%
method: R4PLayout
borderRight

	^self border at: 2
%
method: R4PLayout
borderRight: anInteger

	self border at: 2 put: anInteger.
%
method: R4PLayout
borderTop

	^self border at: 1
%
method: R4PLayout
borderTop: anInteger

	self border at: 1 put: anInteger.
%
method: R4PLayout
boxHeightUsed
	"layoutHeightOriginal and then reduced at each page break"

	^boxHeightUsed
%
method: R4PLayout
boxHeightUsed: anObject
	boxHeightUsed := anObject
%
method: R4PLayout
boxHeightUsedAt: anIndex

	^self boxHeightUsed at: anIndex ifAbsent: [0]
%
method: R4PLayout
boxHeightUsedHasKey: anIndex

	^self boxHeightUsed size >= anIndex
%
method: R4PLayout
builder

	^self parent builder
%
method: R4PLayout
built
	"true if all the output for this layout has been created. Needed to recalculate a layout height after a page break, 
	since borders are redrawn on each new page. "

	^built
%
method: R4PLayout
built: anObject
	built := anObject
%
method: R4PLayout
cellPadding
	"on table and row"

	^nil
%
method: R4PLayout
cellSpacing
	"on table and row"

	^nil
%
method: R4PLayout
exclude
	^exclude
%
method: R4PLayout
exclude: anObject
	exclude := anObject
%
method: R4PLayout
extent: aPoint
	
	self fixedBottom: self fixedTop + aPoint y.
	self fixedRight: self fixedLeft + aPoint x.
%
method: R4PLayout
fitOnPage

	^fitOnPage == true
%
method: R4PLayout
fitOnPage: anObject

	fitOnPage := anObject
%
method: R4PLayout
fixedBottom

	^self fixedBox at: 3
%
method: R4PLayout
fixedBottom: aNumber

	self fixedBox at: 3 put: aNumber
%
method: R4PLayout
fixedBox
	^fixedBox
%
method: R4PLayout
fixedBox: anObject
	fixedBox := anObject copy
%
method: R4PLayout
fixedHeight

	^self fixedBottom - (self fixedTop ifNil: [0])
%
method: R4PLayout
fixedLeft

	^self fixedBox at: 4
%
method: R4PLayout
fixedLeft: aNumber

	self fixedBox at: 4 put: aNumber
%
method: R4PLayout
fixedRight

	^self fixedBox at: 2
%
method: R4PLayout
fixedRight: aNumber

	self fixedBox at: 2 put: aNumber
%
method: R4PLayout
fixedTop

	^self fixedBox at: 1
%
method: R4PLayout
fixedTop: aNumber

	self fixedBox at: 1 put: aNumber
%
method: R4PLayout
flip
	"Implemented on PRwText and PRwString"
	
	^false
%
method: R4PLayout
font
	^font
%
method: R4PLayout
font: anObject
	font := anObject asSymbol
%
method: R4PLayout
fontAscender
	^self pdfFont ascenderInTextSpace * self layoutFontSize
%
method: R4PLayout
fontBoxHeight
	^self pdfFont fontBBoxInTextSpace height * self layoutFontSize
%
method: R4PLayout
fontBoxWidth
	^self pdfFont fontBBoxInTextSpace width * self layoutFontSize
%
method: R4PLayout
fontCapHeight
	| ascender |
	ascender := self pdfFont capHeightInTextSpace.
	ascender = 0 ifTrue: [
		ascender := self pdfFont ascenderInTextSpace].
	^ascender * self layoutFontSize
%
method: R4PLayout
fontDescender
	"descender is a negative value"

	^self pdfFont descenderInTextSpace negated * self layoutFontSize
%
method: R4PLayout
fontHeight
	"fonts like ZapfDingbats have no capHeight, ascenders or descenders"

	self pdfFont ascender = 0 ifTrue: [
		^self fontBoxHeight].

	^self fontCapHeight + self fontDescender.
%
method: R4PLayout
fontSize
	^fontSize
%
method: R4PLayout
fontSize: anObject
	fontSize := anObject
%
method: R4PLayout
foreground
	^foreground
%
method: R4PLayout
foreground: aSymbol
	foreground := aSymbol
%
method: R4PLayout
height: aNumber

	self fixedBottom: self fixedTop + aNumber.
%
method: R4PLayout
include
	^include
%
method: R4PLayout
include: anObject
	include := anObject
%
method: R4PLayout
makeFitOnPage

	self fitOnPage: true
%
method: R4PLayout
margin
	^margin
%
method: R4PLayout
margin: anObject

	margin := anObject asLayoutArray copy
%
method: R4PLayout
marginBottom 

	^self margin at: 3
%
method: R4PLayout
marginBottom: anInteger

	self margin at: 3 put: anInteger.
%
method: R4PLayout
marginLeft 

	^self margin  at: 4
%
method: R4PLayout
marginLeft: anInteger

	self margin  at: 4 put: anInteger.
%
method: R4PLayout
marginRight 

	^self margin at: 2
%
method: R4PLayout
marginRight: anInteger

	self margin at: 2 put: anInteger.
%
method: R4PLayout
marginTop

	^self margin at: 1
%
method: R4PLayout
marginTop: aNumber

	self margin at: 1 put: aNumber.
%
method: R4PLayout
newPageBlock

	^newPageBlock
%
method: R4PLayout
newPageBlock: anObject

	newPageBlock := anObject
%
method: R4PLayout
noWrap

	self noWrapSet: true
%
method: R4PLayout
noWrapSet
	"use #noWrap to #noWrapSet: true"
	
	^noWrapSet
%
method: R4PLayout
noWrapSet: anObject
	noWrapSet := anObject
%
method: R4PLayout
origin

	^self fixedLeft @ self fixedTop
%
method: R4PLayout
origin: aPoint
	
	self fixedTop: aPoint y.
	self fixedLeft: aPoint x.

	self fixedRight ~= 0 ifTrue: [
		self fixedRight: self fixedRight + aPoint x].

	self fixedBottom ~= 0 ifTrue: [
		self fixedBottom: self fixedBottom + aPoint y].
%
method: R4PLayout
outputOnNewPage

	^self newPageBlock == true
%
method: R4PLayout
padding
	^padding
%
method: R4PLayout
padding: anObject

	padding := anObject asLayoutArray copy
%
method: R4PLayout
paddingBottom: anInteger

	self padding at: 3 put: anInteger
%
method: R4PLayout
paddingLeft: anInteger

	self padding at: 4 put: anInteger
%
method: R4PLayout
paddingRight: anInteger

	self padding at: 2 put: anInteger
%
method: R4PLayout
paddingTop: anInteger

	self padding at: 1 put: anInteger
%
method: R4PLayout
parent
	^parent
%
method: R4PLayout
parent: anObject
	parent := anObject
%
method: R4PLayout
pdfFont

	^((PDFtalk at: #Fonts) at: #Font) fontAt: self layoutFont
%
method: R4PLayout
sections
	^sections
%
method: R4PLayout
sections: anObject
	sections := anObject
%
method: R4PLayout
sectionsAdd: aSection

	self sections add: aSection
%
method: R4PLayout
spacing
	^spacing
%
method: R4PLayout
spacing: anObject
	spacing := anObject
%
method: R4PLayout
truncated
	^truncated
%
method: R4PLayout
truncated: anObject
	truncated := anObject
%
method: R4PLayout
verticalAlign
	^verticalAlign
%
method: R4PLayout
verticalAlign: aSymbol
	verticalAlign := aSymbol
%
method: R4PLayout
width: aNumber

	self fixedRight: self fixedLeft + aNumber.
%
method: R4PLayout
widthPercent
	^widthPercent
%
method: R4PLayout
widthPercent: anObject
	widthPercent := anObject
%
category: 'accessing - keywords'
method: R4PLayout
bottom

	self verticalAlign: #bottom
%
method: R4PLayout
center

	self align: #center
%
method: R4PLayout
centre

	self align: #center
%
method: R4PLayout
gray
	"foreground color on text"

	self background: #gray
%
method: R4PLayout
grey
	"foreground color on text"

	self gray
%
method: R4PLayout
left

	self align: #left
%
method: R4PLayout
middle

	self verticalAlign: #middle
%
method: R4PLayout
red
	"foreground color on text"

	self background: #red
%
method: R4PLayout
right

	self align: #right
%
method: R4PLayout
top

	self verticalAlign: #top
%
category: 'actions'
method: R4PLayout
buildNewPageParent: aBuilder
	"Sent when any nested section triggers a page break; used by tables with repeating headers, 
	vs. buildNextPageParent: which is sent only ALL bage break, either for new pages or rendering a previously created output pages"

	self parent buildNewPageParent: aBuilder.
%
method: R4PLayout
buildNextPageParent: aBuilder
	
	self buildNextPageParent: aBuilder do: nil
%
method: R4PLayout
buildNextPageParent: aBuilder do: aBlock
	"triggered on a page break; parent layouts may set top spacing or render borders so we need to trigger that here.
	#setLayout: will increment current Y.  Tables will return to previous layouts, so set the current Y back.
	aBlock is used by sections that require updated Y from the parent, like a cell in a row"


	self parent buildNextPageParent: aBuilder do: [
		| previousY | 
		previousY := self  layoutTop. 
		aBuilder setLayout: self.
		aBuilder buildBackground: self.
		aBuilder buildBorder: self.
		aBlock notNil ifTrue: [aBlock value].
		self layoutTop: previousY].
%
method: R4PLayout
setBoxBottomLimit: aNumber

	self boxBottomLimit: aNumber - self marginBottom.
%
method: R4PLayout
setBuiltToFalse
	"for repeating headers"

	self built: false.
	self sections do: [:each | each setBuiltToFalse]
%
method: R4PLayout
stringWidthOf: aString

	^self pdfFont 
			stringWidthOf: aString 
			at: self layoutFontSize
%
category: 'actions - add'
method: R4PLayout
break

	self cr
%
method: R4PLayout
bullet

	| bullet | 

	bullet := R4PBullet newForParent: self.
	self sectionsAdd: bullet.
	^bullet
%
method: R4PLayout
bullet: aBlock

	aBlock value: self bullet
%
method: R4PLayout
cr

	self text string cr
%
method: R4PLayout
horizontalLine
	"if not a fixed layout, assume a horizontal line across the parent layout's output width"

	^self line marginTop: 4; marginBottom: 4; yourself
%
method: R4PLayout
image

	| image | 

	image := R4PImage newForParent: self.
	self sectionsAdd: image.
	^image
%
method: R4PLayout
image: aBlock

	aBlock value: self image
%
method: R4PLayout
line

	| line | 

	line := R4PLine newForParent: self.
	self sectionsAdd: line.
	^line
%
method: R4PLayout
line: aBlock

	aBlock value: self line
%
method: R4PLayout
section

	| section | 

	section := R4PSection newForParent: self.
	self sectionsAdd: section.
	^section
%
method: R4PLayout
section: aBlock

	aBlock value: self section
%
method: R4PLayout
string: aString

	^self text string: aString; yourself
%
method: R4PLayout
table

	| table | 

	table := R4PTable newForParent: self.
	self sectionsAdd: table.
	^table
%
method: R4PLayout
table: aBlock

	aBlock value: self table
%
method: R4PLayout
text

	| text | 

	text := R4PText newForParent: self.
	self sectionsAdd: text.
	^text
%
method: R4PLayout
text: aBlock

	aBlock value: self text
%
category: 'calculate'
method: R4PLayout
boxHeightRemainingAt: anIndex
	"Spacing top and spacing are added at each page break, so remove the value here ... in effect, the text size has grown by the padding. 
	Add zeros to the boxHeightUsed history for cases where this layout begins on pages > 1"

	| total | 

	total := self boxHeight.
	anIndex = 1 ifTrue: [^total].

	(self boxHeightUsed isEmpty and: [self built not]) ifTrue: [
		anIndex - 1 timesRepeat: [
			self boxHeightUsed: (self boxHeightUsed copyWith: 0)]].

	1 to: anIndex - 1 do: [:index | 
		total := total - (self boxHeightUsed 
								at: index 
								ifAbsent: [self error: 'Invalid history of box height used before: ', anIndex printString]).
		total := total + self spacingTop + self spacingBottom].

	^total
%
method: R4PLayout
calculateLayout

	self sections do: [:each | each calculateLayout].
%
category: 'initialize-release'
method: R4PLayout
initialize

	super initialize.
	self initializeSections.

	align := nil.
	border := Array with: 0 with: 0 with: 0 with: 0.
	built := false.
	exclude := nil.
	fixedBox := Array with: 0 with: 0 with: 0 with: 0.
	font := nil.
	fontSize := nil.
	fitOnPage := false.
	include := nil.
	layoutTop := nil.
	boxBottomLimit := nil.
	boxHeightUsed := Array new.
	margin := Array with: 0 with: 0 with: 0 with: 0.
	noWrapSet := nil.
	newPageBlock := [false].
	padding := Array with: 0 with: 0 with: 0 with: 0.
	parent := nil.
	spacing := nil.
	truncated := nil.
	verticalAlign := nil.
	widthPercent := nil.

	^self
%
method: R4PLayout
initializeSections

	sections := OrderedCollection new.
%
category: 'layout'
method: R4PLayout
basicLayoutTop

	^layoutTop
%
method: R4PLayout
boxBottom

	self isFixedLayout ifTrue: [^self fixedBottom].

	^(self layoutBottom min: self boxBottomLimit) - self marginBottom
"
	^(self layoutTop 
		+ self spacingTop 
		+ self nestedLayoutHeight 
		+ self paddingBottom) min: (self boxBottomLimit - self spacingBottom)
"
%
method: R4PLayout
boxBottomAt: aPageNumber

	| bottom | 

	self isFixedLayout ifTrue: [^self fixedBottom].
	bottom := self layoutBottom - self marginBottom.
	bottom := bottom min: self boxBottomLimit.
	bottom := bottom min: (self boxTop + (self nestedLayoutHeight) + self spacingBottom).

	^bottom
%
method: R4PLayout
boxBottomFromRemaingAt: aPageNumber

	^self boxTop 
		+ (self boxHeightRemainingAt: aPageNumber)
%
method: R4PLayout
boxBottomLimit
	^boxBottomLimit
%
method: R4PLayout
boxBottomLimit: aNumber
	boxBottomLimit := aNumber
%
method: R4PLayout
boxHeight

	^self layoutHeight
		- self marginTop
		- self marginBottom
%
method: R4PLayout
boxHeightAt: aPageNumber

	^(self boxBottomAt: aPageNumber)
		- self boxTop
%
method: R4PLayout
boxLeft

	self isFixedLayout ifTrue: [^self fixedLeft].

	^self layoutLeft 
		+ self marginLeft
%
method: R4PLayout
boxRight

	self isFixedLayout ifTrue: [^self fixedRight].

	^self layoutRight 
		- self marginRight
%
method: R4PLayout
boxTop

	self isFixedLayout ifTrue: [^self fixedTop].

	^(self layoutTop ifNil: [0]) + self marginTop
%
method: R4PLayout
boxWidth

	^self boxRight - self boxLeft
%
method: R4PLayout
cellPaddingBottom: aNumber

	^self cellPadding at: 3 put: aNumber
%
method: R4PLayout
cellPaddingLeft: aNumber

	^self cellPadding at: 4 put: aNumber
%
method: R4PLayout
cellPaddingRight: aNumber

	^self cellPadding at: 2 put: aNumber
%
method: R4PLayout
cellPaddingTop: aNumber

	^self cellPadding at: 1 put: aNumber
%
method: R4PLayout
cellSpacingBottom: aNumber

	^self cellSpacing at: 3 put: aNumber
%
method: R4PLayout
cellSpacingLeft: aNumber

	^self cellSpacing at: 4 put: aNumber
%
method: R4PLayout
cellSpacingRight: aNumber

	^self cellSpacing at: 2 put: aNumber
%
method: R4PLayout
cellSpacingTop: aNumber

	^self cellSpacing at: 1 put: aNumber
%
method: R4PLayout
contentBottom

	self isFixedLayout ifTrue: [^self fixedBottom - self paddingBottom].

	^self contentTop 
		+ self contentHeight
%
method: R4PLayout
contentHeight

	self isFixedLayout ifTrue: [^self fixedHeight].
	^self nestedLayoutHeight
%
method: R4PLayout
contentLeft

	self isFixedLayout ifTrue: [
		^self fixedLeft + self paddingLeft].

	self parent isNil ifTrue: [
		^self spacingLeft].
	
	^self parent contentLeft 
		+ self spacingLeft
%
method: R4PLayout
contentRight

	self isFixedLayout ifTrue: [^self fixedRight - self paddingRight].

	^self parent contentRight 
		- self spacingRight
%
method: R4PLayout
contentTop

	self isFixedLayout ifTrue: [^self fixedTop + self paddingTop].
	"self isNestedFixedLayout ifTrue: [^self fixedContentTop]."

	^(self layoutTop ifNil: [0])
		+ self spacingTop
%
method: R4PLayout
contentTopForBottomAlign: aLayout

	^self contentBottom - aLayout layoutHeight
%
method: R4PLayout
contentTopForMiddleAlign: aLayout

	^self contentTop 
			+ (self layoutHeight / 2.0) 
			- (aLayout layoutHeight / 2.0)
%
method: R4PLayout
contentWidth

	self isFixedLayout ifTrue: [
		^self fixedWidth 
			- self paddingLeft 
			- self paddingRight].

	^self parent contentWidth 
		- self spacingLeft
		- self spacingRight
%
method: R4PLayout
fixedContentTop

	self isFixedLayout ifTrue: [^self fixedTop].
	^self parent fixedContentTop 
		+ self marginTop
		+ self paddingTop
%
method: R4PLayout
fixedWidth

	^self fixedRight - self fixedLeft
%
method: R4PLayout
forceNewPage

	self newPageBlock: [true]
%
method: R4PLayout
initialStringWidth
	"prior to word wrap"

	| stringWidth | 

	stringWidth := 0.
	self sections do: [:each | 
		stringWidth := stringWidth max: (
			each initialStringWidth
			+ each spacingLeft 
			+ each spacingRight)].
	^stringWidth
%
method: R4PLayout
layoutAlign

	^self align ifNil: [self parent layoutAlign]
%
method: R4PLayout
layoutBackground

	^self background ifNil: [self parent layoutBackground]
%
method: R4PLayout
layoutBottom

	self isFixedLayout ifTrue: [^self fixedBottom].

	^self layoutTop
		+ self layoutHeight
%
method: R4PLayout
layoutFitOnPage

	self fitOnPage ifTrue: [^true].
	^self parent layoutFitOnPage
%
method: R4PLayout
layoutFont

	^self font ifNil: [self parent layoutFont]
%
method: R4PLayout
layoutFontSize

	^self fontSize ifNil: [self parent layoutFontSize]
%
method: R4PLayout
layoutForeground

	^self foreground ifNil: [self parent layoutForeground]
%
method: R4PLayout
layoutHeight
	"row & cell don't used fixed layout height"

	^self isFixedLayout 
		ifTrue: [self fixedBottom - self fixedTop]
		ifFalse: [self layoutHeightCalculated]
%
method: R4PLayout
layoutHeightCalculated

	^self spacingTop 
		+ self nestedLayoutHeight 
		+ self spacingBottom
%
method: R4PLayout
layoutHeightCalculatedNotBuilt

	^self spacingTop 
		+ self nestedLayoutHeightNotBuilt 
		+ self spacingBottom
%
method: R4PLayout
layoutLeft

	self isFixedLayout ifTrue: [
		^self fixedLeft].
	
	^self parent contentLeft
%
method: R4PLayout
layoutNoWrap

	^self noWrapSet ifNil: [self parent layoutNoWrap]
%
method: R4PLayout
layoutRight

	self isFixedLayout ifTrue: [^self fixedRight].

	^self parent contentRight
%
method: R4PLayout
layoutSections

	^self sections reject: [:each | each skipRendering]
%
method: R4PLayout
layoutTop

	self isFixedLayout ifTrue: [^self fixedTop].
	^layoutTop
%
method: R4PLayout
layoutTop: aNumber
	
	layoutTop := aNumber
%
method: R4PLayout
layoutTruncated

	^self truncated ifNil: [self parent layoutTruncated]
%
method: R4PLayout
layoutVerticalAlign

	^self verticalAlign ifNil: [self parent layoutVerticalAlign]
%
method: R4PLayout
layoutWidth

	^self layoutRight - self layoutLeft
%
method: R4PLayout
nestedLayoutHeight

	| total | 

	total := 0.
	self layoutSections do: [:each | 
		total := total + each layoutHeight].
	^total
%
method: R4PLayout
paddingBottom

	^self padding at: 3
%
method: R4PLayout
paddingLeft

	^self padding at: 4
%
method: R4PLayout
paddingRight

	^self padding at: 2
%
method: R4PLayout
paddingTop

	^self padding at: 1
%
method: R4PLayout
pageHeight

	^self parent pageHeight
%
method: R4PLayout
pageLayoutHeight

	^self parent pageLayoutHeight
%
method: R4PLayout
pageNumberPattern

	^self parent pageNumberPattern
%
method: R4PLayout
pageWidth

	^self parent pageWidth
%
method: R4PLayout
spacingBottom

	^self marginBottom + self paddingBottom
%
method: R4PLayout
spacingLeft

	^self marginLeft + self paddingLeft
%
method: R4PLayout
spacingRight

	^self marginRight + self paddingRight
%
method: R4PLayout
spacingTop

	^self marginTop + self paddingTop
%
method: R4PLayout
stringWidth

	| stringWidth | 

	stringWidth := 0.
	self sections do: [:each | 
		stringWidth := stringWidth max: each stringWidth].
	^stringWidth
%
method: R4PLayout
textMatrixFlipScaleX: aScaleX scaleY: aScaleY positionX: aPositionX positionY: aPositionY

	^(Array new: 6)	
		at: 1 put: aScaleX negated; 
		at: 2 put: 0; 
		at: 3 put: 0;
		at: 4 put: aScaleY; 
		at: 5 put: aPositionX + self stringWidth; 
		at: 6 put: aPositionY - self fontHeight;
		yourself
%
category: 'output'
method: R4PLayout
outputBackgroundAt: aPageNumber

	^R4POutputBackground newForLayout: self pageNumber: aPageNumber
%
method: R4PLayout
outputBorderBottomAt: aPageNumber

	| bottom | 

	self borderBottom = 0 ifTrue: [^nil].
	bottom := self boxBottomAt: aPageNumber. 

	^R4POutputLine 
		newForStartPoint: (self outputBoxLeft @  bottom) 
		endPoint: (self outputBoxRight @  bottom) 
		width: self borderBottom
%
method: R4PLayout
outputBorderLeftAt: aPageNumber

	self borderLeft = 0 ifTrue: [^nil].

	^R4POutputLine 
		newForStartPoint: (self boxLeft @  (self boxBottomAt: aPageNumber)) 
		endPoint: (self boxLeft @  self boxTop) 
		width: self borderLeft
%
method: R4PLayout
outputBorderRightAt: aPageNumber

	self borderRight = 0 ifTrue: [^nil].

	^R4POutputLine 
		newForStartPoint: (self boxRight @  self boxTop) 
		endPoint: (self boxRight @  (self boxBottomAt: aPageNumber)) 
		width: self borderRight
%
method: R4PLayout
outputBorderTop

	self borderTop = 0 ifTrue: [^nil].

	^R4POutputLine 
		newForStartPoint: self outputBoxLeft @ self boxTop
		endPoint: self outputBoxRight @ self boxTop 
		width: self borderTop
%
method: R4PLayout
outputBoxLeft

	^self boxLeft - (self borderLeft / 2.0)
%
method: R4PLayout
outputBoxRight

	^self boxRight + (self borderRight / 2.0)
%
method: R4PLayout
truncatedImage

	^R4PImage newTruncatedOutputForParent: self
%
category: 'printing'
method: R4PLayout
displayBox

	^[ | stream | 
	stream := WriteStream on: self stringClass new.
	stream nextPutAll: '#('; 
		nextPutAll: self boxTop printString; space; 
		nextPutAll: self boxRight printString; space; 
		nextPutAll: self displayBoxBottom; space; 
		nextPutAll: self boxLeft printString; nextPut: $); 
		yourself.
	stream contents]
		on: self errorClass
		do: [:ex | '#(? ? ? ?)']
%
method: R4PLayout
displayBoxBottom

	^[self boxBottom printString]	
		on: self errorClass
		do: [:ex | '<?>']
%
method: R4PLayout
displayLayout

	| stream | 

	stream := WriteStream on: self stringClass new.
	stream nextPutAll: '#('; 
		nextPutAll: self layoutTop printString; space; 
		nextPutAll: self displayLayoutRight; space; 
		nextPutAll: self displayLayoutBottom; space; 
		nextPutAll: self layoutLeft printString; nextPut: $); 
		yourself.
	^stream contents
%
method: R4PLayout
displayLayoutBottom

	^[self layoutBottom printString]	
		on: self errorClass
		do: [:ex | '<?>']
%
method: R4PLayout
displayLayoutExtent

	^[self displayLayoutWidth, ' @ ', self displayLayoutHeight]
		on: self errorClass
		do: [:ex | '<?@?>']
%
method: R4PLayout
displayLayoutHeight
	"can raise an exception if we try to get this value before the layout is calculated"

	^[self layoutHeight printString]
		on: self errorClass
		do: [:ex | '<?>']
%
method: R4PLayout
displayLayoutRight

	^[self layoutRight printString]	
		on: self errorClass
		do: [:ex | '<?>']
%
method: R4PLayout
displayLayoutWidth

	^self layoutWidth printString
%
method: R4PLayout
printLayout

	| stream |

	stream := WriteStream on: self stringClass new.
	self printLayoutOn: stream.
	^stream contents
%
method: R4PLayout
printLayoutOn: aStream

	self isFixedLayout ifTrue: [
		^aStream cr; tab; nextPutAll: 'fixed: '; nextPutAll: self fixedBox printString; yourself].

	aStream 
		cr; tab; nextPutAll: 'layout: '; nextPutAll: self displayLayout; 
		cr; tab; nextPutAll: 'extent: '; nextPutAll: self displayLayoutExtent; 
		cr; tab; nextPutAll: 'margin: '; nextPutAll: self margin printString; 
		cr; tab; nextPutAll: 'padding: '; nextPutAll: self padding printString; 
		cr; tab; nextPutAll: 'border: '; nextPutAll: self border printString; 
		cr; tab; nextPutAll: 'box: '; nextPutAll: self displayBox; 
		cr; tab; nextPutAll: 'boxBottomLimit: '; nextPutAll: self boxBottomLimit printString; 
		cr; tab; nextPutAll: 'sections: '; nextPutAll: self sections size printString; 
		cr; tab; nextPutAll: 'built: '; nextPutAll: self built printString; 
		yourself
%
method: R4PLayout
printOn: aStream
	"  #printLayoutOn: uses computed values, which depend on a clean layout structure.
	During development that's not always the case, and a VW image will lock up if a breakpoint
	is put in any of those computed values. #printLayout uses #printLayoutOn: without #printOn: "

	super printOn: aStream.
	"aStream space.
	self printLayoutOn: aStream."
%
category: 'testing'
method: R4PLayout
canFitOnPageAt: aNumber limit: aMaximumY

	self layoutFitOnPage ifFalse: [^true]. "default"

	^((aNumber + self layoutHeight) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
method: R4PLayout
containsTruncatedText

	^false
%
method: R4PLayout
excludeOnPage: aPageNumber

	self exclude isNil ifTrue: [^false].
	^self exclude value: aPageNumber
%
method: R4PLayout
hasBackground

	^self layoutBackground notNil
%
method: R4PLayout
hasBorder

	^self border ~= #(0 0 0 0)
%
method: R4PLayout
hasDifferentFont

	^self layoutFont ~= self parent layoutFont or: [
		self parent hasDifferentFont]
%
method: R4PLayout
hasFixedHeight

	^self fixedHeight ~= 0
%
method: R4PLayout
includeOnPage: aPageNumber

	self include isNil ifTrue: [^true].
	^self include value: aPageNumber
%
method: R4PLayout
isBottom

	^self layoutVerticalAlign = #bottom
%
method: R4PLayout
isBuilt

	^self built == true
%
method: R4PLayout
isCentered

	^self layoutAlign = #center
%
method: R4PLayout
isFixedLayout

	^self fixedBox ~= #(0 0 0 0)
%
method: R4PLayout
isLeftJustified

	^self layoutAlign = #left
%
method: R4PLayout
isMiddle

	^self layoutVerticalAlign = #middle
%
method: R4PLayout
isNestedFixedLayout

	self isFixedLayout ifTrue: [^true].
	^self parent isNestedFixedLayout
%
method: R4PLayout
isRightJustified

	^self layoutAlign = #right
%
method: R4PLayout
isTop

	^self layoutVerticalAlign = #top
%
method: R4PLayout
skipRendering

	| pageNumber |

	self builder isNil ifTrue: [^false].
	pageNumber := self builder currentPageNumber.
	pageNumber = 0 ifTrue: [^false].

	(self excludeOnPage: pageNumber) ifTrue: [^true].
	(self includeOnPage: pageNumber) ifFalse: [^true].

	^false
%
category: 'accessing'
method: R4PLine
end: aPoint

	self fixedRight: aPoint x.
	self fixedBottom: aPoint y.
%
method: R4PLine
endPoint

	^(self fixedRight) @ (self fixedBottom)
%
method: R4PLine
lineWidth
	^lineWidth
%
method: R4PLine
lineWidth: anObject
	lineWidth := anObject
%
method: R4PLine
start: aPoint

	self origin: aPoint
%
category: 'accessing - keywords'
method: R4PLine
gray
	"background color on other layouts"

	self foreground: #gray
%
method: R4PLine
red
	"background color on other layouts"

	self foreground: #red
%
category: 'actions'
method: R4PLine
buildOutput: aBuilder

	aBuilder buildLine: self
%
category: 'initialize-release'
method: R4PLine
initialize

	super initialize.
	
	lineWidth := 0.5 .
%
category: 'output'
method: R4PLine
fixedOutputLine

	^R4POutputLine 
		newForStartPoint: self origin 
		endPoint: self endPoint 
		width: self lineWidth
		color: self layoutForeground
%
method: R4PLine
outputLine

	^R4POutputLine 
		newForStartPoint:  (self contentLeft @ self contentTop) 
		endPoint: (self contentRight @ self contentTop) 
		width: self lineWidth
		color: self layoutForeground
%
category: 'testing'
method: R4PLine
canBuildAt: aNumber limit: aMaximumY

	^((aNumber + self layoutHeight) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
category: 'accessing'
classmethod: R4PObject
errorClass

	^Error
%
classmethod: R4PObject
fontTable
	" 	#( ... #(normal, bold, italic, bold + italic) ... )  "

	^#(
		(Courier #'Courier-Bold' #'Courier-Oblique' #'Courier-BoldOblique')
		(Helvetica #'Helvetica-Bold' #'Helvetica-Oblique' #'Helvetica-BoldOblique')
		(Symbol Symbol Symbol Symbol)
		(#'Times-Roman' #'Times-Bold' #'Times-BoldItalic' #'Times-Italic')
		(ZapfDingbats ZapfDingbats ZapfDingbats ZapfDingbats)
	)
%
classmethod: R4PObject
fontTableBoldFor: aSymbol

	^(self fontTableRowFor: aSymbol) at: 2
%
classmethod: R4PObject
fontTableBoldItalicFor: aSymbol

	^(self fontTableRowFor: aSymbol) at: 4
%
classmethod: R4PObject
fontTableItalicFor: aSymbol

	^(self fontTableRowFor: aSymbol) at: 3
%
classmethod: R4PObject
fontTableNormalFor: aSymbol

	^(self fontTableRowFor: aSymbol) at: 1
%
classmethod: R4PObject
fontTableRowFor: aSymbol

	^self fontTable 
		detect: [:each | each includes: aSymbol] 
		ifNone: [self error: 'No entry in the font table found for ', aSymbol printString. nil]
%
classmethod: R4PObject
stringClass 

	^String
%
category: 'instance creation'
classmethod: R4PObject
new
	"Answer a newly created and initialized instance."

	^super new initialize
%
category: 'accessing'
method: R4PObject
properties
	"to allow for extensions that need state"

	^properties
%
method: R4PObject
properties: anObject

	properties := anObject
%
category: 'accessing - keywords'
method: R4PObject
bold

	self font: (self class fontTableBoldFor: self layoutFont)
%
method: R4PObject
boldItalic

	self font: (self class fontTableBoldItalicFor: self layoutFont)
%
method: R4PObject
errorClass
	
	^self class errorClass
%
method: R4PObject
italic

	self font: (self class fontTableItalicFor: self layoutFont)
%
method: R4PObject
large

	self fontSize: 12
%
method: R4PObject
medium

	self fontSize: 10
%
method: R4PObject
normal

	self font: (self class fontTableNormalFor: self layoutFont)
%
method: R4PObject
small

	self fontSize: 8
%
method: R4PObject
stringClass
	
	^self class stringClass
%
method: R4PObject
veryLarge

	self fontSize: 14
%
method: R4PObject
verySmall

	self fontSize: 6
%
category: 'actions'
method: R4PObject
reportError: aString

	"^self error: aString"
	^R4PError raiseErrorString: aString
%
category: 'fonts'
method: R4PObject
courier

	self font: #Courier
%
method: R4PObject
courierBold

	self font: #'Courier-Bold'
%
method: R4PObject
courierBoldOblique

	self font: #'Courier-BoldOblique'
%
method: R4PObject
courierOblique

	self font: #'Courier-Oblique'
%
method: R4PObject
font: aSymbol
	
	self subclassResponsibility
%
method: R4PObject
helvetica

	self font: #Helvetica
%
method: R4PObject
helveticaBold

	self font: #'Helvetica-Bold'
%
method: R4PObject
helveticaBoldOblique

	self font: #'Helvetica-BoldOblique'
%
method: R4PObject
helveticaOblique

	self font: #'Helvetica-Oblique'
%
method: R4PObject
symbol

	self font: #Symbol
%
method: R4PObject
timesBold

	self font: #'Times-Bold'
%
method: R4PObject
timesBoldItalic

	self font: #'Times-BoldItalic'
%
method: R4PObject
timesItalic

	self font: #'Times-Italic'
%
method: R4PObject
timesRoman

	self font: #'Times-Roman'
%
method: R4PObject
zapfDingbats

	self font: #ZapfDingbats
%
category: 'initialize-release'
method: R4PObject
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Replace this comment with the appropriate initialization code *** "
	^self
%
category: 'layout'
method: R4PObject
fontSize: anInteger

	^self subclassResponsibility
%
method: R4PObject
layoutFont

	^self subclassResponsibility
%
category: 'accessing'
method: R4POutput
color
	^color
%
method: R4POutput
color: anObject
	color := anObject
%
method: R4POutput
colorName
	^colorName
%
method: R4POutput
colorName: anObject
	colorName := anObject
%
method: R4POutput
colorNameAliasTable

	^#(
		( grey1 gray1)
		( grey2 gray2)
		( grey3 gray3)
		( grey4 gray4)
		( grey5 gray5)
		( grey6 gray6)
		( grey7 gray7)
		( grey8 gray8)
		( grey9 gray9)
		( lightGray gray2)
		( gray gray3)
		( darkGray gray6)
		( lightGrey gray2)
		( grey gray3)
		( darkGrey gray6)
	)
%
method: R4POutput
colorNameTable
	"#(... #(name, (R G B)) ... "

	^#(
		( gray1 ( 0.9 0.9 0.9 ) )
		( gray2 ( 0.85 0.85 0.85 ) )
		( gray3 ( 0.8 0.8 0.8 ) )
		( gray4 ( 0.7 0.7 0.7 ) )
		( gray5 ( 0.6 0.6 0.6 ) )
		( gray6 ( 0.5 0.5 0.5 ) )
		( gray7 ( 0.4 0.4 0.4 ) )
		( gray8 ( 0.3 0.3 0.3 ) )
		( gray9 ( 0.2 0.2 0.2 ) )
		( red ( 0.75 0 0 ) )
		( lightred ( 1 0 0 ) )
		( darkred ( 0.5 0 0 ) )
		( white ( 1 1 1 ) )
	)
%
category: 'actions'
method: R4POutput
getColorValueFrom: aString
	" 255 = 1 "

	| high low value |
 
	high := (('0123456789abcdef' indexOf: aString first asLowercase) - 1) * 16.
	low := ('0123456789abcdef' indexOf: aString last asLowercase) - 1.
	value := high + low / 255.0 .
	^value
%
method: R4POutput
outputBlack

	^ColorValue black
%
method: R4POutput
renderOutput: aRenderer

	self subclassResponsibility
%
method: R4POutput
replaceString: aString with: aNewString

	^self
%
method: R4POutput
setColor: aString

	self colorName: aString.
	self setOutputColor.
%
method: R4POutput
setOutputColor

	| nameArray alias colorArray | 
	
	self setOutputColorFromArray ifTrue: [^self].
	nameArray := self colorNameTable detect: [:eachColor | eachColor first sameAs: self colorName] ifNone: [nil].
	nameArray isNil ifTrue: [
		alias := self colorNameAliasTable detect: [:eachAlias | eachAlias first sameAs: self colorName] ifNone: [#(#'<none>')].
		nameArray := self colorNameTable detect: [:eachColor | eachColor first sameAs: alias last] ifNone: [nil]].
	nameArray isNil ifTrue: [
		self setOutputColorFromSymbol ifTrue: [^self]].		
	nameArray isNil ifTrue: [
		^self reportError: 'Unknown output color: ', self colorName printString].
	colorArray := nameArray last.
	
	self color: (
		ColorValue 
			red: (colorArray at: 1) 
			green: (colorArray at: 2) 
			blue:  (colorArray at: 3))
%
method: R4POutput
setOutputColorFromArray

	self colorName isNil ifTrue: [^false].
	self colorName isString ifTrue: [^false].

	self colorName size = 3 ifFalse: [
		^self reportError: 'Invalid output color array. Expected three values: ', self colorName printString].

	self color: (
		ColorValue 
			red: (self colorName at: 1) 
			green: (self colorName at: 2) 
			blue:  (self colorName at: 3)).

	^true
%
method: R4POutput
setOutputColorFromSymbol
	" #FFAA22 --> #(255 176 34) "

	| red green blue | 

	self colorName isSymbol ifFalse: [^false].
	self colorName size = 6 ifFalse: [
		^self reportError: 'Invalid output color symbol. Expected hex string like #FFFFFF: ', self colorName printString].

	red := self getColorValueFrom: (self colorName copyFrom: 1 to: 2).
	green := self getColorValueFrom: (self colorName copyFrom: 3 to: 4).
	blue := self getColorValueFrom: (self colorName copyFrom: 5 to: 6).

	self color: (
		ColorValue 
			red: red 
			green: green
			blue:  blue).

	^true
%
category: 'printing'
method: R4POutput
printOutputOn: aStream
	"see subclasses"
%
category: 'instance creation'
classmethod: R4POutputBackground
newForLayout: aLayout pageNumber: aPageNumber

	^self new
		left: aLayout boxLeft; 
		bottom: (aLayout boxBottomAt: aPageNumber); 
		width: aLayout boxWidth; 
		height: (aLayout boxHeightAt: aPageNumber); 
		setColor: aLayout layoutBackground; 
		yourself
%
category: 'accessing'
method: R4POutputBackground
bottom
	^bottom
%
method: R4POutputBackground
bottom: anObject
	bottom := anObject
%
method: R4POutputBackground
height
	^height
%
method: R4POutputBackground
height: anObject
	height := anObject
%
method: R4POutputBackground
left
	^left
%
method: R4POutputBackground
left: anObject
	left := anObject
%
method: R4POutputBackground
right
	^right
%
method: R4POutputBackground
right: anObject
	right := anObject
%
method: R4POutputBackground
top
	^top
%
method: R4POutputBackground
top: anObject
	top := anObject
%
method: R4POutputBackground
width
	^width
%
method: R4POutputBackground
width: anObject
	width := anObject
%
category: 'actions'
method: R4POutputBackground
renderOutput: aRenderer
	"set the fill color back to black when done"

	aRenderer fillColor: self color.
	aRenderer addRectangleLeft: self left 
				bottom: self bottom -  self height
				width: self width 
				height: self height.
	aRenderer fill.
	aRenderer fillColor: self outputBlack.
%
category: 'initialize-release'
method: R4POutputBackground
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Edit the following to properly initialize instance variables ***"
	top := nil.
	left := nil.
	bottom := nil.
	right := nil.
	color := nil.
	" *** And replace this comment with additional initialization code *** "
	^self
%
category: 'printing'
method: R4POutputBackground
printOutputOn: aStream

	aStream nextPutAll: 'top: '; nextPutAll: self left printString; 
		tab; nextPutAll: 'bottom: '; nextPutAll: self bottom printString; 		
		tab; nextPutAll: 'width: '; nextPutAll: self width printString; 		
		tab; nextPutAll: 'height: '; nextPutAll: self height printString; 		
		tab; nextPutAll: 'color: '; nextPutAll: self color printString
%
category: 'instance creation'
classmethod: R4POutputImage
newForMatrix: aMatrix xImage: anXImage

	^self new
		matrix: aMatrix; 
		xImage: anXImage; 
		yourself
%
category: 'accessing'
method: R4POutputImage
matrix
	^matrix
%
method: R4POutputImage
matrix: anObject
	matrix := anObject
%
method: R4POutputImage
xImage
	^xImage
%
method: R4POutputImage
xImage: anObject
	xImage := anObject
%
category: 'actions'
method: R4POutputImage
renderOutput: aRenderer

	aRenderer isolatedDo: [
		aRenderer concat: self matrix.
		aRenderer paintXObject: self xImage].
%
category: 'initialize-release'
method: R4POutputImage
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Edit the following to properly initialize instance variables ***"
	matrix := nil.
	xImage := nil.
	" *** And replace this comment with additional initialization code *** "
	^self
%
category: 'printing'
method: R4POutputImage
printOutputOn: aStream

	aStream nextPutAll: self matrix printString; nextPutAll: ' [image] '
%
category: 'instance creation'
classmethod: R4POutputLine
newForStartPoint: aStartPoint endPoint: anEndPoint width: aWidth

	^self new
		startPoint: aStartPoint;
		endPoint: anEndPoint; 
		width: aWidth; 
		yourself
%
classmethod: R4POutputLine
newForStartPoint: aStartPoint endPoint: anEndPoint width: aWidth color: aColorName

	^self new
		startPoint: aStartPoint;
		endPoint: anEndPoint; 
		width: aWidth; 
		setColor: aColorName; 
		yourself
%
category: 'accessing'
method: R4POutputLine
endPoint
	^endPoint
%
method: R4POutputLine
endPoint: anObject
	endPoint := anObject
%
method: R4POutputLine
startPoint
	^startPoint
%
method: R4POutputLine
startPoint: anObject
	startPoint := anObject
%
method: R4POutputLine
width
	^width
%
method: R4POutputLine
width: anObject
	width := anObject
%
category: 'actions'
method: R4POutputLine
renderOutput: aRenderer

	aRenderer linewidth: self width.
	aRenderer moveTo: self startPoint.
	aRenderer lineTo: self endPoint.
	self color notNil ifTrue: [aRenderer strokeColor: self color].
	aRenderer stroke.
	self color notNil ifTrue: [aRenderer strokeColor: self outputBlack].
%
method: R4POutputLine
setColor: aColorName

	aColorName notNil ifTrue: [
		super setColor: aColorName].
%
category: 'initialize-release'
method: R4POutputLine
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Edit the following to properly initialize instance variables ***"
	startPoint := nil.
	endPoint := nil.
	width := nil.
	" *** And replace this comment with additional initialization code *** "
	^self
%
category: 'printing'
method: R4POutputLine
printOn: aStream

	super printOn: aStream.

	aStream tab; nextPutAll: self startPoint printString; 
		nextPutAll: ' line: '; nextPutAll: self endPoint printString;
		tab; nextPutAll: self width printString
%
method: R4POutputLine
printOutputOn: aStream

	aStream nextPutAll: self startPoint printString; 
		nextPutAll: ' line: '; nextPutAll: self endPoint printString;
		space; nextPutAll: self width printString
%
category: 'accessing'
method: R4POutputPage
currentY
	^currentY
%
method: R4POutputPage
currentY: anObject
	currentY := anObject
%
method: R4POutputPage
maximumY
	"page height minus footer"

	^maximumY
%
method: R4POutputPage
maximumY: anObject
	maximumY := anObject
%
method: R4POutputPage
minimumY
	"space after header ... set after header is created; needed in case the output page is reused, like when buiding a table with tall cells"

	^minimumY
%
method: R4POutputPage
minimumY: anObject
	minimumY := anObject
%
method: R4POutputPage
nextPage
	^nextPage
%
method: R4POutputPage
nextPage: anObject
	nextPage := anObject
%
method: R4POutputPage
nextPageLast

	self nextPage isNil ifTrue: [^self].
	^self nextPage nextPageLast
%
method: R4POutputPage
nextPageNumber
	"allows for page number reset"

	^nextPageNumber
%
method: R4POutputPage
nextPageNumber: anObject

	nextPageNumber := anObject
%
method: R4POutputPage
nextPageSize

	self nextPage isNil ifTrue: [^0].
	^1 + (self nextPage nextPageSize)
%
method: R4POutputPage
output
	^output
%
method: R4POutputPage
output: anObject
	output := anObject
%
method: R4POutputPage
outputPageIndex

	^outputPageIndex
%
method: R4POutputPage
outputPageIndex: anObject

	outputPageIndex := anObject
%
method: R4POutputPage
outputPagesSize

	^self reportPage outputPages size
%
method: R4POutputPage
pageHeight
	^pageHeight
%
method: R4POutputPage
pageHeight: anObject
	pageHeight := anObject
%
method: R4POutputPage
pageWidth
	^pageWidth
%
method: R4POutputPage
pageWidth: anObject
	pageWidth := anObject
%
method: R4POutputPage
previousPage

	^previousPage
%
method: R4POutputPage
previousPage: anObject

	previousPage := anObject
%
method: R4POutputPage
renderer
	^renderer
%
method: R4POutputPage
renderer: anObject
	renderer := anObject
%
method: R4POutputPage
reportPage

	^reportPage
%
method: R4POutputPage
reportPage: anObject

	reportPage := anObject
%
category: 'actions'
method: R4POutputPage
createNextPageOutput

	| next |

	next := self class new.
	self nextPage: next.
	next previousPage: self.
	^next
%
method: R4POutputPage
outputAdd: anOutput
	"limited output in case of runaway code "

	anOutput isNil ifTrue: [^self].
	self output add: anOutput.

	self output size > 9999 ifTrue: [
		self error: 'maximum number of output pages (9999) exceeded'].
%
method: R4POutputPage
outputBorder: aLayout at: aPageNumber

	self outputAdd: aLayout outputBorderTop.
	self outputAdd: (aLayout outputBorderRightAt: aPageNumber).
	self outputAdd: (aLayout outputBorderBottomAt: aPageNumber).
	self outputAdd: (aLayout outputBorderLeftAt: aPageNumber).
%
method: R4POutputPage
outputErrorMessage: aString
	"add bold text at the top right corner of the page"

	self outputAdd: (
			R4POutputString 
				newForMatrix: #(10 0 0 -10 20 20) 
				font: #'Helvetica-Bold'
				foreground: 'darkred'
				string: aString)
%
method: R4POutputPage
renderContent

	self output do: [:each | 
		each renderOutput: self renderer]
%
method: R4POutputPage
renderOutput: aRenderer
	" an ouptput page just holds the other output objects "

	^self
%
method: R4POutputPage
renderPage

	^PDF Page
		newInBounds:  (0 @ 0 corner: self pageWidth @ self pageHeight negated)
		colorspace: (PDF classAt: #DeviceRGB) new
		render: [:newRenderer |
			self renderer: newRenderer.
			self renderer textRenderingMode: 0.
			self transformCoordinates.
			self renderContent].
%
method: R4POutputPage
replaceString: aString with: aNewString

	self output do: [:each | 
		each replaceString: aString with: aNewString]
%
method: R4POutputPage
resetCurrentY
	
	self currentY: self minimumY
%
method: R4POutputPage
setMinimumY
	
	self minimumY: self currentY
%
method: R4POutputPage
transformCoordinates
	"Matrix method name changed from scale: to scaling: ... load latest release from Cincom Store"

	self renderer concat: ((PDF classAt: #Matrix) scaling: 1 @ -1).
%
category: 'initialize-release'
method: R4POutputPage
initialize

	super initialize.

	nextPage := nil.
	currentY := 0.
	minimumY := 0.
	maximumY := 0.
	output := OrderedCollection new.
	outputPageIndex := 0.

	^self
%
category: 'printing'
method: R4POutputPage
printOn: aStream

	super printOn: aStream.

	aStream nextPutAll: ' Y = '; nextPutAll: self currentY printString; 
		nextPutAll: ' of '; nextPutAll: self maximumY printString; 
		nextPutAll: ' @ '; nextPutAll: self outputPageIndex printString.

	self nextPage notNil ifTrue: [aStream nextPutAll: ' --> nextPage ('; nextPutAll:  self nextPageSize printString, ')']
%
method: R4POutputPage
printOutput
	
	| stream | 

	stream := WriteStream on: self stringClass new.
	self printOutputOn: stream.
	^stream contents
%
method: R4POutputPage
printOutputOn: aStream

	aStream nextPutAll: '---';
		cr; nextPutAll: 'page width: '; nextPutAll: self pageWidth printString; 
		cr; nextPutAll: 'page height: '; nextPutAll: self pageHeight printString;
		cr; nextPutAll: 'maximum Y: '; nextPutAll: self maximumY printString; nextPutAll: ' (page height - footer)'; 
		cr; nextPutAll: 'output parts: '; nextPutAll: self output size printString.

	self nextPage notNil ifTrue: [
		aStream cr; nextPutAll: '''next page'' size: ', self nextPageSize printString].

	self output do: [:each | 
		aStream cr.
		each printOutputOn: aStream]
%
category: 'instance creation'
classmethod: R4POutputString
newForMatrix: aMatrix font: aFont fontSize: aFontSize align: anAlignSymbol originalWidth: anOriginalWidth foreground: aColorName string: aString

	^self new
		matrix: aMatrix;
		font: aFont;  
		fontSize: aFontSize; 
		originalWidth: anOriginalWidth;
		align: anAlignSymbol;
		setColor: aColorName; 
		string: aString; 
		yourself
%
classmethod: R4POutputString
newForMatrix: aMatrix font: aFont foreground: aColorName string: aString

	^self new
		matrix: aMatrix;
		font: aFont;  
		setColor: aColorName; 
		string: aString; 
		yourself
%
category: 'accessing'
method: R4POutputString
align
	^align
%
method: R4POutputString
align: anObject
	align := anObject
%
method: R4POutputString
font
	^font
%
method: R4POutputString
font: anObject
	font := anObject asSymbol
%
method: R4POutputString
fontSize
	^fontSize
%
method: R4POutputString
fontSize: anObject
	fontSize := anObject
%
method: R4POutputString
matrix
	^matrix
%
method: R4POutputString
matrix: anObject
	matrix := anObject
%
method: R4POutputString
originalWidth
	^originalWidth
%
method: R4POutputString
originalWidth: anObject
	originalWidth := anObject
%
method: R4POutputString
pdfFont

	^((PDFtalk at: #Fonts) at: #Font) fontAt: self font
%
method: R4POutputString
string
	^string
%
method: R4POutputString
string: anObject
	string := anObject
%
method: R4POutputString
stringWidth
	"for cases where <page> or <total> are replaced"

	^self pdfFont 
			stringWidthOf: self string 
			at: self fontSize
%
category: 'actions'
method: R4POutputString
adjustPosition
	"required when a <page> or <total> is replaced and the X position of this string would shift.
	replace #originalWidth in case both <page> and <total> are replaced"

	| newWidth delta x | 

	self align = #center ifFalse: [
		self align = #right ifFalse: [
			^self]].

	self originalWidth isNil ifTrue: [^self].
	newWidth := self stringWidth.
	delta := self originalWidth - newWidth.
	x := self matrix at: 5.
	self originalWidth: newWidth. 

	self align = #center ifTrue: [^self matrix at: 5 put: (x + (delta / 2.0))].
	self align = #right ifTrue: [^self matrix at: 5 put: (x + delta)].
%
method: R4POutputString
renderOutput: aRenderer
	"empty strings can be used for positioning, but once the layout Y values are calculated, 
	we don't need to include the empty strings in the PDF"

	self string trimBlanks isEmpty ifTrue: [^self].

	aRenderer textObjectDo: [
		self color notNil ifTrue: [aRenderer fillColor: self color].
		aRenderer setFont: self font size: 1.
		aRenderer textMatrix: self matrix.
		aRenderer showString: self string.
		self color notNil ifTrue: [aRenderer fillColor: self outputBlack]]
%
method: R4POutputString
replaceString: aString with: aNewString

	(self string indexOfSubCollection: aString startingAt: 1) = 0 ifTrue: [^self].

	self string: (self string copyReplaceAll: aString with: aNewString).
	self adjustPosition.
%
method: R4POutputString
setColor: aColorName

	aColorName notNil ifTrue: [
		super setColor: aColorName].
%
category: 'printing'
method: R4POutputString
printOn: aStream

	super printOn: aStream.
	aStream space.
	self printOutputOn: aStream.
%
method: R4POutputString
printOutputOn: aStream

	aStream nextPutAll: self matrix printString; 
		space; nextPutAll: self string asString
%
category: 'accessing'
method: R4PPage
basicPageHeight

	^pageHeight
%
method: R4PPage
footer

	^footer
%
method: R4PPage
footer: aBlock

	aBlock value: self footer
%
method: R4PPage
header

	^header
%
method: R4PPage
header: aBlock

	aBlock value: self header
%
method: R4PPage
nextPageNumber
	"allows for page number reset"

	^nextPageNumber
%
method: R4PPage
nextPageNumber: anObject

	nextPageNumber := anObject
%
method: R4PPage
outputPages

	^outputPages
%
method: R4PPage
outputPages: anObject

	outputPages := anObject
%
method: R4PPage
outputPagesAdd: aPage

	self outputPages add: aPage.
	aPage reportPage: self.
%
method: R4PPage
pageHeight: aNumber

	pageHeight := aNumber
%
method: R4PPage
pageWidth: aNumber

	pageWidth := aNumber
%
method: R4PPage
resetPageNumber

	self nextPageNumber: 1
%
method: R4PPage
watermark
	"answer a fixed size image; added to output first in order to be behind everything else"

	^watermark ifNil: [watermark := R4PImage newForParent: self]
%
method: R4PPage
watermark: aBlock

	aBlock value: self watermark
%
method: R4PPage
watermarkBasic

	^watermark
%
method: R4PPage
watermarkBasic: anObject

	watermark := anObject
%
category: 'actions'
method: R4PPage
buildWatermark: aBuilder

	self watermarkBasic notNil ifTrue: [
		self watermark buildOutput: aBuilder].
%
method: R4PPage
calculateLayout

	self header calculateLayout.
	self footer calculateLayout.
	
	self watermarkBasic notNil ifTrue: [
		self watermark calculateLayout].

	super calculateLayout
%
method: R4PPage
grid

	0 to: self pageHeight by: 10 do: [:yIndex | 
		self line: [:line | line fixedTop: yIndex; fixedRight: self pageWidth; fixedBottom: yIndex; fixedLeft: 0; lineWidth: 0.5]].

	5 to: self pageHeight by: 10 do: [:yIndex | 
		self line: [:line | line fixedTop: yIndex; fixedRight: self pageWidth; fixedBottom: yIndex; fixedLeft: 0; lineWidth: 0.1]].

	0 to: self pageWidth by: 10 do: [:xIndex | 
		self line: [:line | line fixedTop: 0; fixedRight: xIndex; fixedBottom: self pageHeight; fixedLeft: xIndex; lineWidth: 0.5]].

	5 to: self pageWidth by: 10 do: [:xIndex | 
		self line: [:line | line fixedTop: 0; fixedRight: xIndex; fixedBottom: self pageHeight; fixedLeft: xIndex; lineWidth: 0.1]]
%
category: 'initialize-release'
method: R4PPage
initialize

	super initialize.

	header := R4PSection newForParent: self.
	footer := R4PSection newForParent: self.
	nextPageNumber := nil.
	outputPages := OrderedCollection new.

	^self
%
category: 'layout'
method: R4PPage
bounds

	^self layoutOrigin corner: self pageExtent
%
method: R4PPage
layoutBottom

	^self parent contentBottom
		- self spacingBottom
%
method: R4PPage
layoutBounds

	^self layoutOrigin corner: self layoutExtent
%
method: R4PPage
layoutExtent
	"transform to page layout, with origin at top left"

	^self pageWidth @ (self pageHeight negated)
%
method: R4PPage
layoutOrigin

	^self origin ifNil: [self parent origin]
%
method: R4PPage
layoutTop

	^self parent contentTop
%
method: R4PPage
pageBounds

	^self layoutOrigin corner: self pageExtent
%
method: R4PPage
pageExtent

	^self pageWidth @ self pageHeight
%
method: R4PPage
pageHeight
	"allow for override from report settings"	

	^pageHeight ifNil: [super pageHeight]
%
method: R4PPage
pageLayoutHeight

	self basicPageHeight isNil ifTrue: [
		^self parent pageLayoutHeight].

	^self pageHeight 
		- self spacingBottom
%
method: R4PPage
pageWidth
	"allow for override from report settings"	

	^pageWidth ifNil: [super pageWidth]
%
category: 'documentation'
classmethod: R4PReport
buildImage: aString page: aPage
	aPage image image: (self perform: (aString , 'XImage') asSymbol).
	^aPage
%
classmethod: R4PReport
buildReport: aReport from: aStream
	| page |
	page := aReport page.
	page footer text verySmall center string: '<pageX> of <totalX>'.
	[aStream atEnd] whileFalse: [
		| line |
		line := aStream nextLine collect: [:each | self translateCharacter: each].
		page := self buildReport: aReport page: page line: line]
%
classmethod: R4PReport
buildReport: aReport newPage: aString

	^aReport page footer: [:footer | 
		footer text verySmall center string: '<pageX> of <totalX>']; 
		text: [:text | text bold large string: aString]; yourself
%
classmethod: R4PReport
buildReport: aReport page: aPage line: aString

	| tag string | 

	aString isEmpty ifTrue: [aPage cr. ^aPage].

	tag := aString first.
	string := aString copyFrom: 2 to: aString size.
	tag = $! ifTrue: [^self buildReport: aReport newPage: string].
	tag = $> ifTrue: [^self buildImage: string page: aPage].
	tag = $: ifTrue: [aPage text bold string: string. ^aPage].
	tag = $+ ifTrue: [aPage text bold large string: string. ^aPage].
	tag = $@ ifTrue: [aPage text marginLeft: 10; courier; small; string: string. ^aPage].
	tag = $- ifTrue: [aPage horizontalLine. ^aPage].
	tag = $* ifTrue: [aPage bullet string: string. ^aPage].

	aPage string: aString.
	^aPage
%
classmethod: R4PReport
buildUserGuide
	" self buildUserGuide "

	| writeStream readStream report |

	report := self new.
	report portrait; pageNumberPattern: '<pageX>'; pageTotalPattern: '<totalX>'; reportTotalPattern: '<reportX>'.  " patterns changes so they can documented "
	writeStream := WriteStream on: self stringClass new.
	writeStream 
		nextPutAll: self userGuide1; 
		nextPutAll: self userGuide2SimpleExamples; 
		nextPutAll: self userGuide3LayoutMethods; 
		nextPutAll: self userGuide4Design; 
		nextPutAll: self userGuide5Note.
	readStream := writeStream contents readStream.

	self buildReport: report from: readStream.

	report saveAndShowAs: 'Report4PDF User Guide.pdf'.
%
classmethod: R4PReport
translateCharacter: aCharacter
	"using Google docs to spell check adds single and double quotes that we don't want"

	| integer |

	integer := aCharacter asInteger.
	integer = 8216 ifTrue: [^$' ].
	integer = 8217 ifTrue: [^$' ].
	integer = 8220 ifTrue: [^$" ].
	integer = 8221 ifTrue: [^$" ].

	^aCharacter
%
category: 'images'
classmethod: R4PReport
imageDesign1Gif
	^(ByteArray fromPackedString: 'Q4%FNC%!CPP=@_\@@@PBAHRB[LSB)DQBMNS"/J2"!FQ"UBP"GLSB1NS"9JR")EQRQH2J#MSR+GQ2WOS23CP2KKR2%IRR_APRCEQRUM3Z-DQBQN3*1GQ2]FQ"YHRB!B0*IC0:KL3J+J2*#LR>)K2:''O3>9E1ZSG1:YO3:5A0ZEH2J]D1JOF1*UMSR5JRZ!M3Z/@0JAN3"1MSR-OS.3L3F+IRV%E1^UD1JQB0*KC0:OJ2*%KR2-N3Z/H2F]BP"IEQVSGQ2YOS25CP2MK26''I2V!O3*1L2>''OS2=A0ZGNSV-MSF)DQBOJ2"#F1&WL3J3N3*;J2*+APREG1:_F1*[C0:MM3N+K2>''E1^SHQ:[O3:7D1NOF1*WM3Z7M3^/@0JCN3&1MSV-E1ZWD1JSK2.%K2:/O3:?@PFAH2F[L3F)OS&/F1&UE1VQIRR%O363CP6KK26%I2V_OS.1IRN_B0.IMSN+IRN]JR^!I2^''B0.KKR.%BP&IG16YA0^GDQFOJ2&#APVEC0>MGQ.W@0NCH2B[L3B)I2Z''EQNQO365BP&GLSF1NS&9JR&)H2N#EQVUDQFQGQ6]FQ&YHRF!O3>5A0^EMSV5C0>OKR6-N3^/CP6MO3.1OS6=L3N3N3.;J2.+G1>_F1.[HQ>[O3>7M3^7E1^WD1NSK2>/O3>?N3"/F1"UE1RQM3R+G12WO323K22%I2R_NSZ-OS*1MSJ+KR*#L2>)LR:''FQZSHQ:YIRJ]EQJONSZ/OS"1M3R-O3.3MSF+FQ^UKR*%IRF]E1VSG12YLR6''JRV!LR>''FQ^SEQNOGQ*WNS^/OS&1M3V-@0FAC06KCP.IJ2^!CP.KB0&IBP^GD1FOKR&#A0VEDP>MAPNCO5P2PB%"@C6U @@R C>? K??0C??0C??0NQKP@C4@CA3 A7]_T&"@D2 TGOP M5@GAHH@C)@@@XZ0@@@7H6@@@2@@CO@@A5@J #9V7N:ZGOF@A5@@(A1;P@3P @3;X@]W@1,P@@,P@@ @@@O;@L164@3ZD@30@@]Y .Z =''\[1)R@)&@BG9A@@@@@@@K@@@@@@MAC4AA0#?@DTIGD"0(LFCBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7L"1(<^OHDNJGD&2)L&SJEN*WL&29T@_S0(U/NFE@!ZBP1Q8VWQ3(BT4EA(-HY" $ZHX&@ Z>&QAPQ"BR!"AT%IPBXT.B@+N<_KDD<9CW(S"5FAA;DAHFAP1JV (@0T0S0^BR^RE:,@0M4@=RTD0#D9FW \ZP!M6:L@!FBKEFDHP+Y]CAUM<("@)+,AL(C;1/Y-7[4EDE)0$HN"I[JBB"AT3G% ( 0T0_QEP2FA7XM8.%0!"&#MC]DD4(A XE%#ZP 3K(!H0<,KDD-P''"[K^QYC(T66A\1P=:R$P$8HYCI0S?=QP0:3@EI,"PRXJGR;A.QR$B12B^QM7 P*4P2HJN#3AIX,(D$!R@:VP6G(CJYEAH.8M%H$W-NGT6W6"(EFC[0M!8TPMLQPD(H@DB*SDI5;\PI@%\6V0&XLPW+]DHH(,L)1@ZE"@1(2^[M!!X4>@\(M8H#K2BV0>)Y [PP+T LE>@;6("@Z($XVHHPR%<HP"B!RDAWS2BQSJUZE<9\T&(2E) PY%B&QHH3U@R]P"W""0&$BPWI''%WZ5EM9LEDQ;F!A^L%A''FIT3H9MJ!"BZ*:JJL& RFD#NJX  B!\0)T@HH)FB)JE  @@%2XTBB@AZ J!EJ@, M(PPBHWZG@JPEI_<P"!I@W.()\*J$$F&*%2BP)$A!+D+%_8V02!XB%60**1JV!-G))9FAPR)NJY2JV+GCC(Q))Q:>."$V(ZQP*2WUA%Z%-L!!X&*$R1R"QJ.R/-)*FI"J*>V(8>)J(RV+(**[D(V0:6:$,"[[E:[PM"Y-+TM\@,Z/(/@[,H_ED(2,,+L6B6;B0E[+;6DNP31DHQL+IV>,E6,I!+4>)RA-PVFT&6:?7[9;+L57^[J+%*E $^?KNOU*2L20?''\2TV@X''GN&A(''*L4:+&#/_IRT7R^&L=K9ZZ7J!PEI+*F@06SHB''/AJ]$G*H,C.+OA"R&-7 U3X:M1456777W#''+__^_O_?;___ @_.M2X0."''88X ''+''!FVF2:>NNPQ2;99IQW#%DX!*Q@(^V\]>;9R!RD>_''()I].>.&():;::*2G=E[+,L\.>>2456;;;[!SADZ6@6FRP@JFAB?<<D,DW?302B]O/OKBG=<<<=AG#;334#MO?_JFWE?==-1+3?774''/?_PIK[I5;R6G8?+,!0GO_O/#I/0???OQKK7?=>N^/??;J@8?+>P@L8M<R(@@''ZDHSF$B @!M80@X.<HDI#N@CF2!ABUIP Q^,H@X52LDIG+BCCHP A#?(PP^BTHP''SFDIFZ#BD:I0 2TT8P,-&LAFGBIR@#1I,FJ P!"64H\3''B@K?6T8QB@N,X]GCNHO%:#DHLJP"D<,(!QI6DD#V%FBS&B@IGBXP9_H: Y %LPM1D#FLY*1#F <(1+SJDX0./FM\ER#F</8Q#KZ\X9&CNLZ=5#GOL[1#7"$H2C?&DY@E''JP_V0#H _IQ#66LU0H(T<$?MBDR#Y!@)WD)BT7.P]J]+JR_/!$JB%I2T46 YR''MFT*SS$ATS[!$:A<YR%QRT-S0%JS&+P$I"&YR4/2T)V>SBT-WU''KTP+3%LP<I#AWZ\1T"/JY)X0%L*OISET6T9+F/FX%Z;B"K)9$B@20 2_7,H]W%/N\+40''N\&)3''Z"<97.YJ\<7V%N^,K3''.(<937''"_=O^Z:3''/64)4C_*\>AF#R BC6(P!O:335$8D!7B\O?/C&RHZAAA2T  $X72-FN^/R#H@6)RCV:!9E.-JP$MV%GL<)Q%&;T)R*M*T1''R%NQ0)PHM<7)R8%@@4UDT E6,LLSE$ETCB3B*D =*%H1$ER#K/V)SXW*T8$*5Z!Z-Z)XQ^)U-9+U(7K5*54EJ5^KF-Z2[%V,WP4PNZ=CTYIX@ L,(HDS4LB@.-+5+''#M*5;72-^>>/V/ @6,X@]K6LHZ=+BHQPQP@CBCK 5AD).XP5-O@"@1=EPQW,A,Y#^K6\92=+N^CZ4WY+AY4))6-J =+V*=$M+R-)Z4&64-Y$<+V,7?Z!Z6,K7-Z7]KV=^.=+^(=R10RR-\7.96-,[EKV.UF5/]Y!XTW*@AIRM9BC)80U^^0@HV,.,I;&:W.=7=+''Z7R=;,%%^<9C6/^[W+W_M:U;7)[V=<8]/]=+9W/.8-K7#/B=?92-^?;JT/^LV;7 C#U;4C)*>A:7/ A)\7@U! @@540M[I!&PH"6"BE?ZCBT0L(\L^;/BGP43"DY-X1B .\X)Q/HPS_7#DJWY1"E,\X1WS&LP*1/FIPP3#E:M813,NLX137NLUB?''GQI:1#H?,82[WNL%PK+D(FL@BB+ HDSS@ HUKX  ''MHDQS@@CFBQA9#J[>\1(S+NZ5<3&M+/9?<50#+N\943''N-.93FB80RX&KIC2_XTNEOBZIXY@ZDI;6MBCK/RKE\5(P3.:4TH.]JP[S^%JP7+Q$%[4(R$=:T076]J C''V(C17)SV-Z5JR>-J$?_^)UT3)= Z@C![_,U"=/VAROW''R*]_7(W_,:5;16,*NY3NQ %;+W''-:5,W%M[FQ#>-#M]''Z1)35,YJ^:6\]^-+X7_V#''HDHKECBTPB2A DY$ -X#VXHS=,B@AE "4XMN-J#''SV=:29/R=2Y4/ ,M;2G,&=?5[/R?;S7/ XOZ8OW^=:AE P$4T@HO@&E@M7LBZI=%>-^]/+Z3*;7*##N[81</=,Y?3^)*X5+ZJO?_],*U_WJVZ33XI&=5,%.N<$GG^-[(//@"=*BH&0"[6-#>N\"C''&5(K9/(P$^:1+$]\*E?ON%MM?+R''_92)TN]2\9)!AZ&@+L!SCS''F#FDI-#-;''\G?N1(S3/A5\;6-+/=;PE''.HT K((Z&N 0!?@AJBA!Z@?SNN%G=#.P%2C$/3?Y1C\^P.HM#?"_E=;/!P\2$QOO81HSW/JP1;3"KP=%G3?^1Y]_/H(UW6SA"9#1O;;<:_4^!"WDPMXUA+-FKNE%Q^3G<4,^OYH7#?#Q!3;0)H<19WF?^>A3//GGS7;%A6=<8RN?=<<7_H:M/6S$Q1;I))\1$A''C@GBKV?X$T__?G!*Q MH''._**5732''Z3#3J^>2MA_/^$;S?#+(9?4:Z>1=W&?^-C+''/(-)''+Y57?:I8@/U#9K @"TP@PB P S)0B@1''_A)786!''.(U8FXPG''3E73XI''4_Q&OC98F'']6JKA7+OI7!?17 A^HAM)(HSVHKX)76;)8D>Y''#RQ8O696FVPC!4\@B1A78W VDZ]!L]A(KDE6P#Q(L66FL^.HPY^HPXVG$ )&L)RG0W:HJVI8MYNHKX]8GD-8T[^FNTY8QJ)''=ONG%I:FIY-7T?BHP]HW8L8@$2E7PV"G-GM8A@U77NU8P&NHA$FHH$>HG;!75%.G03FH!H-(I''.HG/579V?2!#YM!:C*\C5JU78/JEN;ZC?<]?''\]$4,^HNAZC!4"@;(^B''!!83T^A*''!8Y/!9WN!>$!]?A<"H>5]?N=Y:+8]3[#!;W#XC_LFB47]1;L^AL,!<1T"J6]]"+DY,9<^J,Z"D(''!=XS!8<P^K>X^B0A"K:5^J?M^BI#Y%V)@H7;^KG0FG\#"E;Q^J4S!$:B!?P]ZN,<!;R/"B''U"L-*!;TC"MWD"L/1]=<M^H*V"@<0"J1F IB["@O1VAN:!=%YY(>O"P&F"H:#],AK&OY0"L1-^D 3"F +"HGT&F!]"Q-O"D0=]>LE"C=?^J0F"DA''%3[T"NDRFD-:ZAFS^GH%!"D_>I$RH)[E?(Z/+V]5+X#HWX#DL6#CGH$T)V!]Q7$Z0H",0H"N&X!&/H]S@C$6D7].S''[:\F%BJ&%_FF"S=9[<X''[6JI[VVH!A&)!R%Y!"\)"$[9%''-H$B@9 T1I!-P(  ZIBCU@]0X1AG&7]11W!N33N;>CBQH5ZB/6#<2(#_VH&@SH"YCI#Y.8_.&G!WRY#Z!7&UU8#P@H!!E9"$]X&\W7XXNFA+A''%Q!A^1)6^9,8ZE!PBB% BIXPA()9&Y#0[!H9&T_X]3[FO,GSO(0QA("Y$4,W[\4''\*WG#2^X"''G9&Y@Y GI) U.(_^@X[''4ABY''0$*#YDNX8&1K5''X_9ZLK?NY;/9)BK=)7^:Y61&I;$:Y SVXCSFYV-FH>KZX"0BG4B>Y9!:I&)%8OJ!72-)8A<"Q(PFF!\:VF^0@RL$@E]4@VI4@V_<@RMH@''%A9]FM))B"W"0VY0''VIH0:IEL*X^]NJJ+BG0_")II>YD;&X (*(\7A&,=*I7[.Q@2>X-L:VGN( B[@@(VL@N_(@&;\7EKJSPGJ(WAJFJF\@&PP& )VF:L8JAP2 "KL@]<Q8"^58;GZXX;! UJ4C#Y^JN^M9\5-(P''"XWDY)\<9(Q)*FQS:RI(8@NKLJLUHW:M$E6GD@M8" Y8^  HHI,_) Q3(J]( @ZA(@A 8@%&%8F@F NB?<*( 1(J#D\.B)B''^*(@$,A7;/%#KY)#M0&E>V"T6^^)%!@<GU"GKA*_ZZ&R^4"L+2"IO$BIA>FW^"^ASIXB#@@@D6@GMJB+MD@DOJ\@9[M<1R!)"W&[6.!/! @F&1@H?"ZE+B*M(I&L&B!:<''!<?]!??W"->4&_5Z*G&"&K,H"-7]!"N9"KL"*''BJFZL7A;0=\0"4@GODTI+4(D\^I.U()"%>@DFD@*N@%8SSZ*&^@D S@4JHX9LT@C@G@@MD@CF6@GN-@D]*@I&-I#/L&S2\&!JOY.JX@F&!@J#+&I[Z&\R;&-<9%?'':*RI0&N8* [\^LD:C(QR3A6;YXI%J@C%O=P@9EP@5@PBX$PBH6CBU!F@3"K,9D@B)* M(O&BX# @4J[,3!KBYP0IT@)B$1@A7Y@-AXPBVM2@8XP&N"(''4K)+MDXYVI+BP&PBT10BZPF,(<7,+V($?>Y'':NW%0<''DEVCZ0UJ*306AIJ!AT< BZF@@JDPB#E@@W; AYWBZD0''X /Y%STI\(S&KD8 U8&:$]SHX,:I.R%*_9&)%M;H.Y/Z$]('')/PW))A((ZS9H*_9,#FYX[]V"CGCBBW @8!PBI:P@)&P@W!@A0L;''LU&$@)@BU9@!C?9X(5K$X/FJT=@BQ)@,BM&,G" @42 AF@ .C^@@S)@AH7P+E-Y\%69#E+?6Z?>Q+TV@@*U [3<JW$7.IL''*+$V6X0%:X$&5*Y=$S$P0;(L4VWZ&0A3P@Q8$@EN\EQO8@T<5P"2.P!ZD@&[DL@X$@ 44@RTXY"^$FFQPEPA+ERY$G:LDP!>P@NL0LB_8@MMT@M3PKA]ZW[CVI; :; ''!9.$:V?G.6''&>[6J21 HT@OQ0YVJ"H^&N)J]^)XC.FQ$&I];JQ@L<!V3Z''JX@@&_(K5C<6:-E0+06+.82ZVPXI!I6KUK$@JWTJG3.0QJJ(^N.PQ$[@"P@LZLP@QOXJZ6ZX4&Z*6,V*I27IS>!7:N2LLF:J3T"([=M6 G*8/861B**P 66&RCPPP5\@#>?4J[$K@ISQ@IM4B[6RN@6SLDBX@H]&@A-EH<KYZ$,T&[ "&@R4@>9IHH].@DW.M#=AHHQD@IM1E/%''@IFN@GWWBN;>XIR!B[X60HG)X@R''CE;)&4R''@I9[NP1YNA$N@IOF ASOCF>)!951#G["&B38*_(7"$ %^]81#HD!F3YA\C^6@A&P@IVI@B$JB,^1@I2[HI^;@H/00IWI0__/@IW.LIFM@D"6CN<@3OJZCL&>]!(!@CDB0IV@CO!IHHSZ@H+LI/,3&[I-_P0&&<; ''CK4Z[7!''Q?"Z\G:-/!3&\EI6^"B$J(T@DE%@HL32H4C2.4#"-:="\*A^ BB$JF^AX^_=K+B?F1N26AH_9ED,@B.O''ET.PBS-J@QR0BFC XXY0@1)@U55@@Y>@A"%0Z@%0BEK!AX'' AJ3"KH\P*I<@B(3@BMH%UIC*-,TG(/KK"N6K%FK:!8 X$++Y4"E:(/FK"JJ+ :(K2M2,DCII!BTVA%=BAA*@4XX)B # AW: @^R"BP2@AS JBPI;B]''1R*BPMGN@B@2@@U7 5X\0MF^;B\41ZD&=BC^ AA  S#P #(")4::GA9S EUCL\HF0A1/&]R''PBI$MB!'' ,; &BXOZBI.PBDU=5L5Z[''QA@X2 @GHXA&B@@V" @TR]@X+PADPPBTH1ZI9;*)=+(.2+^T:X =W?#KI*J@)ZQ9U9OQG:Z<@1P@PT\@%O4]:R<J(7 @$XP@RH$APRESD7P@M8L@^#/@%D @Z"4M;-[WHU4 QO0BS-+PRQ@M"$X &^\@NM,@"O7Z3/) P1(J>H0B*V@@&RHB7M& I 9@''-$ &ED@(Q7 ",$ HW+ EL8JW)X>EG1PBP>&;''GK!L(@&K @ZEXI!J<@Q>\L) DJ1"V-_^*+&C"G1,OY%6?ZZ0& H3H*.@ZX^R@]#J''HE&CESR66IL(@ GHK2Q(@NID@*V+@ED(@NJ@E6:* $7$PA1L=7QA^Z!LB"LT@N2) LV\@QD0@ID8BM]V8-Q.Y&**IS^_]6XJV2V2X66?1!:</"\U<*-\J3(HT!":VN.9\4P:&*#P!XF\>@C%GA.OVZ6#[@G''8@E$&B3"R/RN/@V^*$EST@ISB@IENB0Q^,EN%@C0I(#V(@AF)4@F(CO(]CDV(@GMWB>N& H X@GO,C^YE0<XM@E_,@H!&$ N-ASE$@C%B@\CT\I]&A]EKBWD^3W''3C-"&@A0(8F(1DCV''@@MQAT$^@C%IQ%I/2L+)"E4^2J37''GE?-9^S2.7=!=J5/)7R23BYC^"\C^0!'' XEB3FS3_VQ%"+Y_FA;8DFLA.C$7Q(1_PQJ@"_R^\F(@G5!%!W$@GO,C)S1@J!+$D$/@DH0>5WYCAXL@H&8@@Y/>WBQAZBI^,BE70AMTN9 * BRE?-UOR^)JP@_KJ:X& @DL#BT>:HCV0*9>@@OQQ@; JB#F@*H8W$B =+V?K.WI9< JZ$@[*X$L@BQ%@28E0BF ?A49@BT6P@X JA+F.@X2ME#S0TI" B^&NA,F#@IE A4;17''[PBE 0JT6<GWW"A7R@BD$CA$> @6PR_MR,*.+[5%)O19YO($[..WNM!<?<#29:."4Y(0B/5:;K&*P7&02@A58P9<!;BG" 4II@@2U=&8R-@7/''BZYY4"""BE)0AD0@OH5@@9F0N4; A5(6ZK2>A9/@-55@@?$L%@XY@0:KA)& @E,]@8&@;GNPO$;?X@_&"0%IWPL50@SF+ O3+CZW0@#?N?!O,@]]DNQX FU4\F:A@@AZ,@%#A@&AP@&J\DO(.?$@LPSS$"DDBVKBIE@ 0(TBC3YL&OB QH TFP:DVEC!QHTWH5(R5T KJBV"SI9D&UK%R)X-W[:DFUO&SI(5WQ)2P*QQ !!D/HP2-BP()I2T%E#Z)MOP$HU!LF''PPQHS!#5OP(UB\CTTE(P\QZGY$>FRIZZX2L:!P0&LITQ6_CBP)H@BG *EQBD@)^OSHT$XZB!B@HYBH$%!2B*H)B!T@"];&''2Z(0BTE#,TFB!XQJPF #BRPME ]NMFH3(5I@7IYJDI''TVG9" "<"SE?9IFSW2 T_KP(4ZFA7W+-!#Q(,NNDX%_9B"\]<P0R1CU0LM22BD_(BBUUR P4!L@SX# 0ZMCQ7TM!R2E4VB''R4&SBASY.XF)LRL,I(](6HMA"ZZ*BT82*XDRI"AIJ(LDPLI"DS,4,\>87"!Z0+#$!#N((>TZ"''@#"YSK<DDLNPS1P1E7::6" #"4<BHLS^S((X$$]B(FN''Q 32X[[<P1Q157UL$RI9*XHPUQHC(O/00J\\*8HTRQQH\Z0IBDC ,JX^(.N4CAX  EJ@GEO3B<HDJSITUIHPL"DD%@$2XVL\2RAA(!X!MQCFFDD X0DVWCL@0I!@81"M@A/OD,2NBPAO?B.LPBN1P@J\=EZM DB0W,(NNFLDR1QIHZ:@@#$4!(XNJ$IY9(@ MCEOCC!42F@L%SB$HYD,JMS%3NPQQKUB9D!F0-#-[!\,45H0>WYDBK.%I2*%D^%6V6VV]-V(H?A!JX@5AFMMA@D2]@>T:CAHZ Z-*N&$OC#$ J0TPSK_Z 0X8M;KB#!$B>''UVTFLIJ(REKL@FP44J<6HBA)Y[0C@<4D$BC"DPJHZ PCK3H9@YPD GCLB4#,P@LP3C0P1EIH&S N4RVR.DHOI 0QAL"#+SD3T5(^@HRLF+0@0N.%$CC!2O@DNTFOAPAH2DV^YVP509);[A(CV_][\I^KWFND"K?)EN@C *.D88!RA#QH)H''LG "D"6Z\@JKL@3[QH,ZM-E$$TV^HDJKFC3IJY@!%33D#$Q"2@@O.:.<!AD]DD$A@3-AB LRCGQX!MW#WAS6U&AM1KT8A7&3DOKKMV^:<52S20 825'',*LWHU;0H:#9)_KY555=7](!E&/@B$#0''@*&QOQX&*; 0_KX@ R"''+AHAGZ9^X 8_EJ''O,1)BIP,SQNIL S<62W(3O4,<2X@N[:LWZL=@Z@ K@00RHPH@K4;KL<*0E''GB"TT,4DJQR;Z$PD"&ON%B!4@4(DL"!FPVTPR"L)^X01;V\3YQJJ@F:0L_"X2FN*H-1C\T/IC)"HX"?:]MAGR9F4&M!)RII= M]"]DXP)! !N]TF,O@GAKZR !)TY\P"AIZ\QRE+J/FMB A.#:DPR:LLP!Y"@SRZ,RVA!!P>0L P%P<DD(9%BY$+P,CH!(0!L$0P "(DDTE$/AC[C@AEA4(SBF&XLEEIL@CIC-T&D@$E+,40T<1N@R''?@CH>[@AC;B#QIY"XPNP(T)RW#AA3\PAQ!4(@ *XSAXFEDQIG/%HMAEL%""R<9A(HXFR.!@N-PAQP+@Q1B&)N@S^4@D''"2A DS HQF!NH<%.HLGR-P@B#V( P><\@"":F@N]1LED2A@ TX$P \J.I,%B*^CQ!CH@&#@!FDJY8_F;_<+QYK3%]JZM+&&TX:[7?Q&NB,WGJQ!T8NO3F@8NTHVML1H!B*DY31!92OZ6\9B+CJ ZVKE$HK4IAFP$@PM)$Q@AEPFD$,H!B9-I1!PLJE%@.$IH1CPFB^\[P"F ALF-)\AG6#BDD9)""E:T MO&BPA"JCC[P0!""T0 P9Z<DD$Z$&IRO! D9BXP2SDD Z&FBHC =.DC3JP@)8NHP3JH4&5FBGJ<12BD./#:SX/YD''L8^)WUP7]A$VTHRQV:F":ZX ("+T0%B2A@W! !C3U.%Y''QV,O40+DG/CP!T5,@ NZP@MPCKLDGG:KNY!(!@9&$A.*ZL@P!3UD@(KB*:>DYR3?B&''Y>CZCA!4<8QH E\T!EAZCQMC %9@5RRXF,1Z2GLHBE%#LI)+P"B=Z0+PVNNLPV($FRUB Z8%HA@T(8HV?ADXQ@/+"DBR!"D(%D =^PHAY_EUN)H5(ZL/]$DZ8N"I]P[\9"IBZ_R95D"5YCQKV]A@$O*FSI\PRCJA(0$;C0KT.JD@4DS/DHUJ@AR_8CY"G(DD"4DAG.9D%CB$PG@LJ)0!(R''M1S##O948WD]EIBGNUOL;#I@1IA$>80)B<HHX''^\%^''WN*S*M*!#N"N''^25\P''/)G,ZJ\?EWWFB7!@QI.XX-QKYF@OS! BF@R*AEX1:W G''PLD/M@<B%"@BQZ#?45>K&D=SK%I@4SPJAZ:926''GDQ<-J2OTP6AG1(\X%<7(@LM4BBZF6RBBWMXS@1*HDALCXE??-MD@B<1IICDPF<)RN@RVQTFA8J"LFV9Z(%D+NEG@(.[*K, VCUW:H(PAHP$P\$PDM@HQJKX4)^>RT96D( RHC\(PX&P4CBAP14&!JU&X,QA0<TTZ9ZEH4.:55CAQ1YM.OHR[[E,TQ-8 B;,%0:_3Q(YJU@1''"K&I0%PKPNB^0"LR\H$&JB@C&"K7$=$R1OWW PC,G@CQTSBT&R9 Q^JB8[#! H;:@P1Q@X=2PZ#;# NQ$9S''@L]$0 84*B<S$ZFHM8=;FQ_!;ECD??DIH+:_$HITXUDCCR AL[,PQM&LT3HM(D@*#2<1:D + J\B\73)DAABD8RU%/4J4MSK).SEG&JG*123%4U$=G5$EWSN\:S#?2Z28%Q"SF=\99#:$\3,N^+74PD5C+.(X#P@Q5B!P@Z''L.(P9 CW[C@)>VU=K04"DE+,PB7Q#"GBH2H9''<YLPFM\(<NWP])P*@V@5+*;317+A=''K$DIN:B,)43P@ALRLK93AS\TE*ACWF*I@C)[P&V[\JF,3;L%TEQ,0=R]Z%WI6UUGP/_1ILH 9T5]+FN!AF)#:''''(S;0DI9Q V#6!0FT1YSFE[N2-!. ]VQ@@A_<IY]WY.Q1CF*/?>C6''0@-;<AXZ<(,$)(P!XT?H!I&<:E<DZNHF\?BBH#KQ6(R.,X6,M\1+D\''S6RJ@@%6DAA[DW8!LRLHP&UBDAZYO%$JN^9DHNO^FFC7]+9(<1M:D]3^M,2?,Q$\TE* 4>3 D;8(? T"AK% -^(&&AM@@O;BCSL@DR[@@G]@DS7B3Q% 3!%DSF) WP6@6W<JD/OFA^\DD@EN EK"DI7"&ZKHDKM@@.+"D(9F &9N9SH(0B),*\I+AG]QAC(N72*N*P6.7B:L$U$D(N#"@]1J=IXP''EZ.]OG&7LB"DK"@:7A@?R]@DN0B>$$(A*V&D*S,DQQBCS0B2%H*OK?DCT@BJ?3\QJDSR+D)I@B2@D0A@LN;Q P0H!Q_Y$9;0@]/Y%]>!AB7X!LNBF0*HC>G2 !*X@6+Q@RU[@"TPGCIL@A-#AA9K@@W0@S1 %A!HK1-** AAA@-TM7_3LA2<LL<)0"@D,P;*LL?)LYF@MBZTQQP#/[_"BRJ("1%S$XH -Y;B $N @C>8&.7A@MLS-]I)BE!KK5F:JB3XH#?XCD.8 T>I ^5R@/QB@81* $P(JUG0!D60 3E;C6RZD8:IA@P0!FQ+KPT8 (39H$1()[%Y!J*(C9NX 29 @D<H-4#HAH.9 Q$(K$$  "N(H&E9-9TKP"J4PWL"GT''2"D72@T<JBRG!K/=P2)\U6S\3DY_,.HQD6DX%0HR\DR)MBB(_PIM(ZXK]\Q,K @@L,A4,4HRXV PM:@H)N[HT6@QE"@G%<$@:*@*M6Z+***P_ED*#4YWIPRJ)"#&X:<FY&33H:2*X*9U10PPYXY5Y1L(S("]EXKEY6Q\D>@P;(@R:X(SOXH7/B(LDTAB!V RN" 5H0@P0THP&LLQL@HT=4HD.V@SC(XPX<@<%LBXK,J,.,@L?4HBKX(@Y>PS"T3,>4PEJ,B^5P1 <(HMCVHIPHD1P$A=P,@NCN<H&(HP''\HIO(@MPNBIQPK?<4(C9(PN[,Q^42)^V@\.$F1%+V+SS@Q6CB@:$UAG]%J;??D,9?PL'',SHV.<#J88P''-<)GMN"X9DH9/ J@H7BBQ%#MS: A(''N(LM :K]B@B(*.+0@@N- D:''PBQ+@@V((A(?JDQ+@CB6B@SEB@U HE.4@@K;BCCE@@I'' B PJC;_BI2M@@R!BCF% LQ!@CCU#GV)*>HY$LMB@XQ[!CAS DQH#HZ_DTG8@^5I#@:R.(NCD?C\D&8WP7B#GJ)S&4DE&B::HD?=,.;**ZX\P*KDBD/U"J#>BWK/"D-_@DAZ!I-4"D^WFS1/ DI:BD/=B6F_NDF@BELE.[#1FE!GNBP22%QZ@CK5@@A^0\,H(7&_LHQV-HU:0P%4MHLL6\ 3SEA\OM?1GU4([<"BN<R.RD46U10/+X0QQ@ 4P(4*YCO''()"AQ(!D;2@R^X@01@ >/0ACS@)Q @Q!>  B;0 PB:T+IX@#CX!A&I!DXH!D<HAHEP $_1M/@*"A/H D70!KQ#E_+2 &$I@0RXG3/P@P/PABPIRX0)R;E<@#BX,\-<@!> @TXMA@/,&R9@!B1!" P8!K2HED&-BJQ4)@$:47QS1\,[-HU4-I@H(S#EU-_ABU-T@A(0NE6$I@[X@2K8C#0 U3*0B*F!F3)@AFSD/L3B 7G=#.?05$3PHT.@!C"3@2"  [$JF*"9 [&  Q%Q!DMXB&8+ _G $$YEQ2\H12Y#@ )(/O=7;I89L@Q>@PT<:MV$*<@0DB4?,1 0R@SXH!L.H(IOHB*&HY*!3LEF>=H 3LG\.29:V8'')*H9<>10,.HPD.I2KR FF::%K@HL[@@NFZ1%O2H2C@PL0P@@,GXHDT@H(BXTTDI(%0@IH8M&M @PDP@@,RD)0T#R-@#E28$E1Z,*2)[$G^\(Z5B*&;CB%HX,^^-M,)].Z(J\ 6Z_&J-YKDA:&''X4H0 P,:M,TRHC0H2T,JHR.C] ZHI2%UXIRJ0"Y$XR.SX@T<@RI6E(DDLVK*M3Y4C_D)U*''6E*";](TC\$Z<KLT$@P0<E2-CPV"A])"''Z># ]*"?U*''QC$.7Y2E\M[?2R/QRNI]U3PN8.2<.#W^GJ''E''XBDSE"LBU*B>9&CF@"DFM"#+UB.IU"BP&@BF7J0)A2BW92CPI#^NS"D*W4H)- 6MF DA#"DR1B5;I4C: 0D!.$)AO _QL"D+@ ES<#^;Y41&PFC3TT@I/C\Q83_Q!"3KF%AJBGV0A7ZKLFD>4DDI/BD:@ENK8492N/^9>J*>=,M*LF.*Z''YE<VZ ;R(8DVH<8"*O_N<?\D@H/"%$0@/-V/Q*GJ4(.(J(6*PT]3RA"M[%SUSL57HB<,0H):0HR8NBG.$7; &H''SWL5TQD)/[8:U":Y"]I<0=_V''Q8L*HF(:%)#"I<:HD\+POB?E"?13V8W5RC(,*8#_B"B7&*^-"UH+$X'';ZX-,<C7C-,W\\T8\<I5Z\PP7J/BP.T1F]0T\SHW3MABV,8$X6B^V,8@ :''Q&[,S^:$@K,3U;IC!5FX8KXJ4<8DA/^,:_%V\N XT1P+C1.( !S.[KH80S 6S0>W>AMBJ\H"!-$ML0;Q[IU,D2>8B\VBH"TRIV06UBRYI,[6=1SU:+HN)[+X:3"L@2V/IMS6>WR4-4%2&^&$DK&9!>.Y!''4S([\%V:&(I2[X$]69J6L3E3^J.!"MJO2CA>X@8-Y2,\3G_(#43U%"G7IFT!#.ZKQI&<VP.ATY&,>TW[V9T.RR ?)XFKQ@+K"+/=@"@P''PN^J_.SF6@%@@4I]MMGL8T5=Z:I*9*\B3AAJC)Y3T3%%E+V^[VW%0@9RP.$LY.#*0+3[-YVAM$ +"1(VSX''.F,XS[TW];P"D$[:(N--/N&(6%Z9Y<VDKL-.''Q-.$9%I()#A*3]6V;UI$IKD$-N"J''-O[^[\L.47!JFKL,PRG>XP[FBU$SCS\%V(!OHQO0@B":&T KLJ2!.H%]&*G7FXR<R@^I-/\/N+KPVRT0HQ@T@PD;V)GS)&76-0[YM%+;&.R&>Z<]&[/WK\-/\FYF>-]5.^6I..T/#&<K.PP+ZR1''U''?P8@%.K^[IV&7?R*%>R*]=R/&$**F++?[Q-G?-/7(:X(.7O;-7Z898P[.(VEV[A;-+E$Z3XNQ*-R91Z;".>7J[C;.7)4UP8@D0#5(^69V62[.(T:A 7H.&^9M;:S*&BW'')YQ-[&;*T RQ8^P<81PU+HW.Q$;^+J6 LC5J+J[&%56Q7L3%PINU<%80@U=*@*]%)P9$AU](Z69"))XUDL8.TT D0''MQ@+R?3_Y L18R_Q[-K@[1C:_A,1W1DC_1D$_1D6?[DT_!-_5AI$8]M6W$>,YVI;R],[V07D7EB#/!KWU1GX]&\ X1EP9N2(;,)0Y$)B9SGB=[ZGW"0BZN0*[1+*;E_GP5L;4U-39-.7Z$KY=*P?Y2 ^92XTG"?6 %-BC.8]B9OE:^)D7*IIO@0=X6Y"@F9>:N8)ZIIS3W\;I8*C;_\307N$C7\4G''<3=''%S?W<4MO]DP/]DH7=DE?]DIW]DY_=DJ_]C>_]H76;SR-Y5=V.2E()7N^<#"%)2]\[''SK(NB&0ZA>3%?.]AI?]PY795Y''K.X>]S/OONDN,R1?DUQTBNJD:EEG9?,V;].5=TW+7 -^6:&DDFV_NV[?SS)_=%9/]&%?]&*O=+9VI5W?[]QQTQF&6$ #XU@]I<7FBL$VB.3=MN1%=7Y7=7^G]5BC=7&''=7*7]7&7=73W=77'']7]_KABOIA!''M1860#;!Z&F''V2]$LV3VPXQT[/<#O-G6I,T#A.0"Q7OAI#RL35I 5''YS+DG!QSP5+6:S$7JD[6PV6($UM?@@96=CE%/;B=FWS6!\]7''.''''&!9"Z1G-F*@(; RN5O\.4B[?% RX@A%-ACXHJ#O8R#U?+8\/*''Y?*''S?*'')?*&]?*(!?+80/*,/7*$-?*%+7*-)7*1A?.0G7.21?*/G?./S7(EZE8E3:*]-2Q3''''FS''<V;CS*SZ>!XC=G$#+12W?M''[&+= 7Y*;7")_%.X_3C^;&8\O7Z%HU;9-//#]Z.L5.@EY5-N]5-3&#<M3''WM;>?L=>CM??3QC?6TA%,4%WC?H6XRI&''P5>000D1^!Q]8ZQ\;.O7?7K_=7^]=7Z?=6>?=71]>7@=>8#]>86>W]3''>9_]=73=>96?>8X=>9N]=:*?=:;\ZA_ (3>[2.,9!JZ;;2U=BF8]BT>P\CL92#5>0LN59G&YYG1]"(T?20C_?" ]<GOQ0B''-2MG_"6;3&X@XHTX64 EH":.CAHXZFHF3(<BGD"AH''T*1(<RKF#A(]F''J2!8F''HY!D$%0"\*QISBL1)QSYT.VP%"%Q$(P)<6SL&#Q7;,Q94:[N''C2E>"1Y]FWO&TRAC''6Y=B!T)SB''T''VID:%P)YZV(OF!H>JPP39@PQ+"\:UP%F"W=HP)",$^@BW(>JAA12;^.7+1<-7+M6=_F(C5??+]N? 08K2DEQ?>R:^0XL^Q@4/>2?"17\U"?FC@D&[(6-@15Z(M.-KRDCQ4]A#\:O(5;M"2Y4N49JRI(!R"Z"8-W[V*UKR%6X9^^/U$V>D9 5=-Z909\Z3Q499=3/-%<^!M&2NW/-0(6;WE''_?&[''L$P0YZD!U*NDPR!#&49=N/[??5D(>MD&B]N+97R\K5Y1:A9Q$88HGGI^#_  H*"BBDC4+H8HCDZT\]]-L1^EHXR2ABBQ9 JT@GAVW1-)ITFGKG$@H:;OEDI)$0\P SL=YH(8498*###S7&2JNOMNH(YIAEC%$#$C,^BRROR=*8IIEOE)$$%SW^L@\MV&C 2_=''?TV(WGH&)[QD!8F,5-)=ZZ*9I''5CKMJDE5"H<!-KI4*%EGM4&$]''&G,^QYMS_#HU%I"& ^Z%(F4E"&"]BA+:JJM:#&%UR9XHQABZ("3A @9O,O$)*JEZ%M=GH]VD95VST( T*<"9*%J+,K9JUZ243%(+++_*J"./-/ZZ::>;>%+^R8,JU6"#H66ER@4!P!SVVBZ^A2AS8+''EAA98 NPIIE!4:24V8H+;[[_%!$/..^&"Z>:::XJK;+#*/...._OFV6>;>]H+K;3=3,..HT+T(LT"WAH%)(LW:#& IY#D\JZ(DT,,*&5M3@CI[!X^B-95BU^''\WYQ^^\[(79^^GK?)M0)?KE4KQLX''((I$30-!Q!F&JJC[*%GPW,HCXD@@9%LOCSQ="6!2Q;;&S[!"VB^>KSSTTL=-]QUT7655U%#/[WVB5_G*8J#]_ !DP\!()/OH9Y(E) #J: TP8_((@L"!"S B]99:;47777;?S_  P,>NNFEF5;8D)]D("VWJC:X(UWIT _SU&X^$F''Q&V.^$Y-0X!2580,;]R1I)ZM7^-"'' 29(''%1C:&#N[GM]\.1SX^\]5II/"J21$VM%:T@EN[PUP9,[_?1DGYWJ=,Y7).9:5=AKG37545-__W>(U!-3_16"P\%W((@B!''-"$\W<_7"JE.![^N#@0M74^/K.??3%5''<?IM1:V?>9>L^;??;,-2;=#X.@?<LW@PT(O7,)LH@I9M[<@M .@A8PB0F+@P,J5*T?S\9-6.NP02BF/AFRTAPU2<5.ROX\6*E%BIY8XQ!\>LHY4)BFLXQ!CR4Q!!3*4HT7?JDO]]!CH_J0"CLD8!A7ZLP!K#FGREP"CFWH1A,R\X)R)FETN,!APE&KI*IPC7-JJLX1Z$(??BGT9O;CJ%"1,X5._BL\82#GN]J1#''Z<H1;3:LZ&P^UK85DVI,!6$A! KF4$$)Z]#*L=+LQMA=+B6;60(K?<:^.@DLQW.AK8O7A),''>T=BP(H1%@S$:0V97,9@ASJ\%6]]HP%?<X6IY"&I3I8P1AV%&B&U!C1%4N+WM1&%O-GJV*X1''BDD,0I#JO*\1$L/NX1U3&L9&I3F$:49''E#NX5%5''MY%HS&=[T)#R7BT9.])N[8C2''N\=YL=-ES''.5#I0KK2T<W-H3\:PBB].PE[*6>S@L?/0''P@L*4HDR-J@FOR!BD:+P!SJ4(PAE#W>B : TU\)C3T)HFL(WK_QY:EDT1T'';7!\?R/K/V0V4Y@T#RZ8GC)BA?ZL_SC<)0WC=*:R!YI\''CX!I>*64 YB8HLDZ);H>Z!E,!TJMZ''QY3:V.*VHW2=!0)*V]*$FBBXFX@5Z3*-V-\!V+B-C*U;,*5+FR-Z1&OR/?V-N*5+I&8!KPLP(?Q>X[$N:,Y42=J:"N%+PD<OD&J6OKD H+6LDR-+BFOR1"D:/X1SJ6,X9=KFPG29H5(''E;J%SI6D@$($OJK&^37J]YQBD7OB@B$*W$ZPEM:]N[Y+I_KIVWI%W+T%GJ-J^+SN5-TX%[S^*V J:D)U@W>S+.WZU2]K \W)L;G5=>[&*7J2(&KGDII=Q@A0^X67V-"=69[W].;-O.]=7''O#-<%;/&OR=:4>-H=[J7/^9=[7.3*=732''^>6Z7/_[%+A1<8HPF_ V:*S*VS9B!K'')!SK(I)(31<K,5@IKFDD#YA@UAP.LHV/#BFL:3!CWN801;>LH!C?23"DU^X@$8(AG(>2*A7T(8+W FK>ZP5U?TEBD@!!U<DU4)A5<96-_'';E6-O"$A;R_BA(83)@&.K62J;M)V1=]XEL="8K-(,N:HY6V\^)-PD\3$#E_NB[&*<,I(Q!2FRT@P@=%@CG;B93V9>L93#KN\7T<HG]Z:3&>=,Y3[#.\=37+N^<[3''N^-Y3(H^=I<O3^\?F9(NSPB@H.ST,Z=]^X.@R,=:;B*JLJS D@#(L* /P*+=2OVO(3''MH_0@ EVK(]V._#V,X27+V]N:5+Z>MZ93+^-]91(@X% 5@OB  HV,[74^_[BGMO,,-RFR]AF5]F!G.:4%&4-?%=2W-T<+K/?]4/Z6'';26R?4ER&7[%-32R*%,K_!J1,''R]0IV3*H(!QO#[#''T='';V&2HMSMN(&C1!$HPE1O@ILM0 D0T7^H0N#/BDL?3 AW?80!7N<A!QGNDJ);#DE3;1#E\\81WO>LT#O/FQR53!E8]803_.\XRCPPE^D@L(9DR,);F8PODLW*X<YH]M7K/''C=D+ 35FG)D(X@=><LH&M/FD)SN=:T9?N-R#K/V)T;7*U+<:5+M>]ZT''( &$SPAD+[UEURE''[A]=U(09>%''PTN_FH8U SB^)K''CW=''9G?&QO#U37B.:=;7+7742S#D+>7_6%J862A#]$5ISA[FQHEZGOH>?TL@O?)6N^1T$X0AC0QQ"B<O;:OK>@+B>\#/9_)A^=:UE?^''Z-?-08QS5-@?XI $!:*)P&5G@([Z&:M PS\;B@A"H/?EG3-]S''Z^DP9$@DF,Q@BR&8!AJ K?7(T7?:5*<>=*>/?^5''/?/Z=3;8/2?><H>??MF7O/\UP@$"(LDSC[/=A6N7+OT_A@EK\@>3N7/,$\EM-H8,+^]5F;)QF0E64"!U$+#IW[''17[;H%&4QHM:A4"S95++E4$QMRK=!2F]X3(DIG: 1U0+AT>T1!US<V<@90QK\S]=H4.F48@*:HM=HD ,^3 3B(@72S^KLW,3MB[P%25L,BDO WDNDPPH( Y18?6COFPKRAE61)@9ZJI<OCI/]SFDBTJDUU"DVT"DV[&D1YVDB\NDWS*G]!NDU!*DW]*DUW.DX*.DY-"D[ "DZU*DV+*D\0*DYT&DJR$HMD@D"<MU7FE>@L\UV]DW8]DD(ZMS9_D6U8P1HXX-H9] B"T/!SVJ=LMA+TRH$N%C]"Q.OO^BOHYB9\ZIM7P-PZQC.;U>@AILK!UB=HRF(NQWFRL#IDMUH!DD([E;''(YQI;YS=;M#\_Y:Q<U4&"!-M:Q#^1UYIWUJNAV@(;$/ %X/,4U8HO%.EED#.%]$WZPGO.BH7D)>@]IE2&HW24TD 2N@@Z*K!6]6P_ZK''#ZH/]!,&,_<#LM8]8KD#$@%YN<+#8DEBI(AH>?7WBCXE%YEI,#%KH''2ZHZ7M][![;_R_-I%V[+W^@.[T@3J R2'' [U%$.&7$IS) Q''H$RDY$[9''T[;F[+@PJ6PU$3MA[A7J#\(E (11ED7+IR)  @C!AK&K$AN4VJ>6"?LP#-:GV@[*^TI;VO*[K-[5VQ?:"ID[#C%*V6>#NC:HE<FBJR2I!_#QA4CUO\81D4_''@GL# Q#Z R-6#S()^--6#Q19%#:V%Q#:YV2)YAE+R-6DAD>1!G;9_R#+[:9"],:P@I"B"#C%!2GPTU+#]JI)TAMDTOVH"Z,EK7N&X@OX]OYJ[7$$"K>*V$9W?F>HYCGSP)J$A8.N58*[!DAD)DV*ZY&*^)&*6I&.>9&+F)&/JI&3N)&''BTDZA2)]QW&!V5%3U(.ZI@^\% FS68/3$CU$.XB\VX3:F7-1IX 5BT#F:B6XN9[7H''UA&I4%5H+\8YR1B9Q?A&<''P%SY*6$Q4BM3=FFN.I7.6)7.>I73F)73NI76RUOF0"U8)SZ%MQ9&I1C"FYVH^)3$2)&JZ86IVX'',N(2QF$HABI4$=H?;(%BW*8$198#L:82\"IRS\ C>:''2IU&,K41O2UC])1U#9AR(L1$/>Q%''SB"0+^#P)"J@OB22WN:B*1%HP*6[0,)R)MHN -)TV*F7@AIBO>?0Q\[T^K\ZAC\DTFMJ&SO"&TQ*&TS"&UU*&UW"&VY"&T[ HS9NZZ]H:>1U?M]P?@"PENM"]MXVDJ^HK]JB"7JR"PXR^QFV %^$H]Z.FSJQB7\^IO=&%5O)$!ELK,?UJ<HR%(:\:<F]!EELHSJHHEOB*$Q**$S"*%U**%W"*&Y**&["*$^$D#^LJ'']@PQ$E)_CU!7CLEW@.!G,.@W6*&+P*J]0* JP*P)_ZF-7"*\2.*+K&L<#)._K&AZU-.O=.QM2Y4AXTD?<"GX)V"CEX)TCBK80M!X)HC> T3#M\=HHN[]HXCALXGARTH(JHF]:&J^#%,2>)4%M%$;B!%JAZ&EX/="$:%KJT:Y9RTJ,2IE%$G^PZP@IX#AG#3B@S#C@^CA0@Y,0QJ,0AH,0!:,0R),01Y,0$H,04[,0#(,1S9,1T*,1T8,15*,L>AABV!AIB!@XC[U[S3UJUHY3YBDK^H"\U:''I7RZD3ABE73BH PB@$A$$G''KIU3BOS*''SK4T@/",S4JBIBBBD2R-D22BD6"@@"#A7Y T4HY^!%I(5XH"TU*PD." 3KFL4O5 F&T+FI''''DBS@?S&D7H"AL2 CAK3AA+0A7LZ-7LK-6=Z-6<8-7.Z-7.9-7M*-7=K-7_:-7^;-7<Z-L#@CHT0@DR3BDZ(I<SGO9VFGNAHAGZ"*#(TBD6S?%PH(@AO\ K#"SP)H0!0(0EU1; 4 0D<U(>!2UR@(PB"$ HZF0"FD5^X2 RQ@KY1VH)?ZX=GN*C(RF[,$X7W68%62''8]ZZ<"@3P!&E($>AJ(^D$PI55:&4X*V5,->R0I\ "Z@ !]4K2#T;B[ ;J*>''-R*7''R^5%!NI&8)FTZRD''LR:4VB($;)S4%Z8EB1T:MH;4&$AD,:1BT\PCPH@"_$P@C<0Q<T<@D[L@H/,@H7\@H?L@MC,@MGL@UO,@UKL@YW\@X+\@;<0P < 1Z@Y\&&BP"J8G".$46^:RY!@PH4P"R,E18P@RW@"C''2X+"@PROL \<R:@++;+%$ "Y$P J\?8,)WTH#8HD_:@@M6LDR44@$ZDH!''B<O3:$T 99O#!I6_-,.E.F ^ XG:^>XY\WMVZWORLH"JLACG@HM;DD_H@D[*@@\1;D\33D]5;D]73D^9;D^;;D^28D*ILDADDE''+H$R%()^[-F,IM<^ BT$/B0''XPDZ DHM5D@$QHH" DH"X@@S)L@RQM $U?H%T4@F(@DBRAI1X @]TCH%U3H% DHLW@K?ID@JZH@"_KH%T4@"KDH&2NWT(&=QS*]&4.V/A!&7\N#W&PWS[D<->ZV4I*H*W):JN.J4)T/@ HHV;HE BHX]MHDN[@HBCKD)C^!IKVZB+*\<C+M#A&6<U*!MY_?(NSLYN"-YY;[[W<GU)G$PL"\U9*R@C&1@GA!CF1"CP@<4PQ^4PQ<4P"^4P"<4P3]4P;OBB?3B@$2CC<S@_]+GB_D&\DQU#T5G9)5 K)9THVB@>71BCE#UI/!@D8CBK)M3-XD.)32A ]Z .4A"?.PM@*P4F(P4SV%/D=C@I&"BTB=BH+#OH$A-]A9(I!5'' 7;K7$0''N&($Z#%%56+HL<N_]6S#M"JD!7SJP3@ACQ0@MI@@K+ABD@RAVZO5VZ]5V+]ADEPBJ5RBV,<5V:>5W]O5W]\5W ^AV<N5WK,5V/?5V<\5W0=6W.^5XL?5VS?@J-2AL9S@I*CM_W##O\\LL(??X3$.(00.P"C;PBUG  WX Y+M0Q@T !](@Q:,\"SPPAO,7C\[P (( !#(@BUX0BR/FR"  ["^B6030!;H,F5W, 84 PT0@S#KHGKB%# #IWIG9''N*)3DJ;3(#DK]$@%:B''SJ?3D@63H\8"0;!G6_E"  JU42.JH;M"2D  @W8 R,_@%XE@"N8C0L8-Z.N5KZ9:%J?]H2R;;WY%%I"KS+2:MPRHAXOZ;#T;9R1VL*DC''\4SB9!3"W( CJ(@@%4 "L8@ % .HY''NH]/.H]7NH!;>HVGNH%+>HU?.H&KNH*W>H&W^H!_>B@DP2?8@Q6 0P#W!2=5[_;RWHOU!@$JY2;J?1S^ZH@N6HDFQOG<WLH\QDHS]LD783]M5W\JZHH_LHH)53_^1D<&Z[&7-F(B!@H#:@@ZC@F,S-]J^:;4RPH#MDDMRHH!WK$)Q66ZY''&,L&Z,M+HJ^&R<1*V;TOT4#+\X)>SAGD]U3-MAPE  GBKZ!''T*!@BF$0B$Q;*$S3*%U;*%W3*&Y;*&[;*&X7 -\LHAMLDSWDJZYFWPST#,PJG%: =1[(HNJ@HZR@JRHLI:_4HJEHH">DDW3LDM4L \.C(^NHDR0O["_LH\2@"NY@K*D%C6Y,@^QDHLR@JNL@BSL0JRX8F.D%:\18?N1"*W)>/8,!X0;)R\="L^-A=*/E,_=_>N,+!X>C@@*_.L>ZP@QHT-/EUZ-$(3\''9K(B)B,FOAU*#DIVP@B1!"EU;BCPQBCL0AFL04@"!@-DL_FDR1AVFA:JHAD52B_ZM[X I-1:-%N$I7OVZ-?O[.S0&L%KV[3KBL/_=Q!6#YO$,8DN@BBZBB3]<<3.^<3.<<3?^<3?<<4@^=4JNBH:SAJ<SBAMA@7]0GKFXLG4&JB+V,&V*9?W#BK^:AD;!U7-#MGM AD\P@E(SBR^>61ZNA:0[B8-BAI#BA;I:4I-R,D4 BICEAH=0@;L;OGB@B.CZBR%,@F!RB5D;WG''PA5M:M7X0HDR#@D/B#I''1BE7SAI"#@C''/BI\C?]1]$0LJ#0P5(/B_ \@X$0"\4P,LK[5PCNK)8Y0*-T3H3G''&F$TV@=PGTPR^$0]CW/.7_O.;''_!(8P#G<0SX3P+3WQ78VW7Z;S_IQ+**>22''SPPX(@RX$T00P P<(.1_<.4J0QP(,0!9P0!0,/ T4 RY 0_U_D3#W#2-%@A940RVL?:[8PP4\P!T*0RD0 @X40-L6M1#D @YD.1IH@!#$C4@$ LPD$RX4M2@Y0 HI24JGC!$2WN!IX,RFER%^/N!)H$RMGCOUHHH( RULI8] R*&RY\.TJ8\,TR$3YY %"B#!DRWJ@)"]N8\((DLA$,&W&I[@SH(R9UJVS(^HN*PC?0>CAA$]K$F "L""EHX<^T) RDLSTIJ0!MKPAYPWQU7PJ@&[P D#TJ@>N]FDXT;XP(":.D7$9LYC!% 1U/2(^JME#XX[P%XLB[G!2!LOL4;<6MB%F%(0^@+#<.SR%:YYG$6:EGWLHRXC4S& 9N_NR7X6,J%D@%U/7;>AA1\>''G!18<^QI>=M  P WQO,LE!R&7)5:=ZGKF+"AYH(4$1YJ34JD5LXRQ[DKE+B,VJBR6!4>IBT@@/VAB$RM]&$II@EQ)KD<*RQF+*XP0L]@F"B@$RX2H@FK2J! Z(.L!''+BS2\JDP,KK*0(9ECFBEBCC3>(>20R93X(0,%1#I$"PQ",O=C!4L<TP@TF.2P\X=HX/!J"TX(HPIGB6* X1DM0VADQ!3)>NPF#"H3<SD)L<*,O(,LJ^PSK[3@0+.#Y!J/JSGCX0(&R4Q)Q@-PZK.N.$M$\:VSMIR+4<8;<\23-48&FVNCI#IPH 07BWWSDB^HZBRA5JA**UFU!)"CBC+&*L<0>#Z!HXMPW!Q+"A/0(NFPTK3(:!JEF#K$A"?60F@IRBI)P MHLCGDU+B*WJ"3CH!HQH''5D$  4#5<NBP-S[20HAIJKE D#O)R&BNCYQWIXAEFDLD" THX(BBRF"1HIIAK+"07(7LUP01]!=[52L*GL &RID-\F=ML)N)-+RV[DJ''?PR]QX.#.)2DN<PDTRHX ;3QF/:/7)Z"X0DLG*=";JL-RL["D/(T.XZRIS9PH9QL]Z''!"$1&D#NF>NTS2H L/YJQC ?,0,HNNK#YQAH=DB*.XL,04L%G]3R0#V,*N#([(,(#X<1%(#IQ62A@%O%-D-J[BYJ+!^1$&3:XX:MB!3Y=R4DDYM''C!S\>56V;[3$EHLLJLOWQ(YK)B<Z;MD"^ZTBPETQZ.M6,02T,-CCCPV0R,#[QUX)D=EEEBHX/BV POB$JI(PX''+?ID :D2P\A#TF8(9@YP@HB@ THN:ZKWR9[H0@<L9EJH #44PH@ITGSXAHDT_&8H1S4($BREEB9I?0FLK/1PIIQKO"F1]BY\]?8&G^#PQIICYO]#O0R^4LFKP1CH)@,]L B#X(< $<02KJ[^D!P/3ZQ))*8[[,7>%4Q!P@,JB)D7TSBAA ^(093\%$@EK!@UZRAAFVZ1@S=$@@DBEN@RT*R(!K$DJ/)J"RT$QR%KOXP"FJCAI<@ DH9)X$] PDB)E*F1"V0K@45H1G4,4@PL!BHER+"DC8GG&T/0J!F!L@P$$G I;W#A!Q&0 0V\4H ''4FDO"6CHHR3PJ2]$HB\:\@IEE*DCR#2A@X2  0<ZL\KIAJ42& ''ZF=77L8*D!@!(.A+!;)^Z>)D&I)Z8RYA6,(QACP1NQM% 8_<D]9*T(LX%T*DJH,Q"ITM,YP<YBA4XICFGB>4!V3GP PW&D@(D7B@Q7$-@JG"W Q.D8 ZL<DLSF DI,MDACZD(!@HR(PMF)@AJZ53ZF7OEK,(ML890%H 0(<P>1#0-R!'')3F]BD0ZG$ZYQY\*ZG/^5!M"H#S*76X@*TJD6A(:S''GY2QA)ZLS\]ZBHAE,Q[]+YCO4X);BQOV^P=VVH^B0C@BXYPXTR2=@S"''^)R''-CDI2V!N\9]QPL4JI9A"\BH>$ "U$;8UPI.8@L<JL@SG#/R!&:'' QPT !D4PLQ:6HVER;C0\!''81B\(P@$?&OPRBGCBD2P!$1+Q0P87RDDW=.C?AD!\9PZJ<LLSK#DGN=Q C,IZ0!0,P@^R=KH19PHZU''H5UBU,*T.#(R\6B]^(%\0SW6!RD9,D^H B2(%N9WS+V8\C-26(8 5>:DH(7E$(P;BP@Y;@H38Q2QHEDJ-RSVLH_V*&B@X)X@:HXHPN/N^I+^BA]$],"B\.(XD=G@0RL0C@D=6"B@/@IPT*WH!;^ATIMCBA,P3H @:Z(@%O7B@RE(!AHYC''AC<@D@EN]MH%B''F (JZ@BS:0064!(QX]NB<,Q''/LUX&F&WW=,"G10,N<;L%ALXDUW>UY@!(((XO+DL1 J]# ?Q@)D34*ISTC%E!_*P(?IW!ACFJL1KI<P@P;?30!EKOU0@6P8 $%[NI#!Y CG"0 BQ]!(+!UJPR/.*@!KJP A''" AE+":52$L^9(O>-04ZY$6J(*SSO+>3CP)DX5E%!-MO$[#6$X&T!;#PY,7J1M6YPA!KSA%\\=W 8I8DBLD''#QD7%54=:ZLHO.#GV;45R$R0:W.J,<I EJ^@HQPDGP#(!ED7YP1@4B,S$@LWP(ZFE!A.(C!!+PPLDA\)4&K$G H0TKB2D]*YKP,A:LYKXI_*ADCPC-@3)8 PEJ&G@!P FFP<Q K_*=@V7)$@%L!NUP^7 B@!+!A1<,  DZX@@F:@B@S_ SZ,;U\O3V9BWW4JRC8-%_*>,)''/9)H_<Q@\0[@P6HPA?/&)1):@P+UJDL.2I $DZ&C [7(B"2W)N=BX/T)@([MHD "  >>EXM_J@CSPF8DE31B''5N)E%PGL<K@KC0-?8LB$S0<"F]<Q"1;%,$;CD"EM,JQFD (PPDKHHH")ADI.!@"TNLYP"!(H@]ME@HQE'' A"D+A@G14BLS#= 1SI/LK:UTZ)BH1H;48"97X:I]%O QI4PX+5BH8/ED:*=0:5VIH:,2YR0U +:S<,GMJTFA5V$K,3] -AL0D@&;Y$HCI\ @K35!BD%P @:M@HL"&&@ACB1":!%(@ 4B8^D6S ''EES_&\=N%8X+''Z%4YQ:X2T>TY4M21.3G?5!)9XL5HO693[K[A#V;D2V.=J7@P#(#BJT*@!4V44=#T R]7/"QV+&4MT)X@01F4(A8S.4\SQKB@9C0RE 3 (P,HTN!<2E0<KBBJDP2!*@<P H&!]%PG-M-DESU4%],5H V"L2&*6CT<G3 ADX! @BHB PAKI[\QFZ@@JB1@@27XPQHJ(@P%&KBD>"2!</?QKQG.*:1H!D5QS/O%Y#RC:/%="]&0?/"KEV[VM]W]D% @0=?^UL@C-''W/=V>[H5 AC@!(XP] PEO!_0JC&*B/3..+YN0%P"#ZIBH,,LA2<B@R7LHKPBDCDDEC-LHBJN-TIFI[]H,BJDHQVJ@KX(B5?1"KBTH!^B#"O#3F<$I+@.L"KCBKBQ(A@9:@DV) C22@BPHAC;0 EN(#@Q@@@2!AD1C D8 %@1"ADY; -WQ@@5#$W\XNB,6E&M(G?D@"I4!" <0/,@0PD4#NW<Z+X@:&@A$/+A@IX-:K@S-"$%"E/4YJBXC(,(HK@2\%DV* BR $QY; Y01!^^"@@]RLA[AG4F9.@1IA@XH''ZXP&DS%LW[)N1H9IZISIDRLQ1Z(0Z*P+HURL1\ *9C0QZ93LW)RB1,J&;$PA1=@&;>1/E_F$;6;AE1:ABCZ!2@A0I9CL[0A''6UX.+J@L\]I#\[XLD.Z@A#Z@BS[$Y:(LEI+@BR8!A/<(XZD<)<20 HT$2!L("!HPPBF. !DD;1J^(@$V(WX<HQE0)?YJB!DV99"FA7NP*A7+ 2O@ E].Y!DP8WM(8@XN QI<HO*&+?J^@@2400H4(ADZ PDZPPL48T$"877VJLSV\J-R3S/^[ ,;BK5"#]%&CXA*X0$X@@>^(C)0#Z5X\RS1!@P*PP:R T,$(] J#?*RSU!DK"X_I GU:KP XQD6 DH 8X\.8QK8,A@0LH[@CW9DA%BNJH\,J*/^ZE^0SAK23X]<,"F6QQO2:8$XHQG:;Q@BPW1<TB36#QH4HAR@2,.RQQD IAE"PFL,8,QLCR/\YV''"Z8883!MT[#T:4R;?6R-+>@VP,JNPDLXL649L<J&=)"K&J&LI. 5C,F@H["UXNDHIML@NJFDS4FAT-BLQIDDC"FAET&W);@@Q3 LOL$@AVN,PC(DI0D@I3J;+#@$.R801%IHJF9H-)>,5!P"ZQBN02&<<0@NOT&O.YLLT/P&\UID$#]L8GJ HY@DOYEE @OC09B&=/,*C/MC1H"G2TH)2OJDPJJ@IFJDP OFHFDAHL,DPDLDGJJAB?L$IK,_L=$B"7B=B9"NRPHDHFB@E/#D\ 2TEPBGY3MF$=B37T"SBGALFKZWK_@@QDLBGELB8U$$Q]F@N5(L#BN0IB*DQ3BHS$J^&EH@A''"RXO,1J?91K?EQM<TANEGVQJ]PO+W8BD?)C@:)C+WJM?(:3Q(_C U: @JB ^V: E$W!TAI%TUQCLN<%IP[KA0KA* 8KD$0(TF+EU*XL_+ZBBCI&X31!BP(FC3S UWHH3"XTA-<E-S#/D! SR#''BD5J@@V"@C)1 C#J!DM@@C6( D0;AC"H!D>)#$#H ^4S''\ ;!768 D4 S@S3,LSBN:5 S1M@NB>I%ID+"+=:._-C/IF0B/LQKEKJ1M,!+7L!0/]3NT?$''X"[&FB6&6:!TKYWFD<@@EKQ D8SED!I@N2 @@QQ@L =AI)Y@@V#@O(,PC3YA$N %.A"@WA"2B--(8$)MTS-BT_>M9%@3+&''P[&**!.4,</1@$XN0"QQ-#F3LQ,^J44[A5S^X(0 88PBZ@J%*D\&TCGAX[S_-!X?J01\G#:(N01LB QH*2Z\8@ D4 P[4@7Y&JPY*9^''VQG40%COO8:"T@AN00IO(PLD60P<R@P2F@AK H5E*K7+XZRFEI4TR@PDX,BHT0)WP2!JT0IU4XC8HKC73#PG,0G,.@T9((AG(L%\)8ACUTAJY9&&6S%\ <&A2LWC0T"Z1A*1(H"+<Q2M?H 1N2S%?@-_&K52%E# \@Q_4PA*4PAD28_>LC]$4"C^9\G=@BM+TJ@VY=HS*[\P&RV\4 B8C[N''V9@ZVH@T,0@??&ED (AT#&OI#TPTEB>DI1N@SQLL&LKPF[$@RHEPSE!MK <PIC<PKK"DLQ,M1E,DGVUO,LL;D_D[DKHVNN*;I6%T740<SR&8''JD!STZ8(''*0L.:-!XF:*EBMKKF@OM$FF#"$,P"DQ-HBBDB@3_T@LO%YK]@@T@"DS@"DQI,@>HRDP\HPAIHEV.<@,MJRWJ(L"+O_;*&++PKQJ2(;"P$0JG<I]5E[-(.$.2^R^FD%_Q$:[R+F[;&87)#Y>^:O/Z.DO&&DB].$9%: ;WL8@*8''IC _2IH>DC"LE\NJS'',@IE*DKZHAD5L\PL(DB> 4Q".=%/TA.EP@OJF,N] \@:H@2?8VPA*3HDFH 638ACSRAUQN%I2N6A BQR)Q(S[+RR" BCX1+D0HACQ[A*I)@P$=''.SA D;3 %_YC8^1 4>(Q@3<APJ;7:<HWB)5IR; $N+$FSEAC<X#4M]I$?]H*S''QM_*VVM51!@KP D)  J(1-+?Z@@K=#Z<ZCI)/KHPQ"D7Q X"_GLAC3N!VA@WHXDY1 ])DTB9Y@;^YLL*+DOS;AKKJQ_SAKN1S!S3N!DZ9S8B:A@\9H@>X@C_I#C3PAD)8.(!P $?H#0R9+L<!NQJ/*8"1NRQ''G.+CK;QR&SJ3)MV8BCDU!T3QUCO=2#9)M_;SK/TY5_^BG9 8 GO,VWQ!@A??,(@,0 ADT0P88;4%VEP=<PAE* B+P:C;(^@L(0I$QKN.PETP93H''YL)VSZ^0R5X4JMY6''QE)W;F**=PCO;5G*!U;P(G5/3F1RDX3#M04\XQI28D=@)/A.<V<B9557\Y9^H1PRA1 ;CD4U(DNHXJK1PADZP_ .P F8  "VJ1D&TAH00^C6 @800D[60@-2(-(40]@D+AE<X@<&IPLTHQHPPVN\C0_'' M7*H;O,H@LJ8T+\T,@4 P88& XTPQLXFAFPJAL\!I(1(C=)!0NC!@#0(J_Q@%F[]SYS9V]K5F"725M9>^424-[0A&*?&I?A%PSR8@/2(H0O V/]2V.EEK7?-#@5''$6DI <R''L@GF@DA()Q), PTW &&B[/_1"W)T,@K-AQU55$"N.LI;N@SE!''EONDPH,G2.(@BKDCP/DA%T,@I(O&L;H@FQE ,X.B2\<9_Z"A ''NXR[WOCK''EQ9]IQUP8T:[%?,0$''?,V<\''$(])&^P)FOSBL*B)M"2$V+MBDC@(G]!&$;D^DS/B@QF@DMX @C''B@S0 HL2. IF&DQ@@6R-.V9IS@QLH@IJJM]7L 5.P:JX]O+3K%(!"XS$^&W''&''-)N&J&:15.?BJQ2L6Y,M=/2&\4G)*GV TH*#@#@4:IQIRS?P;=B$=,+M=ABHTE*4Q4F@N#D\M4SR!F DQ?3HACDC<D" BCNY C,@ D2JDMG?/DDS<H":AHA  A$A\D *AL%K A.Y@@V(\LP+!02-1DYT@1$\0YDIG74A\TCL!ECH *DQ\0I @CS)<UM81/X''I%DTVH+.D?L(D.F=;/;W8+D3QSVQTI@\\WA7(@UK!AO2 A!P@+">H[0!0T/VH''(<4RUTHWP"B1T<YV HA:HH8"I5@1.-U%!)AP>''5^0>A@WIZ''MFTBY3 )SA AFO HLS"D!Q@ ]D@DY N@>BG.A[A)SK@BY @:U 9'']E;LYQ43<N7T^7H*1A:SFYY/?7H!">5N#[U/GP3_<(P&ND+<5H EF#\#R0%ASI)C!AB,/?@(JXX LW5CZM<(@XBHR3(@00T(G!##86R=R5_N3D6%6_E3-R_^K7#66^''5]U]C''=V3%)_@1M*3AQ13E/KWF+;[!S\PM :3<#T]\'',I8<RN):GHL+D0@%$[)0)85X(CU\ X"2.($U,1W([T1JJQ@&FPB3<"U7FP-([\2HLW%6.]GK\\ D/''-KFX#42B1PT@AMD0S+7HM)O*>E[YF"\][4;(,+$!7:^KK58&]T<\SWFX:0E2J1)MM:O<91>PQB4 AH$MJ=\T(L.D-7QJ6G &BFSQ>,&X2^!,"^GB%\"@''&P"BD?_"^A!1I!<AHBEP0RX"\!8TKQPLH08XVZ; \7?@[?$)5T4W%''B3UY(?A;$XX#&F@.:\UDO1F,''60/?<WVT;\.<9/%=I.8*XI"E+A^F_CT73E@I''<IZL.E0Z@PIBED;&*D)A5LHPO&!Q6U$T&]097Q7<U=6C''\X9]<;:#K*?#U&\4K6W][;R878E_(3[0S#DDUHFB7"L6]C(=?[=:+"9P%KLG14JN_H$L)+^*2''G"D,)\#IBE,NNULK:9,$6[\SS?3R+&TJ:<FE GJ^\^N&9<QE?WD&@$2RERZH''W6@P.V-RX)U+S.[BH!X%S>3%+7#UN-_6DA@LJO#4AKQA$<"CB!JDMN"CQJ F%HQD1KID:4"OG"''C4>9$AJ@@$K%)@$?35!<QPR)\"PH4=:^ &SIT.QI%V*[I&R)D.VJ''WR!F%R9IIPEGQ$NBP)41L<ECJ=?@%TI$6]L2G5''H(SZ=ZVM7OR1IJID!EDBRQ&#E"1H-*3Y#EM1FQ)BZHZ^APZGGKHA2!HP3HNVWIQ;]>7_-4ND\TD#08F''!I,-V*R:,*[S2N[MEQ(D04_#CZA4$E)S$7HI5^N;G)Z:.''UU''62[ ''[*.''T$6$?;&)Y*.3V)''^/M*R$A(-E'',HXQ+/V,E+$!Y$3-3P$A 4]R!J&4JFLCR8R*K);?08>/O#19L.[O8<>/_(4IH21(^SGR: 0].-[\-KDR0)QY E#=N<\V?:E@\XQX/<,X !H,_UVV4$LT(VZ[IJ@D $B!$0&F48QO#X[[:3!Y!-IBXR2BQ5D8DFDTS_,A*JGL$W68X./#PQ\E5*@ (T(!@FXWH>C?\_VQZH0(@TE!R@4AAN_(JDPD3P\8D(''ZZ!GYYUVW(&%^(NPHDP)_#BCA"[56[VDD7,<5M]E3JTU8AA3DDEGHJWM^II$L&X(T(L(A_V"Z"3&:RJF\:;&2RT14JUHE1[(TDLL -Y4&9>(\W [!;*%%$$MQJAQWFAAA.X# FGHIUY=P2!@A0U<-^W#\QL-=1]F!16"@18LIKC'')UT1^IV_BCC0BR" THCAHR''@6B^ ,26((T?IR)*,)Q7?[-"+(IE6.IH!%=R AP[E,Y*QXAR5EZ-29A87QE2A4GE@]P!]X,\F**CBWY[$)]GC%OZ"(.>>?*KGG",*\GB#IFL*MLP"SR"RH;+#*"''.\^J&F8(E@C!!BH1\:US-U%$9N5,J$$#B6:V0''V3'' %896?ELJAWBAA(LHJH@@(FFJB#HGN.VJ1[@_WH#E&G4Q?R+8J*I]K$RUVRIJH7\:J9AR6#PQ@XJG_I$GUK>27WWW*-GP!*@"M@DGX T]OAA!&"2A6MG-7*.&4Q4=IE$%J9,V$<-4?Z,2 -^-_GIJQ3"ABM]YJAII)_0V^__H@M.\.RS;#2[RS]P  ]Y%!C&Z)AG?;\UE1(>:GAW40_!)]^*''^>()(A&HZXXX73.FU.K$>_Z6BVRJACHHXT<1Z3]IU/+Y8L!H-.[;\V?-/2_KK^&LWCBDV\\$D$;]?6NE!$W@17TVX\]DM-9SXH#9)?/2BC=''%_>%N2EOU;9 72]9RBN,BJGME)8\TOZA=7W!A%@ #=.@UU32,V9HXQAD!X84AI&U;/ LR!OEH1 R 21AIQM4F8U#I0DO>"SABS(IX[H&OG.!*4G _A''M1( 91HH(A>YZ7V-F5JQ#/R?P*CAX@%14 F4-#;:BWFH?2HABP@!#@''P8E[>D4VY3 PQ\LUPNQ_)29/(LH\9NX=YF%1V<5H(*_<."''EWLK$DFCH!"VL9Y%]^UEX[Z;NZNO)FQ!>CDT(2-R%OPF]MBN3#E@$3J$Q 3"A(6L?)TJV*MB%-WAFCEZ0LH0)Z6P)W_,,J9OIV/I_ ")JM05&]TGNU67D0V%:9B]<0-BGG]YI2$<NI-+#%+^)E3F N\>SR[&$4]W$/Z ZA50[XTH%:>R,M%Y!DJ0@ABBF<@!B-F@V=4ELIGKPBA:! QQQ.DXQ<!Z\V\L@AK(IH1OL8@!]HFH@VKIBI0:P-X_%1XQ<S:K@^Q\PRXKB@E!@TH[6ASHL):>_GIK +8[''1"783&[IRA$HUJ,.TT&F!E7JDR5)^C9_VR0;S''@Z5I$[?$%5Q@&\8O0+R<= OA*QPX"LL8[<''.,5:E$%NR:4H)21*#IWP*2''.^FNST.*4\ZF4*T=/6#N 3N:''Q@U*2_Q92)3RQ*&0BT/&2''K@V);+XQNI253*H )E @EI^]%K&.B)/[@JJSFK@U8X!=^''R<Y1MI^\80X76L+#R\.S[GP+<:85/@#:KC#C>U[''[E%@\$71\7@Y@!)6B[;,#J>H1$BBFW[PB1EDU!]#@DPPOO(]D(2"@E]XA"*J\H\Q+BJH7IDCL_P0"/&EEC7\ \XQVF@AI)!.S@CTC0G;0;)P-T:AAQHC1&ZZP)_TQJ#D+T1)HHW\85[F.LLE"''B[^53%H!\&?=D%;''N+"=7$V)^:2WLQB7D4MD<Y$D5S%ZG''[F $#_(P"NUI09ZLFC[U/ >>*.WW^>DKO>>8-5>C.F==,]Q_>;+//>EQG2*6YJ<4NJHUY("FC#RQ KP=DT7HDV-!KGEENS5EU??4)8\;?D<P_=!N$DJ+"D<<X!QKJ2U 4YS&V%U!7KITKXD,%R#PLLAC)$)5DW6''Q\P5,T#V:%Y6IBUVV)%S)_*$+Z,$'')NS#C^^AL(+3^HU7)PZU2MC8)W]:-P#B?MN"O48WLYYU;,R8,-9BWM_#) DI3Y@BE+,8@0''TDX4S+@KU,3O(>2)1R0F$@MTF@DJ4("B [.#XES\PQ%?0O>AH?R[A,32R=I\H<DC]/DLKTS"DOPYD3,Y)"LXE(6"-D1WOW6[@@VIL$?C]T%6''5/\V@,W5-Q-K''Q#[^/%[/^:.N95+V]]&(BZ9&_1Z] KRY6=*W:9ZT<CAR<O!#T([Z6=^3:0//[K+4%?I=K\W->TC*7-[.?K_],63;T)KUHR&@HE^=BAA#11LHX8IH(S)^*9EL@Q&]H1JAW,-;??C_A?<3/ AB>80P>N<G</+3Q!FT,B=%!Q0$XLH:F#AN&\6N&;]IWGA 2UW6HH.;H""7X: 6.HGZP+2=RQ^\";J<<JZ#2T#;1R@*7)-C $/[9VK6$<:#":I"ZX3WW/^0"9##K?1K]&^;WYE8PP1B:B<X,_*N@D^2#EKZQ$1G:=K06,ZLTW8H@KHV0@@''@0\O$:4P$R=NDQ]5 E]=#S"_O5*71F]LR4!9#(UL1@B5AP C+-@,@Y;N\?6H.*&"210@[",<*>JUP&9*@@A\3!DIJ''_NP/#?''LZ3;3$:?<9#?_>\9?W/N"G?7%R6=:4E,><*JW1BT45JDZAX4??.FQE@''?4!''64XZI0JG?5''/7@-="E&X !Q&.L@YX8JHJQ!"A@X1OARC\H''7FRHT/''$<J8>?B6+"HP D>DF%T@HHSA!BFK51!#D][2[60$DL1#EBGH.B"E%G8Y''#RLH%_5FDRG9!EDYI^?2UF]@-=T@I88@P9Q"Y, 2XB8#A>HS]XEF05,PQK DDURHDWZHDY"HD[*HD]2HD^BHH\JHH!RHH#ZHH%"DDT"A,<@0%81B%;YFE_U''!LP2)DXB*H1A^\L7ERET>3L&RT-D=8,6I UD$''!24FIUBU9A)9QVT%)5@D9RIL9SO[4&W!=QXSI8M!E"R:QGPG8T/@AHAX4&Y7@@E7 @)(!7X?(@3ND@2]X@3WY@2SY .7T@R, @(-T@Q% @*0@@GID@W,$PZ+<@-:@@IEP@0[P@V+8@#\DPU_(@]_4@)A@C>+4@*S\@2)@@,8 F;><&")H@ BLP]"X!_,9@TCEC\:VA"O!O<IAMI@FJL G8PE!4@AMT@IO&BK-8"K.Z"K.<"K/^"K/5"K/2"L04"LO$@I$]@ESLAI _I]CRMX8TIX\NL*E=U,33X&4\Y^8!E)/0@A3R@BT1@K &@F E@EJ!@MGB@C,R@LB1@@TX@J$3@B]#@@0"AY0 @M=NHH).@K4*@J:PLKH7@FY=@KL7@FRE@F=''M6"5 >9(M(IAA7ZU\>NR@HQ(@D__@A*N@JI)A>B&Y79#LH+\@I_Q@E,;@A+9A6:XL%Z]@I1>@K#8@GFI@#],DP[ZMG8!U&+WMEL(TSXA@CLX@FO_&SOR&TP>&SQC&TPV&TP(&TR9&TS^&TS8&TSQ&URS''?%T?9%H^ AB>1L:ZDQ6R!RE!(O[Z4GHEDE:[RUR$@\W<$P3>G^8]AU+ICPS8%Q1C4H\R#\''P5UG)"%7M9I7''IQR3C%7)IU4;FM4,PGO_TJR0EP6?A\:3SE8ZEVDVGG](Q!%_RBYN0ZK;PA"^YA%?@A2TY@+'' @C''PB/3"BD[@BQJP!7+PA4  _''4XARP0BEG@BR\@ARK@B\>0@[J0B)4PAC?PA<=0AB^PAMA CHLPAGU @G?@B]J0@F.PW4PTM#< BFK @3G0]0!QV:YHP8,4FJ5#FAR#^BY4I9M1BY+PAFKPAG.0A>/Y''.0I''?B9''/L)''>=I''?])''>>)''?VI''?RY?9=M8I;=*Y?.2Y;8>Y<E"*@A&*C0RPQM8@]"H@ZM$DD\=%6&ZG- =RO$AVO$ %:>I0)!8@&! H@G8TL]QQ:C$@(\D@AF9A:BD@B#(@(G,@-U @.7<@\''8@/EL@)T,@B@4@''] P/6IS@\<D,?: ,K@@6,4@&0X@ATTI+FL@*+\@.+@JV7\@/N5A6#\@.&\JW6L0[O$@-24@^0L@%=\@QFP@J8L@&7D@U5&@X ZPY0$@NDH@Q.V@-6&IJN,@)78@1D,@$#^!@SQ&>^L4LQ@UM1L""W @ED,@]D\BI8 BJQ^"JSB*&QJ*&V>*"TZ*&[>*"W**&Y^*&^J**]R*&%V*&&*/>)(0*)*B*)#%(BS]@QE?IVI.ETK> :![)HBVQU 3RJNK 9T<U3Y>ERO#!I.QE\Q5TRKA 8#M]$09MO%LN,3;*,2M*,O]U]-WM%2W(2WAYKK.TVZ)FE CU*<8PI:=JE/QP/Z,X5X? FA!@E(5@K*0@HY+@AY%@DH_@C2R@H,@A7*W@BY%BI_8BH+0$ET[A Y&@GI?@G,"@M47@@''H@CIN@JTJ@LRS@K,,@A%L@FV5@I\^@LD<@G31@K*RB[HAT6]VAN]H@F6W$WB M*C0N.[?MN8)I8*OZJ5H(ER.@D_*@C#J@IF*@A&/B3P$.4QQ.4P4.4PZ.4P''.4R;.4QP.5S(/?-D^K-EW;-D<K-TMK-U*[-UZ[-EC[BI=@A@B @RX4+P8ETQHU,63Y 3LT]A#%[@"1AJJC@U^SM\F7[V&PB%C@B6#''BKA A''5 B*) A;@P@(,8BVX CXAPA+H @$Y0.N7CGPWPA;90@''G0(<S0CE>@B"E CDH@B5F*B#%0A;5 @DC@A+E0@&90''DV B"H $C$0B)6P@2^PB</PA:&@AMK CKL0B''C@BRH B@X A5(*B4$PAS&0@Q\)A;I@A)-8("R0B)10@G/0AHX$-6Z2T!,JYA"A[16AARA!D$PA@E* FN][J>&K/..+/#)0@N7K//G[//AK/?I+/?VK/?^+/?&+?0L:,@_%"0XO!J5S4YTO=81-B86]D3*#X9Z)<5VJ^TB*:IZ1P55A*FH^9HPPL(P[WGHX+HQ3UWL*YLD*$7OT\:,09''N_L")C=61F!7S+N &2\@A8H@7OL@AG$@1;4JL*&P([<@1+$B>]8@(CD@-$L@%T\@B'' @.94H_SM0,GH@#PP@CG(@*D4@R\(JWB\@B2T@F,,@).0@FQH@R($@M>\@A?<@N@,@(DI)6(<@T''(@T[ @XQ))78HW"79Z6EE2BFDPXU\2C$NU@4\P''84R! D@)  L"I+L"K3L"M;L"OCL&QKL&SSL&N_L"! @BHP@M"(@$O9A.?(PQ@ 2N!M*9ED?>C<OP#/N^!&BBK"M@$]8-Y>YJ/OE@LEP@C,''@DX6@LU_0EO>*PNP@A*WCDDM@GPA@GD/@JP](J)<@I]R@LI.@H^-,GB=@K]:@G)(@K'']@J,J,J,3@LB",A.''@E,L@J[F@FJ.@A]4@J+,@J,7@B+R@GOO@KYA@KZ+A?PD@J[F@CQ0(K-<@IJCBRF;@KAZ@K*+CF=&H?-Y@C[1@MS5@HF^]DB% V XV+ 7V(F%XZD2HF$[@HC@B4G=4HPA/RF#CRG07RI&7RIX7RJ<7RK]7RIQ7RL[7RH27SJD7SIC7SH.7RM 4,:W$6^H$ESH@9-$)EL!X PK\=)HIUXH@6]8FC"/S?LK$''+)5C+FR1Z#MA\E!=L,/B5U/-5U(M5!4$5''D95&9$5%4]5&T-ER,8![C$YUG5\UG5RCQ(Y,?6!\F4+*M0A0_PAF?@CL)@A1,PCT^@AJ0P@*$@@U[0B>3!BGU0A@Y ACF,@6Y(AA@@AZ[@B+5P@ U0!)6@B6[0BB>:CA-@BPVPAUYJ''QN B*2P@=E @IM ]60\S-DLB<<@@G"PBP%A".;T''X@%TQIQ,;=%%2%1BX.0A9PP@8XLA)J07H/L7L4M7X''L7MNM2L<M7\=-7]U=7]+]7M"=7];M7]$M7-H]7^S-7^C=7K=BA6IP-"N''D6#+QI>R0LF:(W@A-;04J"%@?<\H\P \![_[= -*H@630@.\\@Z$8JM.\@B)HBTJM -<8@*5H@,[,@B''$@PN<@VU\@,9X@J7@@^$(@(PBP!=X@VBXG8?4@MF,@CDPJT<<@1: @.)4@],L@%_(@)0\L/OD@BS(@*6V0@(<@))0@&B8D1_0@Z@4@E=,@E2PI/D4@),L@FBT@)7L@*URRT*VPPF8@^Y@=D3.[4Q%60[PS_""0U 8@UU\0NE L&X#@B!,NXH4NY1K.].G.]/O.]0[.]7S.]53.Y=_.]?;.^@#.^CK."E/.]+;.^ET@ LX@]Z\CYE-"0E?IWQJKLJKA]%JP(T0DLZ9<@QGLG"EQD!A9\6=?<!/-XXP(T+PIG*LKG*&(S*+:;*,L;*,.;*,V;+,7;+-X;+.9:W3L!WI-119 TXJX3T<1P]$W$PQ$^YWGNY]?@FY9@J O@JP)@J]4@HF0@MU[@F3D@K/;@%#/@E32@L%\ I!FBF @@A4 @G+S@@F2@D4^0HO\@IDO@G-#@KKO@F.''@GC,@C_Y@LQNA?NU@B]?AM4D,>IIC%@K@G!9@P64%;.J^V-S]*+O#GM3,)VCCHSU@CZJC]3G4CH=O]G"?22>71HP<FM4C2I9?2HL?2J3?24572HH?2L$?3J9?2G4?]H ?3J"?3M5?2(\@@MJ@E''Q3HMAKJ,2]EZ0K*%Y:!!-+?(^(E2>O!W+> CL'' B7\0A''+ #)701C?PBU/RBVN A*$ 8T_@A+$@B:>0B*1PA;S0A53 B"\0@*O0@IN0AV% B>MGB<H@BDX @+O0#+I A+@PAKA0A:(0BZ. A;3 @L<@AY00BV;03,-0A\$,B8K ]/C<A7\ C\* A:4P@B#PB"( A-G0CLB@50$&&;A0A-MP@0(0V49$I <A''"$\'' >($2HAA*C A<%82M3]<^Q]7.H]7()L7^VM?L]_?M/M?N"=?L;_?L[/7H#,9(7P:H=NR+P*E)22\3DH<]!SX3X(B)>@@D#25J[\E#.(!\QJY@&%5P)GPZ[TRU)15/I/?5!=???1#??;W=[3;?=[CQB^HFD!"FU PXLIBQ8<ZN!RCR4XOHWA!F%H1X,WKV:4.DQ#Q(0\O6J4!B$FGQ5JQJ5\^\''NA%V(RJB"V]O&SY07GT6:0<2AKT\5-3#0X6[TK0!V_*T!4^%KG&EFI''D"]@\UH@ C#@BB@"EJI9&(1$@YL2(@ @4K]LVJIT2PBF"5\)RP!R/M(I19=^;E^_]X''6!8MBQ .WKH("Y^T(#::GD#RL\ Q68LH<$B@B^FOBT\JEB!I2UN=%BJHR&TID% T-=PKX%5Z-"-X:M>CW/6:=) \+M>+[.5:->6 \<^G''28<M3B%_-NGX(AGSFZ%&1&"LV3H_<%'';Q80RHJX>SG8S\:C$=^,$UK(!))@ZV2<O/B!6 \\MT)#]84Z2"]H%@L!3FYN)''% EP:FRPMX8"A0 !#*E# EQH*,V,TNT98Q)!''<H@B"RK>&HVD(EZQP1@9V)%B%TE8. HVX6BQ!X5#?!BDCU#\DBZ@U]18)!T9"@$F%S,DJZHVVTQP)YT\(B" ET:. BNN@3 1P8QVC.RKK1KRRJVTI" IYH#7%!BM@T>F.L 1<<X[@+0!9"CB!4B,,0:RTK30H8MC0C@N-^NR61L6O?$DMC#%"ALTM=TP]R48QYM+%MED!4-N.E@44DDKQCS#[D:BAL*$A#0PRZB$MT\:D;K?T,OK2BMK%$CCA15VT&JI0(X81@]PHCD3)ESE&23U#4Q!@ <]2M1LHT"(:1PLMA)!0E%(''6U@V&*]+WYZZ;OE]--+.=WVV6:?ES]\\,D55=)-9; $ XT<L> :IRAZYJKF: 7),RT:4%__M_TMX8%@ZD#)/QQ4T@XHWFZ:\.F];%CF%2@LSLNQDESPP)A[XD%FDE"X:,PUBFI)YQIY&+DJJ6%ZZVV@CP@I2)D4NHE #AY2ZFJA@ R@Y^]_5"!"$#>"F\F.!X-^^NH($)"@!$XH^<<RI9*X@QKFQO+NU5;/-RRLTB0PX1ECD# HV^(<.RS*F-CP43YC%1N4[]4DYZ:8?=TJAV:7..N6F3V7B[7;M;2E@<L9F,SPPMM$A6HHN>5@:^;W^>5]%U_3< 6V@R4(J@P>3(V]+0;;<.)A26Q2:JPSQ;H\QL@R6F !"%P\^FZDRTZQQQ@2N"G!0BA.>TTOIC!IQ! WQ#DCB#Y.LPZPH+<08(084.CICAAX>^TNUP@QYHA[\I%%@B(&"^NYWNQHP !\S.G#%S+NLN@VUFP!Q@T8)DR2&VA "ZVOJHH2V"]T_HDV_*BD@#@A''2U(X =MN<?$,KX+A^2AC''NP46[@@@(?_BIO1<&[(PC''-=WHQ&?E.XG]R("\/<V&M2-L8]1XFDHX3.5-\SLNFCQ A40-@_<!"/OLY)!@BRJ 0QNVLIW%+@X9*?4KDYS@P>],QP\J:@($HPEOQ49%MX.@:Q@:0@LCD%B63"!.S,<9@AGL^DX4)%FMZ6Q#F=78Q##FTX917@LQ"NTERW#BL6ET"DL H!FJ D^P0L*ZYM!DFT0D  8G\@=KWKHAM%QBX_?KR<NXLPHR!NA4ZR"FF[R@ "4H@P+R>LK)AARMA\A!D%R(B"*L@@DN7HHU0"ABGM* RVOPL B#JH@2'' FHD@SSFB+HPR.B$@N!6^T>%FP&SPYA %ZX0P=6Z@R-8GNX1EAMS]>Y8#X1\\#J^B432*+NPT@#F-J \HY<D2D;[[L[DKHM[.;?=NA,=,YA.QWJ''"&LSR PT[#CJP-YVF@\^;(SFSQ],X+6D,$ 4;N^=''PNO/H9@N"V6Y\40DHP\W@DW&"R!$;HXPOB&@T''^K@@J$PAEYOH PFL@IZZ @!5)."CB.23"2'']8P<(4HW+(*&BEH7!C*<0!!A2HH\(\DHD2=BCK1[0!5V(P@QQ4HL,@HDJGY% E7?( 134XHX#EB@W,1"AJUPA!U2P @6B4@L.OD)IQ31 E2_PP 4N$Q;8FFILYY("P7.UQS_9 HHB,ZB].,BD4;R0'':EH[C;''&Q*:&UBF$])[Y^M96WUJU(WE*QPNDSF]1B6$WU#8UJ RXJX4>SUMBZ5H?:,P@R(''7 (TJ]"UE_GEK5=Y3''KBH)Z16,VP3W "AY,@ A[6 @_$I#^9]%Q.\97;WN!F=;''L%V95+W/]:.*@BG,@0A8BHUC F P;MVCA/B!"**31R;]X[@1ISHJRQ*:$XL) P<JZ&YMNSLHAN##CC3(@"E\ @PTG6D@AGMDJDYS B%E0PR(DLXDE#D0V3.CAUSX@@UM4P WJ$HXJJ-@JUV1 @+JX1B54<P!"@JHBK*AJL&BAB$848QQD.2<%,=PJXV#AC&"0I''2 5 QEKHZP/4)-&"0RA#BH<8."_Q]''3HX6-Z5M!Q4\GFO5ALK#YG&]VH[,%$7C6ECP#UA<B+L<-_=,''AN6$GAZ$D9A$(V, .[JN9IS5]UT^4RK"@I3&''/ODC3QX9X0XS;5.V!N[,FFW6SII(;H1P#:8H,;3BHU$;#KTP*0"$''V9D@8PDH0+MRJNE1AFK;00B :,P($B@EA*]!EDR)1"5P@H "-&LTUYNFJG; BA8BP02)Z8X(".GHV,8!B(9NP 0K,@!A0>HT+I!FLVQR!D;]PPR)FT^"#C^HE+#"BE"KA!CA4S$0KSDAC<U1''R;!) ''J23 T3&J^=!_$F&\ DZ<(\F2;/B[M=&6DI@X\\ O=[8G32M<DC/.WN9%B48_TTDHU8W/W6*:>6QZ<R ^!DAT@QD$R,X&*=.Z+?#F"Q"5;\(9,SH-0''"HDRF)#CG@K1\)_C?NT3)7''M[U;3&M=\93SON\97//NX=;3''M!?:3U6>BS3,@P4UKB]A?J"EQV@!CDQV;W$>[--VJWI !W%$SCI]X5P8X!QTNL@4''DFH@13 DQN@0B0&4X$2*N@MD<"CHO" CC8PX6QT,\(+(]@?(F6 FR\P!A)J@@EN8H@DZ7!FB]Q0!,Z?PQV#ZLL_:NIV[HL]O2P P2?<PHM@@K+O"NGN''N=5;/N$RQRV>Q+"NBUF:51"DZL)S[;;1C^6"[&^X!83OVTC*DV93Y:89:C.?2QB94AGN.P$;D@J2)6)_1NI%[,"^*<(?X^2?2^>&FABA&H04\>ES">8LDXP\!JDT^A ES XQRUP8]D F@LW^2''?Q5E!"5TT8S=7H[?9TQG?>\NEHI ?UA"EH% EX: D>PO@R!!@ULBETY DBY&D''3DFU  BCFPE@YR?MB@?>VL&D,@EOY@FO;B@S@ 7\UL L-$UP?J+15B7N@$.R@@CK6"BK-" 8I,MA]@@CL@@SX"ASIB22AJ!SDBCPL"DK]N=/.DM1V*!7-",FHKB2W*!PRD3S_B,:S"9:"@HI @UTRDU:S,7H;N>U$DCR( U3+DUWIF"!.(+H9(\"\LH7"*VI_.-$<NBEF@DO5@DAU@BITB@P$@@PR2DP@QDP?<$1DAL1DL<QDH41DE\1DQT1DZL1D^T1D*DQDB41D3\1DJ41DX\1D&D1C=DAC,HH.Y;L<TYK:"#E2-:08*8KZ*;+\\ H#R@K8H1FHS9.!(#@VMH!PA@@PLPA%40@FKH R> MK2X!@H A!D !S=P PJ(@0S<AVMD!U*H@3]@Q!EA U  !%&H@5UX@:C@AU# AEH8Q0^@!%DX!BC8AT9HA\43F*X@!COP@!>H PNR*A>[&**Q''L+YE9@;,"PS@2]H@GZA/_@:&2YH&;T)O+8I,3''@ B9H!B]@ 1.8M=1#I=U@0(3,2A%ZF7<+C#BX 4VXRDY@AMY(#Q,(NL!BAB^@->G?>37_V8;AXPB@4)RAD*?,LJ!>MC]B*#O50!_I4CL-RHSMXX$%X@@"X@S8DKRJD+><XH*7,(%A\@QGPI7].Z!''0)J7^*Z+SI6X<*!''.(>;.@>/;J",7L)A,II''2$*+I@F8OIB-!D.27A5&\(PWJ@@>4@IE.@F<D#_QXBC6N*H7[AL"VK_ 0 I7>0P&LK9C^HH35@GI)@DJPHP0B8UC4HAFT@C^"8EH<HI@ZD"O5K^N7C_S3J3S5B3N."EL4X2&Z;IL\K !,+)^>\$87A^KH8JUV@HTMH2L":JI40"J"11_D[%P<PR16QSQD"9FX@DKBHSF$)P=X\H\YH8(!AR7(\9I?9$, ,ON6:B,_X+BQY&T4;@TT9RS570:PBINE;2>"[L-=@BXQX(/TW"$RMIE,CNFR\@A?,RARS@F$I*$KI$DPC@B.3BFC3RFTPBPR+BETY ?*]05T9 ?V6@EY7JDU0NDU#BF+_2??83G!\&RC2 ELQ@,LID(1"@=[Q+N2JD>8 2''5".9G#J''4JB=_\(MLL DS_BBRE@DB?CQI3 D2AKM-X$ASTCIL!,\H!WBQF$DS\"D42@8QN"BRK@@B: AQP#RP%B@QTBCK LCA@"ES_BBFK"2HQT!IQUB6*B,0_$''0=DL]:&N9>.NE+V:PZJ\"X.L;H,(%+BDR&FB"U($P MQP/<-UDN%)H&9@AW  N4@@1Q]BTOP!BYXPXU*3<^HHLFBAKD)+G\S3^Y @$[P+$"  DR0@C,@,-@,!DZ( TRX@2I%+DAPA@*X 2=MT9)LT$?ES$V)0%6%+E8M/%<],<F1%A0BH4:Q$4<!@%F)KT,M08B\1RU8+RXRASR F)YP0=$"H/",SS-[+:D\%&KQH2XKK^-H R_P@ .H W/S-8IC,>O+-1-5+BEDSW"ZL.:4S!)"RQ&J0!-RN/@ZKT X+?I"1V55QS3=Q?SBAH(8"Z5#"_&*K?0D.3RXVH)M =F1"]DYG[J4D//+6I( 2?_CM(,E6[^:6DOM"Y''PJIUS AME4Q??B;J**UOTX2>.DZ^0LS&D_KJEKMM<$0P''<@D_2@ADP@L''(@P;0IL(3\!P:K9H6HSFWI-:F93R!H5@(@@K&HOFP(A@@@T_:HJ!WPP_0@MMPHAF(@MFRCOW$@PMX@SN!J19*=V73[H''AH85Z;M7H]^@7T''G"]''[SE!CX*<77J4=N<)G=Y2''3K2SSU3E5[1''L X R@[6\MSBG[\U+KL8%K 7!DE6(9LZ/LE5KRDDP@LZ0HMM.L#S>J\&2@@&T@HL6LM@BBG]PDH&^K$S:#H&RL%>$ Q:B1S_^BDRZ,HX&-/ ESN<@P8=X\7O6*F;''YOXMLW34*6*80!XUM!(YZJUT@SI??UM#PMC=OJ8A'')E82R9FUVH!V#N]MVSW;T,17JT2JIB_N7NQ1D>M$UM.W''B&LQQ-R7EFF@:N2R(!8"H"^B&P!I *6DS:,,:V>R:%?B:P<TKA7:?*C2PG,BI$,T6.WP&B";K&;CX1WV&MO"AY1BCF&BB%$UQ[B*=[YU^UEDS<HAQ,F$>S.$!KEAHR/AR$MP2ASAUC^CHF8 AK=@AI; AAP"D1)RD6Q5"TJBAK "DP3 DAT@CSV@@HP6EH,2S47"9SF DR*@CS^ MAG NTJ =LGBBF-"DP4@:L)7]NT@DCP"DY&FBSEB@%&,DI4BCHERMP7ARH2Q">4V>98"NKM1B9??S68L*(%M!+SYL9IGXT?*TJH(Z5@:NYD''>G4_X@%V@@C> @@S(SQR%WK8BWN/KH$2%(D7E #*9D10\C+JUHCPH!TF<@T[0 TP8!D2 @A[0@R]P D0H!A#X0R9% !"H QHZ8$NX 4[XAB\@0.E @$QXAB]@!DO 9RB$W?V-9-S,6^Y@W-CJ2XO(P-O"W(<S#1SV"CJD%YUP@D>(E]%"00^2L>J4H#@AU=?J6S\3U7RES,WJ)2F,57'']/_+-(N/,S"<;NL,JEAT*S170U2\+I5T4K<K42X [3DT^ %),VO''BQ_.ZYH7^ZISU$!B6 DL(XY^MF''9,PPHN3,!AL"T#)8W?H:236XLZ8M$$3HR_=XILT@H=\^UE4@F/7XQDP@LD.@DF:@IF4HAHZHIH\@HM>@QE$EUE<@HFBM6$-+IO@@TE^@H;( A<%(QBP@P_(@L3C T%$HPX&@L4*DD;V@QD@HTZ*@DKJL$0C QF,EIP"HPZV@P$Q@N6A UE<HGJOK"\-,$7OQYTSH 8^13\Z&_L_^#6L!N"ML+BWP&''-B"N+.2M'') V"@M%<HL.PHCH7$4EZ!*H1"IU4]1#TT0L4*B$AX;P7Z8 ETSFN P#/HF];*:7C(QD*@D/,HL=@HV5A(UC(F5R+XF/5 F0C,IP*E(;(HF/Y..+QXCK<#]=MS Z -_*?0:8:ZZ2X_T,56L8RK A6]SVQB9X6.0H"9/VZ3(D#VOA"!!C#$C$(]2"K%(2O/(LRF@D]EWW^=H''..''#: 0>&\P2=:7N^%TTE7IW XZ!9B!O?T5LO&*H?0T$=6S!EQ8I>A:O_=D:>+QOR[I,C9_$(O"B@P@@D>S$Q37!EXT\=7QQ2&B=Q\!B["8H6Q,MF8X!6E#ZCF!ZJHVMP&B@R@@E''5:9P+"A( TEA @EOJ@@A A[GT DS^ BF$ DI&"DO\"@P@RCRK@CA]@DF*@AL69HI,B@RJ@CB:AHQI@DAFBBS2BBRD@C&4S*"(0DH& DMC!*K7BBS2C3, XER. BOC>.K/>^53Z=RYX^&<O6C*1I8S+3NCY!9L>&JL+>\D%/XAH  E''X@A,$7L#V*36 UET)L.D4$5GV5FS1AA+,UN,4#TO( "[X@4WH@B] @@TH!T,(ACA  A(P@3/8A@ZH@Q(@@C/(4R_P@C2  S&8 T<8K%[M@DWP@!) @BO. #70 T?H A''P@P@  $@(ANJ S.J=51@R7+<Q=7@//%B80&H]W?JM3V?&5.;-I .?BBVB+SRTK]+218 3XENYP==ZZLKBP?1N52S%$<DIT8M''KM N,>M+U>D=40>"W0POS25[C.&V^G4&^D%(!HS^W6:&C(D5+;=M]F:57G2Y1_^2ZEE86H3?''''RVA=DP7@T(8L,[LOEG_U$!<=ZRA3&CMYVZ#]D&0=/XP1.Y--^%)]IE.D 2N6, ;8HZZ@P%.@EM*@DM^(J73 R#=(H>OHPL*HD(1(L''BML$(8ECX@HJ8DLD0K][)8B0CWYF&HL^)8F/''0L:*@GKK@P$Y8@:)8M@THKG+@DL,IPL@EMI@@T;:NH(;U6:-T$666Z22]/F(Y+FK+J _M[)3[OA_X>)(7&''!NRV;?1& $.VD#D+?62$CN7SF$0UK.7CGJ2EB@UP,LD"E$%I&HMO4HD&<@L<0AT''(ET0J@P''<@L/NHQP2K#-&GU&0XLI.(DL>C0FBHUBX@H9Y0P&8GH_?6#>MD<DL\BC[Y=I OZ[;A88D2)(-M_&1!>-YAT"<Z;M\E[?#7B-"4/C7=28''51?8#POTYA/$(/1\''4B903MLZ,7 L!4P>BM  Q/RIHDI.GB!#\VJ$0XLRID!!D_R,JHT^EF"@43_.0H4"IE#A8/ -E8\*DFGTSP^OJD1QL$KE!*5,Q":EHMKQ <!\FDZX!P($RGK#%*-F#R($:QJ+V4IAB]@4)DX\U:2\8FUZ!H(@(+]"3Y,&[O($6+]"7[-&7S(MJSCH@WRVF28,6[]<""I%:0"A(*6F!S0XNEM&5JMH0$B0B\FIJI<>[L''I:TNM%CJX[B#1QCL\%PX=NMT@4QLO>0@@)C%1*MB-7PUJOKHTXVE&WR@N)II-"Z:C1A!N\I@ Q KM@91HRBE0THRF[LMH_A)!(:L$!"@K1PCA?MP1TB)ZLQF">Q9%1" (DR(4U6LAQBDH*Q:41 OG,L)U6L)"T6Z](4&RFE_JKEW8DM(U!!R$E5&FDIL!"UJH5(@\)UVU&R0"FE:GTHCP_T4P%\[)EX(($''(+"VH;VL4T0SFZR %819F^HDDX58L RCB3[88AA3DNECHCLQBXXW_''122G4TLPQFI(!,8 TM^?#!E1) WMIW\0!PQXPSXL"G"@8>3GECA'' 4E=<M8&U02I&ILAF_IB7!DX!1J)F4DI8S:]''?Y98#"XSRRG1>5)HV"B0!FVT@6(QEI)PP T BN#*8%FJXWE()I%J!8XLNL091"@> PDJ)T *^"-Q!BP85!B!L8JDC@3DE*N!MLZW@"AXV1FCZY6"@8(LO%@1KB[GCQ*JA_QO=6]AJM<SP"@J>%/QQR"/%"UAEC%%+$+[YV(Q-R)5I4( ]^<PP8J6PJ@*IHT+4-@!P*TK(UK4OK+#D 8XEEPL]N%2XUP(:J@LDK&B%&KCBB3L\E% EP@C@C&C\MZOE%##Q1@2P"DKX/*3^V>=RP8U1''A"K>M\(Y^/N]D''FMZCA;K MY[HHIQQ$<)1BBC!!Q1^M-O[ZCQ+T<H$BS4R2?0 SF%B0BQN!3N''CD00LI:\EMA12B@VJSK-$]!$D\,&]S$Q"@Q*H@L\%G]<!HA8C"HB"2A2ELEG]D8/(,D!<XCBR;KKM:$^CFA)DQ!&;J;=[HB"@GZT*XP1>3MQ >(J\%B LZIDH!5$- PX]FN#E1H\!#-"0:Z^#O%XZJ7I2PAMO\F211T-$5,"$$:]Z*UE@4#DG$SJE@$(S-DD-\2 HEAJJ]F!($(D]_''!12BTX>CUGJGOPX@\"$(PRB!(:=G;CI3SPU%0&EM#Q)!]DXIBIZVA4)4LL!WCKT_7W6)<??/-;^7??>''<KCJGP A4N%Z"\SHY]ML%DCU;"BT.D#D\QX/?J*QZ3ADQP@ > N P]JI@BR4SNLIUJSHQD\P ]8HDABY@L(="U$1P< PTVB@R5E(H@\6%ACFH@@@<A(DL18LDI27KVRRP"$X=((&2H<ETQ&;"''?^VOTHJZX/6".AJ(-\RAQDH 8''#"D:CXB5>PD1$E^1RU*_0+XE+!B!,* [CT0SFNB^,DJ5P@@R5P@@F2$15_?LJ1LDY. "I\SFOD8HPUF$9]N\EB%#PS ?-X"4$JTHPN''B@II\P''DIST!@J:8@LFXDDRS:AA$&2#"Q,40 HTX@H$P$LISUC-D8T(!@I<P@LEG@HTW&BBD)+$AC3@BPEJRDDL3HLF;V1BBXG0#/W?6)[B<,1PBX_XABT04@ ]INJRM9BRA!8R2X[(!0;<R]SJD) @IQS(PI\ZFUE8UA I!-D(%)!P!]RHBPV@  D]*((+QBSG_0JTKRP 0SE$\X@=[BIF^90Q;Y(0J7[ZJ5=J>QDQ^ ^IAK#PRD!R4)HZ0 P(6^%82IODK?_@ B05 V/W25;LSOL=N SBSGQ(T?_NY0]FGDHQSV"_+;)SI945<T=RCN)P!T)T#9!$ @UDQNEZ*B9GYUARHI1 T$!HP,ET=R!!.F@MMB"JR20AK:DZUZ$^=1PHTT:LO'')U+FX%$4R.3AL07EV/.@TFAVC D1''8!@W<@@@:]BDCFU"D@.;S?37SGMZF1VD"^I8CB C @QEJ\BIG!!(([EUVWBZ)KIO,)26QDAA]S4V YM9U@1[HJ034T.,XGQ]Q*DCP\0CK2<BT0XZCASR7.%T]BU#A!#_8HQEJ*M!B=XH1/<R(\#5R*6.MD(YPVNA$!,A(+QZ)PI\5@VZ]9]M=''DBIR@PGDY(HE&4$,P''2ATHS""@B[QYQ 4RH%1J_B4Q5D*F@S-U@D5CZ@16X$@$/6@D78DQCIF"PB@4 (!FI<@D#[,@@F, MDQA.)!_6PA8K5B@P4''0BGSZ!@@)X8@&M0H@]<GAJ)HX+''@@ 7E-IZ9ODUX!1+&6PV]F:EG*%]R"W0>O&NH^EAO>D[''S>7B6Q?3''PV=3AFTSXAFBJJ:L%ZLKB$8K(*.I9336P"T 3@PL(?M@EI%R$L0)H1O@$HY?#WZ<F^=B@D+Q4B@Q S7/\@8NX2BP>N#@B3@ 8''0:R5HT=0B$>([ &G.ZG8$L39E.YSSR_EC43Q"M:4P(Y(@8L*D![2XRA$T(@AEV%*P"%MTIR0R@Q,NHELH!U@Q6DQJ]YF<YL-]L(I$R!B)6:,)''$R(X47J9@L,DDI>!@CJ@(+D@:\0L&3BDF\6ABQCK![H,L9@Y3\@0QMOAM$^BORW#*2FZ3_]QF":-I9WHISFPBH@C%YB\=>T%P*''2)*<K3''P2RB%U&"9^-[HC?CU<)L+?!&H[^R*@YX."BF)4,5+8(@#AUM".,3X I1$PWL.5RYJM\9$"/8X?[C@B0C.2 @4(<PT$HHCC8Z*@HG6P D6"0S!^^(@ ;4L@G]K@@@4CCBA+((@X@-(A=L.@GF#SBNA2A&08:7/EDAJHP&"PBAQ[!@0(TE G(X4@L8#Z7.(4F_!SP@P4" P\:]AO#GP&\E/+CP"::B941C,2''3>)60:33P_N$$HV\;JDC=KM4?]:;0/9=B18<  "K@KKA=UJ#F2T $DKAZ(P(6+/_P\IHST RIK5EOQ(("2FY@KX]X)BBQ_!ADSPLQOXPTQ(0/CR&''<@3D>SC93[53@]K?0SCH[( A!4$WW>:!2K/.>7;[ F?V-0E@0DM2B"V9^QQC 2JV\4HS3M2"!J_D$TWP"EVTXD"AZ[ZU=,W+&MX2P*Q!<LISWC5AK)R278BMD8#<CC07''QO GMX,L])XHESL(DQO''CM\8@UBP+TPAO8TI<16;XT5[M<$5E%5&\-8JLEUT) 4[&$">F4DD6X%!Z$5&*A6(5UUTQ18FI  +?X&<A0!\F<D]>!HH*P CGLP#N0P@XTWNE!1WGMPGJ=WYWM&L"TSGP-0''RU4=&53L.4%FX9Q@1(@ [\CPM0%IN 0RI  BZ  [HUABH 6BJ@@ U  AL8@Q) 1MI$HRIP7TL( O<V3(EG)LP\ZLH&OLD&ZD@ \@>4N@DC3@DZAHI= DD H@HSLDD 1HA=7LA=G\JSMLH ( E2LDA''VT,8CT;!TBA%0A")]@3%YMW[#YD$-!YT;I#&NI''( L"P)^@''.,T $@@\=DD4^H$''2JA^4H:T[Q=3PYS#CX&9XTG0[AR6!HH&>XDN+HXSX@@(8@DQY@@SXA\^YD@L$I8N[H=K_U6Y#@=NGP<S"L^W7PBYLU /: A$3XGPE]T6_!,7J.@6]$?1HP(P7$)%ZA)L-E+7/RH [QV)XTT!!EUVCHF*TPB+1Y)S2A,>W*J+''EA$4\)$TA0$LHH_QDH ]MQ&/ZDF8DG&-M;?)LV@A[B@E 2KW?&@A!3BI?BPAR @M@K@<E#@AO#PA#0A&E''QP2C$YYFDRF@[$4CQ]/$IQ&@Q.\$"ZZV[E;E[FQ4E#7@_%X4Q/UUE0U4B0[SQBXI"TZKEHC#BJL0BH[2HP*W"0_$Q6=5#2BCFA38E1C5FYMC$T3GRH%2\''03U>-5G\VB\\P#P<T@R^HBAD7 A[<"G4I6%XYD%=<#G_X0]WM[%)A$G&%4KV(*%W"(]JHCMQ1&KP[9$_#3XNI4[E>$D TBO0/G$ID[P#^$#), SO]U]\]4]:Q!%Y9;EPKTBB$PCG& @8S5%U!!B%L7JUE%&Z<[ZU^&H@&BY;9![9G''A??@,X[!H@"J C=_YD-\Y#Y$=9M]!0DM*577D@BUXPI$< PT8ST5%@G/D2Q14@Q78PBSTP@OY0]0LB%!:Y7^B)4*^!G!B&;"D "X,5PFU45L=R&Q1&/MUTA&5I &MV%]-DE%-G1&I3C)I8&B0%Z18P K<8BKI%Z;0R 6E"9L()A",D&J]S4\&R]4\@@A@ PIL$!!$C"L4 Q=$ KSU@@A8B[$ H*P!9L1<E+^T*JM%B5J1*K"E5 P2I JYE&*ATRV2T8;EY08JQ[? WLF%P@''"%&\J*U#<62S$ CN<#%N^9 1&#A]0CA&=E$?:2@8Z$ >"WZ58@''Y)E=P8AD/*Y/@9B?=73T$WXLCSDD(B8(^W]-S.''V&#SU(''VT@"_HHW!M#);Y8@;P^KTY0#J$DW1A#3''QVNQ&& T*YR;A E=M ^\VK^C^&PC)PQ&LD^:@C!KB%^+JK-+L+3:R_/4FZQ]E$FS@.$]\[R_DK\^LD''LHAIOD$FT@@''ZPI!"U$VL%,S\*E]L @F&I:/3XDFLLH&MHKSBP%\;-;/B]?=CB.XC&.SJIV%#Q>&NT(CRQT ^\244-!!-FLF P(=%-UNT.K";Z^N3I);K.Z"5HQ\1]@LPYI.\(QBY,:[)TR''@@@%)N(_N$XS/LZC@X@VS@@^GXK2^@D@G@@C4H>6W\-JI%(UDYW.OUG?&88(R="A@;UX][6XS\9KZ3(H#T&T69&US8: U "%F3V*$J[AHH3BG1BAG6A@;EP*U/RQE=  ;''# _LZ''IU B]D%W>PF$B=$D]&7F&(9(-15 ,4#BGBABC@@-06IV4**+4(;(4#ABA RG"!*O!IV]^)Z+ J#]8''PLS+(SUT6%62WE7MUSW$CPONWEY'')"2HK"/0FBK$2@+FCB2''INY#2T%C)\*7A*Y]"DQ"WI>5RK''!2[G"HD.ZSDPFQBQ"CDQ!QDP"2.-01.H6R@D2 @P#CAD="AQ KUM7[#9&*.TV4D)Q&_1M:Z<$%JJ::#)''@_)7#J_YHJ)^CKS%*"1/:''V=TJ?50UJO)5:\2$!BZ8W8NF0 4,@ 4@@IY%)9+9D@Y@#QMLZK0.$_H<U (U@-H^;OQJK; IK\I26:F1!@:HU#&.Y=+ER<T6G5W^[PSYB0RI(H<BJUF*KP+6@@%L0 "T0A9 PIOI+R#P8IM:F+V*(6A@''BD!T#&N77<44''D^4QDM;+X$L@IC1;!01NKJ3@H3V P7,@Q3% IW,KD)0A0\@.FJ*&D%));BUT4<H&QJS''2"U\3"6H4]Z*HN@QM<P 0DVR_*W_/2GP$L@"CD0 SP@JK\K5Y\:.50K\#82ECHY)[IX"#\I.T]LG]539%!''E:&)[V$*0C!1?FTR07P@BO0(W/(0O<&.L>#RY(X$;F#&[F[Q!(Y]0[1)Z]S:^0BMQALLE=?0&^ N,8RWJ-W:\T<+)*)<B\J;<.B!F.-I]AS&R/:*^&:K&P0?^8&8DF>[* _>DDI9I0F\D<-<Q@%(HDR LZ?))CPY^?RD$+6?,<C?$&J-* @#Q/D+ .:S^2: UD V8+W2"]%C(UOV$U^AJT2^@W;6#B?+\8*2N<)?#CKZ,$_#R?,5$-!$H3IX@@9[^WJXLYW(&$"U3NZ+.$5V?L6Z7L7I5Z[I.I>&E5\2V#VL$;\(Z;]$&?XX*XZ]\;'';AO^)^4/<9,#)LDG"@@O(4D<?/C!XR(Y1U/#CHD%<D8,U ZW^Q''?Q34Q,X82QF1D^SK.]18B!56''A\ W*KH$,Q)+;2D+Q1]+ED6ZNA)B(3A&(6!Z-E+%56XU*/ /U7$U)57_*G3PO.)(-\(Z>MWZZKEP QJ$XXKK;28(''MPTA R[C72BF#9AE8CBI=#ICV@@D_BPG3RX/0H@L 8,_*C2=G*[*I[DAY>(=@[U":*L"9D0/MB(Z)7013@3A48NN8Y #=JVC+3A[_$2O^,VC!_AGSPADV BJ!X3?&ZLH+1,N.-$#P642T!\H9I_DFYWR7G3X7\3YD-6YEO6X9>)"(%0H^-D5"H3(UI"/(PPB6LN(.IEFAQB3L00Y=)5OZLBKIRBG?!@CLS-?5=CFR-FY"@QQ]:>5R0^2R^PY@H^(H)^F8*BJQ"P*!\(P!](@I!1+#\:]7N[L"RIH3R;\ZYAB#+R-BC-8:!AB%Y(0"V$6**19#KS\,N5R+ "0#<FYH@XJ@5-MQHEV2J8J2+Y@PAD0)U$AM%8P]J57>7Q Q[X RU-D6PEWY(^Y/T"KG#:37 :MHN;*D3*+Q-_8D5JY%U*[FTJT%M0KE@JYYB.M)DMP!(\@3ELPKVYY#F7KFPFMK]2G9W6(J7$;L9&!"Y;#QGY>H7#^H;+>H;3^H?;>H?''^ANQ'']&UM@$S"HE @U+CY"635+.Q-=!&Y%ZD0YQ7"ICA1YV#@)Y+^YY3>_>V^7&W ?&W"7&X$?&X&7&Y<1X*?H@ 0GX DE\158!IOYB+@[QRGCE-G(;$TU:Z@,)7!&$5^>M&>Y)?^UN_F?*6GS""G7*"G3"_ N889.1"L$EW%R99^;[C<X#*S%<M7DAL->:M^ 3LD+G#3J;8%U?-7.:A(&!I<B:CO,5=9ARDL D*TXHVTDH 3@D%@H@O/B)T58C5IDJ>>''XS_;PUM[AV&:S&@*61*)=X$7P#: QO''KT2/>;)U(;H(F<Z1WV>;Q.Z''?&7^7.8 ?.8"7.Y">H-&LG[6,9_X4#F5NCV!+*5E?Y@6>0!Q^0O-(1W5,@#Y_M$>7-%@?2?B?078Z$8:V''?MI\0/H=PV:^3-O+''ZB^*;GC"E7PBC*]AC8P81%=<1''O<1''.<1(M<18_<18-<2YO<2X=<2).<2(=,F*0NJ*SBCK@@IR#@[KM;DB=<Z1&E#.!6Y]A"74Z7%3HK#7N.,8"T70Y<4 =<028+((25HTOU<"5\A5K+-;X4A''''U$HQ7O[+.J1Z6B/L#+Z$72=0JCN&Z R]Z1 C@J#7G_V @GPB@G73WH7=1HC2VE&0BZEC@C%G@QOLPDQ#MPPYUU!.;D3T:''0<>NL4I!M,ZS%3 H"R93''.+ 7Q->V88[]$AL1Q@I8Q@I7#>98M>:H/>:IM>:Y/>:ZM>:*->I<BAF_1\H?@3?;.7;A<1E;1=(HY[ ,& CC''*[D5(:]3LD/HH_70P??@[_?D#??D+_?H3??H;_?MC??MK_?DK$1K822KV9+#B6HETI 9NY/]SIYSG8H18BA8T0@.LP/*+??*3_?.;??/C_?3K??3SO?4[P27,0 %(PP4\ *.0N1@C!IL=CC0MFXHI4<D!R08&1LPP(\JD\?[8&HOED1ZMXK04>\PDPR"QH4&VACO2YD&UJ5&JSA''*YT.YL6WF!C''S9$ DBA+Y4\I "R]HP#UBJ)(1T04""AIX\. P(,NCDA-V_] 03AIDM_BH</+U:9ACO$B%,JQ0"URDZ!N&-_)4"B &^GP0RI@Q"??Q/G$3^$+ARH.EFC@% [%Q>C@X000T5WBRJUQ!RS\ZV[ACA@<MR(-.3@EE!<J\P&A"TK@CZ 8ZQSS,IEH VSHXRX!#33X\N3E"6[A72?ZM.7_"7KQ?R>:9I<YQ/\.1FEIRP<,"S6GPK*0Z=W)5.AG]H 2CJPXMGT+@"+*DA4<_U6/*5O%R1=U;=?C%/8?_7''9=>/#73;?/W;??<.MO0O8FAIC@@AW4S1\?;DA#"_H$''ICBBR51( $/HAE%H["($.+C"DP\H 00#(#.+*K86$,#S9Q0X H;M&F@ TY*/MGFGGG\T\\^^_3Q12BAGEKHH($<4$]D''"@B@D6BX-F((RC?\^8SKTCA ,O.+AN1KQAA5L9K 4Q!P@,JB*&0O@U4&D@DW K QIX788QS3#+)/GONON7TD<<=?^0STC8E?WOPN?=0PA(?H&D"CCP]_WPIS_Y(I@FI.M3RN"6G&HLHN KABMQP/O@#$TA.X@KUUEWMAEUVL7GUUUU%[YVIVF-M%]U[W:455%95?]WWXHD]]-]"X^TUVVMW/\FI@;Q@9D&>)F0Q$$0(0PLMS<;*L#/.+-/NN$.4((PHB(]P@CQH''IJ(6;X0!P";.@;Q@P=DOK&WNQZ1RNDIE"0HIKK__K-A-$3&&NL07VY#H X[96B"LBXBL[T06P8IQHGC&I #$DLH?98,NLNJ>;"8$&7;.CZT^;MMXYEIU#$2CWP H%.L#BJJ*NX. P:C:R8]<Z* 79+J*.+DCXRN@< CJ0TH-OB#"Z"[&BA***^.F..+-[ZZ::2;7-++,LD^>>.21SY[::"''(RFFBA==6=1EL$2AP7^!F''*-K[F3A@0K1E#$R[6$1_$RCY(@X@<<"LA,\\TYU13211._''GGIH7\\\<,%+33322''7OGKNO1=]=L,!1>M0KQ(1IN]<D5B"2"-EXX-((H/>&_ZDKAF%DR.W?"*L8BV<(XY)?M"#B^RS#5+99)%?_/''(''Y\^>.&-+19;:+V?_//,I;B@B[#E%=@P P 22N;\5_<2"ENKHD%@R#A@@\BGS99 !IG;<<]_??;9?7=?=1M @@G(/?4]4H@IKN@BD\!@ASXP  <\8AN^L@L-"@DQ&BAJ/(ZBARX(!R''(,9.%R@ /D&IBWF" !@;LMQYPPDJD73+!]-HW%;''TITURZ-6=?!JX0Z2LM1USSF4T@9OC!@HA/S''I[QP31BT6<VQ@CA''KEA[DJ!I1ND2$(!R#J@$M6@D9F%GQ-H3"''A*0PC+T0]6W.LT]M&;''KCF 07#J,8QFYJ@K''=C#G/''XQ3?>DYBAENP BU%HP>*1B9-P M/F5< KMVDFF<I[)#3DK*&PRAJJ@L@"VJ\\EUE+C)D   8NPD+?T9XR%Z]TYR)Y.T)W-!JV+9Q%KF$9R5.V$"940^TA;FBAP2Q K=OZB9V,-BDO/[E]GT(&],Z$!TR\BW"WN@PBIIP@I& B@49X1BJ42\5-]!N\71R''M<$Y3''JN493)QN\:3=%N][)3''M-$@@I65<!FK$D %@HW]+*3O$;Q8RI=0TH(J@@@@O!!@%AC*DJ[(EBGM)R!DV7(QBUZTX(>5@<X5R!DNY+Q#&;T(2DE:T ?6%D-@D@LZF@]A:%5 6-%:3-8*:G-/HVI+BBBWB5LE?)*]4JIX@\.<*JW/^C7RVG225<@><7GUGYE$P&Q"ETTVLV"6-SZRHY$W\2*UE%F,J1Z?5T6SOUMYA(1,9*5;)NF6I$V^/Z]F I5''9Z:&8"V<A494!D,XT!@B%I0B[;>E[BAE^1 BU-X018V,X%U[E=S HU:6''M<P9B[!&X75; >AZ9C8I,E(!L8JG&2DGM@ABI"LM+RH HM)!5-Z%F+V-J:=+R''YR5+X>/Z6\KV-*/E;V-[6=/Z</Z70]U-Z=FP6-HZ=1AJ0@-Z20 ;KW AB6+<$EV@6-LQB.4*.>,]JG8''B$N  PXX&E@XVI\@<98W/^%U;7+Y65;7/!^><YV/^Q$IV_G!\2@E&^DH(9JPSWT*(FJ<AA*:@@(/GA#ABU[0 !''\X@\?FLHQ%/BDJ^3 QFQ@D/<)$-X'',VB-)SSE["Z,)FT-]UN.-I@,J^A)F;&U%"=)JRE2(P-AL,KA^?$ELHH!3EPGI+B,O+WGTQ4X\[Y8UR5N%\ <1 5/.A!$IT/&"6G\<F_MFI7)IONXL*3)]SVK"TBH);/6E_NX2U1&L8?/$X* 6;-$6*7,%B R_>,$%MBJ$_XZP+58''&=:=_3^O+,W37<F=I;=''HA@^0KO"C;//XI9%FD:=4)*UJX[0SSC6:6/&VXB2:XT(XD3_1+T(Q[5_RUEJT/N]G4S:]R''Z,1C&+A$"S^9RZ1SP&-Y+2SV+=[5+$&R:5&;9MZ6W($RHAF%L[[Z 4+I5+[XNCPX]4 *S,G?Z[''L 8%M(8,B:*+]7_ 92[2IX%98,D.]#[6/_.$8+K.Y3[+USTU6+=/]<[X"U(F8UWV''^=;21/^;X\M/^LM;"CJ#VU=.E"V"*IU''O//)0-\WH)?&3[=2UM*(JU91"9-Y,!''BD 43-V5K_\ R&>5,#V766[40V*CKAQU^U-;2%K-\#B=?^X- K-BX5;3%MI\93''\.Q)>''GB?BEC*U?@C]2#K<!M4N6( ](%7_9QTSVJ#/1Z%^]Z.W)71MNE?Q A(U*)2+H F^,,?I+*>2''17-ZU_;6-''^]+^?7^4EK7''AWU*/R%&:))[^61IT2DIQ]@DL&7X!CM$5WWA-6S(6''KD''?8CYPVE"!E>ATV),& #E2/,F<5Q5X.X7+7$%\=;3P.[<:DO/QBE.?/IP!D&T$2MF%)(Q#S8[$Y[::^1%J,P)W<[+5W''_>8(?L)I543I?,Z1YD?4-!9<U>(+ 7''3''O1?:4T^;8E#Z7B9XBT,MTQ>&6%#8RR,3K&SR-N?IW?:JQ6)R%W+J"B6)$G<FVG@*]3''#]X:QF0O31/''W??;-7?>>4A?(;B<@T^;?>&?>B%C&TD8@AY@@=^<@_V;9[BX)O(3Y''H7#Z(<!4.J&\$(TFJF;3&V''N@;"6L=60F6(;,KX<$T(@DLQ$$,IB.DEB4DFY9@FZ1@FZ3@FY3@GYW@GDV@GX??0AW6PA'',0BHDPA6%PBH^0A)OPA)-PA0,ADR;#+I #Y1BN+Z9L:TKL8;IP+.1*#,KL?LIPCA<%82#KV:"":68GN4*D,0@'':D(N+Q#O/@KM4@3AC 6-C.6PC/_0C/\0C?,PC?%PC0NQDOU0DO5PD@EQD@MQDQ''QD@6QDEEPNWJ&NRA-8;YDB4V0]*)+QI2N.<XPEDM1_L*GBOR)4+Z#.!P ;M;G6@1-BP3!EVLQE 5QE%=1E/WPE''EQE>7PE''.QE6=QE6/1E8VQFH]1E''61FIFQE)_QE8?1E"V17OKEV+AEV2+-P4S,ECUKJ49LEKC@6+9BKEI,V2[-+[R,ZFR,K!#?#\9V9AHX@PC,H@N6ZY,0XA;+\QG(<Q;3DQ<1 A;;4Q;=\Q?3TQ<ED!<GD"C-\R@MT"@S<"C/<R@[4!>7"PJ&)&Z&3M"*KH5297Z<C?F:K__@SAQCL"SS[L5*C6 ZK-T4J1S<!)MH3"K78!HL80Y&$"Y+4"Y/D"]3T"]7$"];4"]?D" O(1@Z2>R$!I!^RO":K<, C!N[$)''H9I%DT"*''T!S02=R83-,T8+<@B%RJH!PN@R3CT"0O P''F4"3M,"3OT"7OL"7W4"7_D";_,"7IL";CD&H.0RVWP:@&$B&\X/[V;=P.;R@6,B,&A@R3[U7PDN%(:%ILTM3. +%0I TV?>FBVD@KK-L2K1L3MWL3NULLNELKL%L3Q_L2O=L3P]L4O=L4N5L3T9L5W?NBTEL6NYL4P1L4RULL]F@N5ED/=\HJL=J:++GMC&==!$C"0I@*$9O*2#C;TAGK!CL"QDDR?LXI<H09<&U_C.DSEHD;.=LBEND;.1L<15L</[L<2?L;0?L<3UL=57L<6=L=73L>2]L<53L=9]LBKL@K4!LT''B@T\NX:J;EJ#N9''-/@99V+!OAD9%YMA+0:?3&_;QF1DUME=" (KB,DI_H@NMMPGJFECOUQCP]QCPYQCR7QCM=QD_\@GHHACO1QEW9QDW5QEYSQEY_QDUSQFT9QCQYQF:_< Q654Q(N4QR$!DV8@F#^, 32,K9,R&R@.B5OHA?1NP,K1!VJ(PD<2&QP/GU.- ?SED1R@@"JAD&*@D,Z4SL''TSMLTS\$TB-!4S\<TS,64S^E4S./TS\]4S.WTS]/4S]VTD.2TS.<TTOT4S-<TB,[T@! !EB24JFDONH&3<E+,="K"H7^/PR=55HAO$/J.>6C,R82OIS-I:E(-@Q@@@8!@CE@J)\R@UU%UUU/UUUMUU%/5UVU5UV''UU''LUU%>UU6EUU7/UU6.5U''T5U6?UU8>UU[V@A KADH2J9H82>?*)RX,O<ZP"4:C)J<S%F3FUV4DM?T1-J[''L/3C!''5!M@+7?@JV"Q*G6@F+T5U7_5V2>9''#ZUU7Y5U:O):G,MU?WEV*PA5?1-U:%I%?7EW''4]U8G]%;=UVG?%U4E=%?_-P%N2 ;&X@$ <INDX I!"''Y\CB7J\_XHT:^2[\UHSN& T: ^L2?W<QIN1R;AL"5_5&U#%"9''E&Y)U&Y+E&]''=&Y%-&U-5&]3=&]7M&#AD 0\[8]<\:7Z"*:0SE3U0.G*B#2>,E.''=,3J4I!21>D@<5KXD@B*42VQEBMB(T+,(@/4A7?NE&7SU&7W%&7[5&7_E&;#U&;_= $8R>.B(#^K )"L# .AI&,[@#*3"7^^C!0O(P/P F(S]<3JI;>,=AQSL^1Z?48AJBD0M$D_>WDQM E3LU]3>9D_@_I3P3]4L5]4R?]3GUI4RY]4O5]3U[]5N9]3SU]6Y7]4V1]4<UDQ-H@H&ET(@ORS)+D/RWX#,UQDSJ00)=REUL3M=$$S?P,]Z\1Y=QH#=D0Q@<5::1A;<QA;+;\O*Y\N-_\NCP7Q.M];99@Q,9]:9?A;41]<51\QBYG>VB*XLAHKH;U#680S.ZURE5Q1>1]MRCH)HUW)",XRU/I/J''XR/QPAO*DIPBDSYG@''H'' ''B$FBH9"BD^BBKU"CL5"CL]"CNW"BOS"DPY"CJQ"DN3"DIY"DQ["BJ="DK:F,= @Q&-WQV-DR@;"DACM?-_=CHZ8UKIZ@@WS B_2W"NGFJ!M >29K,=9/9RC!DF) C1XA"U(8 *]X *58"+D8IKZX"+-X";.8 *4X#J$X"\R8#J.X";_8"7\""278Q^""[XI) 6 .X:.1V&T(@9EIWJ[MJ>+*6-K%KKK6[5D-1,I-7CZL.[+R]1]Y;! YC!,Y$!>9J7<.$)-X$!4Y$2L9$2&99LAVY1H.)*:4RXU3 H<&Z_"7"ENYP2XKR?+VZQETJ,I N,W@Z1/-,30!ECJ :@8!=W+Y%7<Y&HLY&D>/%8%Y&H?9&IEHD1QG)T".7@PT>89. J<BF:''Y88QFPS\MB MO%[.9#,1G/1ABP)%TJ,+?]YJ_^@<08@Y P&@0[?S^6Y#[&_H<C?S$F_UH3?S2._JH&Y</;?S(.SADX!G6P@_"6J \CW#/K$HCT0QSZHV<P L>\OAF=//V:G8YP$.!59ZM5.2$3:L!$NWXS''@>&*R#1L[4=#%"3:7HTY$8=+(6="6<4EJ=.Y,5U_!([:ZJ#07%+OF\5_8PPI^= I^-RH.V+J!W9*"IH6FR>,":JL%Z!&V^","&&*&MKCEBPQM4XBAVJ''*O\%MQKX^I=7:9)Y&"L*<4":[S>%/UK:27[2,CSB"^.@$0HANP^##&C\%L1**9BH/2N(.(6*''5&*<GN8*D ?IB8US1 G_%E6NU3X:Y?<;6J@4"M)CZKD@R?%!$H[,#!?]$C3%/T7@A]08AA5B4G?C?EA@A98?''C#C?U,8AB]B5X?.4V1.$@=PJ%SZ,G=]@Z8(!SG&&4;*H+WY6S''GD@#_$>*Y+.W)0#@6W ?(5_@2:7<5$1H+^(%(7W.[^PDZ;<\5$6$6J;@48.&)%&,(7DL@I&I&FD7%/V1&VH[-/,3Z[ U.>/8I1->;0=JY(7/*\(Y".SR>H.-/]/M.:==*:0Q.*?1.;:X6:*9.:F73_''$01E*DD]HAYWV>9MTII&6H1''WN.-,58/PHM:JY01_G!N#UL-NQ9[41_,GM_E,/EW13FX53FE\./T,C19O#FG_?52)IXH0]9_^HHIN\;%PEX>9YW!=&E[48D\@9Z XM:*KO;/?/-<4;&*(#Z.(DL0_M-2HJCM<C*.+^\*48"*6VX!*V7.VK''D+E4!2D52<J/SKAU2MTZ''FMHV+LTD2#TWNNZ/2F#29GZ<*I<2*T<*)]Z2 7[++<;2%,&0Y4L*$/&+4<",^]@.UT06PZ.@$%P;9A<WH:7O@9SWXQWH1&3,2W1ISF"DAP@DQ*!DSR Q%S]5U>]5SW 5UT=5&_]5&&]5!%@5#U@5''N]566=5%6]5U^=Q(X=6E\=57D]6V,=5(O]6WU=6V<$A!B@;F0;ZVUO1LX9CY^N''D598.J\"I%3.M_??F&GY*\W0[3$=6Y2.^"^&:.<''J=?KK2-GFR,"+2K3I:M#LG9V-=U1+0UA5*NE*WQWH5^F]SSI=.Q_G@?D]3%OK=RKZ]GQK<E:!@(HY7+F*%#H3I,82RDSM<5[=:%>/LL7FTF6</)''[B!F&U X*@KF(E=%4TR&*_VWKN_@$*%-ML''V#A1):T)KZNSS8;9H DB0P_60N"O_''$JM.&Q7."S9>"UG.(K%.&O'' "V7."+?.&37.&3''..[/./7@N.?W..__../_.-)(AFT*2 5 ''8%#UMK/O7TWJYQ.^D[5JX94,RK''E-T$#)E-W^E:U:@^)\AWJ+2^,BA2NLOGMB#.);??>7ID#>:F36 ,9(HFFCN[/3L(;''E;/.NT;H11P?N:3:U#9#N,YKRUH4+BT:.>;/JJ8Y!ZBPFL$GJ"<O][(@,@33QZY?K$=*(;96*,0#8.731S6HQLHM7:T0/F-/R!9_EJ-)CPMX0,X70L!D[)2,C-]H1"Z+:;H0A?@@@]J@F2CS<19?<1W=L2==L25?=0??<4??<3U?<83?=2W?>:7?=7Y?>6Q?>65?>>9<R_@@ -@A8 ,@S%(NPODEB"LWP)Q)ZE''$J 6$H)((XLR:1.OG"$H8ZNU*,6GFH)RVA:N!PH**%29\0X<*\R[N&3Y,8\>+\2[O%$DUM/D@R9[D(Q)EEO?>N-C 2CA KX!XYR+@PR=V+VC2E2.CG"0IIXLBJ/QF6KM!P(QB$CVWV+M"7XM26EQ,V;E28$"R!Q\@7UM:2[/>"%X.7+%&2^@T;0TLD$ZF,B2L_[J#$$1YPVDP1KP%29EELITD/EU54(2UQ#R:3]F$)1ZEBOVOK''$6;-.6Y!)0PZYQ <=JN''SLNFZJ@BI5@VR\_*+DGP:Z8^QGGY[KH !4<N.3T0JB@KY!C"FI$(//7A LK''9:S?9.X/^G2YQDWG&-XN%#;XOC+[2/_\M1EQN@Q"BDJLXPUE)%P4% B%#A5DW@NJ/WYY42]!D M^L041BD> @KI\DYI:FBHIEX$B!O?6CG "TEWFVQUU$,0PLP^S0PRR@089*!#CC_."FNOO XIII@:D%%$$C0^"VRNQ/+XH9EP;''!#H3W80T (KA+H("EJ5LBBQARQMA*EIA65AI%#&#PDF"*-]-.[\LX)Y4>VNMFDH"%(9-FYG8$98XQL&QPJUD8$$NVKU+''(BPI\^WDHX^?)A\X\F&B@ RY3PI_W^/FM1=:&'';;E7 7=6TT)A)- *.$M9DDW@0O*5_U))I22B% (&."0A2IKHM("IE16\]%P0"WEY7A=>,Z''''4^I0(@V"\C&D"Y(4OCD''M!&*:6\R6"2A6?E##Y"!I#LX]0\2R&4WAN[RB]]JD1,X,\^?39DD($O^>#@BAM !JJAAY/0R1]4#WSHQJ/PQQ+WWHSA5Y>-''L8*\VHJ.6^++ZE DF@ /T[FX(DFYUHCDV!84.B^ZYZX;I$X![DDH *6E@YLF=IA0X_#Y&S&ZLEUMHP(!>"@A0NFP&[ PY8X $">&"C@\LQ7O_74_EISOU>+@6\MVLNP.&_W]8(4$TEAK%YUHFTPXSAQZL32;FCOH4FH2T$)G^C&-''#''+W]MO0VUFXU0(21829I@IQUUBB%T8DIZM_+UXVD=3D@"M]AQ[1^MOK_0^6GM T  ]''7]U,I67Q@CH#_8YUX FUABA16TI@H+@ )(LH_*XZG5!E]N"09W0/=2%P(F@(,5=)#Q"5M&FVY!HAU.RHNKR:II*J''64!H1TNKD7-17/7].^: 87M/$S4<\/^":"LU23V$.J JH6DGDD8=>=4PS^F"R^ YM]JG90&H9A@OPPI[M5XT-@3NL]4[5G// Q1JHJYTDF4!AA''*-KA''[FHDNYCXDTPHO"N M.Y U''H4 :2H''PXLO]M@RLA#"IQ/*T@)@5KX2;T08''S%Q"%[4(%=%1Q@LXI)ZCD#DH!+1"D!L(!JU:K/M"V:IQISTH[1 )XH$*(\/\ !D0ER,-"6+SNL:E+$(D(L6^^>LZIQS''Y(0 :F$;C] _E9E0#@(LS"APK9"REXV5Z!G-T+?N/?1 P<24H!FO@E6&,"DW3ZEPRY40P.MRA7VYLWH,BB"B9= 0"JGE0L/TJHKF-A@A''1@BSQ\(!E6\DK//*ZI_RG Z%L+CLY2YS0=I"RK%[%LY,HH11G2\$=&L-F3JBB-ZVE!BV%L)#I%4:5/=^X" LJ!.L1E!?QA!''7N$UWB''NBGRM1NKXT8!BJH<H%CYHHB%0%DI#I1BBY$H Y(T@@SEGBH/CCA''^=,AA+*U99@L@@Q9&QBO^,S-_-XKS?<RR!]=+MP 0HFP@KBX:J0(!BQ$\1$D)K&CW5ID)_AKDN"(D@(XJ @&4F"PV2KT@%?,9R_AP6DOFS\ YH&H31(PGV%?<HZ__@20P-^3XDH!RAA:QMA3''''-O)FKS=_,42)1M.EJYW/Q6["4QR04S7@,=YNH-E(,4:CD#L,L*5#9A!R!DDU.WO7%Y<IP.J $@GE9%N)V./J8"WWN@#QX1LF> 0ER( DLB%@@VV;@!C$0HPX5,LLSCN-NMF"B FGIQBC8%Q=:G*J/E( $]@*!@S2@(#- XLH''ZNBDSFS L.8L!@HB X 9(DDC:62''@!)!.:<I4GZGDJ1NLZ I1#A BV^S#EV8-K2?H^V84ML(\$]3&-R@8&9##Z94X9J[7W BQE''%Z$&FPD7$FLP :<*&WGCUAA(0XG!=B\Q#C?FDOX"ABH)0@/<ZD FJT^)@](% AE$6TU<KPH@FMKB@A,BB C%<0 \4(DLMD$FAK!3"%XEI:%F9Y-\JT<2''Y=FX G*E1X$^IDD YIA2R]OE+%+D(1!*BRLP@DLN^P";0N1SMC]CHQ7* F#*R0!E Q" )&%3X*KRI,RP6-C<NL2(TK,090ZK5H,!6\!CI[IC1ZH@J*98L$\[;$MX(CZJ=KJKZVIY&H?R(A#PXBWSSWMX58 ''OWET##$[ %LH!T\]J:8*VVETU?1XG;FD8 YO<DD"IEFH.K@%@4SXQ@1PMP\DYLJPS-@D/"#@ EY&8ANIRLPSE@BFR3J@K_F*TP[0PH]M*8:3QK@@@0;?DX)KLDDS TBCH+S  <=Q0@,T(OP&L''BHPC@B$8%PA@TPDQX4_@HTCP[E)"M&%5@8PU^N,^UT\>$!(,Q9^)0I51^S=[M''QT/M8I[.D)3 SITBR(1G(ZX55\V\[KX*EHFHQAO($@D''-OXFR+"D(3E A3GX@WN]APC,O+&IOUBBUU8P@19<(@ /4DDK7/3NI8! A5A8D ="(DF##02_<!!42"BG,,_;SOJQ_06"\=# Y@:D(IFE<FQ/;@2:66Z!&H$BB3=3BWEL*-Z^$<X3K!TZH#2ANL&\+REA5HD&DC"5*"&U*TYM:,L$KNT_S55"S=9)<KKNNPVD[V0+4''G"E$KU?8!LQG)1\9.15C8".-GA[.FN^?_:9 W#F U:HE&)Q231%J!L!XLLZ]2^2VL_AA0BECUXQG<J(P$Z]@DC(I AF +A!B]XH@LZ0M@''DODDF-#!D48@!VH38PPZXJ@P(S"DARHQBG%QX!G<AF0"EE1/QG0%E@(@QS\=!8].Y"@F"-@8@72@A4U  @K?;+T"_MBESR#BC3:H0Z*BR.C^$ 2O0TUH@''IY=:.RQ.?I]Q;9R-I\:<''=?F \]1ODI4\2ITTA=OJ.\"#QGH I.QDV6HL_RN6EI2AB$VGAO57@J 3P[1PP@;&%F9G@J*@@@CSPBI14GX''$AG[ @9<FA/DF@C;PZO=\Q6QJ-U@J=XD":HEMM%:K$B<10FE"!2 !(2@/!7]/=A$)MS]K @ZT0DH2DTN $@H0-23"8(L3)1P? 2H71$M99CD14&N+=FMQ-18TL6P"58EKU''WB47EU14!OFFUZJ@%SUDUQ!V]$-6U])#OYIRH ,UD!, Q$AE[(58Y3,$Y-9FYX)UV\LR92A YGXD]E04G_1T]4ESV,P#,3D@$L$C)% PAT4 UOXGFHPG&LD@&[L@\TT@LZDB<>P@FAD@*HL@NP5G"KTF"*E0$J @"8%"''$(P@XDG0A8 V0UP @0 !J @YDTF/O8PU68D>TT@NH(H& X@FZPHG)\PNAP@%6<E_?$4PV.@I-1>L1"DI\.&Q- ONC2''VFNRMLV$AL[)"M^]ML/@E#YY TO*M.:XHEB-A.;(L5\1A:O$@D_-@D>*IIXL@H^?@IXMDHN*@C@8X@M>@-B7 CEM@DEOA %F\AN*AW(DANSE@H>/ DB;]1EFY!EWM!D!&Q''IMADQUUG%YQH5L2LN]>J0M''G^EQM!\SN7\3%,AU)WE-Z@\4P(M#6\^BPLP4B9Q@ <DV7-D/D2YTHI!$!E\1W\M1V4!=PPU5Q2X< OU5YNL+4;YED1D=I:QQI)XT[(]&6%"U-TE71HJR QNUI/DTD_E7X9\< .\(GD] T9QY?XFH''0Q9W#A9%Q\I?1!0BH% @Z##_JV%AIUW@:%BA1# MD2 BD]0BCE@@Y''TN6FA@HVPBX;5AAP@@S: R!)@AH.@ASG V_"F@JH7A010AAS@AB& @I= @T=0RD: AJ!G@WP _V;QG;#"V<@E! 3!#L2STVF&T-UH&95Q_,<ED9"P@L!$%\@YF=TEK$ QF#KWT.K8WYC@O.4RMV&!#:;UR&FCA82 RH3 A0L(BP3P$F !BP+H MR)RX8FB''2)@IE@AO*C$0V3 Z=4]U_XT3,U''5P7''3()'':D@TPN2NL$3FT2@HREDP6*''WQKBM" FT"S)X#"S!#4G!L5B!B+2LWX&USF)@4;@]FNQV7LPVO>A%ZDJ$JDO=I@U2QZ,T",$16P_BD,_53UZBHUS]  3XBUX()P=)DUZ4FT,]Y01A(91QI-%]&[PEY1@Z!M,%"\2%%U_-*M305Y4]" ,98]^<YO0< &T<@RR4C.!T@ X(B<Z@@* LG&Y,@%0*P@TD@''?15<Z\@NF!0ES: Q4,@!JDE*JX@DJ @Z TD:/AAZJ*P%L\@&(%0%OTGELDI&[@@%(XI%(8PT4D@"M(@"I(@BEXF@V0@"A) DJ"PBIDG605B?O-"/K:FGC)PSB !''P&B0%@&[[Y*+]A"7E)A%L0@ 1DJR0^!O#U&:7BVXXXP'']%R;KVPMM8F;>\P!(4@ O%![?*J\@WUA^ U@H@# ^6>$C#?I''72$IW/BNZGENO+@H9X"^*^LWCD@IO)B,QRYKHT"BPSVB9T*.G9(1JJ"B"YL(EZT I^M%1:J#4%MCI(DILDLDF''HHOG]-IKY\8A!4P;NGQ?!CMWUS& HV!;@HWYA+&]XEWYA)=\U*(TJQ;)DI QT+GR]!O5%AU2"T<''%5HE,VW"\6U PY>5%6XCJCL&"FQ!*U[ ]7,S*3NU!V]%^/(^D[WSQ''[(T8K<$8V3DAYC&2X$D%TH@F^2DI Z@HN-@H''ZPHZI@B#$PI\T$A]G$CF@BI&W@I1!(I/8 G&:@D(Y@=E B,W.@?>! 9#J@C&=@/?71AIQQ@JRV@@YQIAE:PN)!9">]A@P* AGOPAQZ0BH. VF&QBQY@A7=5+''RA@I)@AM^''''G$$&;,DSR8+NC"J&<^%&7\SH8)E,9;;D([ K^37#_ZJB_B7 \%1DM#$O-HQBJ"9BF+AE&*ABCQ@ACEPBI?PAI=@E/YHBY''2Y?0H''!$ ''$0@B-^*^/T7QD:0@^3I\TP9$Q@Y/QO9G4R @2$HL%!QERHSX"])&6!U!+%Y 3\(L3N#\286P>B+KFN6EC[6(E 6T1N:]D(EA#FPBCR@A6\6@_.WGTQ@BXF0!D;''G)6V@X-%(R!J!^ Z+''76Q<3VMU68X!#YH%0RBUGQ%B%C+?<X#DM&,(YT>[&^B8].IB[H\*(ZOA2VTD]7%BU6Y#YNV%_1<Q]1XU(:LF07(I"HV$:G4@T>,GQ(T@M,^0"_D@%N\E$A! ZR(@&1T3-8P@G,1@!8X@FG9PT3$H$X)@D:@@EC_@OFZ ^L@@XZ,@\YT@"EB +[J''*JZ FM>*"@*0$0@;TJ,@ GX@^*FWI:$R.\6*:>H)/DP),X+KM0E$2''LT2+Z &1J@$^?K&3R)1PRZ(Q0%7''(*/!)Q8C(0BI4@Q2Z"-!,P :(GE:9#?Z.WC]@Z5D4H> DI:E>;)LLF)^$H$H0@RI@G4J\J\JL:IP]#DNY\.5_L.:''L,"95C** O9JQ''?ENUAE7T2-/*M4""R>"(JF)@''N%]RI+%U4\RUX"J0NMYC6T]S@WIS+\HD+:TI#Z@1D9@H&*@A8J1IY=D/BP-@@8LVS"A(C*&SU0]5N\V1=^&AO_U#YQ$VIP-6O?.)U^U=&A,=AI63^?]UGW3H,X*UQHF*IGIBI=YV!0M8\-UGI@+CB B&O$@GMN@CM\@H''BXIAZL]Q1@I64E:%(LA+ZM M\A0"EA "D(GE(BZ"+ANEK@GTH-@*X\A"YU %)LH2\(&I_@DF&@GEHB79L%Z#M*7A*XHY:(A''&PAW$@DMBA=><28#HDFJ"]6A^JL7Y\3NF*+"90T5[N[K=D ):GPL3.+?>H#OPX]K+&*O*/K3&UQ G: %0OTBA$ MLN+AI-@Z1(0V06I$>QV@0!GAO.BE,U+^$0P[7,@RX70BS(@@K:;!N :+.^Z6P1%+.9"E&0ATR&8\&LGF_4YX#@FM<ZI3AZRX"E%2N[[HP%J.$Y:*$N8P3D56 Z+!DK&-(VP@)&0P)=&&%X:JP3@@CF@)&E1BHZULA#[S)= A9B$,R^*.EE(\%MFX]UM!T,VE5TF(5>(N@]AUU="0W6<[RM<&:X1AFZV4F(M)DN*I2T<!4\*Y2!L,CJZY8:#JUFHLY& @U6 BH2*)0-CGX'' AQ$@3,^]4Y> BQ$ [OS5BXW8R#FP@SN@N_>Q1+.XE@!1TT@78M<@; VQNC1LDG*[4G*ZAPZM(E\J(@&300RU)$:NIZ&RU-TF7A[N)(3/"4VRB=<.2:B>MLI %J+X6M8J''\#OAGQ1-JOC$Y3W1J/N,T$X\0NK(B@U:C+Y4PU3PF E.@^I)0DJ1&''9$R.J0B+HQ3>ER3%O+ FQP@NTX@F.X7C1CK52[+E3W&G=(#G@+GI15XKE?NN5>\\"BUK(P%H<A9KHAW;V[B 2%T\4IYN$41Z-S@\6)Q]4\P!N<M>J@@*K$B% 0@@Y,@& 1PPX0@"H(@FJ,@\4$@#D!-75NQ^[?TC5?KD:RY?I#YQ!Y3P3>!@5^''[?>)S%D7;?O4^CJ\G^QF:U]C^9%S,!XT48! NV''&(6\4VV.4T_!*[NBKM@Y*FO#AA_!*#_:.1D5!9!M%''#T_P5''HHV<8TA@&VC%ACK<X$XVD4D/;V_U;G#0_;GLW#^EZF91W;H09$@M=R2[ELQ)2-?* ,E;_O(LA0C#F@A $PI(BC 7H4AT+4HZBB)%FT:''8@A+KHH!LS!S.A?+NKM.9LHFAA(M\BA<.21=@&_K\?2+];*XHB_18N=;V)QK5^\/KRUFYUBJ4R>KRYCO_!%J)&3-W5#AL.GOFYSM\8^!'']&Y0(X+Y(]2OYA@Q$J7Y)(XOB%N!@ILXCD;"! S6 1IT_''Q/YG,22E?>;Q7QA,R;GYIVY7USQFX<E$[''8<F!3<(?6^#RCL80(:!6#B,8W2LQ6VDB6,7=Z]LC_INUDTE:?DW?+#MOM+PC@LJ]UN50C4:E,3L@ P@1YP@9>P@T^ %1=JE:5YR8U/DM-WWGJ82CIG!''T_H,92#Z.:=;GJ#RJ$(=L<O(9\SYC,9A(+0A@$TH_PS%AHVNYT0;PLPY%0L\5_G#V\E020B@CU?J%\\^9>#I"=6Y^=$90=N#N?A0\0HM ,WN>JA11IK/M-P/Z*!"?3PQ(B3R_U,-GD<1;A$$M36;">D@$P/41WU@@!RQJBP7S0ZI@TB(3BQWY([I*#(EDMGT>XRG*21?=GH0Y4]B2:0^RSC$VZK(HA\2L%FD$+A[I<6UI&SIVRZ,)4NQN&R9X5^;*<2YJ%@$TSL"C09@$RE"1K&3H5]J&FE 2^0&CBM@R+U*5Y/W+-*''TI5:5[16:5-BPP''PMJQK6EF5_.WK)5;];E&5_/W+9=?[8],*"IE4""2)8EV;[+V[E!KXF1HLZIHT=L%29U2!RK)5@Y?G 9!MN&4IYA_X;FFQQ5R#RZ@*$4S_)4S):+R><<OU-5R$R[L''3J(NG$S-P-DS#AP0PQ9\6XK6L1)NRSE%AXQBD^ %71=*3Z.9OMZ$%TH>).?99G''5;=^/Y<CS''Y0<BS)\S_3X[E&''TNDS+?\3[?O:RFOSCHABWQ\@(%E@PPTMA@(QID@BVERE/HPXU0V  ARSB 1HIF[,!DD3,>."FT@8''SJZXTU4R1QQU]5B$TCH#PHXX%J'',*Q:^X*DF9AB0!B3/F-@,R/3BVPJPFON(Z8!@_PHE$BO30,8>+I[+;J+,!QFDBC14XR@AGRCI+R"%OCFF@BC04"S@&(UP""@X]ML#D1E@2<\JOK!P0$AD<J@''$D"Z:0HNNRGSHHILEL_B!"4OZYED''5V9;2Z^[T",-4)QNO@4''()) IAP13Q13*^!*XFDQ*<HR<+,+-W0525#A./J*FC02+35]]>V55;<,\ZHIQUHP!R0+VY6R?3,)00C#BCDVLRPA):Y=J"$DOONBS3]_5IS[%V[K9@Y1S=06QQ_O=]Y[6E*:L9@XC$DIWP$54XDHA 213KKJSIVN.,J,+D=K,52%MT/E%!BO@R4(JBP.R9RH@P1_I:Z88%:W J>QAAKKK.@*)]2O#$C>Z2+@I  4,U0XU>X64!Q#(D@GG1RI1@\Z=&0II)]Y7+%%'',<MYYD=]@"D,&''7+T2)F2 !@ 6+<#.6+HA#AV>H-AA!&$$E:J@ 2*+OX$3*JQ''[<!@]<D@$*Z^(1P3M(SV94DTD&JC!@@5TX"$PR+QPI@N?L5B$B^TJBVVNF C0@9P9F@PC@3(RBRQ.''7O;2_?\E7]K+WIJM:=\*DLBO2()Z''NLZ***+ (;;FS+89#H-M[RHU^KY:^==+,BF<0:;9H]B>1$KW''L@ @&6;^93H;/;KOPCJP4-.J@ ''D6RSVG/%K*Z7H>^9[#''U9E X9KC)D[6W;J$DJ&<<H:Y\N.\''6A-QNOOEA$Q;H"6>?G7?X%MH''OD2''3*<?.,#JD?_#@O4''C  HDQB@G[X-B(''% ABD8PP+M QDV(LOLE''FHB5GP 1HD8P]EBLKFD^D@Q]OL\7JD!T1P@ =N@9I7CNZ*H)T%KV" !@;^D Z9MN%IW!M[0S)&''1%.*T-_B)NN1KRYISB AC)0@*P6QY)BE@POG;+?T@0N!0\ZNHPFMBBBC6J0(D14@WEOJMABM$FGK$SNSS_ 7A-I03$8D&^J$;N#2%J$ C01@"''9T.J)T/V45RG+O(\4FJ.@E@L92R9?#8Q$^8CU!A''<B4,]F6KU))ZULD "L(-XP+;H-9''NBF-97[I\J/MHK%W*$YU3WFT,GZ")N.H-EORJC;;4-\K(SJ\:1W+U;(SI*$S6[(B"TA#C8(HIAXB"DYFDY#SQ ;D=ZL1XP/*ZU20Q, L616PL_JWO1M$3E/VDBX\8AAO*RL:_#[N];E3HH-QTMCJUZ)^Y:AD"_#P5P1ZQ"D_BF!G^@ YC1FTHV>/Z?0SV&N4H+H!%N5/Z?:SU''KT19T0L^I-B)O^=P<!ID2WJF2W6$@FNMJHQ"DADCDBJBA>H P 5X@C#M.FCQL1!P''J$2\*\Q1.YXD>V]NP)FC8U.'')&1%QR(\**LHGM)W:M]03%6N-0IT6*U!T/.BNLXZHVMRH.1%#A40J4%* #)"R L1OHU #UN$J6+-V-[XU+BB$H/*YQ1*#: $PB>)V>:>BGRD5E9DHEZKW1%B\.Q4)A@*2:VLZ>1QC<$8=BD8&U/1JPO2OCDQXBMJ@4,-N]''7UY !QDNL%95+S/QF7NY%R#TMXS$B1$&#;?53;6>YUJV F($-9B U@X5D%P&&4!+5SL+''KIR?HYERCG!O<&M[FIQWC*:I1J5AH&QFHO"4A@B%I0BPT8PPNGJDP EHDGK2Q"C59XWB#V:B MYZ);[;+M[-XY7?]&:%-6''F=-C-EGTR$7W:\J*5HAY*3@U!YKW;GUUA.;XF%N\%#E %(P$QWLJXT!EL92P!JOM*X2\PY[)?R^3$P,$I&T^LP!I/FISY1"E*OX1R->,X)%_D,];FD9JWQ-K:%#GT6B!6OJ\-=V$K&0!#GX2H.%),Z>T.B*R\&2?_&OT#ZK@[3E5\)/1_JU-Y1%BRYH''''!@XY%J]S1H8KM)!IP05W+GUT1^3XU/:PHBCH)PHDK5&''[.J$S1@J["OX]L[-LA7H!3F S?JH@FZ<H[R99P*DT(8A@1,@@QPJF@F2Q"LH\H!GTS0PPD[LHNMWCBR\ 5ZE >+:^2IGVJLK\[(XHNJVPFYN&R^)WI>&:2G+OOVJSZ%"O7F''=XUY=C;T2$7PUOL-D"77OL&(D& DIQ!U P :J-(@UQV=+U''''Z6,[7-Z7_[6-?V-+_CCV9.$5/\9P97HYSP"NR 0V#>!T:?_!%X^+N/"J$#+O0\:V->70=#SX#,U+&3YO6T0@^XAQ@TA)RPB\WK8P>G^LP%O''FJU=3"E<]81!7N(DT<4TZ.3UE%<H&6_[:/$DG<<P5=(DMQ>L>7O9P,0O1ISFLY=4-*J5ME&X#QPK]I?68E$Z:ITJJ@QM@HEE60P@$( P 0NDDGMF"D!#Q A3,4@ 0Z(DLS:N@D9-47WYSJ2PS#"2:0W>*AL>F#G?281L/,R9B*B L1ZSW  VU2U%<]@"M#56>>T62R%XR00CD9<5\M(YOBV8RF"0]HC?/A@& 8Y3(%_4;JS=;2%\_<9SV_^\9/7/N]A?7''Q\?9SFB ACY&3.#89T-KB)E*PN;''6AIF9K;W''''[;*6XBMM%U5XFLO=6T<%PR$UH4EM?814]><)V?_NX77?''O!7;4)W?<F@R"B78 @#7IV*42L06F2\J$LV<[XT8":\54NR#W(''S/XQ.2J5$Q!]''6GJYQ-(7''S/>8J^_.=H$.1NA@B- DK8"DRKB@CD@DD@''@Q^@4LL"DAFPBI% DB: ACL"YR^F)NLK@B=Q@B90TCO$\(7 59:@(:DB*43$X63+A.UOA+9B*/[N=E50O+B(L%C,9X]L&,@J%,[H''">(LK_@C*/,"!1CBK"KBHSSBH$SBH5SBIFSBIWSBI(SBI9QBIM0@L@J@I''B7(''(N<4F_XCL0F+P5!"JR0)(_FCQCW''$OH%@2OE.]KI$C#TB82# D8V$BOOBR@<@COMSCONSCO_SCO SDO1SDPBSDPSSDP$SDP5SDQN3CI B@;G,7LU,!G"D92[*5N:N7*=D-=O.-E@BRR6(()$+?IBL:K 5S(#I!+(NH&7WBCW\I%95@"T2H@P7PA@Z@E0Y4QZD@@2Y@ 1"($41@ 4ZX@5#JK?,2CWN)I]1@Q#)ZQ);:%ECQ0[TALK ;,A9[,/F;1BUHLA\<06;\B0\#E)*;.6AZG49*%&]I/Y1+.<5X-:1+@#=817"D17&T17*$17.4173D177T17;$17?418@D2@L@#PR@-=W[LP#+*$N"NP$K+B5I)"JCB430!BW01(/L"2SSOPJKO[I9,&9*"$KP!D" AA<02YMD2YQT2YU$2YY42Y^D2Y"T2Y%$2P;9AD$0Q[HRN^>[C7GDQ"F2C8CJF/R#L?+@QJ?8J:[RL>PJ?3$U  :^D3RU<X%8X[TJ:[*.21$JPP''LBR*.%J<<:$*0?D(7RS."T#6(JDDAD<WX@22$]K=]63^L%D/@D@2>Z!778;6/ (20.!EQ6#9H@@LF\@IMFL3BIL3CML3DQL3EUL3FYL3G]L3H!L3I%L3J)D3/& L%VK3Q4SF>,#^F^#?0F20[H$OYFXIL^HI@&L/U#H/G:)?]*:5DJ*@92B.''0HJ9\[S\O@S]9L7]=L7^AL;_EL; IL;!ML;"QL;#UL;$CD8&BHUKND"KJ+M<>!DB$1V6/J4[2"D&>R4 2$ZYX3O8N2H>02-I+L 4VQLM(X7<0"<L"RC8^,?6%L<7Z$[:''O=OSK''OM6FI?V*B4IEDC.LWUJEF6BMG0UL],PDRME@05%3M/6.=$;-D*CD<S7*V''O3K)(@DRD AC^WPC_WPC WQC1WQDBWQDSWQD$WQD5WQEFWQE]7P:B0_^P,6!+3NFWH=&(MHKT D"[0NA+@_A%5M#^R'' ?$X[/*O9;AM3UCRIVWRI''WRI8WRJIWRJZWRJ''5RETKRT6PBE?*>\*R;#+PZ<1N(X.D!.D"?!CH9^%-H#(&?"D*J.6H<PH/JUXJ1N)V1E+MSOK7SFMOSO.WS]G''F?%*!KT3K.J/QH]$=[KR5E(1KHC5CFSPLV5/H-^PJGN3K2<ATCJ4VS0 SS$6JS W?5T<UUT<%5U@-5UD55UQE5UT=5UYUUU]%5U]=5T3MTD6EAA7;)SXDS_J3PZ\J#=I4F@Q@@4%05K''<-8B#KZ<R&2]CNM-L&&_]CF!U&6&U5&*M5&.%U&255&3%5&75U&4E568MU6?M+KN,E!Z"S$,L0>5XL>;H+RTQAT68!I\CAT><,=_K1"21N_$02@7SE3Q9H#Y)DT/Q3;H+VI8 VHMUVHP=6HQ%6HU56H"5H0VX@[W;(?JLTPEEL>FREUB<)@O]1$X-5!\D1<@C2(TB1RD2Q6\QJ2-56Y^E6Y"U6Y%=V[5"OZ5R2!+%6DP]L&T:KLHZ6X/DOV/"5_YYJJ404&"<?=B%M]^&!UF''Y]*''%]*()U*([]*]_BEBZ-],F%J0\CM8-PA"M]N"''K$TMM*14CM= #U*RPC&&!M($3XE$].8)].9-].:1].;5].<9].==].>A]2?9\>5(2 QIDGS^Q*7UENTE[3CBMN58KV =TY 42+G=R^O*USH$H0%RJH7=]3OA]7PE]7QI]7RM]7SQ]7TUU7UWXIK<BT,LEQ]AR1%W]_PQ@- %U0F_X?W+JGKCP,ELC E8MP0HU9OK];#M];$Q];%U];&Y];'']];(!];)%];*)U;*GS%=L$*TV4E!00:UX3$E,@83;T9KED\"GS=R/K$>6:W%0" <V@QI2HQLXO<B>+W_>,W_>=W_?NW_?_W_? W ?1W @B[ @S[ ?@VQPK@@(0"E_/WK))#FUS%PE_3I6:JZQV*$7N5FA3WYSP*?#!6 04OG"Z+Z*[UZD4[!D%[!D59!D<YUG&N2H[UNLA62G]U 5*P&YI7!.=,J71LYSL7PHA["HR["H#["H4["IE["IV["I''["IV:JKRV9S?SIBAN.#!EJ^CV<NUN?8JKQW+U\>IL?<''RJ.3HUQM CKX"D7?"DM''[#M8[#NI[#NZ[#N+[#N<[#OK9#B,@CO7"B?"HSRYP5D:PR@RH6E@POQ+5!L8QT2&*?G6O\<MC\CC@Q\[G$R<[$SM[$S^[$S/[?9D<F9U@V9UD^YSCX+8R\7XG[U\U]U<ZHG<MZYK$TT*!R'']4!(C4  $4HA@TH!E;69U<F9&@V9&D&9&H69&MF9&QV9&UN9#$HAJ_K6.@:TN?ES$6D5;''0(W*%X GBM\YUW:[T.SH6ACP @ @@@CD097MVY7Q&97U697ZF97^V97"&97&697+F97/V97!N96_Q3KWILRWPVE(K/62:)!R48IBMYYHME"<H1;/$W&04OCBHA@CX 1* !H3V:H7&:H;6:H<F:Y@V:YD&:YH6:YL>:Q+0@S<@ A&@XSC-Y )V%);%4XT>0:G]2A<#K(UR@B60:B:".*@V:*D&:*H6:*MF?>*$U.*%Y.*&].*&%)M2K D4*L9V,U7,K@.!GEM,I-,Z$-P4/Z46''S>$L]01RX@[BDBZ,PAE,HB6I,B7- B8%..8)..9-..:1..;5..<9..==..>A&0BOHJ5; H&JFL?\8)AE#CVP]$ZMQ#WTP.6DE&[Y#C\TPS]F[=C[$-X,Y)K\HH\N)/PO CQI.4;KF4]FF7SU&7TO.7TY.7U].7X[.7Y!&7Z_.7[%.7Z5&7\-&7W=!H;6C(4V4#[W]MG5#W\_P-KR@D&&F3J[*04WDM6YTN4@@LJ(@P:0N;,5.;-9.;.=.;/A.?0E.?1I.?2M.?31&8H<HEO@HOX;M:5Q@3-YO<9[B;_,^GH5?OI_\T9_54[C]V.;_+/@A_0@R_0@#_0@4_0AE_0AV_0CV4;64SRBK:J0R/.\V1HX\M [''Q.W>M .&.]TNR]EG 4XJ:>W"91]?G%D5_1EF]1D6=1EG_1FH_1FU=1FZ=1F''?1F=_1GN]1F>=1G@<D9(Y//NR=;.6N6_OY-5"BQ"@BQ-#0_-MH!Q(.#$Q_<;DIT,[2KM_2K^_2K!=%I[C0Z(3P0\,N+]ZZK$9T!90,+!A+L/YK(>ITP8 VNV=[N[_3N<_3OM_3O^_3O/_3O0_4PO?3''M3A1A[(V\O\3W;/FPX,QW92W8MT<AL[;Q!M3*U33,W4AL#4S]_?=D;''=D?7=E@G=UDW=UH''=UL7=UQG=UUW=UH7!B%O]L[V6ZHEB>P.%#''0 &]:=E;C/\ 2\$('',B,Q=*T"=&D7=&IG=&MW=&QG] A2=&^G=&"W=&&''=&*7=&-''= GB7BDG,'']="0Z&U><<W>>=1!86+#7[;?ZU6"1%=8)*=7]7=7"G=7&W=7*''=7.O];HB4LL]IHH&Q96F4@/WQ$28EP7_=PT+VQ/4VF(^[$,H@>BA>H"W>H&''>H*7>H/G>H3W>H7''>H;7>H"_K\]&>F:&UI(>+N X78M'',B"?Z#T#]2$A$)"G>Y&7F)(G$)+G>Y/W^Y/''^Y17,)<G>*@W>*D''>*H7??*#Q?*#77$''H::.=]W00@R, U]P:J61QWLZJ.9CYWN3PZK"LVL8)]&0E?.1G7-XJ<=)!M7;)!KE'']PY!$.UM3KJY^V#UO"5)?J;!>R<1?.=5?.>9?.?=?/@A?3A3?/X7EU.W<!WK$N893/W=GU+KE/"I/3IE?3J)?3K-?3L1?372<,XU''Q-V H4VK'']$!#2!S&I;%!X ^Q3]7M3#[KU!_7X%?7Y)?7O5_^@G."EE?%"R%Q]F0HEMW#F+:(NM?IX?6BX!?"GU7:GY?;%]?;&!?;''%?;()?;)-?;*1?;+5?;,9?;-=7; >_ALV''NWK;A#"$#!!?JL6Y!#(S@^]$/C:??>>O?>>Y_?>*_?>;_??K]>K:;A!*+QK@XHTZJ0CAD(\H BN!P CQ&B2^ECSD, C)''84FID") ^"#*$P0\"S9:0^HID4"PV$2(/ETJ@(JWK%3E''4''PID6YM&S%73&3)T>[O(CY7B-UYT2#R(SRU(HQTL"TV*E LWZ*!AXN''LA$%T+S(\JIE#E<5^,U$ZT$ N#*TFF3+="7\.GK''4*5+=2;^/G+]C%''T1@,$TQ,5Q!P+=*KC1A$[J&:<5SG$19H#T99,.SK&29(3\=8,>[A"1HMG#2U-^( %TX64 FJ;=3W,6KI''4<Z;1L$^A)8ZQ-3:FNO$3,H=C2=N?K#19L]G\27?WGE,VH"GLXUY * F''+!CC/$@1[A+X,N=''0=^$%DTD318FBPX6_P)5@RYL""2DN&>?_2Q=OO_;;<? O<IFBBA@1)XHHHGJ-#__($($DITJJUTT$&FJFGUH%%)EM*F)HD6''TMZ1ZBVZ;VY^BJJJ^9%"QMMJIJBJAVMU=ZF9,&($RVPAIIAH#7:>BNPPP(9IIEEF''$$$$$*.RRSO''ZAPRZFVMJX^Q''Y^EE/$''$8''" LZDEAHR*JNRZYYQ)461:MI@BQZNM-=I %''*C1RQ]M6''$''''''''*.^^PWV12P0JV>LZ!&>DYN-EY"EAB!GXIK]QPVXUJ6&YCGW4TT IMP_UT@(X0?3@A@E(P,\^(^9!:J**)*+(**:6:>"*,,\+ZJ!EM@@C@I "LMJF&T5V%QXYZJWZXXXPQJ!*"ZME1PH%&N/,,-F?5=Q\V !''*Y*S$[Q"FID_\>"688X(;K+''%&'',...&*.2:;;X*+1RXIZCT)(]_N6BF"*+GV[A!TIQ@-0@F[ZX SQJ")IW&T\%QHIN8:?CCDDT,<<[''0P#J%\>VA1921Y"6A! <:ZL\]JB% 7CGG8''FH''''*:;^(T+1M:.$\SEF"""QL9:8033#,;0[ONO L]=L=E@=734TP+/[SRRSN],=MOL773I#S80X".TTF%*XT5,AC,&5SZF=: U8*F6!A($O<(LM--&<!"D3LDQ''YW 3J7UV)L4JEECT<0$,G_ F\ NNFCC?878(P''K##""S..NNNQO09988][K#''&$5]N^^RP\?988JDW?$PWQF#A"K15O6^#^FU[ZU)*W(KYE$HTLN@6;+''+-XP&ZZ99[K5W"(J@C0C8@C#(''X-^.NGMV;:8<8X/O?7%#"=>?_OHY88==] +W37#7''OO"A5Z_GJI,JZ=S''_JHU)GRWY0HZSP1_]*7E2G81W$$P;,TZ!R!JK"BT<Q@P=N@HL$[*C@ASJ0 P9<H@P#JLDIT+BBE)Q @.^ "BX0H!P# X=STDHU*6@%C@%C6[ABP:O)''BT-Z=D]CFO?*A6?@DX0V:GQQ,B2,]:DPV=M4DPJVJJDP 2Q"HT0(!FG*LP#E+FISG3"DYG(QB%B\X)V!FHU,4#EKT:Q"5*<(!_CNDQHRLHB)</JZ]Q'')X7U"3S:Z$5[%-BHOVQ@!''Z4H<E280%A;[@4(9''H<B#!!2\TP(!QEFLV%:!HKB92"X L81Z?FDT0R/JQXN0"IY-8BRV@P 2ITHHI;0\ZC8%-H=V9S/2V$A*C[J\;7;D_0.:G)XJ$Y37-BZATSCI@A.1AA1)@PB!B@PX0BINX03P&L(=93FH*,9#NUBX4&]''LXS8S&]V$I#R1&T5)S+N[0)Q&LDM1BB=\KV,@3FWW HUF?;*Y;TY%:9!FQKR6N=H3]7A;TX3BM,,45. !P0#CH^!@A@XL0Q@INJ !CJ)P!GZ*T8XXXDL]6-BEI#R"D%7(QQ&*TH1RMJL]#R!GP\)P$VY4(!+-ZDUCZ-JTV-PPP;"DE;S0AB2D,''5[,-.LDFN# , .S@XI02TT HA:D+U-ZC)X<KQ5-$@60P$I0LQFOX+R$:94*""%*DEI^-VU\''V*W_6*U5&JUX5JUJP_WR!T.0B@K*COSYPBD\\^$*!EBTPCLFJ%(59Y%+6V9(Q^6]=GWI[K?9FD G#P C@S"L@[HE@R$ @CX14KVPQRU+JO-^1"F3-Y3B*V,9G=[FX;F=''PW/?6,ZN=;FYS6=''RY/Z4#0VC@#ZX Z0=AXR>0-@:S8OCO1J*VL%JB;NJJ%2@S\,K5UIX?#K6&A;R0F@O5^U;( ._$5@7.%KY%GU-Z=,PT#]&D%II[ZFK7_FF%;+ZOR\@1[.5BF57.0JD1AHJPT9F^LIZ4YFTW<$2M+&>,U''C?R?@4JP[7#Q''N(M":"H $R%\(#^@9PT/]2OL7!@>QUO"KZ=7-ZX)DFJX.;%4,HT''7E4O@=B=6KW-"M/SAT>"C9:&HYX_T_N1$@''DB9J G\%LE#2DW\%XKLLCH&;98T7MS@^ZRJ1&S5-Y5$+6A*Y=L&*_SE$''Q5&3)*6,Z%WKV"\''&\''?W,Y2$9],YPRN,93.BRCLHMF5+:4S+/[#4F8)L"V5/QC@]!XS7FX@H2+%EK''%LT,O:Y@[HS\83U-KK7,G.;T3!3#CD?(0"]G<,/V"%=FP+/BDNP230_X*3QRR]H MTP!E2ARM/9%%%YK*5!M6:T,>/SN,2<P;74TF%.\!W!LR''H@!H;+W%O85 7=-ZFDC&<P(=#V/ND1)R1\;4YVNFZ\1CSNQIDHLWPC%*\&61&5C9IS0D< \*(UW>/F1NP)K(;$+1S?A*!\%.20 X(_I6\Y>.[MY#.1%WR-&1>K[2/W^L)YG6>?U$#Z4A[^,YFOKPP>JVB(#/H*)R7O_RFVLLH])?>D<X:516QP7LK0=F5=G>T>=F^2V$\Z%5#P<8/U&.M7A]''R''MR1-$-A<U6#&=\,;[^HR!;#Q;/Z0HQ@0W04)=8R05J?:TMO_#SL]QW$\,@*/MSXDJ?#$V/, 3Z>[<*3_?K(_''GSWA[#5&6NZ/A/V^XOM#''K+\/+RLZ_.+-WJ5''0^7\7FN ,Z/!6#LNCX%PQ.8;&U2!FO6IKY7M5%K96@YHQG.\*%+S]$F1?9JU=929[/L.XQ''.R@A532%Z<<&^^[MY%3M97A4!XN8\1'']59K''''U.N.3/@+\Z.''N_=@IMQ0S-O0Y#6!L#-_&3!733X[?G;\G6LD$N6);%"8SWK3-63/>]OU4H973''JX%/SED7K<^<T9Z,?5C,WA7;<\O&*D?MW=7V2EPLJG#QD?H=<=,3ZYD,''?''@S?9A8Z=?% ,0?/I''.Z_97I$IW?EIF@ABW0D:&<0=V''(=UKU]V;''-D?*5T8<%R 40"/3$EV<X7]&$V6=8A\.@QOMA& @YE&JA7&LE$3Y=T2!85,E-%&,9''+5]&\A17.^=(N[%XFLM7@4>%,KM%$"\''I)]BLR%3:+E%WR G6=!GA;8E?$=HR/Q4I95HMJ=ETT0%7M)V(P\5BW\0A3D0A3\0BT\UJEAF4$477^YE?U5FA[<W1)B !IHP"[\0A0*TB (VAAVU><]'',-I&L??/U_C,U5SK@DBJLKU4AQ!7ICQ=UVVRI1C)LY*0ADTS"I]3A+TJRH).QDBTDJ.UY7JX\D%HH@\J-@\R(HR>E9JJ@E$$ZJSHT@[0$\!IE@]3.D\!$HJSA($)D@(LE@)''.J.]Y4 #!^E<Q0@$!7O%Y4AD&LY5!XV2E6K]RC>\HUW;AY5_@1]2P?I/MKQ:QX[@]Y:(FK1DQ$CEI@&HHB?2RH-4"H]%.J<(QZW-^L1:V@N?).?7U/&RU&?:Y-#.RLBJXC(*U31#QB[[Q<E(!([^U>OMTPL!H0ST.K89]''\1AWH,Q;G?AM7CA+X*TR&YLH"TH@"MH SYDK;''YN0JTD&.BJ%?:&D(REZK!WBCQRB^4!HH\Q@A%A@H''QA''RQBA#@@FK1!LZ[$(1V[^6!!-AT _H!Z=,''KM(IG#5E G?FTE"SB*8%BFDCBCSR$P<J^>XWF-"T]I*3_RHZWIC!AE5A@''^P$A,3AFJ[@GCCBS[9%E3@BH(SBFVKA#,C%SUK@I3PB04DWQ=+$V_XHA!3BIU#Z6?W\<C%_,!6_=OG^@[*_!;WG@;ZU">FT,RRK(( LM-KOA''*_A4;J8@UV^8 WRQ[Y$X$YD3B@D20BZ:8&Z2;BI& BD;P $07\$.7#X F\ZMV#YXDVY75YO$;Y&OG[9&DV[F5PA:E!]ZUDN)TPYD1#YU+!^O<H2( DU5YBX\_Y$BL^XX\$Q*BU7EAJRD$DP 7,@P7(I@WP0A9P@!)H2B]^WP)\A0L<5M[Y''M^)E4$  @X8"BGTI0I,@*''\177P@Q84PQ_49GQ!GT.NWX''UISD^XM+9GOY]SW55XKE8IROZSTEDX+M<#A5 0GUB(RG43"U2(N)11BY6("F\44#D5!;P0W7TPBSPPAO(PBM80"T8 P:,)75D $C-@PU$@!%"@A8P@RWDZ@7T BI @A#<X!-BP-85@Q74:H1JZW/VI4A.B-\M(@CMG4,"F!-FF!<FH(N)&B^AT(3]%<K\52MZH0YJR8;=7XV2#1WRT,,(I0MN2AJLH1;?%JNU!TH )B\^6HD]_@R!6 DQ5D@ &BO#7^ZXC]L\7L0!;I,=J!$?9*NT?^[$!Q&''C&])%Q''VCNB0VT $"@GXGFG7ZV!AU(\KXVVHZ-0=;Q%4(.*M)I)9V@I@:Q:![X(''ALJ"TD@ NM$M1D@W;HDO1L@Y#*@.>Y8''JLDS>HDSXLKW0Y?IS\ /.!/=7\@''JN(RUI!HHD@FZHD"3HDBJ@B9Z(I:[(HR?JK2(ZKWMQS=8QJ58*J"IRCNPXV($](SQI1.)^+**P1T3(9AXLH\^HDF/B+9&]?-:YQ( FV&''M%H>J(ALTF9J$@LI@K_YLH%KLJKH HS$F, NHD ]XD$K@D$?31AD?!@H5A,.YJ+IAP&]"DBG*QII)B+1VJ,AQ1B<<F_P]''''F_[,J3)X.6Z$X9[!)_D^6,&+I41&>+SQ==VZ#FV&P@@J;^SU%FPK]6I-X%!J?=0RTP8PH"S^8 6S@"1BVWXAN[D@IY %JFR@TJU NMF[LQ%''LF&BH''SAGCA*/_D [3JYCQ*\7?:&[:KV\SXADB9''T9!^U$3\YZZP&)9(]\P@C[0^0#Y]\Q4WB,4)EW9''H32W/BH@A_!AE8QBP"WTD(QBH+!HJBR@J!ZBSHHADSDAJF!AE2B@D(Q"HVTBF,0AFKQ!R(#"ISREL@TQH% @GR0B@''"XI8SBI3PAA"S@D/<X ''% @V;8@A!HZ0)$P"B P[BN8P\%PB#L@Q(\0.5JP"D$G2R@0_[&+[J.WJTM(?,R8-BE$($:18VN4-%@8+:4QQ $0O%V[.3]Q!M@''^Z"G8I%7\?MPP78 BS4"?Q"0 4,""X(0RHP Q^$PDEI+9380A88PW<>@Q:@"RVT[$VEU0J$@A+ @P4 PO=F;1@0 P<P RXLDAX 0A0 @"K$+W%]@!L P 2@PP($T@+ XR (@BJ 0P6LYM/E3A8BH0$J(8#E''[V!:]4 GP*MQ''W$GP[ZVM=9Q6^>EZ6648?-W(P"'')G)+VYQT2D00^Q*@CIIE&P]0"E$0-3^@AL0VQ4> Q7?T@@Z2FC_S-8;@"_$ZU90]&(O>*NYJZ["4*>?I-44^.VTKFPS@#C,9]$T*,:VWOGH:^+3$TQ<6(DOBN''KYD)@:T@ XLD![DH ''FKDL$HL7D@#4HG1MDH&Y@HCXH@SYH@WJLH''3HES)D@#Y@@S(HPRNLDS3ND&6LD^V@@"JLGU^PJ8>,D&/JH''K@DV)K@]%B0XZD@WJDI= DH#.FIIL@D#VH@E GL.O4"3( E=VH@W[LH!7JIPQ->"[Q)R''(9RY+H>=QGE4V''@S"T%I63/*L''*6]R-\RK;KY"VA$H$PL@MXHK<XXI<D\D"EDKAJDH!VGO2TW@S%@0$[DHHE<HP&MRG?9UDB-L@F@0A?VEBFQFAD7 BV3HBJE"@$!:3@HFAI''"A$#IBH2SBD3 3E#CAD7#A_''PAH)3/@$Z("@$!X^U)@*+Y4*8UY];OAJKZKL6UY&IM5WK&;SD%H38GXA6@XB71UOA)/NT[YP6SD."M#T(BLLW#GNQ2A&A@HE0VH# AH-@1@"FBI.@0JA )H=R57OH#:MW )^9[HY>VO/:1C2K'':K''W09VPV:5)G3%2-6GB"EA.PLMZ]%J5''1W]XJAG\7U.^KZ!D&C@G''#AI_P''-*Y@:B8BLA</SK[GH-@@H?P0IX AI^Q5(AJAGWRAI''RAC$QBHNQ"H(ADB+RG_MEA,FH@(/<*@!)\ ''$!POL>@P@%0BU,P!OTPD>B]@5, -''R A48 QH,@QMD  98@PY4@R^.A2Q\P"3[PPZP)Q7T @J$WA?R,8TQ("D2@"IV''GPFG/8*GX].-(#"1(@UB).&60E?DG#Y9PIGRW3@LA84P#E[\BF0=!''N P9P "R$@IE:0P7@]AB%0AAWU:X   ;X@Q!(^@H$AGKO@RW(0G1#PBPPPRIH@(9&@A;T@BL< RK( A!8PRD$0BDDNR X\2SX0R[HL4,RYZ>08UHCX(&M1H(%@&UF2.N&G-:8*T@0QA_+FA C#>5-[W)4+[H]7):R8="2E H\@ 5T>@64XFL5P 7P @??7C$%XH@$!HHFT@H%!KL$LD@MVH@FUH4VFN,"FELLL#X=\)&R/RN%\"(> -$!3LCU=NU"&,RZ''R)E+,?B+(:U,F+FF_"]3];\-IMWS&QF9B)F()&(IPHQYLA*S5!;OLD^_HI>L+&.7C(QL@HBI\KUAED >H@_YHCF7 @H-4XJ LH@72H4Q0H^ALH%EJ0OZHA4R0T4L4IGG0HSLLD!3LD";B FEDH&^@D]M@HQ PD#:H@"7BD#DDDW''NPM(OXO(SH%4HG"WTH( CCI#"BDM+TPS(U<8WOS?)&FN/K+VD''8BZ2)A3A.DKP?"12JB)J.[Q VJK@]ZL@M^O,\M@J)VP@X?60,DVA4_;:_IH!7OGL,DU @AXBBE8AB$(>!^JJ0GZ2GIHA;HF!@IKAFJBC@D13O2<X''GUABHJ@0''"=*HZAACPB@AQQRE>@A,+ODGO @G_!2Q!:[0B>1T,^\YE+[L;:Y L^U-6&&-E"-H/X3D#H88XVD 9]]X_GRFU_V0K69V&#B''K?V]YS<Z )7.$.BO8*+@ RBA^2A :BAAQ@AGWQAG3/*8A;\86.*B5K:)C<^Y_6 +!@-)?>J\<;*!:P)=:TP]R:K*39<OQWWIW<\,)C9P7A.-TZEHXPBA^@AH2S7^+TGA, ;@#PBC^SJ<&6BC#ABHZP@-6.@IVR<5]=@:XXB]/<( AKHN"I@^2G*0A18 "Q4PRP(PK^JY;?;@QG Q2S8@JE&PN=J0/!" \&F0+!GP KM:BE@U]BA0!9 ^8-8@QLP4RT @ 7PPICMGOP!H4A8 (PEB:P%![38X^QIEJX!&IX<]A Q44RIP2!Z]K!184UL%$P54 IJ"R"SI5F&UK&RYT.WK6GF%C&SY$6WR93,X^C)8TVJO!=R#B J@ZT&FKA8H# 0JYX9MY+P,V@!T 4ZN(;DRG@I@1EE!YXLGI# A 4Z!5H,6!N5A*V6%I8TL%QPX@IHZN"0<JD($ UJ]/AX&NMI2Y09%>0*PPNEA*IP''7Q <@P1!XX=E$H1(ZL#Q*G?P$)BTRCBBDFB $''EK!U8&#V$5Z^Y.!;8N#W-.%":@N!R,*IG#1,3=!P*L\0RMIRH,A122@\(RDM<=>8]OFOG#*JX8MF147S,)WPML]"#04$(LIKN ;%1G#6BP5\5(I\TR%J"I*@N^YZT@X>WNX)+0HLB?"0HIAPDM/D!$S$P$@>==^QKC4HI47.P0 \%0YB=BBGT<HXL5[LP1@$%TR@!4+9+SR%C%J"A!TT<BTL:##C:+R\ZY:111.HB6Z0$&8@LT, !"R12IT.\ZFHFRDPA**L%+KLQ2):0(4N''AIQJ3YAB." ! 4/&" 7KI?[H(IAFZL@@@T>07DPG4 )9((%E?3A1* XK0F(JEBHXBJTKH!AI T4DKLA#C"02:ZJFN\J$21LDFF''",*''::$JCTC1!L0UI@-G@BTX,ZLHBLFK00P-I9,*TSD7@XLROF)98X)MM.*@A#4Z0IJ"5WVES#R:&-%QDB4XR$EF::CQJU$*K)!1BE@Z4(J@P%H+C0!@#,\5V666CWDJSOWBE#,X)*[/QVPQ<&ALR.5QSZ(9H?M"KT%@0XBHEP;#BP1DD9#+MD2[,(HLI+(" )IEC@ %$#$A.DOP; _CRP-:>/E#$A-\DP$BAQ)3X!@H;- MCDQ<XVE^ N^20PAHF]B@"@01$_RK@QDI)MC[[^K79555](0.6KO>10CHQL[*8QDY3)<MQ.FL-,&PIQB#QPS$E:JC .Q.UUS)+!90=Q@\<DLGR.96-YX@HOB95$K747JLC;_W@@DLA*CJHPXFE''=BAA#RT.@EME +.L9QPLJB#B0[OH1GD1]U&_F7GEU_/<P )%;311>UV)H$L5%RJ;HG0+TDK2X:FBM$Z!9L.]R]%#LEG[&FOW_Y-!5"$BR>XYE++\)$^H 3M"F#DDJC+T*H+K9S(5=<TPI&3, 40JHQMS3CP8P$D%GCU"RV\" PT.XKF  I0?PQ[TD<JTTRGP6=HIII@%I<MT#<2BDTI331K(Z%KDKG [D*<(@ <Q@HLCJ@AJEBU%@S?]DTSM?"DE.2 "J%PR!EZD\!*K''"35J!& :AS .X2DBO!RL%IV_-I\A0BD)FPABVXXDH"FC@;F\:P!#D1QD96X &-''Z87QCDJA$1VF3/QHQ@)<@36K F:R62BBE8@75  DPL<5@@LJ\A@_2SQGT!,,U^(PXPN]G@HRAPB@Y=IX%IR(H@._J5*(OBAGQ  "T#8@@7+H( B;N@E/=''!L!J$%@WT5B L#$46F_3N6GYFF7?EY"9]DDL"#OZ[''60MZ1916''FR,9K%MN]*NX(H]S#2)H$8AC/Z6T&VCN&/IY MC9)@PHUDIH&6:T@S<3%OCF( A#-P0 ^T(@PM?@@6L-:@?0H@<LLSL#D8LF3B\@Z"''H#V=*E(*$6ZH>HP9*0Y(,%M+''JRNLRILLV3)[AH]C@*7P"S9"P]HV5\&IAQHJ82+Q+N$984QEHSEIFBI(ERH%FZT#,-DXYCVH$A.XJMW^Y E U$:#R&^P<^C)D@C[0ICI8X7"[0PI+,M\DI]W(J'',I"E2<@B FIH@HZAIT@P*4OT^9#5F,2EX(L>@DCBU"BHWA*BB0)AQD5(@L#4G@HI#!AJ)H(%RI.0C6AOHDH+H+TI08QURXHMQMJNJRJ]BZ[ELU&BP!P!DJJMZ9JZ.5IX5WV,:H%S9N,D =O*N][8[(-''H@+@]XY9]KP^R:#KFI]7/=1U05<H@%K9ER''R-%JU;:"ODOL+0,E0R (DHBISD7VN>-J0Q]) HCA9#QWYED_JA 0A48)  ZMBHTWR!,4 PQBW2/S@QD3D]UCJN@PU_QYA8FVL:C5+HM];AUSL#V47YRN$''\%E4Z,T12(RT4T! CIRXY@MZ-AI2!&GV-4*NP5OAP4-6N3*G!(ZQ9I]F!-;(DO7BQ1GC<(( -]RDPWFLFHQSA!\H]PA@BZP@GD ZE0"SAP!QQGH,-]*DHC3%C#(O''L1FFN/OIQ@@ ;%:KW!F94H%R]N%FHH;E61F$=4,FO8!)"D].$]+_CP$MJ*BO XF6R/&LBCXH7OMX$!SB_.@??D62:!BT$HAM^2J<RLJDI@XDAH9C(P!LXTX!K2D$SX_BD@''1@!1,XX,^YLD,&T)@H\FDAD0$X*A4N%P$J5N@P$''4X@#KP!B](UT-F=,L''%A@FTW"+A@PL!PCQXH !FHHI%M"CA":Q)JJEHP2VTDH#F%EQW\6F$E^5C[@>*HT''LLQ4NNI]\UEXWQVNALR",DP!4BBIDX>Z5B>Y:4;D-T:2W,RG_%!DT6@J"Z]@HAM!FVSPB@ZJEAPGD8YH YB7L9\''8@DTXCBD]<VT@#S  PZR8M:.CAFHOP LD:HH005<DK1BUB<QPLYDHVZJ&T3P0P87<E6G''XBV7ZJ&"85&M@_Y?85U]*<FR;$Y]E$/7K-LU<R]1$FNRQ" S> 213&)5#A2T\^,GC$+N=-!*F:?^3YWE)!1[W.[^^S&94,)@PDH2HSBL(D@I/A''%2:;@PIB0\0.JNBU"8/&M!4W\6*>WNX4!2VGH&P"!Z1I7!94$S$7[JM6H)1\F&;]:4*]]JU;N$%KZ-J*;0)5E1OT(KW1!G6I#X!L7B@S"OCBG"I1*J\ESAN!.HDF;HA$ADCBBW8(]"DBXYRUZ20QN/"D(BJ%W?]$P@,,IVY[DY@B)"S%L\LZ?G_,L%MPJD@I)KH@@I*] DW(0@IZU<@''?OA''KA2"A-Z; ]$?8QT&LERC/''K7U[_$!_=!VU"]F5[VO5T,GIA@R5)K-?7-9?2-\OF[!N$\R%FNX$^-J"W.MFCB,<T"DAY-X-*HXHI!4K@IFC<!EFD!$1^L_]/U=D0 "BBBC)3]L7<%P@E:X0@XP''FHRO''ABX"J!@8VHUP''4@@@L5ABB#I@AEC<E1G$Q Q"J&.+O ^U^.-''[(-''I$.8H,&S6@''!) L(''FZ9SFITTBJ:**ZSWN>:&NX''QDF;.N-3?BT<1*O,8"YCF,2<K&8>IHDB<&,NA.\F''$AAEB@T''D@G_D@C4L8NE$FV)@<T4@B[R@QB!GCA1(,H2Z/A#C@ID\12..''A5L93B)CB B;6^H<AV:3EK*3C7J[?47B/B6.(1GB''H[HF.1:04$ I^HQ''>>Y-<?XDC2#A@&+ @I+(TM!DD/:DA"#@B:JFC#Y!SVI@A?0 DVH DAQACOSE__Z@@#JASPH!D(" A!KAB2H!J :%DI!/C98@CE@#T=BLBCZA-6@JB7BI6F"%A!2QA#I!BDJAD[RC%6*@BO8LD";KA0;Q_>  A+#H$D+/U5I)<@0"_\B*]BYI:,*0'':++VDJB4;10FTW,!''RBI<Y*$+AP+=REWXB+^2"!5!PI52YOB?[ F6E1C6! D4H!@^0B(2" Y''9+4U@CCPKF6W8F]DKA)B#!D3;A@''+IC"P#@ZBFBN  2''1 IEJ@142J?08R@QR.H"949]G&3Q_UTK]6,]EL0=98X=>B[ M/9IK>SQPN)PHI+)L*ZQ")LD+N4IP^K/$^I#3N9%H(++0N0P:HP@L>C37@(AE*@N2\PAO2#0 ,A!F2#_*X AG60@]"HAP&K>-J4NVT,"V;"^I0#"&]T).*RY-@A@0NXPXTH)3X$[_H*\JLIRQSKD_DT"N(*5P>#A''Q\''[.JY>^S)Q";;!:12DD".* LIF0PAHZHPL(@AQ@HPO"RO0 PQH4HQD( ADPSPLBPP%T*!DRXQO&H@YFY!D0 @HR0P$$ \YRPO4(D0LBPQLV@P0F8 X0@APVHQRZ "BTH@Z^HAA<<V\.P_<A''.B=''*@QM&XS[(AMPN4I9@,MQ*)$OND5M2DQ7&,SEN@R:#I''$EO;P@\!U"<L3JW(#CG*^$=Y''HW65B(-,UNF9 )W<D5)?&$#_B#86LU'';%HC2,8ZG0XREHAVBIL2''4@CEDA:L$WVLH@AB&D<]5D L DCE$DI#)OQAKN=L$@S@"DFL@@Q+@TR!.(IE*DQ&@=7# 4/O8DB8@,M9ON*1@$DY42+ULP@NQPR!$\A38&_/!N/\DR9(,X$_J\B)^,9NDHZ#1D(T(^TOCAW F.C2&XD1Z-CN$S$/L@BFFAB5DLCKH@NZ B0JHDQ&D@4JVEAB DL9 @TJLDIL&DNJ@BLLHC?I$V$0YZ09$0P&;)4BK.)JT,D0.YS$YR(''FJD]6ITKEM'']W)C!53''@K 0N>5TRLC01KCF4-AIH8HCN6@LU;)KU1PHB4RNBY! SVSLTR;!A!S X,YHBW+&D.HF@PJ!A+2 #A;5_F[CTJ]*SUH EB:A1 )!OYJHM )AUM>-M[@$L1;UMUH@^4H!XTX5"P)!S>X@R;ADBZZ***).*68MH%M$,UQ/H\SPH*$01YC5H9JQ!^;4V^W*V5B-=5BH@=E%S&@MIR>!88134PXIMNHF?^3G(,PFM\/(Q#]1U[\5EG +7%1#J69@*APSD,*(LDKMD0X2AY2@BK+@YN3BJ.''KV''Y&?5U;"6A5TP@K%(L&<)G";C]H<!"1LBNJ@9NT >AR0AK:MJ?R*Q!K4.G*L/%&X9T$+.TH[C7<9&BRT#;&0@''\:0$PXS42(QD48UGCMQB\(AFX@@0BXQL>H]E Z\B$JV =YF"#$&"'',$-!;#14##P>-";I"PUH1;*$<9L4])6>$4^PC%*5%"[.2^$0+@E7RB:Y84* \M40Y+H&"63P5''MP"R@,B!K"K!K@H&5?A&5)#CW(%$MI[?/Z=&3S]+K@XH4>4>00HFV\CXO,]&@/:K\X,-6,9XNNBP.\4:8RC*<>*S/_$&/P*/Z(YP"^Z6-AE2Z6L0F,"45;BO"@*C-6R2$>5/?Q5/YLT>M,6P0A.^<?]6U-7YYEMHDFENDPA(\A^J$QS)OF=#Z+-N<@D9ZCW.M8O5Q("HX70''KU(BQJH#AEMZ%E]^!F*+V:S,!F..Y+.B.U<EXD0>-''X>9CQ S@.@9=D>_@5I\=["A>/;QK:Q_&GJQC/OQ>8W]?99\&''YA3R@=C.1IFTB3_JG\,!^9I#.8,P;^AV0HL\>^@W5Q;]^QO4WA8@''@[J\-]E7I8E<$.EB@RL''U8[O].LP!/6\4!T6Q E7H>7UX FL@N;FA@PDDGJNG?%#][45L@M3QXNSQX:B^,L''BH!Z9MO[]YN<7PCLRA&U %CDEZ(YDX\\RGI.CU5,7?Y0"P 5>X]Q\RWWM+F;5+$QPI\_5EHF9 S21@NC^#C578!Y.",''3X\^MMP3<TN^-MM82FIF\4VSMLH3MIISZ)8G1#78(8''T+IX>TMOE"IIV-N&"S$&,P5WF$2JRV''ALLU83CDZCUYJ#6DP(RV$8F6YB''.&;J2.0 0]J@6Q#"V:A10:CZVH=8)''))8%$5"K_T)IH$8K''5''(L#V=C)4P>F-W^,XMRQA@1 @@ND-$RL2&Y^Y#"=HBPJADXR3B8!3UIVWE3<4 1(WYK_2HH*UHU"9CE?OI4KIQD''IN%E";CZA%&]9]L/*.F@O=,H3]YFS$G@&^X\YI_MYCSU(^_TYMV9@?1L>0[6@2#QA=()O+3VQ%8VMM2H/JK"^]7HY\I2+4@EQ%K$@.TTM[''\<(%08,D[1\5\R G1ILC5LV$,->V\E#D2Y<''INN*UQ>*U%."%''6)IY>)''P87=9K(UYX8CWMA(5TMW8VMNV0LO*%I67M$?'';F-WSI2MDUA#CJDO:]XJ,J)[08 JHQ^-6*=::2G_#Z-/]8L8$P$T @''<L8@[KS$CTE K8"BDY]JH"8_$6*%W[MN\5RQ\*@/P@J&Y>MR"^J:#!B#R!Z?XAT49&MGTLH8M]*7W^)F@EU7G[5ND2#Q3AX0[T(/+^O#D./PH%(L''D(>OY%$)%9C=6BPPX@$<$)ODQ_?C?J%OZ^Q;0^X$,= 5UKJUV.;@\C.7\9.[]K.7]9-1_C.8!W.8"[.8J\]DI AE^M#''-F@J1SJAC2;*$ ,SWH^A>S)4.9YI%"Z:>[ #]-$G^%%1V9NK@MM[_XT<1^?!4K-,U5"!^;"=6QF>IP179S.-;S.K%YL T$<!IA\,''Y)DG[BH4R(Q++MIO@F5+3-4Q=\XK87(R@''8BA.7+$*XDUZN*9*=59.13_Y6"]^ENY._)1)9A=T!#S\^LQQDORDQ@N@S<!!3M7ZPP>&"I5CT(D.#L3[CB/$:/FX;[GP[B^I)Q!C%0''WH"[3H#_3H$S3I%W3I&[3I''Y2T@S"KI>0R2*''?/>&Z()/:XT5GB?G *AO\S"F8 J$P17E\K%<,^L+U,\&FT@.I3TA<]+>Z\VMW&=.\3.NQ3/F<3(^O,/J<3?W\3#'';N=1ZB3H @\QP5Z1CIDV2''B/-"E_H2;<\]F<H3T''2 G5#''(M(*.5;.S$=(]\1XV^7''#.U3T><''-^1U6IM3)L3$S+U$[)-\$%(FA^=Z_0-$3 ''-PW9VNIY.%&Y@;M#.48R@\N#AGQ@@5AN&PX''6YU=6Y&=6Y7=6ZD=6*U=6*'']6UG.&9@,''A#;ZY5;-M6RKJO[.8_@KBD=4-GR%-/2''WF9=TBB%3>07U[W[S4X5.R]">/=7+''X\>"]TO&=7?_]?=<AO.@E_.@I_".WN?T$3\J<4=H''F-0Q[''LK7M0UWO]H5;>GNC0#GK87WZ49GY#%&5\<7+4?W.SS6.L?O[;_&;E;N,VIY E97_\,52M$WAP.(])*?@H7&(KID -))@M58@B8299''P0R[HADZ0S05H^&U_.&Y/.&]?.&!O.*%_.*)/.*%/''C(I<H<W *-WJ1 MM1Q91!S"K+) D8%G%*U.+ 8]-Y?A;0YPLX2F6=3.NC)/.;-?.;1O.?;7Z@R6Y,UP)V9>;<AN3#,.-3O7$:]D]V$-:N)AGUO\>!5KOH%?:X&''<(&??H+O?H-''?I5[OL7W?H=O?MAW?MI/?MEG?MC7??4T[?4NY?J)E2CO'',71I1Y$+V"&ZZ4M^$#U5-Y0_8''XO,C67U<FZ@I@D@K"NC8"Z@D$O?8%W?9''_?9(S?:)W?:*S?9*??:,_?9=0@@@F@S)D^Y_Q+VK7\:G[2UDQ5++_/0,SN;'':942[<7])$NX"2> ;< I@DQ3MLI=E<S\+K?=5??@\JIPB^ZB@8,NCB!0HHHCSI4BOG!0( TG5*,NOF"Q(2ZMCVZT< SE$"^HFDY"]IPHTUZ''&@QMVSIDD01X=JT.XS&SY45L]6\>]M''SUDL-BP*IB*)4*UL&3)=B#V*5J%T*5*="#W*D"];F"W JUM(3)$=^8)BPF''OH$ ISI;?KLQ$3)1@\.''R''ZO +-2<\//:?[/7K%><  ,CG#08,E?D_QG+IP1XL^SI"1,[3''.(DD*S''DVNSN@)$Y .%<K85H$ZZD>0+F^FVXJH$(:''P1SP(PA)BM& OEM#B!,VJD5QSOC(XNAIIF\,IT>RMJP T $"I_XP,T;]N/X=5Z=O9:9]>7W/8+-[?1;>./#57\6O]:>=?_''98=''S%9<=O70"]AB%DE%R\<5!XX RMV"1"B^&>[[Z[$FQEU1XN(W5V 14:JADU!).2FFGG''9H62IM^@FIJDJIU]Y/**T&8QCD4]FUH\(9M:AI''!SB%Q!Z>LE##3;>BFRPP 9IYIEFG(''$?9EZZBFFC7LX8%2T3"6A A]>LJK !B-.^^JJ0WWY)RV"MJHEJA$.MQRHZ*;IY),Z[+TG\+*]>NVJV)9E"Q<XLG^RI0''DP@$^QN! ''GFDG%(((((&2.""##XJ:ZNRQ$+))IYV2" ^^=CP"E/L.[T\%E6H$T!)CY;XV6<N*&[ID&!PP Q-!? @R&96P, "!J/R9NH!!BK"I4*[5W IFD0\\,"12BJ+;KKJM/,,,=H&.2255TY+K[SSX#----->B2:68(J[RRYJ[AX%RRXY\$$MKFB 8J4(#.U[/[/NYL$PL_"@19%.? -00F-Z8$PS"*P@$5#6<,X SVGL6 PC(MFH[/>_BFP Q!L>QEHC13U<7GGHHWO,<\ _$6122RJC3GKKKJO,\,023;023SQGX(D]@CRA!(0T*4- HU9(0T BB:+VX-K2H,64TDQ)PPERR 51RBJHBH155%)S-XP&WRV06:8;[W&VC46,5QY3!# A0L98B@(7DWGOKW_]],]]M=94845D77''[;S___O-M>M>F;4488(TWO+^ [V<R4$" 3)"\ZE4(XY*.O2V=T:84-Q+[[D;U]).-V(K9)[7A"^K+\ZCYBJ*T''!"RT>4>9_S[;[$/0S//.>_>.>>866;;;,LG77//0Q/?V?KNB6?<<=H?7733@LX.KHDFH) %[:\J9>W?+S#M-HQ)E&J8]_+*[36D"E:<AE[8*F+^8(-DMNK3\+C7FT(F_-SP"D@DH XCKB@AX7C@@RKP @(48@HO&L@GL#BBD60 AR=XPP1BLHL\)N@\FFD]QDB)X&9Y0$*HE*<G*Z!!7%,!%387)#K9R1QKX@@^''+B>GN*0SV.K$2_DQ+:25@<-Y6OKY02A@PC HPNMXD!GBNIDJDID@4:,($T:D,T''X''FKU>P"EKG8QR9:DX1U#L X)R#FMF;1BS8PP0XP0J]OM\\$(N$B@D"S._'')$T.)0<Q+W!T+TVCAQUN[UZ7&ECY:]\8&#ER$"8*CA>Q8)#OBH$''6,H]ID(9$Q)7I?6P%4<WI]JGK$?([9R!#U<)T&+JT&90\J-''%K''"E V%JP9D-M\\ R6@"DCSP ]Q6BL1 Y(U SY!A"Q!9R<:E;R]C^A"LINZYS6;RD1_3@2"XDH),! HL6^P&FK29SWA6<9/[CJ\6/0%N\&+3''N-,)3_IJT96%+N[=I3''OK%)37K"L9?;5BXBDJ@APX%0$ G"#D*F=(P,K\6VX6,(4,X&$:\]YR&VP@@CL"GL#F)TJ''C2B"KA-E@WDQDC[KDQD,U@"T@T(!@H<F\(V +S%<)4''SE-*T1#^-N\8+R&N,5)M''GZT9;25J]BCZ)Q[_+OP!2B@'';H FX4NQH(VZ84B$L&QO>[1#SP2RX)"9 !:W@C1OGE[:05:YTNWM\V@[%2VJRT4$$>=U[,(ZQFTZJ+%O[75+3B]Z:^4*-_<3* /0)6,GAE)Q3W-[4D/PZHKFQ,"8SR*$A\ZHX[+^1F63^"X3XV)J^9EWE(\C<YKX^.2^&_G;10"F=JP+U VB4:W_/Z5*IS-+NEKV5YJ5/W:!Z7--7-Z'']KV>@FE;^99R5+_X-NAF!B(EB2*4%LZBT,3WJEK^RR?L JP3JY*R%!,J17J;.6>74%"LBQ58''0]KX83&8S@J@DF!A06>GJ];[D''R=2[?-[80*7/,ME+''A;^=0@=5_@,%T@JI+:UMKZ2AM6/M14MR_?1JZ]C%>. %URZ''@CMA$2A_%*:LH>SA["/D6R()R+W#4CHNX@JC$,9%M2WI3"FLOX1R*VLX1Y7FHY+1#GON[1#W,L9BCGLZ>VWD9J6,V=OJ).$\(D*XSLM=''/R#&X1C1X0%*82E-6:VD>"MODQ#$ Z&[@XJ''=[W8%\VX3 >DF+T73&-TL93^/.\52/ F\7\3&N<,Y3V=^+Y7)?N\<2?[O_L:3''>$\B"_( @ LV@ITHZDR%+#DQK",2U!0L+<H V=>S8/Z%C>=4X:B[UVZ7%2HDV@6$*Y-]$''4@RHPDF]B1;''P_J85'']6,9$@W-=B75#N-943(NQ>:5/''UMZ=?3^Y ???9&7L8,EM''5D+''RGT4)***G+6W.)"<I''QINTPB4FP[7N 2$_H*;0++53)(_''JN442WJ#$C"W#+[=;3O.T)727/_L];%_+.-;878>>@B73_IFF7WP-4HE%JNDWK+MM#-QSYJHM:8%+C;O-L%N7GA!D5]W)Q''DQ[5<%90+R(OWN^CR5,W@?;-2!G>\)SW%2W);''X0*;920U-9)[''E._9UR93(P44KE@)."&,YZ)6$67TAUEL6*T,1Y>.MS#I:WR>^[!HJYE^32P@"^5E03ZI/^/\(%''M/T;96LU.<3[O^.T.S3[Z_U5+%;>=-S"?-]5WN0\+N]VR4TYI@+@0*$Q$RO=U]G*! 5+TJ$CJ"%Z&X9AX2T5VD^LAV@R-6D&T\@!MK@HCFM#<H#Z1^]A#POR\K;7(O;<H37_>=JI''_^ 7[?+T'';;3-E\=:$^_^-*WW/V55?7,[]=;8LM>=97W@AONE^4"D:!]KEB,94B<,D[2IE<1:JWS(X;=C1GS&@$S''7TYR!X.18" \!79&AUQ9+SC?\;E1''V]3816N;\_9&[V,?17[_?837?X(U .DTPXU:E4TDQ#ML=7J"+DH(PWX$W!Z]'''' @GSMU>3S@?%_W 2@Z)&T!#PIE87ZFT''B^^D_;X& *?5Z6CW\"UX\30GZHBVZ2.H[G-V]#+GY6CP[D70Q#S?L#%QEQ+T-"B^X6$KU2]ZIS)M<UV6X&8OY7W3X5XG(F9FM#$I$@& 0BQ>,BQURHUT.BMY^HUYVHU].BQX>HU!ZHU#BHY\:HU^.HU''VHY%FHY,^HY/6HU)NHV_@@X/9"%@ 7AZHD,.5E&EQ54=XV8)  $Q]0CW=8BGZATVU2H>ZD-F)1(^10@#EGR1$0@$)0B8I''O8QWL#BG[0M7X,58F\BG?8E8J7]''I"98$*A0X>16!KPE^ X$JJ\BTJ<''! X&*Y1$]@HQMLI4ML$R?]!X#A"AU+D3D?9B@OT&+CPTQ(\4S,QP&/Y''I-E$87$@%6=''W9]U1+E7Z NF.$6H$-^F06?1^M;'']6$*BB\YY7BLX''!&UDCXX9RL]XQ5\/_6Q!(;M!GUY)V.Y!#BQ9IMY7_I\@!6@A@D@D"*@HE''BPA6$AB:&PB>&PC)&PBB&QB!&QD#&QCO&PE7&QDU&QFM&QE\&QC1&RH;&QE,&PW(@G@@@JM1AM$;!<;*IX)HYURX]%R@M%:BNLNT$UUXX0CWI)L=$%X=EL!4@G8/U7+"P 9'']Z!9A+''!! ."VB7U"B&:!6>.^T=S]7U1%?LW!(0YY("0Z@4+P.P-LRK>E=5&Y*=KI)D=!)/2PJ%*@D P@FN$&WV.D5G-UYR\  YUMD*<Y530!+]?]M&Q@CF.@DF(@F&ZAZT_=9]7OA%G]G#/[W%LA&];IVY3A( ";8]'':F#\6&#)V$GBHAF%NUQ?G($1R8ADIHF>DFB^N6Q:%SI0="U*;#W@JR@D0PBVI@@X\0E0K$&;<I''LD)''LMI''LU)''LSY&0JT''L_I''LH9A8_PAZ_E$*D$KG&8!;&8LA+''!=N7AIJE$7TI''&#"O/C3__K8OP;CAL<D\&KIGO5CY$?YZ1=(KCF@A$3 SYGYS]58#+JU''?07''3FPV%=7''7BVBT0@]3,''&BOH" NEU90AWR!$H ZX#H[7VLGA"=.%EKA!A3 T''!4*B.G%EXQW(Y85T.+E]S;@ ^ZXV:D BP3 ASQ A7Z @3R B@3?8E(7TI"A\F^!0@RI(@"Z0GNL>X$1FH)/I7^""H)4%9VYNXH4ZBV_<EREUU^@Q2*WPF%?FH"7\2<:PX>AQHRFYC*(ZXCW=$!/ 0 S$8N]DYBQPCP,UT?>5D<45T?"AE<?!T=5>%I1&*_.)D?7!J]="$?"ED=;.$;)AJ!/>%MJ,@''V1II1!XL/&VSVE%ZP!82+\YNF:JGA.G6Z!Y+H-IXJX0%!$I;"AWPT013U9@VW:HF7MP^[4@VI$@!],@%2*WYXJP&X$X*6^''X7LA]2)0BK4@TTHJ23J $H0@Q($E+EAP[@^ #W.F]+]:A_.P\=D8B1D6%%*VRFY8,>6Q)#H5E.?5$[(L@@&Q*^(/YQ6AU!:HT6F]!5@%*NZA@I_$@GE @J"$@G_$@I#R@I!S@G'':@HF%A[7FR,WV@APB* ;=R$A7. 8V!(>7]3)B"CK,"Y3@XJM( @$?ROL&H9&JL*Z#&&U#TD*"$J![@D8EX:\=H06&)./LH:Y>VOPOND.N%F<CU V.&T68"3M#- N[.36J"32'')_.5THS2B]]''!XI6F]<]IGB:]GML$*7R%19D*W"!"!D[Z--QQ>:_%1_[IVM$I2#7&JXA\CEJ@CO$@A(D@I] @JL]A[+(T@M8@A''6B#9!QN-;U,,MZ*&8AM?G$HS4@GM@@J(N@CN$@A\7@I"A@I#_=PBO/TT!$PBP*0.G_J(+Q%T;W5\*.5(O''CU2\1]KL8WR5$]U<R&?I8(YP5AF@#-WV9E\TH#6SJZV#!A8,PQ5.WQH@IY0"0UDQ @S+ZV(D "8=;BX5@A3: BX.))IH0PH>9V"1Z#U@)''=R(V>\X-*/J&D\Z]4.J\7VG]7*''FXF5O7?WXMWVLM*9VX%W#1P0%8UDJ11VZS>)%*W6"ICT!G_5U)8 ,6<4.\\V&XO&Z>?''/1M+&TJ:_9B9)O1K0O(+Y6@0-DT+R),AR<7WOU E_(8D_W:DB^^C*Z&[_UV&V_/XYD/CN$W)EY2D,R*F* +@V(J6[D0@B''[@B@CJACG0BS;?8@VADI!*1*N TKA,5&[AE)T,2 B $@"G\LLVQP]^0@CID N  @]NT@ ZP@R[@E>_^J.A$K5QVX:].F_=I2"QFHC(L(AE$:4NIUZNI:D,28A.*\G%^)] P7 ^3AO(!XE*(8D(:*:XF0 5\DLJ?D= (@D>P@\:" D1> %3\@NX PZZ @"YL@\10@P7DA\J$@&APA@1HF!+) BM8@QI_@"N/J-EZ(I]6YTR*78OZ6.VRXONE*T.BV$;"D\INEYAB[H9,V5[IP)/YKK"5!.#Z9)BLY/@$#Z ZRN7ZPE"0@#!)JIR6V/RV9T*_LB[:G[IGF-J^((2^JSQ[K6=! AD.9H/?6YW6)M0B,VG_N!0Y6%*!I#AZ?14E/\R/C2>F=\"("+B&*-IR%%2?W,C&.@CE B9LLWB^K@HB-@QB!@JM8@HAHDAMN@GE*@AS[PHEA@IE@B$XA@CS<BA&^@D"1@HEK@GN-@E\2A[!^@5J7$I%:@DZI@A"L@@%B@FN.@D \@HF_@IW*@AF+@H\ZDIFM@EGILAA@4F_[.P''<@HS)A>^,ZJOQMJ,ANKJCQ]6)&DS''6^HQ-CFJ+N8]%C\''I8%A(6:&)D[FDH3.!57H @\6@A^0@J\?AOKVWP#WB,$]@D_*@C#G@H&>@E"PAZO=(E8)(IZO@D"^@EM_B7]ECH1Y(I"5@CMO<0/AR BH''+0?$'')MP[)L];]=&</^)XTH<*D(EGUQN8Q:>9F56ZED)P,.);RE/"RD4FT\L!/4Z;R\N2)#O+#[+*#YUI/^B(#[Y=0JR,0(=-2''N6"(/*AX4:KI.1A@XB0P?6,QP,_TBTK6 P-UW= M/W$;!XU[DYM$O9SE>6U"[,O:&Z#WJF.8%P@1!0@7U[BH- A1'' AAS AX"@^Y- @Q&@@SAB481P@9Q @W>].IJ @SR0BH/KA@^IA%40JA?=]P P@1X@.H/@@H[LY".-AWZ @X!@JO"-@_X:A00PB_-,@W>;BS]0BOY]@0^IA3TP@21X\=HZ%)<4% "UP+ "$0;?5X^&''Q+_B-7%2!U8BW9AV^L#UUI4C@B.M,VCE *E+PMMD@$Y(L"W*@QJPMF \A458@P,K@Y88@L54@U?#@\X$@&M@B-6(@!]8@U;\DNY4G<4 @\V<@&BB0AZ<NGFN9W"RG<S"6\PJ8*#:I''O1)8DD!)7=H:]_UTT''A("R8R,^Z9["&5,&V8S\=$12:Z_LNQ(],&09U=5C&^T;XF=?X40I8N8''X''IWGXKC@*Y<C+U"+RIE\EZR''V-"55?NH!P>947''''5T6;+ZB&J"B%(#GF6$=[W 6E*!(@A^,M=*A5BTHJ. 8@U(@A]OD@&[(@BID@$W%]BQD@NE4@ 5P@D18@P4?8@A!Q@J@,''OZ@@JW]B,?I$ICJ@H^F@G$]@EF"4IR*@A^4ARLS@(QB4IW&@G\8@H_=,HBH@HA($H#PB8A,(@LX(F<L%''Q:69]@U]''-/YS!/K9;&KT97N,(9=T&^L< #U&LBW,(-"_9&"])\IS6@G[44D-NK.1\(@O$B<!WC/@D@GM/+SMZ@CM570S\@H&U@HM?@I^6@A\<@DX<8H9W4C&;@GS\C%-C5,1:.B4U2U(I;E(X#[_IZN^8^1MPJ>5HY1F8^%),75EJY8=,!8JB.&CD^&YS)9ZD*?J''Z?KXV$9_#]T_?I;%^Y%?.,A(3)5^-> XY''OT^40T5>:UH [J)06?=Z$7))-Y]*<P;H0U\6*]W-TIB(7ZY4*-6-/FVG@@)0$H5P7+JE@M]>;L&>;IF@@UU#@X!P=L5.K +0BYF0BI)@A=7>48-=BA/=BT0@Z;-:B@30AJA@A6&NBI\@1_N^9$Q\ISPPBH4PBU9 .@*0493''7215;3Q08/6+8%B"_JA2+P''5.^\I14^W!B:R1$01AI806(,O]Q!?:@:7%3;.%7VLBL:ZW0T]@7S-@4S A7-P@4B*AKEABQ(@DD($JV(B:%B!P$0":U!4 0DMOF JHYC$1@8%AP3(>IBHHIPBB7,67P@CQ)H$LC]P''#R9L*WI%#EY*''3)\&YL%SYQ4-2I,.S?GEAML"C@  VRI4!F$Q[59B$Q B9J0 3AQMW*D*-ULUU]()W+UZ)!%"B")DOTV[Q''!2" P0GRDKAX/6:%R!^SWK1VQR''P\XAA4:QF"18M''DB!&JD5V6X:)LC1X<]3EFQZ:]M&2\(20XPJAYN%SYJ_SXXNWSM3:I48O[]\V[+42]P.B371@2)S *VA@6L1]H&R%$V^0-CMZ?^+WJ+E-]YMO,SR$$A4]B!IV=7:]^3Y-V?''7-7;]?C!,P=Y5LPKE%E7,19''/%3.<*)!&M@!4,#PT,G9&X[J8L^K@,=PL2&TP1J)(ZEPVB)DD1(>\PHTK2Q"X)MHLB 0D X26\PKCSIA?8@ICF) QAL_E"$DCBXT,^@PMDC)8)CN]#($DD$>#HDATF#HHALM"L@@D#RH(DBRTA@@1X9@FK@ $TNT&NLSB1!!!AIMDOB(B1="VD4R@QGPA@<"DK%O*ZS0V0HAK?1 QC#8"FLNNS#_%EN=*"0Q)QD-PJDNKT0T<HHA<PHU]EABB3TTN4L46ZNQAKIB3+#&%!LE@T(&V@PKS8)J0IAM@J@D#\9^P0&AP$J9XP9DMO&DC"4>KXPAR''2(T!H/<FBDBR,OVV"QSA+1 XXXR+7A(!+&0L@NQQR *JQDQL(DI%EG$>$4ZCT;R\CLX(/M,</&RCNCP#CM+<2$CLF"B3F:.O=$.OZ<* .>H]9+U204JBE"/DM<@NV-]-.[L:-8@QZEBS14X@B70_AK:* DC+D@L\828=FKF"JI1@,/UH2$A"<Z8P&62$B^E(1LTC6D69Q\P.62&UR6U*^Q(?7XMI%&<>>F)((Z5: %%J!AB03X=E_^+/*E%>#%KA$"A!*&N?Q)*JNVV%AK''F!"!!QD.\)]M8.6:45LK@''#DC+6NC!SL!L>2!LD>/O"$L-JL=TIG1Q!X"H+IT$D#4TP SBFECI9((YMEJ@ D#Q.6MBIF29! !GBKU+$D PT"FSEE'']\="P''?$QHBT!"(DRQNW*<M@X<PK&!2BO''TCHQAY2D\)L''ZGC?8!H%P FEC#P061Z%TI0@D9D%]AZLLM8J<THKQ!I 547(7^6*3_TB9*(*TQ#PH)EB4!*K!"^&E''=<<*U^0 &3OXFK>.&-/7:HRR%)8,\D2#PD@0@8"#G%%@KQA@7 VP$LZO@A<2RABA?H*!@7THPN[%T$!SC$A(6 @>>D-P ZEF,S2I+C"S[S!RZL!CV*4Q[?V$[B[]ED!ZK9CKUX@ Z >@E\Z!.LXI*R"GR-"3#/"9:?]!"/99CEKJIH I7P,)Z6<H-+QE.O/B@%"$O(@@>H2M''BZ* T04PBLP#0WT$N PG)RMDO@O@CG )FA2\<:8VX T%IU%NRTL0!@59@AA]??>_B$F6)MR!DFY^81[HR" XA-KDM[!ZFEO349&_A&P*$(F\4(<6%K,>I3 G6UC9LY%JS62FO^R@!B"[N!W*O#@<SZEB_LU$1M?-1F8B2IZAC9F S S#DH^[0!K(EP DF4(P$4J@HF%#HPD881BX(PPD@M,HB(DBCD8"P"A,\0 $4"LP!X(@1K[$D@7"H$@JXTD0_]HEG^?!DHXK$AP>E0 ,:BDP#H$DAAQQ"C%6( RX4PHMD3NDPCOBAGW(W2EE=JT37,^J8%%@HQSAONC:D#1N+E4*.6P%O^$*KIR81A0Q,T*L[7^S94*\U!89RK9Q*0"H $PB#[@)?%J!#-,@ OO<KJJAH''B''RI?3P!Q.P1P\ZTLHMZ/TDI!QIU03I!DZ@9YE!6ZEXF+!HHD(U"$0(0%%,YJEKT?"2O6:IY- "B[Y &JY2"(-\M\PM.!H!EXCEBZRQ$*QV:FV/,3"!^4_L5;;VI<''7RN>!$"JX0S2A4*L(Q[AMN\1PD(PSI,1A''6"0@AFX*PCEL,D''H<1#B76" U 5X''N'',R)@E]LSZKDPY] *X[PP,H&[(^6JB3ND35!0*TZ:K59\&>5,7?\(:P4''A-J9ID]=>=.''UZ4I"/!$=RC%MUG:R39%N=-*Z="T-/$G[''5<"T''P8@T;VB@C\<0.H$:2BQ4(@ NI(HD]E''FHI=C?@QPL^HH];I@HC@A3DZE@P0T3<@R)^(DIB&# +V@R"$B@P ]4:@H#JD@CG3QBBX7@@><X @\J)CMGRUHD;IPPBB!)8A@Y(HLE.!@IO=@!DE7\U/A4P@PF&N-8MXSDPY_7AC[14KY 6X);QO)C>F&O@''MER1#DA%0 A5%PG&WT>-(%81)SIW;3N:''=U-+RZ(TBDWY(@"OTZAK>!!@LBJSC+F+50@?-**!4NJ*)LD I2]R@BH,(2P7.N[>Y$YZ26R(-BT_&4&"99J-BBQ^K16T.GJ*+$S2$[P<!E\R2''LTB$$ KD-5"YJ8%52.OB!#<(("G,>%&Y5#H(! X,[?R LEJ?9^PQA_,$@EIWFH"\BPP@93@ JA"FQFAVA5J&A@HM@S"BS*0@2LT@COW6M&O09ZI3N;<F-KP+KI ,E$!DZ'')(+P6D,BI<[?(]CPZ+?U+%NR-$K7=[T:V933)ZT=H,<5V>L6''O #S#\H01Q???I*O''8''AD1IA@P($8 ''^KT$ L BJQCC"BT= 0J$24@TMYNA""T!DE22D@A8%@!R_VDPFQ"JIQW !@7OX''2PB PDHPP#!G%H@H;30/2880R\U''=D&MAETI& B@8 @01\K#HG2?+NO@\WCG-@0)*W I7''KV=O3! [I+?VP4!KMT6?A?WR(V<\P3"02[R''=2D''9(JS"0/?T?_HG*"4!XLM;6HM8MZFI0^6AD$:-E1F<DHN(D GL!: AG#AP5I1_4@8>0N S"J@#I4ANCG.8^9-GR<I$U9V:H!-6HNW,$1#N4M5$P&$W(DJ]-&(;<9FR27OZ_)Y@(H^.>%I"IL?-QK8V;B>YZ''^;F6XA@BR&!Y;9D@W,<H%X@8<IFZ?XL#UPH3QPPADL6H3"/KBIQQ@$1A"PKL1DL3M@&!@''J$.MU85=D9XLL+U(N>SB^/X[Q))^%IF6-/-46>6()5?H0)5A\Q>I>*GAQ62G HCYDCX.([OM[R\;/I8S5L9F(CT;L $&PHP@3@P&$B0X" D9<(IEXH@@''B2O40C?QK"A?TJISH AMC"Y5D PAV DB(PQ3FBBWD.,P0"M1K*AF5B@D42IV#*USVB@P= /R*@A#@J-&3B1 S*^UV(1!OJCI4BO6,JJ=)F^8; 1<,,^K] 1=V-B*E,BQVDTH2R?()FKIZL_I1NCS8$Q6H@IA\ @"D@0L-LAB? =,\,@GR@''A\BRS\ DBOHBV\$DQM"XPNB"F=@DR( )16DDG:BC2;F@''&,H*,IAXW,I@S%D5W M:^NIZ.''B+9J!O!N,0B(W)<"!8S".[ML+^LDD,QB"<VBKQ=OD!&H(]HL"JV*.SR.SKMJBS1#@6Z.QSA@07C,,K%$DJZ(0QRBBAINDP:@@?3=HE$%(AO]2@ 4 "BZ @>Z#*,PC#VJSE)6HO*::L>][#V[C&UWRC4TB&&);C?!3()AZH.^@C/Q3P''K\)D;2 $=J.$''#OA="C/$8)TYX *@+*JBC-;^1J-QPQFZTPJ(R-Q-8 )M[''PC)1<00OMFR0A\:2I#PC@Q8BC)"@L!I!F\)QB<Y''/.C-*ESJGHSQ^.Q+WVD''+1 N((*Q9K<+Z%+ +>@MH=TJ>0!*T-A&47)EO5Y#]H(*#''ZFEC@ C''@#EM9 "#90D6(0L7HAB_@ A!X0TV0DL38NB^P+D380B_8A@38IRK()\U3Q&&4E&SSH:0$-&.1E! BAT#D%N;K#401J?>44+Y>>TZ6^*-;.X8!*J,TTA(YL3^/:\"*RC_U*2JEX[6?4)T-(,&X B+[^9E%@PM_0XL6#JXL,HMH\B)DTCMFPHN:61D0VHPJ\(H;Z4SQX$P:T;X:J27QGBE&(054@*2DNY;VV*RER+*=(+GS0:/3\9*R+D71DR9E2A+#6$R<]JS9(0?;<KN4 Z9V&$Y=]BN-8,_OLAUM2H@L:[ 6D$2-B+T.($:T&\8A6S4/6@!J^HJ]7K''?4(P4IA;<N9:CR*  _I9BH3SY0"6@$QP]8;D^,47:MI<([AR/\H?]GJ%J0\J.^;KM^RLB7J<YIH$6. GFX@HDU\GOVMA+(P1C3@S?%UB@Q]"DNTCPSJ"WJZHH90N6373E.C&!3-**2_JV,VR=5_*+R!R4SVQO=-EOQA,":7@4RKBDE$T7]^0WT<P4]*N!0"",D?''PTF@BB-@A7NN"3/"DO[B@N.RL^-$C'')HD),HCG= CB(@QAF DR&@)C?IJOLJ*A!5H1_-L_PR6%-@>K;AF,$0[7+"D''0$Z(;,6-''P$R]LJ[+N$>,1SP3%G=HB(RI(K^U&.QT$%''R&(]6,%L_5PQV5F&6 LAPUODL7J 50YQ/T*CDPCSWBBQ''@%3QBP8A&^L^GA0C@DMEFS5&2+*4OBQ<$+9;"S)*.("0(EOY5UPY&:][-QN(F?ISL)?<ARJT<AN<Y3"RK1BE]$"Y%B3!GZ''8;3"\;(QS3(@%)BAB?X 4"PJT$E44V5KJ8\49]A"\ K%<$#CIPJ-KSL389D/UR%B,>#AC3@E=J;*?J+0!_U";9ZO^]J&M^C&@PAKY*@.LJDDRHA@0(P >1RAHN5 BX@ D6($Q- !CG:EB8B@05HH@[ 4MGB,=G,*$;]*.''KR!VR/-J,CY1I3WY[3]^JLV2[U3+]S:V)K=*$UY#5C/Y3/=*:KT5LJ;"@(")EQ^S!P_6[ O=(1H8UTQ0,V.%[U!L26###KO8IM.3#R)+XCB/1BL8*2E@PS9<3E6#3O.U!G.\!O2*\$1/+RBW$G/?/XP@G"%&59P8"Z1R1_T=I.,H&>4<1<@D((;OJ4+NB-@06 %RUHQD?=HIH(@EE<I",,"2+:-.=W\"C3EZ-@(M@>IYHI@2EJR.(4JF7?Y(*WL.0F@,5N8,_(:,$ #S7]L6-(CR,,KQSYK^>ULWB"- 2!R)Y1IWMB@TJ@H@=8CT]8CT:*HDN(X!-D LK"@FO@@LRR[A"5]!@T#Y%^;3+Z=;O !$4/X50MX),!ENO#CSTOU\9PP9.^=&5C]?*NL]4OC_MWP?2V:95P=F1,"E;''J8<B#[9E]+_>R2!UU9C+I[9]Q%.CUN/1D:'']P$DDA8TT;E9GH33UJ!FT ;+HTH_\.C?%Z4SU17I,<BDPH DCQA_C_X^QT''IF<,K^\&:+TL[S?B:&T1\%&DM1@TYE''[\CO6D#UDDQ! 1D-/LEV9!EP8YG\;VG_YB,YP<#D0J7@ 43IT4!?KF=^!D]#6KF4"@Q)MK^KWY]<''\5OL+;)MDKMHUU+P#OJ,=N7"QAN$L,T0DMO"&19"C"[3 BR@B.U,]K:EXK([T)@6-P''3ZL^W_&MFLZ*0"$.TM56H$F0N[;DV:RI-M)=/ -\UM7T02P;/Y+8D"9+)HF&JJ:@+Z^^M!R\W$RM7$C46Y%"EDSRYS8LEZ<,R?T^5ZFH/S+XG[J]X+H 1$IVSBQ.,JQM; --5M7''1$?9K231I^JVAMS&A.H;8UY,T-9."TA@R\P\*@3!R>CFI>9&GVV6NNY&L6T\$3YZP(J3EH!GW!7OF3-*>0TPI*U5D@ASA(MEB,4]N[D0#VOMW%4POF/4:SOV\LCW<5T%0I6@3X ;]I@RUP  ''9!C%P  I1.6N1''RKAP2;]U$.5UN]S7#5"(\_;4.033YG5,5E5T65T37=AX+/0T?W87$N.YY#%T7HC:V>$-G]D)PM.W4=87>##9D2^ZY&.:T9.Y))67EEN09=3M=50LPT6+%T=.$X6K)D\ZYKV8DQI''2!^RQ6%ED/!.)#4%K--XZ.>Z*3N:*.&JX? P*7>Z+@N:;C>"_<_A-^^-ZH!/%2:IDUC*0,''B*J:N0,L$L>8_E^6[$]R(%_U$6Q(H20-L"3>5S4AR8QX,:9HJH!\$=X=*@D-^PJ5(3WG)(LXB@TF HI''.-@QB.C@3-=WKLR\8D3N1#NX,I$49VO<V45*F;QA3.,8.T,8L^R$C->Y5Q+=YN7L''[>]/T#6UP+=*05\8V+N:F+!C&;"''"''#G.;#K&;$W&;%[.;$G&9"''Z''(''&;''Y&;)G-YB4@AP;VOALHP_CDKZQ+KZ3%FNKI(\61;9#F5[Q!<)QM=U1"/86^V9;^U_KU9$/V?<3&?=7&?>;&???.?M@OC=9 0EJF,VD3)H@KQ4(P8*;-;?<.OD,U!"H(J_H4)''RD,:BI;S/CSE@:B">(''''G76XCN"L3XR*K*BCCC"D0:0HG<BCRD"DA/HA1(%L<=*LP:"UEE?C7K6U:,/#6YN31?M1"%;^"*8M:!6+7K#^Z MG< Y[(;%S)EY/>"S_=GC2%XT?PT61G+2"P96@8Y5AL@?3[1K3V!)3L2=3MB]3MS?3MT=3M&_3LJ<%NY=3N)_3M7_3MK_3SL@@H,!ZO<.ML4&S(*LS=<YD)W,(*3!*JZ?%<4EI=\%\41M!W$4)D0XU6L I+\[4R;>VE-Y4,\Y*S==4K+''*TB=5S!?5S0\YZ0[WEAW"\;''\8WCO8"A$9/@<G9CQ??F-J2VR-H8V9JHY&K6<8$OJ8+<N4*EM"TW8AD6 5I3J A*(@P/8A@Y@BT7HRV-E3HX[,Q#( )>T&QD=QH0%TZ0B=4;-P)P ;X,.26#;88TJ)XAAF&=&39X]14WWT4WVF''%-S?[P7K0\&2*5#2/FX$1)F250+B9(NHX;^HQ_^HU/>HQ?^HZG^H^O^H*_^H.W>H\?^H5G>H7/^H2/>H1/.D>(@Q[X@3DQ=/30[*K;V&<VYOF.VQ6M37*79_-D=L7=(YZTG595L B0EQB4I060I:GW J@/^*@7>*IO>*DG>*T''>+MS^*!''>*C?^Z]7^**/>*V''^*GW^*W?^Z7W!DX8.?>0[?*-58AM<@GD:KM6T44,HNKXL/Q8G<I0I@-2''%DK_4>PW$>=.KS&6%(,J"0.7E!I6J?&>20VPXPM+CTEB@PE7H''EU2LE"HGI.D++@</J;41!"1/AG1UBN/IVU?K''JU6=!>@GS''Q1AM>Y''7I1P8=\G&<\)U_ UKF1(,]B6@PV@@@14@K]777^;77_?77 C7;!G7;"K7;#M78@@@@]"@E3&S2^PZ" 3-UB[2J$\>-Z''>C^F ;^RG4=-]W6Q$J<1H(+9K(RW(S$70L6Y.LRRG<":C''6S7?7!7?4[??9%7?6#7<61''?=+?<>)??4Q7>@6DMDHA:AQ@8"MH#0(LJCAP_?P&28\BKE VH@LB+$BX,''RA61PLHBL(F''K&HRJQFEZP"&%T-X,''3Y\(#L%SM],,PTY &"F''!D@P4J]L !G:@ C\&)=NTR&4-]8)09QAPSOC(X^M(H\*OG$A0=LX&$IPLBLIKL +%1U-IZLJG@&F5;026B.&?O "&;="2B-7MC=X6KU*7 ,8SYC-:[.GAZ1XX]M6Y<FN7YP$?<^K&Q]R-H$RDMJZ''AX)F''LC^_(+Z)&"YT&#A[&(9AQ4]J([Y/8<:-^3_/7+9?@0<.GK\%I45&PAHE,>''S5SI]-7PZ9!B]OP1H]/Z*_^N%P82:IJHP_+38<.WIT3!/O#7;=.;_-2\/?=?<?O''04=]__3=??R:Z0BRPGU\_FUJHE5(<T])++4W%7GKPNS#SR:H0(DT"!P!EUA\LCM^!!1>BFJJH.R7!1A:MIIAT!J-EA9L(BO 00RJPIABRI0''L@T(MO$C @14>>A#$#4HRFZRQP18I9H=KJ-%$$T0Z:RRRPD*YIIECL#$%%U\R6VNVPOYH"Q\1ZN\YU2JQ%H XWU1"&''MN5^PZ\3\9YP%O%ABQ61@J4DDAT >2FAM4DB*5$""G:HCG]Q=%I=IVBXP%1"^A,[VW)WE]V"&&&6+ZJVJ^Y/*)*JFR2.&())J*F@JW YHIRVU6!(T!%=R APXJ*![U(J+EJRFCR/?YF\!,-X5X+KGGH#/\DH,48TU2)97VZ:DMS,TDCTP4X,!F#F;''EQX)WAJ..NNRV::99:J[++++,-...>F&0EEWY&*7!HE>LEKZS]I"0">KL#57&"V"MJHEJLRJ0!LMFBS[,LLOH6.HI-Y9X$& /&I,J@JTMCG#*11]@,X!B)A\,,$J#G222".37KKKK<L\,<03+73CIU''YVJYGF7TAPA]J!BD(-_7>F%V\]*IA"P9C!YE!TT^)^KES]O;:GEUVK[)U-11AJ!YY''5HV]%-#"5466V^[''S[ZZ:/]M-- V8ZYY/Q6QFA(V)@V]L[SK T(71K*!H%,-DE\.NFG>5Y\D8+?)J@\\8=G&=+TP43'' 7WX!^PQY9%''-]&6GHG^.^]_]Q:::J^#''/+)([M^>+Z#*;;:9:?GC+/-)''.V^:LEJ(H E(;CVR" D^9JKXUZTHA!TIX(LT\("DL_O^H%'')#@9EQ_OMWFSVA@87Z^YP9I9)6%HC9H89._?/''_+%<>>>F/+;;<<I//O/4!6P=>>>7#_7;8:ZM/_0@4'':M213''O)F%M[R)ZZ5"TO]#\JT>"\@KB=-R''O=&$I C;&:9&\*!DHVH37)+WU2H5J["<3V4*SBDKU>#BE*IMLJ/2P:,>E"./3J)V-3IM0G*5PP\JKW +BQX]C(@0:RD1"RMZU+M>]35B?;W(N]J"R''V2)+MXM^)3+B. RKSBQR=2LX1"GFLWV5]@LHJN#E?\(!(]!TX0[&-<V)NWRIZ@@B? R5=E:=VJ?-[CHP2,X@^3S]BZ),QCH+IC5LNJ"''I"-BB:9$T\V4PZ/]J9@IFDIC_JR"Z1(L%O[$RS''.P$JSEI2%E.L)V(1@8(Q6''JTLK2%ZS\IB 32\%Q.''JU&6''%I5GY*A->!BP&PX%JHC&5/VTPVC31BU@,H@&''FPVCC0)X5UJS%J-]AS-3EECW1O@DI_P%ENH\I3''KZ\93(#N]:%0''N=/)3''SV9QHXLI!&3''QCVXU&MG*L''APY2I(EMVT5%!!BCF!@.DP"MO>!.5D\\)SS0FO:,3VTJ4(S,*TU3PGSV67\*A/MNC.O )R#YO3(Q=\(4#A*,XAW7D*AC,RHAI"&[7A:JJG&UCRZCJ1B2UL(S7.:F>*!2IF;<--MIL$=;9''IDHYX0%J[2-R%I,B)SYT*T:.Z J)N%Z)Z1R)W-4+U+''I5BU^U*%_C*-V2W+V+Y=J\3 2AASV12T7C$>%2(LVZ''RBBD#<QAQ)>E1PK>"&"2RSZ@06EJDVI<I:ON(PES)JIP:PLYR^KKL$(J5&SVSZ3$=4,Y#%[L,56M+R_=V1%OW.HSG0BL:;"5$\>P2-[8X*OGA0*5W)E1HO:MK^HYJJ3UCJ5NCGE&IO?(2J6-MV(J:YN$9\<IR!SV[/''P!]5+FS.KJL+N%6&\++Y#Q4I]VZ/7.UK[0GK7!>O>TN''DL1 Q=P-^1M)BB] 27(KZ!G64K,=#3EJBS_HAAL28]?><)^?>?U/ O,K8@LOFL@EG''BAF63 A \80 >^LHD_GNDJK=#BEO8/!3^LX@3?56Y4;@27@-R3''>&-TC<T:-A,,!L4>FA)(#C$W9>V (GR5J[R&!M:*9IM[&7S$8^HA@C,DI;6 LH=RTZ2$)/L9B^3Y<%Q]/JT(Y0^JU>Y2%&6L V07NW6]LDNX*C@7AS;&W3&CZA@AA3_^C3M6@2+/WJN''.HX9=@>P(-Z3I%N?1V52]Z:YT^?SA 4(P_MW0(#&-BGK#R#F>5(Q#,84(!^=JL[K^%EW=+QM4@@> [T&YX"ZI>RV?L?<42(*^!T^WM^MQJWLKF U/MM!SI*=6+D-T@(@$"T6GTMJMG+W.?Z5<K>=[BC3V,_EC/X1E96,Y''-[E8KF=+@=#V2(Q5-Z4];6-).]+JU[V0_3D@AA%0+KNF:PH0UC4H3?Z^]<++W60@VD!X[ECW+^=N)GCZDE161#QJPBU@@@@AZ4@HK-BBF QO\8@ />LDG3/BEJ=3!DT=802$N<X(?WNHX''7#FKZ;1"8O<82IO^LDA,BYI8H3_H,$![NU*U>B>Y,VO''@$Q#_?H:),;#K]N%EOL671SP5$KV4- KZ0< XAE>H@F]%#:4)WN=J]C?^%N#3+U*6;5*6N]:%M7>-R3;''T;PB@C$  P/VHE:/C>?K_FEI:)LQEH=^H<;(Y;K<U:''".@J$VRL$+##Y10DS=L(@%>FC3!A2?80"L^<X^_PNHO''?#GP1;1#B?<9@$?^\EO//JXM?3 J>?92C^^<H\G A<44LT+; 0K060SXV\+QBDB\)''/- 5Q(!&56_XS\*0AN-Y.-L6.5D4I ]!DA#;1"R8X7?#HQ;;2#9?<9S?_>\5''//N)C?7)Q=?:4*=>=+.O?^=3G?3WC7<FM*F@R<"K:O",UY+YSM??T-,U^74[JA+"KO_;!8"!2X$(3>U/4<*]"E/=4)=90 4@G@BTPDT(8DS 0PH:HAD4HDUD8@MRX@UZ8@QRHA9(@PCP0A1(B:1$#!4) !<$"M:4A%M4TE45Q46-AJ)Y"J+MVOL<C?;Q(BJYBH)\S8,=BJ4!%R\X@ X@ @940RY,0!LX8QD>PQD&(QDV(QH"8QD:HQL.HQP>XQQF8QMR8QP:XQM&HQI2(QQ*(Q]J(QI^HQ 28RYP@AFH QNL$I$ $B\$ ,>\V9,94K+QBU;!"Y9PQ6C-$]C<C]9M!PK(0@D,"!2AHL9\P ((("H&8"JB"2L>("L68"INH"MBX"UFH"U^?>H&R"H''Z&H'' .H''"*H%].KN JCY/]XN;P6?PMF>2MPPKXF08EXM4*I/<MZ3:JB=&Q)1VQPVAYB? XHVQHHSZH@&FJL&NLD1H.L2I&L2H*L3M&L1D"L2E*L&RFL5N,D4S.L3W"L3R*L1D&L3K"L8Z*LF"NL1N.L8L"L3P"L5#.L6Z(@F3H@_4D@ BBBI5X.AZDE8D\58QUP?]Q@5/]4 A\TR1@@%KDH-J&Q/KIH''7E;+DUZ FAT%ZTUT[XHV^LD!EHI]=DUG")MG "Q "FP8!RQI#.QI\"Q )FQH"*QJ(.QG0"QK^.QH &QI+.QI#&P!EDH 5H@_TEK:WYFI+T%JDO>O''+G]4R1A4,!XFMBX\-#X;]V[9O0!URPJU''#QJWZT]V''%U''I%U7+%U6:F(6@%IHBFZKP_QK7I.,E\''.7D8J3WP,H%TMQY8:BF[NW^Z$1T''54T$G7ECQ1HE13F*X J*%!J)Q#&XQX&*B &) 2&X(9JXY)JXS0&X2IFJFA@D7P 9)R]+ASBBC;A3-TVGT)%-J3F<^3TW3EAE2ABWKJ&TK!Z=\0W604MC=*ZC5:$H,1AV\CE[/I&[?+&[0I''\JKE[.JE\OI&\Q)''\ 8''\_X&\!*''NBVMG;C!\Z6U-,S!I;C^&*D%?B5H.>V!JM1@@&PH''?#IPO$#XPWW3LWI(VBMY [?T@C19U#*#''2Z6W3ZY77"I77*977.936=9:^%(+:,"K?L%=+9",WH8%.6Y"7*WCD=TW+>(WPDGX*,%\(UHA9= %.<AWA.:H[B!X_2I( *)8^J:E.H:H^":F96ZH"&ZH.:*E.$JH&*ZE8T0"X4@Q7XH9C%(4/!2,T@IF4ME$PM9AD-@QX,0XJ69'',5@RO%1G&%&<YLD(6LQ@I  A!80Q2 $FRJ#VN,1U1$")\V!)\ZA&MH@) 6Y%(LA&VX*YZNJVJ,:Y*6JY].:U& PT?N"E[09VXLD=@@")LRZJ; ESNI !^@@SQAS]W$(L<9Q[UT)SZ]5JMBZ*QJ:*QRZ*V.G=;(?=EZ1%(CBT>/-JTO8HFBH"$ML!Q].%%@5ZV,.]6!:NU<L$)V?I,V_@J\2"''XS@[X8B*YO Y%2HV.Q(YZ#L6.J(ZX(*&[M*Z0B.-0V.ZM6"M?\*Y''8$+?*R(PT]N)..BE#F*6@,U+A!UD6^T?4ZZM?J@VJDH :LV0P.[[-B&/%,6-+..1Q,ZYA.Z/I".<B"Z=B,Y$0DUO>NR8]T,B1QW[*Y$J@ 9NVH2;@\TS0BAQ=L&MOY@O2V[^G\HA:@@Z=H-RXV3FZ.3F\&3G^.3G &3H".3H$&3I%.0PPDH$"HD*G&VSP-I8_Z/[DUPQ"Z*61-4-N%R''J"*P! DSV@8C_B@]9_<L&&PBG''WAXAI+&Y)E4![+W"R-4$K-6TQ-WH#)&;H++Q[+%&(-5JY),BHF@&0BO\;A1>"(G_F(21V%"/&!?<GD$M+,*K9W_F6''+43Q_T$)#Q#B[S)O+9IM%(JJ+$KF)9#)WC1-468-47H-&I))X3I-87X-.8YB]KI!GM&''LO$LT]*]5O!M0BAM#@FEHP1L#]$^(.H]S @W^"DJDV#BIR"AS++.:<I.;L+.;MI.;]+.;^I.;.+.;.8.IH@AI[P\''\A^(*J[>:%HHA!T3[8-*9U*3*K <F)''T/CY9^0%]U:(+B9./)+M,JI)=%(-4+J+5IH)&:ZJYK8+>V+/VR1+Y)K?F4Z5E@''6:F#*"#>^Z&&&6/H.JEB%2B-2Y7*FJ=] @@-8PRC\1Y''^QWLF*6P$LJ8.LFP@Z?(6[PM+J[F>*Z7VJ&R@@[?>YK^4%%.Y&9,D#46979+!8^2M90W!VK5E[?1UB1:H P<$0)G]103S\@7[<@7#\@;+<@;3<G,$@" 40PS,DO?*(+?$XNB8I_;VHL;JG?Y@;F''4;KUXU@CER%[8)Q<\+]X>[Z8^Z>DJ;-K2J.JFZVR^Z^C2KQ$''[_ZBK[LF+W=>53;.T4@J"&2-X,>9+QKG9V,24#GMK=?(GTV"R]:RJ0GC1]NF "P0PP0 P 0PQ-#8[U.X!X@-\O#:*)]&?:>[)&>W$.$%16&1)#DXU6[RZLE4*%1''BBUJ)E SP2PT5TR &+C( $HJMEI-QZ?.F\(M>HCIH]0.<7H/>?H/@7L0B?L0D7L1F?L1H?L.W0PM(@EL=YG.UR,QF60,6!<^297"=E[?V!ND+&]QTJ>+\(Z?FZ6''FB;X0J D(?O6Y*6XX+I"0N#6I#B/W *\)"''S.(VM,&== -)''N,8_4.<I("V\9AS20FCB$JT579?>)%7DP.%Q5^Z8%.,A&3LZI@H%FAPMQDHFA@HJ7\@\1L@!^F4(LDDFI@HZ[GG U''K8N/@YD68&1>,$,06XY''AO$+H!\(U5J%AL''Z@441T,9)TD0_<[^\)[$NK]#4KTDJP@F#1AA#ABT3/5T4M5UD/5UEM5UU/5UVM5U&/5U''L5H6Q@A#A@H_3)WC&17WG.3M(\P-<,,7 AW[;^$9IZ35ZQ\P6-GG%B+L8*Q5L2W8QBI*CL)!526ER-!**4FP/F!%9JVVSBGB#@CQR0NEG2CS306)R1I&/JF.M(4OH%ZH0 O:*-K)X."46QHBDLI#CAI:BAV,^]1MR]SLU,P7/LR@#2@@\6+*JAH#RAC"@[*N8AA\2AIBA@HAP_H*@PX.PED8BBASB@X4M2\SL&\SJ0R1.6/V:2&%*7/XX2I_#$4N*("U7.''QT(J3N462V%9>IF;Q7*V>/?(.XF5AH41W.;];?D=73K-7/C];?T]7;S-77O=7/+]7?#M7?_M70S.G0GNG<[^HG[]8KWM8@+.HL7>HC?M:(NCW_J6''''RV0-"@/KN(&+O&_:AM6''J;3=M%NU$"0F9*#!#\WQC1"D 0"H00!M(P 1$0"NK#RQOM>M&0*5FL!(, !D:@ROCQXB1Z2X P"L,K#)+".H*:6U&M''U&3-$9\1?S+^,Y)URXI 32!A5, (__''O;N[XUO2O[T&+!VJV8"0K*F  IP0A9D@ OT>BE(@ 5 LQM\  [(PBR  V=JP"#\@BH0@D#OJE, =''@Z-7NO:VQP\K;JZ^ERLD(S1+0VY4>L?7H%?Q9N,<&<4]^U3=W#%OB6M&V<3_I O_E9GX4%!@F**3)S(''**.?**.?*+27*,+3*,-7*-43*+2?*-:3*.9;*/<3*0=?*,A3.1C;.-G7...?)C?"&UD3W60E&G^3%;L_F3+67?/PZ*L0D]EA_%J-[59+U%\24X3LD''4H@O^HDE?@ FYDI-[4)_X<@S?KZ!L8X#R4H KHHF-G-%L,D&>H "J@HMTHHSY@HBHDHW(DDA.4U*J(HB)K&!\;G8  7XS$@>J9ZQ:&N>IH0?O[F5-=4]2:W03^B425&IKJ%CO&PGRZS]A #^W"Q-27M040D^XH@$E@I 7@@#D@D]3DFMD/>AC6!B#QOXHW34CSCAZT%B D&B"<\@D;!3I PBF,2AYA>BE8>1X[\4=8+1E*>3%-[)])LPO''+RV05%"&5P,?.!SK 2TFC@0#8MA''D*;DV%TV=.[MH=%X_67AMU7T?N7]N[7^<=7,,=8J^ 8M^=-]*[B,M_8BP(2W>8\]#Y7;>_&850W'';3@_4YLJ:86:P&CXCBH#A@H6A@+6D@D8PSV("SD&RBH 00\H.SS%JJ!%Y&HT"BA"PB.3<<\C\BGTPB:C\BH=@@GXR5I/"@A"!AW"C@Q$+B$Q_<Q6;$.RI?WU@J>J*/$3\+V=DK;9@ ZC;3MIV.''+W <VB+TCA%T3)>[''G?Z8)D4S[OF)''7(BAGMFL PDDQ0QOX/E7H.PX0 RZHVQOXPPX@1JEEWY9D($DA3YLL V80XIP!$TDZ$QX1@QL*$1MEMN!P $ !!"P0$$S^FE%RYD*TH4>6/KFRIA"SLV''JKM&29$,0N1GU<NLDR5@,$C1AB$(TT IOWPA<.!Q&BBYLTZ%JG[H$:-P%V*-N-V))BRIJQDRIT R&[M$!!>!P CR$Z%Z9U.%:-S/7[%6<^?W63_.WK6B? P$OM"0X\^GD!?%^;Q(UJ=6=$Z%"-XP)A)4CR-I6=/0Y]F#Q(4&WM''4Z]V+U''(\,Z.HE"2#@$[4V#&()CAL:QA()GW*4:F=O?982^]G2"V[J&YIB7]ADJYHBA@ J!VI@2P^ZSCDBYPJSJUBLNQ+,:M"$HA@ZMH.^N@%DT F"P3.7">=B00,C;2ICX_BSPX%K%DA DP,PTXBBOTBYH1MDME D 4C&XD"AFMA08(%M=@,ECBXZ^PJCQ!*IXRZY\FHN T6Z(FDN781B:+\%DCBNDT> ,(*6.QRC+B+J)+ID%DZ4@HV35X0<D,$$%U0RMTN\8B6A6/I2SB>,QDF@$"XV0\JS(AIH@@L1/I"C09,$P^BPK( 8(@,''DFDP@RVTBFTQF+RX HX''C GEC3/,(BFQ:6!(9@XL;M"C!!(,(DLKF#P0RQLZ"JAD$_<Z:ACC#4Y6R&&$D%W2=JS%WFII5I%D9SP9''E JAP5J?E#D*NBL>,4()QHQH1GN)L11R%99AZ-UG\)"H+Z45/HA%AS 0%F.+Y*=25$\(Y766V*%-UX+Z*?]M%-,+=J6V6:?=S[Z\L4]U=124S5W7WZ!#R-GY=V%J80%@*EA!2JY7I___/5]3QH''&) AD%D^"7X2^F/K#X8=FOC--8!#=^PF83;1%ENSP#&D@$(V.XGCSSO@X9EFPFED TJX2H@BCA#QXT!DL(#D"1+,<BDQ!![1P LDIH&!"28X<FIE%C]EPAL;KF!$C"X2XXKAQ''3P0(9EDH''$S1(F@$TASQR1 AK?F/B(PUL&LKCYA0/,<BJSL,5\UTT:@#ED*E''-M*PP89:0\T)&4>V*5;-DXT@KB +9E?GDEU<<+RV\6JN7YP''C"-\+,<P *U M0T@KQPI1F:]@PM%CB2I<<FHSMI!@TQL]Z$@D X(A(LF=P0:I!@;=E"G"/4LPTLBBOQI!8!AE=,#@($M.9X6%''EQ*?/$RLUX55UL3M/M9LG):-V; !.)2NJZ:^D**6& 3?573(:)WKKID@[J3HQQ(B9KKC*8,K,B)!A]?J__7,W?;T9?>A"#@@@IP_0#<W?8V:C?JJOBACH2 @0MXP@#6:CE=DZAWZALFSJCAA9-!7@!IVDKQ-NX5?0W3V5\86AU^STTT..FMH[(TJ;,I!VI^<DLWTGVR%O2ND#U@04U"T(!E:BACE,!YHYQ''@T642 \LXLH''<ABIQ""@DS98P"@6TXN^RPHM"*C@GAXQ"T?LPRT[^4KC_MB2=7S(YT>P!@ZHXH\''ZDA4MY"CD7S @2\(XAM4:HHB)@XJ=SB"BWQ  ,> 59HTIWI.WKK!T[B0!DH(P ,5.!E-H!N/@<:EU7HA$)BHYDIS''!JU(#BDI!3&B_/93T]S,)2V$#JTAA!"D6K:''H%*@(Z.T\@G/F,BG''H&BRT4P \VPHLRIDF4QA0"E@! P"P 0H@[GIDFZJBNII:0-*8%J ZEPO<@FA"0F455"''(&LQT:''4^ST;DSU\)!I2?[2R((ZLDILIHU+V0%!"8(8TZM$\1@?<J>F.@!ML\B1U-41A"GK Z"N7*(QBL:TX-VEJLT5V"<6IV>*%!"BI''IU2)IV%H%AZ0I"$"A0]JUO<C)K01,\Y"KIAX\S2P D:@83$."]9K_>R@RZFPIFIAF@4X<0PJ_BDP!EIBH)Q7B@!X81@4>8XMM7B@ELUBDE3SQQT4T@ 1!)@@SMM@2$@65P9+( "I0U0MFKGHQO-C@ISS0&+YUSH=1APTSHADCB7A5#X,X4A3(P@O_QX=S(X![IF$EGD!XT(^Z+JAKI:^0Y 5!\E)H1NG?SM)Y32KI\YBK4/5&89UYX"8A&.N\9<+T2DJ@MS:ZXHS1W*L@I[P.D( (1@4TXP\LYHH:SKBT;.0P5N)\D5%3^A$(_K^S8!EA@:5URS1I5\"X:NQ:O\S.*J9;/ZE.[4-5*2D%%]HEWIG/XE2QE9V:QQ&0"@V!(E$K?^CBH4382+;8/Z==V;!_??Z7$:CL+8C=*=<@4>W@A.[/ O?KX@T7.L@K##BDD43 @4.) @RLYUZV8DFQ:..3HQZ1V%0CF=''4C<O=[R LY] H.$&R$+LZS''E6JII9R,H$!Y!CI@34OBO"XQN[,D@W@*FDI%81D$F=PRHT XX33RDC"&@DA)1X?8"_^RDQWPOEI)# -!,8[R^A4LP'':F@GI52SA%?UAAD> XAPL@DT$P"DD2KA"DL(@P6 @LTSO*DHA$0''EA-)[$48!P@L+J!E8=4''E/BF2[59\CG:1[BN7A.$HXGXD$(HAE)F7N''ON*8ICOCD,''Y5V_=]2P\S6EJW.KP9LYF)$S^X@=O\GL5PJ@@C^F"BA *!@Q7$M''X[4TS[4EPCG1B*S!XH!I-/8@QJ^DDAH<-22CH1@2I(X%O\1]:I^F''][QLZ7L6#7$9Z1[5=6*4(- K@>E"Z0AUS^";@<(F09G,HYB&K ^:&H@D?.V<C:)-??@Z8ART86XG7N>C?[&CBK2#0!_<ODE0I.9Y%LGD/GWCV41$''JT)'',MI5,___K:UJSA/6,ODR9W/_2>EN*U.RM7^AD$>8PZ6[$0 "NDDCR[U-D3V @A8+8@Y]LA@X&K+VHEMB@4(H*2J^*PGT-R4%SMCDD:PS3TH80P:C#J,&%JBA85VGBU:P\1&_0HQB9G''K'';C"Z<E @\L2L%W<TQFK%HI.V$D6$99(=;#V&6FP2>-\&SU\Y9[ =P1(GOF(W@H+H6\W"OO%-I%+=RX:-<.ZY@HC]L @2LXY3X+9XQN!<GT$4KC[#S#AN6"JQAP1O9D9#ONZ%A##I''P@;^*@@Z+P=ZEM+N.R]"IVUS()U_@Q22''-5PC?''2"/:V<R HU[?_NU;@)L>-!G"_!>Q'';4@2&DL7)Q#W(_?M4W??_GG7:IS)C@$<Y0/PIA!9D&G/8$QJDWB)[AEZ-XK;&Q%H/G>;4W^X''F^J!97*$!,@LQRLWY%@4Q,B2\EJAR+, B "(S.*@FJ*HPDDFLMN@IZ@@CB"D#Z(@BC$DS/JC,V&-%;F@SQ"JZH.TSALKLB&GM6.3M8(2LH$EO%B@FPHE''M(DFM,E''L(LNA$6^X"IEI$A.W.3?;LZRGH5/F""#P.$.Q.''R8$]9DBG>+''A1''JRUIN^E>(\2Y&''U/@QL]H%CPHVUA U$&DLR9"A8E$G4;DBHSH=6ND>8\ _M?>RL6QZAD+"&D^  ]=1,#.2@M5+K^+"+.-1I7@ 0''Z"''!6H"+G0BU&CL?8:"/M[-J_I-4#2I(@0*/&8 @Z PVQ J_S8N<D"1/Z"%EB^.(? NEU\1ET?QEE&1V%9QE-5EE%T1E&&1EF5QE35J9KY%VS8LB8_1W5AJD^+O[4R.B><B,7Q#)%#-!(A#9R9F7M)II#[AA2#!D90@@1H!TQ)!CQL!:3P DWR@C#Q S>2@D]A@SR A@93N-2QAD<YF@3@ D(! ^A*!A" A=[IM@0(KEC@@@3K@A<:QS)* A&I@D9* B<ANDR!!C#B@D.1LBSB02]A =S;!BSCIA<3..*++H/<N#^9XK^U&1QA&IN=.)F=\LU0ZX0&@!G@F#?AR8ANI4RZYYOEDJ2,24Q\#K;UZ[[U ;RMCHQ@,0@>L" GT00&B!1I"8I 6PA6]1 /L#M NX[ 0+;"V+S''"R!F&J!FZHAH4@QG4T@2X11(?L.8Z$_^\!? R\RU8R-1X1UV@H,Y";JZ\39?P*4*,AU5@C!O^Z23JX X8SR7&125XJNG<1>G<3-<TT>DXL3DK3#DW4>@",3D%L=<^,3H!$3GUK77V3584@<Q.$#QWX?9"0;?,:=6V4QH, P''634VBX1I/J AQ)QA1#<(, @KD#*-@9 Y2S(%@PQF:PC.V, ,Z0Q-A\C_?38")''@(T.(CIYN8PO,F-EFAUL,E+JNF/H$DQLLC,4@A,E(DAJHICL"DCA$$C6,S,@"DCL(ADDHDB&B9A #A6/H6(E(.FXJR1CDDIELDOFBDAHD7"J#NV@N06KJ6TR%MA&\QI^FOTR@/E9@HLZ2$)\J''2)H-TFD@Q"@@O,-DG1JXFMNDFB"DP/HAC=PRX!(6(&@A,DNFZHDCY&L4I(L@K;"0F)-HG6L(G=(A9.(4$ONUT !P''7 ''GFMDP/8490F.RF"/]% IW@D+BSD45=5K^:J4KD@@T$6W;?KK<.)S</-S<0-QK1QQLD6LYO7M\K,O"Q''MA65P4NL;#U/K/Q-EY?8Z@XY;QK&%%NB)&):+QQ3_%DA(A@<SSN4 %A)3@B]A P.X#D=B@@VH O#UAD:2&QC1$D1+!DB)DIFYM"+H''TCE D3Q C''YBI&I@@0:D@TYUI-H#D2(D:C($D@K!DI!@OR1DKC(R;*1Q,R@IBRT&JOBFQ/PNS[5OD:\B,0!'',=1TVU\#-A*/L QCQ71R-U8M]A1QD!Q H77 S6+ D6J@UCM!ZF" AST!@=A@X9! D1"AHVPF@:AII!@AQB0BOC[AB;0 @92MBA"@DE/.DD&%)>J)DV=B>G;OW4&57LHK9YQO1#3!>R8QEPW*WCB!>.H+@^B''LKV/5B[,0!3L0"R,0/<P#LDL%LIB%&MA],ID=,ER-&MG=&QKE&UW=+KPK3V50,O\#46WEV_''+6@N[CT!]B)"Z$\YHC<%<XY0R(]8"FASYR].HAL20\YP)V&U 2QDH!NXCAFX8@Y@!V%9Z#&6])9$ &$K=UM^H&,?9T^1Z6(50J6X@M_6,L.N=C8Q#ZY$42"VT@,^#T]T5&SUK5)I:VY1E''@=H;QD3_86M$]PS]V8IB!.Z+T,#9869PXNXP8&U0EVY6&?@6*&:&%8"&*1E,^XXGVT07OG88)@=1@XPE-#HE\G$M.*14_?UY:H]D#G;_!>X$''?39Z0PG141]S:R1%Q#ALS2-8V"-P&B?6NE>G?$A_ %G_!&M]9$1]:%3]:&7]:''5]:XUE\3$UM<PW#@!]''TX+>VF(V_3E"&;G$H@Y7E?[3AO@-61^-B-[F,&6(X"\CPLE[P<[8REU>RUT-P4[\=C_['')Y?!8((8>1+H*E\/Z,&G&$$[2!! ST%L=E8CZ!X1>UXB:];/U^C22H''H2^CEEM=)GW2K-QHQVH:S+!Z"\+M]&I_P:F5WI#7K(IC[!PB''H@9E@@T"D@QT*Y:/F- V=]=X\K75)JH.>,"C%Z<D#X8>&%<AIR H''PY[2L,@%N>CELT21RK2URK19RK,;"K0R=BH\2>6L=&M3!''R21N@T#R''MANW7NF=(%.E3^''??)TK^/XS>48IQBA@ZY*T9KV#8DX$MN2]\&H@''[ BUP57FI"U8<0"R''I[/EVS(%UR*T0P\."W)KB#I]UB4T-> P(",EP\[G R<KDB9[MM,^M4G+/KEV9^U[FGF]@;G:MTE 9%U^9%%F9# N6^THA#B !>X!VX,++RPN* &_VLN(E!N M>8YWEBONE/>FWI1Y&*NY&*GY&%_1&&L1&9<Y&;-Y&0VN B9CFCOYSS$NFRFV, XJ,6SJ9N)R"X661 K9#0T9; I8''.=Y''/N9'' _X%%%XIAN-,_0O,/8SB5Q2S&>1/_3280R/^=_"D=B@''M64VT\KV)L1J$SXD3Z''\8R2!O=[3*O-L6%Y>AB(#@[, AHR8PB+<R1W.*MA.''U_.)_P@AK-4.:F(6DMN''AHR>P.@;8R2((%9>C8SX4#\:#=+* ''6J"IV*&S&*&Q6*&E^*&_&(K-[2?+ (1EJJHWUF]O+FU_BE(Y9(5=MVJ8!D>QP9_)FY>UE*75NZ7[."UXFJPW^N8RS\Y0-=D2:QNM^Z+53W\_ 9IA[@$X@@>^HJ,W]G@_UB(L5?8&-O$*%OIX*:N''Z?^&]+JSP;H)^6*UP7K''(GHA-+H/6;H?V;P1N;QIV;KC2)_3JYSQ+UY4]=58%6^M6T]&V8);H+8L0VHM!)&K]8.=6K];F;"?VK!=&:N>YQS?(:K=7*>0R]LXOT:AIJO/QF:]ZPAOR5IO\X)O.&A:AG"(.M.;9_^;.1.<15.<23.<39.<4U.<&ZL??N@HS2;FYBR2B#*RH6*B(A,&L[ 30 @R[.A/%3/>N# AJJ,KJP^F,JPITD.5RK"_<_"NG12O+P;P^A''BJ=3AK=3B?YP''(HA;JL%WHXFF[+)JKC*'';ZK:7L\IK"E>_!*C=-Y#4>?EWY1%>7+FX93F5;#F\13G]13F][3G^_2I(UAJZ%XGUA3@[?H4.W(1? 8/\"M(:2: [X(8KNY))2.3+13K,53K-93K.=3K/13L.S1.9Z[.[D(IK>%.[VSA M03?_J/#52S?1>G\J%Z/ZHU@UH-CB]OSAS@!SG\33O<30L]4@]]4M%[))NO[.N8R<0+DQ36LPF'' "5#BXJ%KK1@D+K4" ''.*JF:*S%]*#]=+3<=*#6]5DN=5DD]5Y]ZS#''J 20. >D<?,1Y_@EOF_$VJ=[Y''F#H07E7S7THNV@X HM]6H^]6H/]6H<]6YM]6^D:E@+!''5=LS442[?I.5/,25P_C0NFB(VE](!>''M=B4WZ D(3UZDU;O_9\]7]M]7]^]7X'']!VM )+7''QU"M"PF*-J#:7RQ6"-6GB_SNVG9:.HO;-0N^8@_^8B^J-#U1BL@"._>[635-*=U906J)B>>4''V4(#(5VB?>468_B7NL?G.QCW.R3WFLV":Y<%S>Y4H''[''CE".3[^?NDU=KBAV$*;DIS)GY\@@@+0JEZ90>_C@>!?W."C''."G7."KG.&OW.&S''.&W7.&[G.!#5P%( @W2BQ+#NB&V0!J9^$([*,B3H'' MI 22M@V6]E:&^Y/Q7).9>Y/[''.7_W.7]O.;!/)*=^U %E#O01^E#W,RZ66B6(*I=,SYD0]8 Q=_SU<(?+0H5@UD[7?D_G?H#W?H''''?H+7?H/G?L-W5I5"DW(Q#<_^M(%2:I7DW-AFNR6''^=IT<A5N-<1B<DU_C 6A0B48@AJ>$?<1OY3G?]7??Y;W?]=''?]?W?"C''?#? ]?8!??8"1?9%1?7<T@L@@AV8##%.HQ!?\''^(W-ZIA"?LZF''*1!#>T]&SU7<+57TN77<Q?7<3Y?<S17=59?IKY+!QSO58R?I5Z>X]3H.0K(7EB4Z!4LRP@D @@CXP8PH0XLFD2I\*K@!08\NH4J\JKD"1X,Q"X APF]N@$=XHFDIRQKK$$JJ?CAJDF[IDD2XW,Y4FWO&29,4!=B,"R%''32FVQCWR@$*I*JMH$2)]2+R)4:]P(4*]R+V*5Z^F''N1!8F&H3I,?XW;]NTPT@$)ME$EJDMIS@#RT;NBYR;^.7[-8<>+]2;^/7;>@=]*)DP KRD<"RX)\":TK B:W0(B-??''U)%#KOE>6QNMCQ=L!!72@ .S5\&[S%T>+S,4Z-^/U+5/C''"6;].3[-GG[3,5;]>/J0E5ZF!JC# :#U9L+W<:<.WN&%)04&PEIED:]XGURI$,6)*!C]KY>GK$XL^JQ''!A(*$GCS//7;.OC''2>?O/7;=/O#7:>?O????-G @2M ^@IRRX-!XT !W&#A"B_VV^Z[Z=)M!%MPCF!APREIVYKBHQ0>I>JHIIZX7AIZMYH@YSA-Y:EMY*FE0U*I)Q\HH 3$*NNNOO[(88=@A"''$$DPVZVRP\1SBU(JPGB#RQ8&H@U$XKL9$9WYVW"^VIT, P $^''2%@A0V$XQ]VSSR%>_>R&&F1:^ZZ\KXY99-25$''''''WO&ZZ^^^N;)Y9>@<"''(''7F^VZ!L%&@RB@7G&^#((9BVNLP"SW!Q''V9(@&](FD3PPDP#!(BT6J#$#WPIFC]$$.***+[J:*..1 +++KKVR..--.ZJ::::Y"JID$4&^A9I!""!"AYO^DK%Z]45>1.[MPT5UEEIK\F@C$=D*.667D*E8%Y]#UTY-C_E&AZMBJX@"[+,+.-..?B>J6><=L9+[;787*-///3..:=")PI,&B]]"IFHTY''9YJZK3K;DIRH5 L$TZJJ%LA2KY&X,8\XZ]:301=&A3KBVIGL,\,$^!:322BZ/#OKIK[N\,,00#=W?T''FM]*/33-1FU6$JDR[LD<X-V#!DFJFI9:R(9H%*8MMPQ2755EQW[_WUVF^-=]YRI?CQ*N\5N]JBCS8(&U!K.I"SR6,/+N%WYVV8XUJ I\H@37#''W^HR&.2!8(/A/V#.#F05;][!G2VN>NJJM<;8889GC/''$$%]N>^VVY7;8RBT!FAI[TWZ!1KJ65YX6VB59R\QQ$"Q@-9!$I#)![9#R[//,.M^^>>6:=<;;;:$)SI0OA2B''=?GHW3U)D8) HX*D7KGV''\N\!,]@*JQ:7+%!:GWO?_\G!.>=>NBO[7;9:IN/?/''+)<?>>8Y%''9!"I:V4T$/Y>R8..]9MZ;0(XR"V?>.RQ<@BX$T+WB%M=F9S%+MLPB4IXAK@ID#ABU*0 !"<(@X32LDM^+BCHO3 RJ@$)\"TI%NFH)]OXKJY3!2%B6B V6!FT9*1BL6FNDS[CWVX0231<H\>CFJE C!DHY:N"D\48 :U6LL"N#FIS51"EI,H1RMV9&[F>Y<A-6!@''<4@ZC$L# =1"CP_[@TSW#OPQ0;G1#V:TX50[FL\72#GN-K1#''OL(175"L\=>+FO L3#Q8Y0BR>(ADKR$6HUS8NS&LQMP2G"("P''^YQ/*X!(UZ+PD\7%ADO4I@EKLLPRPB%JT(82%J\,IR)M2\)U.%JU,D2%KE,YR5+N<)V7+BT.[_?I257:,)RF4L$''G!NY("&L$Y QU:J<ICDL?F<H,B/SS=)F3^-T42[W1D866V[M[&KS&=(DI3^?R\90%''N\9$0''N-^YLWY.,970_J\<-[PSJ T""9SL)=:VY2''+]B^YF0,K=Y PG 4 I THSZ!BE<+P!#+4(QBM*DP''R-FJV+R"'' #E,\16FW^JD98T$!YQ-J#O$.[-V0$4W_;J!P@_>NDIBK!DHVYJ49+Z=JX83Z%N]<+S''/+4)4@MJ%@/(PQP@NA $%&!''''K#,B9E;B!]$VGEK+Z;*.+/*,CC*%V32-V-^)UB&LA"3$1J5$!9DX0L09K4 HL4N/!ADT9X1BH0LM_?.-K5+''[MJ5;7*-^></V/_ 4,X@\+6LHR=+BF;Z,S''*@CA;D$_57MD%%DH[]H%/V27NJ[71H0FXB.1"0>FH O_D@G4)*6-J =+V)S2=+U.+Z5,G6-[FMK6=''Z-+Z8/Z5,_]@DJX7.QQ!#F3%=&C)J+F9"4YR]U)\[6^Y>5[''L_Z94LXT(Q]DAC2SE+GZY04?''TQ^,&P''CCV( DB60P@/&Q^=94</^=[)W/_A-[73_J=?:4/^><<6/__VK7?7:-;<@5.=F-K@IY\G$''5-]S]1XPHG,[/_A3<$J.F+($3:E8QHY,LL^&.BGI$3@02C>,H!CSNHQ&;#DJC:1"%OL8!V;?;#ELG:1#FNL8 ;/(PZH0HI2OZ.)!G6ER7C13LQ&ZKFUD,7HRC:2$)OL9BT;.\%P_+JT(43%JU.92%F.BYU"4A$GP?#KT_E"]]:FSLAQ1$NM@HTEH,G&M+/93WBN,93''SN\:6?''N^L:3''/_LY -<8!AK(N*UA56.CHGB, F<AHS@3F"*VGIE/7%2@"RA!$X4  FW3#R&M:7)S''O:498NMZ!GK^)R$?+T)$85*%^-:%P7X :WPK@QD3&Y8!97JZB!P^2&B=5^<?+W4P66+7,G%BW\,7"MS''YT>C''&W/_DDE:K-+R''S^5*V?/Z6L:6-+_M;V9;>=/S=.R0Y8L!A#=3C/=^ZHR25=4T%HXK![Y[ +3''S^=:6?/^>L:7//_M;7;;>=<@C3"=QP%N R(U''V +M,P$!.,Y$&[\0HZ8,BMN<X$G^3 8<3J;F>43QZ@U.$AIR!"NL''H@$/3$I$=92U^N\)Z+/NT0_;''LWT;3&M]<9#[ON\97_/L0!BEQD&\$Y\T0M:QXB5,[S7(%>?Z726!S,%C7""T$L7V TO7*U,=:5[^N]Z9+/^- ?;+X/T;6,I]=;FYON=+W_''Z/41*IT 1+%81;EL;J\D3R+G#P=\97">?];3>6;%"U/.;.O.?/"N=;8 GO>J9"RDO?,8PREA@J0&=\0"%U/NX[/?''E#1/H%AC?, X.\_]]7>S4#$1=CUF?^-V3?/V.#77+Y0=;6,.>=+"?/^9-3?/\=7;7/ <><FN?YW1Z//CR(X95!O?;9#O?>\M7_/R!K?7*T??:4<=>Z\)]=NM[7-5@;;38NT=>34-7B\VU&BI""ARJ PHKO?^J?N]O??+[??;83;?>=<???//??0@X @H8 @P(F_\4^M8GY(YG @3X @;8 A@X QH8 _T7\''J#\P''HZI*EBH[ \3?''\5O7 RHX "PH "X8 "]X ""8 "+X "''8 "0H 28X 3P8 3X( 3!X 3%8 3*H *I BV#0UM?!ON4''I%: I@X2LD*X!D18FD:8!D?X!E@8?8URVHUQ^HUT"HUV&HU\.HU^*HU 6HU!>HU"VHY$&HRF @VHP@OX%XD\I14VT@"XLHY4ZHY5^HY7&H]6.H]82H]:6H^@>H]Q 0V )@%DY5%..F8(5XFV4H"N>H"PFH&RNH&TVH&V^H&X&H&Z.H&\6H&^>H&NNGIC@A_GUWIEZ@\:@@*I4@U],H*.6H*0>H*2FH.4NH.6VH.8^H.:&H.<.H.>6H/@>H/BFH3DNH3FVH286@U(TPHX&H %!P$X0@ID@@*_\H3V"H3W&H7X.H7Z6H7\>H7^:H.+6@V_DE)G:H2W!0E"T@LY0@!O8H;0>H;2FH?4NH?6VH?8^H?:&H?<.O>O?-"O@O&O@!&PAC&PA%&PBC&O&<@H$T@PSQDF$ @J^J@CN''@@EF&QEW&QF)&QGH&QG+&QG=&QHC&RH%&RHW&RIH&RI)&RKK&RK*&RLM&RL_&RL%&SMC&QW @4:O! XQA$D6&SL1&TPC&TME&TP&&TQG&TR)&TSH&TJ7$@^D@CL;HTO+^SC3XD"J@CS[BUWM&UW/&UXA&VX#&VYE&VY''&VZI&VZ+&V[M&VX6$GQ>@TR<@D%;X#NFJWOGJWN*JWN\JWCNBW I&W (&W!K&W 6&X!]&W!:&X"_&W">&X#Q&X$_&X$(&X%,&X%0&Y&\&W^($HM5@VU*%]XX@E!4BY)#''?&Z"9&Z^)&*&I&Z:)&Z=Y&[F9&+OY&+A9&;JI&;SI%3EPBJA9EIY0BT2@"JEIU(V BY/0AA"0B\/Y''L39''L8Y'']@9'']IY'']Q9'']ZY'']"9'']+Y'']39'']8Y''.@9''.IY''-^)@P+0EE1BQX+T''.39''''@G''40T''?P9''?X9Q_VI''?^9ROJ)''4LT\,V9WR_$''?2Y''0V:''>9)(@&J(O59(@R:(@?Z(A@JQZYXRV!@@9,P(I#UD*KT(Z#$(RC:(RHZ("P:("YZ("!:("*Z("2:(";Z(#C:(#HZ(3S:( R''(S"Z(3*:(31Z^I&P@XGP(4H:)HQ7@0MD)D"Z)D*:)D1JETLPSD4Z?:UR2$TVH@%S^*UX&*UZ.*U\6*UR. $H8JU"N*Y$V*Y&^*Y(&#0)TAZV @ALLC)4T0!L$@HUN RR0@S0%1RF@@ZY,@QJ$P@7H@&FHGH)0@R!<I,@%@JY@J]HDPY22*"UE@*YPHS-I0%F*!R^\@L78J^M" VYD@*XPJ 7T@!A QRR=:Z%R''I \@!4V"6RR*&U)J%G^!QX0@Q DJ*]^ N!0J$$)0QL @B)>#1.*)NM.**-"!RF\J^3F ZYB Z<R*.><*3ANY3AF Y.B*''OH:_D^!RX0J]9"!PI B+O2*07@@ZC&!RE^* ](* $!PBG(@SA. R!\@"0>#1 4C*X**''''B/>\''**.:I(I/$$7/*)E%!@JS@@IER*/%=*(R6B)KIDT$FB+4-*/"B(JR @""H(I;+*-O6"015)7$)@I2>(I&^B,R%F+I&.*0%$HE^*(SA@YRZF1+J(TR8@*2)HTIB.(H)^)$/B,()B."C*-AN.&LF-4XHB''MG./>5)7/+J4XQB1(QB,O?.&"C(DE:-EF''.0BQ,JM;A(IC])&7*2!9B2I@^5"A(F@5.5G*,TP8@@M8B0ZQ*7\X,I"5@C#XB(B^@DQ1@H.E)7"V@A!5B!!_@INZ$TBJ@HWQB&#Q(C$_@D,3(D\:@HCOB,$6J7__.3#F@ASOB''"P@J$%B!DZ$H)M^(SFC?@T>0+VDP@:C AKNZ@@1 @YL[,81@FI^[@$<PBY.[EI]@@QS0.T%1BH![GZR+BJ\+\(5P@:2[EE"0BIDPBGT:.72[E@#0-9UW+MRH+ZW[A]-*[AZP/H6J@YR@BL>:/L5[(U!@.D0P+@"P@]4GPFC AY>@+X2;B[B:ACE @X5P+X/ @;FKEA!V@;&KEI;0BQPPB*@KB!R .D\AF%;0AOTZ@59 TD&Q@A(PBP0P+H[0AAX0A8!:BT? .T)QBH%P0AV*@AZP@ZN;0LC+0D+A@C: BU8+B%"0BYPP)G**0Q0,/X& BEY**#_ AQ''@,ZT[/=VBA)G AISK/F"0-E"@@S8P?0MJ$PJ_@@(7DJ0?''@ J?HL7@@)AWJFAP@%SZWQF+F9480RQ,LR:&0DV$I;(2+8,&1RR8@TESC]3XK)KJ0*AXK^-*0$>PLXB/@%^(@BH* RL$@ 7HGKS.2DU& '',&LKV(PAG8K "10A6F<L3G@%P#KN[,LXUJ;^^;JUKP@GH\+&P4@UM(@&H" T48@](TJE 8@M8$LV"$@%D8@NG7J!N(@TV@J-C(@EM,@''O. QQ< SO" @V8@\6#AR*+@MLTLHE$\V#R@RJ<C=!8@Q84@V-.0$/E\3$)PFHV  V4@RY#APH H*A&1RH4@P><D2A,@_SKGHY(@VILJ.;*0TZPKNQHO<FS(B(M;CJ@P1@!4@DMUB=RHDHQD@G?6LHF.@G6B12Q WL:B+J=82.Y!PH?D0IMMCCIG\HN$@I;H\T.^115ZHIS_@DKT,A@K@IR1,J9IWLQ9DBO$@C&UB!\4BQF/4<:]1ORZDUFPB+VL@H2AJ,BWCLCGB9(V@AN,CFIB\IMBCSE\(@ST@I4F3PH)4TS?@X,E)H@CCQ8F)H#_B,?T0DR/7H^D@IV)SNT#7A":@EWWB90*0E7'',T%6ATS"A2R/@% )0TB+@GMGCKB53S%HBH& @@W.CH^^, R(DIWZ@E"3B+Y=DD"J@T]44CG74TB$@D_U4-#\@<,2(J&.@ O#5L#AAE1X+PAF!0.P PBSP05&FPBT7-.4 Q@0AP@5%,KT5 @SE,BY. AS6M+,]R51C+AW.@BI_;2\Y=7L"]7L*=7L3]7L9-P@DA@C,b')
%
classmethod: R4PReport
imageDesign1GifXImage
	"created with
		((GIFImageReader new from: (ReadStream on: R4PReport imageDesign1Gif)) image asPDF) asMethod: #imageDesign1GifXImage in: #images package: 'Report4PDF'
	and moved from ImageXObject to here and fixed namespaces"
	
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 1037;
			add: #Height -> 317;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '"9J^ZJo&BBUdRO+jPmhRU4d&d<$WY-`59IRioHo0Uf:5[N/ESDd]@P2>kIU21bpVhZBUT:Hk[2%<)Q]
	Ng9a777-s3WFE2J6@Ut.DKIeSu4?>%;b,h\EN5WH+]XjX2ro@<f9RM.sr;+n_)AJ-fFA,:hChcr!daY(
	]K\Y:o$jm.(il''?p[.Ne1bf6`KQBYW\<(onP/1N253]sLDPdIa''m+T+2L3+[b,ZIf<FE1pao$fKO1q
	^8eS<ej+lGUo\SFlCE*#]gXf!NrHUdRO3XJ1YiAmER&bj=W]XK&?9&eo_>Hu*P64?>=ScE+[mS6TAoKR
	.J4qV4b>4D\b;gtLQ=hRj7(%.*,peCM!3>$O`n9Y@Nm]XkcRqYUI#"H8(Obf6INk.GMB<*WHlP`h:/p<
	5?O/A/EUS<eQpm_k?MHn6lqeBY.[Nc_(gKUeX(/1rK\Y*:1Y,^acG*$-(f7QdcLN"d$A4?bTDDHmS[%#
	fdSbeg0GR[d>P7/Zc.,UOML`l-s#jJ8ZKN/iir<`DIV7!*ToA7fA*L4oL(e/TMDeCN,35!63\[-ZY^1c
	@2gm_nbkoZYV)m-a5AY,m#"Hu*\:KRRb8rnQ;cgg"/j9MS9o^:aCCr9WFUCgp@K;FK<gXF,%nr:8(EZB
	UlBI,s"go]jBEb,htMN6K#3`4DIL>"jTqA<_sn<(]:6g:[(;`8&eJr9heY`iN]W<4&ZiP_O9b<_Rm]AA
	kpHUm?\P^78_?9N4bLFD>(AhRrLC`8&qN$jHhs2Cp[;,cmSj1bL=1-6siX*)&ga[&/d%()0/#1_^E.rD
	:T9V2,-F<WE+l!<2s!s8N*!rrE*!"-G/8"4.%ac2`''2od5Iq14_a;63D?1E(D)Okl:]#CB+>:E]*mt1
	&q=#!!%-AW$CD@cKV%K(]\[Y$3A2S!6kcLcaEgrE&"Tt!3lM(J,fR6YRJQh!6q2;cN!rfQnE+aB5tRu8
	.&r/~>'));
			add: #Length -> 63371;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVMtMjGC''GqV^lY=(3(a5Mjd/-,P''>Y2iScuC+/!=KKr+]AhB1("Q^>iiP@Z@Dp9VD,8k9*,MGJi
	P)96K:4H@%]ski:I#B@LlfY77JV@a3q@rAr`+o[$?,SX5?q-NT!]RW0pq6j3`pf#.gl#X7,q6YM&:6jn
	%U*jmtoE_TAhU*DAO3l''o,Z>OhujF*)r>6$8V[6/#^*SbB.l&T(=<Ei#]ZAWo(Dl(f`ZY,.CU[)].^E
	3<eH0#G1?)7apfOd9cqK9N*4hQWp4Ec!_4eTQ(`Zr/tkPA>M(*Bgt,E3e`IC<u1j"=8JS*bT&=eZQKS)
	7ciniE#qsE0;,1ha7<^V:$nR=TE2RTDSdj4!!ACRnGYU9''sWq;ENc0b)]#r5JA6VR<\kV+81`,b5\9#
	%IGbgEc$+^)9\:Wp1BJOMq":iD:!#uj9P!nWO8L=[Iu;Vr*^N#V,BAk;#W/4#l&"6mHA.C-oY8*iDuP+
	<Z>#,/YLU:k*3L-C3.Kg:q=dCAk&ga;#Y+C`Mp7.1USh#XSb;H9tPe+;''4&!l:=Y"Frr6\e*ckL)f-m
	pe>?D`]@*![m;1=Tedaak]8WT@YYoWUX+!5T5Kb>d<OrQRX3sW#ft>H3A)lX1RH87DZ4WX*ncg,Bak^n
	_f[SX)1s2l,PtEj9c)[''LfT]^*NS7L2(K]/@XDL4<[H3B!UY!d>5=p4"8o;/>eu#hS$#EK3Inm]JDtD
	hb35]J=d4%nA191;KZB9ibafBX-W3pKbW8jX29!)CLCMS-nr8F`+QX!"Z9b0"D>$J1@;_+:)EiM<*YP(
	)dg2s_0YlAFeAhr(GAsE<%CQ%KX<1bkKO`r7SN<#R_K@bk&_Q(#oS''u.O)>,UjD<6%`l8+%Z/ul=e]<
	j"+n;1g!@*8"3YK:`>otcMh:JbfD5HNG-l?=]-(+OmRk6(;Hjn%U*"VLp&(&<7QN#%0Ir#''mrm#!+4H
	C<*O_IpD=VOp)*c^2tEcPMETA3PJIT.cfiT.b0*Yc>K.;67J3q.3k4\S52]:(>Ca?jG<9_=d6%3!K*5b
	qR&dTNI*LfCSEn@Xep3n)Yacns?\TO2Ke\;"hh@jAAuinX.nTLtoq+D-t1jY!)fOk,i[_H07kjaR9ZD4
	#O73BgL6=r86m[f;]tiZ7%p:1#C/irWUKsSH#LhM1m#''''4YPk$jM&?>)>[2Un*(onk2&RY%r$:T_5.
	<UK1qW08BD+nhKfPpIPn-.ri''].)2"C]HY&K/+>oT.n<\#aJQU4.%c`]Wa@:QWa<]Tm"=fcK3E-jQ])
	jE(%ZSd^Rk;F/''9MZInougrOB/0QGlUuHguW1(\]t:Ta:+g>`MH1D(VMEpe$jB>>@Ca2B=Gh=4)DHhq
	onh;71AaAp6%h5j(Ih,+j$-Rs3&Q6AJ7,rs?jlbKiNfIoF*ZHa/rS[9>2)<PIs4aP*)4b%5^h:NLQG+9
	)lLDQ?nm7sAs^j''?$b-ZTUps&ePHa1fS/qlJX6^Pc;"[#Y$Qc#bb4DJldE0#,Z3r*c+Y/n5mK.P"#Oe
	+hY(]Jb5gW_AWS-cV2q5M2L%r@;lY(t8n,)^R7uaG?Se+hnoj&0n2^o!`cF4/Hh!D2A$7ackF=oWW$K\
	W6AmN$$)9@b2FH+!t''H=n%2i1GPP,nME0<"BpoEGC2t(5i/<E>s[?O`f@<Ze28kP(S^HkfHnoP`_WU=
	JAr)JfP.BN/q(t[^?SN''-L6-4/oDC^&T>]FRGRr[QV;Ab&&B%><FXnEGb13:6$aK>qB?dRK!H]\"\ah
	t?GNb@<tk"L$/5"^YeOWBpPb^j%]E$$W"aD41#N]X0i2(`#;9Rc0YR["(O^6(EEW4HWKR(9eSjD20`Ne
	nr]#;sZhfhP2FHVDS`+JK$khY*8fAp:c-r;&iD#e(ZB&=Y^Jk*A8,8]V6iY(uaRr34fn*9;''+,5(Vsu
	)KL]OFf!+^m)D3$Tg5h)]g:.\rj>.C%<f))TL*^Ti0)ZLoMMl1bZ0q_b4-8QmePBi^Cct]*-Bgp&<SA@
	l=[]$fj>3;*BFd?!.QRoh>0`uSE''8''(R#^/fd5*H-_K)d/$JY9jtKI7`k0l;I!n''kji\$QYO`Xe^a
	H?l8L0^<(?-pZH^\VPe6%).]B0eTM6*[r<%#j/mT>R=qlDqB%79l>^Vhq5%E:ITcMBM4aQGe?>%AjYcl
	gqRjaoK:OtBQ(\^%/c.dZnLD<%>kHAB<]Pqa;K(J>/8*.m#_pO''OYqoenN6d&!445oHWSKP-tDb.n6:
	q]n''i''?uBn''&FlFMO3D3YSMPf7!*Dd:!#skm0jp^ZL5_"V6*:P''\J=KXb-cfJ]81=j#hlTf[#_!C
	"rR@C3N?DCLLS."h2W@,1I6GBaPETbUYR-Bb-_0#d3f$<83jim3f(1U738jnPi*&mct6qlRh,^QZp0:J
	i#P+s,/P5aH=F*$)U''qY(?nW?@#-MB2s6;Hira!WN.(hE\EFS.+Aa"f@*.<gC4On"2,RD#AO3I@1''#
	8^`672:J/?;+"@o?]lN7:3_FLS^E+u[R,B;Ju93*]^RO:Z^6B;.%*N?DHHL1m]i,S2o-)1Oq#I1Et''D
	rQ(c1>[?6R2Z\Zc"d70$n\SR!9-l0T=*NKeX!HX.WB<:]SG17Wm#g",!Ic4W]$5qI9(T5q`A&?E$.;F5
	L#Ui]o''GNtJNgB+''\D#X]_r]`XfrY_H+;"&60O!Jgq+1IN,0?c"jgWI#5JeKB/4^B<QA@FL(p2Si9>
	jA-$-NKZ?%9lM9HM[-j81QVT)7#n!=^]n<+bd826/''o[rd2Q''#*ZLee^rJ,XCEPj82?54_$''l4"E=
	kI[;9CVX1YC6DM!RYF''6kO-=.VW0YBk8''.%p91GRY5"7M2:fj2W_''=YL)f''ofGZbE.8bn3I>!r_2
	($`''D0J%*jCPDF"7R,qVGAb8]fI=d<Ndeto9rc4o">1>%Op0W>''23m_a2#f0AG''G''A3!*2Ra[C"^
	S\NQW\0V/tS1K?*OB+\!>]bOj-pB^/Ef#JblN/ceSSn30K9#,cg0&e>`a:]C(/B>F1lbTJc1L;6,3bFK
	8L=+i&)=UdOOlo:sb=aA3X1Xd@>^dP8,-)Kai,>n"99Y6DV!m<&U)kT5K"4tbp04K>NjN(-EX!hK1XN(
	k(i-`^WCCVYg$)A$]Rqt/,T2fK>4nWgM$>dhS.Hm@<N"Q=S3X:aLmS=h67UQ3V3BTGJ/LmeK)3g"N.!g
	.5/''&"YV_?#qLOX&BabP,^=Y^c-nl4:FkX&_)l\!2cQR.^0sSf"N$`)0$r*KQMa#/`&lo&Ns%9''-K0
	C;a=&Lsdc2cqBX^)DZMG2:XoAtA6RZ3$]4.=LkEk.&j,>+^O7.]o4F<ieFd4)7>QHo<m4c\YZmMUT6!,
	Wl,R-.d`E-frt=dDJ?!7%4)\in0=UN/[:55UEVB0^guq]7<bMgg/Ogp@0+mQkFqg>E]jA<,X%bd^"2Jf
	pGR$ZrZYAk3N,1sThTQr*p8i97,,6t9$p$C(_\pN^0X?j;]K:g98-ZjICp59,QVm1bUq(Bt0^P?%EWJp
	6;/@A:VNV7eGGoC(H&pE(r@il.%Heh@B*70U&nC+[KthV;eeA%E''WnAWp8pk3/aWEUjFFNe-Z7lg^7C
	2ql:o`O$cF59<>TnU)8%sPV@aiF[2>-*LUh*AI,Y:DGU(+U*XBkK@f%^J.U/XTW?Cn''K-E(!$,)/:!-
	#dihV"(BK=7e&8breso<"!qcKc5B1/`4NRElqM?GWR%i4Xr@!s$HK0)WlSWE43JoI$(e%d]`Ba"$C3Kj
	%YY%<U]Lrr"XO+u.M>E[a<&/+1d8q64qt/$7IST/.TBO,J?aB-mn6gh(#.[ZP^oWD9X=+qAP)u[5q*M+
	Ao,qh.hq5="d,Z\jB2Rp<#\YsF_<GlUmIoP8nM>UKH\qI3@Bo7B[T:G_6.n*+No)1?#>-0ktq1Hadm+U
	P3Ie;_t''q+mMP@Lr,G5&aGR*E\(+)AQL[CsPFkiUr,#PD1E6qd--@dH''sCP@?5Y?Kd*eA8kN-DYrh9
	Q!]$npiB!stB7]Xkg,eF''b4olZ0fPjYj:W3_\)1jZ;"`?-E@A0''P"k,d@G!Gc*Qma=QJe2=(i#PWd#
	NEQB.a%)g!.a1HTH-U-9cAPLU@jB>;b7:GcuB>Y;Tu3[(JJ''>:]((%hTip;)'',um8>&F`B3CZBa1K/
	=bl[\n=[5pYU,0po\A\rjXO9<''3C!W%gF+C.\[,kH8u0eWV!^*ZXe\@!c536^<c0q=D,RTXGm0WJUB;
	W]6?j>d+r$rF1&Y6b1HK7(5dG$iW`LcJ."J]pfe[dU&ng,Q4,4g9jYGk:G<h54XXM58#PR<&-HD%IGfp
	J=3NlL`mW[fG<.]Z)@Z>W]-BDpK5u7fT$mnO+Q`!h5!C]<]AWQlu''k`Vdad[;OFaH$+33*6\60s+%Nl
	7XV"C[=AOPE8=D1^MsE!ApODR6tU,iE/:-;[C)J`1k5E3qC)@g5*,Kp6(''-Xd*VXZ]67&iLoVeFL/p8
	\=@s.41sC_u_Ktr?h3QNjT0o:o&W@at:jM&@Q%DE!D9\Rm:VaNeg2[X0ONcHH!dK7gf_fbU"`kHo#*#A
	-Vl+Jh&]mf@VW3*62H"@=t[ie5]cD2l`#hRg$N(?_"r]#W_EqR\R4G:22P(=-sl]bN1eFL>9;O@-]YtN
	j`5hZPciij)DMtnOU+k).8!J^Aq4f8>=[9GS-T?g46QVT!:)53!7_JE=<suSn39-:f)+"lG>*ASaf!q.
	1`@s,U].G#e)SEB=YMd&K;dIFsA%TbDX23$T8uPeiY&I-!YS^^iR0t!tTX/5nR(OeS1r3YmF`D_@pE)e
	+orGj]\$<''$Y!b[b.ODWs[NnEi#/E5RALYo@RE6dLKt>PPkh.&i+(W"ZU-;M[9uV-qtVM]blV-3nE*D
	:No0c.8>(3U#mV+oT,@W<=)()Bg4_M*OGp&:rrH+jK"D7OJQs]&HOG5M3TH8b8g]hb[=''KX&8Zt7:<A
	U#[<E+p5#8joT-?SE.9j2mu4/*j\;Tgl9is92@VZ%otXJ<+6tqEU$a0H$J9os=E;6WKCZrbe''5s3.0,
	c`#sP;P_Lnr.^1C1mD(''Y=6N3k2hd6@MY$W`U*J''Blk"[k3_kfVYRe`9C#E$K<GU;d%.DQ%gR5&e^K
	O#iQP+!,Q!7rmk0foK*1PdjlE1WS21]>Z3\0@)1U,9,AVO.Ignq&^cCSJ,Q)+<787[)ps--6''>$gBu;
	geU[RNF!68oi"5>Bod,8atU5+V<Z<)dL!sYI?Z>u"*G@pF8''+RL7QT_g*q4ji%K!%k[@lVK+jlUMi@7
	,Mt=$''!KER2bge&e2!81:<GJpF51,G''76]`[H5M3Sq:u-][''W''<ZM&1`%S:08_m$So8<Vqp.tr:]
	O@b2H''MXZ0e,50Nn>$f]*F8`>''>VPTifA],XUGOdE<c;a5A$R=S_8]pHE,*S[8PK)o/iuS5I6%jS<t
	flDDBUFH6u+d*Ig#c-<=AJ*2#FU1$q^2>@(="o+aX''TtQcd2+lhT<_*oP4b4%H!\q+r>1p8K!O(QrfO
	R"-!DM<V(sXW.QH$`^=6VYZJ0l>1DWLn<r!O''D!&0n5RMnp6NqX0!!/X!fb!KNq918VRILLK*H)qCFc
	m1*Nb/pW9[7qO(''qXO0mK#!<rT+Fc$stZB(OC7[6l0[''ct9ECni(D''8#$;l5C9k"V%VnbB&5^S.QS
	@3X[p6nm7mNG@''EQ8o]Vu)?>.m[bt-7j3q7/A$;?''mR$7t*9b9;dQ*''Bm3s/[EP5F7$;CN:e:HVW@
	lRA^,[^%cK5p9*C1K%85Z7@sc-RH8M(?ifKEPtTA1T".lc"=<dIS[0Q2!OEn@%9=R''MM78QH1rjW62l
	`QT,*Bpri)?<WrYRTg5I[#lrbf''djE,mn]lb-63uOOMu92lI76=OuS(hA3=AVr<6UVPC9l;3$&CB((E
	@]2(C\,(@>"gJsLo1fSbG+A93*K6>"mC9Bg=Z1@AW=/)_X^i.kUYFGAG""5LuB_XIPe:''puVXN[Og#\
	K/5pta45j<LA,a"p=]+sdag6WQJ6%!''=)''i($*J+6pO(oU8g6?_KA6d@rOh.,p\=47Bj=;pfYV&YbY
	1oc)/]$eLSUlgdrJMY3T<gl/o/4<*QQM*L@MQrq4]*]L!lq_Yi^l"$^+@#m"AelpT1V,K\`173HAK-IO
	&DVok!`:a7j4[/^Uat,(MC07^&ri4Zj&)kBN%FeC7&2SjbuZDRI.!013G=35?<;UWE_`,Te<9;hT[2QN
	ci4",ETRT6^a(CkdhJHL3Z:8I#)gR;fIPYYl/0@''ZeSB^+s@mV3GfZh#WXl+%l]nZD\@jlG!Mur`njQ
	1&d''&0''/!U5(Hdo`+L2`3g$Nf;,5]eOH7&7kAToYeBK;EJ"+/o+aUTo"65<:iiLRe,X:u\-HoVpqZE
	HEQmd]qX<Z]*tN<1do[%N#FV/HDu9NH>M2PIde(bPsf"_VI5$Q<Zn$U0''5.O=!Y;UXPG!n@N+=)JN_b
	t[Q8n2+Ko6m_&o*`)L`''#)01hB3K$Uj@!IjdhJhE5`LD=Vac:)>H0h-/W*OV4E[d@`_0u2XQ5AZ0$Dt
	E,_*L264*`)6D`u1X]lU6BSY^<k\m%.AXYHjgm/+D2hG''b%LP2^/@2b>TaSQf=43s02[8P!Cs%"ZGTB
	Xb+B\lm*%c#aQk=aB\r+8Ju\oGNXju"oa.ES!O2''YZ/eu/6gVKrJI!?:V&8DFQiRYh`6)e''64ji_8f
	3l;3$o8Q./5!''>G@Y/Ane[eObc;JUoP7U!E%#*<X@m@)2K#n_VK[6h[PXWI+LN363ulee]2+-(N*jVi
	CPjJBmX9lXnC/V/.,D;,f)e[`=1o`?%QLL88nqtAcNT=mG6&*<6D&eGjH@1VJMP''@gt4*3haTh^fC(V
	!FE1g=2b;6@8QJ!Cq3A_!*`u7M_oejIsr`._V*;`aM@MM3Ls)`*:H-*!B20ITGs_4c`tu:1`VO_,Yg2n
	Z!!C\2%,K_PgVH<HQ_oEYgB!#N]M]t"$s;AW]!._+kM,UUfC;E!_<JMP;qnXg%J=d*XiO2[@3FR.-s4D
	hoMYP-;S9fmEgI:ePq>j.)BOJ"\`HK!n''%;!B@p(=p70Y<h!bB0O$Np0WJ_nYm35j*C/tf9D,CQ7@tU
	1b6^5iG=H!_lH,R4JMcEr*6oCk@fU$E#*:X.8H9q[#)pJ<A7_)a[SlBHYbbJufjEcn@)8HC_A8-mVj&h
	7f`>FA$q>rj?j<h$.lP%cBh%-hp4-UbJ@VQ[Z^Elm"pp''f!MMlt<Bph&@-[r>4(hAMPdX/N!Kc[M<6g
	#c!I(=k",O<n`M)og78/f"!Bhni#5)G''U:`;Zr_tLMa=@JX6>YZ&^nG]8VBo]U!69Gglg-nlc4S4l-L
	jejY]"<.#r]C]fZq=?64Ba`*04C$BkS.(0kkni@ho5(:kP8$nI-ZCEO4GoGY!)G`Aj)O'';`r%S/sY^-
	V>]-,:LS&$^%^="ZKj_:7DF!bSfnQDlY8u"4PJ2fibC.#BqLCQr+&\aHSM>cttH1Op*&jq''41ajT!*G
	ohZeiZh_453_SX/%Yh*=S)PtTnb_6d_B^92Y=I;5()#g9AMKk\Z3p-nQ26TPnCbY?;U^pPoSYHVLY8%B
	7TJBs@/$-O$U&i$@"X1.;.L*G@CO/mf_^k+f#m*i9S^l1@?sd:bS:]5J-W\H]ba3ZP-bOYC6;S&''3I6
	j=ZJm4mC?$sYVIpE&FWGP1[rQL!T%qO6fn6*Wl08G,0?MtM@"5S[>fe6FE7`B&%8;[-E*G#E$fW12Ff%
	50apu$p=Ng&3J:CY1,0%lD7!^O,0\$jbSZdV$MttcXNB0!V]+>Yd3cr2]r?6H69qi@B;IR%HHsl.Ur?*
	%;$E`)^I#35DC@0fMW"mgGh_>EGEA''."NFA\$qrr.bUO.Ie,A]g"MmbMk*R2tOYld%2F/SAePsJTM_[
	tE<%NC7RC=:&0Pmk:HkUN)KlBl0UfSmAQKn6R;q<.H+N8FHq\"u-K\TM:bp>(f/B(R&ZGHOgQt;Hn7o4
	`L.ZgS!+imH0BKNTC;=8Hd8lPG&aj0c+:C+RO/^U\Z1hIA0B"r"A(]qtu!aLe>`EiQ:Sr75(&VI)qO$a
	#''1LQW>A&TT/*?A;pBV`+/7Rn)(^bWUs9#/FO]7)3XGB=JDZ(6+A8oEXuQjYg1G0"m;<k:1ijpZM31X
	=2q.l9j^YuR*rE9C.-@PHUY8t<`XO;RU6cU;D.hRCae)T!0/d\TeM=.n1`FEGgDX7Cm"kDh+7E%!k_`#
	DPs(Cn&Ij)DM''6"3Z.Zs(`W/oJ[f_VLM*<Id33GMMgV7Y7`Wb/k''-R^p[8_"i/1G78ZuX:PO+#@["U
	q+0iUc9S^$PWUC2S&Khkgn+9K/WS.TX^]r=Eebl2:<59PNo4kS%]*@3Ic!Z?U5b7Ulq/!o4>aZH#]3UB
	4+g@:`ZTLU=";_81f^%k>*IrX''U$F[5FL.T9SQq5D#,*BQF#LYB"h0t$^h.PAuJO=GmbZ8+@1*Gf4TU
	T#,^lI<[c@%Z9T+F/!;3PesFGSSk4#dXj'',L)<8%sEliq9qm5(l_RSVt[%4&-O3WWRU2saVDj2U!R4V
	]dgLEgi1W(L`l+Gj$59?1imGf7E)"[nt3$p"G3RDPrh"Fo7**T"ih67"r&"TEQbZ9[]HRu;j"i*IBL0>
	:Y7iW:Z!iU8H=[S[oWu#1/Rl+5iO;26(bmZ8Z;h\\j;5&4_->Y%*roO7aWMA[H''`<N%&''R"?+t-#Th
	Gte:=8a+i)>j_?`e)''tJ4=Us*F[5!F"J"2-gn[H:!bF&AQCD7kkk>7jA<,a#Mr3?''Md^p3K=krJUjr
	d#WkrC6GlR7#0`#6Tl-d6=eUO_bMNcO3"QA,a.ab40T#E''Q*n#i`K5.?bmM"aZTTc2P8WW-dq%)QG#U
	kU(/38T-DkOAd$Wd+,,V[!Y_<M4@q>mX%Y"Up,+cV''[kiQ)J_e''YeY<e.KlSG:.RbP8j:q<;0i=n0Z
	(5L7P#NW''no#!]$c-nZ6Ld''c.@#I40^''q\$_\meBu/r*:q/`0UhjkA#_`LIk*S]0J\."VL"N3^%u1
	K:0fsJN,^Y2<_qCN)gQBRS2era5@M0!gcr`^"''9+9]YqqIoap]]K7tUD''@0^<JR:%@6n\!M3jJYblV
	XP$o1t.pdL<4u)j9gFDfbcCRr3t03IQAnY@k]IeJpFL-Hc7p0[5.f?IYSioH5W1g-u(pMmW^1@JS1@MN
	<qWIF`/T?Q(]bEAU67hP)\L1CT''?Zjn3@@OA+14=X23-beU*-`#pbuH/[k8;L''b"Zh/lV\K.LnNWSP
	9BBGRS!"68$1^:a*WZ\Q)pJ_PhDE+I$.!8qDq^T=-Cs#;q5,cI*1`CHIcE"@#"W"%2]s7kubli*YKpq@
	I#cd]\Vc&3ZT!_Q\oq?4`($-r2ei].Y)U9+N[6QS+l/[F)UuRl7jA4''3i/TLqf4-"C&N\;0OTWgU!HD
	''=\iI'']SUl/HS(4$4jZ!]GGa,fgUj]D%SE<.X?''Gk4Aa1mL!0UrG(/1k8A<9QM^hNe<Vb05=+!A83
	;-&5F"geg/*Nqof]hNBpX?%TJO''VI!&Z![qUi2T8dE"m_g!lm?fSo(tbg*Ld6cm0#5s=T(lBbN]92^=
	_!QClH(+3?VM80gk:''\`2G#Eq.)=Y7#\;;IXn2t=9VT.JbAQ?+hN)qG<]o''9#I-s*..:*rL;Z%YDC0
	cC"Pp`^a,*5pW8hiN1`\Yt&794G.1PJ.,E%N))"J$[Kr/K[UPOT''G(l5+''J;h_UM*]fRUP1^%]Fg*f
	-3=t`<29Ym)l6+=R&<P!_e(%CS*.kP>;Y^fD1W`Jfkdg(NfQhGH]rDT#tY<%*W`L.]egoXjMGjYd[4sh
	FoeB=*jpY:88e[<OWbUp#E:F2#hGH8!F=q:Fus>%P;=.PA#^3jQq!"^Y=nUHD:i/5TkZ.<aU?,@Vm.Xn
	Zt^(=$`<.=5a]S0>Pa0nK>RaH&BHi#Ui8ldR`"K_1?8`4+?m^`R1O!pSXb4jEcY2d_*f78fK*DY6q7GB
	;a]8\U9X+\WbXE3)fmr''*!bom,Yj!GVTYI"j5Pg4fbMSqeAbi?6g*Be@H!_!=Q`Bi.S\6,`XG=WFC^=
	r+tRF/D.8OLi0HBB,etu5nB%/IZ3UqP=E+Usb[C[Z@$m7/,097P0a!oeCCur/hWfVR1S?FkB-J$%+e%>
	Lmt04>NAa3]Z4(qf)>"2o5YH&+\<T0!e`2"r2o3FTR3Fgf?S(B:/14snqRW/g^-Hs(PFRIiIp-el(Ans
	EXQb$#XGEYPAo.V-W85`:O;tJY0A%BZr"$3Trr&C[A*n1\O.eoT!H(lN;8iq^BKQB\cp84-./_G"7gXM
	0ODF=Z\!inH+BX=d`W`0O''HNT46=DW<#:dZ%N]1K`4c@qo]RhKW(?5<''H^P28''''<;c''E#\<djD#
	=''8QXMo1^khrF15Zk63BJ6X[@p0]arB22gahi1:Rq$,Sg_?OkO1a+@;,>''oN2ZuMJR:)\iYD&V<0nZ
	/DIBY;(DVmS+FY^!M^b.#E=''7^63$rE^?5p3I8iIX4s[`k,&%0N!2huqap%&Hs3?F,b!Yfbo-:j%@Tn
	#<r+MC3uj3+2H>7@W6l4\lUiKL[_rWP/[FJacB!_Lu;O$KVia!6BoJF-l:>rBAABG9oPj@m''g^_(eWg
	p&o!#q''MK9YGc[uQpTc$nePrY9aVal=0k70[g5?8Ytt9=D.S9+3Q(3_GQt#j6$#uIm@Ja50bHMhN<*L
	&B<]VqC!%3tf4raGAsZlEU@$"fkTT;W\m\+Sg@b#[]VqeTIKB#;Q!^sq.>o>J<hU#sP]).]&b(>[*(/b
	(L"=?T!,9AgicImQlX7GcBCT''Ii/j5ld2_4n%p:$Tg&rPB1Vs4E9^-3)D]uCGg3/BF7rnf&WN!5Q<7k
	Q2Mr4.n[YdTfr"N$)TXh_7QIRWjr3!=>*-U"Ff.rQk(<gu@77YA84..>GOBI^-,R%ee6''rTjd[[[eRi
	2E)NaNL8CuRaH,U+6OS,pm_]:(O;e#SV6nO.IBP;ZiNm,R@pF7;fY.W9oh9-U;C5)Y^#*@ca@"`.1FF-
	#Q-A-(co/V1"gLUk(^.`grNMO[$6O:h;=aVnS@?j;@(_R:U1#^:GKap[GmDA<\62?O+0V3"[o8mnJ?0E
	RVT@?CiKe\XR@NHCuM)>%204ePNRb"lP''s4C?/VA:Cu7=oXj6s:_hSUm@mgE%glLVm4G7_\$O.pp;Or
	ZZ,\<7\Noo.P_;L^!DL-''su6XRfO4Q>f*L(;-Pm''<qCsgO"5RM"GR?irF=M4\OPt3J+q5EDd8IZrsS
	B)Z0[!0W4geP#5p:PhZAp"O_St$O+(FhdfnA_MX#U`XIA8ku!)c`_*''2;</RS+^U[Q?Ti)h=d7mdo&R
	bLMlU&h#-Mc!Yga;lGXb/\jO&kVp)_$YM"!`]+R6aQJ:U>]_ON0l47NElXWB(;@SS(kfcA/>.*^*n"5:
	1;h"@%cfe8BbI;@%+1KLgt$1k?"[0^uZR55s..+L8g((/Sp9&,sF(j''?7!;BbaO`Al$A$<g,TM8hq&/
	86nc.M\*P!IRGa90Yqo<>M%&WfO((;7kDa(V6=[a(0>8-_W$2dr$X2do_3@%KO=/uJk\T"D`M*Efg,Qt
	1B!)3dGqPc.9tU,3g)+4?&\eH>*>2Ku2`U,%dfg/0;F$d''<JkB43u<6f5cN:nK21&W;X1\b]X'',0N*
	R07k+S"O-n5Tg.f)$F6NXiM+Z(g1$$"N_R;N0g''[#;Ht0C2.>Lb?:1\ccX#$ri3E/=FXFab<VgFkY8@
	\ljTtS!RV=a]5sYtlkqNpRhY)mEX"^FVjKb%,`)i]:dg$t_RGF-*=rhN?R`0pkp)ZP#/<V=34,!-?3d;
	9ns=%f&*Y&q#Y:J:7Gq7262b-4W''mM[68Ri0/?iA^jAR[A]NA55da%qm1"/Fi/900K<PR\^\T&(r?F2
	sC3<D54qJKVa155hohP4NaC-^8h@0SK"+A^?(UWr0M_?3m_#pMG!1FU.kE#*bBV4*4H$O2m4YQaF:b(b
	ck=QGShA&!RX@=a5B#,j@r0Rd9.P.+caqT!6Y$jXRYhdD![m''0)ORK]F9?rP5?$?l"bBMa_#TZT?0''
	BW^-V7J)7(nml=>EP!KO?"=-#j@5mOf0L&!P:&-5_piOTPUQsJRIlILW^-R%5XXX:"gf^K1$ih5@hEkO
	-9-8P.jS_@O\\/ZL,aq/c4(BaS>#ufOh''!:/G)W7Y3IE;j,/fV+=*#D37ARk+cG,A/?nZ6raR[KX(gc
	dsUn5"up4&#je:"_Ua`UU&Zt1CRbRF(*N8er")426DOdUGsF%c;mKdr5k''+$1IraX!TjuilK3H<IH<_
	=Vb:B''OY-NcQfs3!#E<TuSA*I]ppi_=?^$o*7.*KA&YdG)3bFKHenf]Mc2dIOOCjj5b.`<$Npk,%hf8
	GC8p6N?4;S=tJ"?i''N\CQGH5AS`L)+b\U1g6A''K):c2!Yko"+*FU#a9-=^T<6"#%V$XN5aI+QpDD%%
	"Ke<Q&tAnO)JW^_r+@>C%%_gJXhp!%>X5Zi=J)0Q`rH*QdpRP3t`MCLW&''U%Q!hNnVM-36(>2=UD#.e
	B2C7AGINs4EX!lTa''N(DO2o@J\Vu`3Gc=I.eu=m5r[`EjFp8dqJ1(kG:_9&a;0a6R#>htgigTo\E4,`
	_.?mVm1I)uO:$LoYhQT8pE\/3@"AJ7*At6nGXCt$FEVkYUm;e0)C3^@R9\=lW7O)_kEsWH]1^c6V\R8+
	6]T*j8H)G;aEs;bWUP:5?8,U*"G7`U#MLU''NFh>Qf[_3d)M7f+3-0C[.d=[q4`=0P*R"9t!&]*4s=p-
	@f+d#-7$0I7"Ra;1QW''\N59_S-+2@!cH]on(PYY;F)EWuiC)gsDa@?JBBF,%6+o%P"0aMbmTh]c1a<M
	FW]+A5.kh2E51oQUSM.RAU''+DdCmW`cLY$f@0ANItDCr/7<%D-$Tjl3FVU<X)MV&?/U8@:!^-0VW"\W
	Tc=.!IDp+AtU[M9rK5U(oY2PLF=&Z8<Eld9[g^h+tnWEJ0*5l&-[,;#&IUGPcB)J!mq\i!!tD:`Ads^+
	]U)uVLp^a8/Q!4g[Z(1D*p/2O"g-ldiqCo.%C=+ap-3.fmi]rFCZr8ZQ>hp+":/+OmL4-UMIYV,%?Y/6
	"afD%b7p@K\?mG/uG;j#*3H5C!tS:?cncOD:hZd[0_)R,FSn07VEGLbWs[#!SFc>gCRI()lo5l$PYo(''
	JYo*4=-^/##(GXEs<m_A!L_=%qT0=Da_>ZKeqAJJAa^k)0]?AU-9J.H4^aB#++LL-''>0TN-5fX/]+/#
	)l"#JZfRMR82IJ5AS2R`Zp9:_HT@U/6rVtiA28P^E/)pP*?PCp9Ji`:"32@3''2;Ll+qp/""!5cI.c:W
	=<Qm];DIUO?_#<<<g''$5n87[sq7''r''?NB`+1228V0UU9jj&^nd''E$#orTkP0rY]PJn=ZhQZA7af3
	Hh2h2fh\G=0Ic0bb^e+`.];k4Ul>ULn.$XV^nGO_(f_P`(kbkMr<oMO`j^a^^8PuT$>Y:%LFH=5Qt''c
	j"es/pr9#`]TAPbG1ub!TC"qRMJXh9X%$X7)l=31&-DE7r30j<&(M"pBk>auUS(N^UWS?$lXEmFo=gVY
	D+fP`>S,n]I>2l6EBJOLXP&$3LC-mJY*A^NA$d,La>X^.G$W/2]9S,I)Mcb0iD%K*OZt&e;$oo%?&(JB
	''d.@`7GX''$k-E*:4aLUd(16,pi`YgV$)&_0MD%mjD@#^ihgOl?8,--WA`Zl,<kAS^KV&u^#;O:`";F
	/T!,TD\j,;pf`F`Su`hZ+FoaZ>T:''k^?;2P''S]8U&)nfbm2>J>br]#_Op0=XT?+N3OPpaP`*LpaL&G
	7Zq"_/rm5N,%pGA;8/P?(*n7/Ee_fB+-Er;(-sd.Sn(Og9e`p\?CJl$!8rMNd3cZdZ42Gk@B_\MMNig+
	J8Q?_cJKlne+=9+#fW3b.''''L@6Ah38U0q@3(>d9484VP9na_b7s(6`eY5daY^F:1,lIpr_@g32hJ;5
	.A0`t#qR\I9WBW!am''q6U\:_(AP#!*QNfm_#''ff%2J6a*)39.Su+7k%<Q%r_l:9g''\@JE6#Q0gSk^
	OBM&!\F55l!_i^W!RsHVJ+N2]miVHkZ5ciq*A%MUiWl`R-ioZqs7Q$i,;!M_QiW3Aqe:,2N#=%6Jb2L1
	1!''LH*XbHr!;H3Xp.?CofJloXUcm6pZXTPQ(&Bqmg#_5,-tUfn0bB-s2[YE^+SNF74YIK_,gHBtbW^I
	l=]7d9SoAP`_+028;#27k(erHCoH^h)Y1OSRcn@,PW5K7Y-uk8*bhrIh</@ia>srfDH3m5]!pQtg`q)\
	SK>IQESNV2l$`?eXMq?q:O5+%KkCjpc#,GLZU;.[ZTKR6uV^n/U7f(6#_@#L&3Rc(\oHl:`JoZ/s)H)N
	i1DC(bdRqSfKPPd22)kgNj_dfljd,&''E.a$?S3_<[2`UsL0fT9Hf<VHq^gB:ulq<DqS)r1Yhq0#j0g!
	i2n>)&EHm<pX)/thg24<5F&En;,PD1UM"jsT\j)F2>5bU/=+jboJ])@J(+>k8&oebbCN^df?S)/+C8W@
	NF_Cu,?qHF^K,]/-U3`8,NM,g]I''1]''6mZBL3;%\90k[^[ZLJ''?C)VZ\9)2JJ6/Rl!t@q@gEi)1o>
	=9&XeV6dTse@mfa!&8Xac!<"r&_b#/]V0t]h"pH@HaI&WoCj@%pTT_8!GA&p3jK*+X''l2F0i4KMePur
	nqI5_03''J4#U,I%gR;!h/</"3Z;G]X`\Ceg[*36Hn?ki?r8#&q^;$s_*PQ;mt''h%koDX3&iUjrFeFj
	:!2abu''+8T`/Ve@"3]1&q710&F6gj9YVYmZBF\^N"od3:E[Xr[uuT6f3=p/#/Ut!gc-]FRhAG`nm8%''
	8Z;/Lc.PF9We8U0HBN+@QBOJX6HID2#.%>+ri#4[s:*cG6S,"n0Ag8.^7cQ3<G7`]:l_jVN)p!K,>0h5
	sZ\S_HL$Z1^OG`?qdjM2cKcF;beV!/gRVh8.:_>IX8.c4KpLXk=sMSVuS#WF082a29>0l+bc$TLa;\mR
	&RI3-t,q?=`hNnNQdgl7*ZArJ-0RAPn9=)D8Meg67<bh$O15TqY7>7NO?1540E?77u3@Eo*]4:<e''ec
	PsS6u!GB3lY\ZBh-6`aPOB%45r4kIhU8J@qaroT$VegBPa=r-8Gt0B%GseBlG%$V2:6JPRm8/gD,hkU/
	4pah>._U!fcsu08U4OJcn.;F$P*0R;\8M]46,FbE75Vrk&^eBtS%qNE;iNTcJ)R*/`I"tn/\ZRLIloaD
	Tf0_2Md/H?BG9;KTX]!?RHqh$Z+>!fdp$[lV"2?*/3aW0&lI67?/!NGdDCVr\a?>H]A3)Pi.6T"]IgOb
	!\pW5NW^p^43BHko$FX<^8$_+S;\?,!$W#U&VC<g4+i6<NZ%>^aXCoSYAjd;5@^b=;M*l$0K=$$2)V5_
	6*T@lB9XjdB(ts&d0sPD=f.P3`<MP1bXqRf>(o!MOU)/c[:E\gXTiE''c9?L"@"fs`0S@`m?ba.]FjHA
	]jgX,mcsLF0k-\k4BWKsUKGc$`D2t5LbD?7MD8T9I@`6;\I5,Hil/h9\8p@6gMG7m#fI4.qkeZD>Fb3r
	P9)3dOg''Pfj\Z.1a$khZ$''QuF$Z54^.!!S2''<,-6)&r&BEZr:jL4r8k]RH_4#S"5Kr8"o>o80]#+"
	Dc;uUeMul)*lL*#91k\XAtS<IXSdED\%#e9e`K3eM.0j3Co<`4"<d-gqH=(BM66o6F/rH[`\P2i10>"[
	*,a+-$O0Ki%Ci1dLi^f&u\ZD[''noWfV:2DOR[UD]kB(:62;^*+6A(oCU-D`lel`0%UMWb%''23lW]`t
	^1N6q7b^dq!JcWY:B-@3SYVdXW0+2D$ee=\JR&MGj8N0S0o+A\0p11Is(qK43=KSLLbsimjV62]q-__X
	h$m+58/mFtj`A%H=*@roonV.5,TQot&2sqhDfNMNBoCE3F$XdURMam[Q,,DHe)$#B3;M/4&gnWqc6?sO
	?EO=_ApY]T=$)I-`d([#b*cq5-?%8f;)H(CWPc0HRP:C`jmE"fq]-fg>Kc%$FTR=^T9sd^o`F9+u=^F#
	?kstY?B;!VOMOAWBj=_9,K-J38GaW=(W]%\`SfNf?\=GOO<nRH''4X:59>8unc3Z*''2ZTHS^&:)%p$p
	^VUZ/7\JE.4^;-%Bd#NjK!''FN=[8AJa:9E=XEf&LA.:=X"''47MNX`?NM9`b]K(mG5KQ#Upo3n]A%88
	>O=I=<>QY2J?r)j0KL!s>$0`kr^$B7;7-]I8q7<U">`l,S"P$?/G,#BAtVoiCc*5Q%4(:Sd#N$6VjjAF
	Er:1#n"[Pk5U>_l(IJ?:j`M:GWH#8o#b%P0/)s%WK<\6:)VD%1iL%''I9`S5!4>H;AS25ES??-6S>H<Z
	XT.#IU@&K-I#V)M(M<:9Mh)lEhQ((WO71!-Da4(pX$pn"D;fE.^D92]XK9L]g(94cAUbMl;''j"@F)+W
	@iMbQNJRodTH+>Ij-]\\2uP?MZq#FhQ5YSp/''RStWU(Il*,HY$b.[3\3)RVeE!\MN6X^m<D515q5a`A
	V-@WbE+dT.O[UH%X(S?U)''[[d?JP@(,Ld%F+q`5aQsV,N8!A;7W1$8]3l''!iW+LhOqO,\81Ap`%eGR
	M*YT)AN>bi^_n8)Eu1C(:9m]6ED4UpBdDL4;CE]/X\_6mcn9u5N&YIMS.e[tdWZ0ID*f!U"lt3,JSJft
	ZA>''&fg3^k/9G`\KFlh*AbXQ;l/e,*Eh2`SSGB-9fN9"idn.V$S-I^unIF_4''QV[&]b,"+7E/N#+;2
	[q`Y%RO$tD=+%nBTg5[.bq1.=/fU*E%Ni8W<N,mI1D\,rYGLtZ"-,8@OTEp5%,"A5(eWl_PVTJ2Il3!q
	Zm1,5FiO5J=u8!,\Sp:Bc^''MuVbaY($^/Vu96`5#Soqs-"a+Yp:bQ5o>;9h0]P?Q[o5aW;!]H_D;KVT
	K3ifjJkI3QN!=H&;fOB;)L*ZK''Mlb&jOr''.gCUjF6=''\JAu[Hd!?m)Z;T2^V>r#hU+9u$pi!AM[!e
	/\;JOWbau#I6RuZoeh"A+XRp$-T(7Qp*oC%@VOL:m.d*WB[a#oXk$[]_G:5WfK3bCYl5_-N**#NG2M6X
	^-WEicA)X/cb"Y$mk#<TWD/_j="*[+)$mf`l:ZU3K*>Ge[Fp;K7@mnqNCD163."+5-mrnTe823+>K:\_
	9`991p=j<;''4X_Jm_PMsd4-HV$N7V.rpCPdi_VNQQdD2hsY_nN%8"3nK)?>j?oU)2c4iRmE8`AbA%mb
	6N"5a7Q/>V-^bSd>]P):S2''8[HhV?(F2C^)D@9hA$/S3`2RQ/.Cd$2Aiu4HbS\#%L14.#f&XF$P#f"&
	F<@0;70ToKnf4<i;@Sc]a5#K>DG6.#9(:-mrl&Q#LNTZf!>")f+#c<k`9/J-]!&L(&L/hq_q+)9BJ,`o
	W+#YG?!f4)5MNm+);OOGH<A?2,[4IWnVe9(K?j,`mh5f:1Vul?<@/^ngBuco9$LQc%WsTF`T[Vc:&*Aa
	64r''l!ArKmo"jl)CHAW?rh7@!#S3q+EoTKuUnb6;Ys1`XHAc7,!f$ej?4&m0kQ*KkYQdVG4"dO%0OT(
	^nANQ(c,^d-"!1\gsbRkE+F+]2$[Lk[^-%VlSoo\`<Yr51hGH*!.d\+!FMBF?<^Nc3:U@"I;o9ob0I-)
	;JH?5b<\\NrA$8WC>VT1Va(m+?)^[R:hfob[;-)l=,@GPd3U#TLZ!"+;&=/9WoDD$#k?3LKr5H=lKiX8
	%IZf6-b!l<,Fr^S>b$UlB9s/_bE41a2I4L.e`6-ILInSJ/h$3YnH;ES_\0,e=f9pHP:CW3Q('':[`f/>
	Kf;9(#u&@>EsDB-(dODSo)CbToTpHpcFsU^W:''q&!,''JUWROX$dh?r=.KZ*Gj;LRZ!EFj&]uW(GRe"
	Gap3^]!ak!)K[iLN#]Y_T=+`?9^eo<"jJ[.(4.A`r(\D(9eO_qEEG.63tG:;O/;M6)W6t2"7#,@(7hb^
	R60^%4VZS6<eCQV;qL2[>n.(#hoaf2`fKsD6%V5hR=MA-t[G\RL3&F"Qh<ER<F^)AJS+iu^rYcGjO`lE
	lD1G=oBiUTB)3C7TA;40tcU;e(tHFa*,k6+-@?^rosl!AE8&iI2hhN&jjn]l:^pg>j:2\\%Z\\.rM9V`
	C#?kQ6DI!nFW:cJP[9^(558@7-JefYiK77C6n5gDVA#0N^+0S:f%/Q.)D\0H],X6qG3K=-/1,mMIb/+3
	%O3XZtS.#:uji.dsCAmbof,*!#?DZX#h)0Q4&h/d&0[J7bB+"a=9fcBo9(t8QK$sEm76V[t6+?bR]ZQs
	#b$p3:(]33DcjY[VnUCq/ml#BS"\3r&@OOnZa)j+a7&lONOX7)IRb/IuHfh?T/^&`r"P$qSrmUi>I(cl
	;[AS*hE246!,CKR`.%),1^&9ViAXBMDG"UU\CU7PhDS?#@gnOCh<8O_T#h/a:r4_MlRQZKQHU)Xc3@!e
	(dh0`(i+!FLge0^R#*n#;M*s''^)r#&lH$=7lI=t)5hmYDb=EjgO$nX6Q&)+]ChlA2pM&ntaaKnt6"f#
	*Lk`DVPU8dq(\fAE>kMOR!L]Flrl..9Rc%"L+,8D8`sC-sX0ULL`NM[o+4@W:$0jT.J''N[LlupgiS/$
	uGWkTI?8!k>7R@'',)o_"5I2(i!b9r(>Rt,i=''+pr:tceXYbZSoM#=1N)1BJW2MkBi`pq([E-L>,-7o
	G+RLAOKhEIi[4!PV<Ds@Kc9PD^b/Hg<K"mDg&$*6=?s"q&LQ''=.gkClm?#nG@_r/l_Zg!35U4iucM*d
	lh"I!R:OT8A)^rGB$JKU&BHIY041S@]aJp`Ud*>1/lV(kF1<%IC@fo,eI&:iRY=g2b/`ZWIR<R8OtA70
	pL>*7bOOMD*uR&Ym!!S+''Ad?/u[rZL;!1WBh;gu.BSFtVp98qcT!J7&Qo25p/jmN@`@D''OOX493)^_
	''cITk0bM"G2eTEpREbUW5/0pKcW<^Bk1m0(Q(IeYZVlAMMW8t"qB+dJRb,A$65Xb[(R+;;<,/ra@-o&
	/)rpti9uIaYWEbHeZB%8GpVI4N]90++u"d9`8WdlS3`I0Eh=4]TL]DK?i+B53TTY%9q[?%MhX,UPjAhV
	Ro;rHY)jKb4<cEYZ0,5X"G,18P(;T4n4T"R#rVtVJLl!8pnjG''JkeE>lnq-TL''G&7`"VfsdXu@:Lpg
	JcAra<j5mJ+(]JD5DJi,p;JsnX7D)5Z+(+FK-D1]nJ5jkO4QFS)/8t%Z(gbrB_''0b9W<B!:/b-9<=0X
	AJor5FA]c(9egWgZSHlF''5E=deFhdQ#Xk_uR-U0JES[AW!),:S1\X(l4djAtV[%n"0Nf#M\++6q8$69
	G!ASTOS2S1\''&n+_NRX9)2-IN5uJLO_2;e0!;"&^g8M$o!-<N_Dp`Dqcb''+$\pW]UWHE01Dl''P!Xf
	mbcChR%RHj)''>dubrjBBfnGTHu8Tr)Uq?AX`<eO2,#Ka27)ZR:X+=po%>c-?<%$;;%`!''N!,_*,7`d
	#%DG>@1Y#`ooKu''8XS)$8IY>X(6e6aKXrn!S-U9F.nbt-_(h@!+7Hs<@J(3UB[4XM+oQ3>9VjV/3p,F
	e?9;s$=Q^of8,b`>c&BQgO&`cgt?2=ljCgY($Imp2/NE1PG_fk4"=k#Ss#HA+`-Hkj=4aa3t,3>qccou
	qbmlr@`m5=(/(VLA.qSoAft%[G:;o>FO8EQQCJ\\!t[IJ.ZP6r6[3R6*m5a;p1$"f$,ATs+!Tk;W,KOc
	JJfk:DsN/h`]''Z:Z83%c\:N^H_,#3t5^jJTH`@p>6qY^a9mV!VAgA"(2C@j`0QiTf0)MH7m[#-V;3VZ
	fST$SeE&YE%!G1_dSU!fq.h4[1G?+;hLLir/8BB[.V>PIr)nQ!EY\R-[7B($r%tCD=mP-%qAdbe2+t9<
	"ciB,''\JslD$u3$E_V["hC](Wp"9K4TNK1M:\8B]R6eUa8-!lU&CU5c-=`:n4OZ@/R#JQD@9q+2,(pV
	YO3)^<"\<-D]hF+VJ#T*I=Q:Uh_OhN&:$MZ;b?&S1ok&Zu0!YV<IP[dLq5kNJO3s/CcD;!uJdjE0./37
	fQ]V617TN^5H2qg9a.ku]D_a6H<Ba"Q\Ock_tc<Hf036#0U8eXeG!0<Y+75>H8gtIW/ljQSEo,)Lu`^''
	H\0EXa14b"JMKB`q&.cHPO@=bBR`,jCrYER0(]Fk`%H@*A$I@/,e"sTDUk;1#Yh]?"#f*5+p*Q74N^.b
	D*Ket''B[o\jX=rh3sN)o#6;OLX/3j_=mREX#AS@Zn#JLEut\PBgrAOP%,P]U,o`<=u,K"q]>(>0^O#"
	GZk_1bB\!d9#ShZP.tJ/ln._GG!.N(M>ul_!$+_W;]J`X-!AP34f3BBU)8;-[=UjB+4In\u32RkA''lQ
	OA]#^n,6$lAgIS@D/q3HdeOA8^/c''j/=2sRN5871Pf^Qcd36rBOhEJh?3p:+Ofj!OcHd[%Gkg&MM0=F
	p@A#%-e^<XgHo`g[8FbZTH]7-PHe2DP)0Pb$CSgpZ/>hR!(oZJ_nm]UG_@=nUCqEeb<fn^s4LldMPP4j
	H]huf43Tt*ZQ3g5U3J7fW.6:#\4UZWdUjpW=:*cJ:Je_i+\KWsZGN)oc,^X[Gh"EI6@JoMk\B06,"o0@
	Z4%DY0]OtFE^c3FbY!"H_J,,dHD<1E,)-b=:A&#H=<M''B@IKgolK>uA^''SJ=7:;!c^^[r4-=,1Q$jI
	]J(a+/1*/?WAb/Lb5U,WEFWm#JcY1gut77OMX$Q[?+bhELldSAch4<g2OKFEPYU@AWtY>KCpQnp5^VN<
	Vk&KP$%-A\2O%r77P;>,g0io10S:^X>ug6''laG$C+s\8[i*!5Tjehh\0mE_=:J''eoGK#8"/!9&rs/X
	+GKsAt[eegq%`<`F62r0e`\\\Zs!)n\"5*bS\a(78=AjKgqo&V[\dQ''gbilbpeO2\`W$c$6>s_C5cn*
	3/RiB\^!SYKVa*tfQB.aVhtd`F!Ls?\R-gOK#&=8!QTYTg&t\ZGlN>[E!>#_Rt.4;83@")";mQ.79+W+
	`Kh]&1/UH6K^1h]@-S$.CV=/^j;?%p*c`"/Ykl_o:1/r0.Os-TA1X)m<6r$0!/Rp%WF9HRPaMZL7'']-
	OeuOi0iV%L/lK>"Uf%f18C^<n-!`On"nI@REL_NOgC)b9nA=cBs1h[n*qu!''HAt\Xh[pJ@''Uo2o5@j
	%3kqdfa>M0+DH(0J@Hj\TmD<TR,[3QuduZU#us*>]or8B;gY$n=OIRns-U''H65WK!do+<h2%>A"1$Td
	a#W^9n)@L<rUY1BT?d?FS82A%((6`*5eFH:5qf)L)2K,?:_<u8m-/l$^@)/B.2]eNf.t$#`Mn8LN)1+D
	ZXNjKP%DA7lmAar=>LL?0;Va,16p_2po(<3LMj32(u*^A5)na&[^/K6P158OdhQ&60Wkg7O9gn+),b7f
	6.AA?^oe''QiEi1i::Ab?CLMpaf#?(VuR%:`_V^f7j?do*+KoOhHl769SJb;GesZ;_,Y!UKt?)PAJW-;
	KSQ)7Ds3^B@1Ni];d`>m30I.\''6imHc9U@uc9*("eA4&s"H63Aiu)eM7lh!6GS2`k+gZXUIQ0<VbfGq
	5gnWgn[,RcJDI1+4_@&nFJspV0aa6h`;DA9B[u-3),bEWA%C&/DZ5Xoa5:9?]T"S%&MP<O''B!W+8`4u
	M61eZs:#XgL<p!4HjLtK3rRiL4cSUhcU6jcSmCc\g0#P6r\Z+p-LE.c?;I^51JcM($e7sC94(0(?s_4j
	I5@EOOA-aJ8E%7c#<btX(j*0U@.$&pX<J-[/KWnNU&$j:#\19gucB>@Si^!@`U:`[eFGgO2jc-)T,>HT
	(aLoBQrkrgDsXAr+d]GP164g/>K1]K%9DpDap<ONdQ5SUR2W$IJ%@KnhG`YfV5Bf(<VJCk`Q0$L-?+dN
	dE$$o"<#=QbVn7911&W(kU&YV":ClWbE,7_[$oI1J1(GjJMg]_;or5SSu1M<;*qok>s"9\26''3CdX\p
	Wka&WLXPpZ3c?IZ"-:G4XB?/ie^ZSUD.[JcIf`N,tu;MO7l+4?0HDC[PU@+[B]KB,^=X`e,RO:pgVl8B
	>Dh3O$-^$k+?Y(N2M$n#upL0_-PuM53uKdE"S1YKXYG<FN"eW5VH%^G=F\n_Ok?GQs%D!etc01F>A8''
	:KeSc9NNMr@kEDe<''`hB;Sj10d:''/@?lC6j;=p+$04[9gs)3hP4=`^Ua''Zo2J0^F(Np"@\"5bQQbR
	qCQOj,o>A&th]lW`AIGashXeR6c\[8GAX1\;fCZeb`bq-6pgLn7cN:Br9JRHnSW/>Eu1J$H7(1iG]0df
	Z4=&FD3WRMas42r+Of$&iL/uR5hj&?k)!b/54&7uSSmd*3.Ye2H>-X0oB"=n1heW6]2fs8i3Xfe\e/B@
	Lb>\Y-:%sM(kkF$m+3Uo2dd9Ol&8-BE(iu(f5,PC!NXP99[).SbJTD$ERUa*bU'';4#p<6QK*iKsX">J
	gQ[CQU:W^m1E\GWa@f&t-tg6^B)$+lcf-*oD[%''K"*c#ht)26>`I.$Tj!KolC@Fs2Vm%n^3DIpZO7_Y
	Nct@o&@:5^Nb"hl/t5j-?mAh[B5W7T716Dg0?O^na]Fe\&CT,`U0L>/Sg%X#U[)dq3&,l''SSbS7atq7
	U1;`<3KYJJ/)jN]+]9j8L$%N1A/Hl6)^GV<W,i,[r$Gfmda%1fQDI))4_]1(X3Yok?52TmBnbZNjlO&!
	I!KLqKR&mH2pppMO2"R&H,OuR''/42o-(]ZO(#I4+T9L`.R3F?B"=l*!YqqJh''=?[E7W`1Xl^T<])/u
	NI+c0Y@*X?@l+u0ObAU.m#j`**rNG!cq5h$)_k*9pFFp;aJn%F2D(Vb:rai>GSs20g*''dJgS/min_jd
	T$d4Rb,mL5Tgd)iLVCFRmj(4LMH&[^I_s#`J3I%]BCR,SqM/HHo8iA75\<:.Lk4oRNl0IL:''KbhMO\R
	e3&\=Il%)CtjC)Yk6Tf+LrFkD$'':sEFf3C@I;A1$s%:O5OmlmDgq$;c+e`(Oo+#unbh!WXKC?jnD=+^
	^A6oJ?U-qso@SJ@$qKuYF8(1RhHRJ?X<-rteQ78!oFHu=I.s!TbUp$5s+Bu/^4(0D*c>rWe=fcl,O>i_
	d68*r!E)AefXk?jnG?MmMame&HkT%pEh,?Ia"sr=n3^ek1nF^XBUY,8''6<>C*62k.k*F&p%qr+7,Qo<
	;b;bB*(RW%mqT/ge[l318jN#%2e]+_$2f?IBjJOqs?JbP-UYjq>HJkQ.bDXQ1hLFq-+8t]eo9\#D[uS/
	kg''^uB5al]Wb5+>0\GF-=GQUJGcE88r''oX]f7,0-Fk"Q/I:17$ebn9/ps23$X\jX<&MGYS^[>7"o?k
	r''bYVX''S",%LB0>E"ph]Db=n!lO\k-#m(h3\/&G<U90$cf^,+";[/K84sP_/CLMrUG6,>[1LHqT+RJ
	rpoRqTDeKK)C335/RDZ9P0*;"gcg,Zl"<fjn^^ooEbfN;%A$m1]lNPjZ0MYPhO"ED/oU[ZHTrtt#o2*G
	OuED7,u)<6i%dK'':4DG#c=0H"c4S+Z/@cd;j%CsRMFR:p780A23''W.-19eoji8nYm-5B>Z4sY%J@pg
	*4Ot!+T"sWfIl1d0u)$\R<c7+D1\,bg%88&[XGffU''0nYJYP97R6?.`GK?hiTr[ibJ"QO[AR1\oM_rp
	]-\?T[rIeeq!e(0?QfHh=k(ceUd0mF''":-VrQIk0\,Fp=AMk+5DABga<V[B5pG)_X`n''NuJB?,g8Nu
	_JDsnXTURKZ7r>OK2MPk]NeKDaqC%Bm2KE_;T''`g`lk5WkioC]V14gVnWi.T42@3t1p9>UcBj&N4!M''
	==V:1-7)ai0$X`nMio7),c@4aM`VmRHl^Kh.5Pj6?cThHFR>n>NRrHfjr7I>H^9k0<h?1O+]Q*WDkHA^
	X3e-D6V-E,$gc,g[T>0pV)s:;S^30<cYF`>:Id3?!(N6J63-Orbns+8D?9\;`;]E4&]f_'']]6*U&L8-
	c/9u,t]@;!@a>`j%P;C[qUqC6>tS*H;_["Oa`MFE=/CqkrFO$jJs_g0gpfgIDk\mXX`U2/LX(5N$/`2>
	tdb[S9mAGe;9I5`eJJ'')WPmE/Z<F4ntTs5M`8JEL0*K`jMV_At@`PMa.Tf_K_ESN1]Z`6j@3r93KI37
	D,WoYd$fdg)M#`8J19CKF7F+$G&>PCMJ>mYSolT)8:%f<9V5s3ff;n]Gbkr8I$e/hHDr<*W]-_&&FF3?
	#dD!"lXGR:G>`M\LlBU5C9u6LBoU1E+)r\nF/HGu\29[Sat&M7hB58>-WA">)_SFZ(KA4KPK&FLj@rLM
	?gK7s%-C:Ealc\Zj8G1Uur05Ab5<>s32m`tsY8+''MlR5Olj`DdRkdBt1^^]XW+AWV7H^ARk&DgFmA+F
	R,p,k:^+''I.EWuJ,Q3f*rXrteY<p&Idrb[cYjePoOn2ElGBm"r9!kOQd7gBpAS.>hmiB>@ttTon^abm
	rcWPsDK+jqrc6GFCD0Nk<qXaKbWt_mSiZQsPsX''fr]/"dV;LSW.dpTa!CQ[D3\$Qi;o$0WE8Le(RfG!
	%B<f)U=9:Z+]ejYcF,p9U[S:aek@bNJ?+h9joe%qNC\pbsh#?:h+-dp-Zb1;:i9fu\D0b0D0>@%eld,&
	"aj$Fupuo`Op&/NQqd9?uip6,UkAPaPs4SQ#^@])lGG!SM]X%S0P@lMMmSp+R_P9a/Etj?HcpW7+K5o''
	QS:TfF1.]=c^<AETYErs8C9.5mO+<?Q''8qlE">^''*j*2S#&8I6:''kZsCjX3:hX''bnS?X.s?>IQ6!
	rkc\+il9pZ(Y53CE9cbmr8HBh\$#Yig^cUDPI9K[V]PWpak(uhF5tE\04)<js#bW(%tFT''X-[0Sn^r=
	Qq;tnJY<R?pe\?G1Dr:B&+09tjc''SFUpE$hjj4m_/R.-%2IXZ9C/$5tfHhdKQ93/AGhZQW^UN$L$WTW
	ehCV_5I*UVt9G);2qQ]D"3BVde^D78tl$_-\HFb$dZ0qC*C6<0es9k;Nl"&.DO6r+>L/Bj?73u[/SjZW
	Jt#>;GR-4mO\Ros_=`9f:3o-a:Q^]1JZ(CpH4R:e0;FQ(%8I"-E]m0Ctms7H/qd)iDf%m9UHbJ-/]hq#
	febnE><_YJ[AfY?nT\oHcPZO^BTI/%p[YHMg0=P:3mI.d3Ida4LCJKSh1bJK2\-\GI10+igX>c"9p^#\
	RP[R2+e?c7^s$nmXjdjs/g$HG:6S>k0Z=N8@rYX:P$;&4C*907dfp]''ZFli2(p`Ac+ZHuK%=NTK90lT
	]d_h.m6;prPJ/oYMP+%faY1Qf#HcZ+,&@eGhZ._0\t,mHFj>]QEJCDSLAd_PrmQ?b#n%?X15AL%U@brp
	7A&]Y/^\m.mb"?97[b:E=iUfXVNr]_(.8IlM_&EPD=kdJI!;orO/kd%mEBRVPc21?l;1=In0C2VV4,<''
	HO0I7HH''DBPQrOWD2u.o`PW`?dD([H=kf?j7D"CfEIu8''`@`+s?7J0s="XFg<b<l<\sLfild^8gsYU
	26*Z7JbIn''B(5a80:[=bW=((\5PX!Jrm&:Xj85m8CG4KBi6(o,f2"^fiKKV3I.?,nCWgL#4*TWT:To9
	@q`d=@V8#(a;<7T$_hV"D>i*$Mbi7X0qtH@k-t\"LV*RFA!''P6%KmF(tS3]mfUVWNaqP=Yc-#I2"0D.
	\\WSbN5Gs9m0WqNf*rQ9@FpcVpP09#tL&-5XB;l3)j.0>;r8+X(obH()]r6]j=^\-<cXc;ZDTA3OSh"]
	IX?$Q+t1GA^nlJ%XXs54tps5.`XqT/Is=T=lPT:S(;Nusm;qVgaEldnm:4`^5V`Uum-rR7*N5Bh#DH]M
	M/QZ''Tdo@hkjMcX4JII`S_?b>l_Y?ZA>q!W,#YIW,uo&Arb(]Rc`BL["oip<ONJ]M@K/#h/XUQ\Q,W+
	*-DLQYenf_4b4Kj87OmE!3G1#YE''Qn>%0!\L!0A2`2LOVWb?$5*Z$MHV-cG-Cb</Xeld*T-j[g%L51''
	ZUd4OlqG_7k#UU>20-jn?63qWMHTkRkLn63<-_:)/QtTpE)N"&q_i$IHJ''B`usQVor[DkFoDdG%R0CR
	GM`/Rm)-Sdp[Fkl2j\a4(bDlio;UW44I",/W2`92,nq,4bK.Q%.<WRB!ceLYWB=plE>hgPJY"oqK*D>^
	ApVO;Q758LTFL1AYo6sbjeU$ta;Kqei@1J>1Vf/W=XZ`&*SMA^J#E=1s75J''DF+*V3og]MQZ%<Igs`a
	(J)K_52f:7?J+_4D+5AJ3p[$;Vf,*&I0"lT)eLU[[ip-B^DdL,"p"E(=I/<2&=24LG(G:.)p6A]Dr70B
	?ZU`odp@-$OLS)3Ib.2H)r6#"[-Q^"-\i^MG\aU."IYGcq"+]54d!QL;0nGgk=5:1lp5I''?L@(ahfm(
	_,r7/F.2KkXj-aZi3\qZ.]PRsT72#F+%_Prng''UZNKc0"pPB4)k7@OB]0d+3R?P!8F=$K7/uGQtNdY$
	%FOn>WTBW;(Rq)4^X"]d''cYXZc=/db,R@''`dd+"7KJt^\2S$B5e#p#%uL0Q[eG=\,Q#=r.4bE\#WT6
	^Ajb!l_f7,aNi%oQe.7Gj$A0M_L3)R&<;ReWIc!aBM;lf1VcI5dF!RB#hGl!rHuuM-%\Z(-)^jd54kZ(
	BYgm.#pP.G#,]V%#.J20#C#5;^:&FJUVe#iCQ$B:)qPV6oV]_^I-L>LlfYIBrS*=Ne`A#7hLFqGF!#i"
	BBN&QpX5%*B<FDq_UC0PZFkYW`.,Q"rlL(2c@5<''c0J_aIf5Pd5C<&B:Ykrj&)[B&Mtc,]](Yu10:t=
	Gjm^%`lKUI/NigQ''q7c?Z.t,#p6q=,2]L7q1Oj"]!&neFOdi''Bc&_%rs!)[?$B?tZH:2-?Y%c7J7-h
	A)pM)/Q3@,_f2T\C=qH*N@N"k+e8OcD-mE<7rCZjA4J^JNQ)q+R)pU_HGq6mTnbJf3LB:I+-9QGRn0\J
	?L:i/u!62E$kNI:DFkJ+M02T=hD5Z#T_d&%oRSr9Fu-nDtUU5DZuCh1mn+I1Z4kq>3@iIHI5Y*kM$si@
	@D.9>k4ne6mFhb))CXSo[WH$ik#KY9EKoJf,E@Wlc4;EWG[]jsY$u6AISC&`?WA/;hdX3#U7H@XuERQi
	SD\SiO:`]gFl6X(Pe%D#oq]?9S)"olGWU_"5UTedp[<oS2?K[^#Pd;ibcHs5Lu7-Vo6Rk:ceuPnT4\Y(
	,cugUEb8r9CP.&-''ggmlbXddUNe0HgTOTXmb*FP@''hDp$-<?T"4<&f,shY`TM]YpsRjIDHa;aCObi3
	8!a:''#W*5O^eN\9/Bfk]+GgCQ]q%or"N.&I;!]&)[GjL=am]"9&.aUR:rEJ^#%Bjk9`TkB(Rf?f''[O
	Qf@qkEd96,Q0/LhgnhXS=VE_0IZV7Tt"[QM-S,3=5nj;iYX7(jAX*4#VQPY>7''h>oG-;Wu(Is6H)^TD
	cq3KD2KRH2,`FO8[acZO]5"3J65ee^g.@#Ca>UfZ0UHp.&1:*dmJ2qMh-Ikef3+oOm>ns6/b!\bJsDar
	)`_?hn*/Qif_b6spA\W"1.*dLkL8WH&4.-roFVfG2q(.*HPJ2&Mf]W=h8aHO;=W,6\3<9[GHqdH%\@:b
	_uQ.2339J/3]Y4G+S+]s$((XSqLJ;t$9iIURYMp&+^]?!>a*%%__;=-1tIo#`mUGjiD<hmi=F;p2X&r8
	QoZ2_LU\Eom-VYJ''Vi>>Lc7p!T,7Nc%".rVEdk4n=TMp\j*qbO<3Rri>?T2f:a$iV^!8i:c>P4gWXtA
	P''''oJZtb3E\3.UB:qK@BdPeZ#,T@q5@T5t,/j[*l+,!`Sj<bn9-#E-o_!G5BW,Y[:s<*R<!''#G\:C
	T7O[r!29p"R5*q+IHq+@KX]rdY!p.+_?kOQQj4FV`a.d%Yr0N=8rK,0ZY[]Da5<Bs_1DN>[*It%F4s5s
	"Qqg\7rJa;V@hgXqh&,uO\++F.*%i#*7C!"Y.rUdpUHh?[^*BJ0-M8er=54j%PhELT1Ms%j%qTuX>X#m
	$[bDUlIb8!5S>"=R''Lk_QIRUi:b,O:@1Z8M_*h@_co6[/]k<?(BTY7T22..''AL"=0/''*&B8N,G%2)
	C(*J%!!lWSYX(PF-H+\''lZ`UmD]M]aNgtUYdM(g$fRbpTO]sc(&lO;BXe/o\qS]Mt<OoV]qW8AljS[C
	u\)+W;''CNU9GAj''[J)f\fdIg_1NuU0#=ZX-Ao+lWuSO24]m?8:J0T2AECG?AlX''SN.aCWfiPU%Sh^
	jt8$EfGIa9Eo.2L:PH''CT%Xbe/9A=-7d<t2BX+;F#kMQ$*aK%CCs8`Xkj%Rk#t0)Rik=&I:h$80^I[5
	)/jkuBob`#<L_66Kca</X]B;[.du&PIH3a[MH*K6W11e8)g^bIX^6\1%e5b00a!(#=eq3]"?kOUna"qV
	/RJ4h=76+HkP''INp]WVrD6;^iiPTT*IdFcj>e9V-i>8rZIH>hFkbV@qo$3$B?b,s&T<@"_HLB$:hu(o
	@c`\C8s4i=!7Q<F"i_nDVR,]&lC,ga"gF?i$Tcp-AD1XhJVjY1JeO\hS;ft4sAH$]''_\SYSPph.iBBN
	Ku7_9_+qJT2uE&.)S/^m''>h!:Z(Kn1+DBG8`XgB=Q+(blR@C.>$jH]H^PDS!5*=V<A;5)O$caTY^V.q
	Ed@F`CQ-COufC<acG>Hr]$WgM:SfH_Y[RE].(/1(7CaoD=!He-tn[Y=2C(6=$(R?j8?V@+LM>&7#-nqD
	:"1Grg,.!rKRn8gZnh98k?M8l7n9$s4bVOJ5J-Bn8r#NVLG(Yq4+TDZ\puNAR;jKinpak\Yh"=det=#V
	ITT''3I*nOd_0#"bITB)T6(9TITT6[AH)_80S7-F9T!/!LUMMN%C/S`XLdiZi1)4eo*-YaLuD8rQN_LQ
	_g\Bf(iNSDuB8*9rTd/ETY;#[siX+lDnjY3-42Ie5).,hqFc_nAkQJ4Ii=onI,u=J:p?k7jTFbK8pi5c
	S@9rV7TH7e![9\W\!&+/eJ?13_@kL8OnE(d;USQ>4kkHkmTD4[App*P6lnZ3_oQoeX-a^C8-IjAuh;Ma
	PlXtE"b,pYaQIiL#cF-H=FK3T#(!Os8LZ;QdQa6dSV"5-6O3=]4#?F",N@ZX<!1?*=ZX9hQ17fSNnEl;
	.5,Mdud^rK%#ZE;H6.rYI=T;JpCR5dj/c3&<4W\\t+CPH7kk[,>4DTlGBY"-4h&,%IaAL-:ko29eJGS;
	j>*AOenX@OklKO0d2c+M"*ftb486P;_7)_0EX(fMfQR#0Z:TH[AZ(-WhIbQ#/?L)n+gPN+*LfYTiFFT/
	PAe(f%643BR4prm.%f\[`Y(iBlbs>=OJNA=:3;+Ti`G3dUsaDG1UelIaMGjWcQNDk3e7>;P0.@=bHaX3
	1P8NXP54gE2oLFKhEXpVpGo'';M1l!c"U#R<B@AVNDE"nAq7oXR,RSCECW%-*,_*37ofWG>@7DDBp+mP
	K:<.6CTmT:a$?.e@_u+/[C2U?1_*9Y]t-GrN^[5(c&2/7UrO:P<.YIH((7,FU*kL"T2JK>#)YfD88Ws=
	)$%!UJp&NA2F.di@e_:3H=1ShWL/7._?KoUj:#Gj$_#SAKT9''%r.>m(lpYCrJqP22\.3O_S+IgXM"ig
	N5f>Va,%o7%9ATLEWig7*@8uXE2@*;Q>mLJo6e,^#WDpL0cj!XH^#T:&8&>JU7r)-:cWTLiX]A%D\.fZ
	h-M/"rcVs=%P@(j)c`9YsC)XL:U0FJm==''usgSi@]<58/(@uT#OmCi5(B.^?U?u.XNRb3)fN%ppo49q
	t[*#)D&EmB3*(j+ncV''#\uD;\LnD[<&p2_:H;a+^.+gAUM"s4''b6ZLD9_!t?)$iu/4k#!t)#KF''a?
	<mJF^7!asd63=RA.6#eqfL)k=3&/,_gLJE!FAGKOG)oqP3t96jj*K<AOj>JurmcW;B!ZTfcGNCC+89BX
	EHeN,''><86fd5%L[6A`R@pP):)9C>2Y-4sa"G2B&dDLD.-I<Ug5ECo.efq+#P`IAA''.D1WbfY7]n%)
	hMX0*pnl^7OOdS(Y$Kg]h3eI)PGVuubEk@W[DG1L<Lk0+a-2a95rTGJ0?-3upLcP_CP^%m#t1IsK&0$L
	V8JlY=S#skLbOMTT2=)5=GWeQZ]+;G?(pUC]K9*s#XgI3Ds_Plc#!>OmiJ5q:\.F@sNSAFZ$HLi6XX-^
	1nNRg@rL>C(MC''%4VfJc;3)48HVV/r_bkYkWMgU$LaX/O/6.^s82S%6^OO]lji8pB$3?1W)2ZtdJm3L
	#_1V15r/\m=NB4,q''sFtK!-bP&=t?DZ-&D/":&]@YJR;+]"8iWD1C7s$=$n''t7k?,Kq`S:NZ7.qJ.e
	Du[`T3l97\k8D?B#"g!V!RJOjXp(Y+)mq4`(:LeaF]j`ad`_0ne/=4JZg6.>Q`580dP(@@8(/KV#/s47
	>"\N''"K0er>UB\#a;KMCK"A#=lrp1iitfr@.]UDK5=t*q:n(K29F00;Cg8_!kQ%JMj<Q.#U-Ja`ioJ@
	[AE+ne+D64L"sJ"(Ledc3k<&3.n6%@CAdi:QV47RPPlLo=&4GV#UnjI+''d9"f&0/g>R3]8no6cmoW6O
	-UGo`cFb=[&/C)?(0HBasKk&uhG''oQ`)Ni$HtrUd`[ZPEF:gNJe"`jXFRf%/YoUXQmJdtQY$f5=na_i
	r.G!M0=/+U*it+cS0D6bLZ/r-F"3Dnc1Zrol''Tm7Gqh6*;PRScR"P3&G:+du%D2h&%`Lc"+]I:X!A6A
	T13VMQt!@dE2oV4k9EB9CqN0WL1''cp8E#RIUA,c`Y01<I.jkE-90@LM''.F!OX#<l4Q#9=BF!aG''@o
	FEU;Zk-O>@/EBL*kth_nmE::ggMr]p`<9TI.=<tlgn7"j3/g?\=.?ps)b&33EBQI@#tQ+iu2_RKR"s8M
	>^L&-27CJdGF"N3[g?&Q@l*9I[NGNS2Xm8>7sA@L=(T%g)a>s4H*<\(jGk-ggKh9)$Ya^Ih^c#h@];i?
	;AKUg.`AN8>FHBVmEV=aV\R6.qhPML)*>dmn59WjmC(&L$Oq7g,;49O!K`a+s_\"DHN+OG\\)*Cum^V4
	X''\)72,htZ;gm,XJ-&u=r(j&M6u\2N&%YM/\&ji6VjEc(MdF4,I''=Djl5e4I/^GM)A`g"0]]S`nh^8
	j=%@8QX=&010Y3YbSGm:L3*C"5''(c\M-O8O5>>MB7Tm(6G3\#^@bcXbUV.5Q-A)QSm]q[dG&J>.o<)+
	!3iQTLkGdW6C:%r6/ibM<tg\B;+*;#CO%Lp]]4M=J&OJBo.@/bC=o4V''7''kq2ZZtnpt]-"QHjs85Y7
	iV)/h3rG_8<:>*6YZg,aQKqQ6/E(as"-mgTIAD_UYkOF$7>c_Y[5:A.nU6H)$1-eIq#gY);oH=jWi%Pe
	^:?''Wu2OnfK-#/\cC\M-\54V8s>"0*^/Xi]0b#/C+Ls2A>''Z4Zf_6j#/?7m9HX0]&9!WX1sSB6N\W8
	$@nIfA(_n1nfm*[AKq5ZZ=#(%#1=k6rC/LXHagOonh\!eAJPCEc@Y_Ck?C''''<J''k8lZ05TY,BBSjB
	llbmib7"-ImZ$"VuMIBGsmCU`%1%BS?I?hf0*@]G-)_?H8cXuWF3_uNaD7,%o''$P+*`!b;l%)5*HrA.
	SY%q''Q"\DaAmP#JZ)na6)tfY=0.5j"SgJ>1A3a!_!<!5`.cYGGugt^m4&T^%M\`>j7DbD3oQ$Fk45sX
	4$R#JThTMaM@A.JIgt-g%!o\+.M#,GLg\X-a,HG.+`K[G$X_eqRP*ZQBtogZ1<r1-bUgOR,C4Gjo]@F4
	bUMNr4bno%(cD]f?P93nCrHd&&)bb2RcqY5pD*_0`i4P#!iHZbOV[U`''*h*VsM2$"oCugdrTlT_9@)Y
	DV7A]QX\]IP[eHi[5t_>dJNbJQ#^=(),1F2+%Sn6E["`j0e3.?eqJ,Kq)=\93l(^tPV_58Hlj4/=1>IV
	4MADG]]nP>A_9,&)R_6_=/Tn]k(ZNd.UlJs3ih0Z80HoU4G.C9!KZ\HpYiOTVuPsFpr-0(H,&=T]Qk%0
	$%kSY-lp.\"=)8L0L_QkYp_QuqEu2Sb,7Y>Ou2N+n_t^EPIaMeoSoHuH""R$1.bH&$1$<mlf;C''o^5R
	K4^[Dh]?1&_5-B-CYX63[:>-''hcF_<34LaWkVjLE%5D?*D^.C941T8H"&dM"C)Bsf$igTef4G.RerKm
	QFS''a7rrdN%E)0qE%h(pf''OIX.A+p7jLd:_Vs;&bnTc/C)8+TlYqo`g&Om=bl.r`IG=Z^0p''K>G]6
	1rs''4(FgI]q"`GCapk6T6m5*E"HH*eZD58,''[%B\;:d#aCd+L)&BYlp0S(DMn%;2%0%ino2:c\YZce
	$dPN%"N4-=J)s$cAdFH$`Bor''F?PXA8UOFc-W.0h$S)+<Wr%e>)hfb4+iU<i=''q#0$"^AUS%e_bhHf
	:Sg546i<.EC^WRf+D;He@s5eaM?l<_WGajRa"UZ9&$Yt`V0)6W3ra=Bg+WGqO-la1f&3DV:4cnlKcL9@
	!VAVhR5Dic.;L/8kg'';]p\WT.\4tuRkBhn63!HGV-E90^eZ)Y6fSU[1!W%+N)2At84dIrgh>Z:JS)j8
	/i*4qgV4l2LW4]%?uTR]M]m;M5Unq\9cuFuT0n^-2qk^DC=4VlEcl;N\!-k1R!tEAl*#3X6O)Y1eK0i,
	_-NC_"<%-mec]![0Pa!tSG=Mi:.qR?Old''5-?N[^=>#[a-<uq++?NsRqDeYk=amn.bTjsoWno8uFDo@
	mkd.MDUY8A]C,nhJ&9.cs+\3.FPo`?qOl["A-tL,f+q;$.8%LC(NLYPY(Ap7Dq9lE9Som^/&b]F$q@_b
	i7:Y6IOT@jSaj-PA..:?*WH>W2IVD[$*kS7h]<rc8XE;n@=2f3Z1IrV4lt^UCO=u''+/UO*-bf>q;U6\
	t`;@-8n%?^l5-Ci=IWa+<hq`Zd.7ti9$6(Jfn''1+=$:nHX+GQpB[q\T:Ss%lq>UQ5cR9`ReZ)O="iHP
	:d;i.L_sUI^1^k-8&1''_9H!`Rl2XK7QXW!>u:[%iLV<<c_+-0XK;Y.?eKJlp:QnB:_AA`ak`>kuJR^p
	*R.P=A=mVf781cH4uZ1U*CM24uR?pF$4nb+"%1)o]DE)\`b27I8q_u''t\hKVFlj#9DP="*`u)pN[c=$
	LSuXUSq4s#cic8N)5)qR"uuQIJ-9lsK3s(mSAD]7ih-53GM*pF!Urg63GWl3V+O7ng57''V(P4-EE6>P
	R8TJtq]?AEt>Ni''O/2pd>;jh7(lg:G01<it-TUL^LZfdrRC@?7^STA;q<=@&^Y<+q:k''3chKFah)]%
	1l0?@+Z*T*/::Qcbo)]+Ik0emt=tOOKnQc/%jtIe2saq7HZFXm+V#.`-m"Umo7Z&Da!p1$JTV04)iR9J
	^`1f!;l2G_Xng\6(*!^oo(iMCZkMoP;">X7JelQ+<2j6c_(Y=V$k]$q,0An4a16onbM/J.*bC6j@[F#E
	E!7S)?OVK@T;=n$`Q,-!/>#T=g!/4#mZ4n")#@f2s>3?"1''\6GLMYmIZ1db:d)2M%k,Lb[/3s$>,*D8
	-&k=)2&i_;HHn$n4cjq]J/<fk6uh2gHYS5s*\Ft>0\"9<!TMrZ>''H(4UZ97nG=I5[&GJQ/R0HmRmI6O
	-YB^63dT2T:Qm.fGtT3G2<esanbWnlIu17-rEg6)Y"Zjelr]nIG%4ID3KcZ"98iHB17\Q.9&RS6W_mW/
	g-+s7^A[bTrqEtU<"q)I;;VKjDJPbn#eCf9#s1e*GJm+Y>eD=uV2ubl1PIXljikRoh,@PBZgPEFlu&Xq
	HL6/m>:S/-]a;7S553ak49NAaA2s`D6os>c3M^:Q*L1#_#Plt;c6*V@]N@%BF5CrTQ^$psJn$:mTdB[8
	:$gJg`8]lm^1t;meQs\*CYGXb5u=7YT*jcq\:fa3UmuuZko%R:_RQ1&O?rDP,(guOrKG"ds"mn<`r_ob
	#R^-A>cUa9''jt^oOFcU-$&tl1Mm7<iN:,Pe2e-^4C.S&Wckd>^o_d-/[o?H,>)3Rg3\k\SHA5-"Z>Le
	@'';Bge^3H%7mC3,K+-"FRl-5t)1_?q#lN),[AeQf#q<PT7Fj[`FanKnIAQ,MFBpg+.KM5=R]?;hk<(L
	V5?A..XXj66DV(u)#IZ32tE1!3tZQp2jM`=_0@D/:pDaH<2\mscP:e0Z]Eg12ZU3_D"/B(T#Q.R^gWek
	i!Z(<5!Z)8+..Eakt@c&FY-Kbf9W8U")i%`c<hF6pu)u)Dq)&[rO`<3[j62O*<1\_<ui.p4fg#&jDAi;
	To3hV$0X/)CmmZ![65bEDORnte+>oll>B>%G6NSa4>?M3)''gR>,2=Lg4[3hdu,CfIED=_C>!#V-q/<S
	tM/1AKh_^]2VEP`pms:CRN42rT:3#5u>QdB=n,CbYMlJZA%I)CQQ(lL@EV_UY;>S1^7@ELgqn<*O8MdY
	ZkhRU@qCJ\[4rB5k@,O8K=AKW+*0iYcP418)+!;a''rt^f?`?fVh[tPt&$FN,(AeM8aWo%hN#V_rLsp+
	%.t4i)O@p(AL"E>eiUHJ8`:<B-9gC7>MB_Oui0#7g=J<m@X=LO/%YRqlGE4K0tIQ04`DggrPA1RkUE7Q
	e8q?b%P4UhCJHQ43WPQ`DD<ERp\e\C=qlS4(''QE%4`H!oh47ZjUAm%c/8P.587baqtm:R!P$<dJnMm9
	"N0mGLc,Og>f"JjU,4$F9Zue*@]rGDe[Vh''Pg;koG:Fo3"=!;</;^OK6ZcgY1c!Eg''-`iq\<0FB!/h
	%A"''7Vr@o&*9f@Q)F;U0G!''lPeiem5uL%0d.`*_Mm+Whf.j3<uM[a^s!*D?c.>Lg!eZie>()3+q`7)
	2LNTp4)r)+)bekYQgh<5t&OTO?h"igDo*J8`CV8@fC(gh3t;oRN9@''5V:2`oanE9QL:akE`?MDUG''n
	Kq!AUlmU,ifm!`\#@+BqW\`KAY6L!_1g:#40lkq9VD1gqXS(6e*[cq''^S$7C/."UY$&=roh%;3B]Da4
	SmB0:DoJFX-.Dc"d<''@@h%3o["CH(%!o,83Wk^<B/D,ZB/k>gBHPqg'',1C8Mc)]rk<I-Br-V''DB:q
	lW]WB?<+/aYD/bSc8-"MJ.XmG_1>*/$92ZprnCeGBCP;,rmUnE;*02H/3nLcTcOS+A*$q#$dKhc)M=Qr
	(RmDeOn/u>S:N(n?t;=.eB02P(+3DblQCi;rqZTb5JA>HW?::@#SG,BJbpe(*N4(5CL^6L7g0\a8M5*#
	m@8(nDRBUt%#!^c=#u5;''ESp_;GB.<Z(?Mi,J"!-">6XXnlqJMmf-78nT+p#6#><K7_Rrm1S1\''0BN
	2^TP:GJ1@T)4OB[6F;CO>X)]QU_P>r:+$R5rk!d/$*e9`X&^I^G<00''mIk(eH^It"?Pc<*U&)/,^S:/
	G,9JE`TM1[(:)r?WiJRfF@lG]1GT4G,tD8j!t%EX''k9cR<Gq-"]AInE5;m^O*)AT0Ia''\-k*5mM&uc
	JN@h(.im<+&<Rj04j@s7;:gl/)=gJrX9Tjg6!$sTX!;Dh*4fh/TVP''N-LdK9mlM=J")>CUEX&m40ut6
	"VR;4;0"SXN4mg8&?*&''MembJI4-)O1FJ7$[RM!EnCs\,=G68pA"YpA-TY3&<7Q!L&!1"Xj!n,J_3Uq
	Rnc*r2m7j#n>*uPHJfA[srpX47ok3C%(d3(H%_XA<Ar4JG!''6lDOC*8+7"FNE@X!>Ms_VZQ/#_2XHJ,
	JGa++F+Ms7#pS\37])W$YmQV^`f0q)ec2N7=DZ(e)7MS[A9_Ig&B&X!@4b69mR5rN@->):G%kfRPu7K5
	@>/W?XJI9Vp8B^_(B@@O]/M0Q[3FlY\eCQ-\K+m-3m%WL$`0C;jYi9>Zpf-n1:7H5:&a2"hM_+9RurjQ
	agJW[#07,C1$oKE(fZrq,+Fdk<WXSNhEPY-f?3l]@\=X9N_X\$n:PcnjU>+ral#[3t4dIZ[d4f*fE&^m
	(19#%C,I$J^]L:FI4ms56-<ZS+06MgsmD,?3QMHn\)PaCn,C,?$Y/Nsp-L`=q:ohk#:EO7</D''7<q+D
	*gh92ke6<''-b4=kY2&''#&dM;*@QabTq$[e$tu(i\L9=`4UlP)*n5q`h,<WGql/Gj/>NL%??]6dbS.J
	k6&6T''/ECI^6oHsMY!o#"$.P+!7O8KKH6?k)*tJ>=5Q3>9plGI++91B9om;2^T)SQOT_kKo!Tt>nS.1
	^XW''6]cH<S;lkKe''Tr\-;ZPO#D+cW[`DBN0"?!YMi#b4d.;%N@kAM=QG:p4)uZs59MsOiQAV\,X3E@
	#se%3<Q3VbO2ZdJ_57=O%D?o:''CL\a5EZ/e]2''][)Yt6D`l,0;k)\6dNZ)#UFsd3[K:`$8*Lo4A%2_
	`#V9TgRM1:U7<$cuHTdYNiq:/L8U;%tkslqf2^X7.hUk_''ja^P:L>Dc,1F.msEsOD@)*"=)6=`CZ+H5
	E,2''N!XVq6Y^IJ>"I7cRhl$U6fjE1R*S>]]0u^a3X7^gIE''g#j9YdBHY]%l)Z[f&#10C@eQ_5EeS^p
	9^EK1F(1<McCW8K]@`aS:Ol?f<:shW:>Q_+4C"H*u(Dj,l,JY8#;konJ\H5>-$25LR<Qe%rZ?6s7A''.
	3'']l=kN^-u]E9dJ8Df)^\"=KV@*Sc<)2N''/\:9]_WN-/u<Za4&L"2:7OW*Xm<4R+>)C:+TliD,2Pio
	oa#mVW3N7AKM$+S%oB^9L3o&ZbB3Umd&!mWU["1KVF''31[l7;Ou<bS"R;gXaO)I,TA7c//%NK<u@PJ0
	d*m<4X5(+mr.=2rT%#d>mY)I`aL6b:iHh/sFH4:H0i@;(6#_-1hUDZ.SAtf7s;mB)_STr)`c?MS4,Ja@
	nE%LVSUHQN,rj#.:Z>U/ha#pL)k]6e4&DiP!CuK-j(a32)4d7Numr-P]VpS''3X]rd\V]HF>,BjA<u$1
	mr''N;<^*4cgUI4C>.j1T5rNAh?<I@n?dt=ITgIr0N$L?PMU+Vn/ks0ggt7.?rQg_^5_thh6+6NrE+1I
	-li=YV.rE"MQ5J6),.Y2atn9lSiZBp2uSsA>%Sn7B]f@Hg%X=r[I^f(H.J"2(l3*\`<r+f7AEa_!tjB_
	Z1oA:8#BgAW-%ZGXouMcX!(8H1L_5p!ouBGPGYlp3<N7"3eZri^''5d*@D:%oGV*Z=+(Om4Ci3UG;aAA
	M[fCkt:XP(W9gAGkqPg%GTq/+,2"$HrKPWPZ+?i9rZVOR7WkCjhNfONd@@";YC!M7%jlh(6RHY)a!8B_
	V125%]KT#bpe2LViRt_6dKj"!?k-CXD1>!\C\e\jLrun1N5`l''b$rgUJp$$,GH-!;q@[/D2Xd[T<&/J
	n(Sq1hK8!*h<GcQQN8:6Y-?o`l:S<2?3<J0(fi9tt*a8iTF&Uan6AhSlP0cQa4pW8ufD$L#jnDg>R`7G
	LJ1Q]t7>jVaGK]du]h=4@12B*7Y"PC+Q48AT(hL+!j@u)`]FZ5i0je`C<A1Z(`0I+1F6S`AMCXM*HMp+
	+''con61>X:(5M$FA*i2YNR&AL>0FA9hA1doXIJD7@>O^djAJ..+_U81FRZYnp/?C&YkJ_g[Hg:8R@"m
	]>#;(/K^g)F#''UT+XpN=6<&]eg*1r2[]mUTo$Jcip`=KI2=8r&s;3l8X/W4P4Qa_`ca+puROPIj867!
	5Q*Q8k4YmhBOLb7WSd7]''QgQ.TjbGPBSVLhLs/=BjoQXPPR\=lI!pMSM&L4U;''a.8*iat.,eP%D$c/
	&WCf/6>""^j(,d-TIiHHHgb1FU?AjqOIsu=Q''D5)X/51gbGAj3/e+f00&kc[ol5b*godfMb&j+#\/o[
	J%6<3_c#$%7nC;PBp9^)fe4EQ_^-?ob=51TGp9ltq=ZY0-P*c(r9):-]7`4nQ%.s@;sX#/0ZTVY)E/;W
	k#1_(\#JR"''@q''OHjAA=!2Z6UBY6''%!)_Ht2"[*0<5>HnD$S\[COmA\*\Y"4`FS3`B-;b5Br@<)R7
	5(u;V_\I9(p(cpc2r[o-lq(Nf+bah9pb1)p/)l(I-lq+k18%60P''hZm-4i,=,pqH2Vsp-NYQ(k!O8Q5
	"mstqIcnjU)q%/]G$[42:+lYI^H\5X_X>_oLI!YVVl04''&i9-FGC\[KoYkXc6s/E],rf*7b:Y%<-"p:
	6l5")H]4\!^^MR:c<p$QT<`^+BlOgcqNInG&DI.\9l@M-9Kj?)US)ZQZkS''rDHl.U5JT*TUKNpq\LTR
	l1A+9Q#a$AHA@1,(''"9&H+^rhJqsGY](Ji/>?X]D:pS/pLb''AVWI]P<XhP1\k\H.`DT%CK,nG8o,c*
	#NiIdJ_\nR+a%>LRm=ic6u''=amD=okq[[BM?QVC&@+DGR;''1O>n*k8noraNc\T&1[$DjL''F$Jo"I$
	gE)MR6eJAjADG$DG(k''3?rsQe2\.WqRjK&7\B4$XS2W*I%rV.%]Z]0g-8l;a[Q3]Lu?Ldb4ou-G4&JH
	QETKUCu^;pg&eD$hLV:HYF&V^kcs_?Mt-)@D&r)2:tN-r40Y8r#`44*f]*W_7#8Mf`''lOFLU2#H\r8Y
	YW%.!)''"=*LPB=%RpM$$5gF;TaAFBGm!D_C@*7Xs(r4N:bk^%$@@8(ui;a^N+Z?X%orL;Oc61VX$&hr
	bUCq?:E88XcT\We"NN&1lC*YZ99q]3,#Z0R<;3m.WaIm:U]6Bf%&A8BJ,);)G,^)uJ1PoYUd.U/M>P8\
	_\mQ`Jd<i,^fQ=!K_)lpdl,.-P7=0s9`D^Wt%D%[?6:fd^htoXs[4"Zdpqn3!):3V=)NW@c&phJ`l[uZ
	g4Xj1E%<8Xg*.0^u^P0c]Wl?G;*D*`gY`u9`3''7:Z6A4p?Ia13R>iYm^.@8''I31222"C<I''`It@b.
	''1N]iN">68$S&gLX1>eICijQ7tI7$:u@(eY/Ql\5SU@\1=23Gf"L5T6MfYiH&?et>,BWp(#P!$?<m3`
	.]ssiY1QbNUpSD^0DJ]J"-\%P=>"9WRVRSh(-r<]+4Y%CK(^2T>!s51*ah6'',o.I)XL$6%ciQ)NKKGh
	)/,7-o8a.XFU^kI+-9-1C2rU?L#15A1C8-<.''7=>PII1/!mkWQTQ@E^kQ.)tgNHAT`q.j,4p`QU^=!o
	fqmW/TJRp6pPDTV;U"rYr+<V,^<aZ01%RN>=6Q](1>_B+$c^W1J`,;L*dme.mg)+;8Oe=2Q#L\(ub\Af
	AE,-msVe)''s^7;4-D9)UP<gDg.TMOZs8K/+\WE;*cBnlc1QO^`-^^''Og0Etj*XYbM>r6@(^[7`lK]b
	+_6;.FbE-eFnE''nu4`D:8;*^X-$_u&AF!9_K5D;Sd0m%!tbp8qT,=l3J-^''d-AiZ[]IuRX.#3HQn%j
	uI7afGjar_bg2N!s''*L(HQCSZ%$#*W'')+:Es<"DmPJCZG6L>sCW)CJB!Nl5"/,RL`NZljO6`6JklSV
	@5j!R(np5X\5)+gQY''oaL7LB2Fj(*KRQ6"&\h?d%#YG(s!>9i%.iib;F#9`EQ''tQjkA,A1];1X-=qs
	#''+Y!]2k1F.hO(]BK_![c5?W+/3,0`a8jjkiunDj&PG;rW[WGN5jA_;OPeJGibi8LO6,nrp$as\esd4
	O0`WR8bo(WpLh@k-jN>1CNW[pGL6;L.dlsD%nL4p9H.QL.rEWkX'';_JEa;IBs3ndn+oF*Qa*+8hXJ58
	TZKVR7G&*]Z2mRFY?26)6a<5n\I4u/LZWsL^-JYYL&3"/3H5h$:bs3gXF*4)/$k30\*gb`S@$8a^B.o?
	T46B\!"p0/%"reN]D1?_#7J9hm5AVE+9U3HkI/L%''j*ic_/JIrnMM:m3G9<S)T9BN#R[NSb<b6?Yi?J
	]U.NuZn5GR6Rpm@U2h+BUq<-OU9*,R;CUVukqI&.J+N,\J5uh&pP*_h<7SUeh4MJ@BBO/lem1Pcn,]Rh
	aA;5rL2Be<iM$ecZl7Uh[c3b"b6GKGGlioN45.6U[Fe_?@d1"iu5[7f/`5DHK=cj8pGCLtUQd,SX/3nm
	D)$L255-JQ,@B7eDRP5O+^Lo#5)@37B\Am83=mA`QVr55T8M=0[df*fXnDrlQRm,e^B=+uV9rVW,2TLc
	ZjN9aJ408Bhp2P;@_G:^s4>!Y*''s''pnE2YUF,aXJ`qHo-iN%5_4?L%g"ZE<UsEnTfAm($u3n`Yga5+
	AD$O8UJ>g+4V0ANF,^`YCuh)eMm?$iOA$FM_=o<D03bi+1o(3Q^kah..f#:c2GNZO`mW0Nn=h#I)9CXH
	lnZ??RimH&TXlN:)>IJ3jD1i%.d]iUg9ERu\CQ/uYM,9:lV?fRj)9,!p[$8iG''LK%jXJbY*0E)i$q+p
	n(BG!e->.l)r.r!Q''8Vdl!-fs''IO.WfjHWOl*\qQ`036c]Nm78<)8i%HT]srPaC&aB;DeJ(SB31157
	;#kQFH:-;"/fFWec5DZp#,,Rc#EC5QH5#*Xp5E;C4\/JnP(]&D1Kj01S"Co"YZk!eW3GRkWVS9aG!kQQ
	h"%O9*NBTUl:2UQ"hK6pW:,#uJqjk`F2T[8C^%/r43]=oe$32g60q4sKsRZ*`,mYV*Vl>Q@\f2$)+E2+
	6],aro5Ps"SNuJjB^..=jOcW2G<1!C;[X>UFOWRIF03eYD=5#?`XD`OOc;cG=-m(+U)^8"<G`Oc3k51X
	KNJ66#bJ^k.@H>8Cr%$`bTo]#D;D)4ZB.&`W1shZP#=S69+clK?!^ZJAPNjcs_p\b''$;%Q<Xq<<m\*o
	I:@8W5IoG:G>KK+GF/sckq)d7.:W%aIcHGT#1t)`$*[(DZU4YpmC)8B;>#hM6u]`A[c+*hE0NI%)4:)(
	K"[\A/.^fEbLi%ELs#R>4,5n#0)G?)2.;^loj;F:h1-?-?,BpjLj!GCtn9NBlZkjhJ=_Mg$BD0e\Xuh,
	27*\fC/1jaeP$?oj"f''laAh\KmuLT`ArHK!bB)Nk]tGR%EouT%(U.N/*r`!?VLg8^''Hk(=A?#bfq?p
	%]Ml5g(nO\cgLqZHahUL]cdgsH0F\$:QN]db)@l;eTBsC1LJ*O<cN%.MF)XKf#3Ib\5Wi#B,/&_n!HVn
	@-HTZRRX`;?NLus=E)!D)\l''L`Ue5%s(>CoAhL:O''1VfRm0^nPUEX)H''$Y7%XHH]rsP$P7qrS?`%;
	Jb:VmESmX5q4"5AnULh:-o&Zd+PgJ\SPj/h6.(.5fD)5XbZtk1hMC;&]knY<"l8:WVKPg;[T)sbo,<G\
	-_776l9(BSAE;0)>Kt<o6&gZU9ocbW*QK7lKL>n*\I=mkU,qTA_XeF4;QLR/!''oT-YM"-\"HTSm)8O+
	(<dJjd@:4FTR"f_1\=ONNTCRHD?L''H)V2A&N_6C?0MU,Q3EHQ+5Vs>s[-6a"<DMXB.SPCZmmZHU7$%E
	U''9!CL1D%8,)KtP2A#.0()/-Oi"#Fm[15N([n4,-;>^Bf-b''7!rR7MS[Yhmh:V*V0FSVGs!QcN<jF^
	h.]H2FDh`6W(Z)d^2X=2,H=a6_8T5E6E;=g+;>B5qCOlhR2)RHmu^B`%4hL:*g(_GjB9KVDE/A/*)EU%
	Ab`W_<oSOApOf5bs.7Va`CTOp''#(e?rJTQeJ1eSR8^gniSp>WTO#F:&gqi9]s&C\9Fndqk\S<D(0k.<
	Pt\c5AI?5o.WJcZpen"K\SK!a=5GCV*m!K[>6/Um[M"I*&8@m0EHrl_QVRW#-G,cWZ20F7&n<:WY@fH#
	Vod]6ur:o9A_HbE`h-8M7bq\cQJB._&8''-RXfD_j=>9$<b6/ELcBOcFtR=N4ls1k2,BK7.X>3/0CsH>
	@F!Run&dl(`n=e8Q12fd8Zn@S>4,]]NOt+Hb&!U5Uf,<Uef(aGQh_g_1[XVlRVH26#uDsiRt,.%]G>KT
	TE.]"%IsoK8C%8oOE.QS,D-!"O11*t[Q3ZQot>Ci$sj/*F`&&^mrn*iBW''RTQXOIQks''$3VKJjIY+=
	D<V:\_`Vn;dC7a+."1#Np''$49]mK#^/l>bo%>7Q$?[(iDP#eHC%O2Z,8F2/a`I<,AIKZ@]NBI$fHeZ[
	F''.*BB!&Jt)j[>6KPVP*(pWU[Uf:"424oiBQQY\sd#`Db[L>V5=4"9hrZNk5''LMU:8S(>\SGN[>KT0
	o/^P*S&NUi[F3h"H*CNL*eB/n^Z:s$$/80Z4d8#I^2C927Br2\$l>>]TP]Z)Q1fLn=t,hjUVM0h=J-RF
	lGs3)XrUop-LJ''_#)tmo!''>AOY!lMgfSqYVfVPb"UXrqhe!_;jX!HG5X*/OQOt''RjAo4U&f3!%[`F
	X\\l.?tT]MWseNE(93\lF3>"Z*jI=Dn7R"*&JEK-k/L52ht2*L1/A]I%bQqT2?h+Tk[u2)33IE^E)-D)
	Gk*-4LV:X,?*W1hT''h=anYH+Tm\`1gm%sGW;8qN,jG-''QI*heV#&XO:ZLj-1UplWKNrY=dRjECM,QQ
	=5.dL<QaRh8o!_<^@/.!`T>&1#(sjqPNi=D9k5$Yg9?c&:6r:f]j%P]Nb2oX:@:_1GBQ]QRY/d\*Ua2<
	0^9#M@aM;6"XY_?4\7[]&&pE`0$LU`Z61/68r.T=CL/O!f1(%^]B3jMZ-@F%S?it''1tnsEWfaTOM4Q=
	QokWrh:DC2oXRm3`&b=UEbVXla#ieoc9gIR%)G5B>7l`%bMZKf+Js;:/cG2pSN-7ecRL.p[AIhJb%,^P
	!W8#s]''uC51<j2WB70Wa["pm\#n5-8>*$0uM+?<&P$AksH*3LVjiX8_e%S6baV=i,$"s&:([M`kPSuL
	mAo2sUKV=U[Y;+uE./$"cCk^Hf.R2ii:<pCKdVNRu3NPb1bes5QoQi''iUcR1@0<ib,Z:Y\pa@%*Uo-u
	oNH0MsQrV+M"7OTe&daP[WEDft,+"&TGg.T)RPk,5hl-u,r4=tLZ1e^7[9&nKp+m]L=1aC+4k&^''Xtc
	Qos,P=j%a.V(50V>K._5.cbd5l5g9K=W`YYZ_?L,Rs&W@%MpCIR?^trXHXb%%9"H:p!,=iCuK+:nt.ja
	`*s-;MUbl\L_4([OLt"Ymd]+4m*^X"U-6/HGF-NRbV`ln-fX7U4F^S8i,Q%-<cd6DaHc!S?@=o[;>:5G
	:/gL.X/RL-cbg:P=TP''a7D0d.TU05CA+!:D_O^0]%;k:Gb=Ut!e!:QO0-5ucdg=1Xu@3-DPoI#0BS*_
	muh"b._1D1bUWj:\3S>id?3/l)/''U+]98*gcP]^T<T*rqLnWhliJf?.E<`03X]XDeV7=[d9sbkYnV!,
	Ba^AMEY3HN%*<I)5A#ch.Pe=iJ)^CNXb''4!B83\b&@&6:1-IC=QijBUO*GLZM2H8Ep6olg>f#9C"i9K
	+!Rc%+J9P`<VV<7AmHTo2V/N4$tR)_VSBHee2PUogG@"?h-d$uOM#);(j4bE_P82@grP([Crdj+AkJ1q
	PIZgurU#NQ:W&7co@2s\nq]<83ej$e6!kUa;j]tMTk.1[;,[cc[7dGCI`_fOWcm*\qDdlo5aL98U2E^s
	?#;SCl+N)?Nr<7aJAj((39W;#$1j`8`=DJ/3_f+_d;j]#_M"+Ug:Omc*]ejc<k(>p&U#PUnQR[uu"B:q
	hb3?aspq.dpKr_i:SIu6<<ed@U6j.^j._QN&2"scn3O?qGb#5Ya5[*k_.Wjl[4QPGWJ<6_$I,QnEAD_M
	0c,K?uVWsMd:_LWAVIa0)*\s.FDRW((F6TNI1aC,/GYFC5kEEFcAGTE(T[G=pc]bl@m50LB\DMTer+G)
	:Wk)lid=-E-4,%hGqJ^aJhAI/h-%cJS$Tn<Tu1ViN=s!''pMJq84]=:;Hb;B3%)e4`#K42m+h[4<W6>:
	Q8IbI2+rMWq9lg<C;A@HqO.L2"?Jq;->2E-(,J1bCT4*j!T@-p%`sJN"FIdf^Pqe#1l:d7l1,)>IVA`G
	.MJi#&2q\9XDi7b+FPLGte\GcQ&UHUe9q#kCBjSq3;s2pr#iFa.%Ap:S9V+G5=d+9Rm`$/Uc;B8<%_2_
	N<].gZAigK5Ka-U+K=KSKtf0S/(,$m-A"8S''?H35onB\U5[K!;AYR#t0=Q5\VrC#qUB]G?SWo+6Fs><
	HmgQI:1efUV;".6F/HA#<ks[#^\''W.jY/u<1K*C?Wf5eadsBr;2t0f1#_);''=<jb1h7_`WsPk!.<43
	FqSc''@Pop4^60VJ*;WcWu=#/H7^Jq3aAkK7*m&*e\`UT<6/\9ePi*)PsT\/^7k2T%Bs07h.U^/a>O(V
	dd;"%[*3AoFAV)u(o!W5TtR]p3M=PR\s)UbF?0`MhIK=C^n2h;m'')H%OC8\GGH.A%TmQHmV?3a;;35l
	LU[aX,NUMMDW,k,@?p#rN>Bkgs95+)aZ?p%hRHm8u532*_,o?ZJ_POXhIJa+NI^_Gh''!2^)afGQp.pU
	LE$_cjg5(#-*G1-i%8KS94''1&u_84A?qkS93+hIr=Hfn08%RHBZaV!&.!s"fTHRPqkd''(1SCNg?`dJ
	0,>/Z0>0(4AJb0@;9"aNXF*)=e_W9Ss\ORW\mDWO`nd^2L0B\nmBjFOKkK><d8Bn?)UoN($m.]mioAnl
	^p16pJ^T!CBFO!$Jd@8bZpe81gl_U14-''*qT4H5EXe#IPj''FUX\M<>9U7L/$`!lLCgK7`;hGUtu[iM
	ZlX3&37.kq-(EmV7BsY$W6WHhJ^F(mUDtI`XZ@lD_EmmF7e?H,sC]NR2Q\>MQ''!rR$X[#s\GUWX37)N
	\%1:P#S[HS''Wr:Sj>nJLIq.p4TcH;MJ<3`''tliUffiB>RbfK9?1prJR6;3U>@ob9/Om_T%(.%QYV-A
	u2qUr#"-=#F)iGjA)B&&JU)"BjO@n:ocCk(H^''P_E<9/Osgm\ipo)KY9!<U^TBXFCa*@XE_/Xf)+01c
	(>$Fn8P_Ck:TAFt-jd,DRlGt,9S]3U"t4S#8OC;RtU$q.39.D:f;*tR_:V"*H6i#2H]od9H@>@Ef"<-D
	>m3)4#4SRm#q,+Qur-)92;//`-OAo"9tHFMFmB3LQ55mHUe<tO;25Xu"sTq$3-/XWB#(YY>YEX$I4-mg
	sf.02R6*iB5L7"0,+<jA/&H8epe_)8V>VCX[:X.@Mq\\''MaG)\99?J6MDTHY!MhBl//:ba_VEs>IeJD
	H!rh+0p''l@\aVaNDSoJJdR`)''ioIOF\uGXFk<E7Wt/VID2:8ck\ktFM6-A0jb*,4+d^5O=jhE^FGeJ
	^G[MmdV=A`p0i)Sfq8]a>9B#@J3-:.0Sn(3TIo)QQN2`P\ogu5rjba0b[=R"?)5U(bhK"-#M1)2@/eqP
	_+u26^@;C0ToQeEp1d^T!;;RF3\#hhF%$Hm5mBW>&DW6(+G&_e8rD^T"7Kc(%Z9FU@U1ImFLsE1XHWVR
	;UmG^`CVF/Ej/skOH`.WJf9qoOkLTaM[qLS!.>Nt!aD+03Vrg,KHOoXotgi6H_Y[n5mNYmLo)YeD++@l
	m@M=4o_''ARn%Hrjfh44nN[%RcRQ=?uid4(_@K]GS/.KNV8\%#%(PL46#pKIY:dT.:PDMqOGif>ejosC
	\AsZi)a4%i98mL-Kh#[`m-58:UT-U^T63@h;9m29IiYfh?\SN8E]a9W!/91seAFYe5(GWJ@c>/p)UfPC
	hGd"T3rG=QA<0U(QD(2Eg6FuspL!Bs,?R3ma@#[dCSBj&(+nJln_]L!MrXk6CkY9DJ6j.oB,"Nho"9UA
	Fg.5*p[*9h88,U;STD1Oj7#VM>B`d-.<B?;S(59E-Hq]VHLBadcKfbVnVptq2DFJUNI[Iol#+p00(K0A
	;@Tt:i''=R`.Lkd*cYT0p<JWCe--+hJZ3/#F7+p)us0bCBF?[s(h5hDag)6hs#)CF[eDfVZJD7k4!AA)
	^)W6HZ,Y7u>sp(Z-&o]K`9N"Bl*c#BqM6_t&T<m\5kM%$2G3(tTI,R,I+Z!u,a)\\T"Z4@>AR05q_;Yp
	]F`DVN$p\X%7L!0\;FuI]4$ke@0m^4INOc!0:Y5pb<=o$@^9nI*]io8>5@Kk5C,Lee=3VQS=IV#dkgs>
	q,GW%@cDjWR@D,^kZ"ALdV#?a@41C,cJ3@$kT"fR!nP`@qnK9l8i4u;`GKJfI@(J:r@EDS2%CR''LG*8
	@aMQM1j!kC.dE@UV2<!NAO\`^dHuZE?X71Q\XD;)Q@$ocmEL\8QuS6Q-BsIKGZ+WR:oY@q#>cqn.U.54
	@$d''`+bH!cq<rT\#nb<>#ls<5[7>6fP\+KPCS2iFnI,(tLtM*2\gi>gV$Y2+6pi_F!r),nM<\)puVpG
	m77''"DAp0H8bP[kZVME#)3O''PL*)JT#b:nhMni,jE`tY''th#^b"Jk$oT6o"1@MJK#"cZjJn0]1%JU
	D_mTO-J%\[WhL:J&W[eaCG#rkgjP/;Zd!XoLtKRoLEm4UKk16V*qi&Ifl4q>4@6''%bq5:.m$4_slgh(
	oVC#FVJ/_)(lQCY3b9DqY*F`r[>#&3@)sbsBrj''h_+h=[AK4T].:21(Y?(I!?H5M[l[4S.otJ#1Wk.2
	YHHNWT<[J6o/IM`f"8<53dB4Jqjb^!k`8@=pI/M-/;om\XbJeh+q#a,)p!;LN-8p^''OdC$V)LEF=fNS
	"e@S%IJFu)5ih4;P1Lmkpl/\$58;H%&;1?$)rPJJ.&35<G"BWZldF3*bGG:Vjoa2."N<anS''Y2Tc9Ma
	V`b7mF;$\)ES,mQ%:DAR3;Q[6/_u;E"("0L*6Cg%rep''9FlcOKLC?e^#,$%,>bIcQ^/s(Vt>tr\-l#.
	<i&j;V#JgRO>o4gU^8*LtE$l[`mmtp=YiL`\i9c8al[iTfHL86O8%IV()%''V/k*?0;H.obcsf;":K4d
	o_r;V^BSTJo/uNbE>4;KRXPRO[>83*"0fX?p><Qudr%+:&Xf,SNRW4G/*5%*ZZ&<kc#k+*TNI=Y?il4#
	\"Xc!8$2DjlhRKNpsMJ'')pRj:%,212rni9"''Z36THUshh3K1DXuiSB0.,<`jstQ,orhoeF*;Dcc=8M
	d1#YD!jBSII%s)!#1\B^i#.3Y*uGkXc"Dau=.BWXp1nLR%L2<(I"U+f:bug=DZ[B?6XU0[.KK`_dkuD9
	miF.L6?''I^cCO>\SXlb#F(_I&VZ3ADe"r#L1U+c.`Lm.p,6Nko$n+(8aIk#@*9#QIE-OA""NaY)W$XV
	Y25+F:%gGh5K0cb%pu:luMmZZ^3s>p1>"nu/>*NTo-MJiL"p7_dlVno\(boQ_N\uR/ni+Ii3&b<S0fU"
	:cQKN/N)^N:ScMnR$LJbJ5]e+NOg>&-Kk!B)1lm@G.;=(2hC>.]fI\*!@s?`:)9o0GkIo,c"r&%2E>(A
	**eHcjd^=mY''-q!Y;GHY+"qK)t,3@n^]&.26I\lk0NYOK,1,.Q0(e.[gAH:8CS\%omD@33bFK6+qX..
	4F];p>@BpZQdEI\H_f:iIb!g&iiWH8icYmSnlAfsDL6ic@D;]&9@SEC4^qFq_%q>T1D?C2;C1T90q=$0
	$68rr)''TZN)l)/so3R;AJ/dbN(>Yh''n=@>;=E:`m$b),u[!n_I7[%\Tc<D``VbYTXl(6dZRuf:I,/&
	''@Rp!g,5LUC>]a5gSBerC/XFRO95#j:1ETeS$g+Kku"Z(GpglMTQ5;=:G1A^QN>`05_F#?\AgY!AuHD
	LBsmOa)G"@HkXk9O>D,a`h=WcpH<=Nq6)]/ZOaZ%CK4%k3hR*#;Ff<$pJ!]3oaJ>m1.)8i?LI,SS9r+,
	]M<ee63cab>bDMDl8,@c89VsCooV!@SIJ@t+U6pBXC[eZ9J@%(!5t_&S2UV38e:''RJ0jpm1*ApaHPhb
	Vs*>4N5651Z(`R/@L_Y4f.X*L_!ilVGi#jeHJY0RE%`?nH3!L:h.I!dg]G:[Mo`-i\:OA<^J8G2e@eBu
	VKX`2(:"/1D1/g\4md\''T+$WHo5,]fadPbSX9)i`BF[(n]''''N&OM@/(r+E@\\3Q7-/;$\+3hA.;Jn
	9j-Q-GiUd,>R[7BPd,+i<48`B21]-''fRCU6E1?+)s%Jp\SJ!_1kU)YnS,:0-L-ud=.$ZTJID_mk<+Q8
	Jegs`75ub[N,O:`jeEIX%(*No,IkK[gJb;p&2k(0h]+(-<nCfg)J\F]H3PHA#q$d<mW7M/LbMR3Bcn%[
	6,LE"".Zt-fjDj:d74D9iR0n2oe/M0a)UrCer:Wad48TIJfZ4_)*OLb''),*#n4)GjH8R7#G#&XZRXhY
	oSB#pu7jUo#7[9)Y9^n6(G;pHZh7$I<-;Y?<6Xr_Y-taZ:g`p50_*DPe$.ISkBq)e`PpnS<D7P4Q:Z/0
	>OTEc;*''WbgoFO-;OP(gb,01nmg?O)]VU[t)8(`\d6e2@#`''>;^ScRiQG_<i2Fri$NG<,?9c*A-4<$
	e&nCC3"F#,0g0;EAh<d2M7C]<tc-,t<?toM#83AK@_UnnJ6P%02fk#BWr1?p0-p`0kj.\$8eEGGeB=nf
	%BH].bK.:n0^o6q8Ok#PGreVq@q-\Bh-VY/NSJj);6Q''Y<,XiYcE0GXiCu&4Zq0>V3+M4,[s%O\Vg`?
	[2QMHI::]7WB!cJDH''h:HBu]Hd6""oF(l8d9%XigV13KQ\XGqboQclbCb8-\_n[3SgO@2^tsaR]RJ0s
	KeX/#/m*_S;S2r,:;<`49J,>;MOn_U$j;k]eniuSOg:-/H#MF"^WNpWW$''#D7/94'')sU9?Q+3F%!Q#
	f&P+B&h4pijrkDrp[T5kO!!qdmRle:V9(l6lHfG==5c#e!0&R-s>22OD:O=XD(D^c\I+q19/UCq?HoWZ
	/R)EbaN''R.?sjL6gU?tiK%C":j5klGf/d-D<c.>''jHI.Q''_c;RGT>"7]iF_;''d@^!,Pmk>nm+BCj
	UgYU-"R(pR-s.sHf<$RXF"HkQ[iunW$L)=A)->oOgLB8>a)<@tWdsp,G0$GMPE57%\"$.q-?AfEb#)K`
	O^YSEZQ0kfNb]''E8K]d(*GQ;7sB`_+h.t@Q2a-W1G^Vk+W-smO9jb$o7\@Mq`BT`jba;fHGJ2Sk\`<>
	A);_MkU_OuLZJYNiO\fG;gT5[bU%%`#N4_PF5@mDX,^1YN1PJrQ,_q`0!$og!JOi1\k`EgcE6Y*3IT2C
	hcO>=_e#r#Ap%HVtJ!.>M/p[Y@Z$l<:UU3=FL%QQ"%88ggP;JDZXMS/3LNor?sV$XOe,p.hr;`]N9gFV
	uu52Z%LGcjLr#QXQ''jV\oc:sL2ZY0O$RAYP1nr*;8YM;i=:"A`Kq^bsOu[Si^9IaRWGrT^+e)__i(OU
	h!okbnXW$oHWbNbW;k!&-B36ZOMU&E''7,#*a3TB`dc+EnAZ''phjd''_Ou\GL2R,m7RPj9JIL2e3.B(
	(dbF.Ir>"I3HCi4,4s_EG`\e=Ff`G;3lsY2R,aUs869Mnt`nC/.:[QAGqGrlOXY_G\./9nTk-]Gq2$(A
	[birHfpO"gC.T]+MNFbDT3hi(9i<fY;T^\(0>SIk2e)Z;:;f,JQIJKknLrIYs"0)ZS>$;?V(Heo+U53\
	tl[`^G=0]B/T$HjSHRl'';(@qs+Vu+),7qQ(c+\dL]anFSP`o\isBd4ZUB-8o?&_g$4rFM^l^gmSFa\a
	:*3]6lXf=5KLZ&?[ls".=lT&bm?G!;JB7Fnk[?j7epnIDL=>;XSg9[5ne48e="p\(caT1Z4cG*NdQ19K
	akSlVoY$EoarYeAsVH(Pi.QAs[hs0LBZ]CrUh5@\?,"4h5Hmf^t0RRe_HJq5?%l/.auX:kB,_-qm"#J:
	L1kjagsc0ElW/7*sP''*iIFFP/m6aT^T4)==g["qD*?9j''>>K^$5;=k!9eF:2=?"aUC..tTJe245ne$
	(&L6e3F^*&.OW_c[*?Gb3p^#Ds!*AIS1cHLio?)f2)5hm''3hE:FdncM-''rpk<n5HB^8OhY)rbX>(f9
	Gs5dP>]s]9b)rG-G*?+20c#5Z6]Dm`.&hiY<f9^d])2ap+?.^]@TY813JZM$9huV"=#h9&%DN[qS0Qce
	l0E1&''(V#ba\Y08^L0j+r)]4Nt-.fO+F_''D3cGKbohbh;=_F+,G!#?bX8%/9%A9AfOqj]<Xp7J3\-W
	gl0"b\S>&(m(!J\C)[-$[<KE:jon&H,B(T<%BA:G#K-4^_D8RI_R^.R!56as0k!%`3F?.KV*37PfL\1(
	KoN8jcr$3\jY''%7fe!LW;;n"8I"YoGbUo&(>^'']ml''h4;7H_RPW%H@5U?''2.6rSjT.,U\8TF-i.@
	oWqZn,76MCQsHDFua,kqrM8h0>+7N60/H*L6("B!ka3<Q?iJ@joF[cF-0''6)tR[:ImI[Ioc.M=>"a(d
	`1nCJqEcS.0rQ3_\14)_O!"T&@Kt8erB9CBQMcYAcDR`#G_IG6qYX--h8m4"<Ba2*pj\n5_&X0N`4C%5
	&;CkEHsA''6BPMM9*AaNeDf$*2m''E^NQgrD@-5"\QsO:BtqmdcVfa+"!+1Bc_+d''EX''$(!I-1/E>&
	Wr0\J5sS+&-KAX06_##E$#-0-Tf<0L>81($#qgWWMdE<HGBn-m[2-[4!''NWp]p=qhkZJ2;@no>6.mKk
	Z-m8_),i6WC!3K/3cE8B#;OE=BePF9KVtT#B-n$1bla%$H7_KBZq:eDan''a=pM,+aZ3+"SCUG"!+4)#
	(n#Zj5S<YG\94C)j64)ZVB4G7oUH1E@_OgO[(k[RD6,pcNb_]KuH>\cCegCS,r>S#F9B1))raOOCRF02
	RJWJiH9%]2TojB9q2RL!653"(PpB\96Suc8OG*$81fXS`''6r@5SM79W*/:,K77YX(WTjD5lh@H?2PYZ
	_*V6C6m`^sq6PWI1Kd(\!sCRA5K''CCr</-IOMQ!<U.@[/O?TPiO\j]+_\klIlsL[sX5ldi0q^,(0360
	U45"452#R\b!=k\G@h(]8aEmn<6)oA<#nu/[&L@SFa?nYbF=kW?Na>?)?Ho-OJo@Am$Z?^:dYjWAq>Hm
	''+35QI$j[D`J8bR7/LnS<lu5!@<BAR.QsKr5IJhUZ,Np"]hEg9K`2\3o3&7/iJE$lmU;"JfS:U)qOf6
	&[6/tqQ$g`Z]WWAfD+e<^gR..rq;GDtI:k,K\nNe"J!#a-F//\jO4u*C[0DROu<GLRd?#Z0_pZtKrhgD
	Y$kk6Vd!J-5.L_bSpF@Y''3lQ_$@N:`6''BHV1*mTS,E&Y?4roimerNpnW/aFIR%4H\j=ZAgV4$?-9e/
	p2gi&0R(p"<6)FEN_e9,S2cI>nUI0fO->Q)%>sS)>IVAC:8e^''@.QCo-#14X1^hqh8eOW63\7:j+u39
	"&f;T2T@[nlLn(<WiG?%if;ZT`h*`72+UODhjOCD9j,-3N+[Yo2/%aS!1W[I]a5Tr@7E@;O>A#/ra,-/
	9(0C!lU3qHDQ<[T`UM^)-FW2X4K<9_Lop6NWq+t3b/&Yr_&JNZ&XU6l4pVo@n/.94k9GUl*YJaC.5G\b
	*\s[J)2OG@3_CM)\N2Lj)%b[.M@7qod=76*>9F(:FWbK65%R%k@LJ23<0T)0gO67oE#V2NMc/`7gdtkT
	(+KjUS%s;YT[*FN<<P)H`e)HTR9FFUoB%d;6#j!@W?2k-k7`(mJ=]UOFlJJlhFUnlVWR:>I%F''$i6rl
	nKm<iApHfHA?Tk7,5RnAkGoLrZ"r`<D)mt.`a8f43f`C''Q^`PE#AinP7HXC?][u]^!X#YIS?Hl^f*/q
	5?gXKg%"sLI"jQ=;Hf%u7>cm0P5PbeCN,Et=K7jCcj_>a6Al>cpg(TA_]dZN`fO?2P8_J@Q^)=W3tJ-e
	V,fj$.rjC`a%l)p#ZPK?8[^LpW\C8/q'')?p<oFBfR5!4X_d>Y"E_;IiUh3Cc@/L^sl3?C:6,_rM65YX
	tFN1[r,1EG8CBB2cdsY.J-$V[l)bZVlrYW]P#Slh5FbgcR;#[OLB:D<jpp@A@\fh^[B4d(FrDXsTf_f0
	Il>:tjhMqdU#*6$JF$kF;Q?#''*Km70_3@0,obeW5%/S[`(^]mHg>(d4Xg7YI.6mkeX^OYdX-]Q:d''o
	8Us''Re(lnXK[a@FoXc&kTL9R9TS<k3cetJT!C8WC8Al!p?VMY*""iT/b0R7M)&_M9P%5RO_`7fr$p9t
	D#4=HN*"E27nmF/MQ,X/),M:4IX`lg/E\Tip&:suc(&0HBJO,52TUOdAi[3EH-+@F"dCDe&_t1Ph&;?\
	V2M0n:dVmGIW$XLLGZROt<Eh&^8!S\`8]np#HL%tan=*)21e`#n;ado\]p-O*%mq?`qm>Q5''"u5_3''
	,[)?2W?@%Nb-h0hMa9k5Q>E>dZhW8tqbnW.t4s+rll"SACoNaZWDE!,sG;1@W$`n)hA(2YBH^;4''S5N
	r.q:5LXt9EHCm.[^Ai1;-B*B]nF''0DIp05BRT7C];I7/#Y:+CFSL%#;Xf,3?b@WFDK%$\<9Ks;*(L4l
	k#''OVmiqol/uf4XM00)X/%M5P+*a=?N$T%K48%%>7\/0"G,b0mRnuW__0,^=J=83=.o)"G:Vh8u*%Hc
	_@>M,f!!M3t+K9H*i#''kPaLRX]/pdeN''4as;9d#'';45*<l[I"V"-dA2@(2gd&:428D:?q]KWNF\Eb
	%-"gp.WAVZAQ=C+*5^)I[9CTQ/GcUQUW=)9soW$**(48P<Y)>@s(S-GMMS.XnpnGG`*M^h!HQg*ee?9i
	\%(tC''*[#e3YE?XH*2uTA6ocM=7AW_4pRa!NdD&RpN(''5C:H1T^HQmqQe\o=1rfQ[k_\nNRfd@H)!u
	;jSOht]>LB6m!\P,]i$pP;a]foAj+,JkO8(H_9CYKhRgD3M-3nk1#N9*&.l_ZEXgZ]EMH6#MbmA3j>BD
	J#(gK<_?<T]#E$ka#^?R?lLqCSM-_arbb3O#Wg7cQ"+dHWfCDd=>D#mV@O!!D+I*Y:GQAU2''7a`Rk6t
	JE@O11LMncr*$^K[\n-fF#;+$^@n-g9L$4JCSs%RlNougj_3fiSP5$\oQF[&o0+-4N;1YZ&;H5e6Hc4V
	GVHF%`nc\#)@<qgMu9(qpOnqig_*=co1bdA7FoQ<l''8puuuQC?JphPk_*Mfe,ujW/j\:lsT?ZRfa.^q
	GUR=F7id7F!^9YSJ7[-;ZY-nb*da_>>\mP>5sTC"=%5N?G7tZ>n--cJDr*one7dRe6jeZg%RpK:T/&SU
	>?u9pZWtU\>Ale0utMlZpZ87!bo?9t+aK_gn,)LM>`&N;sJfB?hBN>*Kn[`f\)0&_2P2)$hYX&7k8>&k
	"Ue-+b070\>uHH#5(Gqf8m.c"=uPVHIt`o\RnhZe;ap>E)''K5rMKu;$12X$Aa5Q&3jem8rK5&Jp\6Kr
	e0bO]DL##cP],4=SO,I?VLeqrd&2+fhV]<QqtCJoBZBrie7Z]i_#j1??W[83)pp):[R^r]ZJLCZ[CPrF
	S]K)1T2n:HiGFg>T,3b4i[6Z7`k;Jg!Y0!/e4=m:#D6uRJ*d<9Q''RirZKs3%n[/PBTQ/!]UBBIC6H1E
	+-''uQJ5XqA;M66H^]esfWZ9..M!L>b]&''N2-<!-@l]l[;4u;Q(N\;Rrgq;[:=a=EO]-mI3Om28Ufk+
	i#S(l4P4"RL;Mt0MR''"8Y<CU:DhSAd05V$8(8^tA^3''CV+W)2OrQ+IR5*SAD([(l4X9-HC%!e:h$NN
	J!aP/#\5\<?//AQRo=5`Cd,>bLg]M[p(A+ZBWY*Mbg+jdR+Bl-6e+o!alg0_fr?4[_@5s\IQ4f8\PK9g
	oi-J2JJ561CuYNah`R`#p<k7S]kkPD_N;@7GMU:/kU##<kQ8A<Ekmbr,n:^O"7Eej''HR3YP%h$Oj&/C
	?oo@sH_!cH:Etj!j(dhieQ\>0RE;__?VftLWk<:0X''5D!W0pNCA6?Hme(qX<@G>r*UHstTH-Uq?''2Y
	mlTc2=dpg?4:.aFm<#[5op/ib]NPm08!dX1<!;qg"[4_3Im)ojQ@XSd*<9;eq`la=:t99Du`3uB;]h&>
	Ib:KLV3<O2<^5O7dXq/3Gm<[QX#/3V[qLYhQjoIBfG3&(sgL.eU]>"sfl8>=.TqBhF=ceqO9G"B>J3%>
	)(8!HEd_ZUas.iq/COhni''Q"l6>Z<cn%4ZrbO8;u#mH(pe2p"Nc<<)c4!6WGa$8o5"qX=dDB!WTbtf-
	1JnV,<=scp''iSJV=CB%Mf:G9csG\9at2"+I\6NR9Eft(XG_/M4_tl8Uf42]c#[0Uj?IL>8ejRo@f-3#
	)dNJZ.n#d(/te!en^8de5QX)XD-<mbKr08ZFmTqD:(C<Voj.HgO!Fp5,n!``M.:^M7ANT3$3E''+]@u0
	l<8S>hLl!6<bo''XZ&rHT")fl,o[E&AWEB."+hl9a0Yot1C9^;6K1<V_/+%L3Jeu)IHdhA3=''9@g]>6
	2WF6^H12b;H1^Fmge90)#P[!ZbUG!3oPQqZd_WjYgbL=CX''LB/Mc7Rq-aJQRDge:I-)Ef4QQdh[K=,''
	RH*_F).)$nbWm)3f3<Zs_4ngWX2;B8<$a]iuDPZGZ=e<R-kT:C4EQ8[K_WK0]["!oDsf-jPuL!-8A%_.
	nQKM6t''`O+7WecTm[6/;3@hJ..$)JG.$"&171n+?N^B1B\S?K7Pdl23nL9r(MHgX(,CM8[hCuK=I''c
	p5ELne=.@B2.>#Pq.;VRSEg?_H$c&sY0r<:=f?H/WJ<?8UQ`67/[\dFF5$eIEQ=/Y5_s''W&35@s!V''
	dRU1R*f5%fBs$gI2T]a=Zsi!fnFf?1u-.^''^Q5M]t,$YAUaQMR8XHXjs2D%Y)M2,CQ_h>WVIq_m85Wn
	sX48n-U=VZu7Y\Ypdej14C`dWVp$b@Xr@<bXO;V^R<;@YXGS(#G;jQ`:?hAfP_4A5E_:j+*aAG@BY6''
	uiLL2f]EH%!/F_RYaoe>=eD+X`C[N=4bF[C:g;-\s^9>:&&7`+DQ]^W\6o>7=Ye7f4''gt5V*`,S].T8
	&!Jf2d2D@HQj0stGWf!ke>GSHmmR9#I.!==Ou(1D>=s>:g=3e,oBP.fi^R[iG2#B9$mm#N0\#oA9L#GV
	?F-fl0q\1ENVtD6)t#bFiRU[<Y@K>CB5/genh_K_=m>=J&?"%\e8OMhV&AacTPB(K1l1L%4QuB`(,l8?
	"ql''6-4kgZ^,PWVa8*hFWUIrCH)<TSpnf_rR3ZEQa,RkG2$iP]jg):^-kinuP>AR/_g^lS)''nTdV<c
	sfNPqKbXp61d>aA6aW&I!FBs?%h@=tN[\e_[ZRGB9_!FR$Z>;hK*)$GpkllYCQ"*Wr*@=ml4KgP$8YcI
	,6`Rj''bU8Y''DC[:2qk.0@j]=8#!\j6?b#--Ir8B1qO1N8nH$T"SQ8n#0<,<[-g`U`*(OTG2N!5a:*k
	ZS*LKD;<F/1,`K8DLG,6S"`YY=rToS;/V50PE9l1@TJRe7Qk9f(G/V2]*OtgR_cEF<!7:8kP"jaicDNV
	>dK1l-4\2/lWUd#:_X6RYM:i(X''++90^u:WQu(%>l4_64Ij1cOfh,)3G^hYcN#4RWfV<JEX!9UNS=:5
	@0.<cVquCI2E''aW<B9j6hNU*jT9Y<Ahd''NPYK<A!?52"/Vmn&fk4*EL+bTdhk^E+1bKT!M!?rQBG"/
	'';jB[[_''[Vs<3R&F8hZ,Efik%W?.ad++J:Yf0iI70P+To?gHX3ub"`q*$?.W8u)CZ10R$mr$m#(@!W
	/6r"rbY6Z!0A)O0EH5!aD3N3mMtM*IYb-qato--*tB4=1C/"[:ArVNl>]PIil=e=<Bm"..C6$kg5XWnh
	eJ[QSHR%PG[$*F@t$UF)i(rR+*`E(Wam+O[&RXU/_5/#h6^NBok+dOJ)$L\@b@5PVu"]b)0g,lq5qA;R
	uUoJh2m0`jroATQLc^Z8OR\Q,-SsWf=q-P5rFo!TF&Q0Rc*cGIW<m$rL"&ufDMsuR-%3$]p/!pY3$FU%
	1o+^Xg-+9Ggt=Un''>aF[[q$PoD7E"ahFY.@t1.PT=sj)af;q4`*8O-.mOV;ggT1hLg1A3<%M[Db[[5q
	''[^VGiJ>/_rTIq;O[3sV#%<,LKaHV(j44t,3:7e%4ZaYMC0=PV4_0XNE-^ABS02`N75&2%*<=F$s''E
	4,!O>r>ePh*j_#nd2;W!/kap@&Gpr+In0tbell>bAmD0q!q)tZq/RN"(ab,4u\2(.Ei_u&^4oC=3rXN8
	J!AY2M\O;bQlI#pV+4KUN+F<''1K?a#<t=\-SjG''mFfc]3k*lR6CJV/f4OeOsbQ4&/c5b4Q0(:UGfn9
	1h<ifA_Xd,@_$6d''`''?%)emC;:<FjIeFY8^$`F5CUr/r2<p8i,*NTb@7%)4CUDhDSgYD=VJE:PY>+&
	e/MB8/<fDH6cY%#pmlieu%qb^K`&a.2"YaEt]Q#-q$*m>Cbo*<OQno!q%60aq\hFBP+@QLIV3$oW]tqL
	-m?Dh3\ZXYdbS+m#>fl(3K;G]?Mq>*[''1Af?aeD<k"pq^h#0=UC)*m''^-uO''U+ck.QpCLVgN+W.<R
	4+-/1Bo&j9=(d-N1V_N;sX(g;p''"0MQ+n(gcm;aU&6r)N#+.b`I''a>*M)tM>:[r[gIAqj9[9D1b9W4
	bLC@p1#50(4:`<ZdmF&FD4BC:gYe7$$C89Q25HB%D-=,pR2Rm$&L9>t_JV=D-5Z<kgRiYDP!(qjZ0HmA
	ACj/C/IM_PQZM[,?]LSaVfo=#Yl%l2:=Y"31d.`VfE*5umH#7c9Uas&C`ei#b[=4rZH+N.ua*FdO^D>"
	pM.&XXi1O=LKKP,XoaL-S#MIrD2N#kO!>ROZa?Ze\1\)(pJ?Hj2iY`/>3PVr2/rd<a;mo_2Wj@RA,3l4
	+V;NuLhYkV+8KZ#0,T2D\WXqQ@"@pTj0S%R`''0!HRiP6?c+/!C(cp5N#ISe^P3<OafYh.RXopSV/rir
	_(o@.NgcAh.0AN`9P$B*%^PF=FI6;+UbWHi*''>+-Btc!6U<h[EF:[)P5RFS:RB]\Qa=en<Ni:V*[YZJ
	2?)andejiD8edhP$1]#0;<>1''DNi]bV]@B7SHL)X:M%KM4IOEoWZ(S2e"@WRSSG92%mNTU/5Fio%+b2
	TO%ImdU(SI;66tVf<k9o:e+UfikQ)UX!J_Z.G=AHe"ZH:8ibs<GFipMh.H##ST$gCP=kQ.W3*@Len''`
	A`Q/&^qj<7.`DULhFT)C18:41pIh@*DPaD%h7P[$He2QpfLqLQ)/h#J)+6SRH3Pj;;ZJ$Jehc5&LsSg%
	Q;jRMd:;YSN;"gaP%tmP''H,5I$=k!2Y3Us$oa<=r?;7Afc(scu&UVu+`he\N8Z6oAWPI9;;OkD3pI9m
	oC@6`CE8p:S]GYouh>7@N?E(Qsn_Vo:-J)F^LiCjdlW=DOG7I4b0\In/$jQ0UK3#ZooVjttc[^0j,)@"
	bAl;gn%)fiRllD\KYPUr.DVaiP&WF:V@<=g3ea4)q_tCp;pg1B#H@2J]lFj@fjQ$&JXmI-ipr9[_f9gT
	D$!D8j6%K(W.nFTq]`''HbFDAZX3f7Pm(l/54J1q''L"2I0$!(fF]Y:Y(`mkrkkek@r^lViC7)op42DN
	<&0[4/*^C=E?;"2"iOd[U>n.3rPf@:?1sP7&Q<m/_J:ZF"k,$nAnK@7)8,C-WY9#Q2T\T[2G^DT9<U6u
	s3Ho/?k>^Vd+Y%M*%O[2uoCA%''QRdoA2VZq[bXrQUQ<mg''XC=PoRu(U#FQJd[8@Zql$#5$-Y0Sq.aH
	*BSeA!EC.n="9c.,U$OSXW1.kLk!7d[!jJW/D!Va.dNaiR3,^Sj2YVoL60Ac>D&''/<"n+!10=bIcj65
	ZRk1&/aZP2dggU5OJH\p(?/0&F5$tsVXmt;4]=t<!+9h6@C_0nTIn\-D?sc8t>''uY?p*(Og@*''JP)''
	#FVb]hFA-q#L)5Z$WaL4nZSfQNLY8H)N-bTOjc,QU;L%t9r@]L`S;WU.%*S*_5rf8$3uV!9ia*PH''QU
	J_deJ>,Q,RSY*<pRF$!KtS7IR.+NuLl.f,&X,H;3],@L?T\2V*$Aq]O15+UYYhU3oR,LKT,!,r<Z+0.%
	LGshgd)&p),q9"*]!F/]Yu[gI1m+-2ZZIG#@36Rc2b3dE$8c@QU16?QNA9@/A%e;(uBWC-mQ)''[PU:A
	JcF76V5#/G+@H`+G8X9lHk`0''SFes4jU<-Y4Xf%:J4-(IM<9se(0VJ%qsdac5$eD<!d`lmbm:6uJP.6
	a)Ml[#jQXe*KW)%BD)O@<7a=7d\kPEl&`liZOQq@K'')k_I''StSoUZ[U@Vb;BNlFI0b@5nmM9%FJQ[M
	F^*atj#M(78F07J8''-6]<t_28F5:TWf$keHG[K.0?4(TH2lrrjZZ4Y(:>g<6p*H>D8XLC=o[]SU]F1n
	H!k=)N#34Cc1Zb6o?;_,>Yu7-^0P:h3D1#J)o."''Kr/A*QP^iR4RO,2(EJV`E87!Jqb+umHu4g$pl3Z
	:.LU5%"ret,BGTdZ+q:!%!ona2TZUd%1r]s//XP]&SP^n]#oRbnoGY9JbqW"#)X1*+NfFmJ;u1+/Q/Di
	H(nQ[ce._i;iig15W''+`f:jrr3QS([fUD#DGQuk&l-/X$&^2,C47WT+IJaa8)8_YnoFnXX@*s`sRhUi
	d$L3AIJ]M#^UL8Y-gL@\tid95''mQY=qqJ1%aOXij]=@4hH)rM5>39E:a#,NdNeUja^"-G5BM-n_H6B$
	.@ibh11s2C''cg.''.+dX:9ZQ&^MY]J_!?Mo%OB928f\&EF8`5j;>`N9=$\]Yq@,.%W_)et^`F\;=Qe5
	jg]<(QJ_J%WG0..i`8`>0",""PBVae3ulh5rS]`/nZDP10s]@H3V`M)-n/n2aPlQrre.@h,Ai0%GPA[r
	=gLd,Pbi0cn7SuJTDE*2$$Wo%ZhE#O[3"SjU>\;65h1i(M;(&psJh>FFB.T00(JaeS3+<$/Yc1246Uf`
	jWqH)$!T"<8)VE[ud?t]h,H0@IWQ$4\bn[XH9<XFe:P-o9f"U>0\oVlpPs6#+HZlJJdqlTf`EpYBE!pP
	,rHg:W-^e^W;IgquO9#JA]E,0G$*00F$SQM@B36:sM<]nmMZ;U3$OM(4AjLq)W`r)H3O5b''Od1N"!Y_
	LXee<.fM_Y"q[KMjG@O*$PTCf%N8mWNNY81Q=f][#kSOs,G]$.\Q*u]V''FJE!qZnkWQ(hbG_X[(i^ZA
	Ze&_LGmufmC=%kIV<d,A_Z-$r9)DoGt*3)6O[4$qU<^g_":sltmTFlONH.0g&GlHCFkC%^lMk)5gZlq7
	;2+aMPp*DD&\acHe-:mZX-@?ItBb@/)$12\t4#NMPP<V#)+8V;<JLfeAC7V`PbtY*YDrO;oMoNBuHdFH
	C_bVIhn43sU%:n#%_-fZ*<$lpp2^T((.H160B/h3Wi?,QN4sqQ*bJ1rE0@hGhejT9n;P(`(q/?TgH\eG
	;URH]`MnlSoPSVr>1JM[Meio+/UDeP%OdD-F$X#1k(5&_2rH2S-^[5O8p^sV?l_RTp>CVr_"HSHe&95,
	fBaXHor)n@`ScJsSn:j?L9k`-&.o9$1UR.n1Y;/7(]i9;VJRGH<11knV+\/a@+9OASgj_0^fL!tEUhe@
	J##;h8)qN/!Dt]g;augXYf,Fh5K2Oi7/X-GB?Gndb5tDCA;!$5\3)!B@9qAZ#>8DJ7q$V]1XV(FuIO]X
	YIiOMlkS$X8"t1r,&W[bUH2r(ug7+boC''$VUcW6lnGQsrMq5$sAQA@gsWT:g3K,=<;D@^nWdl(s;]ZZ
	<-ks;DuSN=8>!KiD:Q"_O-K;pf$h0mR6(7i==`G%+DCCH!+n/ORS''7]NgaWqXS=2$u%6Q`AZnE7bD):
	g3LAtTaG0Pa[e<h]oUIm`HkP?`$ZD%(A_idl])+>\Ll1RS`#2-Ib^k^n&[_7bJ,_$''MK;m7Vt^HuI]A
	(-''pN[IZd+W-#)6o?.;[4$EF$mJJF>*$]-2@!%:2r[E#,?Ol/*I=b`D)N-.1K$s*^''PV_BH_17OMRK
	GS%tQ1!>0Ef!msBrdnFSF?''E''\r31r8kLdB(4kq)P9to6_cR5s"ae,''\,L''%GX]9Yg.R$T6M@5#e
	(ia''8OIh##Kua6T.7s%fX<o()f19O6aBFppos&k6oN<ktdct.Rf%^79TLCAMMC[uZ!>pXM$Q!EV=WQ_
	WEsA@8QVauk=5==#44F9iYefuoAc+K#d7%UhPAP@9Y@''#.M=+l*O"3h?kR@_+DjR0JKS]OL(2T^[!%_
	dPVBh4HcSML@63LTQS)?sr0>$abUA,HV0t[?HIYi:_ln4lpn?*Ah`HGg%V-^JA:t1jN1hRqr#[r''870
	^rH5R,U[!#kT+-kce/+JD;1m0;TB)\TIa)9Nm@na4CJd$C.71n>AIlo0Td0Ks^e"sOgA3E-M[ZML-&0Q
	XN;5mA?f1H$*c>^cdH6<$cHpVcCV''&nVnhl`Kj?m#q:XX6Hh/5"''t''$oM3&U4LF>*C!?m\o2?c`pb
	W!FI2RC@O6D%KQ9)D02T#f.9*5IJrqCfOj..%gJ]YJQ*;S@I>ZlTLEo+-'']X+TPO(0Ih1NVn1-`mM0&
	ed:=e`CJ1!7](PgVU*O.]\L8Kc,@$?/TA[o6AaI#d:KRFE.,F1.$qXtn8!uGQ1+qrq@KdHQB@AS5aDH(
	K+gCgPc!`;SVLi(9ER''u6;bI+,MmR9E!!RqUPAelT!5*D[=''CcK!!:K"Y(c$nIAdT+2J@;08fH?GTo
	9T6>9&"H\#;J8cdS@NQ.2`P%U!3l7%&C=)NE-]Di3eC6OHC"Q?m#e0=?89(BMW2Hko6jlmf7RnqkFm6r
	U#I>l_%aK].d`7I`^)0-lkH;c9RS-r''CM5QV*l:#%E)kJ4(),S7$9nHF+1sUUGlg''l^NiB<6?qor8+
	)+O34>[<UGV=`i"RSo93-6VWRL8BtVpFX:=37`5O%_F?nrE$qNPfte#@M.(&g84QRafC9%I5][G-%,O@
	e%$<\:<DDBfPDBnY1>ohP!A:<TVe%3]bUh)`9^qPLZbL*D2pdOW^4WoZlu-F@^P+4Mr*Pb`lSEc=51A>
	.H6%+kOdlI!5RaJ]!3<F%<kWS7<MY2%MnPQE#4&6Ji-k_^5R&]U$A5A,W[&>\A>J/N#&2N>Ouk"P[H=q
	L2[C)lW?E7H_PV((*+,PU#nQ1+Gee,Z1)PDs3J.YV%)gf`-YgS]^ol<Tp-Zg)T0OCOPdLF#0IG.sX;gF
	JK18lBXU8"F1OoW4W"C#P-6Nt3Hqc=",&"B?+0SHnY>8\Tok+4QR/-AHT@b[K$?g[rT7G[&nP8ZC0<Z2
	@EB"2mae!4VL7gim)g3tLAFRi2oHm,c4lWIKO9''0TN>#ia^fo1QX>?Pr,eRir>E()fDG6iV,q2S\0\6
	er5uJAu0tmi98`8MfK'']hBq''R%F#&i%=:)Ze.(M5r=NN+=q@llnK#S)DE%M2bRQQHQ5NYSdbBZQGUH
	4I"''BL"I#%?LUW4tnAenMsPmKX]*;V5)BcXoUZfRPUU(NY/bKZ.`ZL?MF^8_,E$>pro?)CH=&uo:=<W
	!GkEhSac''<@I1!T"uK3QDWs]OEjA"r?$qm,JL2an2$S"$#%B\Ig>GDe!:K[\`Kl5ZOZri42h1pJAQXp
	I''[Mr4_/''H^84b\TGRkHk2*q7u5R)0"Z$HAj+9HijSjA)@#5"BL(37Z)pD>po]s/krG@Vq=Q%j#r8T
	OtCTHk&m)2N1YOJC5IXAuAk.ife*$Q(B"ScS:SM#[MB]h=:Kg=eGi]bQ0Z%9#[ng/"JOF_!!)r#d#6#>
	@a6;[Nr<>4r[CKRu%^G_@i.AJEYD07B%(]Z3QXY-n#9h_iY`1J,P&*L>hA`dPAbl`t[e"`NK$pSJO0rQ
	uRtri;#DM*80RP-/Lc_`1T+#mpD>Y,K9K"uM3)g]TUJ6mIjp*El2D#EgbTpGa"Q_N8),ELWt`gMl%/YT
	sBgeSSkP"(&B2a\@ePSiI6u!,r.#aaN1X7+J$"L`$a,@N7WQL30L,rOcF*9H(<=53<$N<a9qZ6Yj4n+k
	_""eO#@t(F#>YZ8Ea<LW>^sFUmtZ7"tFm+km;_5LQqT-2(<K"2lpHg]^[!jh0F@-I/TRI''^i!es]-#C
	W[`DUI44kd2`-"X:fGnGcj-Xg?2OP)>"aInJS>\-T,OjZ]lop"Bb[7''U=@@.Q,[aS,mdHJ6P&qpLB@1
	f4!979Ea=#d/''Z,+W@o`C=:`46D8@AIE`\Y^;X7L^!Q;7n2TVR,0O@PHimt!c6agY,uX4:-K$[nlF2,
	+=-IV.0^[/d(,%4h8<2#8pGsq`<o_g1et8#+WsVE+L76XGSl#>C9`7CU=3-e\e:,50VJmO:R0Y+#R))5
	LK\ln6&`+NT"<1:_?uA:6@WF;7hC>c=3#O'''')+9cV=\C+eJnD%_]pHLdPI_\qcF_:U\@c062?C`)?s
	b6@9,YP`8->k)n/9T?!T:D[D?:rPXqhpFiO5B6Wf2AOd89(rpHq2<(LK@p5<>DgC^bREEU@p*)MQSZ@^
	hmi)#@.el1;EAL;i[&%6"L/OiSj\j+(bU;@"-_Y[:s3DW^B!2W76L-E)eo,b)U)"O/I(MQnnoY$Q>Ka*
	)9h*&cH]<SSmUIXm4n(HNt#[ud?fBOj=/V:u1[6kWVGJAZa_dud73-M>kFRo"''7S:Nd@!QQ/"%%t7:m
	t*WRcqi+bLSKZ!I[irF.tR-\cVYmeZni8)gFShUd![U/$$Ofl`!kKe<S8=J*QV`V!Mq3U&BhlJ;V$k<h
	''SVkB2u-im:u8)L=RppPHaHaF<1;WD<]%.dILd/+18XR47c)19PLSMQX"Co63LEuI"(''k6N2mZ!i2e
	r.pl8!j:#W6bQJ!pTU%l;:d=>.F%p^*`n9rWPK@s#,/\5A=p1V#KrgCX\=HQ`L^WG$:>YTBQL?3,$&fK
	2-50A5!(`!oR"58#nsBH=3XMep-9k*R"%mGTYp22k2\<5&(71O*1FTtR6:VK?nudjr4:j1iDLS$D74ET
	9%i!GaaAM%5Zu54WnF?\n1GC-NLSPP:_&<0:#nFn,Xqe:lKF0Nua<HoN35R&<W(0UQB&0>"rH-IT[-4#
	P[F\l!HWh5/Kr@qKG,$QHkcEUWXO/-/#1mR46Q[E;@<n!m^GJ">VL5d%Q*iJ1Q28+==UuOV>:a)>*RBk
	S#t%N<_.s]XBe5Ds?Ng=6"r3Cs\\;.j-4E7^iG54Jlo*9K/NWB*CUe_;D/?kk]r\VBVuA7i%(f0E5-Bm
	K,@:i0p"J%HXEIgD-?@W:,KT+?q#rXF^sW!(Ah]=i''UA/1N>]*''%RbFr!\2DmcAp+X#-jYZ9PVa<]4
	(r:05R9!*ouhc[P),tU7k:CF3=Q5)7.So#mlAX+:P3<clCnlbi&"I+mC?Un5)u=aW>EEJ.*cn\#bp*iC
	dgtSs2;?HdVQ4`H#ZTZR_OA^1c,B>JF''?cQ-e$*;3T6B+U&9>.%JEXn*cjSG`A3;HYiRqdO+o*p%T8Y
	rJnWOgh=A=2eme$IEj6b[Brf#eE"E&ZeA3gG8e8[eL9lC]aJ;^bU]XLu`:[K6?3g=$A3X^YO(5\(7Y;Z
	#23nUY`0kdo*F+[jJ/i4e$VIn5PkmY;bM#q2?:RVi]!X-tjoW"CV=!JO&R!VB_7-PiYC]39&miG2tJ-<
	?Mi+0P9g%e[X''0+kJ3P''r@d(AFuo2W^mk5''lT,fKNjNOFRbI=^f7<eV=Y%:O>9b(@&=fU8''ZYk)"
	_6O&VmZ741a2,915iIqraG3S6Oh*=^UUf6)h;ncF<I5[8SU#6a''VALGp1tFM]08RSJ&)[Hk:PZdO3.e
	`_7l4W,ik[B6l>XVT[Fc1:IiaUf^MKOo+R[0alQfV)PNRQ1L[!lp)<9ak''l+9IAH.d0X!?<bE$3gu05
	Af.GVDa03P^>m`_CF_Vpg&uJ=SJ=Y\4klV@SS(`p5brdY+=T=QMFjJ"[d6j6&?4#tA/Y#(5+,EMW$[lc
	ArqU01uUtNg]W^5TPc6=)8L&(LktU,J<=dbW#P;u"Sb^2D6&aYqP^Zd!HX4R4"eY34=7]C#o0)+R)''T
	/9a#RVCD2$_1n3D(dp6;q[l@j!D<nhj.!a"q!hFPuTWMRFldN@#GJ)JOEsl&B,C''GGhn>#I`Pk?D''m
	0`0U8M^Kc/%gSd>-T9PtI/nU%rl6$4L!N)=U7\2aWe48>nc[+#pihR).Ub,BKl,ZO%(:3G,K$H+k%oca
	M5IS^gN7^"Dd(D1D5#oB(+0f3"eqD*!UcAS8=#o4''$!%/&V==p@N<W0LNiaaBCl''j@*B&W"qM>k14X
	1_jVM9;G_qP)L%G;k''D*j1"pi;RY%j!r99UVpf@U#8E''_penQ=JN7UT(0aNg"TW?X!I,%+bq0#51\f
	T14p*T,Kd_K]109rQ8qSV<-?)C"Wj[]qLuQ4)OdiREEDJ_^eZKqJ)4]a4+L2F$FsYM:;LNUALn!<Wb5c
	g-:9g8&e]L25)%9^Q5LaQa2d]g9mEr>Og$DLs(YgsLi%4G8_F;To*!KM!X^9:PE<\qh^?=#*)[de2+;W
	BCI&#?t*&Ct]YVm$>BQ:VDTD-6KIW(aN??Ec`1sLq>K4&KEWmMbHPPV47=m9+3X"7oH^esK5.EUd%Ehk
	2U84=Y-#h+pYB:r3IV;i!6PVD;p0^W!Q%*"X(=*uHP,b#?E+t%JFgT0>:DW<,N$Ta3)F>-mCYT&59<oc
	?Ai=ZG*%ibe>>%E)^.=mKn(<<;r>aL<$KZDE9KRYqSa*.N@8k_06=9c^)7ic)Nbr&5eQdbWeAlg(,DZ-
	XQG!gA[46XT9eBa]-qEr_UEG&)XE(6p=kNDI1HZd(A<Ee]pFa?DpB/#3("$Q&\cJ;[eUE/426UtC8@41
	=K9I>Vp^"S-m2EShpR#>-,1N!8qLm;HSUc''B"gs[q]''kkn>Gi&4RfM+W3`l8?CC`.''0j''gFTXD@L
	VH*S@-akQgD)$(G6<0j0]e&[^E]/]qTlaC&]Hj4<cJ]8+=LIB>d''K6(4Ru+(og''!-dLaCJBj$mPTqX
	?P:2T?d(pqU?b/?Lf[gGl]pe6o"(j/kS=TRbuiW);T@!Q>q(:dWZAU*f$$m`\4=Jcabl?rR6$W''o0X\
	7NGcUN`c0?-aI4NKpOtlR.1lW^W4>^#P[XKqZ75hM.TP&p^&%cE#YW)<sjnH`10*g#r[+fl]JpfmspmN
	Q=gBkjqZdI]Cn`,;3t/L;98<#XiuMn8*]@$''F1!r5o(EM]X22hE(g.KHlURJ8jSp[&=Y$P3Zm*/bVZ1
	NU1@<l3sn.O<<)S)R0]#PJ''$g_dF3`#)V\tRZ.u)":V8LlnUjLM?luF_K?M;JM%cOUm=;Z/mANX[5k0
	]TS07(+ockNE!Fm,j:.$LgJAuO/68#^KasWqhUlQu!8CS,KtJ,F:BSK:6\/e^Qt:ej''7''c".c@*,;O
	Kj`@KmH*K-I#kZ3c3>7#Dl##oA)):Q"7SZs*8(\ZmV9CQl0X*T-GDa^eOXe^i\u4>(hZ1Y^@<&M"p_h-
	4CEh\9hF[_85lL\YX-l3qPf2nt5$"8?!#Gi`qM^O%!5U&p5A)$nqP$qK^m32i"qQAboKL&!F4n''40@&
	<AM(,$>,W:^K/@,*!4Dq6/>tfi]/%l6[(cJ9;(^g4#jL>AM8FQ.f/.Z%2eV_Wu4H%OCeDcJXQ;$s3W[N
	R''kHVB&PLQ.3*BSl0-!ks3oJ[L>($?INmNmDJBgb5TFdMpZ9G^6=UDD>M,/4BKRDLMjRUg#sR''+B?E
	+aVI\c#RGO"Um#4A"+`8\4j0KCk\//V8su\R0GR(_E$49gG3"b&Onl8_G*dp"ZFbq<Na''4L2r/?mNQV
	2lH"SdsrK=&^Q0%DE/t\9@[`]m8d;_f<qe#RQJYmAPe$._diQ,TV4eJh&3gCJC%G9R&C=k9A3.kA(9qW
	X3i[1msJQe>3k,GY_W$J.#)*KCl-;`&dN''Jr?AujAEe?.&kJNk''!m:rk?`LVQ]T9b(VF:_q@@:G2&D
	Q?h+D!E[[;IgXf3PUT;aZ*.9.M5\UTMS2_pUG*mRC)m[M*SBRDGRB/OQa\YaE45V0ZR1O1!Y;t6-UlQX
	p!9iXmPtfGcUVnYcu8?%d_k4Y?+N''j-#Z-*_LSJEbH-f+qD/%e6''qFC.ao]^$I)eCcYI^6=ssGPQDp
	]Xr[f(5@g8bbpGue[nc/!gT?BQp=EJah8"na[S\o&f@Kc"Mp42iZ9VI?LBQ-N+[ZN`I9Tj!0p^$%]%4r
	\50FifUdlBFo?c]kpFL`")em0MV,`FY].GXD`juJHh^GJ0fZYDcd>@''65RC,Jkp?B?6qhJbl9*4sC!@
	LH)T>>(2)JoH]l0Ena>0uZ;m=p)?-P[5I8U$K2TBuC/5Bhnj=%Kh_l76YgH`''>*>*t"!Q6&2@nbo=J<
	k[@>rNp(D%=/j-DD\8;h-YH@-lh&LcU0uT3=1NX6$?mCjiMV]$gQEAC@HYSt;\%^k$#Y]MCnh8m?9GOA
	mpSHB(!O+G+D2#p>E"0;fOZKaBIq[0]@K/7H\=2i\cC:js5=%-TuEElC/]J7tlsP;4!t!O.(V8.DMa-[
	!oq<g0`?Mu9Hd$pXe)`sfn_5qR`8@l(6''&Z!"E`H3C#$p`(6WA#?`\Rc<oRNPtGGP,0S''Z5#_h;Uo]
	Sek8e)0AJNUp!F2kO"QrQ65>Fm^4lV[8+H+E0Jra*2s92H7DNY\Re#,5sT559NqGV\k;-A0Vo0ncmf`q
	!S?''0#Xh"5(DJ4p(ujt"NTiGg??rhtkDOY%1TXBqJ[6DY4*"GVD*_2-PX=XX;>/tl1sREN3<=orMFpA
	V4YG8:dVCTo*/J>fKUY"-VPqFIU+a/o&XZH?HjdJB6?F^?K+RHnGcuW;?PFYDXGqHZ[e[q_^,qKU`i$g
	m@>^7WVGo.9$0&W4VPCu`>6Z''6#m8\=l%kOmOCN''/\nBusl_#BS$4m]&`KVa!>ag="qh`$NO&-ArpX
	?(GmjQtJ.btC=WhC\O(ZPp3C,*Fs''g;^5<[6%>,b<8SGuU1-V//J)\>]mL0Ge(ZZjMq,TK/]eSJ>E^%
	#=^_V<q>2;e&kt6>a?l0BNc2n5Q>An*-"2T$$M6D?<1+0Zu^8kTk\*,(?)Ng''"72>_"4,6;`SrW$jWX
	e:er6:UcGGJ,M9U"G&FPA9T^iFgelGk/OC>2c2I0-<2WEb$?,\''ZLOk+/A>^@g8scfJ0(h+P[+T:MO(
	3;1gg''MWK)Pn3u9,2KZUK[-r;KHa_D@U`l86.''-EHSfnt-^a73$Zr:q3JZ?\''3WcRci;$(a1D#/a$
	tmA]Sf5$G69fQ!09bC1[44@''S:(i`?86r:;_a6DVGd(2M:Bf?''-:%$="JJ-\E*"1WA#DmEE7,+QAO=
	KpWoWX[a7bDjQsqroFY41hf*3Nj"aNaln\LaH]T0+jXoKXS#)2pd:[85%,Za;KaFdqPN''b..;(O^c62
	#sA0@A''+Ji$LL`-EWo!f_sYs^I`<)bd4ft5sQMkgb$UnJG[)XuY6r]>S$b@:b;id`4&;H3m>V?^^D^d
	f3tJ91u\;+[K>:pUHp]7f2QfXMs]QpJqe;L"Qd-B6#U>d":!?^e\CX&6''F15me8AH8].ks6mg5`V(!j
	di''!h#n5(H`*0\4Hc/XUC=.ZWKpLA@Ko^MhNW*q"M%*u2?D<aU*Ro:n0!n_&aL\B9E]JXM*)Zr$p(2;
	R2&?0W^RLA:b*:p>)Pmp=1N6.JaaPm,#T=887iN7[Q"<(@hZM9KAgrS?Ys%;=Xb64Y_=`V#sW,WOK^%<
	$o#tW]*Y>Y>J]e&3.MiK"t7N%##2)8ms%OP-La>fb]IqAf`W"%''8U4=HX.,`(nWntU7Q,oXju-u:clX
	^JWlNs5e^EuSZ$e!M`PPPOo)-%12*\^0hI\l%-]+7''j!(QK=#HHm\I/Ws+a60njDk&rG@A2%/?/$MFn
	5fc4R@W!i@2CRp[<+hD12^]IC5pXAC?;jR^&2VO$hT_#MkX^[kS^Ae5md(j*^<9i>t!c,4O77%M0=&M\
	n`FPKQ#''8/gfZOHW5&\/4QAJ"g7o$$eKgMQ8(*OWRlb:ZM(SZ"?[0a!]r/NEs-Ph4G6""R>[cjC@@O>
	Y-&>Fh1eg&sk=#$PaDVPnTf>)Ti`SX]=;l41Phm$6Epg]Y]YCf@H)55(ACl"Yq:kg$L@-a(>A9Q2,+g''
	!!mr&6"I-l)aYm1PVu)6i^pM[Pe6%aCfm"q,\mor''Yp.1!ke.Ybo8R!EP#>.)H38CjoJ[,MMeE;+Gn\
	dNjcY<(LmhF68R\/ON0eQRuD\e>(C6j[!<7d$thN[3=7*]%3W">^3c+>GfdL]]@T(2:''j!fn3q;''hs
	E8/1q".uX+TemIERRPq>\h#nY3M)c,S.6Fga+cher,D&2)%\J;[A2%R)6spnZ38sn\l0J?\L,rg7-70s
	''+LXKJ[tY;]`bkjOSHH8+_QT.InlRgoAi^R56-OFM%NU[?$Mrd<:+c;_A%GmI)Far`lBAUhHM4`4l2=
	apA&0,]X%33%Y>`Vpe0`Pi;;_kC.G+^T:WSE26@*VTM]!(LA.Gu&/;%l[Wb6`U`r=k!n=Mb<]Id:lV%t
	CGAX*n0C=,up,0beB=-aDR-H-iN`?IfIPtnUk`\@$p<]N,_rd^dbkn=+;./=J7Q1Rj\D?BF`-jSa/P2K
	/HlK6H,,dRfS>Z<%)*ArN$,0(SF+_G\A4jfCA;6JN/f=^RBDC%I20Xf0=lW!c#NK0OYg4Y^W_ABD^QiU
	h\U9nR#l,Zh(@ua.Sr[_RnY\jM=9`UI&6oTZG.^AM''Z4*=/\,J/F6iO8@aZ9#WfD=qlQ<NY&QiY8T^^
	:l,6''X_=@e0lD,9[:9B1JP2[Zsc;>#+!A4B5]InL&^lAT-@UQM?;]+KscPV_)5nqp*1hcqOMJ#t\8MI
	VBgYAH3<uJ.pd5.''Gn+!A.`?h1Q>Cn!7iG)rDSeL)p\Mh6la1\f3WC?I].3`[i;-V`[K6n)ou;mJfSu
	+K67]Q$pkMeL!*fGe[YBZP_n^=U1;hq=fZ2k8uKC4>a''V,(Qe+''1BJBi`EY6+1DX_a51$DE!?;2!H]
	"+%hH!I2#U4_Rc7&U&[1FOZ%K3uW\l#^1Hb8iO<:AVqKg:@H[48f!@&J]>5V@"G,.58:+^pK1B@A4lN2
	WKE+''/)f''\qL&=KH3CY<Yb)Ab))]0=.p_mCu3^\?se0P8,Ue2Cr0<iBdIlqJAG9e]QRB''+=o4p+YX
	hYZe#rFJ:=m"%BM0r;6S>,:MJhAFN9PETJ\UWe:6<4*K9Q(_7B-@#KjkU&iXm''3&LNE?CqdB.<a_\a?
	?1):CFK._OEg+?A''*<tD#>)`9Xr8YimTAg[d@I3CH40TTDSBp+!&]Tn8&R,IAKOk/(:g!umWEQ*`_%k
	kara5Ka-S5Fl9i.)DNcL[Wk&_7N+XM+KISr?383GFLr:j^L,@KQNf`]<GBVohC"2@i6<2u5O4e$C]<aK
	f*+V%k-31TLhRc*mH32)CkOLL*:9WGL6.c:@nJf#^D)ps?u86?(q1,qCc+G#OsbJ<kC^Nc&R"r@?J9$\
	h0,SpbYHrdTi3?NmR!RYH2KQgal(7k(7S]td?$pK+p_+&C7I##:oY/s<2O&T!@;_<)_Se4nY)''%@S%7
	_2e"Q_fZf`\$Y&KQIIik.G6m2fEXa%*eZO)-b%LNreHSim_P:Mic($Z!di`;=ko*Y7YWDLrKW3%BqF"W
	bSC->bbT@.Gd--/]o5NdAQ:Lb&YkYO2dgmoTcJWGSM(0!^k\DqTsbQq:7;-U`CG5s#_%a,Xs(68"&3j&
	Zo+g\G&,WGbW-nb%V#qPA0SWn3EX+"1,@%[qT:DBc&6$[<u$>/YX(,o9_H\I2R"D6ampoT\20q#rtNdF
	W_SP;OSc*;L[>naW84DCO@2`cUi%dnM:AaV8AQa\=ae!=QB9b''7V?Lf%g3D/8*;5AAhAl1kO&"os3&+
	crPGO?)3di''7E""'',#GA.tP*2?BN`s7gb5TDuFGGPW$Q1<1A?''Fr''Yb._N*aQOd''f`\7Cd"I[Bc
	n]im1[4gLk4f^]F**2ioCWMNF6X"H2o(J%)pDjZJS`1X+gK=k-E*Y:e/8[A_D4*nrO+/+O)W<.k!8Qr0
	?_0&\9%;oXhEZ8CD639`+&uX+ZqQshit+-JZ,PuOn__TW5PXSQk>t&n8UjT4+D:n8HVqL6btu@jhot/T
	MS3j''4eo^;8FZ"/=Bs.3\&L5,=jE+#G@%X.gY-!/$Mj#2_FS75:8,D="IqF$fAdEZZdWe]L.!$qfoCg
	IB\;<Wggf&dZ7"PLL3p]Jhn85C]`cUOBE#d=I[)-Lj1&bH+Sh<#r1?:l?.UI;F&#pI05@!AqE,VZ6En:
	cQIq`QMqT6Nf:k&nMt;.Mb\gL?"RWnGGZL*rq]o$ia#*%#Or1(RfEh%V!I9ddo;aP))P@WKZ&G/F''"+
	$hs]lUjr=6_^2Ba[U<`BoFA4r\[Gos6[#^8L`]nTB6.FpLK1*4=fPF-L_*JRKb@)#)1D"\L\`!2NnGN4
	]F.D+dCY-<c5J6W:&MjSa7uD2FkR0]?1a=^W''=''VjU`c:gl%h?:]o0<WC6NldCH*X7r#8Ror-eT$r_
	`B14\g+h-IjG68s(U.6''c7!3]!qU/5$NCg4W,%oCDDImHKm2Hp^$rQMuauI?W8Yn`njZh4UD1NP@jWq
	"X=:[F/Qq"HlKp>)Oj1V^[>I^;:aY3FNuV(@9,D*9tsqcfOK#J%`k&ra38iTCi_Vc[sgQL`Sc-_S9];p
	^E0:"p.p=#m-5Nl1$2m;:#/)BcrZ)pNEFJpd^P%B_qX'')nc826!M$Gq8k=a;P]]l*=t2_6''ee_&@<\
	4-8bS^F-fR5]!1@''\jc@``@gZjd#>[.g]aguMu9p-nAET86L1K>hIAeQS5"6)ZO,&3,-hNV1QZ_mbQR
	)E;,e/H7eg=rhEN(Aksqi40B27q3=ll"/.$NcSX;\e!5a%=&MmhZKZHi6hK:<GB^1ZoR89@`ISul_M6&
	cZl&iV%H<R#EkR/F)@I+NNr"Dle''.a*1XgpiEo`D-/NtD/5KX?:aHiS*iebqN2f`\hsPK+)Fe`*q&br
	&>LPpl8s^-Yjm24.Ve"r`=6@7in(5''Q=rlQ3-2N"qU#!`;t0''KC^G`u-ut-,NWH-0Wf%!CYnEP>Lh2
	RX4Im]^qMAbfUGV?+?"X0Ja@u0GEY5;Dk=".`E8WA3]gN9Wo]"ZkCS$MWU0tR#udTlVDZ?dSjucLtcXo
	U7+k7Q)dNU0U%kie*8Tl:n+,D?:&hroCH^mrHf985$oW0I]nI4L/j/LrG1Se<tIg07V!BA)$3ZUr<7(m
	+qQ36$/m\%aOJJ3PYV2L.-O3j7Ag)T/7OoS"pt[2/F*s*3Fo0K!>JV>4^<\D6u\7dHB$H+6''Z3kX''N
	Wm3nTO3KumA(nl%mESreGR2EsEch)%:t=:la4UVuBt0a"b+cbc`A*EcDfOqj7&V&*@qMG5d$68<IO57A
	uG4phh+U8s*CU_<T<^,(^!K@iEH_ZXUBQG=)OfOVAr[Njf@A7?Hc.hKlHpIG"D+#PcEG,\)<8o"g4+gM
	N8Ba1p[jiR]cZ5sWP8!cm0-<sCT1EKFB1XdVPiBF''ONb$7=Pts-s5V0mF!D)V%C!6Pm+s\HcN_4.T-F
	*r7C(!IGnTj52n0%N6GZN6`<TF`+[4soI-HV?"`2H3f*KIJADSeIGmX]2+`''":A2hrX''-?SE%B(1$V
	6ViTt^o-p2@sEq5''DFIDhfg_?GZDUXRVI\rTioVAT0\@C4R_A9Q9k9S37C_WY[@:AX>_;fJbn@l#UFq
	k4rA<>#!0:BOiA`?B\gXF!f+[_)Mo=WTcQfcb06BMP3aE\Wb$_:KM^g2=-uj5hcI0Q;1bu=V>>5*$orP
	A.>@u&(rC0B-sPqE6Ib_lZ''eu_(RGit1O)!/@2UJh!-)bYFo#dtmrNf!p=r@BoZ5:aRel0oc&)k7W(`
	;)Lbd8+,`n*^/VCu)9^$#KXO,uXA<UAf[ZI=e)V<n3&hsKIH&,CW/=W;/9#q<b1L#521-;cj3Wssh./e
	%qTt''&Kk1QfRT3njs1[>EHJYhT_$6CnO'''',>ZV+2:,`+)`F8/@QA+Ghn)%/4Kg6_%>-0p^^rL&I^o
	o(M^XPZF_.bSq$5]6hne`,m:)%[.4&;@Z+26&/pm5gcT3mVO42Y_j''S01dH8]Sa(iM6#QlQiTGp*bD:
	*>,%+[9t5O/J[6TE_KV(D0ldpo-;NZX^/\H<&Yh%&3,NtLTbPPl([?kaX]7S''>6&,k^_2SWKBLO,%p0
	$&;<b#/W!Ys>"_[>W9V`k`ZeG=F`Xa<?p_2-"6Z&20a94+EW>%\binf)A+a+Mu1d<B<(V0oH;ieKr$@A
	9VVN:PY%uWunJ[hY6+SZ.WrK;$U!1=3B4H"#BO!,(pW^VX*SYK5YVZ:HFbkJTY[g\EG]nr*WVb2/fbC-
	KigQc%qY?#4.p7YJm%[!cNi2;8>(l)5)RY]*iMl:46Gm=nF2aEY2;UTUYgB=.b6.RYHRo5ucq)I`,2g7
	?KbP?EMTL5iC,EJ7K)*FpPW04,L3=m@_&IBS;OX&F5#QrmMKX-nK74?G?87LKKU''oW`Y;sEiO-LQ1r_
	<\AW<892?>(313,![3at/1?WLB1o^o,a1*3JPrSmqtp*SG8OC7\>I+EA.J!?/,4)TLX$c3be''F3E/=Q
	*Ac>>;;1*T1diL>%>Z)5[S4T,t&Q3m+''''0KCsRe6u](3ZcmEj6j-FK&=_PX\jYteBca&&A:NYHf_hH
	?;sGflJDZ0:5r''''/GlLaknZ^/&5p50e0c!"W=,aUjDn;i]i.*O-2Gb]R%YncudFX0>DRWQ(\s:(*''
	Qq5Y#lOb>%N`)gfn=RH_p#+rO,A>0Z''iIg.UiVE@WK`s!`?P,''EmF=$L:X2nH-BXgZ8OX44R[7ejlR
	A)cE%gL_b9^a(a5*cZOJ<^;G65&BlDO^jmt($BuA&PLqX8mdpWF.(''HnZ4r0?5tYVkk($Y?9iXVWj<Y
	++kqF59=mg.=REU2K$&X''L/]gc3!5QN\FnoH2&VpMN+TdHN1Z44p28_&,ND^=n+[$6+h2tP2id)U4\e
	9[`mJ^p\=0(&h''53:f<Xk#]oiJ^o0A&[-!F$]0DqIcl=>YUcpf'',#''*FCqRrE&QpC0X43.h?R<mi`
	G#?u8idulp+27.(W8+UfF3!E%g"CBVGQ5;sFEo;;&W^Ro4^`AFHnmm2g,^Bn+Ce,rV-p0t=#*^IJLY7f
	hetO^3,saj&l+.0J[-#S+DO[Ul7&"L+E-/G&FOZP4b@!:7E$/I8<HW9@!CS\IrRu+Zhj3R)P(a''#+r?
	pnXDHLLT$?F:fb<91F2,"3"nDPoD9''8ec)*DEcVI$,m:b3Yo,$.(`k''GQS3ba`6>n;a''<l&`0SjEK
	6kaZSjqkT8K>CiWS"''JNJQ1;Q,7n/''o;P`*Xf`aUPJ''o63_`qP-=q,On*XqEaPh;>IT''S@C:2mOM
	Da*h[ILuD/6(_^?c%F\8-'',J85De#TZ7n*Ln[e+5ZHPB;e?*L0nY`9f-[:R$(5!?PYt(+@Y@0-,>:VJ
	1mWT.)*gd[b9_rq!ta`.d:pAEUO,#Fmu:aPFe_gUXc=lp``[M@OY>($I:$?+d?mn8NqtUp+5koM[G:ap
	C[9t(13.<.?r:''ngl[NelEJ`#a%GdN[ThI''X''dIc[51?*7Vdl;8?+$`@Q<&Z)6!<@;4E(JL`nc8Rb
	)KVf''b.ZJ_"^ole:s>>1^C":+!A[IUCj(dUZCZ(WXk0$?AnLep^U@^V"G)a+r#/,i_092-cBi"?S;L+
	;C(hdueI45spX_-<RrLD1_,NAp>p)73JRuOdL^Bn1;pr!t-4_YrO5k$t:DqYHVB/(bb6BVsE_eUX-&_T
	sSMrh\P=E?(Vs,(Tm(^k#_HY]_tga`''TI;Bi<`HXM>_X>N:"7?L0#Hg:Dd_:TNKh2Q^m<Wt[EC:@'')
	;)=R&g&;[tqK_$([5Yrb]P=_916C$c%bU/G#i8/(l/]0a_Xf0-"2<kPYfakKNlZp*^5$5:#bFYRIB1pE
	Pc,u@4/RM[c8)XQ:qLC4UL*ZYHiXA5HksAC;-!`%_%\Zm\#<A9H5i"3-X]B@-;djVb)S]j[C(Z-U!KL&
	W/MaR4$"jegr;+,kFCHn2^2A&=`MGZjQGq<lDre%dFLTdZ7b9fTUX!8W.ZDrjlNe$l??\Lf$74;JXA,J
	U=''Cf*`P''elSVGP>cf;aR2/^=jk%SW<iGk;\&-6UgF<glQc"8_qSLpt_6^fJ:N#B:H0a!__eP?@Dp%
	gLN\pf"\-k>"GrNu2`c.`+@O4K<E:n.@<M63Cbp$,e''o$L$O+JJi:4NLS*V2/Uqan''KHYNO4.L$+j0
	**L)q1fZ-e0DGs)$9pg6#[g/Y;iB>.>"k5c,]j\PKP;d=%)dsql,Ja:$FNfLdk+n^\\9V4]jfAV=6#5\
	X"4''hY+OiOe/;AR64"72S9=,#G2rG=dagnLU[f+e!6D=(f#Gs\Y86M(GJ%g/UN+Q-KL<p6"86R[<I+u
	`1t/i=+IaM&CSp&!ie''%Z48RsD=j<OS]mXY^(3?jOPQG]>eRQd1Z5_%U''HPlSVK)2aa73348gkoH,Q
	Vp-+:m#V&\a$''#+PgYW\D?WRL-gLPhZBa&4t(g23X-HD.!o''VLk4mC]^S>K^_RS%DW[CGbndf1PdUb
	SpR&iG/XWq,hLN:\_),\O_(cIGInGjY(jYK.^(b,ib@g^mA59CSZUPU798>+mccd(FO7UBR\*bb/[TCW
	\$Oh6mG[H`]rppp(9XCW5TjAi%S(,)7k<pQ,_;Vc_X/hl$k@.+\c6Jt;uk+?dLF?@oYt>EnbkK%N\JM(
	?CF&Xk*;l$d*Se9$:<"<)r''TO(0rL[S\;.lVor1p0_f;HjJ^U;^/Ei6/!;5uN/eRl$on0<''IlOh_P`
	o0POt*.8Y!V,Z^LbcJjRkWAh(_FbdGIg^+BF`[@"<1d]s0)Ag#*Wd-C)3.&O.:b>tN=]]sKL:sjJU*VG
	<uB<Lu1kOZF9N\#gZ=0^(2s(It7HH_JuRIp0[EGAQej`tOm4*SC(>4\[K3+$maBYZ,RbJ:$A!@_!BV%P
	VmQi[DU@lR*)^l89IrFZdC0$u>Y#si917\Sst>3*Mj_\d"t=U)9/i+Uuma9SQqDPT:+]e>>i%9oH[N`U
	$s8>]4c/lh0SKd*L$gINoKfM)p?8gLfhO=b"ZG/RkpBse73LS%tOV\CPi8LBgb''^&5KK@&<TO\MKfAD
	g7Xr8(PoO.C^H?.d?c>[Q$jH>tkNqXq6R?`XU"DQP`+3q\)KZFdAQi)o*_fnlJ0[B$d.FLYm9ME1jC?+
	npVF7$()ea]#1#"87Wr/J;pmo4)-9C''\u6#;(e+''8_ADgB[i->uR:5se_D''%9^J9cu0CBKj[U>86q
	-5:Xt0@:`=aSO(ak`,7nP)=1hU@(j>>g[R8,KmmU]g4i@[7=aH6;-8"%eFI^51[#-obT$p3VU;[L^)n=
	NT]@F"JI(;M);g`Y$DM8i"#5H&c\.+b30RT8Ku&.EeKQp^cGok;.4o?.XZouaqNYW%>k)]qc*6*ZQR_^
	)jI`=B8nlm\>2/nfK7SoFEU?dVQ&Tc0>X-c>Vl+uS)6EP,dQV[b(9>u)\21;De[&]/H''"Ls/M]Q`9iK
	,-P,B\@2W6BW$(QQmH-%dn&^@Y8DXk-CaUa/tibVt=Du[So`X;''(#J1<&a>0-EcG3P-B_f"s/_591:%
	4@b-!r*^,03,;8sjrFSGI]VoiQ''_>3c7G"Ir3]V]ub/''''ZIX1LtT3-#!sW5o+DD;Q)tJ&.SrbF<n6
	]''[%U;,.pgY=Km#C$&,<U/1tkAr\3OMqqJ''!-ku%6-k7]_qTDkTH.Vm^O0>A;[+Z>`h5V/L"i^F$^K
	ZIg;"!Y7I`Inhj`2(f$EI`H(0Li4aX.;E4^=J<;u$jGPhH6r;s_T-m0-Ca_l?/@B>=S)O_qOA9oZh`S(
	:AKmb4dWf.4cn7*R4L(MNaX&i9<L*SF]k`)uDc=e&2O!apQ>$ISKfJl7mU/qdedUGtY=B`K_q-/TKimb
	sO!?fNkn_cN*0fhNtAU''?Csn/3MBoQ4Jp3!6%b#V?-H;9%J$[*I\D+&tCniQ1\^@$n,5<i=Dm&\lHAT
	!t:topsJg:N<X:Rr`0%6l0FBZI&3j(351]?Ib0J\ZTQSLXk?mDch&(!+Z`[<Vg@N.JTVq@@89-,tXIK=
	g^d%LpY`3n#hU@Xa[BALGR$+OmdEB%oEc3hRI5r/pWRLZ07K)-ZID??1.&`q+rE9inO\kM?lgLOiiSM"
	,Je"6t4>)CXH+83$^\?gd/aoR.?F)_u`G`5WW4-,IIhi_CQ7.&f+dWFb9(Dl%h"?I2iBsrkOT(Hk]pZ_
	/crf&A?3f7!XdoR]ps3e;2&M&W2p"_l"Hf?`@rp]sPOW!-BItL]V59,rgP,1F)(o1W*k@F;(;O#H_0?f
	n:></L&U^+E''(TkuN12S\Q.h#hOo%V]^:NQA*BUBQI/+lB^PnNCd.AV6M,je&^2FPg53*8sfc,-*:?M
	MaS]_j(k09.0\ljrMNWmdDmeK''5W1a1_]*c(c,"19oqc2LD_tf(<.Afnu^&*$Y6hSl?,/YCSViE4*5D
	Q<H2(]6JkcVf`Y2=:T;cB%,7,V$(4^k3_HX;oP=Rh1ouc@Dhj!R!r93&":sVA6e20+`t.iICSf>XV?$G
	_a=g:=Jt/+so8m&Y<+tUgc4lOC^,*l!8FdWTS&_/D8W\0+5lG%:Z3de*T^pKdkWE66@O/I6&G;+H7G^C
	l6EU'']QdND6s*$sGPF$eE/!e"q:i-((LmZ#oI)"hE,0hMT,bI/lRt.gORcN#BZuTm,*T&>*`rEDiNSP
	@ukC%jFH0>p=,@DBo!'';sGS=o*%+H=_*p_-XS*7H''Y;)aknCkAaeV!:9(!B]j6X''-d7_4OjMa8\rI
	!cDLuM\0tP$3XgVTX%;n"C/NUJp5i--5)re=1S\pRimn9rm@JSHDP(fZ<o4(kML!?p./)>J/qe2$m`Lh
	3H.@=O]BRIM=T!h=qtJk7Q06(o;JtMb=kDGB>A9r8Cc^l\^CF1\cU$r+d$mT2aJPt2n@80WJ''..TJjX
	1:V1fm1\e\%K''F]\\Nc\h(ZO)aT5O8^T0k14:8?-F"t$De"e-i^''B5C1b`7)K#o><U*BDSGB(udBD[
	n#]?;4iu`d@srE0gE59B6)>?K9NZ:MlR>[LBnB&gX1UJMS.UUO,0TIp+cS9Tg(giJcG6_XQH!PqkGHia
	9?5Yt#Oo7*NGG"B8&)3f7DOSIaC/8<VD3I@Uq2_:aL&/CZ#FKpTZdYqR=Nj$2UP#+SQZYFGmD+Er+GE3
	2R#:&(1>LuVdHV8Ab*S%reQc7cX*hmfTaj#IO''F],Wg+uiPU=_9KJjdZD61U)92JX@RVane&\Xrh2J2
	?Ci,9NeTepBkoa!4rP#;5b2PX8-2fr7+a0K_%085kkbdefq4j?o7h]B8"!f/O"gIpN7-gbWu''k:BK$L
	XAV4a>.N(V--WU;;H=f#E]sTk+sRYK:-X<-]LfeK<`GG0bIGK''^d(EY>\uD9`58R)\Q.Qt`arTs#cdM
	TXBJ:^,M_6AA.*%sPS9nqJOjns*r]/Z?+9[JB%QG"IOBhAN#`ef-@>kIc(h);"E4RiJW)jY''bc%b[1k
	kg<bn+Z)(;Mq0P#mjOWg>_C`1aSVp5g?:+,or[L?:5Rc+[<f`Vs<s)Q1UVJla[,afU4)-eB/i8OL@g19
	b!Qg)]2RYc)q!ENqQ*+a[iM;UC^9fOC5''7M-4!T[*5;O[ZE/eqY?_1B\8G5^15_lVCWrIDHOnhB2M.d
	o)R19O>*;b@_=J=8>a,o/:-%''(SAE6/R`PNe."-?ZeAiBRKs5l?^?++iVQ%rhfoT+QDQN^FSLNbHW:n
	fGYC!TFNDUN"UUp"JI3br''"k>XS\s`XKR(X;.X#A.oH6hYb+D"GS^odaeDL(nChSOoOChBV&]6Pb(,_
	!^nQ;:1iNr&nd;Zq>VFm''[&\sIA)h"[4&Y3oF^X3YiR)^2#''AChgPfL83(:DbU,Sort46!)O\AY8-K
	e0B0^jQ`udMRD&m(Bp*i)/*5dJX+,>O#iZZ,Uo^f2G=?bmH/Hbn?''1Ql":#I9n74!L@TNjuOE6@9E/r
	Q_`o9]Q#4Fs`GL`8S^i\I-cR"<,[9.^7_/!7Gr\OTdTW4X%.))N!].%X\:f`XMe`=.4dL%GYqlJ8ckYB
	lh(313I^E1gBmcnL:%50e<NO@=O%U;BT6#%(FYQq=F!_-?g)CD3-l-Bq^=hl(+*Iku-ZRZ]ZUUF99hT^
	Tr]K:fG+5u+Xr_(te`?q5*,/fr]&/V@Oi*ae>Ra^(>W0g"sh+!d["*].XlK;L3\8M,S?gX465Dtj##eh
	jR(2aP<f9#BX7Ro!ie"Cf6@qoRDg2^''+[Ll+=q+Q_62W*J1$mrQ''n=?#2`r8%U"bUUn''&a^eK`7M0
	c0u)''!UO@KFWtnE09(aTt0ZPo#92gigr:EX:/;&VW1h9+*?$4`adTt.hDBi1jrk;PXYI;Lnc9UK0ME6
	BFR:2''CF>f^i&J\;,J]/$3SAF*U08Qq@j(S^:#&+YkL-S0"LPP''MTC6lIH$DNnQGN;do[m3o)>mg^<
	,I5d&qUQVb*1N-ldN>TS6qn#ogS>2+JFG*$a!,oaX6r81FZ9Y14_5@at=af3'';(s`SLJf`N*YNfukhM
	$uVkUna5A"M;!WXq@doKE4aBPmCXHfQkI>)TaO(7Q@W,T\d<iogYe/&5(i;bD+F1LGL+amT0=D''DAb9
	$p,3Sl#q6(>H-aqY-p9Cq\I7:S,WQEsIuiG/6X#E*l-]!&HFM)Un>H0Gchn/N<4[.t&_0V6/iQn$LV[7
	LJ!ATZQ9gN8&:[Rf`3eWfKku>R1\cgQ7N=[h>)R]Os"T.ungr.T603`C#)=Ok)uAfcqkm%60n[6]ft_J
	l7W8Pd+mMj+jA0n3<ZH52)aqBUhdIE[1LV.Xs!DB2>6aho>$S]L+p[<Z>Wr\93+h+ABPPqdk:SKSlHd5
	XobOt"S];.roAe/H-K;"k1J]ZlYmfInLS&Q)Ho?b"b[<\U#*[&Dg$^To(3>!*oKW**)&u;a[r0g4`OUe
	"`mo=<Zd9T&Q!Wb;:%tQUc^WaLV\TO:D;&\$p30Z:%K#[U0>#&>++;,CbK9Nr=t@;8+$R2IkAAUkqMD,
	lrsB^dg1"BA`Qu;Z<t!NjkK]a`rLH\Q^,j;Ij7$L5[=p;!S\(`icT5-Li2<#.Hc;^Y]m.&*PI1.03Z1H
	)_te!VCZg%..0K2/kFLMSk3&r4f5rr7?r2#r*JGRqU"lh]n8#V!hVdeQkk%6?UAsNrr0i(/^SY[*CVPn
	GYNa[2CPo"0k3#%:g-ePEDEX-lhs00[2^^<LgN(AbM]W1b<nJZ:!GOBXe(/+JoSBp<;r?Ve&8jYp"lYS
	#6?=-X41)+J.CF0.8%H$aB^;8OO@DAIP)j<])K''ZH/<Un3Xl"L62]>q_6b[LmnF8sfU*fnsrQ=/j[QZ
	_Qq3<sqIa=tc)`]D?=!")C8WK"sOY:O*"JupApA(o1N=O@>,\HIS5f2R+kZ?aaLX:@F^QSN)JJfIQ]]\
	)p?Wa[QH_g=3o,_%Mnged15_!@\IO/bMrCV5k4pFo#dm4e_k7Z-?GX:E;LgTto^8FG)RuTq`L-ll-k#g
	qn.m220D>GbGdQ0E@r)_l]j?_'']oQtoFHcE1WoN]hq+[$c=%D<:$h2adH3j.dQHt%,es)ml1QWk4gHt
	)5>RZQ-%I;D4YWk0mW:`Z\/q#!%Ff!s,fVJ)+XApNufC]Ci[Pn*ttfR5$ID]RUoQS4NQa%`-&bK''%2[
	/MHf88E.oIj_@"[.Uk&>5-T0QZQE4[C.W@Uj''=SaXqEGqm_dQ(.k8eBtl?uf"J,$iTjYt;]/u`VQHo2
	1VOaNC2lnXP-mTin41#a^?=T]#jJNDi''Yucln5i$WceR''%(XM5O''BAAM^*V&5Irf5QHbc''[9bE5l
	Au`tSMs<RFmlsjnMK*+>HVr2"1$,2Zrk9t@2U0T3qR7TLA7Y5Q&I3''B<O7I^BCj/)JOUMPKq/''P6?B
	bRa_g+41+ZN\O\JMO3sXE^X=`fjLc;@UjI(H>B2>l#&/KGgH,>*mEH9+rc^q<#Bfknp&?>)cQHTPdU0P
	cdagkc=30<T:M/RrMf5ThrmM1GO''_3rLejaT9Sj]#Zj^u(43p=s@''2V_n`=C9?b1JL/&%1)[o[?Ij?
	ddmaDE\KXU+rDBA+^Ld.rq`1@#Gcr%I6P-ZW''tP6U9-(/G,`F*D9(Fnp<N,Q5T&Km`LJ9*_pq[9%a6%
	_Pi5<5''qfA)-:Mnk/oCi:]FMn0;`IBZs`0H=-88lc(!4C4(2r=/7?,4a"l&>Bi-fHQ]CS\?pB1^b:Zc
	%G^fiLfk"&lWG)IUYb.DMsnPgnMQAgE*-r7Rr%$7B^>TFSfFLoMu:cEmR&)*ridKcpWV<o>Ef!-Q;2=s
	l(/=''$edYNIdS<"fm"ilhoG,X6\aOtPh]gGXG4g`gt7g<roq8u+G7K$L("Igk;@.Zm@P049\J%=&^ag
	Y[CL]h&W>lF]HP&\qES4KY+CBDCYmU_Y7,/''d]ljqlM,^pq55G)!tOL,Ef;&@chMj9^>$!5n^l%\C\f
	peRXRsqI/6(%qdk9~>')
%
classmethod: R4PReport
imageLayoutGif
	^(ByteArray fromPackedString: 'Q4%FNC%!:PD,@_\@@@PBAHRB!DQBQLSB1BP"IJR")FQ"YNS"9APREIRR%EQRUMSR5CP2MKR2-GQ2]OS2=@0JCH2J#D1JSL3J3B0*KJ2*+F1*[N3*;A0ZGI2Z''E1ZWM3Z7C0:OK2:/G1:_O3:?@PFAHRF!DQFQLSF1BP&IJR&)FQ&YNS&9APVEIRV%EQVUMSV5CP6MKR6-GQ6]OS6=@0NCH2N#D1NSL3N3B0.KJ2.+F1.[N3.;A0^GI2^''E1^WM3^7C0>OK2>/G1>_O3>?J"<C1W''@@@A@@PA@@B4F@C&@@@X@@@@@@R0K0@U@@@@@@PD@ECLH@G;:& XF@@@@A#M8!$^:0B%F@Q7@J!1PD<Z@4>5I N[@M#>@G3?@GW?@@C?@BA:L@@8:PB!F@A7@@A2Z@@4E&"!!PA7] @@#0@@E H@@P@@LJ 1@AWN@@@V@@Q6@IP@1N\A4A @E @@])8@:C @P:D@-W\@=## @@G''@F X@@@@@G(_@C!"@JF!@G]7@JT$&CU"A 6![.17@@CQ@@@5@@@M[ C,@@@@*@@@:F @F@@@@K@@K1TA@@@@@@P@@@@1A@CN:P@VF@A6@@B89PC''P@@XG @@] @1=@CN^0@V.0A6 G3L? G;?6 X?0@@?0CM1@@^4@B%E A7]%BY@PD\4V"5E B[]''#>&DS?A$;?[ O?@EC0KPG''4F XEP@@] B@JDDVP DT-PQ6= @B@@@@@F @@@@@@E@@@@DA@F @@@@@@EPH@@C(@@@X@@@@@@A.H@CM@@@V@@A6@E@4@PD@@F @@@C@@@KY9@C):@@XF@H@@J@L@PK1@@AO@ @@@GL@3@@@>0@@FGL@@GD09PC)P@@XG @@] (BC@CN^0@V.@A6 J2(?.\M?1!P?0@@?0O(KP@X4@APEP@@]''D4^A(@"KT@P)/@@GD,&@C(A @X[ @@@GL@@@@A@@@@@@@@@GH#6SSN:ZDVFG]6@LLATL @@@ @@@L@@GD@C@@@P @@-P@@= BOTDDV@@DA@@P0@M@."Q%''#@I)R@Q&@BG9A@@@@@@@K@@@@@C)@R0AA0#?@@F @@@A DDPA!L*WL"0(\NGDBMJ''D"1(,VKFCM*7L"1(<^OHDNJGD''RHTJCAP\BXKC!0X$SG1:<.DF39(4GM''O*7L&SI,:^PHLJGT*4*MF#RIL*W\*4*]N''TJMJ''P+4)5B+MC><>GC 0(4MC@@ .OEB),.[C=K"''J&V+\2[[-&:UY/VKU&4]NWB1T,W[=2=[0O+EP18,NGB"@$+O+08L^OG#"L7'' 2Y,.SJ&B=+-,09\>_M''$NCG,69K]*;"''DFU&771M,OM6@@(OCB:8<KA19\6L6;]><[/(LKG?:[.OG"19L+W<:<._O''4JMK''4:=./W+6KM+I0:\]?_^K$?<?7C9 XIX8C?G>;3I_/5O5^7!V77OG/;:>/#%7<^_/;?>??8EBNB@@!YH8HDFI(#   (62NBCC$XH8XPRU$#!!Q[F99:F=OE''G4:7VRT[C&R9]LDK.Z&&(%)8''][W"".6>JI<J.YGU5)<.U ###3.:JNNPOXX9H=BE$''$$TL&ZZRRRB;)YIMPL"''%$5MFR^VUU&YY9YYX\#&##C_B2M\CN710 V,88SC[[/!]@I>[M['')EY125*PZ''O_M519;\N)699<:VRV''''74N^)N!!O9Y*I>GL))((8-F*."$$EK::JVHY.*())IV:"&&&8[ZJZ"#\&*)*Y>"R.*)(+IZZ*.)0/>:Z*2/K.()SWS>!B]0.@**&45=]&\^B&%9]\LI\_+$EUYXJZ-S''S[=Z.^4U05*+KP93Y%-,,X&"2./0GJ;;[_]_$/..K2VB6:::F([;+''/,!./.=;JV2>=9-*[K;;+<-.-/>KN.>:>@?][<K<GA71/0/@.+B;BC2-L\LPDX?/+_L,ZB>6!;BDKWE$CDZC,1Q;^\L@OI<148 L__K@C"".#]>1-.:E<00</1>SZ15>)-=XMY.H(7+SAW.)_4_$!KY?R=SDM:HANM2) 5H8^_RCUVE>-=]Q[P=655U2G;[WXXH=-]-%(?6^+5G/"!B^#[K>7<4=*8+@,17%:IQ,@,"G?!DLNJ<!4V5$>3YR2U1?@AE1LY0J-L$1&5&S&C#^5PE9?F&[^8^Y9]*:9991?K''+()HM.>."''%8;::**7''/+++LO.^.2436:;;K!??-;[[7O(7-5UW4@@@BJ7E620A"G@M0P  @AAA%*=#JJI\/W)6 T[_G,B''H&;A!M.C?10 0P0(N%1+#K3:_[:::G_.?/40]<>>>?SG;?=<<-\/?;7<9<?>.+3W0@A"K?=D]A?A&S_@AUX0O8]TG8KSE<C?<]@AC*0 !B\X@PSJDDI<.9M#O+ !''1E$;*9Y6:=Z=8E(+\@ATB@AP=XPV5 T9X9.^XGJQJO^FKR*M*\BF#"TX5+?1B2O+T=3UZ!$%(R%8#DI!;1"T)4X!R!2DP*R+FJT<0"E+]81R9Z<X-Z=FHX0\!EL(*1#FM,V,%J]"\O7^5802L@<OY7@8N$+B,?6@@@PCB^D>4 !+@AF&=^LIL]Z@1NJ_O!(]"4,1L QG0;DI>.[%^;7EG2$)[LYBT7"T%NZ+JS(O2$JC5IR$0FJ7=1$=$$66R_.+''IX"O$640H>X@E;MFP-SDABQ@@@Q]$C@XP&HDAL@@CET2@SG%4@PX@P@L[/N0F3.,A@#!P134J!D5J]M[.8''L7#&73&=(L)3_ER[I1&+N\:N1&N+''IS''B^T970[B\98>''N]]ZS''/NT93/5Z_?O_M:S''?#\93<G:,>B'')N[=D$(<DZV-?[E<W]+QL#K3+PBAXB@A509PU"RA0@^+@A''^;R(VEZ2  >L @TG$P4KE''@B6Y@@@C*8@DHP@(M+-Z5(NE6ZS).64:]%,6)@3R%O!>+S(O[4)4H-Z%JQ^%R#D)V)S7V*T).:UJ%B]Z)Q+Z)V*\+U+GXU[ %%JH_P!<DZ*T&NZ-3\AQZ2M0@4PC\A@@@KA''BA$<H @682B@)*\HDVCLPFI8#A[G)0 QJ(2PT?LH MV(J" +1 ^1?*WR%GF\+IV+Z2&JV,Y">;6\12=+N..1#/DM(9$ WO(P@ $_E6A1>NE*R#GZCIBU PSE+?=.BEYCEHBUI6 !Y@ @@O0@DH>E*[D(B@@2,0R@?JX"ZDWB.,F92 ?JX+7^)Z<K+T1Z95,</];W)W.>C-[''"?J=;2$/^<846/^]L''6RVB++6=Z,=Y9? 96S1 @1(EP@3DL9OWI$P6EG"I<-2DI+;] G$GNH5A_-@<L"4+-R]C$Q.1R.F+V-"*FM:*U3]\8P1?=\HZ;''BHP_1!C7NX1B\6,X!KOFK]YP2:?WO[P5\+PHG<@DTCFD EC%@[/ DSAB(A0KF@_BZ\G@@EDJ#&G8>5U&HY9@K".8%AC+@=&9;R,5#.+IYA.>T,\?''KW 93%4TIW<21=)S)L6EZ>6NPRI;I?0[D&<EL^@B@9ZH(\^E; V9/=(@L@H@DM:@AA@+0 _FTHKTO6BN/G//H1=Y03PH5ZC<#S^%IV3* %0Z() %ZZT1;^-NR?#R''L35*TX\Z5I5FMZ''#M%)@ZP:N1N,&GQD@@)NE;0C&,PE.GHBA.Y9 @RZ@@@<V(F\XTHB/IZ@A@BK0@ _H-PD+*@DCHFBBE3#/,[@9 DA:0NL7,#[EH5X1"%\L[!Z[.=3(I+^:1<5.\[,;7O@>=;*AR"4P% 2>\2-!Z%?)3XT&F BE_,$CZ.B<C"PNA0U!G @04H@WIAX@B# H@C#010&0 J[JX<DDO&@P(F7@J>ZYSXC@OFZR&53LI?</N\)W+/JV#:;UYWX5)XQG/O,,-C4PPL@I.''HWGTA@@S^Y PVT!0LA]LA;L@CACN L@!=89VPS\HA,BFBCDZPEA@#8H60^DHM)H>A$4UT/^-]K=+F[W^1(K7/Z3:;6-+O=;V.O>7*?/\C#!_VT<2UK'']PZ038F[W([BIE*S$X^N<[$YD,> L.4 +#D''V2E$M5MC^\(;7[GN=6V+?2;MW?9^W\><9#__N ?K?(J#3N,''F.#65B+V)''C=8B,$S3"CJJ31LT,9[!''^^9]+//^<?;7.=<=BA>X^%W*F:6:2%-S67XW8BR2^XS\W%YT[^)T%9+:6K>>=$>=_^-3??/^C???*,U_?_F[/?3(A3W*H<UNVBN_-K[CD18YM0E S,AM!\XM^&+%*/;S2/>3D(B*L(B2P(C<=7<HJH@E.H@G*H@MZH@@BHDIJHDNFHDLZHEWE''N?X1=)%%('')EXB-UA\TS 8D7"DX3I[A72>E70+6HH*>HH,BHL.>GJP5#+.Q7&=X6I0$Q.E9A*OQS%%0VL3NHP2VHP1^HQD"HQFZD+:$TUC)S;<)&Z0]D%JH7$>!BEX&BEY.HUZ6HU\>HU^FHY NHY"VHY$^HZ''U5(Y$1>G(3I1AC39E$A2VCW5(RI,0&>A,XQ:*HQ<&HQ>.H]?"G/L)2]$!V[RP#\^2BO0YB%,QAL*?5LSI;!( C"I_T"I W"I%)"I*KL#Z''!O74Y31TL(6RP:\/D@I9M/ZI"JY+"J*-"J+O"J+!"K,C"K,,"AYGD=LEE<4HU7,ZYFC_UM.;E8L1E%UEFL1''"L2I"L2+"L3M"L3*"LNHD,F>A0M9D[6$Q_P/P&0=M:<_U-=IXVN"@COD@CKL@BGGBN:I"N:+"N;M"N;/"N<A"O<#"O=E"O=''"O>I"O>+"O?M"O?/"O@A&P=< BL"@CAX@#59JC''VL(M9A7[HFJ+7LA,!E$IUFQE''&QFI&QF+&QGM&QG#$RAVD@25H,Z1P<6%!3-5IA@XPS3YLAFY@CFU@BAU@@L%&SMD&SI_= $3U)$3^Y$379$3)9$4CY$3:)$3.I$4 )$4NY$4X)%D4Y%C0I%TVY%CO9%D-Y%T@9%UNY%U^)%EI)%U0I%T:Y%VA)%VJ9%E(Y%TSY%U!)%FV9%&K)%VPI%67I%%II%4^Y%5+9$70Y%38I%B$P[CGEL13HO'')2G:9T 5FUEV,E@@.0@!^0@2.0@!-@&YYY&YT9&Y])&Y.Y&Y"9&ZC)&Y+9&Y(9&Y=9&)6Y&*"9&*+Y&*39&*8Y&;@9&;IY&;Q9&;ZY&;"9&;+Y&;39&;8Y''L@9''J&Y@BB @''CBE 0UVXXIGCN6E#-SY)JED@\ &P.0@Q, &]!I&]*I''] )&]6Y''R/?DI:UBY:T.Y7$:Y7^2Y7*JY;[^Y+02Y6W.Y+ZJY?(^Y'']^Y;6*Y>''VY?.VY;82Y>X:Y?4FZC7NZ@B.)?/ZZC6BZ@H>)8O2)<L>)?#ZZBXFZDK"*G9:Y?9>Y8D6)$\*)<!B*DJV*@O:*@UR*DYV*H)**C3>ZK.6Z@)6)?<.P@!@@@ZDB_M&S>SEHT^"C]K83.KUD6V.PL;$I5HZ*S-^ZS-6ZQL2*S*BZUH&*QJ2*S\^ZQO"*UT&*SY*ZUZN*UY.*U\B*UXV*U\B*[_:ZUO^*YT:*U(B*Y*>)5,V*Y..*Y0J*Y.J*]FV*]2^*]%**]S6*Y#6*]!>*^C**]&>*U6&*!+??*% )*''"1*''!?*(Y1*)V9*(WS*("!*''#W*$J1@CHJ@CZGE3X@VGI?%>9\QSYDTSA)F B7(@D6B$,W*$A3@CJ7@BJ;@@YF*$.[(@A3B^''%&+$;%3L6B+9;$AB7B]H7* H *#C:*]GW*"@_*!G&*_4R*#BD*-38*-4)*-3&*-7H*-<*&-8J*"7S*.7>*^4N**GX*.E<*_:6*.8"*^9J*.8Z*";,*.9_*.;=*.IA*/C * AG*#J,@9U:X!/F 7"+ !)^T^+]*/<K(CI9B]2P*,F?B+$%&!2R*''QU*]N3@C6EN,Q''(@2G*,Q7*]D,*/E=*/JG* @/."''.*)?=*2@U.!G?<Z,2(K+B7*,#XK,>VY,3DK,<5:,3>[(DDK,_]I-C(;,3-[,>[I,$,K-S\;-C8[-RIJ,3OZ,5HK-E";(SOJ+4[+*"&[,1,P@PAP,LWW[=/DI<?Y#_/G]3W1,BA:''"N@+B^0@225@1NP&R2E)A9;)TY**1,0@SN@''SO0*;&:@(W+*UZ;+\3:.FAK(-WJ+E9K-*8*+_UY.VH+.Y![)L5*.Y0+(9'';.Y-K-)*+.YI;.ZK+.Y@;-/)J.ZR[.*EK(ZO[.!$J-B$Z+<.Z.9<K,VZ[(<X''P #[R+5XF8\8U )E)K"KW3-PDBCPM2" @PU &+)*,[$:@1V[''QNKW1-0.B-@,-W?.Y:O"ZR.F;%E"99NB;W12[S(&;T2^;;)^+9E.:G*F;=V^;S2.;S6>;8:R;=W69%\>;?>6;;+*;;3>;<D/K=R"<C0R;[9";?7.9(RKLD@?KK/FPKGVR0;UYIV-(4_:C-%=+B^FY<3HGDJPPL;)+_H. CE>+42^0CI>&.E::,T";K\"[#;Z*EO6;..R:B]F9?W"+SG.+)@WK)M><MBJ;,4"<QHZ;9GSLQI;LP>CLUM7LO32\P<_K-OSK,@''KUZSJAAWKO_R\S<J)&?.8%/.9I)4(.*921I,3-2.982^98D(Y$KT@LR JN^^+^;D[:=^+!8N9%X"+4W^+]]:)<-"+M]?:3D?S/DB3+G,0/D@P.6$N3@UQ.^D%+I*+/@:?''HAV3D>D/I''<3H(Y3I(23E$63J=//E*^3I*23IW:/J@=3H=Y.4%D&0LCX-0H!Z*O)"*RRGH,2:@;(@D''V+/%(AHD@C"Y.Y1Z*,E8.7/3(C-SPBO3B^^3*8R8.%<I*250*0SQ/ELE*9*E/DV,3E9&3E9R3N,T/N::/N-)/E672 803NY_29<53E=R3O:43O<^2 =03OJ5+N;Y2 Y01VX&WPBV-3=]Y/7MR*&P/D5E&YIK,B,(E_D9@ACH@BHDA5@:B$OR@BR\\AK%A0^F,BF(4BO-@@I8.]84.:&.2/W''3@SE.9*O?Y3VH,46H,3@I,-D:[,#EM292\43\M5C/M,9=)43<M0SBM5D0M4D$=5D+-447M/-Z+2O;++1^J0PV; P2+G3_8UL#KM''-$&%.\WB 0*9KY@ KAT Y 0"@ @KH*WMY4DL&*I %1WCA<)KNJ+Q6:.9JK2I4<2/^J(X8,6OH:.U]\/#P:.8CM2A$Z1"8Z6N$:6IG-6H--.!V,6A[:.($-4I/=59%]3%8\-"/K.=)9(3%:]7SWKG!2U"NCP X=U''FL-D\:5)V9@B40O@X0@3,@SC:0-3&05 ,@Y2RP@@]P@GZ=@#X@@ 3P@BL0@AP@@28@3]=[/#+=4""*2I3\/;C<4O+?6;;^#]6I7L@23]6WWJS$S\/&/]7ZW]S+;];-C];(?]7%G]?5'']9G>=;2O[?]O]?"#\%U[JUFKZI&.=U!/V[A,86TM;0B!AOAOK[Y&W@&#@D=<K7]&0L!(@H!!5?&$PEH& IS! L04@FJ.0D5@@LLHKD3\@I5:<>[SJO5B.N+''J>3R>N .<L5S*<3+.NV;J4=[.N[">L7?,62J>QAS.RQZ>QE#.S\3NRO;^P27,SK*='']"]*#*M+5DX\N&R?.AS_JN<Q3OI$&KA@F,E/W6PHB4EY6QL0@8I83HEDL\QJGN; WHK[Z[\@L''N\D#N];+._96>^@?.^B7+"C3+>AS."H?6;(!Q;@;L/''"<;S$M;(_);(#N;(%J:5BF3^S;.  &W HA!Y6PRJH8L4*6O[KP.-A=@<%)&,KOVXM2@@B:\@JZCV@OB1BIF$IP0@6#[W^0Q$"#.ZMI7HK#7,,E/,6T7,16;,0*;,-Y/,3+;,3=;,4C;-4%;-577-OY7-;(/-6:;-)8/,5L;-76:;LU+Z9I*_AX5D ; ''+[5/V_8!?-RX&5''I!F,P3?0R^,.+3SLAA7@@GX@PHJ-,FZB$HE;+H-8BG8.72_*+W[F77:+(%2;)!0;1EE?)$5;1$V;)FB?1#<;1DZ?1E#?1H]?1HI?1E3?2"T;ZO8:]V&6C\B-#J@''/,_<"5 C 4>>I*0H1@"^;4-^9Q3$0@"& [@CP@Q,@Y1Q 7H[U*!Z0D#T0@RM @ @ @3L0@[&J*4I<=[F[=X>+=R7K=S/+=X7==U,?=%5O=&I_=&!?=&(_=&3_1V5O3$$\=5!/=&</=6C/=''"/,._^9A- 9X>2$!6$9\TKK^#4W''D<0F1^)/\W,1IPDKRVDC''P/WI=D''.4@AM 50)Q@;_Z/]/[89;?8*@_2ZG?>ZI_>*Q?>*N_>*Z/>*"?>*;_>+CO>+I_>*1K5@:L250-)C9E\=1(_A^TO*Y.&!@M@+8:@''9L&Q_P@B( F00P@PX@@2I0''Q6 @ OA@J@J@)WY@ [0T!#?(@HE4N>DGNN3G?./O?;&W?;(S?;*_?;+''?;,??;.W?9WWLBZV=B/%+X]6L/J=5O,L]._.>H@LVCF# TG]&0X^FCE# L3M&08\NN@0@,[U!!DV@HB#!T7K"1X8WCAC)H[RB98VEK%0YT&V[94F[O%SI 4Y][D^UN''SY89^>;4FQS(4I=E!Q(%^%Q)T*YH''R;%ZUK*01TV+UZ5^-W"PZ0[P(CP\NOAC[E$18*=<N@B6+M*18:=P@@@ [U/:7)<Z5Y/V[H@@EPE/@L+5(LWO*8@R]C$ !$WE5)</@@DBA\=U 3P@TCE@LXKS)#<CIH*5QV%SY=F''U+5Z-Z-W[>FGU/6?63Z-V7_1)5[=6;^-D-ZET05^M[!B5]D4@07+5.=^\.^\H8C@H;&4L.6MW,W;P6? ;$JC,889LJA$EL6''BE:H\''L_-5S:LG0H$FPUP>&U2&U)W;>>?77A?@?@PL$\D@CB4S0P@TSYGAAA1.D<DDIH:Q0P ,+1G@?+[3SZ$O!/%K! [[V."8;--!J22N9:K(.Q[:XZ3F/;#0$C"RE]+" -LUFT. @#D1B[@LGVD@A@!IT:BCH ,@[SL\NA1,-J2"%#GIJJ:/D$$(-+=02R2:?=CKLK,\D$40124S3SCWMYCOMM-]4L<85G:HS,IRL@3IO+0@HB:<RTWQNN[:$(6:;/_=F1H:/B;(K3,(]S%" HI@.&F@40N2L$*@IDB/-H9OR,6*$ 53:;SDNS1UNR*9RQWT+UU=-5UQVY7T55%U+)UUVWG_U-]][_XT555>GCYYWX&4-E%!$%1T66VN[Y]YXVD$"2ZC_!HL2606PB3EPE1OM"4R8U+3+@^,DOR/E[;$CPT_/;A1! 1MN6FFF1"1B*RSG9+O7 BT?N0B!"23]2,_RH.6JS(TWY+!!!1>FNFJII:Z8X(,/1#!##S_&.FNOO<X8T(S-07OJQ(EDK"3,U''9Q4^"&D<..D%$>,Z>?''!2M*4!YF&&AG%+((H\A^."! 1Y:FFF@(8/.(PF"Q4B:!!G?Q#A:: 8ZNC+)(X''&.&.//0X;[KGGI+-,,<=FN66556Z;[[__!#-..]/^^(QM/20.NACA7P9Q/UH\]:;,5B++67TE''YGJ8(B3\0XD@@CAO\$'');1222?GOGOMM>^<\<<?A3544T\''/WSSS4\=]\$'' =2/DJ)$L,,P>C2QY\C=MA]F:U"TN\[A2;WY3+3AGBE2B#C@@@W$%6^>^^^WU3;99:]G@X_)''8?>^.R359;:;+<GO73112^?_OOOQ3===]]''W7,XTHBAAA @\JB41Z=Z''E.>>69.<LK%J%R:QGR(0>$%\T8R7+.(4 LP8J@@E\AZB;@VPP%V\HHV+F@KMA#A?0-6DHL^5F@HM?!AD%9P!B@TXP)MVDHV^-BEKXS!B6TXP1+N4HX51NDM]Y!CG.;P!34D8 6MU(DJ:H@2*5+\<GX0N9TAK"5N9I.81+J;\(''K_3T#GG].!#]-;Z@C@L@@4X[V ZBQ$X1LN:LY4V!FM)XQ#V<,X1/%:LX84-FN[U3#GM%X-C7VLX=?=FL >W!GP@:2$F(TIBHM&\ 4K-JQ"(Q$H2MIRDYR<)FR1JP%I7''HSGIR$96,9BG!6@DS@L@B]\H_%?Y6JEXRK%3L"\-\5/JBB;3 WE!,6U%&EA"RLZ9JKP@@CIPF-APVL8QFL2X1%Y''LX"JS&RI49#NWJT5#Q/=3&-^4Y#YS*D5([-N[7PS''L[<)3''A*$I/$QJ\91:''N\&I3''^9,93''Y&T984%N^=Y1''O/D)0!( T0L@L@F&^.%K":R,]*3<D;+HP*#&ZL]/!''+K@P&6(^?,X@@ PD@KZ-B@A)@QZF3<*MEDR$03$''R$ITU)RD<*T)B2=JL!MR%JWR)S%-[4ZB8-6$-3:-F]F*6''M=T)S(TZUJK2]J!FKZ)O#:+T)@HU*T=%J%R]F%V*S-V*O<W*T*>*5Z06-Z-R?V)UOS+VC/RS#BZ@ @-.9J!TK#D9?WO%8^)"1U#2#"3*()%]3*IEN>D)NLOYB&B@"XF CTA)L(V)G&4*19\2U(??#V5#X%,JV\&R]ZZW)R1YL8-Y3WZV,9?][F =J5+P#-Z4)TT-ZUU;6-V&%+V/]R5*Q_*3A/3S@@9Q(DB)X%AX6,:7 9)NV-1R1[#.UVZ;1IHB ]LC@JB A$C+I3&]^[U$T-^Z>&2'']B$83V16EF/V?^96*P.4<IKW/N@=[7''Q.5;5-#^=;6T/_M4[W?+N5;;21V==<7-_?_ZW/??][8C=J6C/=+NTM! U7+J22)&Q)\D->-.J=+J.!EJXTPI]''ITF@DXIS%^CMY!@C)0V0Q(L(JLE&LE3V3@B\7:7@0L(Z0,L60B3Y%B]UZ.@AG?V  +85F$ZQ^X@"L!"YRKS2NP5?5)G5Z%$I"_Y2Q6F\)N#?FP)U9''JU992%*6,YR1/6\-]A#NW1_3%LX^Y3F\6\9+K/FX2E?&%&[EAXO>*7J(0LZF8=MM]73IEV]; A_3C,;^PJ27#MF+CFF"1Q3/Z Q3 @@D%FM(@'',-\EOPX1##]X@N(--F"[QQ(L<XZ\6D 0Q%T(F 0G$D?>5&A(T''PZT43<''][WFP%W46$-TX2RWE-Z56_]M^?=''V0^37,V0.;6LSN=[FU''V1&<7+Y3&86,IDM[V)K>=''V#+Z1,35-[E][6KQ&)0EL&]0LA4]??TL7\7(W.@@Z3%/;N6B6S"Z%27BX1A\\PP''<H,DRIK$A_%D1Q?<=SU8SD1NBTN.(4"(0:Z-EK-Z*;$DAP-6C#WHTZD5K=K[]/OD#\92]GP_914W.\YJG/NP#M7''JT[;2$;]\9R9''>\-%G''NZ013&4E94B>A<O<JX[IW.5/M^B/\2.>ZN ABN:8W!1IUC-1#FL.9(CV@@ %M+TF$0 LCUW&7XL_Z;A#.%LPP13#VQ>(WENU9Z"\/XX1AC[XL?&7&,0R57.,>:;.^5^=;1/''_+:+7/_I\58N_.=<C?7_BEQ?3 AW?702.><X1OONP]G7''BS=;#2AS7JP.]V4/)C527<9/Q@4^@!+;;Q[]#%;,B*2\<WZW)714A#Y%V 74G#]O?A(AOX5?? !+0G.DU(E*( 4[1(CT@ )2F@@BD''F*!ZSS8#D9ZBT1-Q"$S,?+^9+Z6,W=-;W_[>=4G_?[E3?71[9/<93]?>+^/?.>WG?-EM-+N+=PU/=+Y_8&",@@WF-3+''BC]I%(Y9MHVVSFU^$N4DNH(-JLZ/4@&,),_LX(AFZ@@EB@@FR"@B]  E6BA2\@@AT AR^.AGE@@>KE@/7BZB%B@7YFACFB1@$@ADO@AEN@@SYN194H?&9,9GJ29GNSAG_QAGPSBG 3BG1SBH"QBH-253C %UZF3 $(N]PM@JA*:?1F\FG$%.GHQ/,(YQ=D0^4L&**F._8NAGOB)#_("@BBZ@.B@?<!1C0;H@P%*C<&Q @8X QQ @_]@@A(@@@RHNC4LI+?  AHP&.Q#@@C  QI3LY)SL.,;,$U<L$Y\,$ZT1D"$QD"41D_DQB";QD7LQD_$1D?41EB\1D4T1T+,1ED<QULDQUQ\QUT,QUI$1U\\JWVR/=#AE-[#KPJJH E2DR+2,;-2LM?I)[TX-LBXE,@X@P9C0@8P1D:KGC[4"?$I)( +IQIH @YH@P(@ L)( ^P3 PXX@A=(+(7:I138P@\  L!)@P,@@AJH Q[H@A(@@P\PQL#!@QQ3MU0C/OAC/?W31?[+Q8A$O78\RO\S2G<$RHP42HP<2O[K-Q[HOB:4%NE ,O=@N[6YNXD?X;__DQDJ2;? @QK:,4VA^#7(:(@\H:LF^D[I&Y?HDQ(DFDLM:(DT@@@J(LLVJHDL,@@I^LG\Z0D\ H@H J@V&A44''A<Q:8FE@0@N$I+HJXC(<#A''<<P!)D(#+L(#/L*,-L*-1D*.5L*.?K#8@8@823@/\Z,&>"7''FC+!. E__@/?.;NGT(-"9C%VLTAMALLIB"X2OA(H>"HP0A(T@@@HXLO$08DRZH@\VDN,R38TZA)5''A*\7C\RZI7& !0X0I+A=D/-\#OI<<3J><3E@<7QEL7R_C3ROD7S)C3U-K3PSL7WWD7X[D7TCB?LF;]SFT&.NC^$Z9''Z(R/?\+EB#,R]OAO@K#&Y4& :ANP1CVH1A+0Z(P$Z,>,@DI :/1AL+H,=@Y ND[BABL@:1I0:#XJ A$">YLR(9M/C0V0Z0&01#!(+#&-H!)SO!ZSO I1O>:3O ,SO?]SO?%QH>=05,TR05Y.?+[B34GL0!P*:,6@(/$#PU"H.K\0JNX.UYDP4ISL1-[,ZD30Z6RMOE@AO@B @(/F:G)"@F(@@FL"1DX A2N$(.W@@F^,A9HB@F*CLGB A-:.AB,C@9N-L/NN5+1QR+2QR,A1RH27RH5WRIOT0"A1KW$$UJ]''M35.N\,''HV%JQO-N+PJL0K^R%9JJ79KP:''8J.*QL#*0L&?3Q, L3  A@0O!V@@A+@&,"I PJH@F4D4Q9P WZL AK0@AH@@P"(@A$@@@ZH PX(@TI% PH@I!QH,!/S15RDQU^L1T*%5D.]5D2U5D5-QT7-UD:U1T<U5U@%UT/55EIUQYOR.[FT2MT3TM*A0#<)(B]"4B)4B?=[&PKZC*U;$ S;%]^;&.^2.G03PTS5M< Q&!J @F''4B0#0 Q\S NRS5/%9U,VLG@!@ S,D3AX(1FBZG0Q82 %P10#*+"UE4''L557Q%T''Q]U7U%58<KOGFKL5SQE"\LDU6<H%\Z3%#B@^IBOR(-B<!Q/XE*OYJ9R4!]-A1HG$!;LT8+U)1T@@0@H?<N2@BQ*(DMMAH[4@@P$@D^SP@IHD0I*@C6;H@LRDF?8HDX&@AHZ:8MT+QYAEUS#U%L'']%S%]%Q/U%T-U&Z1U&^5]&ZA]*^''URW +L-IB#/NECPF<:D.*V6;KL?<8#S^2 GKT:\JQ&KJD&U8* I$C!X$2@SJ2M0SD)0!C5VL2.''R\&NZ+8I:KDQ$;F+JS X:3EU_USXZ*6;-].<#R6<7U.=9].?=]/@;]/AAU3BMZ50^=JBWQUS4\672*V>&Z,-GS7!#D-\$*"S*\.GRDX\,KW-(*[OA]7PE]7QI]7RM]7SQ]7TU]7UY]7V]]7WAZ^]69K!^X"$]P9]7Z. \=)>;S??-]SW! KII/RUEZ@4\S()$$K^85W^9FW^9WW^9(W^99W^:JW^:[W^:,W^:=W^;NW^;_W^; W_;1W_<BU_:>T(IX2UVS$N14UPWHKB."(=2(TQF[$Y<;!EI,QZLA*)B^I_U?M_"0O ? 7 ?1W @"[ @1; ACY AT[ AW[ A(Y A)[ A9; BJ[ B;[ CJ; C\Y C-[ C [!C1Y!A$82NN,+U37!OTEK5GM_,V#ZA,5]_[WH.OKR"](<UE*A3V6,0.U!0^5!00W"G1Y"G2["HB;"HS["ID["3MH ^W6L6UVUJYUZ8IW\KB4:8L$3%!$K)_N+@''4HR(,&C!*!CAI#L!;#?3IF83MVX3M&83Q.83U683"F83%>83*VX3.&83/V833&X336837>838F9DDV9DK6X1,;7=-TH-5ZW9U))R16TM4%ENE:(,_EH!J)X_3EE\AH3/G59OHE9T<V9U@&9UD69UIF9UMV9UQ&YY]ZUQ/(*6O9#*=0YM>=9B*FW9_I52''^8/)EHB+)8 7PXX9J1BL6Y"T>9"UN9&UF9&YVY&]&9&^>V:3YJM$%&KJ4U4G!T.188Z\E-AF9,>B-$<UM,B!I1$\+ PKH WU&97Y697^F97"V97&&97*697/F973V977&97;697<F:H@V:HD^:@IH91K8I<4+2=2J@AC(E''3=9/>C8-]<''UU-;&U90>"I[@E)QD?U<^"OA.&PE.&QI.&RM.&S7!06/MX8<:T929SY.U^%OZ ?2SN7]NE\[[C#6"I K._VN8@QD@@BPHB!)$@BH@@\NN*$S&*$Y.*%].*&!.*''%.*().*)-.**1.*+5.*,9.*-=.*.A../E..0I..1M..2Q..3U..49&(TP@C%8X@H0NX/AX2F_. )1./7AT:Y&VEHC%!_''#_]2)NO*AX\:X7CQ.3DU.3EY.3F].3G!&3H#(&E>@0T3)J3_M@R@[+''^I$@ * (=AY1+)W/4A[<D@3DF@!*XX#UW&6C\N7V!.7W%.7X).7Y-.7Z1.7[5.7?7N[-7_[-7 [.71[.8B[.8S[.8$[.85[.9F[.9[9-!! Y$%"ASXD]7E3_[&G!KY6Y6<%%?]M.W*S_*;U,U36A Y%,$TFX=@@I=P:I=G[/=W;/=([/>Y[/>&[/>89/?JY/?[[/?O[/?_;/? [0@Q_0@._/@0=0AB]0AS_0AG_0AW?0A(_0BY_0B&]0AX<T^#FOI:;WEJ;HW\Z3&(XYBKO$>[6+X)3HZV$L$''FHCGG1B8G1E9_1FJ_1F[_1F,_1F=_1GN]1BKDW4U@OI"%''I&3Q.<8+KMY+K^T_(NN[^@MS"-*@>%"XJY%P>$N%JJ''2J<=2KK]2K-_2K-?2L@_3L_?2?3K7<#LW\3MO\3P''\3YW<3Y_<3"G<3%?<3)7<3.W\3/O\8#0$R WCSA5:V3F"7=EO\C)9'';-2@\M[^7NYFV9%XJP%^O4$AN&=D&7=D+G=D/W=D3''=D77=D8G=T<W=U@''=UD7=UIG=UMW=UQ''=UW7$G.1''5>9Q_G^F8@-\^:VLM8,HH.V4AOFY,"(E7$1CU]/=VH''=&L7=&QG=&UW=&Y''=&]7=& '']([@E5<ED<D8-8(F\QE''DU*R8X-\#!O?"4X![Y:6C7?Q\)AQ=7U''=7Y7=7^G=7"W=7&G=<>X AZ7;,2%9^6F*?.;)X''VYU6>0(A-E<G.)\@VCB%W;XU''>HY7>O>G!?"H%?"I)?"J-?"K1?"L5?"M9?"N=?"OA?&PW? Q.@ M%0>\VS6=X]??>5_\8[L+3./8+^DB;WJC:I&GPN>\5?&]9?&^=?&_A?* E?*!I?*"M?*#Q?*$U?*%Y?*&]?*''Q1#OT@#TC &3=ACO44T$12-56V/>,\"O%J [5.PQ6HEMLP#DDO&4U?.5Y?.6]?.7!?.8%?.I1:7;^AS<EV3L1!5]K_SNA%9<C^6Y!95M_)PE&LCVDT2TU/3EY?3F]?3G!?2RW$$DBHBQGO2:]."NMK68?D5[W]B%Q]CKG\@,Y:9 :&#VPW6?TF''U''83V[77K T[U%777VO7JWI7J"_7?6I<\7Z\\7X_F%[S=8N]=6-?=6Q=>8$_>69_=6)?=9.?=8*?L87]>7/?=:E]>98]>9F]>7K?>9@_>7@<]:_]=;>=>8<=>=4AO%(ZR\+NO@75$V,7"P:]")MLN*-7)<VY\0I!NF-"(05*980VHE HGB.1P<NAA 24TL&204FFGA 4YK"0(4VKE!0X#[$SH4ZOE!!T#Y*R(<RSH!2UGC-184^QG%R9W%''19LRUI"!4S//Q8L:]L''CXU4 SJ$*#K''QL13(RYLBSJ&C(!?&0Y-V#PE#TF=F"!@XZIEQ,6+M !-*1X,&OOQ B 8,FMF7C!1)5[U:;\B0?48+5@@@BAB7!/?0"F23\/7[6I[0@@ EZ-VKL[IDM^N0@@A(HQA;RPVLL!0XDMZ''@\HIG"208$AP8=.-I S8%U%VJ=V#U61:&0]_+\CUT*5H*7N?[T7Y%7S]<'' ]-FSS07S>V4S?9,''_3 \NGPQ5Z=+/7)]=_V$X^''+[*EC @6JJ]M279-AAA/;2J.^7!17LL8@NCP*; 0W(,IJA!##%TF&VS-+[WBYY&=UDHCD/WPP@?PFSQ!!QFV$AQK+GDDV%DP/R["QBIFQUUPPCGWEEN?]Z_"U]2%RNJKVQ''54X($1- T#]09]ZNINK:(88(2=\#!"CT%Q1U2P''Z(%H; RSV[S1$Y@H@EYI''5W/=;%HW 5 O86QT  H[Y9Q] ]27F57778]VX &+I*VBBCQITXU\Q5#A!!I495L@@''3%T(UM>;"R[[<U9FA10SQZGV8#RG^^#TM,9A1JN$<I4TWYH[&()]#=B>.&$77''Z69N!X#(**)5^=Q2#8BG:F:N,3,))[ Z%-5:\#5U&U%,Z9F\WWXNMZZY\\N4WFI%*A*!&XXKANR];N6!YFXNXG]]@AS6X)-%98W[FF%\U[L*SUZ@.I:VRN]W(GYAK?]!""M\U^RJ<O[5F;;''2/#-%/-MAVU.<N=XZLEX" [+T$4N&&**<F]54YY[TV/1XBOJE2U]"GM/W<U5(8$B__?RI>^3?-F]Q*6AZYM%YT@4-QUQAZ@OQ_I9@LZ>+F%DKWX)+Q"3]J.R%9^4;)T''%OY)([-$Y3Q/RBB'']MMNTF$7%T]T%K]7R$7Y=]YMVL3^54E0?A3SDPYOZWDJ:**^%2"2/M]ZW0*89)+D!J1[W,&TR.E]^G=,WU8E1385 %2-$::A@@;"45P <R=;ABHFN9G!J80)LJ\O&X_)T)#C.67CXQ2\L>*LO^?8*S*:MS+VL&3-ZN-R''Y=64:*=#OZ?,(J\[)M[,F&_%67N.YZ6/Y[T57;L]G=/W&W<Q(NXI^<>%U9-6SZ.VYN<]_9Z]M4T8H^TE5B1P@YG/6Z_+,A5DF,EOA[?#?;>2&!4]UD@;_W?<B*G]ZOC4U3S>E^)PAC1Q5_YW*'',!97>3BVCU0AX+5-&*\3T:S7(,4K7H*F1%23.Y10"CKADNY#?=Z][I''#VXB:S,\L''#4 "4EY)A#R@CBF LC''FH AQD#"QAJ,!.CEHA<;VOW?BJ%[.LMC-4"X9C.OOT@87D.%^!2C2JT-G<^JQDJTV1"S.[T^^$:J8%1"9JS(0]@)''X(;Q%#WPXHY8MC%PV.E5KKA%;2<&\Q2[@=TU:4G*V&4XH%>4)[&92&5O8X@LQ2*T@AR@@@@0ZD<%GT*8BD6!A@S(P,0%48FY[JTF9N,@C@@2 V0V)DJ6FI#=X.\YR.??CG5IZU;YU>,:U-E.WKG%CIR)YBH&+6$''<Y"''LW$K)%)''JIR6GVTPKW,(7D\L %""CKQ_RCPCBH"F2P)!G/ EFLB< WKFH=[_:EL98]TQ^GQ\7+(]P"CP#HFHMD@@C&+V J3S;CE\T@,+QAF(DMRAMCT@@ !)\T"@U6MN<WH]F B%LZ3P*(K:RFD- P+E]C!T[*!**- :ICW H_F"%KK)Q".J+(RWB*K6XX"JEU-R$Q;KQC;\&$B/E$X8WN=8G+U\_0OF4K2G;2< PD241.^%M#$''PY@:I+\/ XEJX"0#$E-JC1#05WO9,PZ@4JRF-NB8G%W,$?C+ .OL8\F49 %W?5GJG-Z>]MZNZ2"IV5 (;>)GJ&WA5"%0;Z-YTP/FV^^4+*%H:''-^%<W<>P<?[UEZ-1F6 [''%;;OMDVKJ>MP.E  -\N\>BULPYL(X(&ESLE,&-<SVFS3T(0P)JH@DTP@@FB.!AC3 @@Q/\3@\ DD@EH''%CBM0,T%1$X4JY*]J9E&$*JV*]VZ.3L;!B+H+=B21C6W)AM^YN]U%$$%8?U<[*B(29XWN""V#*P"8];$/LN1Z@2H$,04"/SMVCKG3KZ[0M\*<=CU)M-3 #&7)V*CD] N5BZ$BC1+PV@B3@I@9PDHDYY@@@JD _AG@H 1IT!G3,>&IDUS''EX+Y.U&S#WD#J?2*<BS)''_0<,)("AV3<OG?BXG6(1E0.8+PF^^D$0]&MD#SXR-=&@ >Z$40*B)U:O;P6PX3K!G96W7"K/)XU3MI0!2ZKN$PAXS==*PFMH\9D+,R@EGP AB6C  @[8@@H\2@DG@IBB!UP@@A@ #V*^N-K=A#^&XC3X0I[HH($Z\\?W[Y#.''"#FO->9)N6J<TJ]Z:MDV1CP^)8QSH<FT$M+5LV TX!838$V9BU(A7\T$L''<9-L>]''NE(K9^YNFD5E99S96WK$GL?$.!B TTA@:1G@9@8NU:I @@IJ"\@DA@@1BH@E@%:DD$BP*ZB BJ1MS9BE/Y%VN4F*2Y/(-6**Y-*/=**:)66JZM-)7I;V6K!;#R=)28"=/-Z9,;6>#VUH161&L(G3H-#+6[8D@]P"RGJW@  624''C1^ALG-YQ?R+5Y/WU(_Q+ 1#60L@(;] K> (@G>MD!)5:^Z+)04.H,^X8,T1$P5"%3$ZB1#2_,\T>0^=>L+);SJ$<SPP9>\Q2%7.\2U&>^Q(33$LR]#KE-@LV+Z=L$[>JB3&C080G$LS]L3M[PD:SEB_*?*\7K5XL_U&GD9ILHM%;B9V#A B!R ''1V YC4Q",-S[Y#]GCT7;>@=4[ZW>>4X#[."TF_L[\>=TY=K];+(C)6<7>;B ==N8WMG;[:?\.5-P22/$+*%%#7?)"375"XI]0+5F5A6K.<M919II''@9V''7JD54$,0WJK] V @XH<DDOI!C+F/!SA/0A @3468J-V;"^O41)3R<HOED]L_#DC:O1U5U<E%U;>L)GO/NK:72YN;- 5F?><&E:?N)O7;#JFW(GFS,'')A/K[2BLG''>"_%%A7(VP&2V/V[KU5C=M!4H1INVDNJLA@M@ @3WHPG(HP@_D@@@0P@.0@@S8 E#U@@B@P@;,"V%(6UK$6B:A&29YHB4MDK%I"+ =C[19H@U^8D)T8@"J(@Y6H@]>X@*FX@Z*2"2](@(.4@[JH@+RX@2ZR&)@W%IQD:-]''.ZYCL''L![JT6 - S=;(?5FQY][12AD: X>C!QQTM^A''+ALE:EZDHT@HE@@NDD@B]D@#HT@L#D@ITDA#XDCL\D[ZOANQ4@*L*FG;)H8-KUQJ-RD[+.E2/ZF \]"^3ZD]!$8_0*D[;ZD[1*D 5"D^2.D A&K1"Y_8MRK98P5:"]MO%Q)]CMW:Y]8 GUW84Y^O.P1&>E@GK$PIP@@H&AIV]T@I2@@J@@@B,D@A5H@B*D_>*P@LZ@AVNTB.DP@>Z]_^DY["<ZK!6T8/C&L0^-0/>&K/HJL0E"L0G"L1N"L3J*L1X&COPENO;ZBOH]VV7E/9''T%$;U/:U][3''I*4FD"/KFG%$TTLYPZ=\L\HQL ;*/?FO8&U.WCK.G@R;VDU-<AYB3C[@@QJ6J''M)[WDPK8QT!R$R "$5J3FPR8$P#H$PS*$QB($QSY$QT[$QQ($Q%($Q6:$Q6($RBY$Q8X$QIK$P9;$QH:$RG;$R/*N4FDIC<Y$$HDINBTYN@EH4?&Q^*5_TVU"^7P"/U&F-/AR OWL.I"FI$5@(GBFP#@[O7IJ!VSU(E1QJ5T%LU$%U%:%U&X%U6:%U7X%VG:%VHX%VX:%VYX%V):%V#8I#1U]:X6E)5''SC3XO$QFN_%B"WG2^,5!VWKPPY=7T%MVRUF5E(J@R![!OU61K9XS]''/S@U%PXO::%YJX%YT:&YUX&Y%:&Y&[?I&].)&]V9T/V%F[U$_GL9G%]H,B!W5A]S6(&WU>Z(3$-5-1LEP*,C1*^C6;&)&;.I&?6)&?>I''@F)7@NI7DV)7D^I7H&)7H.)7C&2Z8TT-V916OD)]44C0&1B[K(I^_!YT?=A5?&!_,5X!NNQP>@@@+(7.R$)7*.I7.6)7.>I73F)73NI77V)77^I7;&)7;.I7?6)7>.I3.%!0,\ E.R''&&B'' +)EJ*1"T9VH/,QEU6(V/"-#I2PAY[APO8%I''M.JH]6*H]>JH"F*H"NJH''.)&*L! )D48PV:D3ZC_Y,T=:@80&1I,#(DY&$SI^PG-3,0EQ!PB\A4W<F*Y@NJYDV?:&QG"&RI*&R..\T](@A DCEMJKUYT15K""SK]$I_AM;)X%%"YL7-)>A?BUP2,$@ @@I)D@F%D@IYD@N,J&[-"&\/*&\1"&]3*&]5"&^7*&^9"&_;*&_="& ?*& A"*!C**!E"*"G**"I"*#K**#M"*^9*&Z%$@N*LA,E]I&>Q GH]6R@^F@YA9%_QM!KI6''X)Y_\%ZN% VOR%HNNU0C..(#OY1@/V(C1*+C6V*-4.*,2"*.;.*-:&*/A".0C"./D../E".2G*.29&*2L..2>**3Q".4S*.0M".5F*.4U..3Z&.6X..5V".7_..6^&.8%*.78-CC.PB++V-#ZP21@O?!S HRTG5LWVH''/EK]NQ[]@(2@CM@@BZC@E*H@A*@@0QK,0AZ,0BI,0"K,0QY,01*,0#;,0#),1EX,05(,1U:,1&X,14K,1''),148,2H:,2IZ,1I8,1)H,2'':,2ZY,2;K,2(Y,3J*,2<;,2<),3^X,3E(,@P0,C)@@A6S@@+B(=7@*D++IX[3(DCJK995Z  Z\I!X]N!7H@%3@CB3@B$3@YF2--V2-57X-6LJ%6G;-6HX-6Y:-6ZY-6Z8-6+J-6+X-7K:-7K(-7\Y-7\:-7^X-7.;-7_Z-7/(-7?:-8@X.8X9E!Z;@BN1@589&/W&Z.=JG/14!Y&4)44Y.B.$TX8C?PBEMJFM5308$P@@DP@!DP@2D0N"Z[ R +.*^K..&[...+./FK.3N;./V+.3ZK.7^+.;&K.?"+.?.;.?6K/@N+?@V[?@^K?D"+?D&K?L.+?L*K?P6K>.N;  \0H(*%,(8%&-6X1BVTC#R:MIE26B@I62NU;:.P@^P@J66J/.6+?.>K?3F+?3NK?7V+?7^K?;&+?;.K??6+??>K0@G,O0>D Q80@!PJF&*QT:IK:+-9W(QQ''--W M_ZXRF*VXAV]W-P@\<$ QH @0H @RHP@"OL@&K,@!7\@^_,@*#<@&''<@"[L@/G,@/KL@7O\@6S<@67,@7/L@;3,@;7LAC?,AB#?;@OD7DPF?DPD7DR93@RM;DRN3DSO;DTQ3DUE?DTV7DUG?DUZ;DL+7@L"8@@"H@HXHA:DN#$;RA''TZ_&8U/3UL=]#$2?=QQ<C\#(I\>O%R\IPHRV"TY/ $!.? ''W%Z  C3H!E;H!G?H ?0&EMH@NPH@I<FB+$\T#\"=+!!;%))^=-*YQLQ[%4Y]Z=N";;]!31Y6LSRL2AYNL(_H))3H++;H+*3H,-7H,/;H,53H-7?H,*?KBUHS>@X@NPF^\-E)<L@?4ZOJ(^.?S^V\C;2P_>VV=G\ N^AZ!IBY9VHUBHU]5XWL.6?H683H7_;L7!7L7#3L8$;L8%3L7M>L":0@B+O?GV8*_^V7L.4X.WZ*&*LD(*D%(_^738R;@UBE@'' P2Z@C2CJ6S''0!4HJ<SHD\HP3N4''3"4H#=4''3R4H$M4P4-4QT?4QUL4Q''\4Q7?4Q(^4QX>4Q)M4Q)>4Q8-4RZ<4R(N4RZ_4R;,4R<O4SL-4R:/4S\\4S&L4QQO4Q@/D!F @BA#@CD"-)"ZU$DWVNIJPF=ME*D9.>LZ+L:L#!ZH/BLC@"Z)''O[6''37@51=U''U2>)VH<5VY^5VY>5_HZ5ODZDTI,@ Y9/5W7ZSWX#@6.^47V/%2Y(_!@R27"2_P4%Z41H6%V@8?2S;F$RY=@SO7ZF-92!P61E!@1@BX2@B*XXN)??L6Z[,6Y_=&Y''M&]?-&^G]*@-IH>=!63J2^UE(&,V"?_N*AGFE1H:<=Q^<O\X''C;J804=$"O%4KE5MV3I86#@E#6IXTR$PB$)9G]U$WJK4WIKUGL?M7MG-7MKM7QO-7UWM7YS-7Y_=7Y''M7]?-7^G]7^O]:O-FZ[A)A77-PKS)NS:69K%II^BK=H><F/J3Y#R$Z.5!C7A0BNQLVX<D 1L)WCD3@ 0&6M"3&_T @"TH!/&#7F\$Q?BC(Q73XQK>GIY^HRCVHYW.HYS>HU7NHY;>HY?NH^WNH&_>H"''.H"7A.CY7Z: <WQR''%+H]\&@7#3CQUM3D;NHZ)'',%CZUX>T5?:G7PLX:G$R76E=@-%XI4L2F"MU([I1>^P,;UT#%MD[$#E3-]KZV ?ZV"3ZW_;&W!7&W];O51PQM''^<Y+3\^!]+%6/V<B%V[R68C[";QG[T''L 9V=ZL&]YI$[17O-LB1\TU''+D<OLM.^U0"D;A8L!AUA;(Y!JQB$Z6B$.:B$SK(DWW*%R;*&T;(LV''*&\;*'']?*&X;*( ?*(_3*)449[<+O!]H?6A!H&N ,8U.8C''DC97S)O@S$S;&#"&A:[DX(&0]XM>X1#K$@JZ@@NP@@NZ@@(5X@L LTL/ILMPHC-DQ @>EK!&["J!3"7 ;"7Z?.J]3.8"3.J!?.7%3.9[?.9*??;.J?;\&?QR6ZI^A(R,HBIIK:+L!<39Z)_W">3S=K;=[IG>K@F(H@H-5@U+KD^@31\X;C@PST@JTIHBVAFC/B7 55;]8G9&F,<18-916><18]<%5-H\[R%M#93.<8G+A^3$[U6^,D+#B(!WE=]$[MD-73HUE4;(IBFB_A_@(1@B&@@BK!@!_ @@''A@A]@@ALQ@!C1R@QPT2F4W^IN7^D>=5U\=5%N=5%?=5&\=57>=58_=5&_N^\^Q4[6P .PTS<YW_]15:FV25O4;*4<^%00<"4%U@3XDZ8W@X!X@AC@@Y=S@%S@@@H"@-40<@EA6K85H.)-;.3<>.+-;84>>9E_?O.R3^>Q_/.M''O.^GC*][83&6V+6+?EKWIW57W%;"^2B%%62#.YSYM,NL(*6AA+J9&\N-H  T K\$/Z=)D&G_DO,(T_6@OOE?/OFK?ODWO?I;OBBFI&B&.Q7IIW(M6Z6S6&(Z8^TVU^F,F(1G6Y6/D:"P(.=5B8@K5BPQ@HRHXZ9!0DW$?H29&=\$$O3GO?4KTO7O/?7''O?;/??77/?;;O4A4ZME@HLFAAQD^UF"PX\JFB15F!C#1XTVIE"%^5I"QH4ZO@ V6B@%R(@8@M#Y,VKE#!\*TKU7FC@EB1XL[M63&1G%S)<6]E7KZ1@F@0HT[K7@ZS[)3*\<GPGDB@MBR?>)K%R1_;+":X,TH@A!Z"F0PU&SXC R%-!#P(DXOEC@"MF"PP6PM 3I@$@@!(4ZKF A@]A!Q%/AY$@XO 1R+6GCY1($]H58\&[G$19L!Y<Z<>WI''29<+!:X<V+M''4ZUA$>Z\F/W)5Z9M*8;]V#[+3VI1D3Y0L"[L%[63Q AP42%O8<^_6%R*''@AQ(3:!9(3NUN]M*[:-1&RI5_\@@C F&''7X(P\@BC7F''-T@PDBJDRUL$. 000\DC#T0@OB18B>HBAT&$:00,0 \4LBQB$S00@PYWMAAARE,LLHGIZ2P0 ,''3MABCSG\4LLNP^QP1@<#YD2$$$9*BZ/-_FNQ)Q@@4O? I*" 6"$98:B+[" \\E0NNY9*_N*F:7+C2"W,#/PNKM1L[JD<@L ZJ@\V)CHO,@!:2@@FBKB,@PDX\&! *K0RGH/I=M@<T<635$2S3S_]#KOMN^F$T<8:<[1SS3/93KOOO_4LEMAA?21TTDLIO_QM,$KZ3PJ+,DLRI.FH,?D8F8N<X[(K&"L@RBFK(5F'':X",*$UIVU)A2_ABH$2$D#A@0Z:3^,!R ^XPDBBAG#(P@@PS^%#KA0@T*L@CD A@ KY%W:,MM--&Z9[YZJ&E5-)'',WUV66&/7U[Z[:/M%%-1A>0@QQ-X3F8%RVFT,S!L7;W1!A-/FJ((^ZWK%;(Z!93?:$!5,8+TJ;CLME\< TZ((H@EJ.A5HHI*DF -I2/(X@J;&N2 ! D@GCEDD$G>VFRORP:99IEMS!''%%T=.VVTQ40/K4T"MOMIHR-<U4-J$''M*4NQ;!?YF:(X%42T U5=7 :ETMB$,2 !8**>&D)DY0Z(V*I/C*+Z7.V&./,0:[:+F1I)/++=DV.>2535Z;[[OA!#--.]6N&66;7;9;[+7+7#///1$;U6&$HQ7\*)%*4+R''SN%U#%9;''1.:*^\2-\;_T?>5&Z)U[5OLLWB;G]_[\D\OGWQ2RQ]]=]MKQ=745E%_G_[Y(R7P4\1?*>(*%VHX;(E8)V-JW*RXH>*82F?T?7_'':;#SS-JWUO4*/K&/Y/''%::7O7&W-,]?^>>;A95;<;044R?B##X84)WY''UE9_''9"*\T_$_]0WT>MJKQK37ZM_4/OFH%NX&CEIO@H48D F.B BK%BACT3 @0=XP@QF$HDP''N@EIY#AB%KP Q24(@X;"LDM"#BDHO3 BD7(PQVR<HP%GBGTPFHR%KQ(J5''YB,8ZM;%=N^8''''\KQ/G#6*Q5Z;'''';L1WS''OZ?\''7NQD5$8!N]FDT(S%FJUZS"EZ6XQR1.TX-]9NHW/Q!FLH9Q#FT$81&%6@HYH*59@YMI#HYV''N)TY3+5\,9Q!G^?''&D*_?0CS.9T0#P%H(!<!P2_H_?G]4!EI)JQ"GS$H!=YO''L58G[@0U50_G^)OPX/I2]8@X5>="''J2UFH<FN^*Z2B/.9<!5F_Z2P$X_%JVTZR%+F,9R1-&[HX'' Q=:S(ZR7B&NO]I#(\> ]3=]+Z3OQXM$O,;T%^$E2U\S/NV5\0%MZ=)SV1.<6Q+?A_M,HN87?4HUDI\R ?/&JL!A-ER>Z-J<73YD*?@P@D*,H@JMJ@C_F(@''?+T@C?3.\=>B#R ?12(PP,JTG<*%J@KOR!CD^+P!C:T(!N5*DP16%BMQ''R#DOU(QSOJTYE>=JH]AZ%IR3+R$:(4)RPM*T-Q>-JU0)R%L$6(O''U@@PAX0D#(R=? !BO?H>L0S''EC"89P//OCSP*OW/7RGRB?6Y,A0LAJHJ#RU[FZUZ5.%Z-]=^)W0Q)V,X:U+FT5:5''Q&%Z5+)V-[WU+%Z1JUQA80H44)@)VXDR\:XB*##_2DZ_R2[N=8.!G)=Q_]''3*@ \80@PV,HDEGA/Y149V,)V%;FT-&5''L[%Z3''^W,Y2L[6,^JM+N$M^5(TP/Y5I9V-Z5%KV!!:5''YT)Z5)H7-[V][6-W.5+V8=V5._8/[6/K6,X1=+@-Z\@C])T-RPYV\L(.*S&L&=Y3DCAK%"F#I.=[PM1^X WJU](BT#I^<9SW/^]F[W/V.%;7-]^=;8Q-_>\:W//V5;771&5?=?>)7A^IM2PI6(IWBET$&MID#8?(ZWT55"'':V6%QO],KLH#+OY -XPEQQ&VDL[5#CG^[0!34\X!BOVLP%I/FISY1"EJ=X1R5&<X-]GFLX(?#B@.YJC_>XE_ZQT%S$''I_<$#)N''^VL&M&5*84G3ITE+NCB2 50 ITF9R]GVR-S-+JT,U3%KENY25_V<)^;/FT/T?''IYSY3&L6\Y#V/&\5-K#NZ8S1&NX,926:^L9#/GF\:69''M^,[3''/4\:C1;&\=WY#IW&J0.E>D.&GF<[.M<E% E>3UT4CE,4/:UK)U\NBTAO(FLPS7#TH=Z5JT&=Z%M''V)T+5+U+RX5%IO&T<I]Y_?GBXX.T89#K5FR$97IX2Z$-OKT''%HE4:06-*.Q_V1%I9/Y27Y6,:F-8]4US-!7=T6#\8X<H@V1^C!(L@>K[L+K8_"]33OR!VF2W7V/&=7-]/^;8Q5/^\>;/#O03X4MI>-<5?)=<XO0#:];K:S\1EI<;[UQ>) ;IB]MQ\G^@HAU &"IS93"E[_81SF^\X5/''NL]=?#GPQ92$X>\9BT7>\%Q#''J%]S+X>KZ6J(FY2RK7.)PKM)9 "P3!]$:%#P0GVLQU=E6H)9''LZ"::''X=^93\W>,9_W''*T.934NDO]2%RW,-V+#OVU5;''(RW_:4(7^93=''''^-''W+OW4W96PF.Y3E7?U7/X8T94M:]];%+_<+7G>31K<-.((0)U++>C/AOX+93,]J)62^70GPA8WP]XL+UU^T%T[)#24+X<]:&]8:]F-_J\/?3$O2=94Q][\9AWG>&-''_''SZ3;2*B]7:[?Y^,63?/R--77-U9=;5^>>M3]F=L)I;4^WXE.OAH]0<&3.JW Q5WFY$+C!@HZ]FY0@7R-G?OW5''/6_Z1?;6?]>=<GO__E?_?3!I??93Y?><*<_?^1W_?/!?7;3+<S!F9"A(#F7K.]R.$_@@?!M#&&NBL?QC =U#*0%Z"3"/,/=FCC>F%C>G#@BGW@BHY@BI[@BL_@BM]@BN[C7-LKB(J*Y,,L%?<RI,L9)#( J,H@FN(Z(Y=;%570N<5 .@QH A&H AG@0A7U0A7&0A770A8D0BHU0BH&0BH70BID0BYU0BY&0BY70BZD0B"L @&80AG[%@'':I\CQ-1<#)4_2MJX;)$<()69S)@":-?-JMY*Z*R"C +]30C^D0C.U0C.&0C.70C,\JAJ0J@V3 <R[LY(#/ALO-+1 L&\AM99 BLG0O?;H0$J3J@DR@ARQ@@$Q D".QD"41D2]1D3&1D2?1D#_1D3-1ED&QD4V1D$FQEDV1EE%QD5N1EVFQEU_QD3U1EE\QEU41E3E1E6=QE5=QE641E7.QE761FH$1FG^QE(%1FI''QFO=''LQYM,QY#<QLEP@E42 ER2X NA8:R):>@((:>\[)V$NB8+WJP395,ZOM\X Y:@@Q$!R7PP2;$\Q;),Q;-TQ8_!!;3<Q;9TQ?9\Q?''LQ<A4!?-\RC;DQ<O$"CG(!8_1"@WD"DST"D#T"@YT"8\\!<],!<3,"@!L"@-<!81L"H%T"QI,"L=<!<;(@K @0Q,8O8<S=L6(@P]#\!>X%Q<BN^^*9#HDP[[ZB-*) N>P#@:X@CH(2!99R"ML"&Q\"&K4%2X$#2XL"*UL"&[<"&%4"&1<"*/\"*%D"&9<".1\"*+T"3C,"/A4"2_\"2=$%_@,"2?T"/_$"+U\"3[\";Q4";?8?H.-=H*0WH@A*@GSH@% 4?VX,9]:*#H( NH@$;0,$7''[ P]\41? AHA>*HB3DRZLCL3M[M@LIMMM/L3(:QNPMMUQ%L3V:T4TSL3S;L3K;L3IZ 5T3L5U5NZOEL5W3L6\YL4VWL3K0 6\?L7PWL6R=L2?VH=Y,"O!H4DCP09/A@E[YJ:NH''[#L<D_:4QH:<<0FH@5LILE#H<./L;/SL<0WL<1[L<.SL<''TX3V?L<.9L,2CL=6]L=21L>)2\=3]L;65L>:=L>0UNZ7''L>>_L>O=H?;UL?>3MJ?/M@)8\<G7H?E9Q@@UP>C5QA87L<6;MB4WMBDYP>Q\I!#KOST*>.?=ZGF0\+&\B-*N3(W(JG<J9+JQI.N0;KZL(CA\ #O\3%Q''D4Q7U4Q7&4Q774Q-''2Q8U4RH&4RH74RID4RYU4RY&4RY74RZD4R]E$Y(P/>E*B>E3P''C+)NYUO.("J9"8M<VY-A8@RA[X3IB3#"TJ3%XS3XE23US3GUQC"P >BN=E4M),FZ?Q4HQCTHR""H1J"U^24S-63HOP4O?>4H AUHPP5"P#UTQ^2T]_SS1W5H1#UPN^4TK5ST/.TS2-5H0PDT$=STF,ST &UH]#TIA9%0+I0?7 -0X(I*[3MDKUT681L.6BNJ=(QA1[RX]*TS\-''"-X4S#&SL>FTPI+$LL(E!/>P5T5]RSLHBHM )%$ABHDH"U(;I6Z$]TLJY#PD:E*!RD@VYU,_)E,!85.[MU3?Q5,3ZE*YRD:^]SF.Q%*5UX[D2::RKBYA0E5(%YQ*D)Q.K#*!"<!>3R_STE6PJB3P(220Z <AX@P6I"24,2<,<$P"X0XZ(@]R FJ4$2@&HFL-]"18)VO%( \F9F0$(&+F(4;)9#J0EV7&-W*,MV/&-FUC-T&2I&X39F$T 5''_9&Y]JV\IRV[]MEAY%&=\U$6EE%&IU#PBADK&-U3N1TV*C_)J<D-3RK(@LK@^PCE[EMI.%SB+#WONH&M#A*8 0J+\DT@0A$AFXB W, H")"?<(&K?F@@F"ML/4DM.9TI#I&BR9EY*DDM[1YU]N2]Z"?Y%Y1Y]LRMO%[VA&OYY1SU^I8M1G<M158QA1#U15WQ1OW]0#1UOMM]0WSYY''_U333V@HE]54WU1JV 7[@AUV@>G-"49F(1VU[C?^B5K[^K9_ (&&*]?G$X E%X 4,H"MZXBQ.A-;VHDGBY"PBIX^&@B^BU#+\(/8SD$?-I\M*XF6&HDBLH):UZI%,!(C6Q*TKYBI@I\O_]]10Y=/TYC5#]]65\48O]4K7UA9)]2*=U=2>Y>ZUY=I8I=<]]>TQ]-+-T2AN^OML47>D8@,1P''EGM+USROCD8@[3T2TV= 1LM,FQYJ?90$VD:$A"+ K>V6@4( X&.@^$. @."BO5(@@F@ !P]BN0_BK$CV[P^"AC)VX628V#=(ZB-7$N WS_*7]ME7V\.U M''W"GWV\!NH \*D"X-XW(]6WT%H""57"RSW")$V"4EH">.W_ <HZE/ ]*04G[TBR6.58BJ-R6\$ZBCL>\X-A%</$JS''L(,7! D RN.BHE[2N1Q X4@"AVP@@1@@A1P A98$+!) @&I@AV @@%A@AWI@N3,@@P@ @TA@AD+@YEE8$&[6"B^B_>G7"]TF",>5Y<$FZ*4U%\%F%X]8ZT79%Z=8[6RY%JNF_HG8"OL&%:M6%<?W%+<X Z_6)1)QN_<S!=\JC$UQ]G9(5W;\6J''PLM! K.KH]"C6VB):0G/CX0HF(@PXPB+RE AHPBW_ 4(@P9L)(C3R5*+XP)4A@0QX@A;S% X@P@PJ0B$/<7DO9(D*08-YU59[=41<>U,AV''VY]Z@MH:D_]:G=&ZAG6ZA#E*DKF$8N>)T/6(&EVJL''F*L+6*LSE:P;>)<KV#]6R/]0)7\*YTTE5.?PBX8+SW^IA PVTQ.?JVD=6G#!Z ?CQ@V>H A:H@ZZ00QJFJ )H@Y*H@RZP0^<@ CNX PT@@@8H@U:(@C06PD2U")8HG3Y0$&HNF+#]WJC&X(CVF5Z&X3IN(GM6''3SV*2UZJ6[EZ;?S3Z.6Y*L87]/4''*,;W*.63)?7?*/KUZF(@<;F#F/C#FNTCQVT9P1);''8,(-?_&''ZM"^Z2''Z[+PR.I*@EL@@FONADG@@BXLA\L@@B!M)I_@@DLJ@F6-A9Z4@/D.A!L @@FD@$YC .C.IFH0Y3,[!)"9Z7D[)D_#-4 5-3!1-X"Y&KK9^8$;.IT3]YH[*H''5-2>9^:J;]3(=.:@P";*1.L-1.: ]"U4I#@K&$CVG*\],!GTGB(P$&0+D,''%PL]C5Y=<I! &B %*R)^67@;SSX-V @FPFAV.-$<? HFS,R*9!$A=A@F1$H*R(B@Y3''B.5-&J]2U)?#B-5#BJ13C65_C?24<02_<03,<1C$\_36<1D<\1C]\1TT<WP^[M6:/,O$MU(V&#,K0ABN''CF4$SF6&1=]E^A%%XUWR/1VH$%.8XP)@J"AV!)6&A )@C?<[@LZ"@ AC-MVV%P(\.)T8/C,:"[,<"3D:#LM\",\\\,,<",=<2;4<\+&\3]]\(\G\"LU\3,&\3,7\3-D\3=T<3(\X3[DF],&;=#X@R7L\[G?G$28@JPB+ "_-0YK)N**66IX&#07UR_9RC9^;ASP9AO:2AXPCAM8V@4@ AE+ [8V#-)$<KI@%@]*"KCJ /:U< F^Y%G79V&.]V&GYV5]Y56]=5*O65/6W57^]5''M=6G>=6O==7]ZQG]"]]-"SG]]/N] Q853@*[B7\Y%'',.XTA<#F\\"X^Y''*^@RQ9D 8.FCJM&JHGE''1  A<(@UBPJ]T C3V 0I<XJ''3P0\J@@H H@MJX@D$ I9#(@EJP@M@P@A,6<@9*L:7''L4Y''*DU/*P]?,8WO.K3_NH#>.H_/.D1W.H!_.L+/.L3''.H-O.P=W$D@''[K]"G?NF?&X:$_^^M]&N(!H1U<RK?=^@#/7-E[8>DA*PJ^(B#CNVRPJPI:%  @^WILA@@V.F VP/ 5Q0@\8A#C66\VO5U![2^*1W$:/W..3/.*9?..=O.27W.2;_.3M/.3QG.3OW.7S''.3Y?.7]O.;?55;.*WU*PR==#HPKV7@:<PTL.]Y+7R\:%DJ>GV;B,/%D3*H@EE3W\4@AQA4CIJ@D2.J*EP@EPH@@UB@C''LPCR-/(05$@I!$@YFA#18J*LO&8L375P[]1L6O5U[?5XY?5Y_?5Y=?5[S?6ZS?7[;?6\W?7][?7 Y?7!_?7!=?6+99J&:''0?8VM&[L14Z''[3UGA"H[&MR2Y!]_\C6HFB"AM(=];J8X-08L(>:H@Y"@%&;0@Z"U#MRZT?]X.S->L8A>M9C?>:W?>;[?><_?>=S??:_>X;1!8@VKE! 4!PJ!8\NLFP(TWE"I46CC!"P\Q[>@@PNBB0(4\JV:D2ODF@@@+?7Y,DC#0)D&AIT<NWKE"A@@LKU)4ZEB3P<4^MW+T?O&30PB\H1*TZCF !L8JRE/4*MB@Y<4AS1-4*FD3:46^T(E:?P(6+M"1YL.ZOX,6+]*5[M.:_P,7+-2,KP0@,N@2[4&T^5=F@GCPXTJJ!@]+_E X8PTBF@\''OL30X\KH%@F@ J&R)T.AN?!6O-@BA@:\M#.XO(4:-^+U+E.;_ 4;-.3Y-F/[/(4;->;]/G/;;)65 P8@M/">;O1R;8:BA6<4#B#8X4"MGB?" N2QX\^J4T]*Q*%7\7KP@EBX''(,>/_+5;M.;_0<?//33KXY[4A2>)]:? ZU7''B1XX,81Q(A#C?=LIA543$T''D$''G=XU\_!/,DA(BO_#4V8XZ[,!!!19>BFJHH I7T57DJV]\W2;1-=A6DB$&6TPUWYSQQ9CY"FM"7(&W8''GI;]@AA@#\!IU<Q!:IYIIJK,%$$6@5 ELG=)&$T&YT,-SY\ X=HFBN? &8F@@8FG X]=395:A1C4;(FW"!(ZBU$7KNRV^]]-;)W%P=RD''\C%''"A:!_ DT''VXHLU(\Q]-PUV.!6ZRHW*H*ZTS!SS3*U!.%9&.*4ZZZ\_.))*I6NB"*)()ZJ:*&*&,)***6.:&*,,L;:Z*626$+++[+&2"../.8:J%T5ZD@\Y.EY&Q]3W@J8JHBTTZXX Q+?/_BXY@(>J9%77;G9H6]X] @@CAX8XHLMC+! K++''6.@B..62:6:<<L;;[+726$///_+&22>>?.;;[;<@C2100PD_SCCBA"_L<LHNJ01101D?KGGEEL>;;+$.&F@A@2B8,DA2U/Z80%<ZUB- X_<]*KI5!+E,(FD6-+!#W.K1&MHNOX@PK,<#?P144DHOSWSQQ!>M]MIJK<54448?CWWTT$=M]]UVW65Y30A8,LA^DZ*)'';JDM ,R#A.EV^MC4DV$(F@Q;X %!FA7],L@HV   PP*ZJB@@"+H4C_  ",0^NFDG6988( /+''###C?.^NRPS2999YQ_[''''&&F>.^^^\?7?.^^" #29:8W<_+(@HH-!P00E "107"0.R>ZJ!"H;I4X6L/,0Q7L''5FI9IB7Q=$&\SG&=R<, /+7333C?/_OSPS2===]Q_[7766F>/___\_>==>NBOK7;99I=///XS>+''A<B4Y;;U>@!U4,&P-;.;(6]IF!!7/52;$GT"%QF8C"UCWC''BB@51) C#[% L[BLDGR#BBEI2 AR.H0P-*LHL\7J@GN0#BC8(0!BP\(P%K"CO"H[@3L5#S:5@R $D9A%-''N%/K1JPP-_4GV&WZ"N<2<;,?.\QO''Q''^<T:HQAP&\X%J[BHS''>#DJDI1"%J,H!I!H!@DU.%5PFPQ(WXHI/=+DV"FY+IV BY#F[9L"(/EF1;16I^RNL)1#''R,(17/"L\<:''FO_N2#G?<H2D@J\)BDKJP!C8''H=[VO^GM+$:ANM+-F''WFF"D*[]+:8N<W<DF\RR)8B"8#ETH)2%JP,)R%O"\)T*''JU+F2%J5<I25#J\)Z4+JT-[8''K+*GDS2,H6P-!-91A]R%%?!L#C''M''-!(V9&4N.!''I</N]BGG+VE2,I.2.RT5,V#N[7M2&M;TI3&:F<9/"KB\933''N]I)S''^!\)3/[BT=,\*,/D62 2_93KQ,",2DH(YF")LL(7RWD]8G"9&Y>>T96J#R!CH5''P1_*4H!B]JHO+Z!DKT+Q"7;?D31FWF@9%QV36*4LP]E*CH;@>C>E''@@DHEC @U*(4X3JEJL4#V%MY6+S''NI4)3_-*T9=2,9;_ B_@WU[(Z2CGPQ]T*@PB]\@Y''BB@*I/*.&#*%V+"-V+Z#V+WM6*U;,J5*>J%Z,KB@DLUGBAE51@*PG=T+OPQ#.@(!P!G8@A@F"@@A2 XJ=<;Z-_?0+X0@)6,HP-+FDO"=#DJ''Z1#F6,X1<K6\!J]+JT+R1#LXB@.6#$@),-: 3M-!BW!^RR''37S@7[@FI>M!F^,3U)+W>/Z6LI6-+J-KV5/Z=/\8''Z7..4-[7?+6>@B];#BKR91#6/\9BI7.\)-K''N_&5,W7N@C?3=X%$\&^]67;(<"K2A)-VIV$P/<X@_C&<@DM#BAFZSW/N)]+7/[B5?62/^=<84/_^=+7?3V];?89Z=>>0- =P+X/0PN\HG?">@CJ=#@CD90 1_,8@!C^K4R_+BEJ83!B^MW/P.XPE)78MY%!#FGE!DSO9<#U9V);PEC_\DAVC0XK,%81#R.,X5/#NL\:7#GON:1#7<L9B@K^\!DK+JQ#83$IB-92TB>@[TN<HF!_.@@''85Y?$)ZHB?-,HXWNL@O''MM%:%A''L&P.,9''O#NX4*7''M[F:3&==,9#GC^\94++N];83''ON,Y3WK^,9??_N^E/F@GW'';A"$-+5D+>$;Q2Y_>HV U-:B*CT]I_''KR%J85)R&/:4)/NMJ\?;^%P]7+T(BZ5*D.M:%N+6-R,S''V+U>7*VM/ON]MU2@>(?BKS,,6X.E-H6;#,$N9R";L?ZE.J#37+YBM;6\).M+N_;^1(P7/Z4*86-Z=-;V1#^=/Z;#Z7/>7-\HO;6#F^[ JKGT''G5G@2L4*T$5F<,#H1: L''(MZ;@X7/_N-;7?3.-;??C_B@B=3N"+''AC*K\$A58U,WPF)B;]V"6Q''_D@AK @@MX0@D^\FC#GM>81#,N<)BK_NP$K;''IS8;2%J/\81%_.\-_C/NX273&MJ=92S=.<93+_NXZ%8@LR(@P-[(H4R(=:#G?_S43Y3W<AGYE@T.1A/V(R77*UJ>:5Z>N=Z1+_V(  @@@CCBQYYF6[K3C<,.T.^.P\@$BLL!@B +0=!HTPN94K<C\;5;7/NM=;2W(N=;5''/_@<77.]@\<8@%?>K+37_BC%7/#;T;812]><H>//NH''G7&=;572''G_<8R5?=<:G''/FEE77_D>?70F/><*[?^>%_7?+IB9;5*L><;N^^@ -@0@@R8UH>!<''.4C9\)WM5DTIF4+TK]HZJKLG"PRNH&^<078+T'';;5*8?=:6,?>=1''_ ( 4A24X3K]X^)505NZHIF45D<!.2Y2/,Z-=4.JR TD#;_*G4\A^''Q[D)KP?M\(?4@/IDAC-D[3ADRM5B7AD2"P(''<ENCK3AG<QVEBRX(@D*H@GZHA_(8DE6BLC:D#4-A''0E3^?$8C7%44L>D39,XGW!E@TZHG?97<(FA8KD@OB9E66P5RJ="U.-UP7TF?^8T/,H8QR-S19]BW4U8Q4AD]RUQIT8#3H$33MD8WT(31RMXUP2C1BVHUSNHSY,8QQRG=O:HQD.HUU"HVJEC7!<8W+D8YENH[G88Y0ZC7((39\>C3P88Q ZHU;VH]<"HU].HW''X8U6&H[+,0HI@A"QM&,!I#O:X5IHE7D)(2@AEB$C,P@''L@D%8T/-\0K$5T,G,@DJ5DK-@8(B(T-Q-PN#6O>DW\M>W#MMGQ )G+ &?R^B+3!@,&"BZ/J@-@"B[BIDE="@H"NAOCJ@.0 <P+QK<^M>=9^K4*RLP@RMK&!M.W ,1&"A8!FL5>!H<.]BH."AJ1"M0-"K1 FN<0L@=TMF"ZZN?VQ"I;VH%YX&5KPRN3@BW,]RO@LCL)@@+#LCI-FOA=QKJ7@@%1!UA3@A)?!R;Q^F0 L?<6 37+PRJCJK;?NMDM&BO$JAE;%EDP$(.@!?UYIM\1NNF$&Q''CRN8[!E?^]H2!FRFB&KF1%M3"RQ5+"R=-]MG@%LHX"AB]TW,)M46LT6:#YFG1E6:Z!. %D31 H(I3@RW \4NE@A2$]^F7C?@#AQ" I1$OQ8D)68@J(8OJ&8@L*7@M8X">R8#P8HOB^I$ S$KSIX"3MY$4FDG=(X_1LHQB/8%''F9%#-I%=0( MQX#,!("=RH%,C( C/9R=$H ?K7$\2($,CTI-28G3J4Y^+8W\I''_-&QW[UCFA+1P><3Q@\0D56C"ST @XC!#2]1@JV(&"/P"R\PL!]0W.53IR\PU_68@RLP"!B)$9VX$/B#S_LX&V''I$"$"''LFI OIH''B*2"<PX"7.9%(49$R)(_;<H%2NY$5#2&2IC#H"I$]G($LB9''ZA9$\=(OM,9$!DY &("M%VVRT_)"F&SU-;E\E=DTIH9@2/@T+R9@Q]P?0F"4TH+ECH3\H$& X%E9B_)]PNL5CTE&)=V6H3KZX-0.XE9"Y0SZ)3VYJFJVXDD]I7PBY6DVXJ6VIDX6)"5JI54:ZE(JY6/T8/IRT<ABD;%^JD889^C"ZF>FDNP)BC!EX%E56;WXU4BQTL!DTCT:QHA%I<+L@LP@@JWVEX^@0LD(@L#\@L+\I@*8JPTD@HH@@H#<@@=X@H$@@HXX@@M\@E[.PD0QXKMRY8[JT8"*Y4V>Y4(((G]2Y_QII81&YX8"Y;&"YO2T81:V*]<6*Y/>*^?JH474953^*\ ^I: JYOAZX0''J*]06)E82*\CFDNA<V.R2I$N%6V>M''8<]G0$0_=?_JE@2D](UU$BFEDRA/A4O:LA:CTBE3DR&\T3J=@CGI@5@H@@L#@AU[%<-4"M7.%B$A$!JI*!$ &@H=""% *YB["^4U*]>HFL3"*W47*]YF&-1)(W6W*,C""-''TRR 8&W\R&##0&^?F^.4V&V5GQO,@YZB^EO;7Y]?KNC/^L H5 %WVLYZ8)^M^B+B-AKOAL@KYPCH@@C0>(@%[HBJT@A%+D@O''AWKS@COT@@HA@B;TNJ%P"\LF"''LG*\46"!J+%?8U&\JW&SHJ&W].%LJY.(/S%NC/&!LI.)DZR2$-*''MC.]D>"-5_R(JS*\6U&(JE,%HJU%YULQP''%476UZ76W?)L5T"SL &''XUMJNQ"%WY@C8 @;8J@AMB)#$0%!.P@+.J@QCP@ N9@RV@@QQPD.]U&G''Z(QNJ,''X)''W$K+QJJ#^*:-8_:+.G(%''3K(/F:((<Y(RV(+V1D''BOC+BQH+(&)(\<X(''TY,:'': )X+$3,:]H%![@/R[)ZT+3SD&PS%$)7AL4;J] R @QV0@J/Y@#R0V*/UFP@@@[5D%PO GRS V$HB@@1 -R/0@LS#,1RJ''^@Y''[>)*N7J (GI,>VZ(QNYIS_;+^6*''-IX/_=7/]B;+\!+''-MY''@F8PA<H/- K-I!; G_Z,/LW,;4Y@U.2ZYOV(=,5]$NGI$%) B"1@NGR?8?*EQN_M@DJ@@HDX@@)T@M6IQ@<<1H18WP3DB:.YU^7&58;DEW&>;OSF(3SNJ&::I!=FZ<2Z[#TB)"DV;_C*JJ@J8,&KJH2&\H_7LDE2JD[7JX\J*I?P+S!>+H8Z(D/FY_M2K''@17ADZWRY2UZ<T:RT*D4-1CN).I*<5I<+8GW=N@D]0COCN+D1PF GTHL@\@D,@@@=@AMT<+)W2*SF.(HM.Z^P^:K!*93\&)D''R9D$F;4[_I0TN,]@B:<?B<]5WIM$2Z6\YIH%F9:8>I4!&95<GK80"J "2;L.BR 56A1IQ3-%EI>JX$X+$69)QJ#H@X(#,PLCXIB/&9/C 0HP8O<CD5@A, ,BG[@@K/B;MS@BA\@CH;DAI @@KM@@A=@CV,(CH;@BZ^(:G5&M&M.QGO0 MG3L1Z3CGK28FM285;+LZ0KG8K+GJ<3L5"3M363F322Y::*W3S3M1<.<$9.94*+HB_#C<_Z#SQ.$#]!(J-Z4),,#@#$R1D+AU&.[L:@@H8DBDX0BJW@B@2B;-$/PHG@BOSB1-,.0DM@A9CTB*W"95@-N9G&=!U2>ER6423&="9+Q N.JGE6#YX''HL''2<\SORNK/G;Z/LJ1/Q!*2<^$2>KI/FL#6)<-"RO"%I%O0P7(T6?9QL;>%CUH.!L0@CBGBI&KF$D=HCL''@QEI@BF C?@31 -P7 @#1C@09 U:@8@ Y@@S0#@C&PIQ>0@C]P3+B8+YH93&_-(#&\%=**#W);*F:].A\X4WB=%64=+*=(5<+<.L/[-7G=0"S8.F*Y#@=Y((L;"6=]-F\9DB@U-] B''</"L$M:[I@=TO4ZQEU2@ ?]%UUI%SM0@[64(FEL0Y>\PO7XD#4 I$LDUQP,$C.P"WZI1!X]4>3;%<%*''L*;2F&]61R=6>_\61294T"J \E]+RA<1"*(67;*+9BZ6[;M7H8,7SCIPGG\)"P-6<R]$^''$FU8$L4@-X&1C/;N#S9@MPEPK3Q.%+QA 2+;\@K9*@BO0T-^(6BOZ_9&[53-<?48/.(096,X&&X@7_I58&=<0J,M2G]HGW.A.+NCQ/J:H:X0C;*3Z2(4VN-\9WCH25JM#T2#WP"M''!<''9*)''1&LL4;W</(PJ,=WP84C*_L^CIGL"U>)BI[J\O6L 2W[KG[NLU#^LPG,\LZMN3GY$;G.SRO^SJ^NOO:>OBB^RWR-5G''*7,R#I<OK4//-7G7YKT+]IZ (;/*D3Z8X#W$RM''<#<UX[)2+LK8-0LNP@E^Y:X5DH*,.L@R&ZH7B,H0;)4</H2B&^\8625<_)I:+(B@7,K+YLOJGLK"]N R&^"SR> K:N\DNNE*$*I7S+P7[I[ ''H2\R)&VV]80H72#V&XP9?<(Z/NYUM9H"H0RU]$R?^ :1NL>D;62,<6B I/%%8+GF%3$.R;+.5;$-6:4/@;,,?;+93*^Q#/,!#3%MB/[0-6(OF:>IX*@+>O]&M10''\%+%&3-D)^T7"3XJR )X=$W+K:J3=+F616=Z@6(\(3C3\/H4(.-_4&#ZY6#:>;F@6"-<7;N=Z:(4H//</:22^7_646#!0.9=+7 >''6SF\:#%;2/KQJ:K5ODH TR<,28#K*''X&2J*-)K\MRPK''/HME7[E 6S>7^3IO?#-Q:"3L7$J+>(L>.J]4.)&Q+3_R*4V28? C73"V32I[;HKR?3#O;\K@7.MB(61 [O-%MRX$;*)O+?:TG-0/*N5>GJ$L,''DEW)N 2>6C(_#CPJ:GPN079K2G\Y=-O9=X1=_59?$&"/.QQ8=-&,<HJ;=''B_=&K/&63Y=!H8=''!O8X%[$V''>5<FD#)\F[BS68T,[-R)C8(YJ4R>M/Y887J.(,!F=<LG.4XJN/W^>'')UO;L"M4+!>>Y=/>Y;O>R9]%*VO<$#Z4Y5?>*-/>**_8L/=/@JA434(8.;(\DK*=C!(''4H-(,Z^:#YI10%O.]1L?MY)?@NO95JN8BOKPLN/?F9*:0X_?L=?/-E_?MO_?DW+<''B?<ADN?PJ^?W<M>0SQ:RG59Z@[:-NBX/_C =&1RSF)9]Y]<"Q+#];+?=^6''_=CZ<G5_>?7C1@+]*3X,DF 0XDCB1ID2OB PH KE2)D6IB"08,;KC*\"GF#1HXSO1+,JAK"1YHUS7HL*SH!28 .X6K4.AK$SH46R[Y<>OI&1)D4SR*D^WMCA@@*G#28\XM)4:]L(590^.NBUJ,D@NB@6/R*4:=U0VI=B B@4I43RZH$RCR-4[XD9Q(%F[\.7[%."=;U"7_-7K59@_.5N?\-7<FGA]=U?I]14\JMDS>F.:E/8<"AGQ.F[C%19<. O5\NSW+46<0REZ:&"URCU;IV)<Y.J!XG@@IT)UK-N%U,5A-&H<+MJ[F3YL7H?R)''''-39<._M(T>WW#7:]_?*6J5''9;;].7[07S%2S+,#A@"%C<J*+0+<=5^&E;S&E ,_*''75<\.BVFDX-WGJQ!L0P@KIF=C@@/=C\DDEF10/0P\Y#MAA@BV,$LJ;KL0P0<D&=OCBC3\L,TLPR1SQQAIOUOD4<#XH@XCW M-MO=&:N"F?)&;#*#7]_..*O[BBN6-E<X+<3,#0#%P2RRZQ]GKII9.D\$()*7RRHJRT>-D=GL&2S3;\^&S/J3I)=EF8B8$<$LLM5XPP1SYS%CMNN-\\,\877]Q0S#/!;EMOM/?$L<=A=<SSTDDO#P@=?\+,[T.+_-S1/$^;9ED,HQN#L$)NM?WT2$<;AWUTTT,M=US?T"U3LZ&%8GOU-=&*@)N>7W34D[<G3C+ O5QMS_UWW8O-]U! "QV6VFP%< D@@:!:0UXZY[01+D&WB/J=HG6;8PTPHBA( %57DG]\\,,5=51445U7WW[[]_]]^NNU]59::;W7W''335W]__.L=8DT]Z.,16?.@>7J> _G3DT^''S+!@.@M&NJD?"".6>FJLL=Y8X88;=/!#$DLV^VRRR3[9YIQSU''%%%%,.^U$]G''#AV/3V*;GF)68#(FC^YHPTJ0 @FF"EAP(2>&"$$5Y:ZZZ[]/))*JNV^&***;[:Z*235'')+++/6>&.+C8 AA@6(4(7K!Z>MS2,\1N01KD]%A@FF?09ZFJFCG/KNF6><^>C[[<C?E#100 $_?GCC@T<\\\XU[?310"FW_N?EJ4_\\,0G3733/"^G''OOMP_?\\<1EK9741$5O/_HFGDA@!;A .9''!)::"M,1-Z0ORT%!_LB-(DL0R_''#""3_>^NRSU7999)-7?''''((9]>^.*+-?9:;KOW_//)XP#^!@,>&A&+,W([FJ-Y^TZ;O/Q/TH@C@%C@@P\BBJB?_/3357=??//7?7<@A%B@@2Q @P58P@P&THDKYF@CG_!@BDZP @  @@LR8KAH>\1&T)%V3+X")!M@R41]BY?CW''B@C52@M6ZK%@)[>DHV1!BFU)E!CV]XP1/&<O>FMM2!C''GXP2CF4H]A[NDP#\#CH?(0"TE\(!JIZLP&=#BJNY3"CZ/801*>8@T;^L@G[+P><,U*[[ 1G>5^MS-G#VT=Y52#&_KS1/KA,X501ID\Y4OGMM[1#&KLX1?MNBL4.-E6_AQ$H_EX2D@^L)BIINP"9=!HN:*/XGFL5RL''*\]IV+I<&JR$IC_)Q4Y^D)RVGNS,H&T[7HUQ[S?[$,4.!SX..[IV,F0%KL?7*O_XD%("5BT-\?&3V0K)%0,K)"+KQ<1]1+JW+A2&#H+IR6@&,9[Q=FV,$E''MRU:S&]Y4)#R[ZS9/\!NWW*($6794O%''&TU[J''NXFSW''LR&*P''[??]F\:6U''O^\[3T_Y,)32''RT=?_+NSPNH''PO^Y340FUI0CQQ=BF[+N_>*S(@;%YDL#&-BCW/R![,2&^>CCEI69CY"4T:L3R_)F$6:0%C0::T)'')MJV1!R&L45)S\-H49/Z%JT87Z%NW>+S$.YT*C4]:$>KF%R"I-V(R$W*T)7ZUJ""=J!RYR)U''6+U*H:PO[JZS6>>BJ%]!!VLXA6+VL%:U+N&-Z1+QR-[5])V.K9U+&:%Z53+N%^;9!V/^;5+W?W*5;]:%FD:D"%_?7)X02XV,H-EKFLU65#HO%Z2#*U,YB,;V\,^5&C(:2(<L8-Y4E9V-I<][V!I^5+S)+Z4*4T-?6/CJ$1I[\V+T*F-T6)+K]3^U+^69V5.^;-[70XW.LO=[WFEZ53"G%^92VT.\)6;7N\6E;+S%V95(7-]:&KW.-''%;''Z=*570\!^"G"P@B$=0P-;(S+7+YV=;7_-^>LYW//N%[77-^5?<9%^?>>U/_?7;W0@GVL@C%.<E3(/B@6 E@CQ@@P(( @LJD@@CD:Y0!R5<XP1''VLL[9''BGO_1!DH]X1BL&\X%M_FHT)5#EJ6Y1"57<X 0S @PD(DBCZVBV60@ ^C+VLP!<?FL A5''HP2Y2$X5<YBP''V\%KY''JS''_1$JD]Y2%N&\)V-_FT,Y5''KP.Y1%>^7 P%,22@''4@#X3A-<Y#R''V\5+Y''N[7_1&NL]9Z#.X6@Z^MPGKA@P@N0@a')
%
classmethod: R4PReport
imageLayoutGifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 489;
			add: #Height -> 300;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '"9J^ZJqQNY7)Zs\,U+M0U82=/AE;b2''G;+EP*ApD<7K@G1bpnpZF"^oFS,.r$jmE%MMt5$9[(Z''/1
	N3PWiU#OD!^HR*#]feR[dVd>hn&g4?>U;]"EE:I/Nj="9o!^L4hra7)[*`,UOe4VPIa7AE;n6''G_CIQ
	BY?L<7KLK1c@1t[^:."FS,;!$k<])Nf6Y,9[(f+/1rKTY,lGWD!^TV*$-)iSt''%l>hn2k4?bm?^:\iB
	I/O!AW4WWUk5YJ_!!E<&!43RCjo>Auz"L''NZ''`\46!!EE):s:$_qs%i3(]XO9(sV%X*pWg&(^,$_W,
	Y''u)?WdC-3K_;f_tkurr@3A!!*$!+F5^A39^F:(]\a[!-2`Q1_sbuK`H=?!!&&[!#>V9!<<*RW%e8ec
	2[h[!!IHFPQ8Yb!RMY`(''">XScIt6!(3F[[<DWE38ac;k5]04zH63,H@K<*q!-`VeV''MEc@Km:8DX;
	QI!7:cH2#mUcD?0#''!!&qt!9e(r(]XO9YQ-<k!<<*"!!E9%!&4TVcKP<E(]\^Z!4_+jk<K"a*WQ1`!&
	=*Gc?fO,])[#)I*DQjqu:=Hrr<''!!6rF^*man`(''&Q%:mh=b*78H-(''''hIGlF''urru6uD?Kn=;"
	-%;kKmlZ''`\5W!.ZI`(.&$9[01[H!!3-#!!$[4z:]LIr!<?d5z<!WI(kPtT"z!,`nZbl@_Z!!%0B:c&
	1R!!$[4!!''e7!nlW7l0&*a(]j[;TF:i&nGiPb!WW3#EraDa!;c`r(ioZWEA6JSks,4c*WQ1`$3CV9c?
	fO,\,^]&XJl-]%K@8Arr<''!"6_<:(si7Z''`\5WEAVnY!/S#_6CYajE@dIWkQV#(D?''Y:ErZ1?!<<*
	"zE[06ocKV#<(jC-''_Z<TT!!!9)!!<3$E<$CI!($Yc[/^4#!09rB(''"@9!!FtUcn:.mB8cn+8-EN)~
	>'));
			add: #Length -> 16760;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVOj%mugIf?YI\F>=''''G&\^%ES+,Wf$90&A^''WCXkk=bjICb4mV^La2h7,ZBIh1b3IcO6X@Q"ja
	bX1#b!gDg9NHhZ:84Ib7-g^SU?5gKIr4,0Vcoh"[u9prVbQ`N9Ug13Nf#.r9=#D;@`SMaMd41dUY^,0o
	JM#cgE(D<?.5E2qguR6RLM8^e)l\e&Of>[^Ul<eGht$5\hp''535<1cMT$a:BZb&e/eXs$8TL0p#ecTm
	l]#e:DKG,RY#]at=1PpVrQ?X9a''!UPf=ke1NTS.D9qu%=?[&X"YE41udr''NFc#f<.''msEt>4CPm3T
	NR\HAhL/L@;''#&2/''+:eeog->D2)=+>Pek;UMjEVi8YpI@s0>ts4)NTnIN''#-sJaXRoNMQTB94''>
	:#*UR21Hi!Ka5eu]3f<j]_7Jg/1r-OLD?Up5af1"M"C<l@PV<_e`p:>].>i*4Gf&G+e_:Cf"''k<uJj4
	!j1dG2#"+tEAeFd(U;p0=8c6F+5PFKWWQ>2A`;4VlQ%m8On[F/$;AbDtq"+4HfC;a+u\%G.n0[q2q\''
	E''JXoR`"`Mdk)\777NdRPhHS/As[Zb*o9g;B6AYh"I,)e[8dTouNeQP+imY6UqS/;mM7d/:P?9ZS`QI
	\>7.A>$NR%RKuqkn2H9"i8)T,ZFf8dL<oRIROGacF2s>bCjK7,<Ua6H;ULN!jbZ;YJL8Pgn)7Y=^5U2l
	?#Z4%:C9O4a2RW/Y..]WL24qYIo5"C4#8''E*-2efpIKcsfj^dMT;,T4jqOr]],r,J''RXZ091X4Bd5?
	j)Kj`9?A(M/''Zb2[1f4uP0$`0@(O<Pon%D1s%K&`Yf][:CH487q7fpq=f3mogk=NP12XOg<71.Kmfj4
	Q9g,W^&]DdOQ&/nP^-90OAYlPm9MZFHHs1r=.EU/Z,n?MnmA8(ooqlqGee.=R[F)UNt''fU<T;IeIpC(
	U&[_ZG2]LN+Df''5C?`hnk7iI)fcf1)feE^d#<2@L2%Hm!nkrfJ+&s=j=t6@crQ:s:`)lPeu`$TQ+eh&
	HBU%Ws,]m7k*MS31a6I1dqV<hMU1@fV99nTF@XDH''!;,"X:&ckRG/cSihf<`ZTHnIgc\*p)Ek]IWB2H
	fCYCVaA\@,0]T"S62''L.P>u^?#[**40e!J`Z8P%%TD,/O=ZOHGuDL8HZ91<#BCWqY7:sIB''7<:UQ8i
	Y#n>hlHN\0pa_U6lS9"`-AT2f[u<DC+?rb9AR27:bT=XaS)P*++-lrK+t&Zj-3eIZ0@''mko#Pk5Lg4%
	G.O[]W\kZ^#D(Rj0)qK_i0N-PcQi+Vg&&V6MuF5,ON&/.7A+/NMS4k=HB?5Y/^A/NYXQaDnBo$Eo+J&(
	L2k6XLl$$hNP;.3h@njU*\J],i]QR>,YNg@N>Kpi:`5F#Gs5K/iUe5dd,;rc8*W_5J$FsaURq#**mkZn
	1jn\XNBV/VTWaqY^Sql_m4_3Ip?XV\l^dQ=gadrJL9Q)kp,YVG"a,''.<Pk!d%br[8#Jk@`Ec>/FV^m5
	@Mb4f,=g^]9A'',.@6S,Im#nDJ;fXm&E0e4/BQH_cU#cH%gZ*1/PS";$l@m)8>5(oW/UYj*RkO;\''5c
	W$amMT$7)^M-IO3Z+gA#lu2]K&HP=JJ.A^R6/5/3,Urga_3T5[.e0cC''E3eC9(RK>W5.dnRmTNj^AOu
	l8&^5+cnUfd+2/r061[Lg`c7H*.O"kTCrjV''In9_:o1DJDUj)4NS5UTk''Q3(H@^+n%qXXdl0XmOgQ8
	V;(u7?R>>M28EgYo]WI4XVnjBj4F3<Z!M(rH<p#@$Rb$7\Y=9M4c&Acp0KVbiiUnJ:[]&2SSd@(N>?`f
	Dd9NtT=H%ITZ?]g6G#;&#jFgo_A=pRG:^9<>h`UF0)<PFhUob,W^lg2KYr)L&OP0^@7@OU=L4$82b3na
	DJ<L2L-Z#CHEVLOMAR;acaka,,1aQKb5]j>$8jPmO^_&:L8#&d:SDaQQBC)L_kYQ?ETH`SD0F.:EG"lp
	#EkgV#e2[!Y\ppSbNRo-(E:)3O7a#`Xs2%77T"8@D],=`q$c+369c^pY&HMn=l`r[%P&`,/;C-+[Qh@>
	8f=JB!7or9XQ*rV)I52AqQ>[&W%scD)+L"OYQ<#W^0H*"^:ZRJbHk;''YHB3LBCN81&#cpZpYB]nll,t
	]i0NQ3`8i3Z$''Z-Y*`s"$1pS[.nm[ouIt.?E3LMGK\l>ksmd+t''YG5t-2Sg3SRB_hJs3enniJm[?=s
	&A<F*DU&1&aL.;B/AT2(=Y7pBhg2`L@#W,Ub)lb=&P1H@oT$K76`NiJhaRrTgNNJkCCuA,cOk^V)jX?,
	3rpWZo*(<X).4b(P8+fI,[<4Qp.8Z9RR1Fpi6D[nZ^+Tls,El&TBhF7fL=%0N_)durtU_'';pg-KZ8!;
	f\rqV''/Q;QE^_C[?.s=6H"pmM-Tq\9>JF:H`#[Ufj9''<Ho6,c(Nh6,TA6''hpU`>^NW&Vh]%Gi-Pb5
	989n`*pOOO`VI8/XVCb.V.d3Ho.0ug]@hj-\KaIgI3_$>G2"O#r*bJN0-4hhU4B&N>,;nqt"P2W\&%o(
	YfX:6F^ou>@c](;.ki-_U3^+h?P3681Z*eW@d=HTg1i8''jk^+E"*a^Qq1WqRCcdU]t$bYJ;^fomP0Ho
	mHR/j9[:]D7T[]`''ip)q5(6\*a^Dc+E@nG;Mq>d:>D''*t8;O4,VUQ7Zo,b_>#qKoGd9`:O)>cmcT"''
	-F7QPCjgc+gkrgI5MQ[k+]X/e#U_.SB$c9t$kTt[Ku&4Njaieo9sbbgTKp_gai79I%n4n%`t^-@\TQTj
	Gf&-T^]\A<_3WPp_J).^6q@-epVd+HkNlX/O&nJZ_)qR5\Mj2i.=9''jhm0u&:GYbn^q\Z,fp!fq?ABB
	j9tE>gpX3(CL0i2S:=heI-Y-T^$pP#T@%.;5@-uqXjpU"&_$MMEPE7s#F?eUjB\-kCL-bB*qD3r#PE)B
	"T>O''L%^IO_dA:D\V&D/G\!!-Q?VnK`a8A?A-l(R^qqGb7@+K-EkN,$fO7H@FN\h`bE7jk3(iJ9c%,5
	G>L\&"s%fCtm`Xoj+"`lLXWEG%^/#c_Z@mNRd=Tn^d],&&i/@W9)`Dk(cmqid"GGU\2PF;>p#./<I9j2
	VH/Pn4Sk.>I]&I^e]Un_U\,5SRoh$<FRTa;`.1GIfj7XV@M-09k,`U*Qh<OR,3nXaS^CpK7#=e)eUj!n
	8]$\cYRKPrGS=?kFD&Aplek2&"QP_p;%Dl@c73P)J-k\(CB"N:jCMU''?WbTnK;blgtJa=8TEapC.V_0
	,''L-KjeJ#^-h&qS_Q#p5J5bRTTh8BIL0/#M8*bD$J.i1/mhHq\[FX$@>H8JTaeJ!X&e/rl.hp96eMK3
	P!ig0=Pa2J"%an]ZYpHEq[YJn3/ICZI`OQ*H"%IB''pmm_pF?Oc/.m4_p0B*c4Z&;ZNF\k0q&ck);?<l
	L)<rnl;"j8''h^SOC,lCRhO\$n''$eE+aa62Wc3a#bS!9u$p#\+^[RHcq)f@EC!HMh%jGX\rn/U:l3pl
	*D.n;rO#>0Jf2b,+"!bF&_Uf`qRHF8(0/:s[DZ&u%e_*p3.Rp.MEBk[OKT?LsT,t=sa\qC$+^lYqJbW5
	Kh59#JL5[snGoH<!lY\N2c@=BP+[nrO:jomd!5Hb?$>LCi''[GhnV%>69:Q.oW+#50*^nFHlWK/SoL<3
	DM9%<JCDV?RD%Ki)?n$+-<QB"Te-/l`V&[8CPhbkKSFD!<g&cQqgg]1p8Q/.d7?T?[FDE<j8J^c$IR(9
	XHN)m0D@UCldFQF[CPh$PcWk?N''@b#?(ZZePEURZKA:+c!9!V%CZ''Ok97Ws%''<$ZSEqZA]Ghh_?3N
	_M<0rhk@gMN;hTC''Cu&.G:/B8@?krs1KWA$NbRAE"1)5cE2VDj<C:<G?l+/F/D36*0]uP!)A-3T;m/5
	4r0oD)ZkqEIoP.S=dg;r_c(APEn^!AOJfT=\i0/ju),e[<Bf:TB#.BAeJjQL(4qqE6%V<a?&ZZS''G!i
	W`sqTdZ/_i1!D>/td?iP(WPC0Pq1!p9HoAU_c!D3F[Rmqlb_4<*$d]IOpN]TdsUP[HeO@O''\%X0t2-V
	*r%M?A]A:oJ)f..Dg:`&LLPYL@RhbIn;tP7g::.65FC\0!L")fNIpI/\Ngu_@*fh:L2&g"$-#/nf&@a&
	f`a*NhsPp].#HaO`=<''Ed&mi.:%pWm^D1S[:JEiU:5]4-t?VqM>=C;K^Oa0''OaDa"t7:/XhSc(jNOj
	+K1UR"EBsXr''cl4T2^h>Q7$u&8''*p(bU*#"QFK1-O&5f0?/m%Sr7EUZNA+)liBua)\m8n1^SteU@M3
	PS[RP_E^`Yt%dKu$*&mo;SF(L0079!08*8FCp,E+Q%#C!\r;!u]h6(dg"jDZ>XgilV%u$d-EubV!MO7^
	t.N6m+]t"1S$5m]6Qf>FlN4gMV^b9LbBF-0Ne4Xj>,!8Gn.eg.F3k7IXDU->rW"9+Z3oUSUtC#a9V@.8
	>85KL*=0lgES@_YhM''M1>bHQ*t#RVH''+4''n:)<[a!R>cL\F&J"\jJYIio*/H!WPZdK15@O8C?+nTZ
	qf%jHY<G2Np\q0g6lWI.fD#,UH5th+).0pL8"r#q.1:Ao7gW7s@Q/:''''5+lHqa9$WV%ZcWN8)qd-G\
	OQ)[^l.gpWpG4BX?E_D9/1Z";*C0fA0iZ$/)nTAIOLQ"+R]Bml&opk$_8DI-pW>Pp`YSqFXafpKTsXWp
	u-nCU^1kI/YGh`"Ed#_b''aTGcu)]?\>+=XdClt5g(!$T;Ua@9GD3`2mM\0g?sBh:7;nMl:c\3$Vo_ok
	,d=,IK0(+P'';`t?T.O_.DZDUd;-<GU-Vm!Y`,9+YDR//M@V8FV!uI:2E[+Ns"Z_&p*kG[2*X,K+&N:l
	p.+.PL,]MSUAJ]cM@mjY;3`jhRoXYKVJ.?uY*+:jg)]EFFem="6Z<d2!Iq46lh7H+5^]X.U\PaVnuVmh
	_7D9"W%S</]bUZ6`NCeNU.o*-7?4m$j]*bpSeUfXke.[:`i$g;hHY*Ukf3YV.Qf%2\(F[i.%6lp)lo-m
	MAN)"/3(qgJm"?j=k-M%3/D%mW>;;X;"L&raI,uW1^,EZ''_eQ.s"J]?D0S^kRa9A?/?s9)FBga5,Dns
	r.0h`XN!U3/\34k3Kmbo3ZeM_G4]?]93n`(aQ$9p+H>-Z1*rerKeTYk6jmss<X(oqJ&UtN?`LG1p2-f4
	^rt"qYEpko%l^q$XI3CNra\m9-WUWKZ*`U\LQ.3Jg(.&,6="qZjFM-gNd3IY_e(`]#l*Xi''EB9``md2
	>9Xr<">>)+^0X&@g5+htaj_X5"tD=X.eK=T$*:tW3bWq_+uVrl=m_-SiarpirXh''f.m)*Jl`?:BsMGG
	kKr["q(4g3>P;MWR;XPe)668/3b&T8/*LU46t#Gs/.qKfSTlYF>&kOG4X85EpA445p.pEd<[(C;\3Zf?
	.t)Cu$lS4,Pq2Q1@O?eueUT<$NGd`X3+FRHiQXlm$#/U:mqnr4:`qS4GXRXu:8L.+IjuMS8)a.=PA^(Q
	?$BB(;+I]UDorLHVdIcI7**$d*DTs2HC)[g2g0#mlJik"d#?%hK6_VK7<_5Uf%YmkIKsn!,aFoG!kY+n
	*X`jqQ)7]KZ]7:6''9r05F>(F6VFMH+.ggST?i1A)bc)](N(l?V56XD`JVG!,GJUD#3&B2l:8ZJ(:5*)
	,2$E9^0(T`f;`M(a^pj8VRaIMABHcdq>80rfo=P+e5&e?QH<CXsPrRq9ro@*UTJGFaLa8g0@>X$/>8>k
	9*jpo4kSR^9(/I]]-7Zrcd/kh5u$%?dp&)#uFMV[=V''<g6<5-$C&QXH8JB67T+HUafu2pk*Aqd*V(7i
	gW><ECRs6]@38qM2U0Q$71ng#12SDjZ=*4EZLq\tRXD6V<0%!Dd-\T[n$F*,d/`.M=N7H+&_c9(iS5q''''
	+FMHKWS,64[l8A:<.Jne1LN8dnW)iZWH,]+o0Vg1&2Z4hmghR[JSdjpLZ''''eN7gF\U&ar]ihchq`<D
	NXq7Ir_!Ujq_Rm^''NlE+GL@j[6QGSN:23b\Ke\kqaU8(kJ!)YRA$]E,sP@LOQ3[:ach;!$7ISLC%]NW
	%!pN@iNm9P)n&$&W(nJf?QmWhN%l9$;T>)c<(FZ\1PYcZBLR>kKoC(LnLM%*9[A''^U=NPQRoN%(JBA_
	hFQ]!JVEM+X''Z-G*i6H]i7*R&jIT''WR\t4."[H`1U4pC_KhpSh$Ro&h'';UrLeS;Ui''SpO@NK8[bu
	Qc%$O5;%dLDA*!N?_n?jqsPbl%P_H7_b>[udY;jqQ6:N#/SHpMotD@SSp1>HCT=0oc2j<U57ZJ^*L`gQ
	VTSSJ?9)K('')5"r@qN*2fa4[h<,Q:>Aa?3(Iu`cbjD:a.l8P&?9iKbDWFifn4Y>`Ia3<Z&#bT9;e#$7
	1"!a.%@>J*(:5mZBGnf,g$`4sRO0Xut>-)5"b\)9];M3;8''7:63+TR?kNbj2*Ee7#+c_kTI=gcI>[tX
	XMm:VG:hZBr/Y!cbWl94uj)8(\)WMA`WIDi/k%+m0,L3=T7b?Ms"1&p#g:>74or&=\0ht:o^$RrK+^ik
	gJ%cQk_C]]N5BVUp1MKBVR.hC=k55?A18''RAC3HlqO-UlR`Vn%RU(Foj.nHN-I_(V0o]un`j#riNKfA
	[NX*k4l[3"BH1-7:`li2CPp-R)N^\5Fa+kC"N;^0R2WbM%,/[fU1ciN_PqO^>''d/LLTTe1C:Sg^N_\I
	&MtLWsTA:K"=7b+G[rM+bc=c82rVhh=YFfH#O0#@]o8>SSrh7%!?J.kFJ>+d2YDltBL-fChFT]sEEb^C
	QJM<Tg6K*G%]eF<Pd,X,p0S8A,Y8/m^BT[>\5+AIekj:r[LR6;/+A1ia$%?gqF0pX5!`=f]^EOT40Yqm
	GFVLd6&=2Bf-_gk#nh^aCkYCEOAn`qh6dXF^$kionDPBB6167^&@(TG(eu)\=0(cjs+Iq@Mg.n#ghCD=
	QC-60Z1kJ71XLmN(U"c/J98+VLVM*@2--4T_,q0RU/hB.(V-d)9l\8O2>`[`A.&bI/O$-5/3P&`Zau>,
	1)+>#lL^,n]/5e34<cs1n''frQka`-"0DV^pmh0-I9jLkVT82s,?-r=/UV6M^J2"#N''@H,k9>!$/o`*
	PlhG]4($5UjRG_,6j>-Y?Ker/.odl^%''INeos=(05Q+V/?PEX:6N+Js*HZ`aGZuOioHpW1FZC10lL[[
	r-J[7?f/^euTfUboL=2&PfeQif)QYd7B=Qc>E)LD1j:b/PhWR/[kFd:`cZ1a[''?d@lRs%FCdoq_F,j#
	WiuC!`SP\t?1%%HmH!!9\Fl!8eT>qUEkhkp9:MZFHZ@''l1-%Mq''LIQ:mq<j%;Q)t\HD3hPaG:1&3#G
	jgJIXC/$S.\Z_)l/M(daf.&7T2gCfuOSj**So50h;u!EI$a?tSXj$q>dl:q;/,%9%Eud%`2Zi''P3i0a
	V,1n8Yo)mkOioTTG7a=uHSNZ0YqYEuR7\;Dc$];)Y7JfsVOkBU^sq^F*/1A7Nu_6ppR57,YOh;`=AZJY
	JBSTLYbBrh(fhloG;"kPcOkchIC66ZKHTJ+e1F9"(G?eh&F_+JG+T2)UKo,tY[/_PVjk>9A&(OF@9,''
	tB[!B&+[ScF3HSk%RqD1pk#R0A5oA\o^aGa]@OOZ@I.Ub@DL6QsKNF+p@lNa7gN(:_6_YNM(`^)!Fi+4
	SD[o)U:=B]14/.DG3Ch0Um(fo.qehBS?V)X=t[$OB@`bH($$4I8T1X0C8i2V_F>@`/TaWRmj1#0G^UAo
	CW8,BEp`dQH*=STFd3rgN&5-d3Yq$:SGjHiH7)`W((:M\tL19ZWZO6WckcB1s8pAm?SJF.M:-Sh%\oGg
	,NScY.R3NW_t#kK#8>FaND%0T$46tOm,*S8r;kNh)JYU*0gdFeH#U4`43]GhVAgMM0kWCXE18[9eF0*U
	7<h321q7>''nT5V]1E2kA(VcLYheRT]RT.()YoF!T9@%1r9<*Hggu1a@$QAB0Woa-h,`%DJ_Eto>5V.%
	lT4YlmRTCSM!Qp,@>7&R147PC+(6jtb$:la)jJ^C12Tki6X$0WgctU`oHEASe^$d9$@%B_#Lm''a*H+-
	9HA2`b!]?oE+>^nX#9qtsld;e%`YY2/,84e(.MVAM$B_g[^WR!`SPkQlfPcC%Ujd?VE"M;a,IN8ZSXL+
	Y7NokNn(WT\Oo>+MO8YR30We0cGh;,Oh*%])P''%;X_5#_K)EG%<S/qF#/bgdoI7o]V&[n\I`M#uC;Z0
	e@^''sP12a]B$W.A@+LLBK/@;]"-pcs`YrqQ@XMl2-h@NH.kH"^mEW''m;[0Xc*CFC2X<-+-#5%Ho<7O
	mX[0''qgd8?a^J)p1aUIB-9cle;>s1^j#bSOl((g7u%2(W<P%e+\8[=R>0&Hrj59JMdohG_1fCO+nl@A
	mM-ZTV`]K0E\)Xm_X"o`Y+%_V>A$m[>H\-Y9Q_CV-''GGS`*H0bG-\_;JjVU_)Gf/pXsg]@08F*34RU6
	7*$#%m*tgn1.2k2XK$0ZnW>e2^%h\SV>W7U*;dssC9j,Y7()U$,17*Qj<NUaLg-N>[A5C10qCu`VcsY#
	%"I7,a+U^#m4KnW:W`j)\rTZ,jgcMmI5Z%l!U8;`7BH$#''CPqPbSZTD>>e,E"Le(<."+%MpB[/]\0[C
	$\G[M_uKEX''Zinj3a,W<^irgN^732;Nt.lD''JCA#^Ps+!cP*#sdtkX''hDJ:KDM7l,d;@;s4QOYE8#
	''Lbgg<(tq0OI!q03l$9QVs+5d7E;(Z^D`OI0-!P''r`ee$Z!&pZX?)5fa.W=Ge>@mU9bkrU"0sB?!21
	K''Qp?>9*bS/CCn&23.XcemhSBmDO<gN7bpO6pU9!7+"9m@q3+Zf$ed+?.Y5PEh=0`:p''A2MV&.a;.6
	d4)r*t;XPY[u$LiWFZe$nm1QJML:O5@7t79TEX/c0)EJ_WU?T5Tm,@V3&p8&oZXt+4lNK87p@P:tN,b"
	_c?$SDc-5J#HC63Sm<_mo7G>K''HZV$g41kL-e!VW9r+*?=?L9JT2H-^9qM-olMEdHau+:F0V57TVXn)
	Wq3eslR])L6;jl=q^80r)us^G)YQFgd3IO,6iC$Z"''s^EK,''cKfeAmnPHd#dRbEuf<)jj*<0Ur&8DK
	b^[?L)Y]eEInLbZ*:pQ4!la1F:k3d0ls#iCTj;F4#TCYB]Pd_I&[.3\m.?[C&brcd0_OMB`/Y,#q<0#P
	g7g:GZGN0".@?.+g$paMGkPEV:,b$"k.K_&$ue8Qk<?C&dDX*Cu.[O:SSBAVOO(\<W:O.04:%3(eYYl,
	AtJ8g$j$(h25E<!rT8#5tm8_jfS<u37dFASG>5[/bYTFps-GR<^!BH\&5>@Hh5Yj2A5\J:KAf0fU6>um
	0Fl<sL\X<tLIEc%,j5\g6JB]cR1<*N\0J8*f%AcMK$_\CG\!,%7UM%qgi%$PY)6''WEXbB/7<WZr?[iF
	nJuh+A?Z-h''BXE+n`\G''T[N?>eVQhr9jS''67EUEtXBl9cfab_D+.$:_F6)#"VT##8>`sI")F!GVDX
	gf^;N(7d.R`Q9mC+H_n(7U''=]@)8ke&hm][;=\r3k>bODbhdtu@FC10:gj>/hB-0HYfks>D''BC6]fG
	eJ!n''pK6+V<;]F`8?nh)@=9bI3?=$mqp%[)pqH2)S3`8O%r/Q(6]*anqIOd6`JGHe8%7fnOia($8a2j
	-ON+[aQ";X3a7gr3/''s\EiiVJ?.`20FD2*US&Em&]=cM#`U%s''rih?;)BueLR"g&_q2CoS)2B<lC&\
	f\:ujWJDG''D8]CDF>0812gfEZqfDYaDFm''W=hCA`GDde2p(`-fG7iP/@=iR,:K^$8SUB[XfOL+A#gJ
	4oLhqFJHEgPBZA4Nf8D@B&rXL5r%CpU9ks1n@5YM5LD!R;O^V1@i$_k;\1r+P-am.=uOEt-lold7:sjN
	$?XIJs-QH''*P&YH(:srE>EO[Ve5GI=fF/A/fUYs7O''&ZOm7''kjOP+!FE77OeXG[fr.<Vgae%M[)/M
	8]JjE0Zg*XQbqYWX:3Lufddu#]F1sa^jJ]4DC_B*Q*r#Z-mjmJ4ZI>5bJ#d+XA&\^@NUii]4W:hsQZ4A
	=fj;8&>*8(2Uu!IXSAHp;a-HC2QgN=s=,lu$h<U-8p5^0YdACQDWD/4:HKc"2!!C!i(#bFY]CT>HMf"i
	:HqVr":;+2o6jTh2T;O1mS;S;]>dO"d3N,U:?@Utb<3el=gI-UEE%qa(HZ''&B"Qr9BS0Ll)Wctu&C%&
	AB"Uo$M/H^Re?(G-4[="\ap9W+3_>Tf;7[QI?%AdrM&/db[ZGHhc=SE.Pdjj-SX_l<S[Zu7j>gtOS/a''
	KHeY3PM0RqZQl^qP<f=e<@"S*#KI;P''o,B`#jBMi7cf`>)V''FGXC[Tf>8BfgEDX["6?".:&>^l%Hd!
	`b^I(#]UOfBO*f!_qBbj[md`>Fo<gn@Cj[W2e*MN]-$_87tjZ+MJ+.?NI.\AFB!>ODHmB[uRhe#_8Q5Z
	PPC6A7.)O2RO6L(NM''=N$>K=agt>((?mGc\U$-:*(<*>;KHIH14,>r''PW2ETL1iRi[O2%bS[9H42:p
	J5&N#84+G"M^;"gVA$5*)\Qmut`&FN48@&q:Qjb043k#<p2@C]p;,%K2AKmp4f#l^P`$C"Z05geLqp%Y
	*W>q"A(BnOg[F2c];f*9pqgo*;&pqQKr9F,p[G+n+Xi05mO1a@3>4''U1h?O-O)W>f@'';niIec[$ZJ$
	dM:AQgJikVai%4cGrd*dd3tCsEm5F6oG93j''E1ptF81RmA2Z;opYC(2q3!rU\A"_<9:r"1LG@G'']8I
	d/5>*<e]r\=e''gqAdi,i:btTdm''?8"''ViJrO,.6Xs"DFO,moon,&d*-$5>mq(k_R=e1E(0c,eYK="
	iEZODD>qh:oF8?2)r0:iF0a\,YFc#P1VrI/%YKDZ&?4Amq''''$X?<#eQFV:==cBX:fM&@c-AZ7b>?`7
	bS&noh<L;5\V)`InOrd7Z.oo3*t:B\d=@sj/XO?\''2\\.m4-e?A>):QkV+f7`6%;QLMnUMg^p_Q?cm^
	L9Pe[I:3/>dUT"u(";1D`r\r$BWYJs5#"teEI9_cp!*>HOl2P&n*^2PLdF0%h(p1e3Tb5Gt5ZMj+Zruu
	-Ej%`cFO61bCDaD?c=(''kXU=%+m\jKmIUAL1\,Z39H?fD;XkN8(Hau$%->"r*XF-MoHUUff.k#Q[K*f
	Da<&eBW1_#^''V8]#@ltC+6;>5uQrhJAN1No\/UHeSl7]<eT2%<0hU%d*eJ5TT=@=C$]:t]JrLs"Ul4n
	mC9T%.QB<V*4''&I//b=''&^1eQs)=b8fU:NhHQbm9t&m#iGrd6nmUA`cZpPS#0nYpPOPB%:_00#lL,k
	oPr[g40G.^<)BPo0.$;"X`1JnD9I8h?`)em2ch_0K02K04_,L>&#KPD$rjqBR2WlLq-=,PSM!,pi!1)G
	U@[)R^<+G.+\*F''KO''Oc=BG;\Chk]+G_^dik#S]9bTh6,#OH<o5q/2c\F,s9b!(uiRmnN6XQf%W@II
	2!$msed0F7TF4m4<@[$nI4Ru?LUGdEoAb;9rBqPhQ4jaR=A+jE/:EXW]HAFBK;\LjU$;*PsmccfM2L-]
	<MD+YdDek=g1JNqQ]:B`s_c+.(5pQgJ''["DrUEFHNBh*2GGKf34k("q*9c3"L(0gHehJ'']8%V&_uLg
	gt%7ZVY=32J)?jJe9=83=o81?m\*a/d-&U*D]*o[rlY=]Sh!S9<Z(`I,*kJS+oT"%bpg8N[=`#^>F%\A
	X@b?:#-i5m3ikXOeDX9#"[Js=WZ5,m3Fcmdn,A:E))4ujS_5e/VULRGoaoX10g!;gPs!eYK!f7h=b2,@
	k^,._=04[Or\^Xi;@8g7<!)2[ZCSK\o4`$_K1n&m''md%ZX02O,hK6jo:tYNRspiubcg''Q@@32!XFfY
	?:5]#gnJe/7!SIe*dBIk;mt\gq9nD(O:I#&3Al++AMUlb$/]f9KY2[pcRnr%q?<+`cYK;Q#9]*C?L?KK
	EJ(r^[TI#3Jf>rLYd)!dC*PpOpDEuh[r/&6-"$6?8c3,h3]+JcHfns=(esg$1Za?<N;\FhM7aH*;pnp$
	6@VN+s@;QsFA;A6PJ2:fXP[O^o"<m8%aNlj+.eLC8\oa6T8@Gu9oAqSdikJ-,FZ^l(p7F:&G`Y2D.=p#
	]!Q)R1f=fc!^+cuWZ4QeXiOb_W''s`:Uo%rFWFM$6HK]-bod,_4-ln#cKX.",&go<^"pA]9=7;kQ@`b_
	W9!uj3YpX?I.8q\bO^bVeCSm%33FV''\k3,;):Cgu?XH#]E/^ModXr>)$T8*!mi48N?FE":[s$Y41>+0
	BZlYpqoj":I*74o*K]G?Y@N`\ZGG&h2Zhpp3f''DD.j?S/o!*2t91<:H,cANauc,OP`XT^I%Vuq9_/?>
	O0ZG/dd[2UM5>hh/jWf*rrYA?Wj5KUK&nQXS2Vl*gs`eeYY"*O0''4I,Q%*4SWTo3]"_A@<o,>WXLglC
	YGg^R,-pg8TqQg,Ns9jb@:)BQIsjMQ"ET\D/I\8g4k\-0=l#(P[-kS.DYnR^G>HUUFAf:RQ)KNNY^qd+
	A;(8_8p2QD:a$lZ2Z1#<5JeIF`XGgG@_>c=8KOk10bRjqk@dus8g+F?c@Z!_bT#K!!Q:q_`+3T_\8Ig2
	fhgGMNNka"c,BCP?,Xh5`!Jc#0jsFXX3DQq0oGOs/Q>gpN*3(aQ=\:CeE6J>F5PZC^)5%\0lSNDITU):
	U;JH@E9]o\)HIEap0Ln7gT?MA<n$B^+QOT<qa.V+d%ck;/AaDL-MIiIh_8!NXr<V0EndAYmPG@r\hC@J
	!L&oE9G_J*<`?n429+s:eRj#jM[Fc.q\Y+jA_OiMH`qCCh8"A+XieQ(158-,AQPA4=73%PeY[oN@K%"A
	HUW^t.=nT?-fk?OR"Cfl7lf8h?bF!1rc3"+m.QLTs)RYkYcE,i:sG2V5!KrUQ5K)@Y0i%1&95o\pJ&j;
	nq?2ZAOo$oG4^N"s%=K=9OA:Oe@gH#[=rjU4]b!m?_C,CHLp:Z]:4"lqct`-L1_R;rV*Mn\QaRFUD`sa
	2]ltpIB*2X\J,e(=9I_n3q-J''`qEXHZ721_6$1;bK_^I/Sn'':hP7JL''&mXqE"<J\<M-]i_<f)l&rT
	L$[5f$#N=dLZF1b3e9UT]4KXXQmN,E`''j)J6Sl&.cbgSE[nTjqGpgJ!F@r^*LJX%k_AT*jY^fGQ,gWs
	6\P_qUtcdJ,[(k9U:E2MdYsQ02s1bPnd^/n>RQ.<2.8JpfHs''.i`+`h9$R,Ds\9R<$_b^XG(oGTV>"-
	Adk-HHDsS[^+kGTooW=t?5""cJno*<_cUF0DhN@#`rl+AS"r7SUm9I^O2#Mmo[sfr;PZ0=K5t&@+Ic9(
	b^lEpH]INF/d-;%,/Dqb:U!@i6.U09#jPG;ck.O".qT4J^"O;M<Wo]&ZE52s=Wt%<OJ,KkcbWH59([s.
	SP9=*:%Ie+rpJhj5:.7s`=LaqYb/W-r[.pLYO87=L$W>!]98_N[i1(S>n"%"D*;(eCFg$la:HcGc&gWF
	#7oI:]''BS3fT.$MBt%:OP0Q,*[OT.hg6=9/hEj=)YSS!b;sKm!oj-<=9[IT0U>5Gj>M7Er(7,I^:&Sa
	?`ISeWR''?q5_P4VC-B95]l?ZS?l^3%_:U0G-#_JY*9ka_F*2pKS_5K4@$FBiD-4#&-U9]"5%#hlGp%e
	3Y>FoLg&AZGl=pcrW=:Zfma*#isKF*X:<D''n)GhluYaDU`11*sgi:ZfC6aRgg`;''"=\>^%uYf*qah4
	(AW90O\QOn1sS*_^3*g<QMLSq1+$1a-I6S2>FA''dsW>3h$(0;2ZWd7LMXa%rZA]L+of&3oj[r_]Wk>R
	aoq%Xg**HU\p):j7O(S#65V%qr+t,(-(r2nmk9t!H@Lha@n9YZq"af*+5U8UpK#VurWbX+.\/6-A2i6G
	fZ''_sXQF1X+(Ls6CL:Zq11neYq?''Z:Pies3[bk@hp"J9WU,?f''9t1B+Y]fBmaNaM:Z,V:OrSN*^k*
	5&:+0un:7K?!8Y]r"TeL5E3%?NPB,:iL\IpbmGFZ''18+RW2NgtT&/pDpm1^(5SC=/%?Ba1gbu]`3Q_3
	.LMlZ&J!L"hf8KCFGY*4_`HD7)Qi3h%6<`m-qS::LqpSiI=re>^M:bgqa-okmm38nO\+h!Xu7(=o0G9b
	PgW=IT5T(E2''L''7jgtXfn$YNSDi=]?)ZI.5kieU16[((5X5osaXV.WVD`#OJ9l]S&aVeq;kdSaQ8''
	B?=U`Z*.;jq$]!VWuV.:k`?`OO!KN=Ti"oLielr97dYgc7p+pkRtUM6(f;5puR;,a"tadVKPX[;n"+FN
	hW,@OXVU2Qe=`Lk!FrF,_f_OfZZX[Db,A6ejjX&mdjD;bXf0FALk1f1"<*3GbWAXi''R=Y)U&ZUkMNia
	L`^;,DhH7Kio]%8IXAYf-ULb!:i=G4n17E]OI"5-+Pi:s$c\dAX''P7.CsrNiWAk.2ru`8MSL-9%uZ-d
	9>>FGhjC/D]aKj-$ekaI*c%j2I2T-1u*iQH.C2''4r.e)J9_oa`)BIq$>j@PrIQll0HJKQ%Lm&IHi_"d
	,FG?@HbUFOqP"id(?,@IouLt15WY''TKp(drI"+Z6&A]7BFc0]\>t8OI7^CRWWl&RXEP79oqn,\iWiG7
	Hl\uJ%\^EXE"X:gB:GMIPP4/`lKN_k+dPs9r"&4LMl3O$q<e''<2@9OhmK,a0f.KM"2"bjacA^!J''Lp
	4^kNril8##nL-5S<<C>**],cgs9$_jiN)J\n%f?9;rOYg%tOTW.^)o)rU9=4_:AW\h)*<=N^/+Qt#2HQ
	?9A!Qret`+,J?^[$?-G`^k)K-%o58*A''%*EIguC%BNa+,r!K!(CrM/c`Hd:ghHqH-''364_pDUgFB-P
	Z<;3[cC&u\$[0cD^''^3YPm^''"T]I>iGA,6?AqJJ&''c?%EG..^uhfq@R-*dLkfne-@ifAhM#H3).15
	q4<dB+tc;2=E,91F+%!dd-p^H.hOp2=0''58)7*?X1XPMd&@bVW:bN5u_DtVO2S$=W.r2R`3,ec2W:*^
	Rq9$b#JM,m$bpdOTp.(&Fi$-DF2$''M=h,^<)IaAF.04ChXWNII''u5.0D"9On)5]UD#&pHK_;0`\Qj.
	r*bX*c_^#tC3''roA1"p2P@4A_PBYHF(6kF2$Zbbrsi\c`M!R<2Y?%Zmu+=@.iam#U=iAS:W0?DO*mH6
	Y''EUI+o_pA#?*r1s4P(rcr3B;"*fAqpAI/_9dkch3n3:@tR''k/RqGPrQcIOhG19<s1ph&o0/f8m@1K
	?90d-l1<q\ts%#RoMtJs7,"9-JkQ_;>a3o[GS9b3[T,VT4P49`:fe8Y#;Pc&;ks=]a:VU&H(.G7br3.]
	bb6&.ZlW3I"jL9-kNK#d=%uh^7kTf&@)Rq*%5EWq*lUq`rc.GQ.">4j&:,*PIMqYhFNs7T4Zb''jB%[8
	C"''aZoG,lh8ml0Q"&_(cX<@ZS''4`q^<,TR$<>)I6W?I5d>]1CC;u2n-?7k@3G!\_P\sJ?.P:WamKK%
	nZp4fX%A68:O.5:?Y+ks\[f_[-dGhkiK2GuJ#4,=#oaa+ZW=/fY\F^%uj&33R;Z$U>R.5it.Vd<RJO=X
	1gZI<QS*cPF;]"C''KYA.JPHS=SGe7p6E-f+Y)]ak\V5K^d)F7a.i(`c-@72R?t$5gZ_TOCs#P+q%K\]
	XW[<MtW5,&NC_iOA[ULjYW/nbZne2S8l,/s!=T@ZIUk&aduZ54g__Ghng[)+4t1<\Wudd!a.qXm8K0n6
	o0B>[`1aXdkROq4g,!)a5.3c)Yr,.8A`I9<L4;<BF>XL!7"%k&E";8[05XrVbB@Z#q"rBb1H*!Xjl?>I
	Pc$C:mUrA?8IN4^''h@_oXaa"7::3-p)aejR:jFr[Q[7G[IlY4,WcuhX\X.9WlZ$+4q,8_=>.P+8i0u*
	j+P$IKc%BdG-7qekHD+WtIHV5tqeCeSZbS.G`SJ>Qbf2MdBRgPAN@I10N@d/_?kc>c!n>U$%O;IWk*1A
	''bM(b/=c9dRV89]!&*6Te0a\(d&,''W?L3o\Wip7K"S>1_e%qL"_q@h<@_gH$>5*l@+\*c]Qmenjl[C
	UaUZRsW&CJ,G2I*$2M&W6AW0bKm8kM$^fC8\\oQb0B^/nYJ76n%UClX>Zui''Z9%kEd$Xf?P$p4:E=(I
	SnNj,+6TI"X:3s6Cc(lF?T1-''M9AJ$(f,%q$Har3LGZ>&&SMLIUD%ZS:-6:;k34W5bFG`IOB^''aW:(
	/HZ>(m`0?iKeH+dju]gK2XVU7=\D9J]%Vd&a[U/\8Hs%$L;"E-$4M]<OSpUat`@_lKG[Ake7Iu#RoXh:
	_Dr(%,D_u@6!&o@+J<<8-T<qWS="Ro3.9U;506C$9>>SV/bHEJ2=WO!+m??pP#Fm40gJA8iH.pgt]R<e
	8Lu7p/mf3i1E3IH_#Ql:mS.a&%\2JW%n]L%MWO`m''rdWFT\m1@C"[eb>Nd]FPi&T0*#F3SbN?W\gcu>
	$O\eCY3>;ZNYu=\kdU(WO5I[b38WQ-\D#iTEpI5<(Lk/ZiPSCWgW1_QIPg3?&["j];!b`M+;QB.JLDZu
	\gUsCN9&P[>pTIc">O=Wo%''3I%>m54L^+.<Jc]_s%Qr*I\&8\+rPE3%Vn;VA=&*=U_9+?aJ!8_>4)MD
	lQ0]3r0-KC@]\BGEb7nt>.g$)PjOU53"jE?@JC;hN+2Rg$aDkd(;k!;MO3Y=1%:C$:.*dEe942_J4N52
	6a`H[q3,VE7&6+pMR2W"ZQE#:s=K/V5o6\ht[ljX&8A&%''r!-%4V_Gt5ZDM)`1PT;lWKt+m+"OTi_]$
	C\Q#:r59`Z9!!sD2d+&*s_7O.''Z#Z5[H^!XWBAUd\dlS#d8Yot;[8W@*\U=14i,$>=!*l+![`Oh$Zo9
	m@9O7nuXJta&09SfE8Fup&s?GY93ZLP@licNf8Ym.+q$bE4S!7k09d1&^D1j$F`OR).Kjd,)7*)4O:g/
	*[KH!hM*YUV5NSF(Ill8#h4PP%k\\;MOmjT\l-ejZCBBiQGANWm78QHZdbIDYB.^](eA"5?s4UMJE\*)
	LAY-64R5UeRMd%,.FYqg?F''\`F\[c+<E]Me:q/Utddo<$F]\%JB0La7lPA,CIAiBtsWBr-mL6CZaNV0
	m)ktrFh)HC.''*4B"^Zf=5<M)DN[Erjm(-HJ_f@P0f''O)bIUesMS92P\6K!g<D9[BU@@V"X;fuPf`hh
	+%4^&!d_cILe,3,(Q=mE-JnBoENL2GD;_,$*Waob4+"uNC]Y6N(HI=JCi),iE:\;?8n/lnNYBbK&N!*S
	C$$O91_PE)(dq6s96WTT_%XU+-A2/@47+!-2m[d$oe2T[eGk:o^E^!Y"X$3c=A[_mld!KhMh17c@DIU=
	sJj.pmml<Wbm^r%"T*+!)s"OGEQMC.0Y?g#V8L#elT7NSL\":?m?]*?)PX3pGdP+UpY^@A+ZUID];H>t
	(hEe\)eX=?>jjr>(T#VYgd;7j%m2CpiU9?L$o,.oK:s]mrW3SETn!-XA<*9_Mb&DABp1tZR"_N=;X$EW
	_U+/KJiCTeFrN!LsVj/ra*X8PBpu`pl>Z08tUYWh?&qoIi''(cWirD-W$h@gjeNKk?*69Q(V2NZ9$Kq5
	)Lld690WnWCg+X2r?07K=2)ic%M\uWXNet3OulHmL<I>iVLe@Y^''VOJm/jCU0[UVE85=8ha_O#.r[hQ
	Z''*I87Wl''SADNR<<r1])N(Qpj)?jc,Z5"1E$KZq@m*.MHm5[&Qsb8Cun0>?snkmc&,e''bXs2`=Fu,
	.A`+q"g[_\b$qr41KZU_lIS''Nq:geqgO3&+I?`QLN*4.D''NEJ1prJKkgKA-CV%]]Iir,W^fbM$)R!J
	$D3)?ka(!cf,VN"2RH_<QT(DdtKt''4QN-$knN&.n1*F,@k^Y[41p;a!B\Cpu^k,?Z''#Pjo+G1r%BIU
	`1Mn3p:!?9%69^W)Ju^69gI#-<g\Yr08C(bk%>)W7XQ7W8I#Q3#*A065Gtl''"sMW8;mV3]?W:5''17V
	.857''dG0_D]QcaDg-!R;OZBl*)f,&8l?]Pau)<2%1bS;Okh-qdoPScp\.=''6Y:3XF@rTVD''4/fW%$
	JibCDHAp?J1KL5"4a*k/=6IGg@c47lmH:B2#>O+",$G6a1#rmhDiMj@7L0&H#nMVRmQ)m5%VZjWUQqt9
	J.ggunD`2MEYObWXF<ep3''AXc0=Wnfl@Ka:6>)-''''@/bQi3]*:M*IeD4J]fo9kA:>,Fj%6;6''"Y6
	:?\jDGoJ&pCj0/V4ZKV#Ed]^+`TkW<V)if1%Zh%]?Y4V#bHp1)&n8">.I_J58q>j@(^CI;;3kL6snAf[
	bH[MJKWY:krL8*X:,r8D&X(''b\-BrD+c<3TG_8okE]qKKu(sV@SV/[)G%9G`5Hm:ref#>8C7`U3TD<j
	-##/.A07:`m9sYN+=2^9AZ%SlS*I4aPYFa:9FVr+8GsIsITr,m3"(W%oQ@UqqiWo"/@^MB_p`=c8+"@''
	m-&"O3F+":aHh1I0SA-fCFh40;.>lo`e@7>`rJ#%ijL/5+2dHs?i2o<*ZEgfE03cn)?ELc?uF!(7]4K^
	_!a`T_o-[JkZ99!)EORS79o"GbKX[Q"O&OA-.sg%bK`(nSBUOEJ";a]PjO<`4do^%$@rYU%#DcdWR\aq
	c#*qg#I>O+bAf:I2Qt#0hL=MJ,Rfro9Er^V8Ga5k!`$TW,S/jc=?5r!5nrOM$Zi+m1b''a+!7iaA9i."
	3e*3qt+o>i4Oajg]D75iIDobd]Sfa+HpdVEeD7c.*-372[?rAkPSM5=!L$g3(R0!tXb&Da7JP=D0AbP$
	P3@3Za6O@#`*FmkdZ1=EI7Z\$#etCf`rh2BURf_5%g-](HR2QNj*,ABJ7[\(@Jb:IgmZ;:D1Al<<6:eu
	=Z<KM6T6#lRMCUn^Q:H]VRjGsE9%a+Ga([<U%.+-i=rpFn:YQ*R1O[RX`GeNKK=RQkhN?Ft4XBJ/gC8:
	VZ_!n*"Nj7;FC.un#Lt:moqNBT1D4?jB(''scpjR?;_F$o78eRIqeadtF1D,K]:W_q)?Ub''X$qJ>&f]
	^j2;6j'':+HkUL=,rlHc%&.jFD*e28#?`?K[$R=%"P4(`%qlsrTHO]@I0&`.\M?-Z^kfhCpn.A.s%?+f
	_a@YbO^%t>@^.]7]s$KVg.(I)rC1m/M!q&`n"H62Ar:N/jgkN"Ya.[0)q"kL<S<6m$sHK@7+6:@bob0a
	2Tt=<fT`2XA?,`#0%9+,tI:K`?9q>Tb=>#h=<10>@1o=-FA`RQHdj:b_edYRJHhp6Kf_09/62C^F;UG2
	O#HRTnpB$E:RG4lKpaGl^uQO!P3T/";@ZqDH>UI^*fQc][s)i@tqq?ZGM"oQE`p&hLc)t@>.Tb-)V\Q5
	$+qTkKa2oab*t9J"+]d8X9RQaQkmZI-&iM7rfLDm7$q1059F4C]N^[`%uA_Z7RATZAWBG*&2=oELX3[C
	`mjHCe2kS"-)E9_E''lHH;Hu+k%GSFB[sf]*#t>g;leWRn1OTkE4q)C>JDSpqh2%1P?sdrhR,%s`[!(g
	6W*Yo'';mD&g%M"/$-Ldh7V,.OAj3rT>/`s"i?r@rVIL/9$T:>sb[M(+1)JXL,MKX+>mRVQ32h_>Qn_Q
	L"]g9m]R)FD$;TX9;H+%=g6'';Ej3?2X++DARopZELPlC5-,;3s1P?A#_12=`.c5PHNb0"-E-[V<YA)#
	Yul!^@G''inel"b8>c2MBdD@UoFe[9;_,dMO<3C+V[sOu#XkZ[f)V8[5+''cig2<DWV]5<Xeeim)6*[(
	9Ap42Clr_GKu"OaI]`+V+(GN*((Q,Y;WhS9YVdh]C!6t;Q?RI%!&4B[9;k0;Y?fajgENh6@5n*]OPP)a
	76XO$r=sf>inop"s+i15`?ebMoe*thS5]mdU1?\[i?-kQ]9Z?@^-`e6PRf_MC4!uY@-l7KC:le,oZQIq
	JX*hUml[3qlKF,Y]'']]J-8FtGbH3_[BQR+^T@roL;7G7N>>_r)!-qWNPQLM6"@Kh*JN^k0Y>nf[Qoj"
	+@s4,N60Jj3q3=J>D<e2eGh''q7qp"[fnQ69Z43O"L+l!7I=)?!jXo_f!N1ZP(^8NP<f%b-%LA(Cn425
	FC+u(`Tf[1\#W@gL8/B#t49.mE#RAd/)I7i()nX%JXbN)7=VOsuAk5QO0Y?_@dZ,kGd0RZN64!M2R)l4
	U5Tmh5GIItr1LZi2''a9k]8_n4,14nl/Iq]kVXCWn''k)!7?!S[lt54/FHULD7J$KgP]+I;%[G51CN>U
	72(,UN=3!!*s`''$Kc#)ENPKn?G71s2StqD,AueLs:#GJNlIIi@Tq)N._E)\o#]/:n"4"Vo";47,U8\X
	[_"u*#45k0?d*PY0BM,KNY!l"$&r\5.1@:-9&BN)7(iX,/!?lRS]@d4eY2iqm.gj&FN(W-]Y#S7h#''n
	c%F_(kZ`\iAgssC(o]U&cq=S.m+=H=(YCC4N#ICUS);c9%U;/N2)RX!/)ZKV,>>1TN<WgBZMVj-_a"fX
	k&J[K"D=NJ>R,Y4G^p2P;$3lD%O*$t3%&q48^<t[%uBW]&blT2E\&J4PEKDpF;u:\*s<"09,9j.]<SB\
	Jr9%?o1E3+3e&tEEk$`dkl=VQQF_2LFtC5/.`+[kapeM$:e*XoJ@8lY%cH+E$kkc0kXbYc7m);#_1AaI
	M_/GMg8q9;A-P>P,"rui4G1mhC/AR])abpg!MWFZM!q.CkHfR]01A`)gOfYZfgQ+4>pFOapZj@_PF4''
	Zp"f3%0E*Erd\m4Ze:rdt;E''I>/qf_Bh\NA$aYR)6OY>*CM@^%=*JlI9Lp1oZ#qO37eK9)YMuPe!VLk
	JCg;:`5''9H8L[E1=@Q)C`d;SL<0q!''Ipfd2%\^6sH7;X,Y;r^4-^VSQ`Nm.duEf655dcXkd?j<Iq9h
	@9CE3YZO`*,)!oBsW;p\(^6CpA!d95CT$g]m`sX;b^<)S.4HY7Lmqp(Q3mJr<8J-luJrI]rSd;jHnG5(
	+kCl)C"\2\lTH@7^I=H5ec''NkB%#4aCm@p%]!U0Ts<&0h5+b($-"&`k`</\T''_UQGNjBZ!\n1!&i6W
	/i!u@S!S4##EhM1dWfei.O5<`LH?S''#e^G@dh`[7>EC2Hj)T*h@gY]J+5WO<kXF41Ckc\8?n(rc58''
	(@<FgsNg4*&MO\a5mS*NiGpK^:a130DJRQG7D''HC*uAafJVQ>]o5WrAtfrPf^M)-1(X3EDTh)~>')
%
classmethod: R4PReport
imageLayoutspacingGif
	^(ByteArray fromPackedString: 'Q4%FNC%!^@H5@_\@@@PBAHRB_LKB/C1BQM#"7I2")FQ"WA0"IK3J7N72;FQ*_G2N''J261MO"=@0RCL+V7B(2ML/R5J*2-D1RUIFR#K+B3L''V;N["7GQ2[N#:?@0VIBP&MFU6_G.B#I.&-LSK3N[*=@PFEA\ZEM[V7D1VYE)"[IJZ''GQ>#B *INK2?G16_DIJUHRJ#C0:MKJ:/N/*9M/*=D=ZW@$JA@(RGFQ*[J2*+A,"KLSR7L3J3D1JQH*V''J2>5M3^7MK";GQ>_KR>1LSB3J".-H2J#OS2=MO&?L3^9KR2-EQRUL?^;O+:=OS2?L/J1JR")APRCLSV9CP2MM3Z5NS";B\.MF%6!I2^+CP:QN3*;@,JCF1,[H*J!DQCQBP"GF-2_MSR3I2^%K3D5G!:]HVF#JR''-NS-?@$NEA(\GM[Z9I2\)G1>_K2<-L7R7L7N5O3>?@PDCLSD1F1*YL;N7HRR)M3%=IRT%A,^IK3J5E-^YFQ&]N35?G2N%M+,?E1ZUAPUGBP&KH"V)M3*;OS.;K:>1HNB!E%"YJ2:3GM4]APZKDQNWC0>ODIJSM_^9B .O@PNGC%ESM/"9I.#+LSN9@0REB04OJ*4/D-SWNS%9G2F%I2&/B,,KFQ+]H2N%KR6/EQTWJR&+APUECP6OF1.]DUFSBP#IMSS5K2</E1\WO36;F1._H2Q''K2:1MSZ;A0ZIGQ:_OS.=FQ&[I2Z''D1NUKR.+M3$;O36=L3X9GQ:!APNCBP^GL3F1I2V%LSL5HRK%H2F!FQ"YG16]DP>OD1LSLGR9GM>!KN>3JR./MO^=F12!G1>!L3R9EQ^[IRV''K2>5M3&?CP.MAPNEIRV)NS^9@PFAA0&ILKN7OS6;FQ._G2R''J::1MS&=L3Z7L3V5K3F3L/Z;APZIB\*MG2F#I2*-N3.=@TJEM3Z7D=ZYE1&[IR^''NO6?G1:_DQNUM3.=@0NA@0VGJ2.+A$&KL3N3MS&;L[F3H2N#OS6=KR6-EUVUO3>=OW6?JR&)LSZ9CP6MN[&;F1:!C(>QN3.;@0NCBG9A@@@@@@@K@@@@@A8@#TAA0#?@H^0F^!/(LFCBAL*WL"0(\NGDBMJ''D"1($VI=-#XR<I&E$JN=#1^GD&2II-;@5F2DW"PH1.WI&OJ''D''SXXJU=5P^3@"R(2$6<0Z>VA"4)-F#RAO^B8(R$L:$TJMJ''U+2Y<Z[GAL$<_&R*%^I=5(YKB+4*=&3DX/BOF%%(AIG[CI49P#2):%Y=.P&GOHT+U>/>5XN/O&7,NGCDPE)YLLCX]B[FQE?WR-8,LFMK.W*%\29Y$D6" &^ANU-2AB9P[\N9L"C0!2V''VL#''UT&F(@+@GK+7,6;->?_0HLKG4:<.OG#2IL+E68K F4@L''QG5?5<._W+6G_[?.\\M0C.3WL??8<V+T76<>#S)7=^)!7BH_/,@F #Y\#ZIO:RIJ 4:KX<!G7II&AE%US"''W(HI*#   0.U14@YO@VG''$TU-# !\K!E,%344U3(GOUTX#!"BP*E4%4V\0#$%"\?DNFI@@\4IY^,F&18W19GCSOORHM:FMDX/D@ B"U@FWZ$T &*^RRSC[)9IG3OB''%%EQVZ^VUVFZ9IBBU(APEF:F4HT8[-.0BU2UCVNFO&&1Z(^V[\E;)31BUNNUHIY?0\Q,@PSCFQ J@>CO''ONFDD6^VTTY9*IHIKN)(A(9 @P@ZA07!2!W"OOHCIVR008Z[J4T9PQ$@5IAGF6R<U)%XO;XJ4Q(@QO<21@VN*B$(&6/&&"../N;*Z:?@?/)+,LPJV>211"ZK;KKJM,/,,;:JMP8[Z*0 ## 5$MBFL6_@-X<?+P0QYZONB"-(.^UB*69A4=8CAQ.?PJ!KJN2TXP8[(;0P*JBEI)C./<-ZH_B:A@M,LKK>FB+HEPF0XX)@0@C 2RS;0EAAO0A8$>!@Q53QQ"\0YILGFV7HH=@=,K&*<%9,PNF@JOLD%$@8+XPC2ABF3 %.@*70;GOOOP<A]L<9D35''NC8O([ORP?0<L=NM3,33''D@3S_PPM1-:<=U[O=55.ES3[OWSXXL--M!%#9736VY__SSVY1/ZM-%,#17.7E+W''SWZNFO?7P( >2#"2)!A*BIKB].Z4<(,-)+V2)+$L+44HGPCX''L8B]M-*K=J@>747F;GK[SU\L=9\=)9-?KBOS10"P$@T."R "J!@DAGKOL@@, ,!B[\N<87''8:334&[K''''_\M>==MMQM74<5YO7GS+W_\.-^^%LY25:6GJ+WS_7SBO=!3<A@@BFXG,@8H0QW3 "Q0=5_@I@G:@BP4X4&2""S!7X<ON]D@_)4\(FNA@+RF0V%QCT/,85)2IA3 *V&1*8FD!AP]F)ZV#213;& RY^HP52 GASI^[!I*UU\HDUSJDJU<#BE++0!SBL8P-Q*DA?7FL_+^BDNCP0BRR@@@922HQ9?5;Q) R:ZQZX49WM&!XNJ;1 ZXH"5;%:E$J3IV@_ \J"3&YF01\&DXY)&(\#\ @@EK!!CIN@0PL&@H@21HHMK6 LMM!P*BF<HG_!8J@I\6VZJ\;,U#T<%;=H6J LS-A2?D*@&''(&*L_ILI@*?NH#I7$.''FE@AHV8BQ$=<P!G4JHGX7!@NGP!BSKPXP"H.DH40"@KVQP!@058 CAT2X.A]H&@.O0T@O#@0\4937$/RMN\;.FO^_#LY)E;GN%HNH,9LR8I)/$TGE]BN^B-SV_>@HP#(O''KQ/7RMM8LI3#G6T52^/N[9$1''N[=93''X&RY3N\2\95=$<]\HS'',93!C>*0O>GM(!AC 6 QPNPH@-X$HDOY5BLPN#D!#+!SEFT>=XPY+FTI#Z*!BV\Q3FU1$V]3TN#00,WN$\*37NR%I3#@@POLEDFZZ3ADS6@@R5 DH=D6NHJEO!T$]*RQL0%PJO!BFX_K2^<NY$&TMZ4G@R-8D",HP5)P_N&O?:()''WN4978-J)VS[)U]ZJ3"XVH!"AL,P\YQ@LQVMAENEK@CV34PA!S I@8O@F@LDQ@E#68@PH2 HPZ1J,LX\!%K.=1 V"THX2V@<S#KM^J- !,SS0XZ,J2*[Q@6V2JZM(G&!3A)U$8 $:-LBH6,3$''M9FP!I7CWL7@][%,W.91+XC-Z5''KVGC?OX9&D53,Y_=FL<[VM+Z6CR9/KP/[P>+6-;@MUL4TB2;%6&294L4&]I<;6=UF=7G7:LU<.O@EICQ CMO@ 28*4H3\2FM_U(BK+]BT0D+$TV&ZGYR!!A+LWHU#!I;=UFMMX0UE7"*1ST/.AGN[LNT6<)BW''V48F*(E60#!G]&PP3;N<\$ZQD D=I EC>X("3(.T&]U)^CT[@T6Q3[N&A+U7S;4I[''DC&1''8#I.\0%,L=<VJ+DS+OE.B93X1B;61<IU+(6G*=-F3XLU@ACDAY0C BY\HQL0*@L7$C@L\\P*M>[ISRA( X4L;@@\[X"D[ RPL,F:B!*F-VO/>GV.^SPF?47TAJQ=[PUHMWF0$BR\$2K5BL#K+&$H#&A\IZ2PY=IB4+VTYB@M/2!IR&I.4^]*M@L%OU$VT%*EW_PGN''H##!&HX1+X&CHR]-FFM'' "3NNXT4LAKR;6J,*B?!JTD__U6MJR4D65$+VZJ.GH\;T)4%L<%6VC3VY>AV)_ A!GJ[9C53H(0 %DPHHSL F@<@B@H6''BWA;;^=<0+&''PR+NBIN'',Z5-Y(XMV2D@1G55,/=5*4LN^-K@+OV<617.2=6X4,B-8:W7;@0PYXLTUJIVEK@ "DP@P!"HZ(@],S@L@7]!EK62P@270 P1''$HT^''HBMAP0IDY08P(CL?JM1QNLV>?=-W-QZ<Q47>^MNXJM,H1-5SZW]0>VDE!SJEDO(O%)P(1+TLB%VL(,W!N-3RO^\4)M>SJX/?^%N#3+T)2;5*%M]:ZP@!#;:HRY.<H> GV##DSA1AE:S4HEV&HWRQ+!GIO69RO.0P!KP)NFN#BL(%YB"(JH$T*%YDH+OB?3J&>:9_^@CDY#8!V62\P8KX@L[9Q@GI%+P@ 3L*!T;Z"I*?SEB+S6V3$#"7H@[1?%[I<G%A]:1,B>''<''":_/B^B33 Y0>55=.>=Z57T2D\\K:NILDW[^#@MG+P@F9,(06^N,%@LCZIS1J!FGE(P35HK%"S%>G/-4U$J613V(DT48JUAS+?ZYWI^S,R^!=4<*0^D9 &MYF+KTL@Q#Q8TB-5S?Z6NT9=X%-[7O8SNO-DEXCZ)7<B&G+9%50B&H@9-''<KFHC?Y8@@2H@I&@SSH -R4@ZD0E]HP@/<0@\*0E&_URS#4A[ZMB\/0EP/54\+I"9@DQ!+0 X[EE''''M%G"8''I=-D\G9%S;-8MD9W?C=U39]1N5L S?\@UE @QDP@QN,@H%$2XWLB6#HC@7930!)BK_L!C&]6/U]BP0M#UP<''%(X''.C5$#3$@SDAH@C9$"*)64C:H@Q2G<O:HO758@9)&:E@@C(<AL\(PIW8@(M<G!D,@7"4@]XNAAZ@@@R<@AHT@1D$@_"?3A=@<D*5N\*80@@H)@39&H[)<]9_0Y>E/Q"0)X&''5HP72\. ]D%I/Q+OULIX(ZE#9U(,H!),S"K$9Q)K3QG>8@G7R@N!G@C6J@G>T@BYG@LA''E.4%PI#(A]OI\;X<"EC/T7PPL8CPTW#)ARK \(CZP+L4]I.GIB  HMPA\N3/E,9?BG2P@@2 @H]6DJ+X@R>?AASIL;]EIL4BQQ9MY+Q/UGWJH+=-D9/BT6N2I>91X(34IC9LZMA>L+?(@GU.@C@I@EA<DK@L@AC9@B0T@D07@E7X@GJ\DF''=@FM^AC7J@GG%\JA3E2$1 [%W!XF"UN9NH\T%!B"-Q_H^P&+6!T]O>2G0OCNHPVSL\FJBK5VA?%BJ)$AQ^POT!)OKR7%KKW%D-CMD27M$VEOUC%MEVYM8@7%T>I]EKY%G.C"^HVC-40@5<@#M"@AA0@@B)PBS1@@EG"BHD1B+[D^Z23TUF2I(''5IW$GZJ/F@=L2AH4!\0OCME3XNUUW%UDUXEX)MH1ASOOPA-D B4''H\^SPA,+@%/ RVW=6C17$E@U2]''G7\72WJ2GTR@PIJ$J#PX@@CQPTP%12  HCS$ 3S"9I&3TWNT3I%OWT^*97^8W2A8J@!:/AA(KPA%N@AC5@A@70AV7PCP[ADX#0LT"@CQZ@AGD@@K6 _B&)L".)L0%&XK$1UZS?&R[8XBNF!PTIL@+? @##LB4<L@(R@1)V(@5X<C*" @T!ZA;>0%%41SMWLA4@<B%R@9UP^S(>(9WO47PF2$ IB$T>8:@EJ#$G2%F^T5QR,:@WR*E>):E[EJESQTT\-ZAJX2 G@@BS0GE''2P?"0@(4*CQZ8@VJXA)EH!Z<L@A*<"63 ERB0 [S(%$C,P<78VY''@ C,]TQ0(SP#>B%7JZEWBR T2&ZO<4\9@2 I\B\_MPR8\PL=T@3^QP%W(@L$MAQOY@RI\C<@(@V$8@"AU$C"L <''VB^\AZQ*%0FL(4 +LQQW8@XE9B]))2 (<U&,8 !C(P 8)%(G)%.Y\3+;9:RI?2*!ER(9\5!)"<V@>%] H>*P%FHP10@@5^A]C]@@15\F%<DF!> A1]@C47$*I+&]J0LE[PP5G^(/8U%Y$7VG4FFI@@@L?''BG;%D'' !@M>I%,!5!-.XDI;3@O4Q@IAPHT-0DH<2@^F3HJN(!/#0ZGRXR(!8Q %,V@6E)\MHYHA''Y\$=US1XY?N7"@)LVF%<U\\I"F89)?!#)A;^\O]G@EEZ@G1H\D5]@F(@AAM?LBY?(NOL(%OC@B>!@M3SPO87@ZE2@0C^T''K[MM''2T.P3@B''LL3RYBL=J]*WEJC%TYT5H)H$3L5!*I-6WP3L6LE,''@[1C]P6D@I[Z@C@/DND3TGS??PC W!A.70AGOPE-(@CRK4Y=KTE, (LBD$MIGET@)+UF.R@PJ#BEX0B''C1JP-+-J@!^!+J(JZS^ <JV8@B,+0E)W>GMU!KNT=U- L*MSLCA CPLI+ZA%3 W]OI''L99DE0''A%,:''Q9''A:2:L-Y7N/:WLNFYQD+CA,8@@GNPCTM@B+"!R:I@J;L CYEP@57B-%.P)/P O6! A]17L9T@@@;0VU[PA@CPFH84(Q?*K0?J)H2Z-( )^ *Z-[B+-Q)Z%P Z*80T.5M%.115-! J(Z5KQU[)S^IVA&60CSBP+10P@(T@P[3FA%^@C#6;MHT JX''R,MJT@MAP.@T"DHG&QJO?6D^U$@G#PA!GQT],Q9NNF+0?46:V.$VLJ --XHQ''6PC0(B6/0PL(\PE<(@6@U!AC0@SQ<KB6ED7;TB."ER*%:P"D8ZNA\W]K]PDC8QD*D"T<2 Z-0LCEQZ''8I:<_"&!OV+.L&+U,5%(:I+I@&HX3\6SQ$J$C\P1-4J$MT@0MT@E-HJ*KPZ+>U@22$J(@L@!=295-)E#$,++>DI:S<;5.H@PXPE_Q(@ /(E5F B<7C@V''\P-WD@P/6@9C$ R7XX0F@ C7DB#N4SIGL610:D"HZ,H\_FC.R&O..,D]WJ6H),XFM(@F^J8 V:6E.*79M6C_Z[HH](LG4@X?8E5''NPWQ?7-T?WTEU''A=_EXFTA@M@%LIP"@JU7@EQ8@C3A(K3(@HZVHD+?LD/ @@TPH@M%QXMT@*S\@D''OU70;[FX#- 6[L3!WIHS(PGTUI*@/V)OPB3N%@PE5@$@J@MU''@O4:HXF T@SB@B@J@OMXA@J''HL>)@[VH@CWNHH.T@*V&@D $J$[N@O$X@O-WDESF@E><@C<;CJU>@I.Q@MCX,3V0RO.N,/1SV "TR.Z #HDSR2)+N!?52U<10N[N.6@8DF\N-]1L^\]H@P]2.7CZB*PT2I[YP@T8M?"6T[#FQ"J=UF.''DE/CLJKUTI@ 0LC^TO.AD((KDOD&LEL''@EK:@HO@(]3/?JA+%1I.DL(*$[*4!#(@[Z410*^:!+1K*;/%(K4DGM44Y,5J8;0+O''5G>4I.H6AHU\@T\8TMT C!3PL;N BKL@@NN@AZ? OD2P@>KXMHJ@BC3@@?EPBL@ LO<@CJL0]F90A_S@@4XPC_=0[-F@^T_<AD/@A,+PAN)%NRS444:]5E1;22DZNP$@@.WQ, 7P@8_PA''M0QW=RBWLPCPTPECXYV]E!C(-+5>:R5)3L@?V AX6AC9!@B)W0B)#@UMF0I%?]@-I*C OL@:40C/=@B$O0B*)4\17A+^D*-.5;VR7F5AAZ*O2<X=]:XF^KYB;LA#@,03P\* LQFU+PA!;P@C7?#@59@@A<N=D%Y5"Y0:BT43NP>W^4#P"^ @''5<@*$0A65X TX@@A.T@<@<@(0N@.6TP&''>@;\@Y<A&"Z@8BE78 "6LLAF5EOCI+H[CH\8!([X&&C%Z(CO/VM8#J8]#HCF).E4GJ&HN(C R,^KUVB&X;K5V GD95YS4@ZE(C,IYMM,P@=Z0GMO8@ZULM-CT@Z$4AZ^EP4\!M?SB@1LDDJ/CPC"T+*:9@[B)NR''!RZD(#.F>#\H),H[;Y6GEC;"6@X=4KJ?7@Z(8@=3AF<50@_?$@MBP@+40@[PH@MBX@T#HM#@8AF"LGSG>-U,(@7PKMO>DI"T7E?8;WKEOABHXCI4?6HN*L19;,T9Y(/TY".2YD-TWNS\4J5\RI6:Q>49C%)LRF[P5@67>X(M]N/P?"R7GR_^9E52P61BE&839ME7#VTEX"Z0OO@N.]DX[A@H-%@O^#4JWSHO_V@K=1I(;Q@M&FCS4U@+<8@K#],V7?EY(GF;+I.;AI*!*P/T.X&:H 2<N,7-.0/U5:;SR^44,Z+TAF)TSVR<KM8C[+V/+HC\''5-L''"B-=B@JZRHCPD-BMU@CXA@I-/@-?3@J8LHFY]@X\\\CH&AG*I0&?2@.##@OC6>L1-"(8^:!S1)A6-[X?!AL!EH^"$CC@7THU3@G1[SV_1IYK&@D)[@!/OBX;=IP(?=0B>%UIO!P@2)@J! L@C%ZI3@HB@5O%I27D#0OZD6@PEC@&[[ \*AA1=-*:^5;.$YS5M''.K6C++''.< I%SJFCP0 ]Q7]9E03ZL0=*=0=<]7-&9: N2$+NY%P% F=$$Q[N@F>X0@.?0N)D@A]T8C:UPG*+=TQ\@B-F@@$YPB^70N+9@:0LGA^M@A>S!+B0M@E4@ZF.&1&;,+QE.[B!.>Z)7W@X&*U^^_W<\8''0\2FJK0-'':!!@N W''L1?:"I)"38$" 4OB.N9"785 #C;@0AE"@C,/\DTE B?)0ADKPAPG:+D6S]%]P/%ER"Y<U(AF?:A(F3''Z&>^5*8[&E35(C,"O?F 8 LA>(:,.U+P3>L@,^<^\=)4=C @ZX,NLE8"X<8@%,8@"8@@BXTK7#\@VQ9P''*?5%CW"P@LP=@$"FU+@A L>:^+QEV6KCQ!-B_/77>D+QJ8F>HQ$@I0%7LN@QP*8$XM_(#2SF\O4@+U;YD2WJ%RH< S6:4.YF&RY7>9(DADNC!06M-S/U( @S[%3Y4!#;,54XL$*PM<+P)=US+U*9]/W8EF5[,VKI%-8:KU [P1)P/U;ZIE 7@7K%V=ITADD4FWRZ3?KDY%S]@)RE,Y+''QA2CR7B."Q+D)M_]_MB5W@BR0L&,2 CI,J*652CM42IL O8K.VUL4S-D5S:_F>G*4?>2^-R5>1H Q=^#]*$''''KM5ZNN%VU(YXBU^&3[X^R''-TD5\HDB@+1 DT+EPFB)=7[@SN<2\J79A1[NI]*RRP#YU9>2IUJ#9OV;QYA>\A<-^&>''(@:Z78.<^JE6@#3[ZZV,I/(7@>N"6AAO4I)04@$F* !18(ZTLG@G&8(KC6?**N!2-&@\BQ/6YY(8''L)FFB#^8\"VZ_V\(P(IVG1)&GCPC>F6IG#T#4[I9H(K"''$$)4YNL8-&JB$D&RU(KPI+],2,5A@&UK:\&RHLQ/)[Z6#GIAG0A@8:&"**F*&@X*ZJL3M.19RH,6OB"&NV32@L@N,?[$,4<??>Q3GL9PR$B''#/=0$4,..*I!80T-[ G BQLJ@Z@TG I<Y3D7QA(''@SYHTZF1V0)1YI91+G!CEF\> VJ18="X@1N<#-P2G@Q] *''KWF65MZY[<\.RR59?#P%J-8)ETJX.YUIV6FV[CMYXXI5$M-$'')15)26E] !B<\N @(@H$.LG&NWGP0\ / P"*)A9=CMA)12F"FXHG<OC9!;5(+M#''O6CL<V^VR P)(9HWR!2"Q<2,.8 = >;Q]-(/XRJIIH=ZT!B?&C2BTDH$JK20#S%<V >PRBK HPM;?Z''$BTU$XDH$Q?CA (5=+/CGCSXRDNP_M''#@QA\6\FGC$W,.@N@$@NZ)39EH# LD"4K?Y"%ZBE/V63&6$C**<*Y%*8P2-(&<UOYX$8A#CZSU[+KHH:BDF *MM+" R"&&.-D*J#E:JDZ)A_HDMGCAA2\\KDGKJGQ,[B^*[*H(\VB#ET]8XFN:!NZY90U<2K"%(BD\RY"MB4C7!8^?6H@&@Q8\YM$3J2+1Q1.,J0DI-M+SW-.4@7U/;ZO C#R0]=5-3547@-$F#''___&\0^^L''F HDPLA%+ D=F*"&CP:*H<>>\G H8 )RB"KQ$U$P*]&JN^XZ ,S4$,S%E#_^\TNTD''T,=N@$K=BQ(IW=F 9/4''XR";$$MP>J4HPZTJDFGDI$13GXKNQ1AP&8@S2$>@P)]APM?7*L007QVLO$ EFHR(3"EY"80&NX,HT4,BDL/4CN#!IP"Q IZ@!W$I4#:@FLU82CEE,@0C/Z90!]UV1,JI&V[G3U$=; )$B!"T&0/(P,I/GJH=Q AY''L5@X4MTAM[GHS''M  I3DTP1YJ0]L B+]FM+Y1S6 )@?RTA2WE[PQ+K;#GKFR7#7%T@!*U0@P@9NFO<"QLF:FC7S4@LP([VRDC4K@Y20)S&E&X34R=0*S"!KT%&K!%N-1JE-%Z$TQ_$R6T6NKUQG9EM"85JV3LF%Z1R+&+Z/UJR<4"BR41M!E0_ZDG>T@BD+#PA''SL0!S;^L#RL#NOFA#G_@!11C.^XA$-FO<!F/:0P"SXTC*G]LI>&N#YKD[1-BG\H1*U\HQ''@IBMVRSLB!03(""!9R1[YXP$P0B%1%3R!#YXZHD!T<YZ$.@PJ=P@D^0@@BZ^(DD(WHE @D@DJTC4C$P$EAO8FL!?9,@G@K2AEIPC@D&,X@/LID$"&W%EB2K!BW&48T[[)H''/CNTZA6$II:&<&KQ,BYL(''Z5+0\-IQ<JTQ[ QYV9TPP)S''OHT/]WMJ%!1X5R%N-T[CV)I)32"8=09NSX(X!X"SXJ"LKF^HV@F&1LY11C&8YA=&DH#K? O82BTU.;9!F660]7-R$N[65")+<=S66?@M+3#DZ"I_/TM<X@7F*5)+S_?#_UMNNZ1D''BIB:%S4I9H1J&Q0!AQI1$ SG,<1\]H  \P86!''^5;@A$VN(00DM\8KS)_NQ,I''H1OQZ6:ED2ZTGO@$8Y@KU_;)P@6%46BM. ]\B<NC52U-H09#7@/&8]WOTT4$YZW\DFSKVQM=#''K#&L#O;)DPGG1"!.%LIT?SV22?,!\4_JW)[*I81F@12YTT*4$V23RTL:V)@]-(44ODJB\OMNBL]0H\UQV<8L@]S"^BCT%LD_R*^;PODJB3@#PVT0((!AY6Z,5L2]YZ"P0L2BG)RQ  5.(]_2 7VC1-"7*3^"5V2#NTS]+$+V(9KU''"VH+3WBVO[<%JH9ZR$:$D?;H-FW^\[4T#G$#P@3Z0 ]%B-NHB1%$O=I;[1>L$@THFZR_KW,]Z2!5$C##P!!.4HH"GFLP''U''CD"S?7%2W#>E!M /DQQ[IKT=:&X?5DB%H0MH^K3BHII&HOQ+ALMXPP!"C3JJ-16LN^D\M9E!$8#$Y",5[LIZ00C F@NW 0C!;$X@;&L=#ZZHJ<Z7$-I!/QL[FF)S.NAGX7N?&MN''8R%B52@R%6Z0O^&B*U.&G#[7)"\KJUSQX8 ([F+6:%8:10:@R< CC^(]1:Q+@^$U[X4/-H $D GYM=-J=_?5$+!-$PC!;5U[GO@>+/EN/DV8>F5QBF=;4U2324?P99,$%,X_>!VAG#GJ@M]^ AM8BIVQV08Y#U,PI5J!E&9\J5#3$2D-[X04\(N@HJ,+*EO(S0$ED@:J1+2_QI@-[G^.O5+&O#VC!04!DD0(UBERH.R2YGMSY$H0N3(@8OV,FCZH1"+Z<+RB..Z1C23RLC:8EB0+BI&PR9>PV@PI4G QDWS@!!GD$(V.Q&^U=V,,Y1S.0)X.>-T1''#&H(6!]BX=&/TK''810F>J$5SL"DX@=FKY_0\<U:C@&W-B.MYN5D%=L''A1F9''NL:U"BW".7]4#DR)!B0OOODPJ''.NXE%&&MIZQO:<+,+52''-RJ)YZJ3GX_.5I1*W1%+L5FKRO&L/Q_ /5IHC7?AC*4(PIN DDC,LFOM%P9''_\ATMGV&+C;RI(-;;QB$R+Y/$?/8<K%F\_1A?P*<%Q.+YH%CC9+6R3S)0SVL#?=DQO8:0(M># IFJ ?4/$_QQQ&(@=!34[F''YG]$2Z.R[H+R-.GP-DI34&R-A*B17 ]!=BF/R(P(^*IUCHTJADI''TJ=A8&)I0J*5''@[W(,[(?"5)V"J/BF6*+"J+@"<E@R<04$\6>.$1Q&2YNF,$+B''-F,IV,.R1WH^GWR,5@B88''&"0OJ7&=IA^#N,-G$,/4*[G?0M-E-BIQR JG0]BI$AL) FOX@2IN @@A@F;:"D\),E9.(*+%&^@V*EH3$R[R R?9"B''#X\HBS$PP!;-8/ K_Q+K0VQ$I,KFP4)#S@A#8R8%C!#%''"ZK7&R)5X*/HK8&_T8D-J!''E''H*_%JO=G@,Y9@''"P<#]P#O=I[E$32"G''P+26*.3V9.0F[$3+!N6QS0UU$LC"RH2\<EE*[J]30,=AXBS+$KU<QCT2"FBO+1V39OIZ(OV*QQBV;ITKLIQ]T,!_L)MI;%"D+&5[JOQ^\+54!E-VSQ)YH@,7K@A.X T%0@*U@@#.X@VE8 LKH.D^S+U J1%2*GD]8''0RHL9E8I2T9H SAQ&3\%$8*Q.#)D W1%T;LP0XJ&S$PBVX)*9U C4%"03\$05+#"X_LPDTCK_=;#I1-J+\8"<"Z8@#[@LXI_AC[Z;5 ^TKHN#2=T)@NKB(6J@*:RP(QEKZ!4A.><Y,DX<V[''J+CTS%^QDQ''^S,-(<E$.R:VHDI\.[__DY;!XLI? 4JT]C\"AJ0^?B/D@,H$-D*(1C\HV<+T,I]0L@Q1>HHTX@Y.P@Y!H@M!NCD62HC*\HPK,IV<8#^CHJ.48+06_K$")D(91D)''>:4*4Y#DV[<E0+$L8QKV@T"GHJKK*Z=L,#MMB+7TR0@Y&X#6&H\D0AGXZYSJ)DZQ_C7UXA[OWB2,?B5]H\X" <[_$#-Q=B<0D+B<J:LCR2N\%L6(\#CNH,OA$$#F*,FSEI-[7L_?0=NVSSR==E)F[&&=8&2;6[NV1/SIE0N%U1(%H9."U>KIX:P/Y#F2N3L"PCBG.VBGM!@GI4@FID AV.B@M"BC_>BL^A@X:*BZ6<N%KTFN7W.''&ZC@]=+N:!0?+H(=8;2-6JB# =0''AQH.=6LI&_@HDFN+C#F]%7N9A<5MKVNO@[FU"JMOI<KPA>PXAC$PV3DI[OQIJ<+M_..7''-"5%V1I)F(@)RKA*SAA*I+MFB6\Y+.-4N/IY"P%N&25%;CDI]RU/GKB.8P,/+(R0!KR2HKBH)UJ@<%PU^N7IVUJ7GR96''&E]PB@J3 @RY"F)O"BLW D])BFJ86DP)(GQ1":03II?;U"FX6(''GYS.YIT''"DMKB%UNPC]CX7AP4C;)4FKD*E"DF0*4<H(C6K,S=K,3>,4#"L1C$J1FT!CMV+A18#Y*5/I"H. N["41EY;-.!L)_(*(GAHS_;"H/<B,CCJ.0JCSY.T4UW=D97DBK2J1]VHQX9HBU2T,CX$L-L /?4<E%7*5U?%5VB51+G;UUPBD6(,U-E33&TYU,943&"4OG%0@@C @$U @5+(@Q#8@3%H#&! A2O0#D)X02P ,/"DB[&TF[X1C"W1URD;,&^<Q2PS&=8T*))S(C5=(EB*"X6(AB=C)"D\4X5T-TMYKT^H/>]2''L''TOJ7TR&W)4A-<S.%40?>)9D7CJ<*7^P*9<[VW/I,VK[Y#X=VP=QLVOK=]=]P-ZP-[/T8&F*@%^$);^4NX%]$]GLJYEQ99"5'']4$.[3T.\9]%=F2B5L1I4"HR3?@HXB@]$(HT_$@H@:HL%^H 8:2LZ+E'' TQK?.J6,[\O$$Z&_Q\J/URTEP[=\ *K581,K0Q@>)C&M8I"D^PG0>4<0TSIB]]_9,,RON@9ER-R8YYJ/*RMZ!@&5.\, 7R''F+KE,([D[KJ-PEMUQYD6<F2N=R5V?D=''JKP,(RH.YDDHB Z25&\GC23OW.2%#=UU!A][RQ]7S''].SK\5#]]7V!U7V%]7W_SXU<09A@H@6:@AY(@W?Q\"FQABG_1 GQ[@65*IP/4!]4"2+0+QDN+0XZSSV6JU]F''RI&GNP#GB% \QWP"M[A)$I%S-X&I5U!>SH(MT?[MH.''KBHG)UY%4/X&^B)7^SE%; VV_RZ+R%_"_0CTOQ@(3(J#!7AXWMQ*#!A23U ,S"\=7(=%MT$7J/[^9)L: BOU! P.EJI%* XC=W $]# U.# C>[ DOY DP[!DSY!W,% %OBREU[!E/Y E6[!E9[!FJY!FK[!FXY!CI:/%C@RQV").^$A+62C_=BE)4,I[FJCIE@DR06GCF8IW$E!E\[ JV9"J!:%J&9"J\9"GN["F([ ;;VU:YA DH"FM(@G?61@@"I@@ ^Z 5.D0I"CX!6^8R^V81V&826680V)X26N8#''.L9GRB^%*D$=CM2,P6LU".1,#3*323W@@ 6"XN9X\UR<"QUNMWC*1D31Q1PO&9KO #ICX7ODU9\7%C_Q;+-/"+N=K6EU&9UY69U^F9U"V9U&&9U*69U-.9TW\!3A( 2.8@4$( 2-P@3ZH@AB[0RD8X%1V9&VF9W?4$.,E!LU3"E:&AV8PO! XMD.]#(QDYUUZYX51GGC69(PU90L%96=F97MVY;]SD$''CC>;YIU"RV3\$0:9]-P]%V4_.W4%6RVBKRZ"PB)(4M%W-9HIN"KP@#RO[XR8!1EO*5H18I?=@W]_?(@:_.K2KK".LI,BM5."N3."O9&"P=."PI.&QM&&QQ.&RS.&SU.&V)$];0K)9@@EVH@M>Z NTT\-:QK&IHM^U=&&W?&&VE.*L1 5OR90D.CK*2HC\APE,4@L''Z@@RZHL3*H$D@H''KT3*MJF*-/ "]6F)<:&*.3&*0G../M..0I&./Q&.NXA/_0Q2V:YH$YY-M&*\/&]5OYE2:V<5R]L6=P3@5L.#@O)3B@<JSMF2AL>SIGLBBRHA;LB-&!.3HM&_<(N0C+^3K-.3L1.3M5.3N9.3O''.11?,,W$D]ECPOEVHM=2HCIEM\>"*-.E.7N%&1/A.7Z3.2LLL9-M(W?CO"^&$:8:>&AEQ@Y"#Y''GKO-8?[,9DY.3@9-\?8O"":TQMVH-_+IT$J[7L1Y$6@P#O5@%42*@I[ID"Q F@7, "YY_%3L^E5(5''6?^;*@1ZBK/UAO.*C/>+[/><[/?M[/?^[/?/[/?0[0@I<K/X F2<!]@B@CL+C/_="KJ1W0A8_0BE<T>!XG@ABGA=@C[ !G\+"BREBTB_=0BQ_1DQ?0/M@KBW&C/1"B@]PM9WF5G[7NHM.V& !U/Z[$T&7MR3Z0UB1/ 7YE/LW.H:5Y6=B*\D@K3 !1D%_2IV_2I!_0/[@L2?@DB^DL28 K>$92I=_2@E\T24CO!NNF\=@CA=+2?3IW<%;8#5[@GNS@L4>"(2B]JX@-W8< *%8K0X8U8H<%:A:77@Q^$%#RS9GD*"+R"GWE#5E(#CQ0 5DX!7]X 7^@=D"W=D&''=D*7=D!?=D/W=D7''=D;7=D<G=T5_@%10 1_@ARG(@0@@!&BVA%HX P!X=BW@ UE8A110=ECG=U37=D2W=CV0]U-_ ''\X!V2 A>V8@R3,F4+0A$FP=U+W=V^G]$>7=SUX=EE0 >;,!PM]UD2D0!C56>''<SL$B"$#.K4)>WEM<S[:#WC8?XAY$V0_<*]&X(:B:"\S)@,8XA5+/@#_@ W;/]5''7=7<O>H@G^A0(>HD''^HMW>HM''>HPW^O>D!?"H%?"I)?"J-?"JG0T(0@URT@Z<@HX5"H_E:@\W\@M-:O]Q0@E>!0I>?8BKU?"G]7&D#8^XQ?"CC?!ZW0H7:HI8VHLQ&@-ZDC:%"@(U0G&E+7VZQ?*$S7)''I0T(F@UR0M5:*H9J/T@\+U#$\]C_T,$->%= V:+0G.B''P$E6M6AW/J*9;]Q7C\"4)4@(BP1O(A<\"H@."H@4F0U+G8T%0O-Q<GUH-7V???.;57%[!8I84HX5:HIH/?.;E?XO(OUZQ?$.&OY_/?/@?7U''%71I??WL#?SM?7.?+7WNA74\DG5''I?7OM?7R=73M373I=7T\PO1_)8]8 HIIPOA4$@O?VU@DW_"DM)BD^H"G>[%5XW\C1N]5SN?7V''?4_!<GT&?4(5=9M7!>.\\EX[_518?43O?;4%]>3W_69W]46O=52(]4W_#2#6$@NR"GJ3"F]> BV^_>29]?47]6>+?<>+]??I??/=??4Y_<40\HGN<FB%2CP>@;M@AJI[@R3,(P_0%ZI_@W\RIFPAGC>V/%C1AG#>D@]_P7,"QJ#0''B!_LA@@6[&F2N-Z&F)DF1A!WZ%H%)K:ZVM!:J=TBBKP<@N3JWL&7*=B''T*EJ''T*5*=Z+L\PCJ^A22D&L8#AP-R!1;$VKX,FPS$A0B\ZHU[U]DZ_.0II8[WN?,G,S!Y,4(M8IG?;4#?D; PXKZ7H1JSD)/W117AQ=4,<QMOHHF766>.0Q1X,:GQ2M^P1C1X]NGT^MP''[#4Z]J.V\>VGQ,6:,M]DI(^M4(Y@GG=ZMER5JMNDC(@>)DZ)X4T88F;O4]F7DV40L2AA&:OI=CMPEK/8"5)OGBIZ]ZZU6-FRE+40MS245=_,/(C CZJ,CW !(QDF8H4!$LW="G$''&+11Y^ ^?BM1%&CC#[H(HHTK! _ Q R&M Z:B3$U$W3\@PR"Q2UYNHP G1%%()!&UUVQQ^%APX@@R2EQ!-\7MQC@5>44P5S?[P!Q ?EFKU@T% ).RRSSS*9)EY%IA@Q"RZQMNIHHY6HT/>I?&3Y%)_>#@K@O9J-\]4H[/"F"3YP8HBKZ-*L94XD(Q$$&Q->EXZCM $VE,E@ AD83 ";DZR  (XEZ%5+F3+Z!TF;OQ+)PYIB>&"%%D:JJXFZV.()[1F>E$XY[SS"R@Z4JHFDD+QLP$\[DM@30).A$UIPE1E\!6"DTC@V&FGZ8CHJ,NNY=5*E. &4F:TPA''+(Y,$^>H9=<W2@''78;=&AGF<^<L96!";;W:K#N*!^-.^=-]*9:A[U+K(^!JUSJDB>8E]EZEF$D(4\&)_PQ%"RIAEIIWZ7$D$024VPSS 5,09MOP@5Y#B1FHSWH$1%+/CGGT(4SSQ$]SP&#QR7FZGK?2VKERAZ>DTWT"!V#QNOIXX41E D.8%T&;C.7..E&''*F-),5.9B6KFWTH.RE)H@KD8>ZCA2K&WS1:WW"5 0;:&^CVVF.];KMX];7(]Z6-].X;>M#RQ%I\*FEFLC74@D8-T @@ AYF=K3''NI[51Y;Y9NHU6L=9C"[XBI<MAN\;8A6T''''2QG3+9-EE/Q!=)NN"RG1H7XXMMBU\\\=@RARK:-]! R?  :&E''3[W*CJJM8TC37$-V/2Y&"YKKJ+KL;8(''_2TQR3SZ*GBNM1''%H15AC*%<@7&4TT+G5U-?/YIQ$+V;Q=57]BT AAO,;4''==''-VQZM\0X^!.NS)/''NA#MM8?6E;B$RXZ="QL :$OMBSI8S\]9AQLJ8K_\IE^YQ&,?RT95#%^!\DG2#ABEI0 .N25F-@\S\2-JDM]XBAKE)%B[XA @0@0D\%-DE@M6##SZ@QF*I0LZ1@?J1*UQ,ECW5EI51<@F!Q81W-,NN,2;$KO+) #XG^,[$62J4ARC@CIPA0#LZTS%PV+BA*<(_EJ7K1MP 9A C68CJ2?@9=6;.R%S!RO">YC6@@65]KW+JT!]6$@S*AFA->0(Z ^J@AES-J$+@''2DDR$ 5P6H+KU(ZQ%K$(^B1SDU(2@!E<\ZPK@F#B@@4BAVHQDC(DZX8[^F@8ZB4HB*R@#!. L[=Q[EH76/<XA34V,1771TMX)&GL?U*H@<I$A#:*>VWX N''KXAI3&LXTI#JK&\1!8 @]<!@DN=*@@E+48@Y&2@PYO"DHEY0!@*RHAY=0HA;<RT^K(5$CEI;FL3!A@Q]PV@20<#HB>.G&\/KI3^''2NQ>BFB(3&0M@T_"C!B"N[#\E&)@2D6+L2R63(P)]I.,DX#.H8N:MW,JR2A8)OKV +9FQWH''19) <I6KCQ4AZ")CD(C1,HD$)!W0)SCO6,SK,*87>.&&?B%ZR<Y5D#QUM4Y#JL@+;/@LW_HHN?30A F!\ T2XDHH[K CC5YB"D(E0A@=H@PT#9LT;)0F@I=P4$@!((9XYB$S?E38 FU1T3P6^7BI\.2#W.FI1*2MX 3 "LP$=_D8VWF!CI 9CC5ZNPE!=>X1Q?>Z:4X0SO@@$AV@L%1!_8ZA/>&2PK,R51RO><B:\2T<O.MF@39F#V8&-H.7(>$@"3+V5:(FP5@H@ A"LDTP/*&5Y+DP> ^''TH5W*:T+&0X)(IB0&]F3XS''*RQ8&I VHV@4@/X"+]:U8%R&5YIF9S)KJ5%CE%LI-GRU[2C BHXAS^FQ@NJ-D3+TS#[&V8&2_&0A"#O.-M#^,BJPI!7%51Q(VW.Y/#](O@/#V&NW+I(R;'')<<EL;#AC''80!B=+TA2(HY*DZJH,NM@FTLQ#EK-AIY<J?6NX !2JMS"(YY: \Q8T8>@BA#E*''9Z&B@G<,DG."UBC!Y&!\>[''#34002F$6HVC<#_BQ#82$$>GS8$.YA9V@B<%B^[[_=7VYFOAK,%$EH:P2 QGWNCQ<-+POIP>38''Q&1954:1&)4Q)YE+";YY4>*V^:)ZLYA&SI=Y HOR8EP])Z&?_RDFOT @@AV-0''MGB>XEQ$LHQ)/3K&0P# DO3XB,BXL2!"QTK]#+0LJQH(&)C;])QX1DWW\!LM=(0CQ;%(0]S"HX+5M)!_=@#G!^8EP;4B98%N@J2$"FH^];!I!1H(J0BRR\.Z#@PU!+!ZV/@!V=@KPC3K(X3Q"5[E062K/=*CRP3Z>@\#90X 3[T ''R&H:*(445*B5([\&C\ 9MWP"IEQ#JRITK#F''M7H(J%IPF,$BO2FIZS9DY,#4I)PDBQ8-H5L33ML>VHU:1L[237C+.PWD''IH@:H<W+"5IL#5*WKHHM@<L@Q.LBGI:;@2W_6C@\<^AL,FRL,N_FIA>Z]=-7<0$+CNZHR?7.SB&/Y-3VHT-5FW;^:D?.N[(R C$W*''HY]TR$#$JHI$[#[7ZRAA!080 5F<ETKF1NX/_''EC[\P2F\D<09QDNXJQ!"B@LS3[E<G[Q3MDXA@6GQ4=71 CT-T!A6KP 8!R:[0MTX:8(5N(G!\!?DJ8TPRI%$Q;4''?NT2>[ZQFLS=FKH_(7<\3[!-N@^ZR?,!9J"U)R1..>.%Z=6T;3]I/1:]/_P_L3!G9])8M%I%QCGX\ [#[N6(0C E,8)J$,C$S^/FO_52!E<4!1QC.Y 8@:DMM>@AFIJJ1!>HC8A5V*LD[:CB3T-B#9JQPJ W> P$7UNKR^4<<?NE*:-Z$. 9HJD(C)-BFW%C+4E)1J0=,4"/(P2$X@RC @J@ !''4P1##XWVU5W8^M''SX@ @9A0ZD)0""$ V<,#WNLGPO^AR;00G[@5Q+4GQK1VA-4C#XTAR]426FTS!KE''02JF&^ Q5F- TKT 9N9W/C\E/FP1F;I6Y.-DT",1IXQ?9]HB]0]JY\^ATU3?]GE+I8T/%R[TTQGSU17ZY]F#\UO>XL%]U0<&HY=7L$;SN@U<A)4=HH-0DIV$PH&@H@3M@FY(HA88HHLTI<-@LL88LLS$L'']HDI;ATHZ_LHU-I]R@\N''8PAXZPD@@LNG#PM$3J@$/%>%HAC=-\DWJD<O\@D@RM6[NHH@@L@(-FG93PG[5RDKCP-  AH)DH4"B(A/4DM!GMI"MI) PLF$ T] 4EAQ-QA&8HB[MH^: Y(R7T7E6ADRCE9A_X["%M $O&LDL]9(3L,NU S,(XRV%DQ-R]3/ HT7\(Q05X!H:P#B=X"X&Q;4R@?5SBD;X,=L[\>?&O>H=?3K[.&N[.DKV^B^''!5FK1GFYVQ_MDSCO=2ME(Q]J_AAIK0BJ]S@^?UBV]4M>X%SK63EGK3CGK0W@N1CJTSCE#BAM''SBKYQAK7P]PS[GI(2CB@8[MJ8$C\;_F*PZ@(#VSTR]YMCPB@A@E;CPN82CYE3AN<1JH]3B7[S@XIGBBCBATIYAHTSB+Y"CJL0EE''P_C/3C+GSAEP"ABKRWO@@#S<+C^8$BFDRBYK3\WF$[ZEQQPX@[0/6GEI&[_Z@[R<X%[=  88EQO]2C/T0D#CSR/"3R:6D#%[GQQXGIDQ[WSMSDS^SD03B!1@3EG:T^0=5C$4 &UE@&$5 &F6C&$5#?%4V0!\U9X;:,#AYVWE\PSH)0''L^M1&UTE"% ''W.U SS0@*NUPSQ$P];M0U20$B5D #2L0!)\ B @0A0H(1@@ B6< 3P@@@JR0 UL72?<3M6<@",513-L&51^91X- ]J%6!]0P1M1PB_VCB9^0U?$"P,DW4YV0!.H0"29@"XX@P><P!''  1/H0=TEW1$0@R''00-W-A$8RB@B<I0;H@0@HR2C 02400S.<P''-%A0CQ(F3<D?8IG.D1(6IQH''ZN2>I!&4K,P^PM0U#H(1.5P.WU&6#:3.Z12OBD8>^1 Y^-%D&!(9$!QWSI!FY*9"C!:EK(ZES0ZEMH)(=ZS9.97!C*?1ZI;,85*!E/(T"XQDPD IV^/]!!ULJ-WLC]P@E''5D@@LE<!GIIOM)(8@X@L9L'']7L)3@L@U4E^]"@@@KLER7\5@J!T@NHHQ-A\K7P''"YB"F)).)0P%L"![BU\O>TT/P]M=''DI@PR@LS?H<(FLE%"D\$>D(NEDK#Z@LV%@D)ZDD!DI@#XLDUPD\$AM<ZQHHP#HN0JN\;ADH=XHG;#DL)^LI3DD-\G]F:?M)8_EXJ!!,R-JB7RH[))IZ^R&HM&$Y&6NT=VHF<#XRL,HTYQY$P($PZUU9ZEFXR7-?@8UDSS,1#K,QS7LL=I@D!@R%SVJY$3,H<YFYL%N.9]".8A&%& /=+.*J+HL6T0^#K<F!WICGR"U)\1)TLZ&XFC''3@X[AU83Q(T_%FCV2EF02$7#WN7YA$M@2U26V__+4CN@E@IF0B[VI]HR8UB6F,W5#M8.1),J*V*W%GMM%_.GD"E%BKN(GU7TQCIO2BP-*]?;3BGARBKX#"F/"$&(!SMGQAF]SJX"#''&K@\@ERBV;5CMI@\C&0AX2PP*$["E $,Z9RM0G8[@M0_0/T@B[S%]ECK-I@,6WJQ[''P(W :AU7 I"X#DQ[&Q//) 1WDT<RS@R''#^NMZQNR*WSJSTR-D(UL3CKK!+=U@&Y-;CKH3+8G[+8$8E8-8(8SJI=.B#=52^/5RI[5''?[.W9B3;^G)PR%U8DA)>8 Y72IF[P05H=KP@H0/BMP";L!UY]4&F8EV7J06^L010,5S.<5:74#Q-X VA(Q_^MFC>V+OFBQ,5DP!,L 0("@T7J4B8UJKP5S(EUE#)8P"ST@3+ 01V4BPB@Q<:LBR8@PB@HB34DP*RRP#S 1_""12-=K9=471+DWO]UP"3PWKZQB4H$4QI1[UGDPE/:J+JTKPFW2;C^X@;N@$U5Q]7.R7[-&9F2;SU^W%/<0S2@@QHFGI"Y89!%*0^,5B: 6X?J1A@4Q^HB +220[(:;(;FQB,4QP&''\NSF!AV0P^J*,@3G1@6;,EODLL\\T!$$$+7]?>/EMUJ?&V#I]NDW^-1+#LJ$UQ(@=H46)HDQ@DN@<$@)*M<+PHD&,N$[%FGN&X4/@L@-''LD;2HL47NDK@LLU:HM<$\H^@@@">DU@A,5''PD8A9;E+,UU&<LDU[@L6F@TR:I<+T@,N.\DV\IU9NL]APHD49DJAKV0,/LOYG\XH-JX[!MW-1$K7QX@LFBT)XB(AF\D-=D0W-@E8AHF/"FH UHHC*UX3)&T[NHIQ2L7W(,EAJX;>G*<L#$_#]XEB1LBGX)1N"Z#,SY0O^&L$!\W]"*MLB@L@$JL]OP0;&I8,$AS L TJC0DORNXK(:.86. 7L8TO2<R9##LO67A&(+@:1?=DI\R0YI9+C[NAC9]0B]=CCM^3N[NAH=C3T)P0N,<0U\0TIDT0$,Z^%_@VP$M\$?*CO-) :IHB(ICBEP1$^;5WMH"@DH!S\E:]T1%AX7#,3? EH,A,H6H,OYCBE-2M@>PG@F213@A@X &MG-^4W@DFY53]M]CBZK%ZF9SC&7SA,DQ1W%SFZH1IJ5XVFLWED1SB<ZTAF%3ANO3BLYS_N8QAEH</X\$4^H3WN53@JA3AF61RIP!B=6'' I)S%K!NU-:''%?4+QB1*JS]-4%BZF,RH+%ZAOW8YEW=)TM,;#F&%#NE000LVDHBB&&_$H7?9DJ@A@\6F3BCOEB?_DJN23SN3?<C>OZ42\J89"- (K@#?G*2M8@#5[)!UD #</Q^KNL0@<6S3K*6SZ@ >(\H+HAC*885ZH#GY)G,)\8X,(\1_>J>A86FG@E=Z!0ALD@TE,0$[NARJ"6F8616\8@#;@@),^@R$L)F<D@ XDY1& @FO\",EBC"?O-XX>C!>40S[HSRA3 C#40).(A WJJ)XV5VD<PR&\''C0LYFCP026X@2:<P''K>)B\0@R;L@Y-RR?\IQ#SL$4@@0J5,@#:H0".,PRD$)2K8AU''.'']?]STC5 BM<;SEP!29''%''$S<J%1ZA"%[X#*E.0!*SY.8[56H1^B%CNC''. 53LL4#1:1 ]?NJGRM?7C4)[Y&S4T914P9C8E%6/L?>GLI >,U,LD</3L[@DB72 P4RDT[HC$M18P''''N,=\<2P/&4[XZ9N!X=OSWAC^4%D8M$>6/\Z8A*#6Q5!>JHAVP6V;-J3LU+#L@Z]"D^!CHYQP([S5H3#9L''0<P)5%G^0^!TN%D(E=D]?ULLU4LBY>A(@<L@&)DG 7LT(/LHQ3H0(9LKP8$H /HH(-HD''EHH''"L\+XHH-^LH1%HE>QZ3;Q@O=A@I9("P.NJTH^LH[Q@LTAHKR[]YZR0*.>"<R@GB/("V#;6&T7*)];.C%[)]^''8U > ,E@:[7C@EKHD25!!/AQX0SN!]K\^-SQ@L9.??3BKN3.WY9B]>0Y;_B._*CB O@.<_DO%R9Y"\-OV>3Z*.0YJ+-Y]M0MESBY/-3@$SCOJ>01#2\QZR(L&<TDWM^ZV*\\B^FD1OK,@0D@M''L,N2"6-WE(Q9FAACPNJ1&3S1G);!"TY5X3RQ5ZO3M"Y^7[;[P^5U@*:5 MXQ@K>0FJ?WL<ZES4A2ZB*T4X^BBJ+<RC!C@X252V/N^8?0O,XCGBFB)JBDNJW GJ_0BOZSW@E.P5[;GXVR-!=]2C5ABF63]P\%F3YM,-9TMLB?DLENRXN*["6]YZMK-[''>E#[\(SYA#S,S(\/%8&]5DM),0@O#0B?,C0+>.L4QCJ@Q JBS?_BT8033W C;,)!GP,3=D@>KF!K(C0-T1W3"0@P)W@. #^P2+.41,0!,> SKHA@CLP0''/;%XH R^LP9T;IY''TPD1\PRP45YGKUF;#R9_D8=-JF_#8EN<4MN8=RB>%!4A,  (I$KN04BXY!NK,''-"&Z&E !-!6PZ?-$!G0)H!)URAXHN''\1+M''ZMV,PS0(Q17H0^_4 H[A-015&J$ZEY,TA$A4\^LF!;YJA]6<V4M/"Q-\B\^="?^N5J!QDM<UGCT.([XQ=KSU8CEP5A9-;>S5P8_#7S-\FVF2%A$381(\M&D.Z\%RE8@6"''(4:HD$A(A#;9Z05C&SJ\6&S95F!S)U?6)U%&.V+IG8;!"@^+N,COG''C9AX,P''BIR#+321[->D@!RLKU::?U''[G2&4EZN2P-C8@(FD3&L61M-VBH,EV(T4Y0(N5-OF@)HG"OF5JO]XLPOO HVD0/Y!UFE"%I5X@,XEF+=A H9#0,REU!-8PM)3YNFI3!S@@661Z,P$7AO]#J;15/?MD[1X)TY/&,R%#N98CM606%\DM8@&OHZP87;/-&\6=797Q)2\LA\A4- %ZIWA;M/;<>XCP0)\?AG?<.6K?<:\K@C0YYRH\&FIIF0S_>V@\%-3PI)8.8"F()''\.F WAM]0XY\FVE@(1G!1F0VVD#KQ1"H]@KG*IPY-X,/=JQ*)(''MGFF*_R\DD:@M FBV:02Z^ZDE1Y(8,5W,I%EEH\&V  A9\XXRA\RC$Q09-H XZT\T"A8!5-#A"IH0F \NO@ U)4AH\Q4. DDP@@4JJPK,X1 $P\[Y1(CY=:BJ(GQ4 HKJN$XMK%1$O1SAQQ&FAB@8@X9 %KKK+8."...LZ:B2?=;MN/O+O0R4,.-E AHHCG4C"L,&HZ>JH10.1!(9<61IB%, [2@JBW=N8)+#06T"-CD].,<L\VM(Q(C[I@A MF''%<5@VX0@LR[]!<6"BOL--G #JY[@BH)+!P&*,V''EM-0N:JF8M# 9Q<6Y F@VW?&8^N7ZJ8]KC+5>DW?[91(2%#++++ 0*/  #VU:>B9<N)+KK_6=@R+% 9\UMFKK\8X8845#.(#ABN9((H[<-FC&1OZ(HDTGK+TJ)BQ(H @PQ0,1MBM"T$YB\RGP-09HZ42:$KCF@_E(X,ON)0YQH--0$&)K)S<XL<6$JBE,!9"ZHNU&GCQA!\\YN:X8;F+T  W+YQ4]H=HD8COG?7R"">--S UB2=LF<[TT+$0G\JJIDBQ@Y7G# &AB2RPH@HIU=61KUX6H!NCL,,0T<?W0Z)-#!9ATNB,D''52&<TSM.Y)1YLQG(-&V,>NH8075M&(YGU?A,.7.B,*RVB0R$PAU''U@QA&B!<EZ6R::VXZP?<D6PG!#"04+>(7>,WGZH6.H?] J-^:7O<7^T>3]''#S;]0"$\EB-2D9_;OWU[7?FJ"6"88(:XMAC*D3DJTT JIXL&!RBVCHJ&"RDIXG BME"1JBLIIAQKN%B#ORD 9^@:B+.0=BV]GDE@@1EC?B8@RTBT:H.:JQDS&O_B\$FD?L9"!MIBD-=DLZ0R2$LD@OS''''? -+66IF@^#!ADMLC0&CB4 P,74@L$=CBLM$QBL;NRWFV0$Z-](V\^.AGO-T[#!$"T@P-3\DL41@NLT\ FFN3"SGR 19-9<FX4[O#]^@[SQ''=\(U*X60]''!#BK\HTKS%^H#&? JA1@ AFN8.ELF/=?H98:R(=_5B-C@,P708[-!R;>(IP%<7J7 +&EK6JI@HF0 #XT#-I=)RQ%1"+1O3]H@PBS$@LL$A@L%HE"CUA@DB''^<\A1#JH!F@IQP.J1A@69(T1J,4%REK"SN<TH_TT;V(\V.KRKMV4&N)DH4,#PA%$X,V*C4I(D%>B"BI&R''H,J)TR:4IU:7NLE@GIX_. V''7":+VE<"6DL88JV\@#BUA$X3S5RUP49W@4A@C @F9JPAL#P"%VV@TA&4''N9U$S''E&:@7"02@HCQF@DK,0CE,7I#"32J!0^^BH,U[2L;J4SCM.XA3!(GX8U<C^DJ-FNCJE;0ONJ1XQ>*.<\-G@G?OWS=DU.+V=4P=,UH:_6+CGJ9G ;]$$O/)\T>?L%! I0JBOI90''0JKN]WS0%V\!+AEU @APCH(@-ZN@D[^.BBNCC1!"O8@ \QV\NV\A&#)LAHH%:SBP;"<XEQGH5AAH''@LG^R6JD-\KE?/T%"14Z3COVDZ-"@)RHBIX Q8,HQWZ I@\LZV** ,5D@6@MX!/ATR<8PT0SKYC8?5Q?02P5;8[CBOL@@@E8H,P4]>DHC"MB*M$@ BX>[!1NQLAQ,K@@@].!LJ=C5J<1!21N)GP0S^CLXO)A/G+>1010H(8$\CJXM*VGC G95NZM.Y#C>:@TS"C^N7,DQFKF1#Q]0(;+ ??S*U3I 57&T6,#6+@VS&IK$)SQY*['' [YM6V4/DI.XRD8)U-AV&,JI\XP._0L$QBLCFNW)P@$Y\0QL@.@HS7$GL^EQ"CTN#&]E (&KHV!LGVQ&TL!NKEPX*<F2I!^2!]GFS&^RR@/4X@B[H@H@"XHL["B,JI#C1"2O  !87Z_FEK\0(ICV3J7.8!1WPL)[-:V\-[WD0)_RB2[5IT''_;JLP?LCBD]''!AFPL H!1RX@D$CFLF4Z AO+2 TC-;(JEQ5IU:H D=4)5GEJ5Y#PZSPC-A?NLI'')$ED:QA"%&8(P2$ M8?@#DDQ8#BL[Z9 #YFPS/(UXH7%QBO^WGS"%E8 /<T[N@AH&I3F=.,8P$5$H4(>J@[@M!4OON8Q1''FH]R$A# =#-ROUWD8-:/&!;[],6)V''R* <!5(15''F<+^=GQL74LE[/SCAE?BP#0X8 QZ;:@\@[KFG.)(H%2>YRCQ+''BHMMWA&W.77W666X8CS!HH]40$)DJGAJ;Q!A"CHA3YR<H@R$HFOKR"Q%1HR; ,[[TK(]EP=L-AN?&!20Z.M(U7FGE-*;:\?FT @J:8@A!3X0%-%ZLLTL$@KH''C#B>6A43*2HB-Z2VH(.J*\Y(KC 7>T6LPZ!I8#DKG4I9BB#*P[!24DPQ#18@LSU1CEJ0 3A7"W0P"@YHL L@D@] %[.(O?FXU#_!VKI4S#B))@Z"-8X9-@/B$R<,BMCE1:Q<K8OQ*@VBR3?PT0-00,K0(>FE<VY+CFM<1.DBLP(2(F[,5+7LH8DH@Q:L@GKPR"S>]@ !9:(@L>WLD@&8 H! R@:!(+IHD$2# C(9'';6QN\PU9]8L@''[JL A1<JYP WB7S1 HG&P1ZJLLDS@GBKM81HI?DH6>[K*Q@[''>7#$[*^I[<W-9SK)9H$1</^L)&@R&R DMFP10.4HHL8+V@XCZCEFKAAB6E<@ C?DDDW!"@2B@6J\,V94 LZ=&T/2LN%]J(<Y$D[@F@T]DM["(*''6D@I2LL86LA9Y(E] HM8_ N:X$*:?:I#6SJ0D*P%ND[C/DY*. Y#@!F/L="#CO8C^:9-?K3G6&9(<]1"*01D=3 /PXI0BBO+ X@@PW0"A"X!A^(@B_H!BEZ!C[S@H!+"IC)D@OC-+1+K1?1M&[P"J;:F78@P10(., P G%:#C_*AEFB@E.A!(M[@EPA@A%8AB+R!B0K!@8RLBDN+85#BTT+A>=2BXRX)T5YK3U!.9V"+*"0I*V!@A+A LH8@@I1AE4A@DR*CE)Q D"SA@\(@B XC.ZBH.P3PAS6PW\PC^,0KW.J(OG!AV\#C.%0-]#P#W?Z+*LR#ASW#M6Y!MOK%V/YE*F8AG8[''5(;!'';AEOF;1G*)%BO<<L@X50=$"*YK.RYHXC?7.P%LF4R5F@YR,J_OB+RV:Q+I\!@>1;5@VBD& 8QWR; @\8P<\HP#D8P(NXQSV@@ * $NZ2Q0++BTT@RQH8Q[ZH0K08@Z2HPNJ( ?&PI"0LA;JYM?RQ6 $JLU:K13OZTKTB[ULS"7(X;[H3F8T!!C!@,E"RC9^C C@H@&.A1 @ @8B0P3FXJCT8AN"8Q>.H1PA0@NHS#FX20C89Q;Z:E^@L &@\ SY(@5ZXK=:DQ%],B&A\-&*IR*E<"$UJS7>2P+^(Q&\@PC48QUV,^6\D!+U :$:RZ).,G/*9(Y&J7.2Z*.:2,X6;62TP(BV)";-<";?<SH/=WH/>[H/?WH''G& M;''DD@*DL+ @BBL@\ODD\# @H3L@&(L%E$.$/J[L2KSL/#VX\:LDM#@@B@D@TI.DO+F@@6NDJ/NA?A@I*IBPBK+L.@T,"M"JW!+BA7,EQX,BET$,>B 3MRGK2PBT-<4O<*L(,\$,0$H\MXHDR 4@M(.@A)N@_7ND]=FTHF D@N,EVJHLT_3H()0,(1VL^=.UZOI@Y)YHY!UK+9&DV*%J:5%O+Q LY&9D)J=@YB^LYN4L*1[H3GF$P<T%S,-DPR^;$-KG21@KB-&<HN\R/Q.S_FKQAG_QAHSQBIWQBJQQB@8D0ASLHL@D:OZDMI.@1K5P+??X-H52,P$74QED4P-?ABK+@DTZAG&2.G:8!D= A@NRA&B@$QRK @78/QQV(Q@["Y!B415!B'']#I''WYH6,R/3GZ3-V+(DM],UEJKEZIALKC%V##!B)+!F''S!D=*@GW; ^V1JB7J2S1JG.ZYHL:J2#,+SL7IQ]?)%$]"4JE70<MR4O[.3;^QT(\0SS?MS0M0#-VR+6$2RTEUN?@ZQO;I-PK"J67;,0#($H^30 \:1T"6FQHA G@Q3EE2@G]( F#!!CWQAE=P EVPB F:OG2'',P_:GIT*,Q,% C+RAH2"5SK) Q=TGE91D=!:5<2#D,2YD[[0/3AZ,$/23R\5R^6P+*/<V,S Q*%*,8A@X(P6* NG>(@J;:=6.,> .@:J $Q_/XQ;29T?I%S6-237K04;K-U?HD"P3BT+O#20$C5:O%TC=0TCE,Y&>SRB^0AMB9!Z$(P1D(@0D%&@G-&@Q=&@U5&@YM&D[]&D]M&H!]&H_-&H%5&HO- 0<0Q''80E?K A7X8Q?D P:: VN[XQT<(P1.0QM.PQP"8QX.M&X)U&X1-&Y)M&[K8A>.X@/JPAQ$@M#@9P("8P@"HQIDX@,FU!QR%&A/]&X35!MPX@;8TO/PY 7T9,..A2;NP-* J$''!-Q@SA%;E(/4D(5*BP0$8HPSZ(A-PI10FH0LFH1SJ]B"H K''?A&E]<SY/=WY]=3L>3F2*NLT,F]ES7&I<- 7_NB<^:@EN2,A[8NQ1HS]2IW]2J[]2K_]2LS]3M[^OSJ0MN-]368@NOE]2(<E1M?]4TS]5MS\Z&L;DRG\NUW]3M\!FQ>DC@&D6?[@6@ZBE6*$>@-PP,QE0BWT8@Y\-! @L("G+=HT0L*DMP*@QT,A-B^MZ-AT;F4@;<?L<;GM/.[];@QVRF*<7 ?\?DR1!<HZS7NJSO@G3^%V,%,@HJN4](BF(J*DRGJE>;Q]?9;\R(BE?:?]>?1]?@]!?BQ"@=;]?A3"AAW"A@1"AF["@G5"A@1!?Q0@P5&@_1*DBJL8FC(@]>(DV?10AF%+AD\[ADW! GB)!GOXW_5&8!U78!VD8!&U8!&%X_4-8GHX@A?3AB!SA!LN!!D4X HV8!&-XF7(AI]5@&ACT1#CRJ;A6H>\&R[_G9ASL0B*I-^A"G/2!?WY+LC@0@48!ACR F=) A^X!@3I GFJJSC7@SI\K@G1RKOGS^?MVW^$8F!WO[;W''O%ZN^1JQ^N&#\O''CA155<4ZABZJAC%+*#!''9#.%SD\"@GV0@C,AA[T,A@54P$1-9$:OGAKWN^5,AB0A@DD8DH(OP,?;0(^YA9GS30AJL<-Q2\K/6[\PB@42%#/YADLR!CT9 F&0@@MIA4Z2+^&=%@X8.O>6X$?>U&U37496LMYM"JF3SKI+; "<,S6J>DTG[PP[ [)&=&U1GH10V[!F"@@?TXAK8(@5J0D>?.Y7].QRR-2D*D,/L!,-L:8&55(>#KX]X*5 ''+?3L]9WQHA(JPZDF0096>Q.TP@?T0A7Z@AF.%SCX6H61U7+&>CD@CC52$U\PC3<36)7;YP[M0"O9FEE?,7#-Q''3D %G_T%?C+P[Z(2%A^*[UH3U8@@AVXQFP0P2,Q 7@(\><=S_, *ZI6''//0PAF&S[A#SYYJ@%D;&GJ+2:NE^VD40XG%RO-01<2HK]NYPL30R]PXP1TPQU.(@HL,1?H 9"3<=@:&!T34IL;H2 WFU#?C(?XG"NYL;B(=UO17J%<C3GM=J[1G*=^9[U@/UFT,@>&.5&/FY,0:JEC*4@M$DDN&HDV;$DQZ"@R6 @X]FLH<K*1=5Z!G(^Q#1 M>F+33.%,@,BI02I<=G''<CH2JIR=,LV&5!.@E4FFT6V@\?H8]'' DDPD@U!@HD]JE1-TA66DA.69!.&\-[&Z6NO-(A5RN:L[(>UWJ''O/$18K*1GZ$,*5(G>7"GE#F%LT$,7MIQ!;@F@NX'';9!M(]D<:[ KR*0MW@DU9FB,9T@NI&DWZOJ!!@*4M5&4N[&41QE7TU%7@;E79\N/ [^5GJX#!1L?FGG$9.DB0N@_4N@E,LA3LPDU?;; ?"+#@T2@@>@DFMRX&HLBS_?4ENGES&7#FS7;.Y@NO]P3O[Y[+:UQW,47W#FI& 4FDO1ZKC+IG;(Q&?L5,ZO!E#QZS^=Z))$\KMU#(:F\/[&3^7%AA#QH[R4@D&@RFV)!]B=/^ F\$T^;$X<XGSHBGKE,26CBRLN"17W([ZJXO9=T$''*<9OR"[]" DEB2)NA$%4=@E()AC&"!@"JACA0@S,0A\.ZVI8=Y*V0#(9G;$?O(+!$P\82-L?Y%V;CE+*7 2;AE4\Z\LI"*A"L</HWW*(OS-]/2\G4O7MR[C))\W]W3NM$3.3<9J@<O0II9+!>#1.GE5?LVB''K!FH8!''_>WXP0, EV* P240A22PAM4_]S)6A;J?H:O>B *=U\=;%F\>'')F#()G</GJV7,@NP_?&B3& \==XQ:0PAC,0D,U@GD,@A..PXM"P@U(XA3.XPB,$>!XQ\VYGD?7XQ:N(GX>,AI>0;+NH53->#F$M:>!Z7R&"4?''XQUE/\V)/Y''E''^SBM''!?/FGP5;CWE8I&T;6U?C:%.>6P:#3WT4;EX=+E@0[;QQ_/D;:W''E1M8W$6 AD. Q&JPP=T(W&OHPDT#T:)7W./?X9K0URTN-0.$"-XV:T%_F,UDZ/-"RP]C9+K4!G^+CW6PS3^Z )@ A$PH@V6HQH>XU=F 86QPAY8<*G?(&^Q@N@EOGC!30M9[L*<.ML<Q,M/D+@>8PV9+>W(P?C!Q56$7>MP]R"V893U7XIQ_?CUW3*Q\;;R:VDB9@<QW&GRVQ;XA980Y$D:45NQPK?I_;5Z^FA8^NWF%R((G_*7 L,B.H@L#$E[O#/)]U<3&/8X^.$\\5]X0R3\482JFP3%N(UY[WB*7(OO.7 0!( K&&3''_.@JYC4A8MX_7*5V"$; ''U0CO[\OJ DJ>@<X)AL@7HCRGJFN,N,3BP@@RNF!PWDT7$P_\"E6:,DVOJF$]BLR_LG8@JH]&0.X?,6IM(*MP''<JF3)<BCF"1H$THX:KU YP %[>?HW+>CD\HG<\?4U:=C"R(4)? C:6;C!$)K<!?-X@<KQ&2[-7<];!<@''4)=B QH\ZKT+4WS,@YRK^X?M4GBH@@J8@"G[%2!06P=#L&= UJ-^0=?(ATO!U8UN.C\MB''VV%8R1 4QPNBT.6(-:GS?.4>]J#@QH$T=+8TK,7,^KE#A,;_/20E8@@Z7(^/X0T,6Z ;7J.^X]+EA(@^>9YFYI@IL7UL?6%S BHM\''Y,3''NM&&2)LY0B\@@PCM+5+5;G])TP2J''VKD:U:P)SFJOS[<6X)@H1)Z''SR&IU@G<*</67=MRZM#4N,K&M2A^,K16/PJ*D+9&@@*1JXRH#Z!W,=9],UHJE%[PD4$$WO=EH8P#=KP122C(,HGNVV0)5@)$E"(6C%L$I\CZZ+--%DA+L[7VR( $ , QSQ?Q=L9MWU"61F^[3Y!Y#S?U@H@(C<6"DH=,%@K@D;3D)T4!N][04C3;"NVVP55U0(XCU8Q#%8<U, EHP5<98)@#S?4C0EM)6^W^!QBI@$@]OPSV01P!OF!&''GKNR^]DA @ 2C,16, ''#Y A=^H;(7FR1@-C%M Q[Q25=MIKH+)VH&>1-PX")YF&Y&Q9CP%"W@?XEMOCE>J8(5A44%E''GQKXK@B@GQA]*]A)YI8535'';3EKFJKQZ<T=XU, #B![/)G_ZDJK\I\<Q($RSPQ%1,SFK!AK?))]^%$N@A66S]\:YXQ$\,)P[.B#UA":#JNGV$V44#TRS+##-"T._</+I6UIMN[SVT?P@\L.0=6SIA"<@5AMWVOO8&MX>W36[5!@@[LGFNF106T(RDC7E4C1#P&,U![D>AF.\M @@V@L=DGZELFI-27KKK%\$FVU.]CE/3SR. \-''<WQ13LB&4ZQR"CQU6"E-G25ZD$"M"(P[!0&0L)%C1VURCC[V_WGE@ZT*)DT[G(B*Z!8@CAJQE]5]XX-C[RS@A!<WJGPE-W^]MPPOB''VBQ!=Y7I/^OI!(8(Z.\"-4U+P=-,EFMD=]6<)WZ;4\94U%)G"""A<"2!*JP&.DZF,+??+S("^A8/BS3ZY#!*NNDHUUC0CJ.@W%B4HHEALVYPB@"PE]CSFOI0A((8TLV9Q"!S?Q>L:TVE!H$2LZ4B1T5S/:X@I@K8Q_QQT[BP"2A]*XJKO1!T?Q@\@GI -V6CFP+<=>28O ^W+<%;5H<4>#;SGOZ]>^>9I--&48D<:=1''JZ806%0!DN732HQ1'' UCT<UX0F_JDMV&NC*ZX#A%$H) GY6P9E%(P0@C@$A8J8#1Z")XUJ<DHK"B$CJYK@@#Q)PSS[@P@T!/BKV? CFA!(QQ("HZ65WJDU)_@EC;P0K\R4;3G]Z,6"&@ZR6SC*H>]RF!VO1)I5-\X&;=)I3. EQ/<>JRU9CW&\P"I1!XR,;B5,0TP4*%HUSFR@J5YAW!,@@@X67MD;''+ABD/SQ''V!DP1=,XL!U+EHUFW@"\U^PP_[P L &@D@F<#B#&TXFFCXUQ&UK;JP''GQL34L!H_/G3CD?^4[ORVBD5LHFT4B25$WPY3UEIT=Q-''EZHZF!JHY3JQJ($RLF-,ZE+X @[]%)5LZ!<YQ;9X L03+HFP@ICXYFX 2 0@X!;]NT^8; EU]0@@BZT(Q<IDTHY+&BN,;PB"X#@PY"(1PZ3S^0WYW#E-KP)$\_9"B<S,RQT?J$XJCA%MYW3DH L&K$RXR:@+EQQZ;+ H,.TK(0TQT+*](RV=@C?8E%(.]IS#MPL[[C!G]PK@ND"<PP+VBD@U(FRCL*0#2GLH 3?^@HOGCFNY$SBGA0[VDJL!@&(>B9V[2SEOR+A!A3UBT4_PHH&65CB_W92*%RE2I72UMF,I(T''=^/B(IIP+IR\1IYPON +M_\((+''&H>*@V $75Z$FUJ4B0[R P#BX*%T!\2IW4)#GI,P;Z#5+]6RZ11A$8IX+U^I1\[%G2EYVBXX@H !OHELYK:.V^0 G,6,TR3N!MQ3NK$Y2+Q&WRV9#Q[GZ!&$#XV5+6EVS&7CFL*R,:A#9),R''RMH.#>-J%#;1C8D<C1=%8HM0+CH,M$C#G7E36KSP]@E?VJ0R?3'':B CZ8@X^@XH*OK*JSM''0BPC( 0IP4"1@HXOI$6F#A=U(@2^+B-?81*02N=F*U$7Y$9;UH7?_$%SQ5H*46+R6T^QZ;T0R4I-(&AR.B "L*"Y8 @3X05Q]>9(,0%Z=YM9EHT.:T%^V)E26@FB[C>D(F>I6*:>4X''EX4%YWOA0WKKP!D&U0@5<X0$X3V%J*>OK,O2OGEGXM;UHKGQE/"''29U7*.MZD[!TS+V5/SWUP"P:ACD4[P$BP<YQY\R$<4!+BVV''''BJ16;T/[.8\"EMNFM7_DNI.K21%&]IP$@LIAB9$A#* S /AXZ6SXF 84FEJZD_H:/(U?6/#3)1K9QA/?JZD*Q/4NERH)T-FUKA#"4H;=22I-^Z3!2.T,6GJLM(@#LWN-:PUQ-,HNJL_EZYL+"3R:I,FOAR8$]D)_A^$T!@F,H"FG5.DI+49I<5''I''F]N-?Y%+".U2RR5C(%)YZ+E]M<''IS&#K:CC^]"JU"E@V''*WY$[[!E1)E#LOR1 X0N^P*B+&#V)BGO[SQSYA$"_LPH!FMV2&$D''O@!N<<8XT:)Y]M;W7/(P?^//%"N<+4:$3NPBLZ4$R:M*>YEJ\%ATU2T])(?JOIOJ@V:%D_Y8LS/A^E*UNLB1?SURA+"AV^T"GB=-*LI"ZSV3:<,+U4NRIPX,!ZNO*897EXHU6N.Y[?#&;XWS^$P# NVZD''H#''JD[''SY"V P#.=YAYE5B];2+ZT%P(Q_30%BMFP!-!1OD0@BHDMDICAJ=AR 5/P0Q?3^JM[0MQ][IG/VUSJU.H@,M%2C4G]VI)EU>ZA 0!MS$9OP]N_F;C^=BE<<.2;*)9F:_VF8:A>  H@(P2E&$S)A#_&J#H,MT7@7Z3UM0-V"CCZ0@W+''I(NC<G+]U#E\"UB!T.@FEM#E6HW-6P)J-,QD6^/ET?B$Z$UGJ7PU3[<$LU29Q]&ALCA L>VHY@!M)Y5AJ7@3H[<3HO9$F&""&S2[D+S\%25W@%M,!#[]74&W)&/6[X]$)XJ<Z@W[\@DO.J2?0S:D@750BNB C,BLEI(X!?) V;-Q#]V0PLL P:C=DIV@EQ^PW"DH0MT<$Z#4AWR4@Y^<C1F@@@MJB\CY3K-QV"T18H,L5=.@FW5]3LO51LFV@<?L1.\%%YDU"9U1G:&]T/!<GF?8Q@N=D,%I40UY$1"X34P,SEO<V(V@4J89''M,P25)LP=PT$!ZVA\>\!Y+XSE^ !#Z9RSPT$ ''1 YPXA_?(A@#4AU(Q1Z30AA>!HY)8P<L$%D+5! BUPXD]WUV!6&''Q7T+<!H4@UF"<6PL=7V*41Y(HQ1P B[_PQU-\@P = IO@D\%RG-]$S5!$S5,@DQA "WTD4 @8@"\>@<^Q/=8]YX<.T@U[AXM([AT9VL]I"M9KX"K]II(''BFC<,JK''Q\C$\YJYBUK.WEVREYPZXU@+N\POQM;CPBM6=@F7UA;5JEA5,E*+4H8-NX017@K-?A6OH@H4X@HB_DJ3HVI6RLU@@@LZ9@?T8D#[NA<D#HN&E F>H@IZ* MN,HC''>@I&L@C,?@_55M''#>L:[#P3V''@E"K@F<KPO/7@ERYT^SC@U;V@KU8E#6(H!FLD9T_QD#''H44RY F%]ZV5Q-N"EJ/T #=<]OV"IN?AHJ)N@P+\@C!^@I3"@MG''QTYX@-#(AFTLHD(/@O-#@\?%@HY^@I-''@DL $U''7 =3EPC''.@I]#[?A:E0AYF@BV"0\2]HL&.R &6 O+$(%&XRR >''$''1"R#>QR/25K)%#\Z94YEW$O;\!U(;"DT8C\,5(GD_XA"[GMR"''W&JS^6U8VF_XDMD BO9@],L$@TN@C5)0C;YPC61@C=S3EU)P@D''@AB34BRVDC''ECHSJ DFTPB:7 A)F@C&,7A=XCCHT0AL%2C<55X''Q6VP#2@*P@@E) AF3@AK= O@*ABI+@@1FRG!S C;#IA''48]D2$HRU"TJ#7''F*UZTPCV2$BN%-GN#F("CP2YQS!T[_6U30BJ880_,$7AGDQL&LB_%3AX!S!BLI1JAO#EX<%I89''GYDG%&N)''9A!^V_)?2_405U_]Q*H8!IP5C>2ADB\-%AUY5A?LHR-10Z/]0*&! 54QW,NXW.* ''-N*EE-DF_%.UDJDP5V4@SN<DYMHP?M4@HRT1]CHD''^\QY[,@>=Q3]KP(>/@@2?,ED:< -.\@=''8P<WTA\R4 *S(3#22AT!:#A/ID"HD:K?$F-5DP7Y!HX@(HTY5Q#IA#P^&U(''XVA3ZVBE&B#T9 &3-Y76Y2?=5FN+L2D*1QT<1!\< $=O0UE.4U/7$@Q8PY>V=WRN XK,AUWG-)>CR!DJ)97>RQR&%CO7L7D&(ZAT!2!3ZTT\E6B<LPPI)D-E.I\$59]I:CT)!6EM>AABI2FCES]O\_<V@F@7Y# D=MA/@R@-L\\#3)@VH;Y+ L\F_^@E[+@FU2BN>J P''/@<,Y$8B,DC5)]DZ@ %NWJ%:\D#:BYO-*)G E@W#;N''^!E5A;ZC4.%J)*\9JUJH MA$"X"(*@M6?_QXTJ PR)A[9+Z\,,HF;L)[O;YF!CTK;M($O>]# &(!#T^K4M &>T&(@:,W.;!(Y4(/N>LSW.U9!WHW0?!,)W\.7B*]@(QPC (5DN*L-] @4$"MFJ)*6J ]Z6(WD&I85.T^Y:DE/-B*_\@F>/A6P(@&WJHE=%D@%YTCVL@C29I<ZL F''(@C%U@J]UDC(+B@S3@GP9@E(S@K''(@O[I@CY1G?BV(DKT^ZH\A O_IPVQHRB(T@A_+ )A06)A,F[%&*H^_W[E3J[GR)\_?3_"[YQZ]T+$#A$!W1V^**++@&V( Q[G)A[LD6I''5:HW=Z\@Q+.AM!*GM;EI61I?''%L:_AD)S2%&X5J[ED^/9#K"D![Q8AA)''ZDBKG%789SB"''\(HID^")DLNJ)MM2E/58%WYCC6@[B_#0A@Z"BC+:A@$1@*F@@#" NKO0EYG@IXHPBWQ0DF2 J6(4C)! B3[EA/!0Q4HPBTOP@); TZ1+A[=@#.M0C9: STN@BJI@C4M:&O\@CJ@YO(/A!78X"BM2J].JTI["Q@>E''U6G,G3R''WF"X?3J?1#*F!G72+>3>G#8FYZGZ<@MXW''6*;"?.KBOI(0E>(LP[H0D)F''1:;9B$1(Y:1B/=823U87U\W-;EQE]]''M,TRFOUR05I1089"OV4!@$8QAID@:F=0:H<E5<*4=&AFKYQ7P+46T<8 >/8KJE1C-FU0''K)IE;HS$]NYHY-1I-N4.4<[X40TVJ^+<+&ZX.<5&OPV39=CIOP["A^,A#?HJG^,VIR(N(IGF+ACR0$U@ T+&< R9TMI_! $V)LP>>DV)F2J&"*8RANSYNFAXL(R6V5@*<X4&AYVIFA79&)J\M@PB](A[ZD!]B11TR4<MH.#H590)T(P=687P_X5''H5)3_L/>]7/*GA,UP\^%PLQD:(3MQY62.#C#FR4P>SPV0-5#K!;.K,[3@)@N =-N0@1("U2QKX^)ZE715I_H!*?0ZF/29@OBL4S"MG72MGCR2DYE4#?M<Y)!3VLHFR0I4K=0PV9''C&/1*%;TV@OLT03\Q&;UXBJM;Z7D%EYHDP+_CY.,-V<*6S=1>G@]E[!.9MADAI>%EZ?CK=TIF.=0>XV10CD6(YX''P,$27"!)1A@"9#))V#7I1X6U ARX.''-Z9H[^)U=N)]#V:1PSH ;%49.Y!8Z3IUMY"D8J$[#GN#T !BSOBS!HV0)D@\C+IIJY4ME4''6Q):4X&L;:/JP9Y58(*]/$3?4TNQ/1C]L.LC@@O\@<X ,EQM*O5)1''OKUX8F@I@F^"D2%0\J$%H,P@15J\7\H]C,^&40(WIU(V50(P6Q(W+E4''I!&F](NO/VH4LGC@(ACD5206UB5C9"Z63P+C:''Y\2$DL8@3&T;_,<BX#-\2B6MFN%K2&W NV''=Q@@-$-@&1PL]_6ZZ4JVS1U3=L''?J@U/M6/))*EFMEO"%1#\H.Y^6(F7=1 \JQWZ\.\=L!G@5\"[-19=Z.!''6DE76Z 73C:,:HS<\CXZU[>C,\%"RE#1"6P7QZ8FU_T$\3''KZ,9Z,^>;ZV^@M7''.1/ ^F#MCI5.5;NU''''C8_(YC8A5P,<5[D=?8.8[CIZW\C:KYXFR=.Z4UV#DPM !Q(.0S<&,SSM](2[\2HK* 99GCWQOL4]V<4 B<H":4GZ''J3)0PO:8@$ILP*H\@/:(@5T\PU/!D6N K.NPF_"1@,GH\$N$URQ\@.#<@?4T@Y=0@N-HAWQ0@_8\@=GE@&^\AF(J P"@@A/M0(:MP_S$-8TH[ T,\R1ELDQ7E+,%;''#<%)0Z652N>@J#U,@3''!,8M+.Y^Y#&[!#W#(8(3N7CQ__$,)+)S%1/CPC]!I6BTT*8 OC+QB 6<^^V$3I/]\/Q"4>)5''Q0@(V.A40V0& 0DKR4!UZ(C;2$@O7<@?2,H<5D@/BB1U]4PZSF_<D//L :CBY8 ,HNB@B3.RZO@8^/6FZU>@O)R@ODGHEX<KY$MMD3+''QQ%Z1CL+VS_7JLHI9X9;_Z68&9HOU.*3,KI#@1;;@/6# $]X9VKR6G)6 7X)VA(T(*\FLC\GA,%_W]75WHZ/W7NDIU''DE[ D@LQ\75U6%[DA''EUHF0C,BY^@HP>R3(F$W.R@J[9@PL&@E!-^::H@EPLPF3 @%WUDW-XHVT+.*]AO@K:N%$P.&-KF6DU/GLBDSAG6R<>_&YO;,]AKF[5W2%O^B''I_PE,6(FP5?I?JVSJ51%ZX[O&")8_C''^53R0GS\W0OH((+S_VMY4?I%48,%*\*I<WR$ CC?Y A LRLF<_G<ETF@AR  AE<8C7FCBZ< @EX )V1!AVV V_= ^N&Q@JCI@4PI0EL%NR-B,]/Z7-QI;D?=5QR];B''_/6102;TX,O>-=8_V22M/EI0''(J"1)Q-OK"?Q;WK_ZTO (ND^:MKL,Q9+3Z.V3]2!2]7U@&/0A$]0C5( C9T BJW C;PB@E''RC;3@A&" AQ@OS:_ZDJSIA,)0OE(0''IA&B3# BE)@%C&@A$M@B-9DNF03AH!3AK; BEOA59?TKYN".WW<D*Q7Q^-''3MLV_6G>QRM_-8G?F@;-?P ''4Z&=D7A>R#Y8C8ZRJIP;''_F=V S61M".4S(O:JKV<4"H?=KD-HP";A@>14?N@AA._,DZ1VX\H&]Z7,6BE^7^K3I,GJE(46KHO@A,!+PA1L[VDC[76KAY 2$Z@BL@<@D(9X>M(D![0-!"L4HK CH8@K"<D)IMQ)-WT,8[:[O(TZQI%R9]N 9'' ''@I! AJ4B*A/2EX*V*-^#W+5J9V,V(]F69,E0B^.,Q;!0OG.;Y17<:UV9_.W[-9Z0@P1]S/W<BAAP<&CE T F5HF&C+XZ3M,\JQIT>&WM&2@PBB8.[E69''3Y;*[7:HA,F^^%REY@Y%-9V>5O;N/V?,S>=U_VJ=\+T[%CQTL @AG![T95ZMALV0U6-AA6*>MFLUH,B4@X@]0*?>#F[@WAM$Q)LF"<6ZIIK+/JME;H+<WGSD26$!@H$\:4%!T?,#1H=^O3OVIZG7KIA,''&#H@2Z(55&#3A3XEFY0M0PTQADQA,:YZLJ,54-*LK\<:AN7C-=9)A:\@R3S11JUD, F@K7(0+ \NG$M1Q!)++J0W8MY B4P^O\Q+#RWV"N.X4.:1P"+XD*@*J OG0$7A5%Z[LI3WFH2MR V%B,\G@M@8R)@6*(&. R?ZJFL$^4[R( 4O")EE.#0@:BT)>][C3,AY"-+OJH7$&<TJM/J4@#,6BB6J4OSX FZH=>PSRRS,@CV/O!;4@;@TY:8PAX\!V$$BPA.[0&''BH[1ZL+_?+I9L5Z+Z''F0-*0+=^R^-TS L4\\^\=6++5A;=YT)N C8PKDFF)O1U6RSQWFP3M9YH%]\(9V+"7_":XH4S)I@#T$*.5UP- !).:*+*E(EZ;^.,O)#''->BJ8)HK(!-XI,6.&''..SG3ZJNT0 #ML5@6+GB4JHEG !P>=] (JNB$PK)/IDH+N\+PIL3K\8!?;^QSVXN^ ''UJJZ$L^\E.)XSM9MY&F8-T_4YIB4"8NHQ6Y+1DIIE#''F%\,\TWX1P&Y:BC? /G@GR$>^#P5,AE2K^HMN7HA%]E=SY40S6Y0"''C3Q)VK+5<M<01232S#SSYVELLM>F,S+@<G0;)TY?>-P="?86><1M ''>@^6K8:Y9G;''%[>^7PV.3<U^,@2J%S5-*#FN!_UT6-#.TI@Y%VK02BQ5''1W(S.WSJQ D5.,!2&N=_14(CGS[F''M)Z795''^05SZ5*18L520(WR/WUJ#R?R)2+70CS+ 68#4..^W.!Z8A:Z"3K+AD44,"4[33%">I?O9=VN>?*F]#^(NQF"=;O*]7&6N''B,2JPZ0SGM%A$*?F''_HK?\''PD2BE1JW5(64^F7T@?&U''K")VM]( "@@&DE%D 1''?WA^W?K5#ZPD@PC5N@2.*8ZYI*^KM!BRT(@Z%SG]YB,\<VMF%K8FM^VP2D9+T1JY"=DA- ?"\W>;#/_M1[?=.N#SX=-16C839TF9@R5=WG"^97)%+J;?C3Z,X%AV4WJ8-,G- E^/BNPU&<R"GFUXC"%T:2F!Q#L-*%,2,^LZ6+HUZ(0E@CN[1 -Q8LG;0F1%UV.T;U46-]>)(55G IR=:6^,(3!FCKEZ(K78%Z6=#;AW"VFV%D[8FR2@4B9X ]KVU#^U>CU0ZF-G(O4X&TDT,H&@OCO"3TJY2L$Q3E-L\&J,HM*5H*HEJA#_H.J842U,&,6S;E/QD_9 P!U<S40+E9$J3OR^FZ''.^J)79%?RQ"''^;404^0]H865R%KE*17E*&BA]OG 6K3?Q\:ASCFL^D$Y3+YH+**/UJJ5*+V-_?@$B6TDL5B#WHV08ZE;)X%Q-+''"(\_R3J\H)3ONT0Y9C8X-945,YN"H:$"@_R''Q0''^[T(9SMJH,N]!\XR@P6E2H30?A@(H0*4@_[L]B^MJ@L?PEI)/RNV;7CZAQ.$IJ%Y9Y[-&5@>/7V%54AE&E8[BY"J*Y!#$.6E[W(SM.H4I9Z.D0(DT)0?)7YM\/G.''>WZ9%P,U:-/0)QG883*+4@''KFHYR95%WR^3U ]N,W)HMKF+Y1K VJ+\VZ5$#%-"G*6J+"PM]G#/@(C1/A!H9^U+W61=Y#SWY;.K4/E$<9,#%#JI(Y\=:97!-JIIF_.+%A[KY9=]I0N)FM^<JD5?Z:A ?36L!LF,&".C,:%$;''8J,''A1[Y #NXXJ$])B)RXS[T6MT3MIN<X"Q$U*#.O+8&3CQB_FJ(+^1DG&NC,3,!:7Q(\QGVO@*E5''.#MH5:6"F.MB&%I\TD&5_\7;V@L9 M;Q*&M!56AGL!3#HR^!"V6(<<BKW@H5RXR4%RO;7L_Q+EWI(9#EW>[6Q=9\^_Z?I1(%30)80@&G$(E. B.D\SWS&-JRR##MHIM,X2U@!B1+%V1/;*B2)]6>1K\,GE/Y5-P&FS)US!''NX/)^EU?''U!N0VH7.VK))*0:#E"?Y9WF@ -WEK::42P%4:50=WEI9V$-6\ASKA7<ZMZ9PD;):;H5 7_?E6<HB,%;JL:Q"DC''%@D:4R)D=\MXL+FKK*&X,K&,0WC*)9K1HFL:UBZ4)68CJPP^P%Z2;<(_GJ4/W7''L62<6&!OP*/4 ^VB(INBEQ6]C[(=HX&V][)(:M&>" HR9I367.$I&KUU]5-WI)(^9)F:4W/*BZL&_-+%)5CTA77#)Z9&U#^&&)SQF*VDED!.<?&^.;]Y703F00*GG4&;2EK*=9C?45DT_5L_Y-=I\P8.U-PZ!''A&<H4I8Q]K\GT> X+]W]PFO #(S=&P .#SPV/J]Y E3"*;@XZ0XNXTW=OXRAQ,OSQ 4[\F4LP>K.^M88J2IV-G+ULD<S#=+DHAQ)!^S?]^NZU1L_CA_S&$:RA:7J3;*7!>RI@7*V0*913E6B3)516XH%#="L-''4?''VX/3&/M6UY,H%NNKEUSUDK,Z=A$%W5I%R7X_)&MV\!+M*K?GY4)ER;%ZKWN,W)[/R823Q1["AS)T/%[)9P&25;]C+JR8R::XEB8G6^\5H\O%9%_?9VOKY;AU;_:75O9WU]%QV/L.[K%U<057?5"3-E=5?GHD*?"1S9W4+#1RH>,KYS&#OM>:#FOE >G-H&WW>P)-B"D#H9CS359ED44+;E)N  !IDJ&18)%_L:76DNC]] /)]@FQFC0^;W!IB?>KZ*]Y8#WI:9[=&:''@!=00W,98!K>?6[!]7_8T$,]I1(Z_4YSK\NO\=9\LY?*5YL3L.KC*''3F#53<Y8T2:ZP,_#I*1-[+!/''K:V+O5M@3.\N4RPL=0L,&Z.HM $H301H:PU*=96"3A'' 3?C.QZMJ&N<L]):.#.GNM2Q(K56@0S%(C>O.=&:%@M(B7P=.:#TFQ^3B\<J&QQS*JV\ @)E"O<4FQQRMA78NK_FN-B/*S*4 RY6L5\,$Z65$:R;H(*A"B++F;TLN;E4J[GB,.ECRQ9DHRI$J?7''$66^J85?@8JWJK9@,9I#L>;.(A9K@?2@B_/Y&G?>"!D)&G*#"O?O ^;=&5(3"_@@RXV_@G;Y$[<D$Q/G''A/?<H-#I\/G$:+3Z:(D8YHY<R-2>,&"SJJ"]I@C;2.V)CJF2CPF53/R,,D\Q)$(*BID/[I2VT(:#;)WPS*Q8LD^B+PJ9;DPLJ(1,J"_D11A1D"!%T"$91FE2<!4B\$U,L"]SH#>"APT\Q1%5<F>/1";A;Q[I3IZ^9A3 JNAH#LUYCL]-ZN J;D*G*-ARJ0.9KI*YRF: J1\)HG2()ERTBJF32*4(DPX+J"*<B.QJL"3LLO,!C)?.K&9FXA6DT#DE$P_D1&C,,$^7AC11*&C6!$YVS1/:#E"6+(M$IA=W0E''F;F"XR,82+)#I[0I=+PLP".$MZKGU\Q8=A,T%BFU9:RW7?$*O''20+^4:2Q +=6L38UQCP:FZJG-IR9@YTX)A.BZY#0Z\Z_OH*$5AL@>Q^WJAR<BQ06^L* OL#/>L%(3L_%J3.Z&*T!SAH;$#9V@1^=J+\A[IJ! ,JF*;D)SA-SR<%5IIAUB4,/;DI&$4_J(Y0#Z0/+:,E=]C3:N;%#>Q\@*@F)X@NLB@%@&T)@2\E;VI VQH*L>@8(&H\6&@>%.LWEILRQ6D0\&H?;2H#0@X@!&ARC@QQ?DL48KH)?0@;,H@)@.T(>*S2-!H-E!C''YPX7TFJDA)J78.\-,D+2P5LS"TX1+T;6QXK7>8#Z8#@3G<"W[0[S+.3RV*Y=M\#A)3L'' ?5OA8(OJJ<@D(<"H_5''L;9 E(/ NJ4@DJ5#L3^3L(!AM<<BN#O"O=#0T)U1J#X@F?Q B<B3J''=!C&(@O4:1J2RRJ#  IDQ I*#2JCU.#G(0 ,:. 45@R:DO@WC(16<L(_YJ_IT''K(&@8X2+GF?,>"V-N972J/6L;U,N*I/D]N1(KI9(._OSK1$MA;#(''M(1A@C@AEW!O(B3OF1ROR@"TX#2O_8!C$W@F$AB\>%"O>E JGESJ;9''L=T#L5A I>DRJI44J%4!L>2SMOPRPJ*/M58$9Q90_U^SM(B)B@1230AKIO0*:$/QDBZQ@D26LTZR($X%N2I(3*2%@::P:B^KA_O?TS,ALPUJ"1QV4E@BXAT0X@$A) ?<X "]0@F@(&E^HA@C@AA0@!INHA/(@A CP@)\@"WGP $#XB86H094@EQ18@ A(@''2@UC[@!OXX T^]AR68@&B C23=C0BX!2,@ B/8EI@@"SIX3@Q  7<@!G''0A5GU /U( 7?A!CKH!W<PBYT8CG38BT<PSX''Y#16,SY%"GW4+C_U"FZ$1,]$"&\>S1E=",]98P&IZ25ES)")$3#,UCK=K.86SK2@C6K3$I/\#P6''<2<%;,+P*GPR2A9GX!3N!!4SJBG>8A23 @R-8AZ": #A @5IPJCCE@""(AG3@@$KA@$FX!5HP3ZO(BZC?/@U:ZHUW8HON_@M3@@P&PHL!NHI\6@]:0HK:DL5;DD<@*IN@IEX@Z@N$/P]?J@QNZX]D,(K7&HT#^@V32X!;"@XU<H]X(M''?!A.IJ@)D_LVI''J_\[DJB2:#["9B@&$\7SS/SJ2#",K[T8:=-^;5=!ZZU?KK*D:G)-C8O''D%?@B%OTC]2#TTTGK9E=]F]]PNO:H:1*XP3@PQD*H]X<MV,1X82LH=9>H_9D@U@LL=(LL>A/@^ZNA"Q$I!YBMZQ*@D-XHMO(H];FEH6*@RZ(H>L@IQF$PF)5@ CD\7/\E%''4H?Q1]JQ(HM%-PJWAPBI>\==&E* IIR0H5\PB<I;&)C(?:.Z]T5S>CF7D^(]^"4*;&MK\7SK)<)[0%@5*>B _;48I''G[E94J/_28=:MQ>SL>W,MQ%A/D[CVHI7"A2=Q]@DCZ3''2BROBD\]A]=6"B@^8IPAGM5CSZ3 VP6C2F^)BFJ;!JAIXF(/"G?FCYJ>!R()7J,A6IJ4BT1KRB_3 I''@B"\^"IU/BDJ6W]''!"O@AWZ(VRC"L3NKH.9O2&U#;&R ''L1#53W#^LML?L#(C.,(_OD($O_0_@1YMOPC^WFC1*Y#&KE*^.33^)AP94<EP0#;A"BL/ WKI C31TI&) G!?&OVQBBL/;OV%TDM."H<>2CR!"GHVAY>\#]NLRDP,@EGO=8#?+@ACQ87]VEFO@DB]F4 /&<SAK6T[&Y3CHFBZ(T3T  B--E3H3@7Z''4WO7D&:KH2 _%2+L3D 3LJX0KB8L[(]%8RR3QKT<CMW,MK%JKNK2EX*V(NK0B3/V+QOT;E1 ="8K-271DVL\3NR?2-ZN VDCQA0BH3UJ0@$@0@''4H&C;(U#^8@*JH!''''XA6A8@R, !VYX3C^@CCBAR!=%&EJ9 ''G @T0ADBD@ E\@"SO6VDP8S9T@ACC(BTZA@.2 C?+H9O6$WR2H!7% @$S(89G@ %]8@Q0YBU.XEMDD@ON(AIDXH-)<QVJ#*37H K-*K5ST& R11CG;0* P''&''CK>O?+E.S/E-]C(0<]U\!I#@B]NVX+I=@[SC=2T;D''S=D-S@XV]RA9HO&[PU3.HU?6P]DZ@MO\E06\@MXAPCYYXL#&L=&\@YO*ARM\@Q "HX9\E$@FVB\FF@6L@IQ"@X&"@Y@@YQY<]UVP@P@.NZ"HHV$_XW"KXWK-I._(EW#+X$\@H@''FH^":H%;FH\''0@P&2N@=5(>LZ@V=!$EPZUA)3K]Q6C_5*)=U=$"N*&JM/I50+K,OG]=;1[DY"&''@ @J\^AV]*3S''@);Y**U:OK017K?EP6Z>:<\\CT R-(K?6H^C$]I90H;;0H9Y&IS3I@>#9DN#FI LNMT_5P?WKL(!RA#T?>AE1TPJP)GM(* D%1TII[!A1J2O>1#S;K1M."*EG0YA7D-BC9PP-:7KY0.GIBX,$&3"9H3@$3P:5AZUL$@R[-2''7(2_3/[M:-03P_63P/5)?EO\8 LIP6FC<OYEJVWJ)@S[B@^PY/PSHQ@D+"YPDI_R<LYO?@BPRH"EHQ@DQL!L;PYJM-#A2#[%B$I%DP*2+MJ%U$ZY\G&P7!GG^ 5Q<"V5_L7%?)X(.U3_Z!IN>D(?LI3_6$9DPLO-(0/LYYZ</= S,AUJ<V91E =HP0F"Y#:BR(CM)J#OF01.;A[/V3RBRH"D_B[OK$<JQBQS4ZB''#;:+*: R/:U)7GK-<0N(XO>J6?.ZV49DS#YPS) .\/=>IH6BN04-M;KL8)46WB=&\OESWDSK\C(I#I?L\*A,38\YH*V\;*",2""-#KZFR*$T]Y?X#=V\W"=&/ "-!?VNE_ZM/-7!%*ZC.!4O+HS[O''HL\(!CQ4W7;>AAH"[_J%VI''FDVP;@26M*T\*6;4]EA.P(ODD6O#E6TC>KN=!O9RYA 36Y,T-;V8V[QKJ4,V=1,QE)R&[9=;423)-!>M-JC42U6PK.]PIP,]*Q@GJ9"DD%JLL(*NM?LL26>3*(;7ALD:#ANRTOLX\J@=TK=P]I8F+1*4S/Z''[U#;5:G''6<L11"#YRB?U;5;R7;/=5F1.BK?CL,?3;''8C\MYF4M#E#-)O3*EWVY+I2]O19$;SL#B*C0^)$!:@,C]4B"Z#-\9>*]7#L_5T^''SZ>''=^&%POO&#RI>4\?R4I^%./A6]?*#L:&&E3;(FS5SQV%15YD[J(N3Z+M:''*Y4E2\Y?.*U-=D51T3X$Y WQE-= K64RST^*M0"9ECBLZ64.GC4K$U>B__JC+UF 5&4+??.#LF<:!;%72$7Z6W*:5?+UHCLM:!63*J=)"5L&_,C<K*S=!''2I0 %*,!63_M\=7UCAQ_!AC_-P]GCTQ8)1#WUF^0]A&GD8N)%JYO(C[M^M4''DTL1B(BM=OH65[1-^=P?4KOAC\XO*>?8H5:6^YFB5X68921,\?YX82^TO=,RUW6J%8Q7P:\4-SDF)_:/]<S"ON 3)N.95:2O_7V,K@/6V/6(IWBH''=$D)8 G"GP2CA PXKH#2(LBGA]>4@%FD#\RKE"!X/X,2(\ROG#!8+7&M#@<BWG#4Z=J#VQ-#G%"9_0(0)46L/@@GV1G/G\BGO''S8O+%&29!6.MV @5K-''I\DP_8@R!D/PB*+TAO:ZT VD]V(+_?;B>^/*MQ0 ,&F?>(LZC$04MAPE-Z&F)LG\K64",+D''TT,[C<U$H\FVA4B/&XXOH4;\\!1DPE_K^!72MN+T*8>+W*Y,]Z/%+$7_@_@4J"^N S=O=?=LS[@F@EFJW<M>SP_@-+''X^!!+\206;=:>OP8BHN#]D-SFUZ_.H+CK4Q!I+@2)CKZ,8>% .6K76!V,9Z[^''4:MZ#%*NEX6JQ8C0FU. 0[[6''R+6J>M&K%3<;P)=W,?_=:L202AVUOWNTW VVBYQYYYWUDW&U^V#QIZTC*Q])2E*#$DTW<[=#]RRR_50HE.GIIXX$057UP!\!^^=,8ZQ[6ST7+53KJT %*%MQVN@.ZXUUM''UP_YU06F%8A9[$54SE36MV@WW''*10Y\X_0T6&A4&X)''%Q^MDT4Y$ C@(HG!"T*U#&VT>A)Y$(G''RAV%B,[#""*29)*V]K8T460_,8S[?8)5?Y&&@\LPMIV^\O#VD@7L@1C@O]@D>.M5Y$#IHHGU]'']'']&NJEL0<X944$SA/+MU@LM!V4PX]<=M&G3PH@W@&(+O3=E5:OBZJE%''TM7("V$C=Z-(ZD\NI2*JD=YX#W+L-^9JEIJH''HD+OS=(["F&8(]:21B@T5%H0@;NE(T9:%-ZNNVF%&VY%A8"*V  %@9PL@RD(D%514MW&WQEC29U\OUPH0BKTCH?[_Y)AU9)6&OEX%''&QUNSZ.YU6D9*Y@)&&[LTM4D$2-JKSY5,LT_''Y\,$3ACZ^12#-%><9Q''C0WXJV9\+^ +0*^%^%7A5LU3%* R)P^*^6=E1=E<8$!?0.>>N%'',-L^\U&FU)/%VJASP:IEZXEBH."  <I:L"EQZ:2<\+IO?1&R,2B*I@#Z[7^D8-!%G>("#K"L\!RM4E$UEX8J(9/@736NA0#BX07IWUO3GH''^$/ :.^=^=AW3+6B00)59QUA@9M&BUGWWX<JS<_"0Y9B-VSE)FF>+L\^ZY:$''''2O/A+/-D &Z,$94Z42P1T^U8"!TSQF9=^D4N8X]:P%_M!9T''0Y@$Z"''''FP***(^3R)]+&I>N=0F?<#Y3IHBZ]ZA2^\J[ELQ!MZP"*5[^K[7IJ:M$$+R3)>9-S''A+7F7LWX!OW-PB/F>0#B?@R94:LIZ+1@T,Z?H"59,,O<W$2JW%<''9ASBWJ43>''!Z5,D %\\/#VYEB5;BC)^$1[A+M1W[WO0.=+(N?2QOHF''B[7MQN!&!CFWE^*KE8-D1/LVOJ5P 4O.5XKU^''N>GB2FL$(KEAZN11C71VUY>%9T^GI/.O.)"WHL#,*&ZP(P:.5N\O,H''-_[03#/24:I/:!X!$["3Y?''0X/?<A\TYJ X(A3ZPY9$D, W24FRGS4*%ORUAI=:*K/"8XI\))4D)3IA WYRYB''YE.XU2!&&P:L3GUBPQNZ=QVCB\I&8?-*S5=2*D)A9X;P-''QTH%2FZODQS2\3P2I$+JN5T3GL8^UAR+&"U:(Q#TW::WJ"*5:UZ3?V+&,*NEHWYR1V".R12,!$Z=+Y$P#,VKI1(\(29&GTQ-I''!UG?H&SV]Y2 0-GZY0WE^(]@!Q^B@U70,DE,(-T&5SV^DVU1$6D )A+IK?:T#&@\SB] NH"U? Y3L+4#F)UXY;)OO$YTIZ&''];,RR$UN)OY5LZFH)N#Q0GEP=Z9D3&J@"H@W1X3:R 1\VLIT-Y*V,JGNR0-:/@TEJWXG"(Z[RIHT=)=,%#RN?4''P@"<%SV?&A6Y$/EAY(1P6H(3-84"B)1GMP0\6;YUV\&-JB%%8?>J(#]1>T)!)[.U8KA"+*7]CGTP''I_#E)&/I45.R)ZS9E^3AL713IP,SLP)I='':?9 T)*9M*=O(VC''Z&+;""P62T2W-HG.''U?J/,R5":TB@M8<WC@ERQ23^]-C7UD0UZYO*^!8T)5^=T2T3^5\LCCL-Z:IZ!UB--#(_K6^6(N)X9&/-L84Z,W*PM-+VH55]RWJ3YJ4OFA]Q^@0 .@"X,:W*#J4#MIAXR&/@S F4W(=#IE8_&TF@BZ29F3KXXWE%P((>QYM($%!SJBZZ1T[W,WURK4\>E-H[$)R??NE!,_KKD@@R1JVOP*HQX8*30?6J\E.I:L>HFSS5SKE(2,Q"40S<&0=BZ,JU2\;1.''Z &W(-VLN"$HDW %0O 4P$9PRQ"E I8==XB:T--).77"DH)O?TBE;A7MERI:3O)T*J#N#CD^O(&*S17%U2#92RA&-;8=90+ 2^69+"6@+_4(GG(/9 $<U0HL(]C:R#U9;HYEFB03W/99UB4R127ASDV$H''GC>*5IH-U](0_"&;@6J-!]$ J%H!D7-B5]9=B@O''6HCOJXGCTW^93NB3VJ7KC*I*V]G\DJ4>&"I0#EZ(*6VSZ3GV0FWMF;!*UDCM0M*^QSJW#9BH/)(YL!0Q_@.TKU!PJ-LV@A4.=VD>VK!BL$25/!0]X EAY/3R6R]*/#HM?3-RF1L[LR]M][QY=3,@\DI\QJ14<[##WQHSZKBYHSQ:LO1SC\.6UXMIZKY''<!=<3''+?PMSTI^I4A]1-+/"*7T[H"1>=WK_UV3E!#^_@ PJ''OBIE^M1I]&D)C&-67_(*<WI24G)M4K26Z!]FS[!L?!NUG9$[Z8J$*G3IQR;;%%''GCW?G-M^L2-$E&NP1^^WLWX3 3 (;Y-G!KV?Y]VH  WC] OPY=JSWA.*UB+ZJ% #R9N5(''\\D&,SS[UY0?]Q]:T**X ^[>31-F%BG&)0_,!=3,V:XN)+]QV_VH6"-H.F;F9''+_.L.5(*4:8A:/K3=B/''H7[8X"HCP.8C^FSCA''E.4IIZEFX5;3Z<L4)B=6_@''F5Q1^''9 (H\KN&''*,=VN9=/"9S680.2).8%VQ]$R]XNZ?7]I)M\JLW^A"^"U;/HC6U]UE''/>;A+J=*!3O/.8''[*8@>_16OR610R]LKTR3[]6F70 -[@%$XFW,)RJ,S6&G_<#G02OZYE(0GT7K><OUIN3LW)&35_^03QL)Y.MG7:M2I''!N4;T()33JJ.8"?GX#II!V,(IE,/1TYLU6*D]$=Q-VJOQ6?5%QBVATIFE3(O!F"G=6=T4AS\MQ[DD''4: WZ$UG3)IHD^,T<-8G!;ID9@-Q\:\"2E%537)7[:]!UI%G:?Y%Z=!4DDIQ.F]8DU$&\H\7\2,E^NIBVCUE4[I''HGEG839U<!TU!A61KZE(DE$R??YT$4YD_(T(P6J1\)Q1_<_JFB;CP5PVUDOE@O@MALUV(SV6]9R%P=\N]@\E%@GA)1X^U+ATY.L+U5J-@GB.ZEFKE0HL->7!A8Q$P:QSUSE@QK9WUOMXE=[5ETE_]2TT\''%)M\ U,R=GQ-0YT_ 7MR:SYN>IU[L\U-4OZFG-]&5\RK>CT(SK!?FK@+LPH]4YE2%3^FMDH! NQDB%$]+O]5+W\<CM C8/RIEQI(>;X ''9U9TDZAXGIY%>E;YF^HH!-)2&V@2S S\!VAYFPV8(MU55E+4%VLL!)CMNMS"9BC ;^@%=$,CUE''7\BL[QH6Z>AZ?UZB )TTWJVG;7U\KT]9#%U)HQJEHSVD=T PO]U;?7B%J (G[DO4H!E$J(NDZCLZ!HXUWE@G@JSP * PU5SGZLU9]O];[!ND[Y*##Z_V] 30PM0G_G&J#0\4X60&"P$)DGQ$"O@DP7P6AVT1X.!!YO"VPSN''"S09A436YQ0:T8DER%PC!J5[RHIT[D.$]I26OD/(C3K6I?,7\J +X3UFV?[5"?,W]KA5E:H%VVAPOFIV_:S%H:#7LU2BR83B%=;7G5KEA5W5_RWJ#5$&Z]*''UE=%L@I)OI:%XUW7 V\8$''B7W[(PDS+IAB,[]?02D@ADPG5''E&U" N^*S)^7^<@2SWU)"^XTB@O!EW09[L#HTG;T$XS9PTFK%8:''I&CE!?2)FF5 6%4GV4F4$)FQZ(TL*A1B9EF"M6A1FUT5!410>X-]]QU4&RT\" _^!H^1Y#)VYIDPP$Q"ZB_%AUZ>P317:@:]Y%Q9:G!>.6T#T S%9%VS&)D4H7O;19FT"!WVE8YF5%SS%I066)C=E1V >6S,:9YS4YPR>H!Q4B_/QS@D*$^7]5O*)4F4N9G:U6,]<@QJ@2F?"IL?!I%Z!)[A=U& AHNJ 67K.$*W-(?R)2TX*2U5V9:KU1;<\X5Q28$$ZTD+"T4U"&/I@''&L$)#WJ9O@Q64!,04&07S[V(3]:F# :''>"AQS;1H5FJ3(#&G*Y9A8A6''D_J /]Y4@V]) ^4A?<[8"Q;RXZA''E"%DQX04.X''A^S$!ZA.*-_G5@FFX$LC5A [QBYNL*QCK,]2@D<R5E5T<VI NP[W7M(F (;B*@%4R$P-OM41MPB*Q@RT;BV#\T<[,&ZWSH4>(^N,5REKR*M80&RLLJZP)!4]-DDE @ RUHL87BQN:.P^<.R7I@W_OB$#9*"T<&]6_AD_X^$D-TD&-@\7XHM]XL>/Y^I [NI_],9U@M+MVJ''RN^H?O%,(^ZTSD"R1(V*QF.&]3(I;&*T!,$93!J"N<JKQPRON-FR%SA?UUA!)[,<W@LB1".Q,3^!*R*U6]!IVJMA''<)MC*R-"+,>0KNX5&&*((Z***%K?MXS@*2($YW;#05GW@H''^_T[)JE8%0/"''O;ABMKARA0R+=7E)[AVT/0P[)$)%%71W. KW&UZ\TDK(JTJ[)<F)YTW&@TRC*/Y@''WJ@*;KA-0I''KOK)P0I]OS0GT83(.)H(/7EMR1JV@''V,=#G%WD"*EQDURQ)(#SXF5;F\V>''[$CAHM%''FR>X!6Y#]P=1BP^I)0/II[-1$''"Y#H\+*7Z1A@MR''$9XK5>Z''[J[$7)T^^P1AHTRC!]4CWFQBXERN,T*NO_RKC;8J/''I"%)4\\KG,/2I!S.4+9F6%!H[ P>!%G4)V*/;K/7#+OXPD7GH"!9;%.MXRZK6Z(XX]UN6(CU;%?5V @2V:(<!FZ!,\PAH8D%<"5I!*:.NI&DQ)A,,Z8CS"(R\@:W&2Q-*&''TRHP!,T*W7\3<?F6B,*:P+NWWWYJ*_*:"X)&3?ZUFS(FU/\IL"B@.H" U4\P@[8K!,$P[=,JUQB;"B6I(OD;)<]Z"N>%5Z.4L1:W&N&91TTJTHF(''.>97@49ATN9:HXKWQDT289%M"ZG9)^A$T%@C)0W@[X"0697 EH!N?^@?C>8O6:X^4]K=\]6]H^3=!BRLA%%*\EP[[B6S9HAA6H03@ @R7( Q2H2#G\031H!C=PAN*^XD7(3$;BBG5R+F-RG=<&4AQWS]#]2C1 0[0L@Q.@@A-0P_<HSDDC\HLE:HD.$HDM,LDV"2P8&LSIB&<Y*D$C(R,.1^THGYX)@&RY3U&PT."U/PLV6DD9-LDL<G@O2LDC\DD[=DL-XDD!*GD=%,J<%LZ53*HKJU"@@@./=%.R+RRJ]Y$/>PLO&HK]B,ED7DLG!@@?NDDJ\@LRU@@@Q@K\Z D@]LII.KEBT.BX3I*:SQS"CFA-$*5"D$VGM!XNOLPV!A(:WDD[@@@Y3@@1)@@L0L@4]@@Y@L@5<<D;)OD#\6H$A0@NPE]%D$/]..AOA&Z-:^5''T/GLTL\,7HLJW@D(@HHURLPT$HDB:@F1:$D]$HD4R@PQ;2T@,OE5@(@A8NPP@%[?WHWQ+\4&+F$%U;YPBK(AZ73.#^$C@E1AKS0BBM@BKS2@GL#BK#PBG0@@K8RDD$!DJ6!/MCC0EZ;T''SUJ7WUW1!WO)ZF[>X$"UF#EB1RBK[0BHHPBGZ0BG;SAB]BBFN]CAU0AFTRBJIQAHM3CKM_2="0@#^J/),(L%J[K+.+#]@QWX(*M^^8_OQB$:UK[DHQBF6CBMF3CFL!BA,#BC^3BEPA@FE DM5N!D/N)7LTSY/KM]:G3%G[*N^+4@@9AIPABK-#DBE13IKRANN2BGKRGGO3@@VC4MW-APG-@F5?*&C[F6C(P^H;Q#P!Z&MFW[R*VPHZ DS0VD''/XE&LBF]@A?24(0 6X R0<P"P@0B:X,$IFL!*T1""\IT,QY4RJW"_1FW^T;8H*FW ZK9!X R<@ BZ00R.,@ BD@!@ 0Q!X@@0  R3<0H=E@1Y\QRL@P@7XL/]$K_;*:86&G#?B9Z?8ZEMPU]&Y''SG[\A>60#5T@PAH Z*F-C"4 U4+,A7XQF%D0AUZY ,&H,;0)5+5:7.SW2<2ARB@ "6 @2B0PQ#00QW\@Q3P "1@ !3,011X,2-L1B0OMAHP0^N>L_.E]*ZUS=ON)R B%%PD\DR[GV.KP Y$&3?4-1R( P:4PSR\PTWL0!B8- S^P1K4  2DPQ^8@S.I*2T[-8#.6?EPIA6ZW?>:LQ, ULJO!PD !DP''MKL+0LDX GP^$CP(8B$N=0L-)3]6L*/T^J5 8#''J"UB(@"U*)1D1^1M+?<LQFD@/;LD EG(/IO*"=8J"M3*#N7*$P?*$O7*%R;*%T?*%Z7*&\3*&%8H]%HH+XLDQ6L@ -0DY_HH+%DL)=LH JK*+F0B,2?*+47*,5?*,67*.8?*.77*/:;*/<?*/B7.0D3.0@?,[\DHY3H-@#LIN#+O]5,L88)6WD[[8<  ?C\D>V@DZW@D/8N$VA4D[AB,H7H@N$CP+)GFJ=0U''/<*2I&NVS\WW@!)\7"\(O18RTYT^&>59$,H?@H@3QLL5B?3@D73AF?3?0RM<0"/<0#M<00-<0FL4L4_<MT.<[#/<1VM<1&/<1''M<0/=CMM"BDNB@F50!!]1Y<K!.$MC;5_1YSR7(CT+SVAPB@J@C''$+DO[A6F; BEX ;JDR&;>;CKJN7F+*JUZ^02$+E8AP/@''*''53$H.[AI)<W]J@ BF%#=LVPAN% =F&"=5W\=56>=58\=6H-=6V?=59,=6)O=6[N=6[/=6*-=7K\=N  BK5S=LZ@CN(QAG<BBB(SAK(QA7*/=6AM>8Q/>8RM>8"/>8#M>83/>6Q\B7P.B@H3CJGPAD?^5=2*UCJ))]>9''CN*V+XUV8@J@B A4Y@YAMKRAX1\BF?#8OXRV?6Z3N37VNY#(=N3"V($5BNBX-%](QS!TZ4YI]@Q "4FL ''AG0C, /?J?P?H//?L7_?P/??QGO?P''/?T3??U''/?Q3O?Y;??X???Y#O?I_O ; @ :,P[NW1 !(00\L1A)DP@R,PQ_L_?SS/7H$??7[_?7''O?>? ?8CQHQ7W]Y4^R^P(DFDAP<NYK!P(\NHBQ-R!E#1(TRLD3MZ5G!1HYQ7;4#!"N_&@897J%&.]M$R9$.YLVE6"V_0I!(@''IIXF^I/BJA0P/4Q[^WO75F%?,H!G\+TZMF&P DA-])4BJ,6ZM#LX#NO#QT6PPBP2\Q&BA,6>=#\X1LJPH4^OYA V0C@#/=Z/W/9=/W;E7A 0XLIE3Y<&L48@FTR?D3PJ(GUAHA>U)XLF[IU*%B[@"TZ^QP@S6.V#LR5Y&Y*&*)Y,135^)P[GF90+[[]F/]-7[%9;:ZI@;"[P@QY&''R3Y(7/7,.UM6_>7G%4%V9F#A09B''+6%*QQ17-7CDB=^UXROKU*>WE$1>_Y);=\?#GR(>WC>X &JD$R-6J-C@@@8H)+5@IK+78@@F\.IH"8Z1CDGG0P0  %_GB\ZL+@""''N$)JJP:(:N2(\=R:3B*& 7!GM))V"448&WM2P3P4(T@NN1!)-/AGGGGW\$\\^__S1''SU^F0&G6)HC[(58O!#!12Z]_AKJJJW?]GH%T%2JY0'' VE11)I-\4*&T<XXXH+J#N&3**J"H6)@(C=-<2+E09 $GC@CP\H.MAM@BA(AOF+%"-@02>D(-@>U*(J:;R)&P4TX]_UP01\* KJ''IGHL,QD+S2?SR>JCZDJ"!0/%)C]E\^,^;K[M;A3/+7K@N5%!%''YWVV&6=E]]\]Z5RIUY1JD4%HT\9*]]]#S4V6VRUWWYVGKSAQY/-UE6Q.7]0FTV''O\S\\EO74LOTT''? @?^=DMLL\X!0V@G@!2S4R$JK_?)(Q9ED++ " ''C4F$HK@C1H4B8@F(RT8HHMQ$3R\TD5D4PO">K,L<<T? &)<+(P[U U)74.G.2L?1,% "%EG)''$G77E!\"W/DO-2II]_!%&&FL[5.LU[YXIUXMT4($''<"!F<6DP''])LUJ**F&(3!,WE%B%5(3%FK2,P"VZ4\>YY(9D6OBF%$ HADDLV.!)X(H5EC38[;[PS,9B22L[5]&$Q8027ZOM>F.*''D4\+C]VT[?9;X<@EC12:0P4''?OCDDU=<-RBW:N8]P\J;1:_2M@VW4$83&1/\3"M;**''2! @E@GSZ4$ZKM#BIPIXG3O CBU V\<M]^M,P@0%D,\$CK;U=?;5QJAX;J*!39]/0P3Z_H!8PD^LK*$Q?R/T$1U>SX11;1[WO''//-/^<^?N=/>1J@FNY9XX#L!/=^'',N) G[8:COYB4(,EP@P!H5QN@F@#614$TTNRCBCKMRPB@@8H@4I&D\?[)^;QOTN^AFT(FDR]+_4ZH)S9^(VT]1W(,;<9FI;P07JS"V><I7P!B%D8P)U6DIUNV9F70G@G"#7$3Q52%3!0"G\/"T"THU#G?\@1QW0!0#>GT@R?2-BC0J8@''7<@0C+(@\[T.\AA>8N@K6X8AZ9:I\J%ZERY&)J4YRW(XT9QVJX<T34)%\=&F#I!WE$8Q3%VD\:7/E09NOD>\[4NSR>2X1$UE*Z$I\4=PC"G$O@PA-@H[0+?B\D7=BEGN!R Q''<10GQ<LHB[2\KQCT C6W+8"";JJ''?.DDF 1/,HW,25I&+TD)/)CGM=VA%Q5/"DY^75FT.S0!CZ:4!@IO3BUP,(3$]""-D;\&!YL1U2G6(8@*<ZHT#M#FBBWQ#DOB@0P60<XL6QNH]ZQB@.0[PA"/*#'']9H^T:H6!JL(GNSY<BWRG-%$[M!B*D["QR#W"9R7?6D:C?EB!/U*J3]8@)B^#3%OKVACE:U&UCR''LS_B#SB''RT3"5C@@DK6"@LV,B#@_B(PC\ (B\6IFDHCLR];.:"S''Z>MF5_)MP2#X&Y\OG0^A=42&R&U:5W2V* PP7(THUZUAYVR2W *T\M''1^7CN80%Y.[G3J+4 )@-JHU=,N_H=SR@PAL?:DG<L GD"* -YOZP26)D4L1YEDW7&$Q)''D=&JRHI1V"+T&L!TR^_@8Y**+$CTW^P]JM"E)X(1;V,H$]W@$ON,O1.N^L^$5^<,8UO3L*1WIV/Z  V,DFI[B!@67 @B60 X4^UD@\_]ASD-B*T ^BT)Q2%R6$J/ ^2U3.))"33C0)Y)WP;L443UK,\AE[WNH^-5_U<(9RZ< Y.A7SLY%3: VK.SR>C,E>@]!KZC''P 0Y08;P!\H]Z4D)E\1ZCK%!4:V3YJ2D(V@!CX83JFO\*J+,.+:<6=H3^=H$#9O;W.@DFLD@K*)LXIKRORH%J03J$I/*^4W5U#Q9V_PD@*N%EDO=-2@PR:JJG[[QAF&)!KQ-T:,$G++^=JT[XX"#UOJ%RM6:8''Z;1^$,*T?T*U\HU<H8G3FL_\<>W21W&7VR,2.%J5<!/R1M%K,(U/XPVEA3.@P2>4H[1,*F<?U)+^''\!LAU?^\VL,P+CIB3O(95Y$L!,Z%5C&CD8>/_G\^;1''NU<,1I6HU-"<%RD6[\T2-9W0WW[*\P*+E6=_JLMWD@TM8+18QA#.U@MQF],0U3)RKG-'']7J(T47;SY2DS%:[ARMKG=I*3+SF]V''U+U**''VZ[EF.L]G[8W.P+E-Z;;YD]WI2U1O]@E&((!!%OPA946)NVQSC+QB4=KJ=.A",@II(1<O?:1&YU981V8V? "4NXU_];UQ?V<979%%O4%\&C!ER#F!^'';(C.Q$R5\''PZ#%FF1R@ABPT <)-FCZ$RR3)D3L[8G2QEI''& <)N)SJ*3&M0#_71V5K''FJ# %+"7JS;\%OTMOM("C:#P46&[J!N''OSQS@''R=WW+[.1#L>OB>,70;-"X[+ DO>A\''_E$MKZU!<KP/"^:F[QPUQ4W\''/#PJ594@.L !.O6B\GOMN43\%CBRH-0J2TC[9N_8-#\PDJU:\@W5782%FZSN\@%)Z''6YNZ9T 4JQR%V5;=JC0@"VLN5\&Y*7O"M(L?A>6;6_''^=?=49_Q<_8I\#^MXX/''FDU0;"?5OC>I 8G&\[N9UKIB\^<. 42Y9;:)DOS+GH3JO$^)%7MWIW#@Y<.@3DM*<G4I-,EH>=4,J;TJ2MY=<N_"*/;F-&.$(DBK6=83#@Q=TR8%D$86\I>[=R_)Y0<XG$O7?92&<>=J$/_^]G/?+L/7;6(3==;G=_>=2G//_E[?72I9?<8N?>=-VO_/ZWO?70_7?;"3Q?;,^_?/"?/?7U+7?:P;=FO*C9CD1L(D^>)J;A6H^C5(7*%.H*2&M]8(4M9(4KW*/J.,G+[*\G$B5 W ?6/$2& "JGMDV:N.:B#J37(.^7+BV8 ,P:TLLEX1@FY7@$ZE@FZ1@G[5@G[Y@G\;@G]??PA8LPBH_0A8-PBH6PBH=PBYNPBYGPBY_0BY,PB*]PB*,0B&70N%2BE12+\([")#[/.V(M1-:FJ^CCA>1$K4X/]1* @\(*=_!M+U8N"0[&@27M%I#F9!BP(V0/)2R,9=H%%)A.IT##U5H"R:;/>)POD[MDDP>QDQ4QA1HQD"T1D"F1DR,QNB[1DSFQD!\1D3%1D371D#/QD#W1D4WQED\1ED,QED>QEUF1EUWQESEQE&,1EV<1E''DQE''_1EW.QE''UQD06Q''9QO:R2#7XA&+7B.__(P=; %GN D@BSPA=+ EK0+]; .@5\*]<J.C)]-8M!N 6P,.%ZIX'';";TX!F"K?0RA*JPA1IBW\LT_^<T[$4T[(,Q7!DQ?''LQ;7LQ?+$Q?5,Q?/DR@G4!<C$$[,<RC?,R@IT"@W4"D[D"HS4"BA@2D)T"D#T!=W(&X"!0,-A6=,*5OB$Q052A?&XU56#P42#OPZ(A"00P5U+8)<3_V:4P;Y1#NFH*> C&!2C-B"8 ==;0("(VZJ!EU*4BAP 2@4@"&W<" ]P"&]$"JV<"&SL"*!$"F$,"*)<"*-,"&5,".Y4 V=L"3AD".7T"3K<".''$"29\"3U4"63\"7O$"7Q,"7!<"7ML";3<";S4";;D"?Y,%VF!BM;I,G,B,J(H"_-R6$":(=B9A&-S/R(4Q([?>@B,?FJV((&J86.8 L\P7I.WB3$1HUDJFTMYL@YBBIK[FH6Y(O8\HG87N@5V3L6S4H6Z]L6W]L6XSL^\EL7]YL7Y?L7[7L6_WL8Z;L7#QL8"7L7%SL8"SL7$UL8''7L9)[L9(]L9#7L:,[L:,3L9*YL9/=L;05L;0WL<1[L;21L=39L9W>PCW.QE]FJ)4L\Y.>WE0ABU:EM.&J$$S3HM66@%6QC5XMK%6 *KX"83T21!W$%C+,(0%SECE)L(QAC[(,DVB K)-HEYL%QCM9QCN=QCO1QDP5QDQ5QY^ T;.&@T$EI2=&@VJ&\)7$SB;*(3>CA)D/A(D*@<HEO^D,4B6?> :?[":;ZQ4 :4/[8HW\8NO''H(T=9FCL7$I:3@-8J2B7B@EFPCY]81:M8(:E9"6;!4R8FNQ+04SLDT##Q&SL64SM=LR<=TS\T4S[.TSM_4S[<TS^WTS-74S.LTS?]TS?.4S/''4S?64SPTUS OUT@_5TE,"IX@/M*XTA2:*EN;!RJE4!31G;\PE2TZP\<+CIMD0L/>3C]/ CU.N=]H+''X!T1\+.I-D,Q#.(=/(0]HX)^$:$CGB@A59CF4Y!H"O1H"VRHW75HXDUH7%5U2.RVG?UH(.5U9LUVX<5VI''5VY55VH6UV)]UV(TUV:E5V*7UVF%$EJ8EQ-0@SO3!AW1"G/S?27#JBFHV\LGV[TY3MA+7X!*+<YL*D4 5$@L1D5VK]CE"+W%(J&9DD"RSP%X79A6.PA+$8QT(X@;&PFG''0@.T0P/&0@Q2HVH!U&H)5&JU@VL''-&H/M&H=]&L;U&MAM&L?%&MC-&QS=&QGU&UQ%&Q!=&U]-&UMU&Q-]&U#%&Y/%&U9U&]1U&Y+-&]3]&ZA]&^IU&!?]&"C5&^K]&&O%&&Q-&&S5&&U5&*+5&''MXP;LHQ]^XV.1HH/NEUP8S<42=Y!0*#OK801QT"UK32UEUTCW"$A9!0;7U[[^:4I^R_\X4;8\A%[EYY"HSG @(@7>H1+>@7DSU7DW%7D[57D_E7H#?5]2I9]2J=]2K1]3L5]3M9]3N=]3O;]1C1^J@J@T+F@^4H#Z$$H''5\5LH.-EO> OPB=^H9M^+=EGKQO,!-Q.8:+,^""C1-CVN*=D2LLJ@FD^G@DX\.@I=@DQ&/\I&!]:(5]:)9]:*=]:+1];,5];-9];.=];/1]<05]<19]<2=]<35]:>>DW=DDKD@DV9,A8!6@^2.R9_!] 4>MR4_XY5;X?PSU@8]B\YEJ=]''^6SH''6#.^GTO]5Z?QA TIV;0D(6DCVB@9?<;N:K''"*LE"CL7!>PIOSP''NCP9"CQ[ 2OA ?Q1"ERS"EN;"BOV^EUQ"FQ="DWS"FZ?"E36NF%Z2E]??X)#@E^);4WB.AKXY :U;@%]KM3NI)95;77MPLF"VP@'',T@>=UF3.P FT+AL.0!:FKICGXT8@XJL8UR*E4S%*8!HOB!"=8"4-0#X.IL#0N_=L8 =/8ON#8#NDXL>QX#R.X#_''X#]NE!1=C#>_G#(F"$MDX#0W9!$O8$@?9#PN9EPZY"LW6WBWXB"(AR"LY!S^( )F44="#T5ERC]$0UD^56NPV9*9X+."JQOJ* \/,3=(MUD3WG8[IB!3AG2*AX*@TJW+9XW0Y&HMY&H^Y&H/Y&H<Y&YMY&Y^Y&Y/Y&Y<Y&*MY&*^Y&*$9@R(AJZ@T&2.!,2!&G,;L9.[K9%:U(W;?BE:%TSK+=WZ)>KV8TYU!*(I0R03-D<Y<]3HD^T32VY?7.SG:ZI??FZ@C.)<C&$0HV)<GF*@Q6*@W.*@MV*DE6*DYV*H[&*@_N*D#^*HW6*HOV*H76)<3F*L+.*MCF*PI>%3UH8"S;G_''>V3AIX_( 1V" V7=465_D(@=(@ELU]''^^Y5(+)$T<6GHRJ!3"$M^>\E<&Y]Q%;:@)#L,2;IP=5L4YKKP2IU7+(E]*XM&EHE;6YWF5#38\J<Z>A"9N*0SLIB6F**=F*K@V&#D^$X["*''I^ E-3)^;.''V?^*;[F!''_F*/KN*,UFI#-&*/WN*?O.*+!6*>W$Z;I.)][];FY?:Y8Z=$<%@I>>)Z>J--F5:P)YA^J4=%6);!@?L6J^Y*]^#]:=C\4?Y"Y0,UIH8NXF N6B3*6>3&6Y]._Z[.!Y=.6]W.7\=.6^1.7\_R# ?.6"5.7^_.8#U.9 1.7_Y.8 Q.:(1.9^5.6&?.9Z3.9$5.:_5.;X].:"9.;(W.:1=.905.8_].<!=-''D,@J+BBYSKAIV;*^2;X25MYS]=Q?8QZ@TW$NR[.4>?WZ%DXA%;"I1Y(1''UR_68Z$DY+ANS*_Z?/AH_*#0W/BA?*!/3/BH52!M=+BN[+AJ?2_LW3B_3/CP;3DRY2"N92_!=/AJW3DV;3AO33ESQ2=Q13BW;3DX?<\1$>\1R''Z16-;:Y:4LU;T%Q700X0<C:NBO''Q4@''%T''W<4-KTQ-,SN/4,I4>CC$5D;_3<.ICVDSW0Y(,YV3A7[SLBYF[\:3H]F6 )<*=G-%>,&J Z;,ZM:[ML<* $)3-,<ZM;\SYAQ3\><3 L[(/C\3<_V3P%[4A3[OOPJ3PU=3_O\4O\\4Q5F4\N\0Q*=+ _=XRB=]P?]WRE=4]-:3$?[L\=VT5_[8@[V6$JD_3=5CT-YPE&OPOV52$/I6]Y($H+Z,-]D"VFTL:CT3-U<L]FZ3CM=5G694:U-,.,:&IL=+Q]=&DN=6T^=,-B:*?W<6XO]4]DZ6:-]3J<=4+O]&JW?GY"]7],U_]*7.*.!WY#IG]&)7SB?O]6=6-"K6]4_I''5J)N6>_M_C&,FD1,D$CD_UYW[UX%8''45:#/I63*MYGJY:3''JW#&8L''8:\)(<]%?L][7L]A/LX1''.L3W*L5''.N[F>Q77LXY6.M''/NMEW.UKW*Q9WNUIO.UU7NT%GL]UWNYY''.YS'',YO?''SC.I=E4987A;^6?F4\$2%$]:YW,"U-..V6;HF(/ND%RJZPO#D%*4Z5N,:S!::_>*7/?M4Y.:"O/Z 5N:5[-:GJ?(#_7I#SW,19WZ/MBL6;F.;''_J7[?-(].>;O>-#S_.>?WN:G6^75/*''#W+@CC\0K7>;SV) G?3?1DY?/9?;119:P@+?1<?;] S4\6)-/DU#@"]*!6''(172$B>;M6JWN]EQ:]FG;*-<"]PELLH[::UA-8N3M O_L=T+6&<#O3^C=82YYIQW"GY_?6AQZ*M+TD)6+8;U$\#Q?8S76C%5= 4_[7UW*>/W"K"Y?:[]?:]W?",9?9O7O6<3_"1=?Z%K9?W?5?HZ6JV <[]$E7V?=7^-_ZV"%>E)#O(,;,N5>,7S2.$QD @HW3M9B PD@FB1Y,Q]B_O8XOD4XLI=@!QX(SK_(;NO@"0(XQP3K$*CF#RH$LK5(<ZGH RX<%H\+\*AI$R(\3UZZ,&AC!3XX/V]+DRYS$4HUD]_=J;M&RY$VXP4<ZQX)4X,V]B%''>]J$RZ-VYT9N.1J"3H,>[H2O&IC)RX\PDB\J1@!B@#U461=)0P]J@;9\67]#X.=./#Q!Y_[G%@]C++.OG$BMK''$29,.WK&CM+/#,.V)%0BQ N<S\:]@I@)U/ESTB:-V''P D2#] 5;]N+W,&?OM*5:]&''P*%/[='']:>F7W0X_#=$6;NGGU4E$_K27\M_O)3:T#W76]^N7,4[^;+%::.6''12%U?U?:]=7'',6)V[S9:=>''K#7)6;)?>^NG''"<6''W767+M[][]O31=9=3/?V7V&?%D_! _M$U.E(8<8@A@A*N9UTMW044,D4[]M05&A[?!/FE!FI-%KKY"3CFJNNLLH8CPA$HC^DSSR@M5AYOO/JXEU%V-PP$TPYI)EVRCO5XIET,^TQ$V5I!QBUDZ!WY)EMOI!T%Q0 E^UUQU/JH9YX;Y[$$$UZQAVYCU9J989!,ZQ''Q&&E::RYLX Y9Y9%4Y)''&%'' ^=RV__5X9)9>BL/''''#! =F%JYGRVZE%"U#)%SWA+V]Y\O[USSP5<M?EV"XGXUI(ZJCR3VFH601"+++ISYVDXB-4E(G7@L9!_  ]3I!1=>.HE&W7 R@'',,!QM.12./.3JK[HW:4X]] 0%NBB5;4#*(W+W]Q)/-,0O&->288B8++(C_-,.,,^%9F61=??F.:21.5)([K;+.V- [/-7:R2E1P8S#08ZNBQH*W<U L6HY#:G(PPL=+I!G";Q&+OGFK4K!6T@EN:S$#69AEBRUS3%EDT%#&,VU2#;-ZMJV_Z+9L%\G,U2323@?IOMOME.ZZL) :#3426>6</MPZI%9ME@^FX424#@)_QIRSV/JH=EQ*=P25S5[/UZVMQM:L<1R[465U$-#CZ_Z[X<E=TX3R?+(P+#>$RE]GX[ZP0?E%D("*(N)*"H6.6CL<^JLM<:F+[L1A;C@.?F*J;_-_Z-,/IZ?*5=;''Y,GV.[3% -0/*VC;);()V->K[WSM%#99:,SN3*8)B?[^[R3318]:?>$9<:9^;3K#&5.4PZ_;/B1>6)<_+<K^:?$;+)[NTG! ADM!7]9BJK DC.&AP@^@F81@GX8+/;:,]+:F,=''!_762"VW!N" <0M2)L%#9X%?$OSSBO>X@$AHVVU?=(.R''P*H0A8)<G<G+I<C)>P(A$)0RO:+X@PE&D@JT&*C[9- A#>8$0Y"LG<YJ^DE>8_B) T0!B\,X@(MX$IG)X$*RI)I?4@S%8M132996T,CF#Z"T:E*@F6XVFH.9"K6N_FI&^%LFUP#(V1QB5"T"1\V\]\+9N$&VVC\C/V8.LT1&#FL=1F#E-G(N#V:LX5)?NH[61#G;I31"&FT(13!6DX=6)G?#_O2H?K8FL ?3+E5!_P"/\;E+''A!ZA:E"HX ;FJO^0 B@E3 Q O2PXP:$BD0R[ KD,,W.FH, CEPSJT*H6L+!80FT8A(F)VH=IP5!VRF,<1SKT$(L%$JRY\],^VY7)[K.UT-!K#<)SFE6T-"J/M&3ARK6YHDSU9JL5F4CJX5$5''KYV;S%=6,) FG^[I''?/J[3 3''N\_Y3GJ*\9_,5L 1G= &AQH$K.+0PSP<YY\0XKHAQB FDWX #!HM! 5I:L\UI(XDH#S %N%[)TQUZZ,!1BT6?ED^,GQERH;RLW)8A@<Y\3UR$YZQ)B\5JR@="$"V$,:%8HK)Q$-Z.9CV-JL4?0T)S%M:48<N4*\2Q^%J\?+S%!HU)#:]*V?TLP==\." /MBKI V:#Q@\8B:!;D\H0AF"DI72UQLMJ?.$&JO<,\%-,Y1TSN:4)Q;UJZ4CQF-[7ZJ&.Z95IGY%6$[2J%^5:!V.='',+W0W+5;;F%[BG+V-!DQ-X1RZVTHM5[FL!.5#I.-V2]JW,X=%ZV\5N%+N[1V-''''VKO3D)$G [[W!IB"Y\6 HI!QO!BP]$0"8LNXJGX:HEC/7(O,_+V\[ZZU+$0-9+?MN!61,4O\(^%7NH2=4KNW]C-8BU]:BXW.-R5''WV_N54JDS^;2X..])][W_JNU;3!A^=7/],\<:ZW/\]E;?=:+=-]>&:7/OI-[7;#*5;==+]^(</^O.4R2&L@HAM:4LLPDSC[HQC&B%3-:%_-4-/^?/[B,6(%+)[4-Q492V0^9!JH[Q["IW5XT"MNLX)W_NHV"9#EK7Z1"VML81''[.D021''FM]W3#D/O8138NL(&G+NH\B;''HN382#G-L9BTC.\%S*=M(9)F@P/@-B[N8A3)\J0\$DCDDQ)0EF0+C5Q9 @0$KZDL/J(3!M,-J".)YE$YOH9,:?>/N]L[3''O],933;&\=:;#N ?23(P _:4HQF=J@W[^!DN9+Q"&84)A<-:T)G>-JT1/R$M67)SG.Z49+.MJ YAB2A1LWJZE!-D([? @X2 JHAS-B$[H4XR$P,-FHTV4@40N+&W,?HQ+]0YY8ZT*%"D?/X1$86,)^-;FX3>=''N##Z4)27-Z%O;6-[NM+Z7+^5.\?/[7 87.L\-;''JS^=*WG]4PMIQ*L[N!E B803''F@N-Y49Z5UT!"1^BA!C1\0QV>C''"L9+FOZO@!+\1M^GHU3/BEN;3!DG>81BMN<X%[/NHX/;#FL<;1#W.<82C?.L!CS/JGI<@JF[ $A([PB3PX@@JX)LTM=@@CA B@C)2(12?4 XT2-FDR6D AM-Q!CPA (!:%DLPKAL;427SFD7Z)!C>,LOV*T?7*U,<:5+^.=Z93?^-^C3/X12;6?;JS?^1&S3/Z5:;6-+O=;V:ON=3''K/^:4?7.],<;7-]N=]E 8Q>>\LLV@G@E@OC!F8*P1]>&4XXYM@D@@O@D9L%0#S)T+@#_.AG$(3DJA3_=<:0DPBPRLH-JVOS4.D(=:%^/>-Z3?/V.#37,Y2?;6-O>=+[OO^97+?/^<?;7/ <><H\/?NHS??#FS3;2%:=<69?>>UOGP"Q8$PAM"B@ZS5AFM,KQ 3+H QXR8DP[@B@J2Q_"DSAPA24^\@9]3@DX$L\B:N_?&MJS70(I.H\U=,???//??0@X @H8 @QX @Y8 @"X @*8 @3X @;8 A@X QH8 QQX Q[H ET7AEZFC/=,@@!3\BO2X@Z2H@\=\@:2\@4>!0 / @IW<@&SP@,=@@L=\@MV<@)W\@UXXFG3PG?3M0PEQ0]C8@"&L@QDRHR8T(SOU8QG*HQH&HQF6HQK>HQHFHUNVHUT^HUP&HUS*HUL.HU^6HU JHU!ZHU\JHY&RHY_^HYX&HY(NHY+:HY%6HY*FH]/NH],VH]2&H]8.H]46H]7:H]]^HR(XPTALFA,L@R/@@A-(@OY(@!ET@SSTG"H@@!,$@C#X0LOT@R2 @]*4@%%@@BEPFD<2HL6@''VU\@>1TQ**RCB,"B.-.H*.FH.0NH./VH.2ZH.4^H.:&H.<"H.>.H.?6H/@NH3BVH3?0W"L1H"L1)"L3K"L3*"L4M"L4_"L4%"M36!Q>7@A:@@@<&\W8Z@L$@\NJZ@K01@I$R@I"(APO  K''4PKE]@C''T@F@B@HU,@F+WBH(3!?G%LF,Q@DZQ@D,]@NQ-@N,R@A@S&P@-$NB[&PBM&PB.&PCO&PD!&QE@&QE#&QE5&QFK&QF-&QF_&QG@&RG!&RIC&RI"&RJE&RJW&RJ-&RKO&RJ1&SK"&SLC&SM$&Q=F@DA"@C/.@X=7@F8:\CD&@C[P@A]7DA]*DH(]@F!!@APT@G(N! #)BOON H&2\C$I^UV+&UWM&UW/&UXA&VX#&VYE&VY''&VZI&VZ+&V[M&V[/&V?7@Y%7H9%6:I%PC@B9CA@.I0AY;PA(&@A;6U@JJX@/''&CV4 C A7C987AAYF%PIWBV3PB:W0BU*@BO70B5)0&Y&I&Y+Y&Y39&Y,Y&)8)&*@9&*YY&*!I&*)9&*.Y&*39&*8Y&:49&;AI&;IY&;!9&;))&;2Y&;69&;8Y''L@9''K=Y''LI)''K.IBH" C;6@B891#62 @&5@A%)@F&2P@U$&"$N@B_GGA&3&&JCWV5[ ^^AY''.Y9''."Y''#C"^^;FA%E E4I A:40B1% E;L0B3.H#60@CNXPC*J(''$3WV:6P''0AZ(@Y:(@#ZY)QX"ZJ8(H59C?^PAK5U''=))E09F(@'':V3BM&ZD\6*D^>*D\498B^!\[V!''5RI8 J%X(&*H,6*H.>*I6$Y<%F!$T^!\XB*O+D1@@N0@a')
%
classmethod: R4PReport
imageLayoutspacingGifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 632;
			add: #Height -> 309;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '"9J^ZJpbGP]KAMKf](\VU82=/>T=q5]ZT$so''Bn%I!gsEXL?''riq*Wl%*I/V.P`j(daY;&ZqO@sOc
	k[7_T^-Qm,m<_FE23]qYUa;,U=]nATiHJJrH1r[)C/3k3DRA"r%jl''\:jj9NG\-@Vh3rS904=-n6M%o
	)*lYHpUG)KSk^N3]+.R]^+kDgZnE,>$M@@":>b]A85q^WiR-`/B#ulbf\.''8jnQ&S?0HSgtpi7ip:c9
	I''gW+`59`sY-a.FN:c]5e)pSPhW2&LZrBq&cee$bqXjFNr6*7$UnXY!&e)_0j>8VKgtL9=ip8$F1hh,
	dS=Zf=3^i?"ljF./Ci!pFMM+Ab6mNrrCN=gPd`e5QPe=raG^=IcL5_q+[,gWP#n@Xf*$!P`jIDa9I"$I
	U]X5r3gs+-or;QTs":W5e`,''1!cHb3uP,-mpoTYT))]g9;b1''X%A7T:om.L:(Ng/8ar*3E)''GVT5-
	7jt=Utk8%o^;([^;PYUKNVbpXLcVsFEA38/4<6<4?bko8kdjnj<uWC":cD3782IqjI;pG`6REP&ellR4
	K>d$92f*VjlCfDPanV`.kE5^CNPMqPdIm3<)d+uVQ:IH''Jq^1Ci4>`7SG''P,h1;m]Y(m^>[74#ltgQ
	eN00XC]#<+Pll6ohFF&?,mdt%%Cn-ra9MSQoY,pF>m/6SLbh1uuH@utB$mZl6bf7SkQB]>3e:`aEN/!!
	r@Usk0F@8;T9MA-rdc>IuKX8D:UoqAZhXkW)KRA6%bgPQO?>PjlS@lScgueMk/i47P''U4oZjPJr#"p>
	nm,f7j^o_.W^DL?hpS?BHAeE6DIg>''p''e@`fFbM):s)C-[[1jk%:S>rs_mdpYi''\qEt:Kh:6Ao*^''
	SE''>@I!U0.:/p$:oF)34$l0haY,iKf/BlD`eE6,;`lsC%N:ci9[''m>A<`K"ro_%qUUo''s+g>k(K2!
	3reCjLaq5"+c*ljO@4~>'));
			add: #Length -> 33395;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVMt%sT)UGq]$QZmC(q.&kr48*?$P:td8\8O].1)1#@unOLh2)8$DSB,uO)1u?0mIP^V(PFEK^A>E6
	RY"uYm)`%/FO9j473P?koK@dLi%Lt+:36pVD>S"AjW[pn`a4h6!*lq;%_&.).h=pl^q7HK58X<f0a_E-
	hT@odkRJ6Tl^\9s(N;;g7FErSRda)\+Aj:X=G+j*>nLG(BK@c-pA/P!Pn::/k+aJaON:hqO]5RHloNXA
	+ReN\A]1:dEBAcU!qXIZp:jMQSn-o#6bh%&k2;lT8h,%_>E"@G)H%\=YH_igVLj$SuDW.>7]P5;Z"0p#
	RB-7;c0A&]a1hG\)]`M(A-b6i&%Y9[2!WREUr<&nPE$5!;,%of<ST`AJWYI*`+E*Rh6"1G3Neh&TIGld
	oR3<=c\Ja$Khj%b%Dts''^O&:9r/;_s@M=PZ/f13U5T@*[l''MaE''iJkV9qO:"=,#R)5a<6:*E*;G]''
	M(Xu\A\t#hn]KKc-e81FlX2gcruW]D.`+#ae^bE0]Lk1Hsl&s\L-f\Dn+Gf/5%=3k0i?tQN,)8Em\uir
	B>R13N4"rG+%SopA''c^deiR6Y''FUW_-J17f7IV\BYI8JgKr$R44g1L/E:eoQa`&n*:=juSG^Rfq9%8
	)R7nLbl;rjRr?!Oo6.JjqlYl]t6n3:Q=n[8.3(NLu7K(;V-iKF6aZfG<-VpmpYO_$mb,j]oE6L+[N\15
	%pKZ^,?oMD`4\<(''Ie^nA$d@f<\asZ"Z\7?c?[tN,gN961$%%?2ULa;:f%$,#7fEDkS?_Afb1Eird3!
	:,:/g:t!6q8is0`6C`A4l9CXe)]m$jF<LVb,hA)i3M:MkKtk<JlIgcRq[n?@rm;Z(</HlFZI(&eroKj4
	=hO3E%mDhkNglW*Jhn=GB,9YA6l>''h;PWEf=[rP,?pqgqt'';aIDIbMhXXWQ*gfZSgLHKDf>Dhcf@if
	O(k,fRqM"nThk#?8&ADA)u[*hnS>_ZK#q+dqUMJTo2E4j.#&E''$!Z*qZSC$f>2+9K4a.`UZ%!fO5XN*
	GDl4^`.=;h7<f)9LFi$crt9?6SCMu-ZQ!g5hplrFKB->=]VA]5#PS)=h37k.Ai=U+s2I^''9ZCkZn)J_
	J$e6,b,[!K;nkD?_:n;3?lF5P6GN$uqBJS+DcsaaKRf>i_[.''^>1;A#''hqsd4ifEN=?Qo:X=*.fH2;
	@;-TYW(DHhl@g1]1aPn0RSBAQkN5=6aGb3PL7(rW&5>nhoKVlDO''^qX)$aMN>+#/3D-Bch=r=%EF:".
	6fMr@WC<''>r02&9)tSBI1UdfI":ESqFLLiO/BVS"$u<u>rA''Zf9#O;/0F*QeD''B.?f&AHmOhUcm,.
	hih-FrKWaIsjUu(D?\_H62@f=-N(Slq7^5FLJbCjXCPa,MFBhe#nPf^"n!G]E][`*Rc[;nJH0,L-(1]K
	<T/b;1cH^hHVP)mk:K<kL^]9GOmX6W#PDZ;Z)meVkHCEM,j%TiaR9$=U">Bm:[-QeW^>[?5%U.0R_jWK
	i@I;5\EO4DacA&[V''jh@c+>Wqam#7L!.Z49A/7ENlc^>4\e&<;i;.bHQ:+T0;''jZ^OuL+<dZ-eLM<B
	Q>K[2gmOGrYVu%X=]K:,ELWdcJ#tnYl4P7PC66_9X^N4-[2ZKhtPO-G3@F@8W%_6Jj&G%Z&5c1iL&O11
	hDEC,Dud9Ic./=5%">e:0Tb2CO3*VF]6=*`Kqj?4'')X-o1`Fo@27hm21<s7pS,F"O+6sdf6e5Q&m)f<
	\De1\2J1rS1jk5F^UX,DFIV_CfYmNE&EVKtEa>=6_&!Sgp3f8&_hXT%%.H`a]!$bUq(3U=!*EZKL$rI9
	D@JKX;%>D''o2&OE@Wt$90QqPrImVqac1F;[CiFiFf;h+rd''nHS\SA^A4jNa''huSB(1cRZ!E$f$7!3
	4.,O1HHuRb$ccnd''/h,S/t^(#PGJ''6hNYNQmLu^DHhbk=KJ@[L\EE5@j7YO-5V_@+F.:\Gtj+:ZT6:
	P#M]Qs3C+bamT3%*u2:U<cs!9ml8BoYA4AVmUj9@"?>uVe[?_%mHPS=WN\''fY!uqSO"r<=A/teKW!0=
	B1j_N"BJ6#;loDP[kKl-\4A+iP+X]L%nct@+Ek-*=b.(>`q@pY@RDOa1>uZ3ek!02<]KdHIe<aS0U04>
	9^+RnU2jVB79`EW%/cN"3gm?UI94l18ER.A2j_hZ%47N(aQ''(%I)&oN1lbd-3n[$1RMYG(W$I?iM^<=
	9jNQ?kVWQSK7NU1>6i]Gi0^%d*Rkd2Fq/%QfSCh''+4`5nR5VtP6"SGWE`n[FR,jI3nj><fHDk=q/T81
	&M0mJ;i^i&.W(Ygs!&c''W#GN`K:A;0sJpaj<;@H?^-0htO(hA:=SgnbR^8QE;oGlSWCr4NEM2c7N)TV
	gA"%CWaa4G>IAp^RGq@+1ki<>o>Y<NVXcApo.*m!\\+:\R28B/?:X6V@0]m;H0tf?%"d2-USPSUNL2Cf
	TR*Hd8/UZ3VguKa$0:7GL8:Fle*9TFb!Cb(.L+3Kmn]!n=2''(__usoApg9r:pp6)G+DFlq''bk4aE7&
	k[tEM`ruL4W5NqDon\"E:G8.e]mQ8d>h:)C$H+?l5''htamoAbEPd^0:Y:!.i"O-m,]I+b4QctT]M[OC
	?B6k7!=)2YoJkYWj7L(t,tbm]%]Ol6M>g%H(B''0I5ko7I9qTl9,H/D`L<Zc$@is8FA"s)I-r]5tLbs0
	h4qAIV`BCW]=$qsaDU^C4JLeOs0P-9Q\NQCF_$5W+GI6><)39k!H:IY,lfh$+A0atfM:r*Jg&lYcP.jY
	W7S1moN1*j/mFG&&\%0g&pNXE!*N.i?N[@$E($3Xt.<7kO*$HdVT(Wt7hV?g-5l%VkQJFShio''ZQt6:
	jB''WEq6g):Ys:N`e3?=hk#2k\j&CS1@#AC*G^h=C`9mgI#IX?W.cXd3ts?N,W#kHcUVuNE5m!\#27/B
	kS4/+9U;#^K:]7US%]kTXY9VIjI^mCEp''Llh0KLS-D^Nb)42N*[&0U`$*-UlV?Wcf_3ZUWEYAk&56G_
	%(HN.&bZP`Be<d].j00_E@dU_j-XDr9(O8JsV"TW/Z''LBr7U6J[aR,m5`/(n_Q6lU2YONX[IZQo1I0S
	3oE,!M,f''\FEr.Kfro<Z[*^\`W\1%GW5X\Y&%hu<Vi^$`Ld++F:lmm1N5mYVZ>J!''*bPXnr@''L?.3
	<h`A@^1`):TpJ._!X34/cfcL''2-G*LJc''4"%29F;kITc,-*IWF<*J@a3qlEJn(Q1T6H<0+d-Tup[)=
	KL?PPQ0149&Qp5_FcgWikJF-i($kh2SLBt;1k`k\PBZ9AI!\E35WW*1pKO_lkBAOn</qba&@dlu"FPK)
	>&VC''K<dB?[L!U_<78U3eO1*__BgB7GPGrfV;]uB@E(VPU<L9RO`gk<NBQ?HNSYH7!%\6IJMOU3Q;fR
	ZA[?dT(6RCo)qm4XK];j[_BU>2:)]<:HXji9K?@CPh#guLP<s,e:0*KkaPEp''4>0@67$++<R-DeB8)<
	cLdPR+^;3ZE-@3R3jl$N0/rKTXto$n]TWGad<5?^b]+a$3,]X#ldioQZtW?,^_`II/$<_G/aFLFn,LqF
	m6Sr>OYANV<#@[-7<m7G4JMWT[Y(]j;qD^TNn(5MsRqN)Z!,]hU''[gCLrm&"n<hQ*FoM#/bMC["4Qal
	SO^:''6inLS`a*/u3_35$I''iA,>h<$a^[91WXOlA)B3QrC==okTJ?pA=]msU(Xd,m5mMaa#TQjU*Kr8
	%=aYi"t*\2jGZZ7_R_l6+A2Z%m*SG9r#pt,^>7;Q^[0-Jl[I_\rkgQ.1NfeN''8>ZY"Yo1G/.i!kkD4P
	c?nr*s;VVsq;\i13Z]ZO(ZmI4%8na?">V3=7]O@_+I,D(08F5oshaF''Pc?lc^XYbpC<.=9]M\/T@G''
	dLg^n:!O7O#03>KO%);?gXfmVL!LcIimm&%[",u+!HCL<6Hk*BSW*Ol1BS3F;7LFT?WZZNb!WJ3d9\Tl
	Ybn:1Hd5ePhegg02)8iOba-D<qo]g)mE&`9VS?qZJHdl(#7fldk>H[P&a=fd;=7fNroa6Ud+?g5S,A07
	gQX_*[XIa?5#YhB]No-/j"fn9FEXCE#F6`91Q18]2Nberh6O[`2Td.q:&)dJe"R!oQ6JOuGhYX4[N5.7
	!eKC(-&B6jKue2k)TgX@P=rF@.C\HtZO+8H]0c<\=<_/!I_LI$$B56(''dsIKf"KD$-/3o;Ws+#/19m]
	]H`jY\pMM@f<6N`$P&2tA+2D=+gWEL0O^smEa$:AlQp;!?;H.@d1,J<JQNBa`7C7L4/3hE`l?*-c"UYj
	K^Kjg1^Dc2$ngd3rO.!(gC--ajki`c`C3C%Y6KU7T6X?Uk>Q^YYJH`a!R/s#hf")<6Y<32@"hmf?G_.a
	HpjT(/H]u"CIsnN+SRt$nAljVNI7?.u+P!.;.:oV:7SDYo(DL^(hB+G[`L8E8i>N,*\U]!V]Oj/;XLqr
	WED%q%QRMmkV@b#I_iE(r=G@^JIFJmSVZ>R:h/sUph()l7."D$aI_\&9-QOFUUkfF@30bV1I/1Pa;)c.
	''*11g9&[IN:8hj,R%i)5N,Od!7d8(+bfC]481gktfO3[OAr\=SSMl]T:!iH=c_Ah82b*Oi<9)c''17q
	(#''R$W=E/`HnVB`.B`M7k)p]2\Ts>h<NSk/&PY,<8\aMa"]lMg=6`qM''hniHbg7qXJ3B;gIC?g&)j%
	8)el2XS##&j/+RYQM<f2B`\3j&''e#Xc7FH7meq`1($,FoZ9H&cT<6KYBujVEC!?7lVQ7ekoekR-*BQp
	8(.luuh^Xf*NP821k#]hBXhlEqQ$([p$fg-Mf#Z>VlSPm+9WlpVLh5pDZ&40s#n]s+TcJWD&F[%NhXpo
	agC`Ec^gC.Jkuj+[9I:GXbp*''Iqf6k=''J[q8jr25%oM*n6@-.Weq+PRdEHti`G7V?,S`V[oJ0-N''N
	+"+9.4rB^P!Cp@bG:"e*<%2t-=P7r^5bDrE''a-@!l%Ydq`Qp[5<O5%RAoSQOlhsX^puC0@hY:Kfq$A_
	N?4AHRA36:WJ0U''7VbKE/JEeKo)O."`\IY&!rseJkj_1%BADV^_BXl).kIM-jmGdap.UBUo^15U]>3a
	n/6tbP!*q>Hj[XJe/Go!GQCSnolMJS3e+2T=q4L#\YZTImc;a&BK&t+U-9-esBH<<tKJagcUV<$l_/Lo
	6K+S\%8-aP)7nF?^Sr`C/4^B&r3_@Zoil%Ro;Cl)*E,]u]O1i303^\Fe]Si^APC9N4Z\[II\hc\^0I=M
	PZO19WHk=d7pWJ=#c]<0;&/4YF?H9M]kZ\<jFpr>p2p)s9ZKT@:3"3&>"@k&`8WJ@d^F/N_^95SlS!?i
	FdsX^<oUs4bc)P[WZri$G>ddn.W5-$*DUN''D+/(<+"du;gp''HKPJDd+*o?AQU"t2mYqF%sM-;%Vea1
	K+WO#((mf''f''<Aj^^0*FH9AmU=E]Xg*^4F5.[BNO.sF0H]XnJD:Q1_/L_1`cdsY`qKOJ(gXU,D/>2Q
	9)`2q#nMp<0fR>]''-c\QO%0&M0?rsImWB/W,KT?O5Z(]#HL+@/Xq6A"*<IUA%3KuNRPAp1^%^Fm^kc=
	&`Ybu''Z<Kima2gpl\9Q_T[3KR$V@?GW#(*V3)OkMV;`4]iG^I2r+%CTDa/gj/Q-9%#-Mr7j4Gol3''A
	jfhkk#"Cf,^2`H2[To.gK;XN8^8-Qt7j0HPP0S@$&T/\*VC:FSGlc-cp),@uPnlp8m-ZZ=tb7Sd**l98
	2cIS''A64QN$$d]bTi>=31QZ1ChN<SXip`JKeMLWG/r`=rQb+>jul*8WB$SXQb"+56t0.Je\t%(rn_SS
	*`&EK>SF?AHqI%J04bX4hED_446E8B4mso1lM.-7/U5>][5\tO\3@CFSAt]@+KJP4Cl-DU]/[Yq2S]/C
	s+b]=I/*X#T9@Jf5>);Tg''YD@u9tAL:8Bl5QE1jJdu3@%V#2F<X-!PkdO?B/-8H_>sKCuEklNWmPN11
	Fc1"4^Ibo)!_pgPf,d&$rIuHsr/PCM>+q@(Bp4c1@:^6oVj7Mc-"F/+MP((`D>HT%D%XoUfX%a]]ZY:X
	qi"m_!M%7a(`%5JQu;0"9XEc\>UO$V>FL6s*JZ!r85.A>cqeePYblWK7qShgR.%LN=qNMIDC>YX2(H=I
	N:AWpU[dZ%qefrjr`=Yp*R!mQHWGMI-)_E;cPD@kY.77aT,GSY"H:*aH5;\`$53Wu9?<&L=<MSNAh6iu
	4<o=c$WTjZ?NR@reFK.L&(;An-/YWfh%G33=kL''/3P`7jTP%i(&ut9:r%84L\Ecq\ThRFFcs;0:[oLg
	MfkjhK)9d//OhoNnR/>g`]h*qrJo;cLGVd6U.Kn9Q[k#,X)Tpj$cX]%?W"H7LCS`!3g2iaN_oc#\<a94
	QTa!C``\[UGma^,#TH0CE5Zqbom"4nc43''.o[<kM+LpIhsh:S(7na9mtDS-Y^]8k=G^.=1$dqX`V(lS
	%s*[7)''D3XD3HEpHMm\;WqXSRLb`1bSagQ_/sGkSt$Ki"1S+"P2PNoI?q7kQ\gc00X<+</4&TIZ0X^e
	fA''c\.UgPsaCV%,r[kM>8:.]n9RU4+VEr$LA+l_op59It?ScV\[Ogrg=YW[H*Y?++:u/kp&X1(EedU+
	p&nkNoB9/(ZD\<I7U^u1$&u<E.eI!+Z[C-5gP$B;H]RXL3M6.mod,VC?76U=KCA\/2K''!Z=AYmSX$lG
	kT2$f9eKLt+)`%!Z1<X%kh$J`6VI:E\@C3Xii!l<B)F''FjN98Wb9N4VB04gs#PK\X:93L^.6nc-RH6!
	H2HZ_ln!U;5.-;.>[\casXO#Ph9GWr,%V/c;)sA27;R<1\-(liu8Y^MtS$A?TK''`[p%;p@Kh8K2G[um
	T:/$p>Xq7a#&>sD:@#=3)+QJ''r,$WX2/%+TN>@IeS.!,ltGdJ>$:;(''.%-Q1ieN36S3U`_]U5JkdCq
	&RI*6nC:8].U=#JJIn0:)A@^FoCTX<M7+U:":_YZU[N.A)CD)@%2''0/A''q6fd:`/[p"<[Ka''&''+5
	PPMhEi2c.r2l?3:I$NQ62""JFU$jV%HXoKP;e"mJloXEfp.cGWn`E5''\1cHr%s9p1b&fEqSJh(cgtB)
	5Re(4Pf`g_dmf^ERs\8<=d<k=ui6qG!q9Umh\c"oE,L0%7E9gF]ulc"C[3$0)RDH&NU!R=%+HPKUGDa(
	\,T(6#Q"824(pT3E:F%`>EUVRen8P''Ef37CbCgcNKu5!OH6ZM:QViDR@7!He?a6*R/Nt<q4Dgcdj)_D
	DU42b"o$e%Tfr:37f,JCYM]:&);AD-q8,[bm&n2kFEWkQYqqb_Z$dG!Z^PIDo]:]B1*`0U[I8a5n^@i8
	".&Z"J^iY-Gp]mMrqM>;7Bdi]c6_o+K9ncjLf\].s,Jbo5R''V''+WPiZ]0/#29)YE-a-[8l*:k2>e<O
	2&&^2`aBb<7XGpqgSphd9jG,mG?G=[#[;FY.8fX8IDNcs&l7UGQl((TV439E.MQW("u^is3ugoO_XDCY
	f>#o8KcCX!dQm:Dm?/7Oc"m0+=[1K[L:q9X$^Duhb.c^HTF=X5BmGJENtaS*JCm?K-DauX99:tKZ:HJL
	_l\CMMIBI:o1-h)-&`5o6]n$B+9RaQg+&9p9Y''ErKo&FWNefn^EAfO0f(b#drKjM+@,B5-9[!oi@iRT
	?<VnI!s=LJrr\iGoB@N%=5M^$n.]n%p+-qJ`gd3$''#BTmjl>%%ni*_1n*-#e348QOITH<%r\X[[''9S
	N@eJN*SM5iXJt"#aiU$;;0*2$89mfDHKF4:hq!&WW2qcc?$=u!L,q<2J1p]#>a@*#fk:(4C"lg=/O1#q
	ml2K4KQu,#"oKu_f)!Ro%l4;LXF[@Dds`j@PVJf`Kd.q2CY`+(Jh$pETY+$R]T?uBYM!6bl+J6`7<.%s
	B.T2f[<1IV`<Lr_SmZF595Mpt])OCeB8(jAg''gKc^RXNl2<nckPC&Z*^uk_&elr#5Ao!c_Mq2&e5?f7
	^[[^#38]4bqe12PlSOM;]YL;,1S_s.@''FlH$W<G%uWH*4?M-"aW.j&/[^h8lh=U/)^`R,(1k;"&6o"^
	W8oB#mPn^,quN#B@MPAhpG$2//;5%cpC+8bL0^U*c''][P$V)Il"sY%_q-bZ]`Ob<"1Y!)f/7=h<E_cD
	1#EA999CI]pbO2q=[h27B43O#S)^8hGktB/kok]/DVk&SLgT!2LX4is2A)OcRrTrjt9ARL-V?Xs&\AA)
	2*i9Eo*^??S''r(ushmd*^),"?k@\:!A%+C4`mu6j-&[>iN,28C092O$al;N%cuH]-\^8;.qoF<u<8Sr
	tV@lPfmsU\nglo!:S(SeU3FCZ.RLTAF?[PF73O_;@ObHV3ROO$![(SbV@g^!8pt68<YS[nkE"H`&j[Gd
	&tH0HPS"QE&ap@g-Z`S4e>:-o([c,EsI8.84N[G>$+0^.<=GqV$8>kR5Nt(@=WS\cO$6gC_:?:cjNHU2
	X%UR)&)^[Vu,qm4WBpG^oB7:1#Q3T4@$h$SY/hGlGFSR=gO+5RBpA)8XGo0#Ae*''93.OQ]pH$9B>"Z=
	C\u"RJ$:`<1\YeX''Q\E4A[!9MBIP1Y2o-2@%1h#$ou/eFc#$f>:.^<3Nk\!7ZQ_&Qa/Ip1f4W@0=-g`
	M1Ae77`gGbGBm;[+%fr_BU7@)`?mm59ifbZud&-:Xg2V:=\6_V:_L;&cn_XY"ekVlD,0>B@gG>_o8!st
	Q1!>6<U*4@]bgR-Dr3.@gNaDq70K[,-b_1a)3d@>O2"pBpjWO%DjDgk,07fP.qq(;*h&lI^I5g77F.`L
	%;fAS4<InlW#-0-RAYt_Yj^uXjAung8BB/UCM09ih:&m%qeH12tKm;2sn4r"`">#8gH6F&7D2uAUN+F\
	a0L05*TIa[-N3Z(X%BfpQ0HArde<o0M[^hU/Gi@?m;>qTFBDl@i;8Oh&i#Vjo)YYLgpnHC<3J++$/G/P
	g!j)hJWHOHUc&$aMC*.Nc/*kphl/Z^Ms18s%HYVkFR,?RKbN1Uif(Ka''D)/%M2h?10&(Zba_+3PXQSV
	LqD''6bYjEGc4q0X''=B@_1"!hG.#Kt#%Kc*V6"%idCS"G;^,\f!<t#6g\YOae/urcGq&&U]PSZseFAa
	2g]5In.$e)qGX8aYR`/&IA6MP9`OM'';ktgrjpHI9dHm\fd7:;:8Y#/XNjL!r;:XX8E,Vl=a(192OOd@
	![03H-ZAifmemAC=16D9i`&Y(^^O"C]g,5qI=TP2^0pV3`_:^mj02,A7H=NmoBuhQL&;GJ=j5Mc=i"i-
	elkGIrd8i7_9qAI.U\OM->WBJi\H''MeaRN6=/gf56s3;JpO02h+`3.`G.R_!'')<r)''T<CPJk1fU<*
	6pmGR"W6::ej$FVLfOlS9Qe(\,do\5&3!n@nog$C?5;#@4mp54t6q5">sF-DPJ3b\D!5$ec.jg>rV/U9
	#.mi>mjbHNLYo8=bPZ#_!TdNOP[@NUo6YZXlfR%P&1j":ar9Q4Oi$U61*-6s?bX;8u"?4,?=c*b$]A"c
	CN*Og:=tqO\Lpm:cNjDoN)4Fs8Y-Ud\eqbS/#X9LZ:On^M,T,@6nQlUQ_[M3!EPiYd(=D<082lp0IY?b
	FQ7=taarJd:F*^E(&kh]DD@`M-Nm26g"NRk$5I=W!d]Jj:d*I1S+bAQN[aod,u<n*Bq$Ml#*4lF&[\]r
	^!B$Tf0j`SHm^KW^IDml7M-E`#LpR;/Fl;^F2B1-XI_q0*\p]+!-@9K^]`W*]lUKc/:QBJpH+g0ZotgJ
	N/Y[mbJHYNr)9>G&LoCAW>_IcobPh>tX=4E-D=21"P4-S%E<GKGorAgRc!?]6#X<;;GI/pD_>MgQ52o/
	kIVZrcmP8sWl2\o!3$5Uq-5<%HZdas3c,^G@[CT2+\anZk,k,,cdug($5kZ2!dhU)im494sECW6X_<K+
	(l@.LWh1REGfH''X4[gQ.&Bm\KaLfp\k8AIe78#B:=0nFli^_Whjp!H47,-1SQ$1cSY/*lu&D#A8L3r6
	(Jqb4+VSnT(";:i8G\:P\";CFk5pgpZ5gn+FZBqq2p`l(D.:%-R?i*GVrcs/"<KC&9.Kb38_7;rHB.h@
	oiu/8''(3ee(o4*g%ETrN6G0Om.@[OCY6U?7BW("q/(5%cY3Q)N;]B5RD[i_klrJHs.5/e[\a\hTM<u''
	X2!"oj$WtM#;3</Kr+_5AF@%aQ^$kqcTt%59>#ba]-Z8Cr23X`hWs1X0Sn`2TqW=m>%(mYnVtV)>9Mh5
	"7tt:&`/47-^>+[*QE[;EBDC0D80R6/B_4@.#%]gbJh?]fP7L@V7_`D$FP)rqH<GACYc/;Peqtb?''A?
	2][M+"/PU?6=T2ZF0k5/kjt-d^P"k/Uo:oLGFF5<L,]Jmrch\f#''i`"C3_;`>TaY!22t<aQmXi[uhmf
	^6^S3IWMFY-*MR#:d\n-*lGs]9Z,&bf^11Kj2bSjWJ]8:RIjNa7+hQ)#./#G*bHt\C4)s@#5*At\EN9q
	a1bthn\a8,c"X;''_''m\:SZs3B\MQh7Q;M;la0G[R.9N%O<40-dYsDcgk1Ic>.?2cJ:qHj?X8>2A\!F
	jJT,YeP%A$[o2RIki3i%+t8E\"2?ToUXXfeb:$*FuaeCgL%4(Jn\j9L/N7?:oZXS]quu3Y3aA$<Bj-aW
	6muL]:4b$$/[4<KC''l)3\o0po./DN6j\Q`''f.D^bW9bF`kq4SJ<T''TgNT7!.JHVlMA86gXlM)cCRm
	f[oGMKL%1se8Mh6eBCb]AEg6n"-<;!R(n''\;#RE31<G1.X(X/"jTX90n%OE#u9@R5r/\KFgt/<jaXCY
	ss^5M>k/$TY*I#XFr=N&pAj''4aVPq0E>nQ-;2$)!Yb/N$U\''G7TR5d&rP:3ODYNpoJ3eSW^=hb@7qb
	Z-,C<mB(<:QQ.RmSu_Rd<fI.%E28cp*5S,/,^@3-a8"&(;>KG$gs`g;r56SU5.6UjprKIDW%X5WTQB5)
	AO%(mM9].l74+b>md9)0WLqE<q02)s3N86uTGcJq.5hWJ8<r["+=7&E)po\rJG9Em+..,M''tkI[HM*[
	M3EhlOOOnq[D87BZ.cjZED;0dQI>k/9+-&7''U]GUOWmjDWJZd,MjD2mC*<&*lQ2;UFGh''H(*?d)Z@*
	iI_-/(Y84RNdmX/TVn9''=gP7eIL.,/5f;.E-_S0S42(ECZ9p&$9a-C,?.Qf7<P?(HWTf#9FkJSGZ?]2
	!!RX_f1RB#!?Nhn2MVG@GkaB>U=#R&kcFiY8]k(7O:VU_$t+).XrPnqVUbq^ZY*=b*T$''<";i]=rN:F
	$FVG#LMbVm-@Y0kb5\ZJj/4[-2FAIt`7s=l@X%<2p_E9ki:h2\_k:X`I9h''qF5`KB1a%M,\,@Rj,jrn
	P4M1o*"lA5<D.IQS[cR4iCcbqjY0A&\m:cNj)<B].F0pE./p9E4+CiQR_F.o$^m&#Hf&Gsr989IH#+dG
	r2B3%a&cPktI-JP''J;icD3eaJmbMBsuJ-C_#7K-6F,''t@i`(:p-n(BOh+X^VYE*uK!h:M'';Y>pQO''
	g<_\cp3C36[[3YcEIE&.EMV4X?''8^\/Y]FOU.2if-s:RbR"l7CC/m.!1mOjPWb49-Z\ZU7c6oM%>;#e
	8h+F%EMFf(q/H4j+hib0r^s*0?81Sfqfb-LVCZU8YY]^JfX%,S59&"(W-''nO^%f51/t%661XZ82=c_8
	nlA`GULBH(GVUX.$P!IGp8Hmm_n3_,qL;s;XRQX$S);uAV,Y<ei@Y#&\f8]!IU0-5K<;rWIIGfk8%0u=
	UU&<>\O&S''f.\+ALeO(L&_FpI&2LP?^s$(''#(_\8]`))$4-k0''*2#Oro3)aTimblmoI9npgK$Ehg''
	J?6o`KE/"B?l:q\08h9LSJ\&31:Vl^MC#\(FK[]@m57J''C0;+rc`G''/#$!Y;`:^se=!>A\mC)cUGhC
	4<DrFO.LGJb223AEZlsR,J3O0O<uPC&Pn+T=T%b].WZL^&dL+b0K\uh6$f]ArKm`mNc[GY.qtg<^Da$q
	;i''osf6Y`Y!irhCYcKpc&8.Dq%`W&*jM&f\b._\CYVPR*YV<#$0$,"1M3_YcCY%r(K<t!&V`<"gU$\)
	8-6Gmkl.uCSH-]gEXqD>??8bMsSVP<UE(AGS%iYA"i5FZ=_YOON66-M"A%RMk/_CesAF]GHB7Urq7B*s
	m)KY0i`Jg49b6`''.VMX)!8P70FA6]G!7lS6c&.VpkJ(m;5B563L%*[5-qU>11irYl@Fmg9Q!4ejrp3n
	P6k_>6OGXk/P@2*Y*5ir<#$FaYjC*[ehd5i;dA93d%unbo.Z4j<ebjZ`VXEm+gIc<9X8QCPpD+i.BCOa
	@`,5#eFW>W.kUj_Ljp`X;0QHs+Mdn''_/oUC)q3--h^eq=$Hbm+E=DQ:b$r/;AHcW0(%:e1<*$J-2)-e
	Z,VjD`UAE!,6+''V]4nT#jP@knH>7AR[?@DE9dmNPtmkkGu8kQJ\I1oqYmT-NdTb@X/h:S%B1gX`_^d>
	9DPPLJ9APiYNLJ#_G@iMnLhbt;FDMZr#6q74[[:%%hf"D]dj>.#7h4PKFD_3CEU`W\d!bY^cP7)eVg)3
	]&.-E>BPMlCkpTf-?I?.A=]A/Ud!`s&VCc,cmNrBk^%;`*2cNWN,;DC80PgLlGM_"28<"j:N<L/`mIsp
	h3_r-EZ-!&.h)KsW)_B10$I+?@o)?"jh@:LPP]8umK`X?aS.OVg^-jhn1Ke&&4I=MF/KAR&b6''4I%ZL
	0ru#&O-O<lMF#[P9"&hr$nEpGCfmcn[eYN98(AMPhC%f0`pES=],[E`#`.:ZYk?]N6_Tg$W1f7%lnasT
	FiqP''!k+O%A_%\;*%h?.mQi_je8\.i;nnIfNN,MdW\kl"A+EZ.B@g`$''WF646KW''\:omnH??.Gl8T
	[a&4RMWtZN3FOTmR?5563$>e>^:#*(m4lJV1rIs:.io*;[l_f%ONb+?NZeuCa!q7c`5?s&ZaVg^>kA13
	@0nO30eoarXq4LKRT.#FW3hqkK7\0$6W6BGV?d!Ed6<2SX"-Kg%D\.04cMGPTiVo-q-H%>m[IS5RC-&*
	n;H/[5u!f]WVITKn_u_>&.*\q5T>5:0gjtZ_Y*''2:2$q32eZ$?q#4r6D5D*rm6o$A*!rD?*EtXCd#4-
	RPOg*WAR[0D?/Jsb1.+Y1UN+VTTu<[DG6b1Z+_9<m/4l&I&YsZph*fBJ2fTd\hG8TTG<,1*0,6qLFi0C
	j_ofX1#T.c(P/-PSd0_"KD''OcTOa0Vlp6YP;M%3[oB%",VlSkCB//:GCTe;Faf8.!q+<N"N"8X&(bgn
	KC2r''K[Tl$Q?3%F#Dh)DgQ0QV.YW1nW7quRoWDs-G@/QbT=Ot=@W&NXf2UrEo_@%W6T^\Hn?[6kQe%+
	8qb%"gqColL7EuCS(-C*,@H*At2#t,?+EE,.)!X/On$,\3.UoUM7Z9^01k;KB\.X8/:!cm8$/V.34V[L
	f4569UEnVS%p4D>(C`mi?no!Nodk7]dDD9GIamqCt3auAb\ph\^i-dHB9db7.[0]HA0i8''W-9H25b6C
	d_n<=b01X''&r]OdVWSOfD9TKY0RqWm*=i/(Qio^aAXK4*OfeH`+43l8X(0ba>E6;D00L8YmLZGN\gC>
	rO9(*!#+J;6/B_Z1%&9NLI4XfV4,LCf[AZEMa/+_$0pm2IBAV(nYX`lB''CIcqJ?f9\$JeZg_jL.C*Ru
	mm,4]^8;lk38R4WM0B%JY^3a?QhbT-5?W8n&JdhKm)"(FF8%W]V#7hui4lVQE_s,&btH\(@*\o2PsD#c
	mqn-fBJ/u!_/EqdeM6X]NF_1"<N!QkS3;,(H"JiZ9C91/n7Ngo=CdpAXU]lH0h@<+j_E@"N7:Edo`:;h
	gL+mmh+@\9lK)$aJ<)X[h*,8_LJo.7UL\le?>U*BA6%*G"r0cLC:+JE#K-V$NF<Qa:7[R=5Y1(pOWY!=
	cgK,1T)C%DB0"g//,3\QL-g>ZUINtb<2/&R-PWeUm-!6qos7MD@Y3@Rh/7g8F;jTJ&-gXAAQ^-\K:ojM
	m],WSXtPq_N@@3f:.s+^Uq#76%TZ)8YBA$c`n(_9O-IK''(p"Vd1pb)Fl,)AL,Ok-''-f>i:SfJYKHcV
	<&E153?_3s^/4[d?rVi>H;VF..=>,b+I9GAh^&4`1WmMRKj\B6"nV0l3&>pL)m=/:DK1cN0:j&HH]?!m
	$5jOaXoGCXm96c[9k?7ud"NLOX9omCpL%\n>-30@pf9B=tRhCW$ZHKTV(>MsY\\aA&QQcgZ/KSP2jgA''
	KS$7(ETL_Yio4^h<JegGtZD(eS]NJ!MF7VU*Mfa_]#JK/d'',LYlBRMX&/C7^N87_WUsX,IF^B*UE2;f
	4Yt#4:O50sDekWsWT5Q%%1b:KmUc''>mTDZ*B;AIDn#-kmGdjihkuS''`u_eQ7s32.%M]EqXBn?S"+mB
	_5PA$ljcIGqsT^G:0M(TLNn"Rn2oK94-2>YhbQ46gX=#-]\>?2s/mmAUNl-1;R''mYo@iYZ9<r3*k-%''
	jVX5a*-9JSI=!QRQZ0Fl+<E_a\hC5SAA>V&L-O$DOQE"Jd.M5RD)HB\69?q;:.PcP2Q5NN+UqG)!C''-
	<"WqK[WITI>1LN<TYi:;s8(_9)a_fM<EeVT7fC=T<EeS@Vp[BZQ1*?6s%jXl1d>&i,[.N=h93_8#pb?O
	7HF74@K3]jQd35E/%4WEoQB;P.D6+dm)Cq<k37IL(u1if&Bp!)K?_>IFg+^Y2CA1j0FO4@LJ_>d"EoVW
	so^ZhJG&+ls%YDAsZG&$YLOFNV44)$::i$h/b8>nJ,oq\P3&@DF%Ob,?rMubA3Z%`_NEJ[X9i!IOknLV
	(6.[%qAZh4]<cONZ@0;;b"eTECqiXANB)^t8XjiMr$Ai2Z`Du8`a@[%4[g6AVg:f453%@''Hg!IL@#+I
	(Gr<k8Gj]HmDqZeg1K&=14D:a4Q<]s$n<4C/&''<cV6,1e;$#h?/''>#[Z/b.d.&",^45e4<C[so"-O4
	GK$@@S+IZ32/`fD<p7es\/W32CO;97^?7''LmsLR_:df.CiW`mT?4lcj3$3l)2*gTfm&<q4`0,!],L''
	6.[gEf!4Lrk%<)C6*#GgL*@Q?lm^S\$R.[ZMI8=>pX:j@b`E>@TI?tXLE9"b]YeYkD]6jK@De9L*:;eH
	Gs49=31C61IoDUbS>/,?2&V:NaRW<(SRe0%Mc3NWoaJ%RLss3:r1Z^fp$8FX5+V6$pb>Eab4QQGW.pF/
	^pCV[OK)"4\C&@X.m^I,R/G%=[I>YoFH`\`.rSXZ;V;rMErHKg=r05#1t''UR!^N''(B>Hrb?@M@hN.c
	l_Ei8+s#D];nP,6CMZLZQ"sgZ0^>8Z$lGO?:F8b/!jXEa*5UucT%hp>=X"#(=$f!X7q_3P%n,L7]]2uc
	u2$(jm4HZ57)@SZgsJ>?js*FC^f@@q<,Hqiht''amMpgQH`BIS=(-)2:7Tl#W]emAeYmCKjA^mKe[[TA
	&n>%9c^pDRGXXVOM0QL(R''T\)"U-RKhgGe*>pehT(q)56Rn=HgYR'']h,.EH?a"N0l[m,^n)T0t!VU1
	/d_V2rI%HPPtC%=)(;`^7/(Sm%/L@+n%)W&KcPf:hFGd2IQphV;)@48:b8@FaVi`ul\Y.E>d(1r*/''o
	@(`#,sY7UO6[n2M-C`<6U<TNV`#jWD$B3+lmK.+oChb7D^23KbD%]iKPhQmgnouGlN_Ec[`t8Hc:fF6)
	kBnH&Th(c(ihUB=AVE:EH"N3d6ZtSJ_/D;HFlo3,=K=n&uaI#]W&_I,NfM$BB<?&''qEB(E#J>Q/Hjp?
	^_,>o@)9,R$^48qMGllB6/WqI>''gXrp]0TjLf>n''q#Yc>V.7B?3jrd1[YMS''Kf>9B,$u"/&A)?PX$
	G)Ei=p-Asl:X8D0t2p4GtLYbp,WLBH^!hDm=I!Q?31>3(+$Y6?q4@''<Rt3*MkfW!=hQ+F*!(hn4hO?_
	&?7A,''=CV"d.(W(KSd99+,Q`"&WTZqG\YY7L9laWio$Q/obDa<N^,2Xdc?3B]_<Fo''GmomP-uB6nr(
	IJ[#PGn<JG/[T\O''Kg>H@UEjA3O2.QZO=bJ76X=N)>N$<`:<9b$79qMp+5\pDU4%poKgSJ9m7$PCg''
	&/?\*Zj*N3T)lrfPa8g6]kL*`;`7tj"$1E>-F=TRUm5`[1UnaXPU(&gPPB\Z$-4Jt@.VlD4?;-=KRVtA
	I.=ZD,3W[&Ite.&<!WC%+K(@iT+V7KNH0U6[$e`ds'')q>uhgT*S;%DWo_C9A1HmfDZ>$/QBo.4U[,S`
	i-)_np>WL8lPVA0:RtWqLumA)Ht1pr''YWQl+[=To>%m=.ObL#KMl?LCMG*X-TMA_ZdH9XhQQ0ebG\9I
	^h#TD9k6ls'':G)/,DsaHe.:G%8onf;Lk,''-mRRpoC)O-r&^"Qg%Hu''6LiJk&OL''oU=A*PKE1:2Qh
	eiOgk''AHo&_[$1I*fI+O:37V?.]J91Q4^/7Zf.#if1[ff>3sm:?7*ZondHCR&>X_+%8[-J57$](o@(Z
	''L^cXu>Ri!H``-NN+g7C1+%iRM#h''3K`3?_=5_u25DG,lrBPXHKC84aiHQoC:9?3?<L/F`\]62qR-,
	g;u/LJJO85fJ7D9jU6rs5GGchWA5uXsWD6%8-`87rR`D"B!db^QY4!m`2P,?+Op$j;41&Yp,/9o#nB.\
	>D2R)eF+2R5G>F.nMH=EM*CpL1>i#6;qc;ZYleu[Z@?_K)=V-tnB+J$$1?r(W^fjF4OI:\0@]<R^?moM
	iAfhj7S"F+K&;`0H*fCuWRuG!66"\\(<i?]<?&Uq9(s!b@$qO&gO80gZ]KGFMEZW#$?Ck2p2Y''<DFj1
	Ui1*gKL/D_hc0iY;J`l9?q?_a&3cBl'',lb\LL1K?)&m3$W12*''?6\H7)c5%QPiFfF5:N$1lNdNqRDR
	3#1Ub;J7r<`.S>NmobOY\\W_)s]fX-LPS-VdDM33O9_T0u)cBm%+dW#/]<O$M$90GA,>!WR43m)Dn*5H
	?eE5_Y,H6hI0a+AUtpLY\0/>Z;=WN42uS^m+>0`Z`LFle"_k=*'';''<`G)JMbFi''J>DaV>e)3*(!?Q
	I)Veah>#@(<*D!GjeH_:YjP0I-5-&@@.YW:!CcaL=*N>\?+nN3sfNN#0\,Z(`G!)40=W?L]`07gJJLb)
	psnmjU;G*5_k_)eoKFr2@M#qK%/''])CjBa8oIG@L-n+("bFBrG!T5kgVC5l7a%%eQ]7^"S*]PbmYb?j
	V+VYq`:;eQ$ls,ZnhjeB:b9B>;0MOMT><k!j''9KRApV$$$:KDOVlV;%Be6rsnh[LThSXT9sF4<\1\=A
	e94M8+u:-&LRtR$=V7-n]iAQE\6^qTgf:?L/AZ3@E+>!L<%&T''Vl!@)JLoYDgW*."p$8/+lWFW:%7M;
	]j*V%?Oukr''/KW__p!=K+X4AI_MU7?f9>E9*;@2U*I);uh:.-7%*nIBB3LHJL!)[VlHU''XY5D=#Woc
	t"E)t5e`i;[g=4.R<eL]ht"6/k#A+<nE15)>1(qg\$O#1ME+iHIHPLh"n4-8dnqiL@sGMrP)Y$TNR]=P
	8,kMO8Kfjr/Jl4B@#Gu*Q$LXpHDo%95:k&T)8DGu!VK6Y?tG_]V7lL`0p#h.,*Ec%>Wk''0\;rt)PJ`4
	H*:iB32%fjcW(g4GoIa!R/XMKj_b!>#k3>ZRWc.+8OIa&,qHlWK4Ee%$+A''bS4P''Dua@e>\%$m4jP?
	KtFPFg!t<Hah:&V*Xj''''W2fFB,MWA!.Dnk;///JA"hY7`%Y8W8TQGL/\H&DiDuoaic@>fV=3*:p4uD
	17p!S_L*?8dUIu!a!QK[@FMlr[qeFbnpUZIe3XP<9PhP&a$d@L;KkBE364RicFTZjeD`7.D!bB=W!gJN
	T''O.%Iskb0L\\%i-]j)P>_rIT"][D3Q\<R>eV$8*f:I,<F*Y$UW/]UGE:?5DZBLEecWdFlTMi]YN''p
	oIE_=M3+`Wqp^Uhd''=Hrq=s/LIuh`We*&bN<,<rmAdfc)[dLn^)mVh9erG@Ee/G[3UdO>F03#Rg&KHj
	\H@>U4=\kdi0^I&($9^+5"\YtB;\8t`]_)sE=Ef?*[-klf9cehp+F/?[usBeJ%pfcEk,u+40VcsWA:Ms
	P8`2cV=r8MTQCEQc"#FHR?G@/PqC;39FsT="5M9*^tqI4igInl(@r$$d0Kr\j3AOZhjq?:aYut<7S/WT
	dN\]sb+A!"_;"b;QA<Q4_Zt(Jgj/5-Yrf/hNmr/C^uT`V$bKP$;[m'']347<S=poOSYeEOd*''0C2q4J
	>h=)%ZF6O?E40MXNOF`,NPaiMgo]o5Cm.p&ngk24un_gLM*oUsS!CMOd5:RE`G\!&0BhS&fpY3UVqrDG
	kY=VpV209HKN,"@FI(*,mQFVCe(>n5Scm.8ujp/eG+9[O[r$ESoF/HYtN,FdV.RM5VEd5)n`T-O]&-8J
	)PXY\98-klErM)JFTZnE>B,3g\c3''LR3<4mSE+AiCn^NK)pID>XAKlL#CjJ_l'']ju)2LSkdQ3[/''9
	n:cEie!>Tq^8U"6k)QGEr]=cV*O!.fCb,=paV-\=B?/#`]Y6Q]PtK&RniL/bJb^^CWl9?^;&`5dNgnu&
	):f^sGo(nnPL$m1\b2$?s3BDD5A[Gdo>uIT(?I\T\)dcmd)1U40ej#ICk;6UCh.-685!gKF4H^_XNG?k
	R$K+W+?tR7JYTIB!n4]L%j]@U`iCZ&q[>E,9:OP>lGTh\,<hGdf./c@Kb5uZ2qAVjHIBL"`:t2;^Ph-Y
	SVhXV%Z;>&[G/FbC=c/b^ASs1G`U@UpJc\\9F:I<!&ed.bYALnM*WKi6Oi-P8f!]:SSqch>p''=4_cOK
	3q>Rl''l;f#g+#N8.7pYreMO1r%.FjT@iI[G!C,/6]IbgD^$b&b6/Q,PcmQPUGM;,8mmsf_:gTu<bHt/
	@OB?#Ln''o:)V]4],B=+Rrnc%@5\!mcf?-MG9XmGqYp,Pk5+`e]I8''nMLk.,n9gDmg![V63r$_&V<+.
	]5QNf7>u#+Y''''1QKBcr+&psc:!TWbk4%8NW7X\B2X]3T38E-[PMFHthqhB.Ka4&S!j$,RdK"2^,%9C
	TDODYs#=?$HP.9FNBjSH9S:%S.rkm^(hj&P2s.3TP9Ffl="n+7TK(a#T^-YMdD<m[%CN5e.1^GM)GE5q
	*HO&/b_;G$*f7?^l*dHs913:tW*=VHjc8RosBOkkORn^mBV?Sct/;X%Qa`0\H.V1G3CSo/c0WFi473L3
	A_J8pEGT)cM)"L$HU#cIVGJX412LAIt(+mMq=^r=opNfEeOS$/J7$J5<p\qdGY?q+Xb+Jb>7Dhu^<Imu
	#j=M[%^uGVkosEpCo,/=;Hr"T!;dQM2IGeK$b*="mlh9EZ6c3\@1*Ir@?QldkE`.ZG\cuhFT7G:V9\nT
	0[tV*Qag6c+f[H*IE5R*2ITnPa?;D>I2t:u()$NII`OmQf"0&&o:IY2_L"gl8fLj%l3tS]$%A.>h&Q_&
	9-4;m/=:I4[q"C)*7npE_HT%>5)"Rsj%St,X(QWbMYE:PG*s3?KW.''-O(qI$O`U^Da$XYY]^KWQflAD
	J`36n,[7eI<AY5Z:.J(4E?Epr]&&FHI03^:@u19d]_=q]\3HPYWVHkp%8''0JK!#T-7@^f-Ti1QR^Rqe
	>1@feNEh:Yh<5gk41:MLH91T)IHIpk<7=VT94Lp#@V(4QV2<0st1:::0%?7!>gO>Z%;3DSegDS''0+OT
	-\.EC$?U`n!neUK)E76LhG$ST3[''K%S`#cj7R+AMk/*b^Y:PL/H!rB]eFp+qVV6n$"Vnl4,J\S/%);;
	[(>doaM`L6Pi)%@iKI8a80+]hVH2%?Jgk?TBKALRn_]1JKrCX:mC7Xh5^mBeTPZg7&bDZ1P^S`XHgU!F
	s6Uo#q5K#/_dAM/A%X!cf+Va,o-@br7J&?\Wj%Sg0Jh%m`8k%cJlgOA*Eih,8?&"Jq+TitT-HN=VAL]G
	ac[*gg@ObKm=nLXp%eHm?AQdDcZ#k*mF]WYa-C$-FD87_KO$\\hH=>W0@%S2Z8#Wa?MjWs?R2Q"49c2D
	eSF@81^bk@Y,^dHf5#''G#6)4g5+<C/SFdt[Y(mF^]KZa?Z*+nVs7_9j<*oJC-A.:<8D_hIQb2Z%,C@u
	LEm[6g!qf)?Eu''FS!A:87HfDUE07f/^Z*Z-\Xs]f*)`HKU''D$Q=<+_QlX9q''2W6aeLn;Q7AQ:f/8^
	8P.@.ighO\c3=-Jg@<GDgs^B.bMMrj@-@sH_ZF7rpo@$lF$6#rY,X8Z2!>pfsCF_,2YBNRY:W,&fN7]`
	''_3''LGLT5l-GVZ:d4eG`$1P^#''eb/5lE5h2U?4l]aP\<O_6s''#^Y@e?T<E3X;UY&M=6?''AVp8@7
	BbGhFn9jSE9Ojl<Q3SM=r`$8f6F.eieoT?GPq6h?/P\Jn`$846PG_P/%qRC_C```FVDAXV,4_0;Ru(YX
	,7SNnG!''Wm![WTA''a#lU"fg>`lXeQD%jC2qpZ\k]]41Q%:?_Fq/HqO)=(o$i=PH=WF*HBq6NX+M2%c
	R;t5`6>@l:Eopp>YDDepR7+i04Q*;mVriL/Qc""$"]#B%LEnXVW]]==VfD9d,EA[8KC6XnF<+mX+#1M>
	/OWY=uIc\_eGeYhHdb!X>GI+T6c.B]P<+lsla-7l2]I2^F62o[aO5IamREa%IZE3A''?2P`slc5b^F8R
	Z[r.;-4^NtrAC,I[!>5?790>;V&I,$83bLrcL4O0R(_9;@\\Oe)ShG__Rc#PgKk5/Ln09%4;G''.3_p$
	-OE2R$hHjL^6C7&a$\PR2hE^cRL=h8$W%l]qMLa3&>1h;(e%\q8h9mFf$"(!kpG`7G7mNO?^)$(Iq@-B
	uJoB.OAXN(qk5HJW"C#o>:rp2#0I,Cf?H="8Q8&4bc8$aqj8n]0G?6:Q9kV8UbJs''8SgC\r)OZ`Sg;E
	[Xld?/uXr1p)n(b-1j'',NQV_4@;*H]D;?!G]/,(V,9nT<1fjt63@1YbK@RM]sOs;\pg1gZO=p*rr"Kt
	m_2uLYBmDLd@C6Bf,>N/MZ5;hcQ_3LU6TVH(IcZF%UPUY]?M85Rf1R)<;sWOG(/kWb3gqRI,be,5!M9Y
	dF?''U!J(q<U+=pS34Y+`/TcW%^I(ZfhlS9Ff5iNX/5-!=5C`.N#BHI6`t)C$`R4>&2dYj`?Q^RdEF4M
	#461V8O%+W=UV@JTcA_>ur,,hJiSRFq^\ZG=^@BUoMM9[O(e";iaIH(.''*lZSY@&\L_LabJ5DL5MLZk
	StocuP(=@sObZeBHtG7p7SfC:UZY5H-!^3A2d*Urd(Y:)oaU(m(5?2Ai7i$''dDaL9M0#`>j!0B)hhe!
	pF,_E$+pPTjMUSZ)W+HV]b9eiZZ5r-tlZiYu0O<u`hrK6kD+h)h6hV.jjg]2Wd^US"i&6+4B,rJfS&:Y
	O!W!k_``rnVn$\24[MDhM\!cCpqOU-/-NGCAKjGMRE![i_uu?MQ_6_+KP)b+5kl"WPKp\NLmtPFB-/Pa
	K!%-VHoT0tt9^p6fd2>8/jd=194F<*/MT^NMU\8"0SKqW>W+YdoG/iW-%tQ-VpSDQ#GPQ721saW2.m7o
	e_$Nb8Q>DMWMVP4S.n>DH*CkKU<(AslP''fSa7J/i`%UM0/uBV=Mq3o''&qumr-R61@BtFp''&&Z<pW*
	>:+`:q9H:7oos6@a`"O#`oZ3sC[@,.@;0%#,S.MnSp-3uWJ]fNIhaYZp&\jgYY+WVZeAJ%g''?aX*K3^
	-n0s''`8>i!ODXCq?Vq_!d%A8,YB]QHF6`p9`Il+UggY=9aFo+Eh?`RsT)NenQ*ZJ)i@j8?BH*S[Fghk
	,5Xdp52n3r$Q>^08.aVV3PYa1GhSm2$oAU\*RDqOlH-(AA7E9:Sq#>$l>@a`lMl,G0H^q00C)4"(''B5
	X+gkheIIU_0ErJ1#>@XXHn4H#g?cD''.QDsF$hS#lq#1FEW37mGZNk(;(:R1Y!!^n@H''2.;,AV+(SE?
	-s3=3q*E!,;-/@F*8,]PVan+4B[V2DsQ00_c@6_Wis5hT,O2$$fk;Vg1Nb.8c?[5$/DD/P2.<\.Ss.*9
	_iS;%i-=b\f3\5Qr>_,RoFCpcF4Eg&4S$2V!\%_n<b(D=8#_7-0[XJ89@3iNu&D?58rs3:((EV]>oMO;
	$[''5Q7MT6EVSpu1o/l=hp$h%Pj(Yc:9ZLCV:8ba4Q^TrmdhPWk[2NfSqrb;r3bN:)l%Mr/()]p_WIQ%
	UD5?2UelFQDS[r+?pTI=`k+,([R:m[c''LNSOqmQs9:B\(,gfgP^c/<h"]lRQj9Oh:&"mhBrL9K2QM,<
	XlY(R+e]77O1Ama^n>P+rL3lHJBG5..@T-@UtSQ/-^tlm?A[`UoK-Nb?DUXR_okjX%?IChC:j,V7a4l<
	,\P>"#Vi)b4Y\:4LIXh`3>Yp+-POF#QRIX?p>W/20"s/;YDehJ5N`<A/6-jjapkCI]t],3_,[VT8+)DX
	6W2j_e*_`]D1jGD''4!RTa0Jek!_lBq!nHM9a*Ql60Ef::A\fT(d-aLUk$gIerKLF("pY''%J_Vr!pg:
	$m&;)j0?;PfTGJ;fqpq&9e<h''4J\VF4rIq,]@k-:RT#=BDNUhI''h5;7j_:Rb-`>TGEo)qd\`0q%`!_
	GZe+W-kr-&et&CkaIDgk#bNDVI9?BE]<_F$nGftORrF''-A!UbU7o%J-DZ2FBSMWDlilb^Z$''G@MZnb
	u^KG*HXUIh%G`P>X''auZVfJQ!!Z>2himY`8#&7YI?RJ$h0/@eM>eo[H85tRHl3SKV/=b?#2G;Q0S02p
	`"kU&RF2C6:L?=pFVeWUV\0an&e-+JX5rl5cUPab!9^(J(0]30-*h0MikLE@N`3K&Eld3&r15#aEsIKD
	RmdChAYOSa-+C**Lmm!=JsXe>''&cJ!#X;L5b+t[$ne#m][Oo&#n5KY/*QL6aBM%o6''V@721Hj_@jPX
	W0*Af`_)4Cm.<4lGGs67X1f5647e+:#cHk[H>''b94T-,Xo._3P''(oEkEsSb`,jHU!(ZNOWJ*<C%[YW
	`TtKR_gV+$HmP;oAE+3ls.9:W6SlJrjr1!3%hs>StXqIn(mIGUY;"X)>aRkU"jY9(gM:.?J3@UO!8tF/
	^KQ9D9%2nOFhOs$qUcobeZ%Z[hK=44l\/nB6THF?(bkL<fV;8hYdbA="M&bSK%3RMUV$A5Lk''E8t?CR
	PFVd"dOjsklSdkQ&F]>=RU=J%4^s8oXr1ZCK/GE_5;@s!YZU;m%hiL#fX@-/4FQd^J+R%/B<`fkp.Hj%
	HbYab,j-2s%HFdg0.8HANW.jGM0R!;r-''=@M,.5&(h(]RLuF_T=\E8\N#&XKUI&i''P"TE,jo2SKHg*
	)3*DUu-Klpa4<ad#ih);ce,?]6E3u?bISlMBI,e4#i>=.1UABtJQZS^3g-gL8:7mfd7,?hmp;(L#"pPX
	KmQU^r/POlmY[*;jYKT6[G3_PLAgrf>I0b$fVYBbp8Q#%qaB!;``_@)$bK4MGZGkndA=S^(u5/LErgQT
	\;StWK\%eKJm"+)phEHYB`Tlb[*)0j1:.Nr%k]"F,=P^L6^S2!tNUp=-h0.@I''XXghnm>dUZflH]]Po
	!9/^21TNA''>n405ZiOVf.G8F?Y/_2TOosK[i,Ah^tW''*M6-)-)?HsZc?!6dc8Gcs''Hfn,9ahfH@#.
	f)"2(]-\Jb`$6KT9^+CP_(/mMmf*->Mc5Xr]m>p4dmPF7abm@0]O::S(K2&&#XQIbDIhM;)^a9DpN2+H
	_`:B5[^J"+/)bJPj<+?Ha]Yoi:1W<WWY<X)INmADih[qJ#C]&XYP6j2aMHXL.+Z2MK0K1$uBA]E/C0j+
	0hW6*]Ib8Xb=.3<IrsnuUIan0ga;8K4B%Wb1oK46OA2:VuQO(/!G@E2]V;8>no6QRJAnb)_6SY0R*"@k
	R/^M8,!4IX*9JlW'')@+TebdJpbU+5$=([:;nLDOGla7KG<9bF!lFu5d@D@uJ%A%DLI54adRZ,9SVa(f
	"YJfnL\0,\CTfF6DeLbN`fG2B`+,RD_Tair`K4$:tfYd)UL8s+r-Z3B@Xq-7gC$h;O./:P(8er3K)Cm8
	iX=gcYiMRKsI_!ab13<B`9`k+7r/*+dZM@rt-%T*;u!hWK&M(HHh''lJc!XEIW$h4sA4JGSHP718[GUO
	4]aa%Z:"'')<rqH2-h.ThgeHL^8U-4GgqngH1$b28a,X!TRG$#KM,\Gi9d4;3lq83flO/Z!+B!o^-/KA
	GhX-&cAdVnSUAcmq=Bm8rK5fEM(Q:`$D5V\B4QQ5sP,)4N<$r7E,[Mb&pK&.<71OM`<,LAK4(cJT?*H]
	<J&RMa)+aEs-^mJ\X-YU18K*GllC+*d`p5\Ij<NDuUinR)cA;T(<P`fc\t`IVIV%plt(TmI4D>pVNK[g
	(\Gk5T4<W#E_GOVh0_&PYDT.EMZU0BJ*iA:&s/@3<C44cS!n#(Y;_biP1:B:GU6U<_XM$pDNT^IZ!kY#
	/!US@ncM&?s]78/dpB-pd(,Q#H0KR1,&:UUSCR"r>1kNF''3P&QAdUaqP4D)Tg-ca!kVGG3*@RNIid>R
	A6rRr4r\W&Nn]uU>!/N6d19tpr]<k_ZU^7b1GTn/CV<t_,@kU*F!esVHu@n6,&#YAI<Nao>O@d)RW&?S
	>\AZl)O*C_I.B8O-oO:KhMlWIs3=c9s3;JgrHC*S837eBP&+kXk*(LD2q!_1JcG./15/;!cLqX$$H#27
	0-f1''108,@Egp5,''Q;%@O5>5T_k@B5XF[;b=J"K=!S0V`fF5/dc"Ael*.W8''''lHg9h?*6#\a3H\W
	g[.05H#.^\<GRg:QQG:Dp%a''cFo^PaQh[n_;?Y22-(l21Xj?3R8t2sR6`H`dRPc0_;>k7FrroG4knN;
	f9-''EV=t@:_g+Bg#)%4[gYj(GmmKD7E*c<sO"p%:1O9!S`$TNkd%Pk=]A)%VY^J<]c#Dl$Sp@^(b.Gc
	AADYrW[(2S=c]$sg@tJ86*$<AT$8R[M<Hno4B>uKFBX"_QK[A@tZET0>Z=u7Ckod8/H*WUnph[g&(X]h
	`<dXf3ZJr4#bVg3XTnf]HRqG73T7%30K;TM_]9XrNSSag_3TtbfBQ<t.oi5i-/F\\0hYQb^YUm4I>c<T
	u.k_D$Ncc2R>H7^`f2oJu]H.V6/V/,.(HH4HJB@:C?83P:9D\*R-*@o4I;?d9ema9H9[=,n#@D)lA&Mr
	25Ms4iO?GS?:164REuec0a%`s^C''34>KH2)0Vm#j!qcKPaqsg&]TsmVu2mQ.RqJ[d*Il2Q[jR+%;Q!g
	fiH.!e0poHAdbT]3Bm!Re-iA-=.F[#X/WERt4),DM76PVk=ed72"Q>7O!J]#(^$:]4l^KX2,?.e$o$I+
	;/(<J&3R#lePS3QlbT@Yk0^D"=\"jAh`Qq4)+q8R&!5`!nHs''QL>/%lNb\$B4A)F,>)j&2?CN]p%q$2
	11[&a$:%GjE>CBlVl1hL^>''Z7H1:D$Ff`MdcLp!]I#hF6=j)o4S"i8O_D''/)D^;lj''plruGTd1RL2
	DbAr^#)Ji5;-b.3NP(q33l_hjj<qMp@REjo<9D%-3IK=h&O#G0Q*(8$;7)C41=+p0XkMUPUruIQ&KWfi
	.i-f--(.!IGD]cRo8WASij]Rp#A!#(V]D%M(WK>qaArO"bisa9EJ1XE!&MGYbTh?`BLr#qnGuOl?77FQ
	X.59qoB@@HMp03B&:r`=h?0(YoAZ]C-+T]TM*cZhJ3mS1l9n9t7&@ogB7''>`?p2$Yt]7D9jh[P;5XI]
	(KgKM5P/u7S@hTb2,$3)#1JCl`k`krKCg`65%S\CG]h8_EIO>7apAO!qt?*bh=_<kB%[c<Obf4+%n^^A
	l0SUTSWg@uP/%:L!q_lEh''29F5jm.=m(k(cIuB,+26!#6rT-K$I`caF''lc%Ph/g%-^lGS^T;U3pHS9
	6_MkfgS&\.Cu+_a@Prm_ge#s2qHFD:(/P/CAib#kebN3S@?1^s,L"r^b,=/P+/`f(+NHFI9*>;fj%s4S
	g9d7f(8D.,R`Aa<Q]-RZ=CMc$j(J#*'':YjbK_MH6\Tu.g^HZV&G\n5GLIN*.,@$@c1Ncs]VC2VWdbZQ
	BBQa:c1LqeAbkfW5ps,!S:b@%^Ddrf2W#eZ4#(Z#YHVZ7Ss\K=";+E>7ZZ`Y0''''b\Ho[cACBg5(VEq
	I^%DAi#5,OX"ouI#SNB4[c:lrIk!PBOq+\5Nln1iO^^[GKR''m7onXLiQ:98<.V''^eVjZC8,r)_]Zia
	3GiVh?%^85Nsc^]b:7G_shQcrk$OK2'']I9VpNF#(HYepG[diOZZG3bTd]^+]NO<''1oMChY)$<O=@TO
	_6\\=L*i5RO4Bb%?/;RA\E=Bi.,NNZ!$<2[`IK"WS=daJRa+3c[-Taiha9B*?GSBl@UAVXESkVmh''M4
	l^3/NQ&<"<SS*aP<9%OFmZGNV=X4Y`t<''nsGN(<"5p?o+S3""oJ"fKR:UfL5I%(2lI(YQZuO5JDXi7G
	+q<5%c7SWnE5TZIS-s^O+ch*R!o;EnnP.MP._(kSI"4`r`UU:6k>,U#4L+PlHce&Im/c'')!Zu5+sNd\
	B9pK_Y@r>8;K[kGM]JW''.9JM.[n"''U4YnOT%2uE_UIY9:Y*X4L\\r[hu1#Zo\V7$?NA=r1&sZ$0<Ua
	$K*J\L-5Yj[j"-/,)CDXta`\,)l6p.4`LuU%TT"NV`Zcp/P]lrJlf[BbcN<*GA!P)B>9VN);,ZJU\F\l
	L"k-//SPe*f#Fp#;h<U,GQZl]I@TiMNgn(ne%cWT7M4XUd-N(U)Z]"A2Q%dr1P*VN;O;>2//=0Nl&V$''
	kg-CW(M;jTdc(hWGGe>ANXiF@>S0ef1Z$E#r21Kde`3[dfIeNHtGCfe^^KoXo4%]59Yb-7hm)$ZR?"''
	eQ[^Q''"0`T,*I1cH?61BJ7@jHDEggJ.i6Y@?k''J)#:Q(K=kbPbsMnVQYafD.=o8EGpf1L7XPCGpB@]
	?YZS_9.!9k:^__MX4impFG"?4;-f$Y@g:O<nKh)$<f$ojeqAo3Z7D!)"0*Nr4?@n2Rd>D.+''iTBVW+8
	_%[7eI=MeL>?(SBjc2DEo@''g3jjCd.lotBA6$0[Map*p3e(8h#V]-<8+0UpVJ5ooff8c%nm*s3p@Tnt
	u_AMYs7b,U=;_@ck:U<t''QS#16:U5F-1EYU.J,\dR@aDbkr-(FjMO@''4o)hW0''DEi3cdsB4ml8ON(
	C1M-jY="AT9"u.N;[_)=EAhHjar>/M?9-mDb]#$Y]XuG&sb\gV*`h@3f''rX0b]!d=[)V%8R]_/5H#]!
	?]467K''%8S^OXiN`d`2`m+K\Uh[M.h4r`W''gV+GR>uYDJ3?4cJ!B/7o$1[F''q0-*%3*d2K"^1poQA
	?-m1?_MNS<891$UU\"`8]K&n(U*pk8)_&^/Z$CLKDVeY;T=3=(FQUZNn1tOrF0do4J_kH>7,AR#c8"fs
	@78^KY`.A+\YiBHh[=\4kl^H8:j_^Z[:*s,I?)GX;u;@-Pu&]tr1oaBfs8#AEJ3-ePjn=7c_GE"FN`cC
	V;\#IC?%\d,)&i''Hh#D7"Tp\&^8''Yj9&&hAqR9?4,QZmQu)%-5WSc7_%WRBDq2Cr-(g,haR@\]bi5V
	ij-DET!3T#niDR8j9>?bb(&SEp?/dIPM\3!F8*"n:@4O:pZ,@UI\60s6oq>]llaoOV(mmE,\P2)T8cff
	\`4gu)''hY-pe2s>oF)KnP/W1%''7/6T,c34A-S\*#GM`#!YH''h`B(4[]r!rBNNBDD^#2UoE`-\9;cg
	?Y_hZ_;8`k"lVnDYM9\JY"VIU''&F&%`[6''h2pKVS%gfa5n^>NCR[$oA=l@?!=jM0s=*#pS''4O[^9e
	PA,V>''J*4Ljr-.,4D-fGrnD*R+]4?bP+&5C7OPco#B7lk17VaGc0.,K*AG"WEM>je''I`JE&-5''aMD
	@9g9%5J"Y5$OD9W7ep"_$g)YVl!I)bk>fD''rdgRIH.XM^],q12\+sS,eIL)A)q+/1G]F0]=I6?K</u*
	ZlhFtr&6XG;1QNn]$Unpf0ce/a,2!C$\`iip8kC8ND/`DWG/)H^l$Xf^>AC;Mqrk!elRZP;6$9?Cq(QJ
	>PUh#-2?rgBN^SYkX@Y1Gu1,j\3iu$%CC7j"^0m]:o5c>\5GMLEClM<SddiHEAH.Yp1u,Rn%9/(hpn(]
	^\aIe5AU+pKT.*n2S15HnbtTcad^oNH-+&=rA?6!;+^CGNjn17!2;ZYHs?.!dY\Bb4WAFP<)m''4SJCk
	"Foh4''rb(cY%4np$iY(U=JkJ:$GRF#l_N8SC,,5q7h`\QLr4Dbk=WWLad\c*N9AE5=`>6P2h''Mb[69
	T*r,24t<XX?a3]"%+[:A*L=/ZkU<`%-.&5H%OY]p;5I@SBc$rQF4!GNVdA(Utk#o:RjN335=5hBapS6q
	R6oj1k1HfRGhV>)kb(Gl\5Q''A_I(n>mKmTCB.tMl`P!__3Hc2GE<i'',+(nZMZf8_>/:R9<0EJm9-(Y
	M=%7lU@"kVU*j.M/BND9#0U;LD*F@95S(SDd(2Duci,KqDYa3JmF(:A!dg5GK4hR$]RiUYH9t7Lm,PJr
	^J!,''^>YE+B[>DS5mYm%X''kX3qu#-*+?nP2rk&"^k:pRXb9?5(),;K9L4f%1(?=61rHE1QSu$F4;TN
	1>_38s1]:\ofQA9\4dd"rkH&TK)\+PT9`WM"F"bMJ89D78VZ$P!H,b*:2qqf_"rnY`KcX%H8[WD''/Q>
	Fn<qH5DUqNB<RZqPdf"i''.Oc%#Hlb$>RN:"7>*(:t?YZX,Kf]/NC]CkCPUCb;<(d4e$E-Sm0FN>#5Gp
	*0+s3WecoB\p1ci-8kPna,sp4[6!>>opab@QiDo:c,@''p?@L.=^,3Wke12VhsKklI[?beSGG4/.qEfh
	Uis<?B7H4sQh9/eXoFq+c(Aj2cZ66_KTj7WA_IdnhfXOB`VepF\$H`Aci.)Gm)nk1A^''K!r%;J+L3K''
	Q@$_:*/@PRaVqA=Q;F?PM*tE8`_sp,ZX.NYuI^b:8G@>kW&?NZQP0?>!''-m)ahuBi(oP>)B0\^P!>#m
	kgT3W=`[uC<1Hi&HjXRP5qVK]:N]D2ALSYBb2fI&Tpbn9QYE''DYVbb''<smFf\?2#Zm;phWoPOI2=O-
	;H1qs''=g?ZgAgG\/[Y/?h(K!g%4jn/Z4%U''BX0$??&83>2)=7n74,Es''7#)ZdE<#JTbI^@8rt8TD/
	9:2o;0"kkj6H"n>aQO4-s8#@?>19m"2"(>]#YNPFPlGs+a+Mpkg]7kPk_SbM:4Itn2=;3=it_b2rF^UX
	2c_>K0EM^tm7#\\i+FP_i9oirdA]-P/_cCMm%l3#>F=hIJ2S,]7";(7[Ol$nKYfp[k*.b`%P6hq+(\L%
	N?JCr81>s.q,RD=<M`E?G_H*$.Rcg,j^mFg\"Seh7,5HI<d_r;I\!H9i(Fp;Pq02O-[*//nf$Yuj+f>8
	;irQ1JB[C/u"AbNmVr[:b@\)NpU</",<N<5//q8Suo^Z4ie5E,9''ITTMaXe^&''pPYntQI"k/!o[Sd8
	cPken]C[XUqqfK7''E9]^nY"8J;#`V_#qfi\6f(-eRd@Yp<nne58uLNU$5.:5tmh"W/E)(%e.cpBX4KZ
	<C\"ST/A9nqUr<%"U?-Y^$qiTCWA/98@n:ejUglYF/8&0b*A=W^C05t]SuNie;qdcr@n2q:1],L*<Rpg
	r4<@H`g?])m9L0;?.F?f''FAs+>afPE)lXB^Ui''_._Ea>e`_-Pl%i]2GNV,K]`B[J9:$ZlV.LEJ9TUm
	''5EfioU<gHEL6bm[lXgnT4!Z47;eBWr)<jYB9RR1,''M$b"31[+uoNQ9/dpW)rJaErc7F2P];+4''EO
	jl>0<s6''FPqiHJ6]/cZP.b_''RQhWYIk$/)4e>C=qC:n.,3shUoWndK5Y>jqq>35=i`*UB@dVN7;U>@
	9H$fK<?fYC@CDM<(4&mOj&qp5,d2n&?ULlFaK.$_aGp#LZgCZZAU`:BW&16K#0Xst(L7?NVAo`2IS>[<
	DLc*)q:Tc<@NO!V^r0p+/D"-@;Tcp+*%XZUhfk#SUX=Eu?1(4S8[r-CaoRoikW<Wc-5G\,o@*r:(I+%<
	Lj%ECfI?fChRYF)542o+HUAV?Uu^m6+P.S&#M[mh;F6P\QHZ&!#%#\IA_4V7ha>nm8cqftTDL''tDdbR
	><6386=BQ=Ehi+%J''h[\SDnk8!YD^a-fEK;Q\(b>@g0h=gd)mfJK`F*i.6$g;4#H?,\aFhOk,^aGEmB
	c/K]7^dj4eKJIM<7MKe*N`tIY[A#OqDblA==G''B!Q80CMsioflg%n);EHl#k&=46\nV+)j4=Bu]s5q;
	ZISD!0YBO@mj-a,g[oPUaY]&!]Dq67f<[^)b61d7@#-!RPI9ZXHbpKa(]kp.>Q_CYBFj<9Xu-HqL+WO4
	XrIK$@T-G655R6dfuPqUC]meN.XO(3-q6EOT[L[.AM9$g!BQ2*K`4!u<Qn`pV"aDsFRsqX!UquK>6#>A
	JZ\S5-B''$VJ;NSd(0HMbRHB>7Sc1s9qKHa^Ok<@O@$\L*S)<<1D`iR,9$p<cH=Mn&-''EFX)Xa9q9P"
	[e[XefWGHN!9]9@-0SHJO>WGZ#SZ0eeTiZ0!&B)8p%nTAX+eW8%hdE.\.pOK:G;3KEp"[qu=-(@Nr2>V
	NM2,q88#O8%oA8O>PGbZr`(T(J4p?uhtfcYj#DC$GTF2U.e5+;rZXL`<!:#W_FTN:/uH/d."]J0k+=8+
	N,8kf72_n?<d^A0IhDlCXRGD>OhFBL_VX7]Y]F]rIeQ?#9n@Cj*bFM"IZh=jB"6so^;>s3^MFZ#Ci^gr
	NLL19VS0EVmiCK8+1D?s''0+(9W5J/Mc&NNTc;4A&3/\[*_f`f)WC/>5ik?GZEC_90F_mqP28h2-C/C5
	''k/%d/X4X8@+B7n7d:"05HjCrlYqSS2,/272HF><o.\=o&DPJj)^n&>RF1eJnk_IIgU=Fq1u$Gd`VB=
	&WEn<-[P+AHMc\KN4B`+akIq9`O21''HX3GGRI7Ak^oq`jRH*]&643`o^A72=,^.i^9MI^bo&W="%%ii
	-$ee5`Be</2\FNH#tuO8puCL$]7BS@?EW>>91)J;qSVsl5K_7/Qj_1#oKg&87dd!N-`q:T(d7pg;hTi]
	QKF0CROH_=66''._`N>Ba&[BZ:[,?pSQ-*lQ(/K:XO?Z((+0)Zo8/EM/NoVncZDem<s6e,g$l$Cl&^n+
	IhfZgoBDbn&+2u6qk[+%3q9R>^4mV$c!UJG[!]5XkL%qZ8.(nSWe6f2M>((#/MK%<''BlWXM]l[To>n_
	+)%YMoT9?3W4U$m$+KB@aNdC)?P5/H"KjpKpB=3#DcD.[RB<jZr^mQ=UdDNHMTe@B).DYibJ)?(;:6,Q
	UFYXSQXm7S;Q372d7>&hd[SMce!*;/d=I+DObaok1HHr;U?Du<a%YFm?!7[e)200dI(hZfi-r<W)"!oT
	%P>tlQq0;$q71jV+LWNDKj"e[q(!9gEfC=h!&\ceo3F&s@bLY/64%7E9k*]FjkOTp?An[Il6@+>9aPb\
	ObNEfP94S8e)*fE''Y'';e4u$HB7TLi(*ZUP%NiRB?Xacdm,g^Z5!g,IcWr8nq\+k-MVGK+dr@2S3E[k
	=d@*U1k)R9=r")%/4,ZSq5U7#&6*n,mV"\isuJulB&[ZU/9+edSR?=ePL(l_B-H]rGD+dD6AA$r"rRB!
	OWHGB];UBVmGp97@>h.XDW8Erjq6T_X;e0-iDWuiPS*FCRk2MeC.D4,#b:=-*k?\1_Uk=e<r_WA(td(r
	Gk)^*I8P4$p)-BlZh#,J(Dd2@RoKa<4.S*n?DFQ2h$^ri6QFn1>LN=0jDud(2jW+)RJ!&ZmK6sXjjn&.
	XqY-4^;&u1M]kr;uI%b7+0D^L5#ouU%oZ"3^''UQGm2O_!i!>R&u#sW0O&A_cTL`1j$<,`YV@ubdO50F
	P8jF*VLKC,GHO.L>,%Z;:p)?X#blf!Q9tJRI-O8l@[7SF%$sjlIl.0ulDC.i`K*km$k0[5Wl3">Kjsu!
	,/;;?Il<mppmKcla/5FKc''r(d&\rDVgd0EEU"NiOmXH\8ha:fQ@@i!sjYbZ)''D61N^`*MfIDn7X'',
	]&sBb.KAh"/=R9a&34)n?FD)UeVkq;]em!+(`''*m[#RQriMI/CE6Uj$4">1Sjrk$m$]*rB%+Z.r_@IC
	$FO6*;Ro%5J;TS8QG@s.R%Jec15L!I:Li(Yk=!PBMprsLmmB3Ig:VEf(Y-*BruREd.>p1iU9Nt?GKf*q
	%igN[U5)rD_,T<g5/[!p9=J_?2AbOc"@n*b*l:?[%N(Z&''gXc<;ui/&))Iqp84ig4OZk;Z_`?)F4@rt
	KYsE7@MQ7C6LnTZ(I3$(5O4eF5dU6tP4G5>N7,7h;R<aP7(\`P>WK[Y@4"r^8=10R<e&@1hUk?3J7J/B
	Q&?#cUI$$p/8\laI,:G:rbTFTLMV<%;;@Ts0?GMiejDU!k4S2Qs/_*%WhqYP@kHTWcVhLRG9VC''%:42
	Mn++Yood7FhdXRSWms%!rKePAD=&jUM88Fsi`d,AW8_1?;X/j7JC3#G0riCm$T<6<f!d5WX,Cub)[$K/
	H5?C@Fchb\SV,p>aYg^EEpcI`lP>D1Z?hn!6qKHsd_15?c(J[I*]$N"\YmbnL4\,I_=9qMKhRU"of:i<
	6I(&7r[3"d?i6P#F18l+9fO?,Qn+n$GGt%4?rY9X''[-du:c!8")><*f9E*i@H\aJ?(rOX2<H3&mnRjc
	je@ht#abcgMu5:_WsWHP0SS<Ya)P#)''nhYVL:Dp3Gh,V`d9NCq<cNYAZJji2dNi/M;GC'',HGl8G",f
	pqg*\do/JMEpAkBi!=4"MQj*O.JH5=X)+5EFS#-YS#/,3TeZM[suHkNDn,pF]s"O(7;g''66&0l:AD/I
	ZY8=d&m$)''Vu53fp[9BBK''D.r6Jjc2qHOr>Q@XhWGsreNG?2-pnAZr_1I-`.nn7RK^Q\2JbpsE9H<R
	?._fiiEgQKs*cNF0,_=-0(Q_e<R*:mfagqQea`jX[#CuQm&4DV=Bp7r](<s\f,BF"&eDA5=s1a[5&?(U
	htT(VQ@o<2!c4*k4L\PEt-U]Rc:-;48BO<a]c&o5BLcBq<_o$47JF3:-JXFJiZ*gE&<-C-W]h<TqMfFS
	?qbnL''d3\j&Hf#:*(S4E=Dnn7RK^N9>jZ"rY:BS#on0`SZ36[nNK[9hh3,;MCEnnD(L(4JEKkB@0#pT
	GF7Y7Pqg23TmQE>"RH;?''1KX8ZDKpJEBD.&4>q@tbXkP$@pe=8C\#k3L^TL#9<IE/>#o&XX##TTpLNI
	t_h4=m6!&Kl_(h*:h=GodISEBKk9mIYEbNai"AB5bjKInRo82FrXW/8X7G]Jj4Upia&NT5=WYrW7#u-X
	([83a[5K+N?1+fs,&VFS''T7R_3U.h''iWn6p:\A94l6.LVY0C[s"8jjkB8&p`E()n$Q$O8Imoi>3Ydb
	?bK7ie.e\\#pGlF8mhVpQ-hZqGYNkqbPXViWN]#s$p0!#YcM=Q#G@''[C"Bs:d>K^<INsksR_Y3FT*p)
	6>T?kM(i[''mt8lkf[?1L)7GWf2422`_2R2,sUpA":KR`;*V^Rg*<rXM\X(C,[#PE7$ZBA''lNjV_tU&
	q(X/Zr(AX/,6Hk,JS"_n$@ZDcRR6n76f.42<#8KOsB_ir5(a.B_s#"7USd!dZT2HHT^l2(Aqh&jSlIpj
	./2=g8aL1rY>FO4,L^._\UGEeC:fb_;Ce?r)od2^liIbHY5_8jSim0+uo^SG20_-/o[s9bQkJZD:-Gj=
	fh7g$N+2qnZHSU160,UlWL-2B*+[QF''!Yg%Jk9bh>,S[2SMOpbo&L-XTq9%rp4Xgh_i`P<I.JN2fa%=
	<)RVpgb!qas8.#ABX!WrEXlSaH[%]3$i]KIp[;;TT<3/443a;?"?\\D<uVh%R6@;DSNlX^2<^;DIZhuD
	B"@X!GZXBIO0sJh]KV5"K2MeTm8Rb=/PHNr$,0];1VYG9HA@-)QI?D">1QLHDIY''N(&Zb9qfbd#UG0?
	cd"B2=@566K_\sPEjS[:b4:5-\$B^urV0YL,MV\35i6Sjc^M>mF;g;g44DWKQM)+KRipK>/TCpR77uM<
	T%PcOXIklRd>A_3K?/Y^QdW9%/cDk6rI=X=8L-5s+11u''f2X[.(,(W@;6e`%rQ72mW;u\KC35960s%_
	gR5bddl>")YP]%d3!$''0:>T*7Fkp$@V9f!F#f.&j/JRmL6<XuL(l=o\'')VY0Dh`Opdu0:h5GZ%_".T
	9[1p.U572P=GA=B>!US0sbF(gWU#bf9>9S5C<?#_4MIV.C#OMC$%^iRPbZH7.[ii]VG/$p[_Og-"J,.`
	ifX1:&jWA]''`J2ia")6''kta`69n4RQ]6FGp7I&n)ofMe_:a3B!Bcjt-C1Yi2J.Gq4.dh0^u(\>rB%i
	f>lbE55RG?Ig7V:L(TN<0]\2@iPO<h,m%GJbnZ<UX<_Vk#<mPE"_OnO''AFZk]qC''_E9oUHo[6ki39M
	:_MnU/o[I<IKOB<en$8sK!KL-M0"rnR\7VY9T&ia8DTZW[p_=OF/DB-7<K*,X$_GG^r`qprMlQV9kp_q
	&uIr)oO7rp1$S[ZeeI!XeXP`LkEK+*b\''7R\iB)kRi7NWqV7hmKlfV?\F?NO<H0OB&W5i-%8Q#QF''A
	mH&b5+7lP9B[OS(N95>8kgs6Q(kD"$>b!!hkg9.],9ktrVX*Y%jK0JfVXdiV/d%*mo8;>qn''VZm@_0g
	m5PJ(Dqfag1A+_Cuph`*_Qrm@AoJ^?o)i3orCYQVm-It]WFZsU2B,.3H\e-U@&,m''4%FGQjY2JiQRf!
	*??e8FDb6pG5f[I^)9#f%FB0(J$I5_23mQ@&d@Q=uUU`L38''K\95rj+`DdJWE;n)@WZpcG7368HYg''
	%lCqD"R2;p)5T^cAM%XCh*:s"ri''Tf07CW^$7(FiY5F2Q`GFuAoss&(7/0Q8,$/Iia%KA''lS8KJZNL
	h@mD!fcs.gR"h3+7>(,GM;6TEFq)G"S<#2;h9HmRS=>J9.`8?hd"-5&''\mtQ,^psusKp^Kkh*g-L\t1
	eB#fBeij$5>JJcotkZ$eO,q-Em3ag-kLDs;N0''Ab\TTI,lenX.>Q.=L7-nA_Y_%1=3FfKS%rDWgh$Re
	j>Z>3t.Or!jY:46bhF%;oVW3js4h.?/tKjRqm(*@APmD*Db-n$MUb@&:''s1%aoe>4?cl*h[WZI?Rh6Y
	)9juGqJT"ja:+#*4p!!p%C9''RGh"X"!YM79&j4S:!r2u3Y:SLnSWa&4$3l1cXKO7(FteLVOhY@J+c;&
	N@QZHq0^c"\t\Hg*LkA1rsDI`OEmBjI>EV">M<B!''^sp2U%,:&+.LTJ:0/H.<P7o4Ds[+L[.L8a>.F1
	W-Ec+H(:^N7U&41L^M)MMgUa"IHdb#B`06"d]/cDhXln"..nD"XZTWY7^_<:#H7)f&d5gEFF0BN:s5D_
	(kkPpE]WV$8\a1Kr=E]HnK?3H=c/R&-d77?dFZ8-%lgLSVJ''hS/GNK@^4-+K$K"enD@pj/Qe9q9kiJj
	,VVbDHAK''!uB<Z^c`rUD>pEd".IFmf9Xm_&McMnot>k.8?hb22A%ZXCd3<McifN.$*V5$;5+HE"9CrP
	Co#Dl+urkdg3?cTD1)?_,^L!8''8j+S94#SPiYFm!SL@m`1\+Cg3*+Y!"i,.?mdh?!c3A6\MW2H2haYp
	YC!cca*o)W;d?hna85pl<,P.gg)OnKf8(MX]*OemB-D_"g2JKJ]]4p4(ma%1Y7WT\pGZH1.Be;''`4!L
	eS''DUe8LkRpUPmd.1,.<NcSCm=Hn6&!f5fm9on_XpM]ik`oo(![dF+Ti_%PD"23S`<P7qJIl=_9%qb1
	+FeN!i^O(V#Zg!)''="1dH)Yqnk?Q<E.IdsQJ=<em[+lYAKhq@ZR#M(1r/W3ns''W#_1N#r;&/-B9.H>
	2!B[^L=N3?;''!\<4eCZch4a51ur6J-Tt^,%@/05MMo>cW,)C/S%lmd7DT<STeB;V=F!`JR''>iNHot3
	g6A2@o''9`[[@1/8NB!c4/P_buJ)NA7$N(J&paN`QDqi&%iUbgE5;N:SSHRdY/e[uNB6mD>_K"^g[csU
	Ohi4Z8>nd%iK%:YcMUTn(^L(4eht9mS`hu#CXYddC/NW0QVrJfHq\8$5-GaaMkpkW]e-P''9O%s,jAn0
	pA?f*mC0C*6$pOEKo?,?*D/,[gS:,r5JWq"a@mSXWhiU;A#p[=.=7cW<f"o]SU](!F[8Q>RBs/??RLj3
	XSmL.Tfp\2-rcbdJlce9XJ;;Dj6Em.tp*Zb;sE/K)IH63"\(Y=XrCEqJhdM]XBH<AL.X",!p&#F,+k5@
	A:p6fg0Is2VeI._3cDnknNh7FK_A*hdO#<T-g<-2QuF5MVb+5;5,maJiDptEEp@-r&MZq5bQ;j9M7"%=
	s-adQE>M343FD<4?XjeBqmkaliJ^18J@cEl9.8EP-Qi:!%_7Gub''\tDl*+Wg+)A"ucmn$X5@cd]bI*V
	Ag#7,jDZ$is^q_mMuf0D1[#b3q1%K(W-gBu*[[1XUK>U-,b(2M!<Ce#$2tkeVb)U9,l6=3Pd6r2r0H=Z
	oF0Q/*@a/hKHbd<.m!rO_=AJ[K:Fmet0TqUPSP^WuLS$bnJTnDqC=]i.jFmW`F@RU''^+VSoegr0Z1p2
	)+O6"/F\SbUY[0fO5fi2Ciru[m_JN+l!3df6K5r=Y1PL2PDERqE[pC=fM.p3o\R89(?S+4\PfVa`Q#1]
	1)\b]cJ2b?8QMKg0a#hXG,Qr/sRbUIA<W:C"`)N9@J<I(A2hEFjoMKo[hVMns9j[05\k8a/We.NC^U(-
	$HF=q%_YZ[A\uJ-q''DQSpT*:4%!*.r@HfNaIe`O&U''PK]VLf(**.=fL-L$lDr2s+qFgc#V<J/odA-_
	''h(/r,-b6F!Jc=5qpifI,0:SiZBQJFYC$AuUM-I_QW+SQ7@t$SKJW>bT0"^UPRVr%l:Fjr-iX1b;4&O
	=]$Gt4"QhVp9O$[Zqn^62?qWY"ah8`U8!Z][;1pTl_Qq)Yi41KB8acBrC[#Md;#Hp;UGL"A"PeOD6_Pl
	T2WmdsL?BE[)4pp+V!sJ@KqsLV''l!(G]bfODqc85H9go-m21KpIW7K7QIrpK"Aj&''ujpJ3Hd;;0%h`
	Y-0.d1po1^GP>u;0m]NGojqnpF!!Ls4KA!YJ@S-V@7]/$"1%ZXR&1\EHh2<fC&KQV6VLXh=N>X><U1''
	=n[n7p:17L._Z-W%uQc"F>qsh4%OR]KDhR''(+c\KQ;l7QOIkPt:Z>Yo[""=R(!Y@:pG.2O7&6fp:5[]
	N)ODk+GI]P%OpGAi3*=q`pI`-fmV''eP@D:&CI?ZYYk-L@qY,c[rk:g.t[[$jJ#th,dIZsT`JE5&9%&:
	ZJQF*Upml^d[grfIAcZ_f&MoO*@d^RsQ!knOQ$u[\:m[j7#@\YWQKn^jM8`k4(kMBJDY.,Lig,&Y0650
	hJ-Ud-C@UG:TTBJCjBK6npU\BA"^Nj:9?2Ku.^\V7*JZf3!&+p"bHoB8P3[\)i5g3k$\?O`Qh\@YO^>k
	dLh[/CRGtp*rGc%M&]s!iGCLmJTJrQ[Xs,$7rbNmm^lHiu/5Aqs"1;A&s6t8@(-)Jtig5jXAGHtjYXH3
	1<GP;G-.+MD<M,*Trl%Ls+9]8L:XfE?5l.k/[Z*)lrpd+sE:qO!8pp5G>H5u^Nj''iOUp1(K#4,1N1AQ
	j)CcZj*tZh?@MBr''CoH6Q#""OAX&4;i):f4BI*igm9FJ+:8!6poF1H!^e;<Q.&7E:/90Obip3=-8nP@
	(Ornr7Os[nE)[^.K<P%2mr^2MXSU2:9:"r''B-&+:P#JZ_/hdXrI!eoD6GUAnUgj/*RI_J.$@^+mKUs/
	N.VfPHi]Q2$It.eQH-YoN;m]"K=HJ!J.7`BU=:oX@ocZI7d)CBIJNP#[P*ZLTFP0:asYtW/-]Qu,UO+/
	>2EQSaV55Ba1K&`&b"jF\PmhB;C4uI3[DZ23[dN+S$L(Z$4Ol:)K]$PZ(:dcPo%2W8&,n3YC8Y.K[rT\
	6`''0?/:HP&hCVG$DQh4O&pJlo<4+V0-%O0m%N/PrjgP-L`TR6(j2^@\s3@1]-IVe;@[9UbD!]AYMh&8
	J.3A)Qjn10ek/Pgah<d7:RMe7[nPau(O>1%4Er0],7*d%pn&Bdo)e5Grf`BD-rX>Dmc=`&k\"N9:3ZWI
	j>`YI)%#%F[f1.9VcbColnIGI1Mg)f2Ra2V_=i?2IY+u0<AL-HFZ)qqL.K\H.jEKG.Y:%o^0a#0HP*f?
	#+7iq;;duBtD0/-Lqu0/.mjpUMhr_8>EL4a]<mGFpgd/;G*<''admhVRfjaN/&.-r^Kb\''9P4=nVK8l
	tm@]''dT<D/qA[jUq=EpnVj,42E;N.7uS)KOu)G--Mo4inX[T46\u1@!?oeZ@FAlO"a[Xd/(cNDf;8Dq
	s<Qj]^mYg@tEFWm%!d2E&`"*fcn2#pLP`#BYg=kb)/b[ps*a7nS)X/iuM?Js7(]$GM`#]C;#5,UVuV#$
	DCm7r%^&,A>LL,/P*P?ZhUl3%0F!i\+t<t)\n]P]NOo)IPO*t^\oLOrO/t_m)u3M2O<EtO2P34lJm:+U
	C<@=+PogO]P<?pRX)j.B''JK/i)^JQ!JA_$BDG"c:Ld]-J3G?.U3_7Thf4''Sm9))N:SJr,/u]s/3%3@
	mS*jp1Z@gaM/5=LG[FT2(l*[@''Marfl&TqU(dYXe/Ii!/)/Dkd7kqSj)6j5VCG>=O_[`n2X&AEDCT<(
	0>hg?1ir1,!q.@R+>Ga:P9"J8c"MRK_`R(!UB4aU*!;%fB"8(o0q9?<$6,Z\+=Y"eZsq!-HUrF^u0()4
	PM$Q]t\Dp8!_I4XpPUY"R)H<Dl^+2sf2G\8IT@sR?eD+X81<QV9''''<r1b;^Y+Zp4>^.orO+$;WdMpj
	89BFa;4+m9%4S\hmbQi;EM,qaPoO70CBg845u%[&n_4S-p8t[H90fnZeBbKklWu^o=KDthOWI\mT9Fk`
	q8_QqG*cJ4B^$7JgjUT\2)"ZO4"0+-h_]3%ACCpF5i*>a6aAO:Z#Ku"<LBc:e&ek0rIX_5HTs6R!?)A`
	OcqgGir60N\u8@ab*DN2@+c*;aL74Gc$tuZ/#XXrMBL/%o-AVYb-CZGE1](G/(eABg4bJs6L(@B9a7@=
	ALA$7#OIR4.5!>ONm7,dTr)F-PkHG@]"[u$B*pG3&Y0Up1RfWWUKZIig<N-2_-Ah7qq%ZqeVPDB=/(=C
	nsE2X1X]L]HAou:=cc.k''&]>8''r3+C^rJ\-!m@%/AJ4oFdKQ0dsZD=C`ZBQ2KEg#\D<ismWAJBIs7k
	K"iaG`G!_\YBBBqh*NaQE''CQ]`gV4Eu`bIK,''iOC5.3Uu*k%)r6[rglukjtQ<!AM9b`TRGg1SXd5(g
	.Hf3jci[eS*MfBr-%jLSW.oB>*]iEP+"ae(@.?;B-&"`TbOOJ(X;Sldu/Fa8-aI^2a*!*l%70im)r,H$
	TA3O7-3NE[TMaf,V&HFr6BY1Je#j,V/&VB79+''.D2q.'':_tONd''i6<c]##)CUULOY/d*^fX\d)H[u
	#Z$hU!M(OW@WIFj6HFBF<HTmpioOY:5"GT3ul^RWrHI\_*_80-(`A-8j4$PgV2>GOE>_i!&`-*RKf^(i
	^7_N?o*;-CWrB&$RLAJJ9lm*$ZjZsH`SMhc*EC^SHiA`FEYQZ_/YJ7g!/`DYSYRZG$&:Na#$L,:''OPg
	WGc_TVf1:s.N[XY4NZa&I9\K:HrX2Nj"6;u[$<`aG>qs5nfGg<eMr\Ns=(g:kQ&PgZ!q=SjWOZL2"_r6
	]qY5=sJ?$0r<3U9gDq6N-3.8VNL^+fHc]''*hK$TF8XqTC#o#:@DE5=[EOhW80$*$b</f.jP''i3BQp2
	GFFa1Bi&mcNXs,J_ITSRGoECmLdOd3K@L-qp&!Vl]6#7@DMT(@CMbDE2H%`2.4"RF3S@@^<L:N^eW#k)
	k=1<3n4GUM7==HZqI4HY!=IHrC&\31U+UpZk29*TKTa%G6oMSiE![WhgM]C?9Q5g66R\Le3_"FcocAFS
	>U[nk-M^,-&kauNW>2\BOjKq"OjF$^<QIbeh:0MS=fW/iq;a*@1^k6@@0#;3r?!hcB%5Ibr)-dO$=S!X
	ITF`9-4Pp-PDMAG2QNX7,oqO]8m)_B**:Pf''e"VHLk@JQDI/q!-p3[UtqGcM(E?31nWn-=d%<tF9KPT
	/410@=HsUUaaW2/Y[#-+;,Mdu]`QC=/GB`_`F(^i;9or[Qe''1AfD+#igj,uB:NlKRgj%kc=hHi648u>
	;oeth;djL=96X@QDKDSeYo\6M*>?Pq0bf+ltVM'',$>eFCV*YZWh(o?Se@"UZuMHrjAR)M/3Y?9I+^ME
	EL#6ci5%40TlT"%$NL_?NB[P7T:hF@W[Q[uJdR:''''t=1k,SWRGE[VtWbp5Jih&L"(GlM;j^VR*V)sn
	kGhT/Gg7(g6p/M;f&ou@_Q;SN61e(*U3n8lfk$1bZ$q@\`kC51NJhe0)+Au`7dH>#*X_6&Ok)*Ej6p4X
	Ik%ZT<(?YcEk6iR+oghlXs1>)bb/*=4frBelub;TBdmCiClVQGl;Gs*Xpk%/8fEu&skg5lWG.q,lV\JB
	=IrX-^2-]ar8)%l0:;Zd26Bt?BI.E2P!eHIUXjYoUk.H;?a3G%M-9u4`LQBJ%b''qa`eckre$K$48GF)
	T7''AN]5d:e"GG''f''@>N++J.+H8hqR:Y-<_a8_G4;RL@WRNLo5i=M-gJfH/\bkXgC<8fsT=ml&73kF
	E*N^`+4"\ol8/B^&"XfNaUcEQThKH2Zrh#I`.AF^$OX`-9ZP/7.eZhO;)d]t"tk.,2I+KKj69qjbt#hX
	\+HF_@sN2T.JomI2Z_6o(grQd''>Oa+`1.86Lr]`TskVcbU]=i&)._%,P+IG@j&):V7?c@qglEq.+*1B
	[C$?iA6Er-(1YD60r,%qSFdiIk7eD[Lp(XDV3K\2\W((`Z%-D/l2%_pD-YQmbZJ/,i_L/Kt&BA$F:5M%
	5;0"FJ=$YEI3+$(lFl$1EXu5CNb/NPDXSBm'':h]TBopfY<E>-S7o_Q$VoY+HH#I`qRfN%*"NE72<"pQ
	@,V0)&GrQ-.o)r<XEXN\EF;RSk7eEHZP1%3q%N81h<&c/AWkR/Rq`%ITqqkLLAbt=]],6K(D4"2VX''9
	1\s$e%eMie?mY"5OkbLUl-!8R@W"BcjT:"dnf1^#5J'',2I87#S)j8%LLE''r@pP?7\,9.[hF*UctQDJ
	R"]br:(2"?V"-f]k"&5Pt7tQMr8kiN)X:1r#e9mE(1(]?)R03;7N@N]YTLkPgKRHYmD?eMkKQm4$U7@o
	Y%u)1G`YZU[6<g6a/J&XK3D''>!Tm3`pj?Q0*9lgdL1.PqKsQ.qe<$ZZrr@on_5[-47?R8rB84o!;$Y)
	"*UTZQ3^4\EC<Tg*ZiQXuj_eA/,dH;NuKjhZ_BlSR\R3?@;MI]=&Z0U`O=RVkB&b3YcL;f1tYbF''1Y1
	7INXZCFejh59nnLa/Y?rCdP4XGk%3.:T3c-%+)Ehd!`>)Y3jorp::-=CnG:tDm48*\\5+m7a7]Sp+`_=
	XncDE/Uo_P&L>RJWhYHu,N''NEI''N''[4MRZUhWH,pmTsi3<olVCQ<ep9[LpQ^l:hsO^MZ?IA,pukmT
	t7o#Jgj20>?iO56W"oIIh*,3d[VDloT.CX8`>mhAdJ:iQ"-o"ef?;(:X5!QonmAiMZOe!%MC:DsnStQ[
	]`UpfRU*!7:?''>KZ>7rYG,-:`G*pm''B&FFa+\TZaf,_-9k&R3V6kO_iNDM4oLkGI*9*l+1aB4%r;99
	^3BRW43r<9M=l2qkk$%R<JSBm@??5_E=s6bO*22(6?kZWrPOuAf9K\\s)i6:\Df.lp\eK4q>Bg&iJpO,
	PLd)5''>Enm9Pjmer*tXn>^Xf\f75U@_c''\;%dj>E-,7"Vat%sA!J;3m?/SXo?9/#0YPtI:CdNS-;@\
	4/BK3s>)GkhBqu[-Z+.;-cB_!t[^H:n@?ORBls";e%B-->`ETc?PO*\T,LgoF4I^co>qs\0(\a8;,Y''
	TKo&$`=W7ufgj\E^F/SmnTTrGi#nq9Z%6&(:=s60PKKUOQTlf`nYeU<S-D;Duu*kCQ#opgcDKWI.U6fM
	B4\fb/`-Rje]sc?e@1U]6?\H4Zo0ej0g)g0q@8m(TX=GCJ+CI%1<]K7er&:U''F/,IBSukopZjWMXLj_
	%B_iqD''@ls,7M`i])c)aQB`5r>e4kh\oR_d%@=\rofr?''BI?B''2(!](T6T[3%u!".MaNc9fl7[KV)
	b_^S8;0Mlo$7Y+mI58V;('',/:Rp@\Z*NNd><f/Q%RTEr8-o#LKg~>')
%
classmethod: R4PReport
imageStringexample1Gif
	^(ByteArray fromPackedString: 'Q4%FNC%!80A%@G\@@BG9A@@@@@@@K@@@@@C#@FT@!0@@@H@@@@B@@HB@@@@@ H@@ @B@ LC@0LC\0J[J<@@@@@@@FP@@H0@@L0@@N @@Q @@UP@@Y @YN @YT@@YW @Y^P@#"0@1^0@:$@A@] A@ @AF%0AF''PA^X A[(PA^+PA&Y A&-!$@@A$@H1$@L1$@R1$@UQ$YFQ$Y^Q%^"1%^%A%^(A%6,1%6/RL@PBX^EBLY^RL#H2NK3B$)JR8.K#U$,SZP4#(@Y#)@N#!%-C*P64H@@DX@FTH@H4X@L4XY^T@:H4A@L4A6+TBK2TZ]545MSUIRT%8@@E8@H5,@YE8YFU(5#E9ZKE)ZV%:-8&T@NVX@Y&X:$FU%YVR-:FN-;6Z6?6!(ZGXYFWXYPGM3\7ZK\''Z=:742@G,1LW44M7-;^8WC7(SF98,#@H,#H8-@FX-@H81VC85^KH="JH.-3H.:+X;L9(/L<H3N=9@:@I@:N)@:Y)EZD)I)L)SV;9C[-)C[?95FFY%#F95''HY&Y&Y7Y=:E0H*A6Q*U;NZQ:PZJR_Z^''):CL9*5^H:-=PZ6KW*7L9*7&;:7&>*;)7+Y&@KY&N+F)'';[?6;[??;56L;56N+5?M[2*Y;6-%;:0''+&4+K":!;/Z6+7&>[71:[71?LZDL\V]T<V'' \R3''\[F1,2KPL2KR<&NW,:\T,2 ],&;*\''C.<;N3,;N8,?&6\''&>\3;?=^]X-R=''=WO1=[N3-[V3-[V5-_3?].PN-&]U^B_V=6>_=/[''M7])-;^/-;^7-3\9];7?=/?6=+>?.^%V.V-Y>_F!N[I%N#]7^_''1._''9>[=?.#EYN''GZN?S^>;W"N7!6?C%8N;.;/C=?_F=^_GS\__V^?OY.O_''9?G1/_[6=/O=9/G;;?/?=?/??/>6Y//L"??[$O3Y''_?&+_7&1?7&3O;22_?5+??1/_?;3O??-/??3/?=5???7/;>:O;?<O;?>/;>? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@O;:<JB )G=?_?<@@@C?@O??@@@@??<@?0C??????0#>@I$IGD"0(LFCBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7L"184Q"HDNJGD%R9K*SJEN*WL&2)L.WLE62''D&3),6[NFO*7L&3Y41&T)#I-DJ4*MF#RH>.<=$S*MBR:9IJ''T*5*-V*R4,:Y\*5:<> P7GZ-IKU:<.-TL&JWR-VKU.TYE6"]Y&''+-6;^OOZMP-3;,"([5''F9R,W[M+@"@V/(<Z8,^OG# DW_$(7<\$<!B^G-W12\NZQ_$6:9X2XKNSS$BU+MU3YL.[O(E''?GY7XL62P(TLBI%5;7TSUIGNK3KLX]^L,2IM''([[.=V7\,$W3-''5[NKG]/@NS?T4=)GVP^X3>O5ZN''KE3''/!6V,].._/''=[R3-?T-DW!,2"WCM2R_!Q&5<3/Q(E94.,V''WU''O0R__ ]0!JMI71N"7TEK>@Z#SB[%$*NFFF#+E(X[,,YRM@7WTQMZGF-K"@X*4M@@F"!4FAVNHJX5X8#+''.@E@AH7 *BNOI<$B@@AU''IQH$SM-U=>IJG(HX28RI"QU!T?B"FFUFS+9HX4(9P#BD3\&.P:L.G"1A(^8\MGAD2<>*RVGWG(IY)@!+BMK''W_ZVR\9T_SBY2?[9F@"_QDAA.N[G8XG$1QRD_L_%!&.H<V$U6JIJH F)$SN''CP1>VD-NR3C(R;M!LM&%Y]F&.$:&9ZXX9G>6=3@"A.07-CH''732@$^O''QHJ$ZEM2/"$("=Q\1Q#U#"ZA:R9)EP)*,I..^)I+P8JH2QS0G#K*V9FB>^*5X[;AA53U",$$[H 6R-IQ+S++!D$@_-!*!(RV61Q5HBT;:OLZ/!,-<3LNB6+''H*I("5IHJH--8]:.2FW5IXKQX>[4#D10VF26.]XC!KC3K-F8@^R/A3R&6D^@U/*LCOKP+)NCCB?4@*$=DJLT+V=,&"CL@.76WCJ4-H$[($U%5/0N(+T\Z::J+''7\[,"#>1)2P93"OJGU,0;[<,.44@CF''#LCB7P7=*D,<E(&/''$-#8GR?[C8L993!,%1,(H7^/D2&/>7#'' ./EJ;(DD[5 ?U7''5!(1JPSOWW],3RB\*/86)6T\CO"ZG-]R 3M(L.2604I3&JS*^]Y943*9>=:IXU2Q/ZCJT3L@"AR22E3T;;[#''C /K?TZZSA2&:B:<T<KS[''NNP0J@ ^*JBX=-<[B$&_0E%$AO_OE1:##$<, /;:O23M.)P9ACL)5R8CDAY''5P4MNNL"1VSG+T;][''4S;42GC"A2''7W2><3V,!">9,,PQTPB<W3TBIOWJ1/-!![6@K,!3+AE <?1W/_YOJX@X]VK?;E^<Y8FBEG?+G/ ]NY16:48T5FN!AC5(0]0BL(@R9(+8J%!A:JCNEA#MHCEO8<H_>PCPE25*(N6M,P :SHBDG]Q]C''EBP"EB$7P-154PY0.U2/ULUB&68QNE!Q(\;;FDP11"!JMK.FFYXQOCZM4W#P_@&S3Q#B=,H"2)Z,WW=*.G0[''#AG.9PB&LLYA''M"@0?RLJE_HS!F3$&Q2#R4X9V''E+/=J ;N+*OFI;8(1D<0\%N^)JS 83"G.9PB$Q6$X*KGEP#"_#HUL)P$''&LX>8,BP/L^JH$''<3%K^5''1#=PX(5,SBP*S;#JNP+S#W]4H!Z3RL%YG!M7E/HIK:LX2D@:)Y*P;ET5-<%MHE83$M&L(B.[-(9*_+NZ^$&''N+/IS''LFAY-W.T(964''OG983"EG>"Z\>=<%O)["SF_TL*D@G2,4HD/R O42&P%VB4HX:=JDP#Z!DI4+Q"%+4(!#M*DX72-FN^/R#H@6)RLF94IJZ=JP(SR%K/L[R%++4)SBM*T1''R-NZ6/R&NL6)S''_J49;.=H<ZA@P@"$CT(!+5*D!M*%JW2-R&N/V)TH6*UJ]J5Z)Z=Z!@3ZAP+<+U+''+5*6@M*5"+&-UIBQU$ZD6+V-_J5+Z:=Z50#Z-\94+W.-(5+&VU0%''/2-^>>/V/ @6,XN&Z5;66R:VHSZ1"E2.),!+VBH2M+FP''&97B@.A]91BDCKBA$''FHX[L3T\\/H''BH["CABR\UKV%UX%+T].LH(JW>+F3/ZE''LZ)Z3I?E,[E^"6-J^M+V#OPQ+_?/Z7\;6.IU5;FW]%]'']:!Z7/@5.Z8F;6)QL-;#PQZ96KUM[9-:6,9?%;C\HHPH@YB@P:>#-]MD1"!4@ @I)JDYJ3FFGE"SPFR[P03+(Z5=/<FEHJR!D^''<1@RX@X@NQVF443 B@A6"!A::E[WZ7R6F6]K]]3XW.\<EA!!WDH!6+J@D[5G/Z]HBB@&-PQ3BTLHPD'' P]#.A@K6CL@MQ200][8K@JT(FNTH!X-@,XP#UNDU10%DDH,_CFFV*<C.1V>L$VU*9-$4_%3^IW/?.-[3N$^5)1WHDHI5GGKB*P"YQ\^Q1#BHK>_Y6A $1\>RS<7SHD=M/[M:?CFRNH,GF!3N^YWM HFPZ/CJ81B2(O:POE8KHS-NDBM*CD3"_1L!F8<XMJ0@@S#&"AMS9!@UV<NLZ=BN:@H6@HS''.:2\RU\I=W;V\)^=^9''2T4&UUB8$T7N,2?&GMJXL2BS[Q F%!P09_SXV+SBTKF(%Y-*S-=$..*&-WPQ,&_@97[<C*CAE. -ZH%G^X1%1$%X)X@DH"@#$_ HK=7Q''^V;Y/,8M*YF1A.<+N#C^5)_;_Z&>V0A":!X"TP(]X&Q''F?V:0R[.1 3*HE H3W(VL^>7#D;RX-!36,YB[S>>H'',S^,P^/_H\F7FKU^A7/>7_/1%]A78]3 @Y!O4''D@AG#T0.7-N#+.8G ;F^N+?#ON]<;31&[5,S4ON+55K/R"=9''(Q$<:!YF.=J[O%.%N#7)$(R;5*"NV:%[O.$&1+/V.4=[U!?V:6AWJ=[F[''SM%O;/Z79K6-[/=I&5?.=1YD/^9637#XH_,7_]N$;+3?^5>??/Z@2?8,1N><FL?ONJ=+/#EZ;71#+\:9BL/=\%S7.&V/;3RL:=9(7N><4K?ON!;K/+Q;;34),\8:%L?=K23W.6+_36+X2?;(;.>=&J''O^:_+O/]K?76/,=:;8NO7NDS?>''@O7;T#Z?<2SJ?>UMOO/RS?/3)J;[:5+>:=KLHG7S,\1>%@PD@N0@a')
%
classmethod: R4PReport
imageStringexample1GifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 227;
			add: #Height -> 101;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '!!!"L!!!"L!.]S"!!%P"!.Y''"JAAt9^tc`&b4#B2!!!!:!!"5D!&FTT3WK.L!!$"!!+l3K3WL$o!#]
	)C)0l3,MZ>JY!''62A5^!,mJ,i''*!(NLZ?=En=T`AE$!+ogCB!rQW!#Yb])#uH1!)"10<>cYl)&[];?
	Af\-PSmf#)0W>(G.f+#5UINd,8kbO,9nFkbUFti/hSb6A?e%SdQ.A?3^>_cA[FL[gHkl?7K=;m!$ar51
	I`7-5WU,T5VkZKX[%fd7\/.*9heM`;I0>''?2u3S!+]CR)-SnV?<Sm`>$>77iai*MAcQFT3fmoaARqq,
	A$,33[f:#SBQb72Fr<PdF)uM_Ed#.^I51eL0en)52g]muKu;c#`oi7)!/i''HMa//<5U24g%ujmHO(''
	MqXiu^7XcCWfMp1[LcM*D]!0B6&O?.!9=q[!416gAYOP@4*g]*g6)4=tRSS''+6R@0W!pRuOMTQWf$HT
	@mJ6)''NjVl-GcbiZ$6,HH5YXc$MfbiZ%in#baKY3FfOAcT8O3jGZV[f>*js8TcU1VIrm]mhoPWb9#IQ
	bK\q\[8HJ\qdoOg:cYE^%(@#nbgC[0u]\^`M\:*Z^o+U`ls?+b_o-9NaC#$;TteWah+Oo_n44>cHa_pc
	fWp&k5!6us4<fNe@hL)cc=V^cIURlf%/F(oDRF*3n^''(i1F60^44nlSDE"uhVQ!''hVR&Cjkfb3g]-$
	^rr2)W>32l!kJi2;acu#Fh<3t@kND$jrVl$"AEh-Jn''cXOf8/hQg[O=_mdBN4rVc9"H1B@''pXX]_fs
	pe>kOJ)Up@e4OrT<YIn,*.Zqu?Zq[V#E\MuVOnr7RY@k,nUEa8P/Erpn22ornDc^&RuRs8TP-s3CZCfD
	km-rr2-\s7$$fqYpKnzzzzzzz!!*#pn=ZGMIt)tJ!!!$!!<<''!!!*''!!<3''!s8W-!~>'));
			add: #Length -> 1177;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVQ;I;IDD*!c!4c\m<%L`iA]U,$0<''IVfX@jmd?8p1IkKE-T2).4<>1fG!N\!tHs8pF27''MqA]%0
	e+06rOZ(Rk^KgfRPE(]A<''CiD6&3p3OD9]sbBPf%]Y_d?a42hsPR#p3A8j<J^[MfEJ+.i]8J<;6h$=9
	UJ](2CL(Ug]qg+B8:0pR<g*(kuQ0in!StRfqbAV,M(K[q)*\Is,!6Pd@Qa''5tkpJj;-.uClK::&o[Z7
	BH"mH.Yek^&Yc_Jn;,!CjPUsQU$9g-l<]VN7M=aB5,KEDS*a1/eqfV"4[[p=eHeJj6L.5aM4DGgCa''&
	mU7<,\`+\t>&h`^mXG(It2PA!>W7@gaD#E(U;NH<r$W*]gUJ[a\<)SNC''Ld4O]YPs:/,5=r:H&,Pin-
	[t,?eE2R`B6?gCP[&=q\sZ0g^;NLm<eF0&ib-qU'';,7.tPeG!mt4RSqF:?Gr#:SK3R:M#SPo1;$HNLi
	mX;oh''Tn7#5qX/)j<ELaj.>%RcTVcIJ.3k3(og4IeG%kjuQsX,At''/(gAU10:N2d#f0pIOF`C1("0@
	>Br2,U-@7T+@[/KOF*+-aZ\9]^ucX&E`Ah_mYG=K1c6`T6:HH0?gPq"GjMt*]Fmpp.\:7=K^=MP$Y!3H
	LWNeYGDTFX1-30L8ff+TqOqWN&-S\F]g60R2GM`BSg_JYqL"d8MH0iJE$;[/i"u(*0g1Xt3#orVTZC.A
	0prkRc+0BFqj`J.miZ^;h4"ZBZ#Tq1I7s95`V&u&1dk3!R[3_MlRu8+&Pl_p_u/:?U8/)1aU!ZHG/oAB
	1*bMN*CSR!>G<<EKp#mkr]YgajE%''V5Wi,U3UBi,DFQ22SJ2S(it_Ii9@WB1#NDXc-/T?GYnBbIZ@m#
	WLZsUmqREQbjWPkHYU%mbAG#HTllY%qV2C$u`EZmLPO?oF:)3Mc+HY@hb5DH`5@Ph/#nqA9SW$[8S*>U
	E*1Tkf+YgQCF;$idI1G)\`)$.*lH8G3Y0)m3oROU4as<_f&:o)XLt>Zl/j/]MJf*6(<p9M])=jesbOZW
	Kj3UDEE:jl1G\R[/p>B/lUYfJ=h[>)BArQHdZ$"nZ8(rD5Z:H4KrRcI*r7B%(aCF12FoV;?Fk:*Po$Yb
	5Gu#N>m30BY9_e>^*o$PZCIbN[kh:TE!mBdSdO!To7f7a>+heM''+50IrL4=Dqm]1UBNPi4E_g5;1Nme
	Z$:\^T@(MfF]i#,"sr9PV>,;6skQC#(?qS##Z_@I1W`7dh$6VRE@T(''_4GYr7DIP4T.S@"/[W?ZTIrU
	\;>dgFj..([Q^`2:O%B-n9d(M^Ukf.2Q[Y@#(.9tm3Y$amr$dnRUGnGH2*R;O]eClca6Bq^#>18Z"M8>
	[gsh>#p_%9X/0.`#7E8DCAOAK5S=8h:l!OdtXVM["P<#9j_1#Xfd:&Vr[T+qT;26m)OCLd2(e&:k*T+9
	E.25Qi8BJI"Xd!YL5R"<nD.#Xfd:&Vr[T+qRJZ"7)S3O8~>')
%
classmethod: R4PReport
imageStringexample2Gif
	^(ByteArray fromPackedString: 'Q4%FNC%!;0A2@G\@@BG9A@@@@@@@K@@@@@C/@GH@!0@@@H@@@@B@@HB@@@@@ H@@ @B@ LC@0LK\/:[J<@@@@@@@C@@@FP@@H0@@L0@@N @@P@@@S@@@U0@@Y @LL0@LQP@LR0@LX @QV @Q[@@#P@@,! @1^0@9# A@%0A^X A[(PA^+PA&Y A&- 0@@@0@C@0@IP03R01 +QE(,15=0"L@@BL#DRD!FBL#H"M@T2M@W"MAYRMK\"U )BBD12NK3B @DR4-KR!=0SHF@CTQN"8.K#MKV3MK\3]%,3U()CFT5#,@@C(@Y#"P4C*P64X@@D@@DTDL@D@#CDAUWDA^V$E(#D,#CD,3FT5MST-\W%XQ@EU@IU)KL5IRT%I^T5U^V58@@E,@YE4N@E(5#E9UPE9^T55^WE1 #5:K0%:-8&T@NVX@Y&X:$FY^W&U%YVBK0&BQ0FJ-9&Z6?6-KL6!(ZF>]1&*&56.-7''X5DWY%PG^Q*WZ/6WB89742@G,1LW44M79AGW?A98P,CHUAGXQWMXQ/QXWC7(WC98,9DX- N(- V(2N.(3E:H/L<X;O9(3M<9D1CI@:@IP;DY@:N)@:Y)]6U9SN;9SV;9''V=YC[-)C[?95FFY>K&Y&Y&Z](OJQ;QJ^''):5^H:6KZZ>-,*+I):;T;Z7_2J3Y5:7^=:7&>:;)7+Y&@KY&N+O_1+W^=;[?6;[??;!/K;57N+5?M["QZ;&W\+:0''+/Z6+7^;<I=N\ZDL\FMY,[F1,[^/\K^=LK.?L_5?,2KP\&;*\;N3-WU5\;!<<76?-.PN-NUS-[5?]+>?=&]U^B_V=&(]-6>_=/[''M7])-;^/-7]7]3\9^^%V.V,Y.+A!^#H$.GG+N#]7^_''9>[>=N_=?.?F!N;E#N?N%N;#7^7*1>;.;.''=:O+L"?O[+?OY-OO].?_''9?[6=/_?;?_?=?#>??>6Y/3P$_;Y$/7Y''_3Z*O?''+_3''.?3,2_;/6/?5+??52O?:3??75???-/?<5O??7_?>:O?>>O;>?/?>;ZB )HB@ O<@@@C?@O??@@@@??<@?0C??????0#>@KDIGD"0(LFCBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7L"1(<^OHDNJGD&2)DM(JEN*WL$RV+6WLFOJ''D&3)\6[NF7R7L&3)<>_PGLJGT*4*MFV6L! 4:&&*]N''TJMB+W^TZMJ%K^-I7\*5*=^/W*$"UU*5+E&3U9$B=Z%F;E&[Z[N6WT-7;]2:LM/BIW-3$->? @LK?//6[E2VV/GR5E-8KE[D]1UK#-''6&>WK&CM#S+37\\-I$5=NZ%36<L+D(U<2I*7R-D+T*R]W5$0;L6_GNDFGG,7Z*N.T,DN/; 7-M<+ ,_F6''W"[)_FT(F-#%/*-G&?"P9<#%32<-?[H2^/>K)_X_NU3%IN$Y89*>S+6''M>S]6\]O;3"<QGK->Y+L77C*M!<8=9;M=VW6''2$FV _W_!A)E=J94G#74IRAS! [<!$*NFF2E3E(X[[2SSN@9C45MZGF (C@(+BNL@G"!):"FJHK85XX#72G@K@AJ[ *BNOK?TB@@A(/AQJ$S,5>EA"LL(H88PI[VT!#EQVZRVGS''IH(8<" GE#$/W@^D0]!GA83A0_ OD"%U%.FFJNW];X20#53E$''''W[BH0X0^ IS# <&5,O\"R"62VE:NIF1ESPBW(%LOVQDR(Z#U1*Z8YX/0^M%(C@R88L5GBX#C#-*U&'')(>CE)F&INQYY#!C>)Q3"*!B&=J%''M(+42INRC#EY*EIUH''+SM5AY) Z#$3 *D:UV''())OZ-2"&H*Y<A(SJ%,@#-#*#AE:24X#60Z+YAD=(K$+&:%MLV:;D8AVY"?X!O,M3$Q61R=#BKK;K:UZ//!,=F""6H1U8A"K[YM>*,%-9&JF4ZO&#;2LKR[]+,''V>&"!D6;T7!6GJD_'')+!IOH6*7BG2U9Y#0,,.<A/0"W?27B7EXOI8!COGK0&3CA.>V6IDX-[\36"PDK..SH!.OF:G''<L[< ''G1*3!&*DGGKJJ.> =\/1=#1305<.=+RFX9Z),:%QW3)3-OH$T.J+)[!]3:.:3.6C+Q_O!BC>R.:*1SNUIGN(:JSMX)75C%1CO[V[W5L\--9#Y> )*F]''.3"HZ5],9>Y84/&ROK''"CX3XY_&*^KC2D*9(T8R3Z[#JM2RNY]*(<)S#$@A4LK+XG5H+9!28\=CJ73KS]O.P.->..8>9;58''DTDN"SQ%FQM%>.2W[4"2K&)DB!TY.(P/?/#X/J82)[" ''W7-,XF\XSEY:DJ%MCCYX;''WB2)W?UCW[2 2L-.S%@BUL+8B:*I<,+MRIP1HO H2<E%@ZT,A$9DMYCC0 !^<2 T!&C?*%T:BCMP A$&V"0EFB!*9RJDJU9"K<&G0!S@LG2-^JDHC\!A#L\2!@V-X0A-6,G?>%D)LA!580]FT\H@(YJDRIZSCI((/E3P$X <[E<E:NKFIOA2_C3/(O&XILXQRMJ@QSZ#DL#K1"& DH3X6RLV_ CBMTU3#@=/8P2@*:84E3FHAQ2LKD49AE(@L)B@A^TX8F%JO8],"&@0YQ3[V,X)\>>HN0;!GZL""IXOL9BT''0\!CT#JQ]@1TI8\(Q1.F$(.QB2H^F5!JA%;HJI0\IPOK.LJ+4EJQX*N%K'']YR:W\<)S9@2[5ZF%KWP;&&L#$)SJWV\Y"J%D+XO%JOY!I3QP:$8WP#JX6-<''M)423&[:,)##GRT919*>\:L3EH=\)$7R:<970#J\<94''O^-+3''/#>3J\>=<''O_/+3''0@M*D@G*$=6F/R BD6(P"^#-XX:=JDP#Z!DI4+Q"%+4(!#M*DX72-FN^/R#D3T!FR0A@B^X=JP(SZ%JU<+R%++4)SBM*T1''R-NZ6/R&NLV)RD&Z49;:=J]@CZ)P!4+T%N8T@A1K*%JW2-R&N/V)TH6*UJ]J5Z)Z=Z)XY^)Q,<+U+''+5*6@M*5"?.%U61TX[J1!R@;8 D;R*%Z4K%X1[@[CV'']C#E&>M*59#T=Y58WUHD=BDEWAW@7G89A1#8DHT:!(S1B*V,W.%"6LWB=^Y4FLX#:4,#"8A ACPK;J )T%_)3@LJYC AE5@@BBZ0@@^,FD-=E#>A&Q#D-/Y!/X''-]V,YVV+6W&\@ @4LN1-!0,)D9J4W_U@Z6G+\U\HEHH..ZUI]H&KV=;6YK(/.R,@%$/]4H86.R-X[''N_>9I5+HHI]E7BI(R[W^/.5+[5:LX"\)C^S_1$+)9%;%>]F=?95/]3''M5QI5RAA@[<8A\00R9L-*DGD"1@A''.X+WZ96=7H_%^9!!7/R=RQA0A((@)Q@H@E;$A[=<)D0_V 1!D@\@H.MDGD_S!,F$;+!V-H0!WC"@HC^+@IE[OX1SB.131",PT$MH@EG)ZBB\:PXAN?QLTK^HE):Z+YB[N70''*=\E)1A0C>6.LVC&BC\K_!A ?0(,''0YZ;>$=/!A0/4H\OM DHG3-2S\6@AA]G01PI0$H5[RL@Q[GX3''NULY7%0- I+$LX:]%G"6[(CDQ P!FGSXP\I>2KMVD:(%%/LAQ_3516F8CK.I/@IMN.6/[L-Q1AT0E9;/BHB#/AIOBJQ UG8@P@Y8@P"ONB''U[_:5[G^;I?-:&QU,3*;0YB0$3N=TB6K=1[<#P\"JOBDS%.;B;T0-WR]SH8\GO,%+(8@^W%B#6QS0PX.XH@R#C@D\WS;6?THM7$MSV)"3?[]0$U1M3IAB6[/5]$Y!/Y3/02ANNS[FXG8+I+S+NA68BGR =:@KW9"#"H@8@JZ0DJWA5DO!4N\.WFVNHC>:;7-6[K989R^;YA5X@ZE>5/S1$T*.4);6-R.-+V/)[RG*3A%VJ\8,UFHL!_HHH ''@57(QG?2"#?<8!DCQ](@RDD6T@D@$Z]8:UU(N(&GWNPFIFG(QS^:X)E^]G(080!Q5$IZ(52F37X[@NI>^[M#7*:?;$"0!CV,N#BAA@@,@@Y0\HVZQT7%0X,Z,/J%[0OT^6U2><HA\$".+6FR>O(Z5-BG!6-,BP=Y[/3A0SE809C2V8=7TJHDL4B07FD>0NNZ]_V0#35(/2/;6-/>(KR?/^97[9?\<?;70J>K;8MO?NKOYO#FS3;1$Z?<9.>^>\:O/.2!K?7*/93:5,=>![G=+?7.79[;7 =?%.''>^/F[??/$W=_953?;=D>A?_BOJ?##S??03K?>>I?L?_OO_;+,/?<@6AO?E8@DJALCVH@H^H@HRH@J.H@@6H@N2G<PFHG8M8DTRG<V^HG0%8DZ.G8\6HG&=8D FG8"NHK]U8H&&G4(&HKU-8H,FG4.>HKME8L2&G04VHOE]8L8FG0:.HN?58L>>G3.E8PABHQDZG-F^HSSM8QJ&G=I6HR+=8QP^G5LNHW1I8UV&F%X&HW[U8U\^G9[>HWDEXY""G:-I7O*U8X!:HU*^HI,6HX*>HY06HI2NH\0VH]6NHM8&H\6.H]<&HM>>H\<FH"B>G,A@P@;')
%
classmethod: R4PReport
imageStringexample2GifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 239;
			add: #Height -> 114;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '!!!"L!!!"L!.]S"!!%P"!.Y''"JAAt9_VDo''b4#B2!!!!-!!!l:!$_ID1B7D9!!#7a!)*@m<r`54!"
	:X`$q1.)9)oB0!"iB7&SqFl5QE?i!&8g#3K=))QN1?$!*c,s?EF3>AcQGO$ig89!"989,n)S&$t-1LBX
	8_-_BT$\,9mdD+W)FK+sT?O,=#Q[6"M96E[>+h+G@c[Mp*RL&M5Rd."/oe"opLm3\E$;1J:q.963%''Z
	ScEl0pL6N!!#%[AiPn!3fr)c!!#7a&O?g*5U-\&<Eqdq>!Zh\9-Xj<1Drn]9hS_t<Y5=35UBT,1K%+K;
	I3:%?<^_9!*]G6>n-j<22kjA5[Ibq?!^c@@#nWP_HsA(AH4`&!+oh(OChg*AS#F\Mo&ka^gaG.B"%_T9
	/C6pBQ''H]C8TTuXkn4Y&U%_NGEJkJYM!m)kC#)cHSI7''1cKkL*IkogKIdOc5okV[22#m:Ku;c$_rlq
	<&W:$XMdT_INk11gk_m[TNmHjHc1@&P%$3LMPWKJ=3]`?`As[[BPg8`^f''f$XooRq:OPBf+7N%hVR@0
	J2VeA2gHUOd0Vla8MXc$nsXg4tKVlnP:Xl%4$g"3@_pT<TTY3FfOAcT8O3j[_`[GLYds4b!^s0uu#]lu
	NNIl4`uCUodP^9F3@g=j6sn%sK5`e6(_NFp2M`l?lK_VXq%mek,urlg9dah+OtcHatje^!.Fc1_.kO?1
	jU:=f+:gA_-*SQ4U?>M5ruh7eBngRR*UVV^1Dh;-oBgu[^q>32htlG82<aHGN&XQ\KrkND$jrq,CPrpR
	j[m_t.RcBRbWh<jLJmdBN-rTO+%Mt>],o@^MBh7NFEkP"WCp]''^Vs7c<fs8TN>r6UT*fol<eSc%3?s6
	$N^kI^ZYao:b_s7NSqouR10ci<aks8TP-r7(cHh>dK<s8Mfmrr2rsm@^,JJ:N1M!!!$!!<<''!!!*''!
	!<3''!s8W-!~>'));
			add: #Length -> 1362;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVQ;H#OEs*!c!-V_`%&TiqU''Y@poUl@k,G3f6NmWj_Rc%9bV]NJnYuSlA.`''k^.H?kmdr`d$OZW,
	X%6Q)]MRA''H^="e!S&WriVoRb,<&Hgf_%c0hH%dO)`tPg''tMIb@NRHbU"%":1eo_D?hNKQeF&8bdoP
	+fO@>a>2D3.aEYaKtZYQ^";\^DItV6o6M+[a!)1RS&gO7QX''^''j-RsQUt/f2S"a`j7.G<^7A+T5`QP
	*4a]tN-\08oJQl].$4f''e''cU5=((T!,KFKBddnW[;oWCEli<J?\_mR*o^j*03n\W"D1(rZsKgmI];0
	/V4!Hn1Dmq2sk%a$n%Ug2N;qI4+5CqsPLI5K!.R=0=_s6'']W_\2[%=P)JjZ2YXi[hk\:Eaa[N3B320s
	l=e)QQb4sl_\.0,H.W4:6KqF:>VTQ)5?a_OrSWVbs&l%cM2SpJNR8JEjCm<*Q7I7&mg7j''`GENlrhd0
	\Epj9l>j3Y:,W![&$BeGoi<lY:(JM.sPJe^6VE)DMBm*))m?[X7WTOZ#es77Qr[l.L`c,3hSMsf];7YV
	^?L/I%Y&kBPlu9YnY;1$9/NiK\)[oTGaNPC4hJh"7jap-JB%!iKLTS)!''5gXrL+$sE2hoY$I3V&a9cG
	d-ogZ?7&K,Q[BA%2k;@W1^$HYei^JqoBk/<76K(9p1(tFekNL;j!fClOdqWoFcWgfd;oA\i78)70br-9
	k%2lK-cau*aq@BfaTCg:\iQD%EF*Cj,bE#FA)d2mj;Zok6Vk_\$Se(hIa)$?3[)jXa#bLCRF/XMsb>g=
	?K3`3HRAT#\;6].;4m-YMF11S(C%=KfVW2J=Y>kU.WioZ/#7U_=`qVmc]*pQ(o^ZP6cMMn[gR9XZoD&0
	d`i]J1`!]arkCVlmM[%dK5i`1(p8Yf=r/L?NWk$#-um5#;''^YO:+A*=9aaB7c=$cOEqNN-%:8<1$>L,
	&_`''kgLJa`*\YkR-Q?os[mR3\I"2mU3B9B)Wt"1mVFOO<0\DWa2=W1rmrXeDE_Z5\DJ9b9\Zu9PZ[e\
	I@Yl9:#7P6?;gJE]#>]C/*->]CVlPiW/Si,YM%jOed^iJ1OZ!p#a:(AJ]3Z.dVDsmlU-_EP^K031>.\''
	<6N2MS!^XWX1MVl6XH0H*>!F[bkFp44mMUi>R-[aLYRpb"kB''Rnm3Q=7F`uqb\NI8.=I;7Of1:=5S#t
	oF5X)hY-4\jK87B\nN+BhK^LKg@%+A-o/>k)F3HZnGD3l^+i[@f6QH*,i5q2iV)H06^tVDJO#ljE;.%r
	jA`h3jpkaGW]1lkT24`-PWSC*PkWnZPW(Y$%o<@+]p$&^ZU4LmKtF`(>]hbCkWJ4cJ=-.>ZFPJ0<].n2
	7P=]:F(U_DH23sU2''CF"]kr]8>3VT]Io/2nBVb2BnR?rr2K#de*tstb=fY&@AZYY#Aa"s#o2+OioDfY
	NDnC!DO/tGCSc[bmG:b/DMDPsO''Xs3##/8eVE%.YZkE5k*8h\XpF!j"[kX-U.o`7sk\jC_/TgJTGd].
	2QJUFkLa2PSYPElsQ;q:<f)NaF[#M5-lOEWFI"iI<_,GNq,]"d65]NsC&g&UY4UcaR8nAj",h\cqqpFc
	C$aqN+TR-2FnoDpR`JI>`@!Z/A^"YObF$XDTk(V%6a/oZ@L>i5Z"\A/8#E.JC%huX_)^]c?0JI>`@!Z/
	A^"YObF$XDTk(V%6a/oZ@L>i5Z"\A0*iq&fNlKR!~>')
%
classmethod: R4PReport
imageStringexample3Gif
	^(ByteArray fromPackedString: 'Q4%FNC%!9PA+@G\@@BG9A@@@@@@@K@@@@@C%@F,@!0@@@H@@@@B@@HB@@@@@ H@@ @B@ LC@0LK\/:[J<@@@@@D@C@@@FP@@H0@BLP@@N @@P @@S@@@YP@LRP@DT0@SZP@UW@@Y^P@#P@@#"0@,! @1^0@:# @:$ A@\ A@ @AF''PA#Y@A[(PA^+P0@IP03R0I%-A$@H1,@S1%ZW!%^"A%^%A=_(!%6/Q5=0"L@@BL#DRD!G"M@T2M@XRMK\"NI2" @DR8.K#H@@CLL@CTQN#MK\3U$,SU()CFT5#(@Y#!%-C"P4DR[54@E@D@@ETL@KDH#CC5TV4E(#DA6)4A9.$BK2T,3FT5MSUXQ@EU@IU)KL5IRT%Q^VU8@@E8@H6H@YU4N@E8YGFP9#59UPE5^U59^ZE1 #5>M0U:]45:-8&T@NVU%YVJ-9FZ6?6-KL61^W&!(ZGJ^17F*57XY@GXYFWX5DWY@FWY^KGY%PGM3\7ZY+7R<:(@/A'',1LW44M79AGX%@F(QWMX]^KHQ/QXWC7(KB98,#@H,9DX-@H81VC8-^KH- N(- V(2N.(.-3H7F99C[?9D1CI@:@IP;DY@:N)@:Y)EZD)I)L)]6U87N<)C[-)5FFY5^KI-%G)>K&Y&Y&Y6=6Y7Y:I7Y=*](OJU3NZM8P:^''):[D)Z5^H:6KZZ>-,*7L*J7N9:;]3J3#>Z;)7+Y&@KF)'';OY<K[?6;[??;]*MK58L;58PK"T[+6-%;&4+K/Z6+7^;;71?LI=N\JKZLFQYLJ]T,V'' \R1''LWD1L[^/\K^=K?*>,2KPL2KR,&NW,2 ],&;*\;N3-W26,/%>L/7?-VWTMR=''=WI"=WO1=/[/M[V5-''<?-.PN-.]U=&(]-6>_=&=*M/[''M7])-;^7.V,Y.+A!^_H$>SJ*.#]7^W$0>_''9>[=?N;E#N;#7^;.;/F=^_+L#OOZ,_O\.//1/_W5=__<>/>6Y/3W&??[$O7!*/3''.?3(1??''1/;/6/?>-_?42O?74O?<5/?>8O;>?/;>;:B )GZK\/<@@@C?@O??@@@@??<@?0C??????0#>@N,IGD"0(LFCBAL*WL"0(\NGDBMJ''D!1H+"KFCM*7L"1(<^OHDNJGD&2)L&SJEN*WL&2)\.WLFOJ''D&3),6[NGN*9LZ3)<>_PG-VOA"4*MF#QX\*WU(O*]N''TJL>AQ\FWEH3VKM*7\)5Z5N)TJ%ZCU*/*=&3ZMN*S_,U*E"0\NMN+W)5*Y&6\(.>IW.W*\R>D>?*)V.4$.GC"AL+O)3W:U: Y^7"[_33<\?H_"O^M\^9,>_O''"LOG%.44-AJ%H=Z=(%9*NCT[ %CA)3Y8V[P.C>KC++:)>&J*FG3%''6YM,WW0''/69-&:],N;JG_GI!7T=K''+6KM__<N=>9-3=XK>I?^9''E/389NE%3?/WBG4$=H+D0]Z:U1.3>^<\3]7S/18''.,YE5!:,@WX''&75QH^\_MSQ]89H>+4AS''??$S^_T@K>QVA*A!;H4G,&1V]! ;8=BEIWD?(''UST,-.!""6J=6BI;AZG3 BG.5RM#"<>H,NL3C."1H81UCT&#PC["V@<="@@  R)KM/&$PL8@@L@X@)&BITH %!SY$CDNVT5=I:KX''9!()$$$NDY&JABSHXB!II\:;+ MGX><.H4\\P()Y) 24 "''''ERZTH<3!"I:*JG0XMEL(<6(@4RNB-X)H:@;U&KNTVFX1X49%Z!YSS5!%A*F*B1":.JQ@<ECZDK>]04YCQC_2G"N*7:BV^RN+K*JH9MX*/MCJ( D>8L*$CYJ3BIP0)( _KG.*N*K&!9%3%Z\&_E)*F(V!F(54;KH*$B>9# $J6HLRP4X.T*;Z:A.5%O./FA@P&"9UU;)3I[N@!WE/0AGLY.%K8[[X+WVY+W))=->:7B*;;88++2/4+''#MEVT(":;ZA+\:;5UPN$*IBESOJ^<#":4HH@AQ4G"%>:2JVX%L''\\L[#\)%%OCC_43JH2Z''(\K;$UG1S-"<<D(\7F;U9:<8314(/#2O\V/X(!>_I[4L(,?4."^T\W?KRKML-(1*VW9*1333^0N@''PM-\,<]@&&?/"L''PTH.[>.$6KK_^*TQMJ#2H8B),J8_TH663"PBR[,$E\=2Q0W[+>S^7_''Y:ZI#!*+=56MY/$,/''X(=I]++L2Q,MC+T3G7VYB)2,*^:JF/,&,8<4X/RERLC--N]'' LALFL<I''MS312B_OCN\N5?L9H<L(K;5X4!OOJ)MV@,AA;+)K#6;53B0#Q?X[1@H>=]TK6*RU66N?_YSZ\7>(DEQZ*SUA$S,U6_%U T<<3\00P:&6\+33U\I?8J,G<WB1BVD DG7R&=!3EI"<ZVBAFNB+Q#\FX(=*<B=8:ZN[!4PD%_5UC8KU@:B)U- ??7DN P!,A3H6<\@VQ%BDG:H <*)!C ?BDH[>JD2^AC6TO=;]9XLH)M$0VE *[ 3#"UBL8#A^>LO*7ZHQ.Z !BF<(&R):LW%AQM8PC1R6[9%0^#YL(QNY:DP).)D[A?1"</90"."9LH5B1JGJ]B#GG8[Q^''),S1%Q]T[%?QE9*E$"B=7HRC#6DW&[^@TP<R#FPC++$UT<9A ERS@3G/FD%D1^H)$XATYJ4YFX''HP.I+%E9V72$)!$YPH-VY-A"**PX@0%H+&!":C(8)_@CNX/T_%HVM#1#*7LX1]#*\UY#''BOC1OWI=FXSNV)BB11_JP))R VT;:RS-,L)3"''VAU/4#H3913HWT3Y3V4.9)70GJ\<6U%NQ)Y%KV+>*\\<=?''D]++1''/ L*D@G*!U=L-J_?D2(P!\*1](0=JGC^JYDCP+Q"%+4(!#M*DX72-FN^/R#H@6)RD]J4)JZ=JP(SV%HI<+R%++4)R9%&41''R-NZ6/R&NL6)S''_J49;:=J]@CZ)P!7(CI)*JD0A( %JW2-R&N/V)TH6*UJ]J5Z)Z=Z)X3Z)V-<)U*A*5UD#-*%#GR-Z2&/V,ZAW+U<N@5IZ9=Z50#Z-\94+W.-+5+''#M*5;72%^=++V-_P6,X@]K6LHZ=+A=?R,@@*XT\[3@R 78P$D\B5''I0%P"%@U@YAE"C5=T=+J SX!"@>YYJ4$ EEOHW 4X,(82!NDI&2W>RF-_F=/PN&R6,KV,P\8AC]+*]$&^@L@HM&![%88VXMB@@ AJ0@TDBHHICC BF!9"#](V)K*?KV9C,K,P;''I0EPAX+WY[^-1?5\N18.4,AA8AD^<Z1K7#YP!<C0K_3(X7/"0-[1SN>8K4>&J=@7&GJ82 VRRH8+7VI\!<:4DNQ>R 0J! RFZGV0?;@ C@CW90@9@PX^BZ]!PCY$@O9E^OAX?#C AX@@72XE7;"!^?3=P/_?4K8G*4008@0H@T'' B@B^3!. $^2G2=LXS%Z(DIO_:1P-YA!.U6XQ.T$@T4'',B@GYBB2DYF,(<+W@,-FJDALL 1ED  A T''&L K$HG>\#W;V1_CVJH23&36@F2OW3 @C^B)13#V4@E#&C&;P+Y.OO)@@RWW@1-N8HB_%60ED7S#F@.(0S%<PPELCK+P@4F4(-<T7@&\(Q//J@ZP_2.OQE" D@I!Q15Z_H0 /;$6\W9ABT)5Y@CK81CY2;TDROE''!L@7GTM0PY8+SH,H,E\!<8!DAT[QA0@,N1D Z@Z0!\7AX!>[G):P@BXR<.- 9?$\34#0 %>]&U#S&K73RHPC#,ABK/R"5?R5+#!0PF6A6LOZB0E7@:Q  1!D%0!ANB>=!75/X7M:5=2V=<@C''U52 HHW9OZP.^7=W?[VFPI8-''\6A$E\ X2;1HJV0:''>L>4DCR1ZH\A6]"^,\FG61$O$*AXH($5>\E;;V-A=FG$=UF5]^-!BA57(^LSK?UW@?".9267.\:L;WUW''V@)+M+ 7R/VDMH]!B3F_>&.-#''VAXE''GV#X4,!LA@D^7@ @4+<_W)QA6K''.9@^3..-^)3/VXW6LHZ\;BX=L\]HGLF0@FG3*,";9X IWVRZ!U+TCZ<P$BK6@F\IBE17O=6QIS'',4C2WBAC=2PN#, C J/M8L]/G%NT;:65+!<[\\Q"ASO0@5V(''@=9)FIA[B@1HI_"(133?/^S7S7/ >><O4B?ND[?? PJS;2%<=< 2"?>]A_?/N#S77!S;?:6L?==[O>3?5W;98^.VBC%R@@A5D+YQ5T>@J6Y]?==#-D1/JP!@\,@Q92@NHC;18J>-W/B_Z;??>"QW A@68UXGLB,P9WPC?:%7;+I7P@>H@D(U?I5 KCY!C-,@%V( J\Q0?ATFPVP@#DYW<W! =E0G<^4@XW=&''%(@YK$F_! @K[% 6@@@@L$@S&A8GH)5?)T@R !1C1X@\+T@34@@0(<@!5]&+'' @5W4HK18@]I4@3-@@"Q!V4N8@UI&HC60@(WL@,U1 +QI 9EXHW1-6$8^G3:IP9EJAC20@_YL50/.F63E0$-,HHJBF9[>HXBDP8''0G?QU ? % GF,HN Q8]Y6H_5HH#>YV"F@  0"D PC^ K F!/W) LZY"GJG@I$K!(>8]-K6XM.9Y,4_ZF2WY: IZH/J]_]E P#1"I0@TB%G!,ITXA&L"J&> I''X!0K7 IW- LD5"A)&!<>$V@A!"G[ "C@#FA*^"G37BG1''"HIX!-_U!''$U NZY@BB]"EV?"K4+^H9$TO+SA?8BFCFG\NO1"DP5"DQ5 HU<"D]''A;TS"E''#BNR%!&E_XK@EA)_$\DR-@M90"G6-!;.6\O4#BCE5Y>@7FAF["AGP @G4!\B@$AIL!?J(@GE9Y1@ E,!% O6RA>@JBA?1!<6>]R2_"Q3A^RKEU''6T"RI\&M>6U[:3@EN*^]$--(UD[W$#I9$1U!$#"9$0"!$339$0O!$4C9$4H9%C-Y%DY9$4"Y%B*9%D39$T;9%M(X%UI)"%QY%VU8%U I U*9%PCX%U;)_&@Y%-47%&RY_VY9%-VW%&(Y_V3Y%,77%''@9$41T$7N)%B19%3H)%7(9_G3Y%2BY%8C9#7<9&J\(&HY)%X"Y&E&9&H3I%X;9&E<Y&YH)%)QY&]4WD@@;')
%
classmethod: R4PReport
imageStringexample3GifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 229;
			add: #Height -> 107;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '!!!"L!!!"L!.]S"!!%P"!.Y''"JAAt9_VDo''b4#B2!!*''.!!!l:!$_IF0`V27!!#=c!)*@mAH3-1!
	!H4#''57F]>Q>Xe!$a`/,DuOQL&`rT!''6,?3g''J.EWA@T!(NLZ@q5RDT`AE$$ihR^1J7d\Zl&ig)ZW
	YO>$`(QL`''N`+((`[G.eoE_BT$\,9mdD+W_jn;^4P9,>3&%M974H&M>[g1&q;1$ii-s3\sEM2.WsLBV
	[AIeiEeC3+]HVOO-Cbf0BK>5QDNd!%^G]$p@Qo6"l3BG,ITN\j21q9/@G[9heY#!*)91>"^YL;GpJ#=^
	D(.?2u3Z!+fFG!+$N_A3#]W<Bshr=''f3H>[WV"NPSqldpB09AH4`%AS#>QjCYk#CJ-I"?=%;YBQB]bE
	N7W6)$"gt)0RkLG!Hpg?7^825]_!fG+#OU]]t###(&''ZI5Em35okeI)MD0LLL)NbDafAthL`W-M^&4[
	3>%07,E,Y6Md@rm?oj3R>)\KQMlk3_`oiH<s,f#EO?*B+3t[Q83flC7O]iB\Bel#u=,kMrOP@477N%b''
	/=U2lT8T,*R@0VZfq$g[S_<_7BKW,43MUgdVl-Gi`20SB,HZl<YH>+4bc%YhkH<XbXQ0,Zl/&j\!3rMq
	Ze=iVs4b!^s0l`"]m)?HGX/d''DS<0=\[8HMg=j6sn%J8h_L4G]MeKqhAAXm8`M\:*Z(/eO`5^ZI_VXq
	"lMA__5g5-Yac9]OTQ\GhWQrf7eahG;jnR$nrmd?3e@hL)ac"]qa4o@]f%/F*r;PH43np3,fr0)M^44h
	LW8$''sh;+sbhVR@qB''R3dkK&kJb,X[]h<!_8kND$jrVZ*&N:,`^mdBN5]m9IuN:Yc.oA$qSn\+Vgp%
	SC\s0cP;f:2fpOT".<r9(]ikf<_d`r>G\s8KG+oZ7(,d/X$ps8LsUrr2orn"?>LG)LD@!!!$!!<<''!!
	!*''!!<3''!s8W-!~>'));
			add: #Length -> 1730;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVPQHW1\Z*67N?S[1S;i1nN#_TXjc1@93^&BM3Ybt$o3T[:nT4_dcN*h$b2BFd.gQ-p&f8pfeoW(+.
	9=#:kZgsm-Z-WH,Am`aqAIWljF1V7l)`O`@urLRTom`]U!HU%9(mdE=NKH;6t''ijVAC*8!LD@Kd3),f
	%Ab0IMjKjmCU&b-LH"G5Z)5oR.HBTSq[=E/lKdm:]`T^E?Tn<G.Bfa!5t*fu''!BFN!uoTUmjhB7)\Ik
	-`n52IE)Ahp/FXs''(``W21N$G<<+?1n31I#YQaDYo0(e3_T)J\4N;>\d2/[;lul+]K]mT,.Za,WOpG*
	FuD[Op`OUoiu8;bpPh!?X>(o0,l`4!2SW7csY2#IAR0SB?(<e6974l`Mel!LE9?$5X@[C69?(UaLIFSJ
	T%/&YU?[df@hG-loe]If4kV4n+u;T5WU/WLVjo@X)!eSKkfkUi#q-r\!7R9f0_u[d9[s=4fe`P8sJhui
	=-ZP\/IXSSb?3bOOL3aUi_ka6>mJZm1Q0W!2Q/bF1YLLNZ9ie%*''@o+0o>Q5@*PGd+1''pp@6;[Fq8(
	JaC=6K7tTe)$$,''WE$5`E?d491kutP#O%JSna5YclEq]$oS-l@Ki#nFs?"3>Y`0;6C\LUq+,Bo[e''i
	LB(+m#l12M/:?!GQ/iAt2r]bq1#.T]EUA=KMm>=,XX="<fV;idm=GC''mtqK?M''gb&@6(;1-.u30LIq
	S>f8LrZMT.c";JWB&pLTd6a;8"^h;/o!^lG53ij''i98CuW\%k9gEIGsQ_;r.INTLsG/''-3[biUQKB+
	#BRB1''fZ3m@lonX%Pj.!ZtK/?UF+:6Mc+f(CrARqo2aHsVK@iUX@$sED63Q<3M!^gk\*OCj>Pu5.t,;
	S==Cs53Y7>0/Z1jDlUrZGAD$Hc!i8!4"q/8K>Qag.U>chO_'',$mSq>UDd,6N1.N11#PK91@B`e._hNN
	d&e=06OnDI#.3O>P0hoK"+fVKJZ4qbp*clSEF/d;gJs`D--Z>Q5:hE,"hZQ.U5JZXj0)Pj=)t6''Z60l
	"+OGOjP#9i[7(2gE''DF3IG$-7''64n8))cg+e_&uLf6^=XM-KX)HdNrZQ:BmG8hLdO%p=6.+>7QW6&G
	>R2MQ$nlZOuWdX/bqd,C)k526>RMC<&Zp7-fJ/NOE?T15\^T=oo>D(AJu.=%ldo1"fhZ;AQBaf9nIi#Y
	0%F,n#Aj)A>ASQ-^5nbZHLM)V2Yq:?AH4d\_#X,/3mO9c4:`O_k!4LFeA=7%5"G-"<TGfLjBYYRnPIB[
	S''Bf''1d>+ma&^?iPKG+i7.TsM,PC-aho6@mpAZ*LGaA-TrqRLG:KY9R1!<Fj2%XIFH>\*nWW#I(NsD
	9FDn[\Yh:U,-b*8uWAX*r#rYF^t%deSoYs-KRiqB($m12NK.M6C"J(2DQG"_K``p5ZS2%&$"r]:O\<^U
	f`tB_c:N!eRfAqbAa[O1gZjb=6L]ip<dt%n2^XaAB-0:\@R.!@ntP:an%#*nN^9.IQ5.tH)=H__b_Xr0
	aeE&&eDor.2m$WL+o"85Y=/Ld^QS)reAA_cj4/#L$>&HU0_D.?h(LOS:aME/D1%U098l`igQSs6Yt>#K
	S6a4%7IZ+SjLqFBg%u8)1LO9C"<ld+..dcU(XVK.1/Wi.3sVCO;''HJOKMlI@2#rBhXL$o_0N1X([7;2
	>*,9o#/8+c/L=KeM-iff+\c-0T::r\^Q?b3iOo%j>9uU''n\XnL/C`i?i+a],ao>oSjM3->ncIR67?bO
	^e;t$2W<=M@XH++;f''K7$7ia*uXEXUuUeIlUZ^O*:@hR\hi^?FD8KpDHc%R+)@SPaNJln4SC!N<nGAb
	bJ&9MR+jrpt^l$NNjI0&a[f]Wi1ZR?LUL&:;&DOe9Z+O_o]U]3O5,k)Lh_ELlH5-`M!GhCR?LTUZ.5;5
	&bjnmI8Bh[LgVoFrNB6h&laIVFa]i-II9"h*UKO0ic)cM/.0fem#DBQtrG''0)iKkkJ4*3N`k+J3rG#"
	ZY[fSlYPd^9P1P>*@F$[LXJZZ;FrGi7hk)27i"!2TaAo7[!W?s+''t.j"6,&p;1R1PNURGmYGc%$FArK
	[q]EF+gF0Z=SEd?.CYLcO4j"Y+(Kl#XFuN.NPV=XVQVdO;*jE;Fh-d+Xn[jUlZ:R6;g>]8KfN.KqseEP
	!W&;$:uBi,_`%U''Ttd4qB%?kWIt~>')
%
classmethod: R4PReport
imageStringexample4Gif
	^(ByteArray fromPackedString: 'Q4%FNC%!6PAY@G\@@BG9A@@@@@@@K@@@@@CY@E$@!0@@@H@@@@B@@HB@@@@@ H@@ @B@ LC@0LK\/:[J<@@@@@@@FP@@H0@@J@@DL0@@N @@P@@@S@@@U0@@Y @LR@@LSP@QVP@PW @Q[@@#P@@#"0@,^P@,! D:" @:$@]K&PAUW A#Y@A[(PA^+P0@@@0@H 03R003Z@I$-@8@C!E$,A5=0"L@@BL#DRD!FBL#HRM@T2M@W"MAYRMK\"U ''2JI23@@@R8.K"!=0STQN#MKV3MK\3MU_#U "3]%,3U()C*P63\F@C(@Y#"N3$HK@D@@DT@#CDAUV4E(#ED#BD,3FT5MST]\WD-^#4%6 $:J0$>]5UTP@ET3FUU@IU)KL5MUO%IRT%Q^V%^(758@@FH@YU,3FVP9#59VP%5^WE=_W55 #59,*FBX2FT@NVPN@FBF$E>M0U:-8&Z6?7D4DV=@FWMKJ61^W&!(ZF6D''V>]1&*&5''Z/6WXY@G\(CGY%PGM3\7^Q*WR;:GN=;742@G,1LW44M8AAGXP,CHQWMXQ/QXWC7(KB98SF;8,#@H09DX- N(1$V9]6U82N.(7F987M<9D1CI@:@I@:N)@:Y)!AE)CR:9GY?IC[-)5 LY>K&Y&Y&Z](OJ^''):+I):SL:JOZ=Z5^IJ!,W*6KZZ>-,*?T;J7_2J3Y5:7&>:7^=:;)7+Y&@KY&N+M(KKOH8KO_1+[?6;[??;!/K;59M[1>R["U[;/Z6+;^;;+''>\I=N\ZDL\^EQ,JKZLFQYL& _,[F1,[^/\#_<<S->,+6?,2KPL2LRL;N3,?&6\;7==VWTM[V5-W8?].PN-2^VM.)]]3\(=;^/-;^7-/?6=+>?>V,Y.N? .#H$>GG+NOU%>#]7^_''1._''9>S,/N#<?^;E#N7N%.?V%N;#7^7*1>;.;/''L"?#Z*/O\.O_^0?_''9?W6=/S>:O>6Y/3P$_7X%O/&.O;,//701??/1/?5+??7/_;63O;:5/;78O??-/?<4O?>7/;>>O;>?/?>;JB )HB@ O<@@@C?@O??@@@@??<@?0C??????0#>@K,IGD"0(LFCBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7L"QX;VOHDNJ+D^2)L&SJENJWL&2)T.VJVOJ''D&3),6WNGO*7K&269].K^.5FT*4*MF#Q./1WN(SJD2$TJMJ''T)5*%JVSY]*7Q(2:5N[MM-\9_+R:4*!XMN&EZ/V)M"VY%%V&$.7+-6;]L%V"1,R[U.T[?V:9@/R;=?CI<VNV<28,^OFZNG>_E$IL\%J^ %?MF09,FB,$;=ZG*78,V''G$TD;[U''YL&Z2&*-1Q.39,<#X,4^7ES,1]\?P+N.][%2''.OD:8>*=9(*[K^&1-+,BG>%\=5?^D''7_''+:24''CG18/>K5:>-[''56-D?&+]>FG-D;]IW217WLG2][.OHZ57?OO56>W5U1=9Z=_RFG$"1_^P]P4#!)5=N24R8SELRU+!L["^U<4@$L8%%XXWG"OC!L,\8@L"HD%H8H(X$Z\!!O_L0@,@D*<@(H84$FPL@@F"PUD*OL[$GDU((*(#"  )A9RBJSEY()HT,6!#BFB<FVP>J2.0A"XWJ9CG%"T0>VRFFLT;9(#D(5HN&&&&.FP<W3[3Y3C$>]E! ]!:NJJZE7+4TUSW9MR%!OU<T:.QOJDYIT#1T6(''"L39,<>D5;H0AY)FH+"" RX12FFNO90"!B"N B+FJ''F<284"ML ''9DIG>^&Y:9C /#VOTX&4@V(& DY)4ZC^I['')R)8:N6L(YJBY#ZYM;="+,((4R2: $4SZ*H8?F@M$*]A=Y8^67U)2U98_MR-!''+TSQB*"./HX)J9SO%$S,-"L&(0T)2R;+K+BZ2"R-E#UNB7@=<28JY5#\; V.EPCJM*:E9TYXB[?L-$E1!M7,2&,=K7S<Z;@4EY3R01H^L<P4>U8Z:<U#1"---]Q2JKH)$U2+[VHIJ>1-009_.WJSD7?XA+''$Z+31CS]<7N=LH O&\8UYI,J$,"*S>6;K?#X:S2L\!*(J5?VD2&+XO*A:,ML.!Q,T2R%^3R_KQOFZ\[.=I-4628OFB67>%RL?KRF$$*[L+M/NY''5&&8"3V]H<*9+]SM=\0V(57!UNSL0W1E1NENZY]>89LWOS_ZG''SW7^^YP1; "@A8?7[_*1)&_^)^([0AH;:C?]S*ZLN;J^N.,6+-::&$C$.NOM[.V\D5*7%78;LYZ7TZ!Q''C]_2_OXY>:<:X*BI][''2W@Q3N7KWENRO\,4''7/,7P<H>U[L1;9=;MD7"G7F6]>?O/]:^5?O91MJW?;4507]=\==ZHO_=>R7O?)5@1!H*PX0I$#AB HC_0M$X@GY]<BZKCBCHI2_9=*G0NRIC&/*6>C-L@MA(4#P #B,1/U@>C$Q''*:CBJM!BA,8P!2VTGK>I82_:V3(NQXV:(!_ JDRYZ!C4/G0!._9W1O3Q4Q"$OBGPAR]DF/81BIVH1]HKIPU\$GFL)*Q#D2\H.9T>K$+6$&M6Z."F4/H-''Y-4X%,MA5&\,FRL?*Q#3N\(!1=.B48D-B@VKQIGS_6PR;&<WLOV$( L5#A)" QFGN44"T72\%J?.RR&WP_HWE6RT-.D">(SFT''NV%JF@*%J%R)12)''B\MVV/BU,L2%K'']YEE$*4YZ4CJX0!8''@XQ)S"X%L9$&N2\1&N/NY4H2&MJ]I3V)Z<9+X3JX6-<''M[''+3&>@L93NUR\92&/N\9$RZN-_I3''Z:<970#J\<94''O^-+3''/#>3J\>=?''NR0A BP@M*D@GR-BBF/R BD6(P!_J4HX:=JDP#Z!DA>+O"U+4(!#M*DX72-FH>''M!H@6)RD]J4)JZ=JP(SZ%JU<+R%+(4)Q==*T1''R-NZ6/R&NFU)SK6%F6>0XD\LNLMI_@)T(ZHSLTPEPEA#P =_EOV(TI7IS*7 5A5M@AMYTE4MZ/HMM''!!B$,5RU^?F-Z(*&V,XCT*R."ACKJ*ETZ[@L@H3F_V\48UFUP  P''@ @ACJFDAN(@CVN#!#KJZ!KBF+^- B?/V-SKVINQ APB6*%!3S+T^O*U,T2F "KP -+D$>V1%5RIZ&YP6-D:%;F"U^]''L()Z3IXG>12.R(EP$_NH$)37,X4<B#$LD([Z7)T%R95*O).8H-+7=KPM,R9I91GUF(W"EDQ[0 6JTIK\$>XX_R@B@F_3A,LYU;V(S6U(VZMXW,J6GN/ @ @1\XP(@(D@ ]I/X4N:6IM  @ AL8@T%@J@B<97IM=Z U3M0 1J1PLXTE+@CS^Q7/?7=;73-XP,/FHDAKV /ET* U.0>F@Y9U^)[03-^5 H@WI#=*^(@@E-;?LHA\D!NO[:QA =X5;;5K^9=:>DNP @8-MBHP ]:PYM/Q@DE51 F@G@0C%=HPAL=?''E1 33$9,ZU@&>81#-6P]>7=/ B"R@IN/ @7&G$.LPCJ"?>_;7PW=#JX1D+U-4DQGG]G]_Y,N\H0 )LX(=YQHA/JW''GH3BPB$H@@@N HLPG9*Q''O//9Q\9=L%M7''N\=!?XX8KT3&-5W7/OBU-@NX@J[Q04FW-0Y-C(6+C%,XF&R=C$B''Y4IOSA=AQ.<X@ELJLHP:+G*U-_#5[E6;)0''+V)V''?($8_@D%3_=04:?-+L.!$@\YD2OZBBB+*%F]V;U4P\K!G''JT^@@$V%2C/5^@AM$XGE''.^5-HH];7LJ&\4)2>>U/#=&0<K!EC,2@[VZ''>\S_0*-^>^)W0@(6G^15[8#?W@=,,A&>LOCBE;;-<J<BHNHSI<&C7^-_>])D4@AP@S''>V D@\V-\/12O;8P+_ND#RO3[CW?81U=^7F_(E0Y]TCDL>D6RUPL@5/9F8ER+N"N,Z)T$:."DDWXT@3/DP,]1E#GT853V9@I7,B>>@6X[WQJ+K3^83*V:TP$[=Z(W +,2(LNN"E.O]5 "ASR8\]CY\=&96?7.:J0;7/_N=1K*/^>@C?1!?";80!-^I(P?/NHU''?#EN3;0#_>!\TW,#$EH?_FX[4/];XDKJPB @T9P0;CY05Z7/(LSZ\6<:,E2VW"8  PLN@I<Y2S/@X$V.:/O?T&&2%XV4LCT=L!FF$Y/><_"W/_H'':* K:BI1V5"=E9''[-]=^?VRZI^;7%4*X)7>,@X@KJ@G-:7<\S.[U@4PV[Z4?S+2;S9U]I@A2^^3QZ$[+%?>>%_JC;Z?!D=><QA+71%@IW,R]''+0]PKM=05[\GE!T@3+5U;/)WK+E7QS9U()DVTAQ =T5 ,V:F9CU&="Q&YBQWX_@G0YFCYQ<@GV9P:O0G2?=&H1%%45I''\QVFIS-V)[QWZ*L0KR0GV.=&"U9&!?=(NW=''<L @W''40(LM0>''<@A7PE ),F/5<FYQQ7,3.F%S%P92 HH150UA8@AO$@7F]#9>) "=A(Q$FH[Y%&)F:F)H>BK^D@P*X@53,@F2PAJ I&*#9 W3U8T4BGC^X >58@A"PE^UY7;,=&7>FO!.!>!.8.ZA=WA/HP"@GUBG"U!%\NT@UU@BTG@>/2A-5FY-?\ZG%WUY;#@IR.U2R6]>=N^@GQ] F7\E+U!3M9]3N0HCYV@H,1];<BU%IOFF@M@A)%XRBN^@B0]((%!W]P\O.CA;I.@F*BA#4Q]\IAFMI DNY0\@Z[\#HIA>7N]=OI@II2E(CPAZR[]4@MA4S7^L*1U9IYXMQJBE:,!7;K!Z3,VD<]!7<:!XWYU3C-@D0W"O]9^O]YUTU@"P@^&GU&BPB"%4BK&PC&$]@/&PD!$SDS&QE%$REW&QE)&QF"&QGM&QC/&QHJ&PH#&R@E&RI!&OJI&R1;"RKL&GK/$R$#LX$3J9_#QY$;)7$3"9^ DA@C,b')
%
classmethod: R4PReport
imageStringexample4GifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 217;
			add: #Height -> 89;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '!!!"L!!!"L!.]S"!!%P"!.Y''"JAAt9_VDo''b4#B2!!!!:!!"5D!%7gM1B7D9!!#7a!)*@m<r`54!"
	;Bu$r$^6=TB">!"j#I,<u5RMZ>;R!%`j*3f3nuO9\FH!**>*@q5RDT`AE$$ig89!$Vh-9+3Ui!bAYJ!"
	KTOYT8//,6.]g,7u(B(aC7K,=#0P5[G[$AKsig-"*>hM9@RQ!A5uW."/oh&NV^@>;Jon1KAl]@#HsTZS
	cEl3fr)T"op[aAiPgr643cY!"hI@$p[fs6"l3S,7$pu),3h"7pK:l?B7[$Jkh3?:S$$9&-,]d)-&pJ>"
	^YM<Ba;Y;H-t+=/b"_!!$I.AQpr.A3#]W<^L(u>[LiF?!r_"CoCa/aCP?3A.ATIL6,c%_-X8''B"%_Z1
	_FXk)08G$CgLF]BP;79SSt(=C8TR*YM"(:!-]h"G%Ep[F)uPfWH>c9F1q$j1&uYU0mtl;J3b3//.@252
	2#m:Ku;c!_WQTlmu&CjN&_K.?oj6W>F0M7N/kK/`oi?+o8ta9O?*B''3]`?`Asbk"OOEHWg&-rd[[ge\
	T8T,*R@0sd4K#*TWl_07bikhUorVOGW,,eBMeTAsZEMdEXl%4$g"3@gqlS`LY3FfOAcT8O3jWKNZcM(2
	ho<,)gU@.^\8O%6Gr<6`8[t%(]@tuQhXA&=q8.P?`e6(eKgrq;BYfgAae4+&`l?$T]u]+&`9mEDpAW9o
	5g50XcHa_]k1S&SpXPU,f%/F&q#/s/3o$</gSo>NgnW`_^>@`''g]-$^rr;)]B&gXXkfAtHa/n=OQg!8
	]kNBkJkNCpm]]f:lm_t.PcBdqMPj[SfmHVa[mdBnpMtl#+oA$kMhS]*NkOnQBo`"(S[V#HaOo<psqrYE
	fm(WGfa8bVJs7NSqpV$S"bl7IirqG@Ns0ht*d/X+%rr2]lrr2rsm%C#IJ:N1M!!!$!!<<''!!!*''!!<
	3''!s8W-!~>'));
			add: #Length -> 1650;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVPRCN*3@''Z]sC1G(%POI&e+;?:c@''j@5CV&$1h@^%9/>SNu&W8a"L^dj0;(4fY]fW(n1]2f&)KV
	=)ZUpL[^4sm^sL?\m\@%=ZmCd*46hUcYN2m4QrE9I3J^"FYY-i:N&p?\BA]%s59/@emA0QKT6S.N5c/@
	V;CiG)ZWKQr>[R,`E>@!_Q#2R$:QVE2T_0jnqa-p>C/U!a,)q_5ia$cYbQqHu/Z1Qjji!+Q77#4iZ4N%
	pO3gkGop<sB_5''*R37b$qsS:K@Sro88O6j"dH>Gu8ug`s_FrgPW;+jXWNH:_h!B<^d/O3-I>#Y\FWka
	V2E^^9*TcF2<lRGYNKj5C%MW0\OL/C"hVFdc9idcGB%&BnN._jfT^[/-nSP&Ne;D4#,n/WR:\5nq\M;q
	XA@(_VSM3,1gE.p=&`t"Ftb''Q,aOF#)WMaV,D"JN=JYYHX1NGTm3Aj1OI:kWRE[J(a4O`*`l%/W!@Kf
	aX=?F/SQ!:aH_"'';<*2;$(!^TR%*>c[Ihlf1h-:Vq6>Qpbl)BL,6n"G6-:<9q:;Zf70KZW5BHD2S@GL
	GB!>balC9S\q6i/:F,XAC\`KA,AWY8mUtk''h%Lcg.)7e[%`3FroW?Z^C-7_E_gto+;k)''5a+ZLGCJA
	IGL&25IYD[UI%Jk>\BTh39^0Zg&c;R&Bn*XjBCX#fEgOaY"Y30LFpH&JtqKa6WtFuTK''=qF4c''PK0*
	:OKC.r;eW4](AM3f%CLN#FmMoM0_r2%SU#k''++rme_u>A9*2oqb,T:0PQ3Cc%0;Eo"R1NKm%I[-QQa-
	I,.lgLp.JfTq^EBb9V-rm"61jFN8t\hXTWE!0RJ%5jX$5Q2i_C[?17P?EgQD/0Ok,V`.9?R/7+%BZ\e2
	b^$MUi)*uoR.Pe$\6$F5G"pQS:V@3s3P?CZ*6n#As\/D2H&,q2KobTn,)#O``hXLSNB@@srY-D-k?`0?
	4MY5or<*oBhd$3s/JTWTfc!,A=k-Bs3f<QLq]@=PkbrE[g:gT^lR_>0s"bSe&/V_=8/NDj"&<nUF)r,Q
	@b.N/#51j3q(TRbi"G2.:B+F]5%3Wkkp.)_e1N%)C4S1h0Mlq=K`F.jeU2c*nBN0s4=AK7GZ^%632=q#
	TSX[+ZcSp&l=u1&0e-lD*D5EY%U2c*g<X*gt=Hn_A",92E=Q#9PXTs0`C/R,8XM7e(o)H@A@N/%ME*E!
	EFKO`aa@=/L`ba/bh#dma%qFlY5-n)j4.MQ%W.O-aCpl)!qsRN1.JmU=R()htnL<t`#jPqB&(H[=Z5/G
	uYVtZ(A)M,@fnd0[r#IOp=)u-A:`QFCbdWF4f#e6`.oc@>B,^!rikkVPq7"U[%=Cn2#&uc(Y5eLZ5UsP
	NLPA"oU$2O<Q#8e<\`YTJ,E[-U*K&KRK#[.Z+U0`4^]S(6EU\bL<i0CJ4"DWgR+`*B4Z8cE("`ZQ#2Lg
	2K/,I!:pGggS8LV:N+P><a#roEb(K]s,``;"eHI-o?Qg>m,pbX6F75G>FOd_0i=8Me%IPdtjJI8@N^K+
	TplmVKQuk!2MtQR;0"UmnZ9mK#\#7Pe:Kf.b:F;G8AqHFYaTdrdhYp6kXaCck(>*ga)ReR$D4A<A,=V(
	:He&Q2D"*qLb^!67k8V-59[]dq3FMj3RBp)%s5YZ)JIU+iBuJ/SQ.dri9ss0gYQ_()`_ANL9K-t?/(Sb
	$gR,i>D8=GuQq%)"AhVX`<.W7Oje>i3oSI!<\d##N]"AXL5)9OeC1dnBZiMCOM^5>%AX[A1!FRpiEe3R
	_R+%)m`SkD:n$E,]/"A20^Sh"0e\Bu!-7O6a''^ljqD1T:<pNb=NJ(O>''32l=A)mHYE_O^/#<>8[*5,
	m)IIDJW(H)P+IVp+r7W<moj:uRs''@"Ibu&&T[Yh_`'',Wdgup)i>=''BP02gqki3pE\`>mqQ+3*Da6X
	nQ&=7S.Zh''8<qJS]fr?;.EhHNn-B^*X$Lb@UGms+nroQB;Kh!fP.n/p''OOV6kJ)e03M81fj<Ds9D@r
	-`mQK4i5)GD]LY:47X!R$ec2&.o!GI8>"N#[eSbJIg+RO4C.AqHFY-C>*OJsg!nQjIAVR:''a&rrLL''
	KcU~>')
%
classmethod: R4PReport
imageStringexample5Gif
	^(ByteArray fromPackedString: 'Q4%FNC%!L@E''@G\@@BG9A@D@@@@@K@@@@@@0@V\@!?<@?0@@@@D@C@@@FP@@H0@AK0@@P @ER0@@U0@@Y0@NV@@Q[@@YY @Y^P@#P@@,! @9^0@=#@AB^@AG''@M^+PA4/003R1E(,1$@UQ$Y^Q%F''Q%^#1%^(BE!*!)8/"D@@BL#DRD!G2M@U"MK\"NI2"@@ER8.K#HF@CTQN#MKV3MK\3U()C,@@C(@FTH@@D@@DTX@FT@@KDH@L4T@[$\@]D@#CC8>O$Y@]$I@ 4QA"TAYV4E(#D-0)TA9.#>K14Z\6TDL@D,#CD,3FT1LSD!H''D!4/5U@IUMKL5IRT%I]V%A.$4:J/UBJ2%B]25J%3%B$7U8@@E0U@E<YFU9LMU9^T51\V51 #5:K/E:-8%4N@FHYL6H#X&Y&Y&]"(FBQ0FR+8V1^W''I1@F!(ZF*&6F>,4F.-7''P@@GP@RGD@ZGP@]GXY@GXYFWXYL7X5DWY@FWQH''GY%PGA.$7M3\6*N)7N]0GR15''V<:''R??8AAGW-;^7?A98P,CHT<@H-^KHY"OH^G!9D1CH- V(-2Y82N.(3A7H+D983F;83L<)P>D9M.$9)6S)FQ$Y!AYY1H@I1H]I5K_Z](OJJF$9>K&Y&Y&Y*+!9603I6=6Y?L7)/Z>I3_/:Z&)*+I):WP6:7P:Z5^H:%''@J5"X*6KW*6KZJ>0,J3X7*7_2J7#>+FJ$;&]$;V\%KB+";R4-KO_1+[]<;!/K;=4@K98M[55Q<BOX;*W\[6+";+Q:+;W;[;:?+??7<JKZL^-(,[F1,K\/<[]7\WY;,2KP<2N_\. ],2$$<24),?O3<;_<L;^=</&>\;''?<;4?]RVSM^]X=WL&-[V5-_$<-*\T-&)]=&:#-2=+-;H$-7O/=3\7M;^5-3 ;NW%9];''9=/''=M;)-=/;?.V,Y.^3\N''E$NWK-^GT1NC 8NW:?^>?,>3B!/GW''>;!6N7*1>3,;OC=?/&>]_#L#_+^*?OY-?O[/OG1/_[6=/7>;?;T._7[''O3''.?3''3_;.0?;1/_?52O?:3?;85/?9:O??/??>8O;>? #>@F,I+HV-(LFCBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7C"0%(&OHDNJGD&2)L&SJEN*WL&2)\.WLFOJ''D&3Y$\S?7K*7L&3)<>_PHLJGT*4*MF#RIL*W\*4*UN''M?>%&4*5*-V+VKM*7\*5*=^/XLNJGT.6+M&3ZL=FS\.6+].7\NOJ''T/W:=*:^OO*7\.7;=:;UY?.=I-N,FF%!AL+Q .X:#<1$BMK''$19,%R9WKP>+,29,>_O(C5_1))9,^''SUA-O__1T3F"8)[F2O''3T=UKWV&M''''\R;->?_0G.#G+85Z,KYSU5/UI .<4K$-H&:#$^=./W+5%$/[I:.8ZR&$9[>"1=OG)-1!MA/?2-_$O-36=F%?<MNG;-69,8Y_&\Z''+7??1B]]5A:R"''G'''',JDQ#_S;"M]U=BBB;4WW7V2VG!!WKD<4=?@G[((TDBF*P T ZVE2E:<B4XUHM"OX#P"P%MP.E5FE)HGX\_9.!_"@VMVM-:A>Y7WH(*L/ ZVB8^ABMBL'')U(13)1HN##%RJ12L6O!)UH''%K=$!$$S61FEZRA''U)TIM\UQ[%%EV6"UET8,P)Y9[?7NDBOR/>H>^^\W;1BY==./@M(G-2Q: 8O-*I93?\SAB@F_40:""$N_4PP@C+9EQA)$ZR1]*!!!8*C))Z\[Z&*J"&*.** CZ!1J/>,B*AB*-;W*IDGMSPB"^_O#XZA 6KF''%(K3&X06\/D_0:CZ*!@** +<AV>,(?O41[K[WS7.JHO=+: 4$,^W+*F*"Y(T(*ZZYJ.^(?WKS+K!]:4"(/(Z;F>.*,<9KBP13!:C(PS,9>2]L-4PH5K*FD1GHM(OK\L,N2(#[K*<@9DX1''(9%"\ X.D6Q<Q#?]Z(/MF9DZ_NQW''1H*LZ@2M!SOYMRI X6:* H5;<5;<)OSD4(H$=L8M7N2[::,;+(''''Q[''^Z O7!1J2,OL%$.(#4%WSTL&4RY-JZX?\F+2UT"DKSXRU:TL:L)<-.12YODT5C[MNL\-]92.8"-''/[K."S_>/$K#J(:-+?)MZESUEF884 T[Z_#")JB13^JLO0099M1MW##UVZ\QJ\FYZO9O4#)5J9RXUH5M]-&.VU9M9Z)WL4$:**:\3"R-K?9OBBJE<D?-/O_^^-6F=;JD/YETH3315_QMC^BAT:L:8Y@#G*1O*T<>!#KW*O;4MKV3O#''&^E*M]_"I?5LDOU-;3QO))X^]EV.*^6?9:8BJ\_[Y-O/>#0$6=H>S;0@LXCV@U3"!JVD^?3@EC9P CPL"THDLUA8!%I@H_>P$^9ZC''.4(E++2!V%7$N,EC+"''O\&5S''8[AD+R.DDDOF$LE27<!<YJI,MX!F1[5@LR!L#6G!ANC(W>$G,]9K  A";4[''XA7I<M?"_@I-ZN MT  1IN X7BLTDI.)@"EZ6HQ^W)*7&-4>C!NE 1C:;O!8YKF@X--;7.YTY58ILV-.Y8+Y10 6P7-N@GKPH?20E1\Z=+A!^ZH\#HCIJP"D1$L9BXQBX:<YFS 6H]%E@<805/FIN,9OFF([13#NNJ+=KE<?;5C4T6(5^N.%P#=O!AT3KM%HQD5*TB0@]*0GJQ&[''%,5HY ET6*)\V?NT*]_JCY5S*T.+[2YX$0))[\.^V!@1$DX$(&TL:\1KP5FT6-<''M[-XM$QM$XCL.,TA*!ELZ822''<))QCY71CAV7#H():RP_QWJBD]W>.FT;]ZHNZC8S%/QT4SH#4$1X?/NV W3W.=JQ3]%5<:DP#Z )/4''HZ(@RU%.<:J.(JDE; ^NV- ")Q4*)2HBN#*RHED\?IV+J 9ZT#F@ZJDPJ6-I\P/M51EA(.=IA#I;:=J_D\B!K!4+T"\(J$^7$6Z.D4\>$0(*)3^ (+N[!CIBJEB_3!F$=">)O&6XUSD*;FT4U:UIS8%R''O@V*V(WJ5[YJUF_#TJSN_)[H._8#+,4810W5*)N*V-TVH?6*T53#U(M:=ZU MQ''N1)+H,"+2+@)U*6SYV-#JV+Z2/,!,XAD;VIQ^5+FDMF%<C#X/1"HR-H L3294"(Q].OZ5,GW>KSZ0^]''Z6''Z(&_WEY!L)V)O]E+V=)T75&&!ZP*H6&-#X!TI"253%4/Z64H4.MGN;V4PFM8^?OR1/-R)PMBZQ,HY%:D43<%3)&-^<5LVIV*?;P\&:=;6S3X1;6R,X;!+I/]2A[7C625?8>/^?@@:0 @\<8OS>X;6!B\6ABP3 ?D)6L0&N,HP''KI$E+5V>CL:0!#_L8P8S6LC+OX2GFY1XLH781B!N,X(?+E''5+/#ELH:1#F]L81*+FLP63+FN]<3#G/,X/##>,YBGSNP"F=&?A":1$)_L9BX;>\%IRV=M)$3%J%/92%#N,)Z7/IK4:/SKX@93&CTQ BFX>\1(S+O>&-_L9#Z;>\50#+N\943''N-/93''#N,9;S;FT1>?''OB"W3'' ]M:DH[>-BHS+R"952LQ'',D4IC>L9%MQ>%JV?+R&L:4)#_M:T9;>-N C+V(Q47*S#^:FH>N-J*?ON%R.?+U,H:5+F]M:5)3>-R)W+V.6=W*,DG95<@N-+B_ &,S;O+X/T[B,I_M;FX7.=#G7''V2''\JNC52J@E# R[V/''V5''=6S[@\C6S>S1B69;.2_ TLPO?)G.]Y=[JMBN=**SW^9KIV@STI %BX[RC#AT0P#"7$&??170]>]$8@C/-$> PWBE?<L_''  @A\ZA%G7PH (AF@@ORDFNV32EG"8X0@?>PB92 0\%7/JN]KJ!LXT@VL@J/@"DD@ZP #DX)Q,E90''NGV;2_>1\JC?WBS=\DXA=G<T\T.BAKWK2C$T40NL_!<F::RG5''/<D9VIFP/>63/W>CZDJ$ ;@6O9Q[ZNS60BGND+P^;I6*;_]I64'']=FO4 8)>D@]N?DGJC@!FJ)O/^)V;0''V062CUQ#><HX_1RH&,P(. E7L429;S,:^=)3,X1YABO\NN,G6''N?$;_=(> $478*! G/"?9A;@M@^^$VL'' @;JO7CH1:@AIA"E#TX0@*R<W''O?:L]_ "@@D[ !93K7^!D$P\,''-:S^Y"";5W7^>@E;6!#?;''0"D_<JOK>XG#GP5;,X"O;A<3>B=[?8198BH@C$&BD@A1@C3+7_T;^W 8 .K0JP''A?HXSR#)Y[8@+(H@&6@@5FL@@*X@+5]7?9]0C;10>9P@T5P@@ (G9GP@A_4G,<109@H@@"4GK!9''CGY1S8(@X^ G]A4WPAT@AW0@4: XH*2HH90P?A8@HLP@''QA0MKLHLFT@_*LHJWL G\,@>HD@AOI0?Q GD[,GQPM''! !''7Y%7#\Y7#SAFZQY663-G))Q68EL@X&6@9-D@F<)1N M7<9%0>H(@C;%1O_D@U 2G=YLGGJH@@$PF8H @%&"HX:,XY-NG.+Q0_#,@?EDG<N)0=''&H[5 @_>1*\L</\S:?@AIZ SP#!KPM NV<A7;^@FGLA;%F"I&L!;?!@KOS@N?"@KK_A7OG@L< @MLWBA''0"DA=\E2R@O0KA?<+@LFPA5S.XL.)!+B*U5V6\BS7!8(?@G^Y@G$6@C73]65VXAU]BLP,A:> @HU3!KB_A<X+"H[T\OKN@AN:E<A<A7PRFDB<@JP; @)X@HPJ"M7J S7 "NDE^GPIFM6; S3>A9X<"H#&"BN,DNL&@G*P\K#4!6?Z!<@\&O?, NM@A54"]=?>"J>L@F?/ O2>BOO#!KAQBQ.["K5*]SS+ JRABL0# J"(^LX0Y>/"Y9*U]>ZR^DAY@D3_"RU(C>"3:G#S''GC"^0#)LGB0]P^TEQ#4%P@"D0@C+0@#9@]#_Y#S)Y^QAW#_EX$4]9#S37C*O B4XA\*:8D?SP#?>@C6/ [%-I /CP%S(1 !: _E^9$HC7C<?@_NPF!O/0BA:7C!(@ <F&"<;@">?R$Q<I$("''BRQI!RZ)[B!I^R&9 2X(C=H0BAPG%S?Q]/&0A7"(!%G0@FDXEB@W@@*0BU&@!_?0&IG9C6-H&S&1%MXH]6UX"C%1"C''GC;J@@%:0&DL!!C/ID5%I$RQ( &PIC;^YD6Q)%#BH%%:9%%CWC%) A>W [..@@^AX%1+Y!H"7%719^G?IZ(GI\"8G\3IG\3[>]8#*%0P^>H6 :W!F0HE %8[_HI;$2PU)&HC+-8C0E9-C^@GJE0B">P?,&PS.>P<NBHDD(@O%&X_(JPK?ZY<[6HGV1(EWXHH6:W8<ZW(.,H\9TY/?.H\OZP_JQ:DP*Y[L=0?+T@I?96;J]7^S=0,RH@# NHHR\@&VU0-PY)]8:R9:FY6HM94:QV?T"F?:%!O6H@(5HG0"L@]T:WOS>HG3M:PEI7*Z17''I)00WRW[36HJ.%:R#RW.3UGC]XJPJ90;A18E0\B&(=0?;@@$B4@E4RQS%8@HRX@%8]7%Z>P=H10O\TH,]PGE/F*_KLJ^?E0T[\@3;H@(!=0M\^X*)"@O>,'']0TXB''JW&EDM)$K+*QB!V#L&)8MA)( S%=%,(T=*@JFI^B\4@JN>DN_]BG]@&*(*(S)L(A3G@C%TA1=!@JG?BBOBDO,XBQ>#&C@YBD/=Z(3''%84B&#$?(.47Z)0#*,[2&S3JZ+W0Z)$_*+;!J,0?*, ^\MW.%,2L*Q31&)$)*L8P^-7G).;N@B@\@@)T&-3Y&,53(J"X^.*2BRL:*-/-Z-<L),;ZB''R''!.5_*(92*R>+*/;L*,/EZ)<Q*0@ ,&6%B0K=(.L\*/B(..?,(E3#*0DA.1S%F06''B0WJBWB;.0C_.0D-.1G$,TEF.1!Z^.G9&1>;(JF0.0G;.2KM/>D2G+*G&9+"H)A67FO5,7AN:*[B6;,27;,,:)+9(P-DH;-@5K*VOG,4#+,S:[+B[K+4TK+B*[-EH+,D-++T7[+2(W-UN;-]1:+><2AE4W-#^[-T_K-V[;+G6V\)CF,V_[-%Y7D6)K--/*-''S[\7@[-8CF-''V;-<16-7#+Y7+K-8HK[G;;-2UY-(N[.HS;K8XK.E*+.ICKYHW[.CW:.IE;.V@5.YPK-X"K.Y9[II*;._?Z.Y=[.+PQ.*H[.JZ;.$2A.).+.*0[.4_!.)PK.;I;.4IA.85+.;#[.36!.8[K.;8;/O<@/G<+/LS[.<ZK-<"[/K^;/GG[/L8[.=B+--K>N;6+V;4)];7XV;+ZJ6?\6;6^>;7-H Y9,@"K<@]2LHVGN;_"&;3$J0Z< [;(&0_,R96$>;;J2;"+I ZZ,@!2,DP6H@[*:;#9*;?O2;>Q9+>K$@\6 @QUDCXM[LC."<C;J1@0:6_>N0%9X@HPCGYU$K:OA9 G[LF,B;4[7LE?L@&OI;=9LLK8V<D&KK/K&<H&,LJK<G"S<@_^5;;/N,N8Z;06?@_?Z0I -<LACLNUV<I@?K''@N<QE7H1H?L@T?LMMSK4JCF[>*0$*GLUU LP^+LQKKLMW;K5Y+EM"L@)\[@JK(@&SXLQ_CL@&$@^ZHLYFR<Y%OK9''?B92DKQ?0LXT[ 3G?2,G\:0I_6CG''H/G^W29@PD@N0@a')
%
classmethod: R4PReport
imageStringexample5GifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 304;
			add: #Height -> 103;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: 'rrE''!!!!$"$ig8F!!"5D!A=EQ63%0^!!$(#!+u9A=9&qL!#]AK)0l3,5QE?i!'',B+4cBA-GQ:7("(
	&)0FMA5?9+bjD)$!YS)0m)hSJbap).&sZ@B(5j^**IV,9mdD+Whpo<[17X,DjPc!#7+=/i!V22%_l*93
	X2FF#@X,3rf7A!#\*''!''gMr7K=;k!%^G:1IV:]7f[Wr,7Hb"5"7e\69p/k6&@j0><uq]95p^mH+H4u
	a$uTP5n!`d,7I4$),*^t84]7AFMCg@,u[Gm;GpCj?!:''BP>7"K:l<ClS]ap-c;:=m?2st1''`_SY).#
	uo?=$Q8>[(H@O''[aW?EN3m%KKrU1L_S<AnGXf@\@+k^gsM)CgLFgE<''ZcBPOEsDlh<$Xkn."!-A33E
	<''Zo!-EB%!-T4tFr<)Z2%b!s)0AE>G%EpXDNf-_F)$`5F.L*gZ.=''1l?''f@J3b3&H[C7YkCbApKff
	?n?7^h?4GS]IOYI9&?s8J/B8hk^N5=;,`93*!mu5K]PWfbEDNgN4:6C[$QpG;R8-#/NFIc''QVeA2eL6
	J6fR@0J2R]D47YfrZOfq6LFS(dY-hnQ%cVQ.b9V9mu!d-7Z=,H5;UX^YbAMdENDBWnYEXOuC3hoMq[qQ
	YpA\XeqlS<U7YMmTjuZerd2h=J-p0>DT/^3DNKF[s6.A%J"H]rR(&dH\HtmCrGr^]33.MeL/5U;k3m_V
	DoGh;-''''m`^SVb`7+pTQ\PTPK^D2cd0q_hseqJpWM^pcKG5Qo_m:*9\5hFe]aK5f%/I6o%KU3fr92K
	\rPRZY276_h9Wjkgt^cCf%f3MjlPU\kNCU_o\SiAg\_!3XCgY%E9Y?''jib''jeAU;''i8sh6n%Z3q_M
	7U4TC1[jmHVaYm-O6?rqX9Nprn`ohQ-8$\+A@enaX<dp@eIan,ChIrS*k?kI^ZTc2R.Krpd]%ouR10ci
	3^js7tmYs1eU5i;W`T~>'));
			add: #Length -> 2360;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVQ>$Zo<[*<>U#[0=PkGb>]lHW6(1TCjppF%/4,@E@r^DVj>/MS"+\D!Ig[](fBUY[]!)=&`YK:mY?
	Ged(#IURP(=bLn<]5Tkh%$nrV<p5rcNLY^DPoDa75p"e]cRsWW2(Adu*F7W3)oh4^+htOm8#]t01F<l.
	a%cHVknZX%`$hLSAP#i_9S&)eJf49/0m9BHDQD[J_&72)F''Rfq/btBTT@"X<6TpOHEmEEbq3YI?6+6s
	XsM+RH)H8-qR,&J<iQBF<%)Cc0Xq3[#&acQ$4Tul_(\^q#GATZ$JojUo?U_r)tVI''+1.!lDC02jO[0Z
	gAVHeJ"R>E>cQlD:19dkB5]ZY.gh`;6)V8aT+BS%WcE9$5@Ek=AUJ:QEOe>_gZ+`\/D^l=DYhDm.GZe@
	^)T''(>QdZ(peY>C@HOjF,4i.oh#L1./6NmsImAT7>Iu_q&/b*-HbX^1,t=LTbYYT"(-EbR=<E^_?HWn
	SJ,i4YMb(`a5.hO%Mu%G5^B/l\iuLWZJt7:]6+ka/E)1hQ`6W[#He.Z%e"OAA4BC2SDR7^=Ds7`p(>^D
	=cCc?`%-[:8ms]]VO<.jG`HG\cIH[jd!]s(BoH36cfZ&8%QW^W(Ut4P>JHbODAZq=+Z0lQS]o6o''(WN
	Pk=gi#rUYJPVh:`1Ln@]*pJWf`f01G_ptYJ+$4Hd&aElP;PEFb-.p"kk[/uq_?nrt??"\B$f..6e.Ub0
	\A1`TlHj=ahP3iZFR''=ok$b7QkO1p#X/M@5KCp..-*YS[G.S`5a^3^a^9b?"j#b1))inh/I0\WdUSW8
	''\j8-Xl''6S`0WAFO8Es6c>q5[SSP#N@D+RF6kac)QZY,cr7DKm_a^1V/rAp?;XS"`M-H)t(__*Tt?;
	U$[HKhSnE7)=Bbf#DGF0E)<*[b#Y\5T0<$c!a%a^1HE#Eldq2`2abVpfEi6U%4<P+Ge-:EY<3)V;Wt&M
	$U)065Zl)"?''#l>-+f.4N_AcoL"o-BaC^G<flZXM##hNlSt/oLk0sTCn*)#ECkQn:K\4TC5U(X2fEP9
	o)k50acOY*Qh1lYC\Bs%bUL2K^sID$c`-b.kdCj]e_=P\RN<hi"[oa/$YD+H.\$I,1S=ZcRM:D4$2hVp
	%Fk<j(*FX.pUn6Pg+"9<fhU<?V=A%ZhmDd162`d)>$9b]i_EmG`C^iI#k*t:/8]\nMGNV.b=jheQlK7#
	I/XMmL.)dq5!*5I?KHJbjmO!^0>j(''omXoo.l9HpQ>l7PukG;oP#V]4J/Mh9\eH@p3Bj9jFuI6r:<i7
	?KLspf$J1NK[.abm*Ae*Ih0mh9nW,u_7dA1FR\9u2lIN''gHiONp*,)hc1p4''c=5+k4V#-;1Hr>#q=^
	91f156d7:(U(#ZZ:$5:nV^1M"*E3o@8JlKE(7]\pc(\IX/iN_A#]1*7<5b9t9),t?`"#F.g>gX2;NK$h
	+netYi5bJ.P!dgao0ere^OGTFF;/\L*shcO%Wq@.^k*S4''Vpr''fPArn*ora,UOd<P(;:FF.p[Tm8+K
	nnPh7rRS.bNm_gQ!gg<#A]We4YlohGga95B,5`''DHkLafA80t;rZnVP9UtS[J9uAYMA1,5umB"`8''\
	&QDH:nGPT2aFqpuK0S_[hp)9CSo(_du2tlHIa?4u&?Mf8P=*CLIY0OINX]J^`d&$mqo_RJDB\o3.B[2s
	h]M@:9(()S>S-R^N!?6@@It#mmE-:7]>."C+J,\BsWn3nhn#KX<c7jKA`T#X;H>h_!Eqd!?bel5@d&_@
	=2ApqCp@9;(Y?RTuS\;"PjDKXN;n6L^74e=sH?</"=hWNW"#!%D]WthL2Sb6qHu=CoZ.FoYQP-9]p!Ng
	Dm*7-1HRkCm].OEU#/CS8rP`kr%m.[%FMJ:Yb?fN''pA$r_on)c_%%300ETZjopL#irDgutWr380o0@)
	H-K5Xs)hl-h#bX[#cIshl@pUr''=`,#DPrL+NS>qXC&.1ccq%D3G`K!5m#?MZ^G@W&6OfC594Q1iEA[^
	P7c0DfTB]''NMCeLB=)na2ZrZ1/5-jJe30H,2gpHR$a%*^HSLG]6fEfoOgW?o@N.7Z@(!@^=DnRakNG(
	L:5$2@V2U%D".bA$1+.)M,gI0k7E''V$`"Gic(Q*c#npoDg]_8rIFG*F''Gp>%*I(4U:UeZ"gV<&!]VG
	!e4Em:^IghDR/1q*Wa3%@p>bmWGi7+$a7[X($R0''A"c#hA2J*JJ.rDR6^ERU*QhBRg&RG8&SRb7V"rM
	X!W8!B.O1jskH2UgI=ai$&(G07;91b)TE9>&tVr5D,/&PrmC3*VP?S@Le&(MM?dn.<Y\mhr&LU1Jg.-L
	3QXAgeXg,Ym/A9T5,U^a!ir^Q`gqAGs_f:\#?lc997MWX!/0nZl3E?(kQOlWb(e3j?V\5(k@V"r)T/bi
	qFE%]cM0;de-HKk;''F,qr/;n3Dsa`4;CO`eI8EkCg3?4X7VMT2;gRFD85T29upS`+kX-Ph2_[NIR(K8
	OFOp<]em"[DC0c3CpPn!_De]-aX+RJNtUaZU!UrQ%-thVUS&RnQ!CKQ]0Xkq5Ef_/&oG)>N!*lCi%Of0
	]&k^BW<B<*jV<hr!sf1d:ZW)QCUSWHMJ+U"+,9:L"<7c@]*ndUHg7SLD&!T7L=$[HR24EdC/Gk9\q:7P
	D7QFQ0TP0l_4$a3s##Z")R)@U;o;E%``F-sp?,]V<dXos7m>r@:q:AaL40_p(DA/)k,D]""*kqD''U-g
	rlYr>34>S.dg:JcMfs?pS.]fOaD($m_ulL(\coqgl$N!0CHfm\/4o!?JLHcEA-b!]snmQROTsF&.CHSL
	tp1FQP^&5+T`k^ZK!6#JdlUa>AS''4?8fHH/)^+.?>hO5]o%/nO\ijT''a28_C_F6Z0e^]"''G<qO$ZN
	:Y?8>RnUd@e)mT%1uiedkE!cU$,\X0^j`[)0K0<b,9?=G1sE''_n7?<1BA''Y&aW&E*sS\,m91A`_GRM
	)<=Z&^&YB^%CiE1`tMK~>')
%
classmethod: R4PReport
imageTableCellGif
	^(ByteArray fromPackedString: 'Q4%FNC%!''0I*@_\@@@PBAHRB_K3B1C1BQMS"9A0"GNS2=I2")E1"YLSR9L3"=@PRCLSR5B02MD1RUH2R#J22-FQ2]OS:=K3F5NS"7@0ZIH2R''A0*MNS&=D1VY@PFEC1JUCP:OLSZ9APZGL3Z7KR:1G2B#LSJ3BP*KNS2?GQ:_JR*+F1*[N3*9IRZ''L3^=DQJSM3*=@0RGLSR7EQZWAP"KKR:/N3:?MSZ9H2Z)@PJAKSB3M3*;BP2OCQBSL3";IRZ%F12_K3N5@PNEB0:QH2J#OS2;I2"+M3&?APRED1RWNS";NS.=EQ^YOS6?FQ*]MSR7O3:?G2B!AP"IE1&[M3"=J26/AP^KBP.OD1NWL3Z;KR>3F1:!JR*-NS*;I2Z)LSB3CP2OM3";@PBCMSR5H2R%GQ2]A0^IB0.MD1NUBP^GM3Z7KR>1L3N3N''6?I2^''M3*?APVGL+V7FQ^WBP&KLR>/OK>?MS^;@0JCDQBSLSN7@PNGC0>QH2N%OS2=NS.?F1.]HRF!BP"IOS.;LSF1DQFQNS"9H2F#G1>!@(REEQ^[L3N5APNEOS.=JR&)FQ&YL_V9MS&=CP6MEQVUKR6-O3>=LOJ5AP^IBP.MD1ZY@TJEDQNUC0>OL3^9A0^GHRF#B0.KG1>_J2.+D1NS@,VGE1^WA0&KK2>/M#^9IR^)K3F3B06OC1FSMS&;I2^%GE6_CP>QH2N#JR&+APVEEAVWN[&;FE.]J2.-N3.;IRV%@0NCOS6=@PFAH2V''NS*=F1.[M3.=MSV7O3>?M3&=L3^;I2^)LSF3CP6OM3&;@PFCMSV5GQ6]M3^7M3.?DQFSBP&INS&9LSB1DQBQM3"9N32=JR")FQ"YMS"=L3R5CP2MEQRUKR2-F12]O3:=APZIBP*MDQJUC0:OL3Z9A0ZGMSZ7K2:1L3J3B0*KN32?G1:_J2*+I2Z''MS^=D1JSL3R7E1ZWA0"KK2:/OS:?IRZ)K3B3B02OMS";I2Z%LSN5@0NECP:QN3.=E1^YO36?HRB!A0"IFQ&[KR6/N3*;IRR%K2>1BG9A@@@@@@@K@@@@@B_@&(AA0#?@G/%:\VDXJ>CBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7L"1(<^OHDNJGN&0HLJBI >&;HTO(Z5^-)HXYMJO)L6[NGF>YKH2)<>_PHLJGT*4*MF#NT4&,]TSX\-^_!@FN& O*]V+F@_NL/''2)L*/JF^JCT-6+M&2ZL>*S\-6+].6\M?J#T-7+-6:^N?*3\-7+=>> O<JC$1X+U"D@7,%J_ 4HR2*BJ-"''T39H^M^_6#%21^ RPAN'')-0F$6:-N''S*EN+W,6:-^/W,FOK''$6;-.7[.GO+7,6;->?_)?O18\LJA\NVI/OL,*T)4DBY%ZMK_8''H%"L@6KM+7<:=.?_/8LNK?1=O/+39<>#S*5?O/+7;=?C#29=OW364OK[0<SS8=G&CNZ-$4X94AA[XC@B !DFNLP,62NBCC$[H(B(PU"#!!QXZT4JFFD*8XX\U_,"!!"NRP2FHJIZ(((L"). "!B^.2FFLKL+8X(P46 !#!"7*:FJNM4;H(8<>@$$$"#E.*BRQG1))8)GF"GMKNP.%UI@3J<2A''S:3CE# %5[11LP%-U 3R1;O>FEODOX(->Z[[LX923M''3!K''&''W&8T^Z=*!999=0A +((HHVR."!!.J)''GI2K)+''(882B"^$%A:JBJJXI!+D(7=RF*&]!%:*:Z"G\#+))9=Z&.&* )+Z*J]5*/=J:*2]N%(++J!VR".-^?ZZY))7>"*,\(#T"X #-4CC%DJO9UGPI@AHX,H)1K3!#I] Y"/T_+7\@T@''^R 72;##",+&''*H"(*^^39#[9)-+O!ND/JIFN*":+NZ;J:JN-,/&+;!BF*!2@B?*:<A54,$../(RF"''BAY=9<I7=R/2+.06?>SCE\>:Y9<PZI6010?,V.''GHDR?<CLHD#>1/2XB^3B[AG!.\<,\JB8)12[<J.7J\3>P*+;36#G,]ME81 T4/MVT1P"V_QIDFK?C\L,$16&XMUD*W@OCH.I?VRR;XX<=R-LU6$!-D,V*NR:^\_XH]K+$=J53''&;G*/J&-Z7+?/KZ\@O?)=:& ]%)78W"&JV&\/![.A=>L6;6)8 $?### #Q-..^BA=05979I77N?&+Q:.>^J C1:D9Z"/G#+%MK^^^NZST,:9:*1_W''OZ,H->>^2]X=8:;W<CW7#%[8N^==:W&>6:<74&/7Z91?])O]!GU8''PB)WL<\8M(4RRA 13BOLXPX!(+_9M7''8=-27=.B''77MC?6\<,[Z,+++""-.4''7.N2''!?LQ+5YEH-M(CH ?(+VM X:$HCWH2C[AJ#@X!WK[F(ZXM0&*D@BY/A,\R.Z@1.8P@4FDHLXKA(GK7 6E''ZP!PO4X@QCJDLRY)B@DMQ ?![X0QZ^<HH0ONDG?0M(0!FJ,HP8EJDNU\!CE?[0!TPT8 2KVLH#7!BDJK3^B$?8QBAF,XX )NHPQ9!DEAZK !-LH_VX^LX^> FAA-0T''"1HM QZ#7%7.(8ZD,JDHACDE&<@ AQJHX\Y]BDM@. BO@C  YY 3RC+">QF.NX5?JU*[''EC$>.DMR^1FP-:F&S^6V3!!?/-RX981IL*&R\=-@&.]Q6SW.JN=:&^7T:V+6)%23"7OE^Q;''&L0!#1BJ_KS/I2\[9\7N:JE3''UDYNV1(SSL!V5/E<.T9#A^6X.]R\9Z")S&YKC)#N7RT9)A,=%#$O&69YW-C-5KIXIEND&$9#A;G$EF7$8$BPN8O<BUO2B@B8 0QE,8HELYBHK/\AFT2SI4H,\:AE;V%S<PAWCV_R#U6\[V1;:M3Z ^Z2A[5P%._I Q@TV#U!0?FC[IE!FMP91"B''\HS4!>DLYH)FIJ/T!GVUJP2H^$X$>1BEKG7!EA^ZT^#-%X![I][4#-/FIMRT*CR\HT3).\Z$>)^%S''S!TG!ZU*!F4Z!N#&MF&]!V*W_W_UJ.8T+FRDJ4%-VHZ33#@X"$'' 0 T%0C!E$CC3TH]@D@ZP)[VC55(BPA6>LDD8/BKQZS#EQ+0@''Z8 Y>0MOR2D*G$5>(D/;-&(Q=ABM\3>(FBMZ''+-NJ:W=C&1#XN0&6%8[)+N9T7OPS?>/UV/K0UK-''9M=;:3V^<MZZ9(-&(\U[3[S.K''I6@B4/$@*99;HJ.891+*M<^++%6F.9.(8-[6"ZW-,1-G''T''A57# #N;!;I.8;CKSNI:3+_A2=4.X3%@ J72T''\K9Y-4ZEQ;D(PIEK@EJ<S@@V4P@0Y)ZDL$YDBBG70"EFKXQA<N\#;LV'' ";SN KQ;UI#"^RU1%D5-HKT(''&/J)[P"<ZF1G2,TK)$%,]DV!R\6H0:\*,XH-I*-K%V)#G-H0#VF\<T)1N$L^O%F-TLP"RGL8X1(?$JT0_[HK$^1EI]?81#Z.*46!WNR6R''F,SW; #;T\9CXN6Z!]C*)T$?3%LX^Y?<1F/+HM^5+&I?H/ WY$,Y9P&J-W@FBOI5&ZPI;Q!$)T@A&KV@L*.+DAK<S@R8!X:HT''?YVC]N4QZ5INY>5$B3+587;8.Y>(KP(6V1S-_''XB,Y7,JL\D%+"D+[L+6\8KS.OFB[>;IAW.^*_\S+5J\8LR6Z)6''[-^S>+W-P.)[''<7JFIC3-[TAGZ+\(4)Y1>7^\#FWKB)+R%+''=_W4T96,66''OM&1,'')026/!WB")NM/I''&EISB=XP@0)KHHE7VBDGB[Q ''=X*Q_)(;S@E?IP@4*TVHD@+R4$$@\TAFI(<8)-0,LEZ(D(,A>HTE_Z8-"''O@S!47NZE9LYRJ\9''%CLX/?$<UJM*NT-J?FHN]Z"#:MLY!OVFH0-_?KK!^)C&XM4""Z5^\?Y^&P,&#W&N?39$F/^T#MS\L)F_>/M__;2)P^=:S />,&%7/N$U;7N]HY3V.TJ992>TXJ()CF#VB(;0@KZJP])2R"<HH%8*J@[/8!GC+1PB(ET^.@C523>[KFLOHBZJ[OH#)Y.(Y5>KJLW27#IL(9APCL@8AZQ;<T,#!FH+VA''FV^2!R6\$5)$-''-:.OS[-VGI^.J*C-^: 92+)#/N\I_3^[]NI^M$K?-&8-K65IS^:7W?W-O74/_LC":5!>=[7!<?][<''?+&M7]/!NQ>Y2@=6:''_;7M.^5MTC)E''?"\<8S3S:0[<LFTV49A@OA\PCD:/00#,@S?>F-J=(-(@EJ]_TC5/$@S,5\G$@L@^G1PPN%7BN54_740-Z$ ^]=7"=X@Y9\@.54G''J<W"#Q1L[A%P<IB96AF]4UGT;1&Q[EWQ,1DU@]''L21(E@5WI^EFL%RE\''NEM-)FX11HIW94T8%(H;.DMN%HH6FEUC9XIH9VQEJGL?BF]A6ET-BDT=6FP!:HL(*HPEEC]L&HLEEHUP:DX:-TX-I3<#I%,KID)=AS1><@JA=Q@3DB5UD@?1<@-2T@<@H@BRU''<VI''!>$G<[](F#5P<OE0#X\P3ULW([A!MS$P]X,03= A4BDS?E?=@N3#F@]^I8^QA9"MA9SJA,"GL7&5QL5@]O6T\86PY=4@P,9#Q]/KM^-K]]4GQK)Z!,-DU[=MUM''R!](5"K.PRK.B^K5;VJ- !<.A!<.+!+*Q 1(Y!-<JT:%YM[/J"J18 ''-6I<#$IM*4QM[WQ+>IMEPXA>!7DL1N@A^Z\@\/@K<_\O]''"NA5E05QE9\5M82<@D26DK6BDA2>@G,N@\''1Y9(H\H#;\L@I@IE&TKR1LHD"BO@@BO2"DP%]"O QAFR(QJK5"EP/!@GCQWNPYUST!UXTY#TAQDOVZQJ/"CVG!CG4&DNFZDI@!UN(!ELH!DI^&D]HRRVJZRD#&DZBY6L??Y$UG4$#DYUBKY$"QYQ&&$$3IIX29D$U.''Y $DOV$C\9J2T''Y#"-^QB@>!_)H A7H CS( A3? AWRH#(FGDI\6K'')(R,/AAO#XC3T0A9<6^))GB99PB0B0BR^@A&#YB=!!BH9@A@A0@,0@O7I)T[7@CH( C/=(CHD $HHR!&;CV8,)_MDG"+22S!GC"''UCON:EN]EWWYR9N9[)L<(X&]%T[Y4Y%]I5&^\$&+VWW*T)[LPD&*6$&),9[,W(&+&H&,\#&=0T^<36O;TUO^-FO\!%U@RTA=0HRQE8B=&P]5,I_59 AVA)!0>EBO5W''WN"GL8AO7!9B:SFMFFPGX0G@H+?PF*7\@.J0G!<6P_G,@1<^P3XX@[U(BU2:P6[TH$R>W785VM*-Y= 5(T19TYYE9LQ>Y=A1GH''NZAO)6L*]ZAERJ@J&*@:6IMA.UPKJ*@M&*@TB*EY2HT9Y4HUB)L\B*@FZ*D''"ZD4=JDQF*HER''P$J*D)VFU_5$B[\$E7QR]T5SL&1S2R\)0I@X#YL@*?H@50JH]_FY6SQ$%&,!2EA6+K$U*IM0^ADG&TU0.78@?K,@3PP@Q3(I7XPPR] @"]4CU<$G @4@=LP@93>P^AT@3J@@C>XHF:<4;[%9.*ZG3K)&7FX:_ZMC.Z6T.-E9.ZZH3@ZI,]4:]:^''.E&*^-.J^S?1N(42Q=!F*J#T*K2$]\"S*)_''*Z]F*("8*($ )O!1)<4FQ.4UR*I\]J?DT3J\&#I#DJ2!&N[5"NQ$)?FXX_!W\&G1^P5W%9,;@X1<@D1=@O5 @D''DB%-3A@Z''$K#6BAA7@K_\@D#A HAU@K()@D?U@L"W\I@D\=V+X?>;%3U*!375)SNMZSVUQDR4R.X''VT?BU''KE&D9!)B:J)FKEV.S2R/H7]&DQ&/;W*.;6*/;K)2:U*/:;)5@$./J"]V>Y*/?5*0B>..:&*/C>./DU-%G&*Q[ONA-''U%9AUB3:T\ BUXC[FF$-BF[<"UW%&G,:(>^O )[UJISN@LP\A8Y4HA>O<0B6[PB@@ %8/''!7 I@GV"^X0''#;T@O73I,7H9 F @D<B*GD%PV*;V*[.)&\R(S..&^<B%WEZ[^<]EOH=JWKC''^5V[&5_[-_EE*]HS--@7--5''V%!+N.UF-%1[Z6_[R6'';-%:K-&UK-60;R7N;"''5;N)G*OI3(&?$UQ1JSQ''92''B%1CL(9B.4''A<9)#",+\@<UT\,AV-%(B=UQR(3''\U$@O=^1BV:P@''<0 L+QC7H)DL8A"K_0,2<A@@EX'' B )WO@..ORB1MWI2MV &+T -\3+$!Y%CW(UV[ET3 9X5H5Y0CZX<T+/O6Y/L<[U=F;$]M[QS[%$VU4/VMT/\9KT=C?";3VB;;TJ;;_J5_XN;0<"HO!*5Q+]WL^BT]8\6\\%6^ B")SVYWQ@(_.=0/5$KJU"5&BQ2%AP7"A4@:H0G 7&;*2F085XP52^P30@0C^0@(E\P2I@@BWLHF7LG)DP@ TDC=L0H?=E0 (DB8YQS[Q%%2]&+V1FF3T9<J;BLL/KLN+BX45CK\37B(1+LL7/B$=''K_L!,L3[LN3"X)"B9%CCLP9KLR%D,Q5><QMGLVF4*&AD)0[25^D^2;''> 3\ZE%&<H52(@O1LH:2F,BSU*.K$ U.P GO0@S8$W ]_J-9T@/D4@&3<@_^@@C4T@2I^W''MT@;G@@]=L@\%,H!@Z0-<?0@@8$BU_0@JMW@IU8(O-!@D;N ''SH%5LSU4U#Y#TI^TQ?!%[_SIN]RE''S3JW5[J(Y2==Z)3=A*B(-3J;!YVF;%C;#/KW*Z_GD#JN;^C,W3KY4[K..3I*]3K,L3J0A15)/3KV"]$ZZ[B=QU>W/!Z78\9OB(U@I@M82"$WC&GJ(/FXBI8[[H\,WJ3%(0B4E(C?S@J<6HK"*@]7$B%#0A9#E\M8W$K%?@E,5B^FG\L&<BW@4#A?=B$;7!Q,B@A?9M<,MQ]L71-WL/P2/!-[/.KCR7Q/PWQ''/#B>HWQKW27EA7QRC3QGI5-3C_R-%_R(/#BC"7RJU7QHW7QKP7R5D^J1/>VM;4"&S''ZM5)%PH2KDM;(@^3''_,8I''^A,XPVW!6\2TW$HB1P7B7IIV(ZG@%> C,)PB9,@CV!:BP<G@@.0CN)0BG50@!P@^U(2K/^(B600A::@C-[@MDV#_9%[\/#3X%-426K6.2BJ!B_''X7"]('')M/**,\"YX(#XE+''$-!XA-6G:M6N"+6A/:587]58?M6HT-6T%F6W]-U(,= 9F-6YOM6YW-6Y\M6$")QWH$3_-%UL%CQI=JI2F+!&3(!$O*3TT-RY*E._4B^ 174E>0J@9XD?7@^]#@3#RAG6,#K,/0CB"0 A\H^_A(A"?!GKMP^A?WC>7P\D=II>\4OD+\V;7?^H/D:M7NBH3!#U?_S];][];#KW7%OZ%_2=5H+M:B2-;);]:TB->=I-?/O[W^Y]>Y2(28)=?73]<A;->!:-HHACA2.#/W"C]A^W9)*C6=<MO!ZLY^PK&57U@O=W6[9"_7$7>Z=6EJ"!=.*X")N2:"U-1A 8''OP@D0B3<#-VG=8F&GN9P)>*4&.[3''F)H Z%MW5J=D.YKF6:86M:AB3)?8:)L=+.MI/*D):.MIW.PM^.Q$5NQAK)EHC.QF#.UU+.US3.T79^TL*''T?#%U[[%R''/T+#@)V=N=\"(11(JKK)-79Y.YU]VZPY+#UH&&+U:R[S$3( I%)3KU>1MB>FC#NF?3H4.5MK0GT3J.L1#-;(">82$C;)#:;]%";)]&L3#QO)!>O)''Y;)''2;*([;)%H;))'';)=</)@@O*+T;*+9;*& 8),18)-X83)T;+(2;+.:;+.S:M%4H44>N@SF@KJI@D7Y)@F''$,D\9GBFDF+1(O3Q%?QI7''$*SF&],2FCV/ARQ>/XI@@*)=#=(*]BR))RL,)WJY:X;.UZ3.;\;.8&9K;2;/<_;-<6;/=V;.&$O/9<;/>8;/?P;0?:;/2^[/AR?0A4?0UJ/0Y,#0=*6KBP?1B2?1Y[VTQOLL"( B1)QB9BT''+>4PO *$7D2$763-%MF2#6H9;_QNDWUA''L.QKX%#F_>TPZ6EO1YTU5+&QR?UI3Q//F 6U%S&<13$]TBO%DE?=DZ_=DV?=DS_=DSF=D?/=C</=TL_=UX?=U]_=U"?=U+_=TK?=T O=U0O=$)O=VP/=!TYAJF5M''A"^LY^D2R%PH;"2<[Y;FA1DJ::''N4W*1].<)I4.TFS5G*%;C"$Z!^4T(!?M"/E\43U>E3D\3^89!T/QA9T>P1$>]%8>Y*_>Y3/(Y#/>Y,O>)7?>ZP_>*T?>*Z_>*"?>*K_>*_/>*(O>:3?>+P_>;T?>;Z_>;"?>;K_>9W_YM$HB>NBC>EB^N)]''MA43P]A,"XK!7]^<''9?EPN,ZQL%K1&4S!8''S?H$;GC?@''DP97F042.\O/:F;/60R^"9$/8A(?;,/?;.7?;0??;2G??4O??6W??8_??:''??</??>7?<@$V]VG(F3 -"[!\"VKTSK> VYE]FOOS>3;@VYZGEVQ''.N@DC+EUH$D9GG[&T[ET>AGD2+/O0SFUO&SI(5[];D&UO''S)8=_^*<@>AQP%,C#VY<U!FQQU->''BJB"BK"UJ)U+U;E^+W"Q(%].VXEF5[,VKI%3Y9E&5[-V+Y-7[:EF5]./5%4 >QYM*-(G%!L<"Q=Y!S!PX!IHZ+;^GLT@D&?9B"H=:.^E0D$_5;F''E''3Y,87K_^:IGSVL8F6> %<E&R)TXE3[!F;]P/@?63Z-V7_1)5[=6;^/W7?A!9\>GC"1X4_Q99\>WK&3Y''G+%6+TZBMBCN*S)"]X,_DL26[H^YA#(98\'':9!M%Y?W+6;]\7D>5GXI9>)+O''*V!=3)0Z,)7?A3A@@P\$,D@CC4P00^I$$:4V@E3)ISCB#A+M(H.LP 0$&0HAHA/3)GG,A0@*\:=DD4<4<[OPG/FCMK5N(;CB_/)Y)#=&&O$C#WCP>FOG\G;<$T\ !P2''11>M9CGIGH4DL, %%P22RQ=55EEJJ:?DL$,-->R2R2>?ACMLL\\$,40334P3SSWO+IIJMK;8@<9ZL*FOH8.022";VS3RLJ[O# %OC)W"Z\$KJ?=PQCQQQSNC;1FDR(L1H: P&''D6MB;=H,((*112RC\9/_IRTTF5,$4_R942UR)UYWUUU5.E=UUYX:U5U%-+1_UVWWO%]U]_^0W6U6FCIWYXX8.E]]MS^PP D;(,!J" >U"[I\N[Y&B,"'' "$:N^DS=[ME11E5TQ@F,L(,<45B1,LX#T@K#%"3_QBNYRS5<M4-9NZ4WC$EE[-__STX<%FM&BC3X8XXPWU+!!!!=6NFJF@P;''B0C4 YD"#P:2JK>HDNESI)H,@=RCW5P2C;51U6YY4ZBFH.!EBQE"#RAKM?44W75WUSW ''$WM=U=WA2Z:ZJNOQ#)))Y]&.&&''''8X:Z*''?):Z:Z*./1-+*JWNTU58@%@&BH\H.F&04O2Z45*[EIIECC&%4$NLG2,A-.V:;K2-7JO&(QRR55ZA:Y+Y ^!1Z7!7]IM1M^]5TE%R^G4=UR);W)K1222?GOGOMM>^<<25=U)VYV;0)2K+A%%)JX8QBE%$$L48:F^9BC;6;]--?Z+SE>RIMS\>KY,/13^C3K_JK^-.$%?#OC9=\42[1[U1":RF&_''++*<_>^.6399;VG>ED@889/G&&'';L/K@"0E,=O.2XNT_(E1F:?7X''.6>=WMN>HHDUMHI*3,9R?*FRHH.7,C0J,6J+>PK BA 12PG/ *UC%NP)V4HHW1F@FMY 9?5%]2!A?HDXYR*^Q":0&S1]!W^-:LP+XC^(<K>$ID>0WD''AYY&R^0U<N[?H2(! EH/$!SF(BA0A=UZ0X_8AC@(/DMT31*A!UJ$X1L"T*@^(+Q4T*$+?<-[ )D&6J@ R#A<WX+3FF$X1''MFLZ2;!FMKIQ#V6D81/%:LX1H*6JV\M#G/V8Q3;6,V&]J!0@B.AC=N5/_VMKHT60IP%-\T-.IMJICFW(''T''6()H:1FPLQ[J"4_A/MM"94>>H^DU90ZDX!" FL6XCGW"I 1M?*E\1B/ HNHUC @,LA3NN<P\3/H$YW?"BJY]$.N8U\7/GMFX2$[%LYSYSX$L;7H:ZU\JNC_<FHSS[3)::8:^R''BP%J2%T^#S)E5,00Q[8<IL-QIJGW*"S!.?,AS/MZT%70-NRM&G''O_WYB7S^\I76-ND=[4"R_G(''I@V=)3,?L=A8W*YQF=&]ZZ1#FI+]3D7,NFH UE$C@L0AW+U0DAA8EH1"&JFW.]P%F-!QLSO<8QD?,,X/Q[FL\KPTC\0(HCM5:$2^;-R''OPW*S8''%JQ?A2VJY4I!@1 X-=O''!A].$2](\@1''I3H4''>I!EK9X!$8D,QB[=HD$?4]&K_(QD(RIC*C?1 UB3\-N^><2IO=7:U)FL=YK<%J-O<*Z;E2''U_:DL0,6&"LT$N\!028@FM^Z #FN@#17?;NAE#)980E4.+)]]Y@X\O("F$ 86*I<UJ&!EF5+R#''Y!N6L\F)+%+(!\42B(*89FD"''P$HAGONQIFP19X)%<Y!T[H;ELNW,! S3X#978JDX>Q?XY]MY3''<VUXS<C2(Q>1,R]X05IV_G!7KZFAAD92V(;O5MV9=[3.; S2*O:6-+3PPP!E)U ''MB0''536*JTNX ^N(/ EL8 "D@/<P#G H@)&=KHSZF ,K7]4CG+=@T^&KV6DHS1!BU_8,90*H/"Z![(I>Z<04J(VUF_2/ >E"G8<&PT@/CF[N]S"C^RP E[=D%X@$A\Q%,FGCS%A5.;J$K,1JR-MBM*S,Y;5(CQ$?:._;+)N.?;S$$''FR[''N]Y_:YL2=F9GP[K*X)NJ!@SY?$NIKG0D@H*CACHEP@2 JPFYE0HDY1 B@NJ[H!3:P01*;9J  ZG&L%"*Q0''<.KZ@E[VEBT2>53^+]T#IRDM"&[+[;KI& HCN;''/B"E77 94JX @Y*>LN;;0027_))B6V0,;"=BF=H,I-U;I:W."GY:$3T&P]4''%+V%\R.^GD<57[*D53BKV 9Y; SG,YLWPYIRFD2@%<]&VEG/>Q(NNB@!&$<@!@@6LPWQMDI[*3RPX58AC.@=8]#0@\PY!93F^J41BPN6-4>#]:;9U5(^!_QY4Y-E%IZYBEBQ"/D_UI$-/>69Q!/PYH''@L#'']'',Q#$7P)]^(M",;/4,7@D2WI@)U96_NR5]X=M(2PWZ2ON]Z:;*N!I9GY*,;?VCI,CYT5Y(LBR_7M*6>_Z1B RW"%J!DT-U25DF+AH@9)J H@OSAG<2@!",@,@<4M@H@-O#GG0C HFZ @P!3DLXS[=QKV][[:<9$7*:>O/Y9W1!94UP-T >2%ZV@4''P[^[P>R7Z2%[1P''OV+0WZ=Z$$@!@P@(/B@L'')QX;=7X!LOJ ]I_'':K6+I\EF,V12OP&VI+.FH?#8!I]_?.(D.HP"R6@HH+WGDKS0S9DX__AB;:S)IGMLH[QOC<O6VX!7H(@0BM,HX:[WG?"4QP@3Z2"D''+W1?;RP:[I+%3DW4JL)DIKPV>&U)\''F2#]FV((T#?:LL- MCXK?B!E*9 !"8@8HX?4FHN5]C>G48@@FN<BT:)IG/<.U]D_)U]?/\7)/H*%*OU*'']]YEN6)G * I.I-V&[-8$[*=*I+N*;T3N''O5@FFQ*?L;NE5VLB@K DL> EMV!@I*"EVY"=^>JGV(BCW&BFA_"C\+*EZ$BCW+BF/*LI^FFFOABEL(,''O$BGX" ''WT@GI!@EZ(@@G7PM$" F[;BFPB#BO2B+U0.ID_P<_/@FY"@I@G@D]. ETXAA9OJFQ4AB]\.4GCN>_^H$(3"6/<"H_\N9(W&S?0K*#7@P!W?H!CHXAC!XA%G8@:VS%4^P)Z$3 490ASG8@7R8AS @ FX8!$4XQLH1''G#C/4[D%;N;E9>A''&U11D*4O8JA)(BY)!?BI(1PGX98-L=8''V2HGYY0B]*I*;DB@NERI6XP!7LYOI  BRJPI0C8@,/(N7W*N0$ B[B2AV80!2]+OP$T!W[ .>]*)8<PK''L  58H@&* #''"2ARK(AT@H1$C(!T9X1U; !%Z8)4R(!.8J"PJ0!''%J!F[0N2UD-S''PJ&9L!F=,1,/ HXV %*5@#_1 -!7IDPJZCPEX!&H0A7!Q@7Z8EBKH.VM !!]\@CM8AFT  $\(@6((!$Y0!_=NHHIM6BW%.TQK1AV\ RB4X1W&X\RMIL&]T"Z"4!E<20Q=(2 PL81?.8''7FXW8H[ S08&",BRO&(4U(8X?*DVW <GA$;D<:K,$0@XX/KE^VK%Z2HM2X( >TJ][NB],.DBSD0$"L@NRV@X&6@PPYHIYB@]+H@_[,0T1\K%@ IAY<@XJDJ=YH@J/&+0<0L"QBXQZ"B]*#B\OE@%/0LY2&(TE2KT(62SQ\IE#<9/*HH1<MJ)K:R 8\K@?4@V%.2$4N@QWN@D^BXQD*HT-CH]A.@WD4@UCH@\@NHE[B@MUHRZNKL$N4#$B&)1Y$RCU%D60R2[TP#-D@:UEH:G<&H#Y<"_?E"IEE5JYUAP.)H2I QAJ+U2=OE"@9PH@>Q"<">.8^E+EIF ''V6AG_K E$."8NW@''V1,<[BPIQL#E\/@FY_@D\4"10TNA$P&,,)(:L*NM$DAN/(L.^>"EV(AO<!*<!Z!@#3KH6VB9LI2I!9*9J-NH&X$H">*W?^,Z^ID"NOBZV/@D? HE@N@FMQ@EOG@PQSBCK7"DA["EP;AC[^P(])AP(!''I6X0?SQDZ_-EHE%7M;YGD:\D0S-F0SE"M^02L=+(P@QP1&U"$Q (QA[RIR&+JN_#J^3*FH(OA,/JF!&IG ,@FM*@!@LBGVMO@W*BSI5-OO0!J(QRYB"P+V6B''JFTB?6+(!HO"$E''X!A1#.X*3AS'';/G#B1+3$T(DHBS?@R<[#)33@2598 6J(4:X<)2USH]@PB+82SC0A0G24E5E@ ="H$3<HAEG0A8=R@3R0!$2X#T0  !(PA6 8(''C(-$YXGF*HL5%Z1I7Q.Q&EUZBIM="T5T_4''(&A5_-;G&X:-D1HCXQPMLG0FDTK1V9BBYPII;4JR%*#S9J8!UL#T;>+MU-0AQCL32LSBWS @:+,AW=X@WY*3+8<AOE:I2Z8!WKHI5)PA;;KJ''UJABUM39@8!#DKBWR00$CH 2?@-E;@1%LCQ*U$@#8HQ:I,Q5.(@XW(AW/%573E-N6R)I5(EOT:-/<]9[\T2;%UXUVD''@5QVJI0N@YO@@AODHT#\(P>&K)(6C<]LPYWXDDPIPM/T@P0J9T26)D;<*N[1]&(@\$FB+,Z[YMLFY9W#QV_CQZ_3TR]P=*#M]*$Y]*%SZ6&Y\6''%]*(AT&A>T#,.]F"$+9LPD23BP0@MH+@ZI>(X(2)"(3ILC#OTJ]><L8%&<IOX<^-<-N7''\H(< /1, Y%>HI@ L!2+L)^X4],=S-/<K1N2HS$R-!AJ@X? @A8:XU T@Y6&@T3HKN/WHX9^L^IK@]_Z<UL$C04THX5+\)I ,D<BHPZ<D[M)Z.Z$CI4*[K48X#D/M#CRUE6PLI9P\ _$ZU_"!O?R]T6@P.0R4$%^"$FA L3H%GQ''L''UR=RQJ,(WACJUUID/8&&^BEHVW(&\:<7^59P ;,V0; W_;1U_PBI_;_W^<-7^<DU_2[$^6=QDGT4J;SBHL3P]O8";;0 T7KJ;>,&''/),8!Y,<X&"(B;RL;\2*OAA FPHBH/BF@6Z''_& E\Z"EN>"D$P''\.5 =_#*2V< G$4TG]^2E#=6OZ#@?]R)BU?@FV$AJZ; 6YPBB!JH!$" G[+ EU6"E#I,CT6LBU3 5QFC!+7%!@*6IX/L$#%$;#UAL+%$ VT+ITX$>&3("X,*TI1)^GX$S^SFD1^$WK[[Z''OW"K5XZ$S0RKS:R^3$\?:;!6R;.VZE94P:B(NQ%83^VX0^ZX:/-6S,^FC2NX3+&X3/N83?^82:>6%,EE/7KTX#0H\EPM*V2!;D],P8),["1R\D<.U8+*>@B3;2\IKBBNIG8+.<4.UL+*:X\,!#,-^JZNCM=)>?J :S4"4ZXIM4KSHFZIK,JPR^;.GZZ9[ ZSK6YE(F@".2X$M&@EY2"$!;A)R4I!"<QD&Q&(CN.%06RY,9A+Q99,@@C''1.]E6Y8H*@%ERAA9(8%''GF.V!=AY""Y9''R&HI3: %?Z$]R$7*AIV"+Y1F1Z"/!M'']XJ &JU54@YED*;**[$Q_.)IO?L-VE[K"D^4K\2U7L:ZG\:8O=86(QNXD-^Z@Q?6C/N^E!DYYQESS8X4X"?( !"_$P341DQ\C<.5%$''7*J.4VH,A.NX%&&$D\&.$[Z*88W*/QP\BYHO1VX)6*> A"X)@*Z"A*X&^&&#G.(@@6*&[.((^&*!!.*_%.**#.*+)&*,A&*+3.*.7&*-]&*/C../=&(*1)R*R4%$PT$G3[^2X^Q>H9.8D8$!G[#9R].8D"9I4&,IDCXZ8..ETB9.$"E^UKJD-"E<"KFX"KTDW":AT&WKJ@]1D@M,^4^<^- (<2]D5V.N9.VXD042!IGT.H8K,QQ:DQWC,^X5#BX,0!I;BV_VK.LE0"UX&.7ANVM51.45XQK?K@H3WE*\](,/%.JEI1I''Y58"6R9.Y=9-JSE.9Y9-6W[.:C;.94Y.:IY.:9;.:8[.;NY.;O[.;_;.: [/<Q[/AZ(:X08G@]MG33HV=:U''W54;51*!/=D&@/R''.TNY?X4!F=),/R9,:)*.\-5(6 J.0S9($P@9.[)[@''>2^AH9!R903>C,'')#G/S JZ8(-$-:Z")&FV''+U+#$ZK&*" \G"(7Z?N)+)EE]1I0X&@.L,I#XUS#&"O4CQ8I&W[.:ZZW SG_<BGY\"G>?1K#9*JY*W(3Y2%3;1H4=2IE_2I&_2I5?2JG]2JX_2J[_2J,_2D9^%21JU$<HTR/RU+O4^?$L*?21++6Q#*,LH4) 0PK^A&4^B<A#N*1"V\8I&.S*?J;+);2N[)8OJ<<=(R 7.!X5#)1?#9XVJ<L7P''<HL[^0@6=KVNS"YM )0[Q09''.U.D"SQQ3!FKRF18-0F]\()GA5!A0>W5\O)AE7P!S7P!QL8@WUP!F!PAEBP]U)WAC^P]U2?=U3'']U%W!E<G=!N =V"0=VJ/=VN_]VP7=&Q''=&U7]&VG=&ZO=&^W=&*''=&._=&27=&  =%"OA#_P!S@@)-8^7*J]Y6''J!K/XS[LAU/["C K\)5D,QXB.G7N*(S7/;4/:<76W<9/H):6:[BESMW\R+ F]<K**)N!J^C -/,404O>H( ,LA2@@DM)L R%*\HT>2@SQ28QL<@Y/<O"PA?&OC?&N??"SQ7&OE;6OQ>FQM7$TE/&X]?&Y%?&Z)?&[-?&\1?&]5?&^9?&_5?&V!?$>&AM/6HQ2 N\ @ZY<\ABN !_?TIB(%?*)185[<J Z4J3=L8N*V=EI/K^-UP)?J8"1T[XA#L$N&T''9J[ 8W6"7EQ''>M)G&Z''A[5/_ ^#$&YLK:LR#76B-FQ05OACEHO>X(^PQVD)@Z\G(@RG2()?+F''7+YBEAV@CA;.1QN"CL>8HQA8HQ<8O3N;?1A2@_PE?5<2O3R573N90OOG8P@H@]N2O0@@G7O%?7Y)?7Z-?7[1?7?7M_=7^_=7*]=6L]<NKMEX!+_LC>;=;VG)G@-(:A/X%5320AN.(.L8XP9.J*]F[I3P\<$''H!X4AXH_\,&$+[108FSE1PGL3"FYU FB#"FTP"DX7#?>H]?=^\3$8I?>@>D?L=?>B>)_(@EL0BHX(E&&QGX;9"YX8DTK 3T<BGC"@8%P)1(,RIF"!(/[,3H<ZOG$A4YS#PSZE$?Y(A.&R.FI%28%2=!?,%WJ1(SI+54=.+)<2]P''S2G=!Q*"0&>''D*AL&7*=B''T*EJ''T*5*]V(^''_"^.PK0I=0WY,WD (5)U.[Y,3M#+/72A06@SDE&3\+#I8"=.W''35)753L<K@MB ?<8@HJ%J/G"?9O30H(C''5\"R+]+:B_$)/*H?L5>^;E$*9D,@+L4J$*^_+W99Y.W5X:<4W0B74*HEZ>6V-:H9_V[.5W.''Y.B]"4+(AZ-2+<*+^>Z)OO03=N#R)4OU2P6@.VB437HBDL6''TJX<#>[YNP,_\''3=^)V7U_$79N[O*]N/[7<2M @@4O@727X;V GRI)ML\RFBE56/;SVKZ:01ZH<# (''''43F7YCMJO@+H \$*W/13G8 !"!#"GP@<T-]**Z6FHE9>NJ#_VW>81]=[#>@F7&9E)U^^[37Y4)1SR@FGGBIE+P_\"D$*RY=56K4EXC#9@@@J$,2A)1-2&1WGF?=2PN*D7B33KS$&&UXI]P,@LQ''BG1)_PB$  SC==9I[W<S56(E<+][W"''2)H>ES(1"6&@JJ5^NX&FT*."!4(Y''H8F*Q,-["W''O)M9N@--6845H=A$WT!LS5U"Q;.%''IJJ*)I-^KZNX<BV\8 41YYX>PX\MS^_#L8%M9N/D8UF^I*#*,"I\1\R&\?\FZKJY10)UIG ":>E(P"C#XXE8QC&XY$&XP88D\N,P#12<]_$ ,..%J5X2I_#0S*V(I0)Z G3CB>-Y-.WEJ*5IG@X]HG!I$2]Q/:RVW5T;>*+/0_Y%])0YL20X0)[D=$R+T%4!&.R-2NYVWA8=5=XIH\\H2_OK?UC''&"BY[[\;99''XQ-?UVWGR92>^$_^FE%;YPAPI@M.QJ,= O@C2FL-J(N'')"W2+:ETRCN<=2Z\Q) ]TI@FK FN*>0_5&FQO+P\YD\RA7''CSZ$.$$F,SJ4-[]]=326,,,;G!R213M,AJH[6^W2"/^2WU59LA)F5;][#0!N>B2@@[([E3Q6.NZ"77MX.7$+O''Q\5@T_"-G!/E0:HTU!9.>YH$''H&HK3 6.M/%+=,ZLQ"^7%NGT57&(8P Q@E1"C-<< .5SO;_\T)9>Q?)&<.&F+>5$8;J:\VWW/QPSCPA35J@_@I-,J31;&]&R&W;I:\^D\<6+G6QOKK/L''2D0I2.''?=-57)&3)G3=-_<,_6;+UFDN$9#E5LM(3E,_@*/#D=F\:E63\M(3(DZ-6N2''Z(?CV!>^@+I^_DD]MY#CKX"1@@@P(QMM6\6.^#DG@B@O@NZ!W KW13XK/ 4@=0 R>!PA@E\$0!;,LXY>?,@^TBF,E;K)R_&*E4LD9&\?#6,\EFT6);"8Z5(N$.C%G,P 3''V.E<_85"<2QB93K[FL$6GW"_0@+=QD;SVZ$56&Z@^@CI9PI2"8A B<@XS5&@DH\: EGC*E/"35H7,:JQ>.OFUF49UHG*?Z#)R&%2OK,BNO T@_S0PA@F;,!#2+V%W9<FD<<B5R_\]JD9/"A3?9*XU>L?=;5..$E,.>;@<0 GJJ(BP!A3%HP0^LPUP) 7''BA[[K QADTE8NMKTJC" &VMMZE7]"#S0.P6GE<@\@&-DSVO"(E7;@6@ O:TI!2!@;E''1\]6;X%OK\#QO-8EQ%@ DDT[ ''N\XPQ242\X6]OBM;RA0''NY-72&XQJH*OZ:V2;-P"K_8%"0:E4B5!Z@XK!]FW(2-]PCO*D3R>I$W14!2BW(QJ6+ IC]L,0C@+ 35VL@\920ABJ73B"S;(!1V*P^H-?*''Q4;GKG@QRYT&#AHC)[P487*!ELY"@@+(A!SO"@D@-=CLGZ/R$]9T974;S]"X''%-Q%+L3T2>2WBQ_]+D=>4Y''?''2KJ%I=]:A]C$4LA#9[UQS*JMJYAS[38X"6)TZ6Y\(R&7G+"CP@\85<V,5 ^M+EBU?S.ES;Z8T;PME_C-\*U:KP!=\J#''2I-<CQ^7D$^NNDMC#BCBW%(A@CJ4TR]S!Y-S[PZ&02*%&YMLQLQ1ENBIN "3MVE"9+!2R (F+)2^Z"50$0]"!8XK0S=A[]0OD-IS3+L''HAPY@++"Q=:0X][=NFRZ@ADK_11UPBH4;!IX9\<''KUJMDSRJR5\Q$?P<;F?OYTBB_/CGK!!"2Q"5[0''X0JZX!-T-<%V"#BYDQT''VB4V:^<.DO5_T0JHFLW<T*;>W]?RHHV35"!8&Y!B:CMC?9TS/F5P[+Z A&EWUX1,; RD8;702R)+E#^5C@7)CN1Q%CDGL2AL^O5 0*:"B%T4(Z$V^^!J^VF<,HD"UK[K )1-*2V/^^FV\+8M#!\-!BDM#^9\R$8 F-4E+=V4TX/O%T%4@XCRB^$$D8SEQ7/Z@==^=H9;7L/)_.] O(A>&U =_U1WX3V+K#K!G C 1LGXX8,?7JHY^E&!$N^0P&HX\Y3=;WN*-**&? 38*6.)''80RWA\GL7 .?OL_UGIIJDLAD=NG27CS8!UA3OFU&P^UH169YP-DL@DT@F@)6_(&B"K\8Q"2*SL@Z$BDV+284$WA!*-QIVL6?V]M\NMV#-"AF?>>+X(I'',ANY)PA F("JP"="B)+(95)YAG4*;NUX$H30Q\ESX)2C07B%XO#KWBIZ83EU__)$K,:''E71MP4:\43R/N[!;\QF7%!FS,3=CE4@8@F=PCDZZ,TYV:30JIIEW5H@W"[4M*.+?N$N%^RF/%848![" DX <''@L5JJ#''**(AS^J,X1@:FHN:D@D_<$+\"TMA<B(9D=WZ^1$U<(I0WJY7E3L> 0>NS!B"_@Y4HQFM@LNGV$YM*Z,,U#+C3,K 4:)SFWP$\=2%D\T(%EGY\K #T4T(1\6+ T <BDA6VQ%!U >XM\IOYU)04)J\PLOY7SRC=F(VZ+*>IT-ZM(;RF_?''K=:+"_ @:<[W:&<JT4$LK0=7^RXJAR7$;IV 12T!71C9(/ B-5EMX<6MF+8MF3,*NY"M=H@JU0<''K_;R''[HOTW4X37=<DYW(J*_.)<R"SV &4;(<%.%TK?:5+<>=+N/?^53/?/^-;90*A=X*O191 %''"9QR7+'']KDLP E"!H/:0*?K0+Q=@/@TQGOFEP1(9C;V8AS]M$.2)SW! &WB8S=J9V66D''%"!7-QI",8(46+$64=H6@CAE]\MXK(,#Y )56)DTA-Y"*6A6A1A!PRT!1''$07W4 RO<PQ8$0Z)$ R;40Q18 2_0CS]AUU5DUW!0! YNAW&@2%LPG+W51<P\?=;&%T+:&IY)''P+8R@A:LATLH,&QYM8@I!:(O@^S4Q[(PUE?3D&A2LWT(]8DXX.CHLJUEZC+!YFFDI^W?ZB:\ER,;\$D&]''.PU]LRM\0ET''4LP\_P--:I@U''#T0/&M/_$@(/EB@\5 _I0Q,Z*EK:QUMQ0A^PCH&?9@F\JP0S<L6._ANP<\#Y!L]<_A<)%*H)''*H)_-IN[@''YXH18AA"[&I0B_-(+7T=YV\9_2EK?*AUP9MH.=UJEV^D"7$]]%X[-9YVTUX*G4YB2&A5T9@F/JU)%!H%1=LI:)H=22E\5I ^4?\40U \6=@X)M\V4!\NZ5I#!LXWH=@J7=P#+PHX_HO<GJ5*#+?QJS63IM2+PTO2H$B!QTUBM^"& W1VHFI"F9!S\-N0VW[B^S41TM%ST!''PHQ.W#,B@W2G# /B2T; FHJ.%!T9@[F*"AF)!CH''RBNTACH)B$F"SBO@GAC''PBKZA$R#:BOL@$G* AMIPCKY@$DBRBRO8$T@Z%T@8%TQZ%TQ8%T"Z%T!J%NP0%LJ1$NJ1J8PAE>\VVJ,UDICYE[:@ALXBP<RP[=; B&)AP=.#G?0G@BHF%''Z4%V;Z%V;8%WLZ%WL8%W][%WH9%=+@T+VB[>:0I''P#$E8YU*H6U .5EES7 *@TF!J4U4H2BV1EPA%J$(&RXF.FLL.4UAYG]''C#?85LX&7:L9R5D6 *!R_IA6%+VP 4P@6#")U66)&.>9&.2)%5>9V\ZC59203"R(3&A8U<^HUS4 1)D%YA1312TY%$*G9*,DE''JI&06)7L>I7SF9U''>DPB\@J$X(D=<7!].9212XQ!26DD6%LF94XLMT2<D54LNE1%I)**D&PN]A!76!XKT216"VQ:*FP81 [F9 "^@ #)D 3(8PCR@ "^( 1,8 #)T 3(8 "MD02.@0".(@2!T08QN:B,(P#UT00$(*C)4*H]>:@.(P8!>JH&V*H&^JH*&*H*.*H\: ''>2JH$"ZH&BP#QD"B-4D?$APG+I#B*1RUX>!S?\ "L\P4:(?8Y>K L,9DD59\D%KTLV5DT G@UJ,@;.;T$VFQ1]YF&V.,/NUL*;_B''H F&X9 FX/ .Y#&&Y ,2Y)*&X&.&Z/"&\&&&YA$DDQ\,3QAB^H,KTZZ$[*TXPYHD?5HH")H3;HA5[JI6;FQ!X4L3S(YU^QA5,3@V*OXTET%!#VA!;# &,^]PG"%5&L*ML\JYSFI,7NL"VY*$-QF$^(@A^%TZ< L02ZF$?3LJ,5"("$MV,L$DV(HARMX^VU :C4H.0Z"&1E*.1G".2I*.1.)F)M*,[)]:0%$ZV= LZ7HH2;JU.&(L#\(]7QDT''3LG#I\D2KDM3\IMG;QT"*HY*,L9**HZKOO=CO4B*3_RE.*XF%VYQ%0Z+/,;+*^(+,AI+9_0+V_D+,O:+/?9*/1I,%*I^E%VNZZ#)0=I*G- CKWP+ZB3N+PD&FIJ^/B4T+^W^%\8BP?YD!U2HFGUY)*ZJ0KE+W9SY0]V''&-3''0(5*G$U]-I2)6\BBG2SA#6CC@3XGB-!CU#QG\; GO&BC^2S-#=!BOLHYW)G+&78)''T8-''-J)'']8,''&J-5V;-.7C-5W(-6GZ-6G9-5YZ-ZUC-5IJ)ZX )W/QCN@C@-Y)LUYYEVS1"^34ED: A@J!CDK@,+PKYP_SBZ:CF+-QE<\4*0 (-/ +-L=!BMZGH''%"Q0>JVOU KH.BI ,C?#.Z:!.U.+#5(K&- ;.VH;.U2K&- S.^N;.&*;.V*K+492NCR*.2>:"1TKB $"*Z=32LBI"7N6FAZ25''%*=S$(#5LJ"8M"!0T2"<\B*Z&[II<GZ\>CZ5=*%>I*''"PZ%8TW1Y([?DU%"4\@9HN1H($:R3,GIK6@4+@0+#6 ?*6;;#NP)SB+94^K/7V+?7V;?[R[?;^K??6+??>K??.;?7RJ +H;*;&@U0@U%PPXQF"0\R(45M@09S4 34\1Z3."W;6@ V(1#H @$(02H?0"Y$22F7]U!A,X/FU[59)4_SN!P-K:0/GL@3O,@3WL@7_,@7''L@9G*#I&J[MNWQC4P9C"?<SEN-E,]^F[[B^T3]*=NQS0Y(-ZE^B>!\.8.JG3L +-U^X1E!1]V@/B/X=G])F1#]V;JJ7R''$P/IHDD@M%QC@P%C-K8.H_Y&OG8F@_S-$N<4KD^;3D_=;D_?3D A;HYE>7R?$!96D$&TDT# *D*2X+:NTT95D@4)K@-YDDP<F0<.$\E.0]]8@OD0PI]''HYA;!*J&@[0Q"%;TFL (N5^6LT3:B$,/;H,@03@BJ7P1''H-S2043#H4=+H-;3H,:?H/?3H./;H0G?L-P>ONG@#@RL:B@T2-4,H-HBG^E.+.H''DTJZ+SA^,GN- -K%!9O U[ZQ5#QNXU#0 G %6''!.@2U/=/BT8W*TYPO;C*OM\3Z*S*L P1$<9BN8RIF:O@=,(.WWB/P@?4#6T)*0I4P.=*O3B4P;L*P2=4P,=3QEN4QE,4Q"-4Q$\4Q5?4P3\4OTN4RHN4?\HB+49CWF@+T9S#N\ZV[4JEAHNB*8[2ZR!DK3CCW@0D73#.L)2 PQN+-U")Z$QJHC"CZ\BN9P[+,?K+/"H,T3^50S(5UI>*0P8,0%+5+9H''/\@NO;-KO=".2V!''2825$=5ZFIK)-[3F''#:19(2,E74N;D''$NR.JGDY/ 3SW27J$3DXE*^JEHZ/)QSY)F8]IO5A@^QA)W]!BRC&TF%&-)A2DZPFL&0H6YU^6YU??M&Y''-&Y/=&UO]&W?=]JBCE3$!-1"Q4E]<4/\[YAH,BO8:%8-P0WS**S]&U$BPC>,\+2(*9T:%@^" L;E;>2ZJ.VL)1[YF1%*4TL!M1$*YHN$G%.''7''!Z4TF2]U)KK&LC;H)<-W_ [&R!T(=6V(D18FC.%*'']1^U4JZPZ[8PY1(QAY/ONMSDR$54]X221\5?]&/TFQS3;PZ4F]:T%''9B90!0< 76!K93-J^KJY;@"P#D<P''H@]7C7-8QGNHUO.HUWNHY_.HY''NH\#+(Q[<-,&<.C-)+K4!;U!E%P$ )CFM+2DBQL, 7I>IU8ZL#^!+>L2 ^%%S''L,@3U4569''MY?J?0-4S9$[@^=1I;^)Z+TSJ3]2Q7^R]7FSJ;WM6LLCT&71VT/EC&(Q19[FE-R6O(/"W.''M)KVCTBAO+JDXVSE<#8%EQ">CV]DW=19T$F((>4V%6DL _MBPJY=,WN8=4>(<(4 RNGO,Q LQ F?XWGB^;D>^(O^]_6@+63%^PO*$5<U^;D>$S21^XG*%Z3*U.<.]N?(2(;\+]:BX3TU]UF..Q\T"+9^OC%(D;>5@+8Y<6WP8G@TE:D\V=LH2<N2,E-[N2Y2.EN1-4^+6R@@-723H\J''Z6.''Y5.&3'':63&:637624V;/YP''/US''/Z]/.6T>64N1#H3(T-TB2,U8_;''M=:"_>^M#L***=HW4! )N;"X/Z"X_2"K56*LJ:9&QASF$%JKLGN6HF*L;63R(=1Q8G :]D#D>S:@)!A$Y!A\-@E76ACHLBBZYUF%M86^13C_)GWZ AY%/XI5O@O^#-*Y^9L2+\FDKM<)M:L2+]<2[><3G>327< C</K''%1$WPP5WJ0:UAC^2="X1S:E7(IB/N0,WX +7Z01<-0B:<03_LEWNOSCB_J:2J1GG/AB7?IM<#".11\IX$-K$QN9,II''P!:$$*M=$*]=9+@=\S>T]K<=$=N=%E&:9&R92P1T"YN5:@T$&B]#F9&!9[*5P4I$;OF;$+#''&L%KRD7NE7\$_''KM383?EXKPV SEZ/''6D0B\,@(W76$8 ?_6 !%PH1NX PS@;989K/)Z.$H>^^(M.^1+-WMO6YES=>7[O)OC[''E3=W''OF;QFR2#C1^5L1]3RVD$Y &(;!PR+P;0>XC?D8:16<DRP$A& Q)FD022@0"X\P"YD 0+;V"I8OCX4?26,4@(UK$)L7Z2F./.K>/,''D?3O_: 7^/3S/:"7#''BSFU;(/S;6ITB PQO.R;"AA04&QH 0WDNAB[?<>PH $1=;,8KL^.[''6R2M_(KX2;#1APA(/UB&UA$HPKYQ/:S=$%LO @@&J''G&5K&SY4>_O8DFES*T*L>[JB<A^NP1321[?Y(>B6HQ?:OHVP@@CEQ(<F@''@G5RG,U9#JH=O;LP^Y2U95''S_!#;@["UA1YJV<U:AUF4"T"-D6Y&NPK0XE&^_.X@CI C(H[\W''''26A*9-&(PQBD1"=R\&_M&395A_1X]F#QG4143''=W,4[KG-%CQ@A@SMJ$9!0I1\04@8I9OMPA@+T5+Z9X=V?YPYBG^"<$-@KM:3U+F)A<51(-/\W/<Y1N@X$2X@U@V=2-VI)@M''77VS?T,*&#]&24^/?7;-OO!V:0/?:I=#1_3:4?@?5P;J26*DK1(PIG.Z0(>V APQB2^,LFJ(X\07F(!!Q;"2,OXL)GJKO9BJ+B8,0P;RZV[#!+%%&1>"_=G W!>V\VK_8+JT\\]^^3Q1:G.T@(-8))*"**&Q+0J@@4MJ  MZ0@(PB^1VL)DH;T.29H:V:"SZ9Y>#"'',OGF0@*@V@C211P3+7/+BEVVV@V@1@H 0S@H39D-+0O$R)H=O?.K3:K9A?1OTTO#0H1QQ@AWU,5B*F*RLL"S=\CBOZR J*!$@9MDJP3PLLV P8G2BA+!>N''J*''2@^L:3HV\9<:) <;NDC D[PBJPS]@CHI9A^(BF"&VM8_^RKW![+1YY@@+D%,&W02\"*)#B;,%)*):5,KYB,P"7[Z;WM =/I0KU,)GF-?_Z2#9#:;=T<HH1&0)6Z20(7I3O\TE>-OF3?JBHP>>,H6=L&''TT]$7*Z@PAI*("GQ#%><LJ&G2&.6NJK^1P+*ZWZT#V**R:*5$JGNCSHJ;AP&!\%,#II:5W<>''$JE%/L,2TIO7()1) 9@AE%&S>R<,^OS- (H@=@@B ''$J-.\WZYPOKH:=.)L<J6:''B1K)]*2+R>.&..M3LM6?%ZJ2:#-PRNC^V_Z"OY$HM0624Z74:53@?"QN*''E:#<NHZ%V&Z5QSI/@O #,%;BP[LWL?KP9YYF[)E7FRV9!F:VY(,+CK:K=MRO<?$>U9@*AT=L4-GPR0^==MEGS574IFEOWUDG3XIP0)>X0H+I_G_/?UNCY@.B4!MSAWR-EGLR?>.XE4^QTP9L[LPQ8>&)+=9:%CY]*-J5X&X/Y@CM4%7_TI>L\,*T*)246+\<X!<+:X9!(%%.@O"B2C=..VNY_!3)(00@S-BKW27FM[58V.-"%4@DK#A4CM2\@3-GN-N9#$>[J9QYL%LU30T!BKCIEE@6YP:8BXQ<@<$G@M1P*+(I;B6H>MW$&OBLW&AE[4=Y!".HXJX2''X$BV\ Y&YQ!">''L,A[=VAU2$-@OI& N[E;[&-VZFDT(S#E+S:2"D97XK/\4YU68"ICJU,P\90 DW4;2''X[8%ZFHSJP"7:/V.#ZRF^S1YAPK$<&L_%DO"XG1^''7481=1(#F%\JP)S?&X /?L)RQO%P0MI0.KS% &0YB4S8A\X $QQ.DGI  1C84)491\4X.>/\DU&_#BI)_!GLG585^(* 1GW.&-*\ R%)")Y[]"*R5Z2#I],=Q%KF<IK/%,IFR)()US5DX[@M#&P0=9F2]8P3]PPFT8Z2FNKR#PC9G$8QX5$M<1%.T\P)#)DNHTG''WDTXMM@NX[.PO@?&[!CK7EC@WDTQS8@!T P?W)OW,:DJLB5Z\?;QN ?\3''O0>%G27V;(.8F9&''E*&/7WW(]0CK"JL(@: C3SFPJCDCLS0 !7#(P@85.!D TY)R%_X"^1\)IEPHML''9"D=CS(JR%IJGO(*@AD,2M]JW9%C? :W-CQ=1NYM\9NR\OI"!E<(@0BY,:@^,*N$<VC+TO07$)4XQUJ,AEUAV>Y$''+7Z5S?_9''DV*YQW9,JVC^PBQ)# E4QHJ9HR TJD:+*R*K2F"JT1XB5''&,@3&?H)L79%ELY;2%BSLP!F7^@L@#LDD[A1#C+\H /1D.SR7UHYA)A''MY34[V-BNU+PY''JP=W/TL6?E1Q\;!B!+JRCJI[. 6CRGHG4AT(#"*%SMWN)"J],HRFL$!I#N)BV-W&%3%D$VP5,BHXP1)M/"HKB.2?Y@#T0YI") M% "2QU7^(-R(AVHY-+BNFY!0#K4- ;6=DD0#9.@H%L2",%?ZF0( <9FM?>2WL KS[7=OD6C>)%[@A],/0P!<XO=FIY " ZVC4+HQ,0E($&;="#JYVUFCPGM.O_%M\E+3,/& Q48Y2PJXS)@IH*@!BE<0Q"6(H[#_\BLH#SAIYKCBCL"T-2OS2TLV3D* QAU*TX_"S7.J''FR3& " P1:2_9;\''1L-5@>+?T&E;GV!6Y8Q[!<*22QGU;X#98F##50YRDT:H>!90P+K]_N[^QR$)^P7.)L$I''1(.!C27_Q<JJ''RZ,0"X]P:)[2.@DCN#M"KX> "D9/P\SE4 RYDVFLN3R#F)*2AE;''XK41;<=MWE;T_I.]3S:T&=Z$M&&)<P,)PBL4V[8.S!;[F)/<L\AT!QO1%DDN\LHT=LYT:XE*0>1 JJ8ZY93K:$LL9@X@Y-$CAGOQQCN$<=P,(DD]PWPDK07R0GXT9U;U6"4T):!I]UO1Z.L_=MWL#,#=\CD&<$L,"58K*W%1F(67UJIF2.H^W''8E%&WN"LHX9SBX?NB:\DY90^*U$X5,,&HO]*D#+X ";3ME.F])UH,(,DQX(HD=4!J"7X/RAS,:Q#R"N(P0"?HDIW9!CI/HPB@=8(1Y32LLRM?LQ\FM$W^/6)R57&T. B=7APQ=:[?$K,''ZEKB7BX4),Y NT-+46M0KYSV<<CA16/Z-=Z#&SXJEEFT]\.1[IPLO"(#FG_C3EDH''?HHX''&IBBGF;"KTEH0#KN<)PQ,^95E$S]7?''.]<F_K/B''F;3*CH?Q==B.2 45"(W6-^WH51YCEGE:V/PDY''0"@.@,N''MH''V]R:R&\=JWG7"@;9*)@ZU9I.;N)>WK*92=ONUVHNH[^AN JX.@EG<UA@2-_W@-W=J0_''"CF6+?DB&J @1-@2LQS!^ TU)LN<:*KW^H-VO7ONT+;<NE>=,G??P_NI?N+.Z" )@HUSFTB+)6"OJ!DQR)_''<)R$$J+UOK -?UN;$/=VHXYX"Z=4$/QDLEYCB$/INAK/(PIN" PIFC[^"&J4F4B287\0@5]4@6JJO@BN[A[.LT46(UZ/N!6?81"7,#(P>2MH_+%-$C$O/*--9*"IHAKI>)HD.:HQ/Q(X$1/A9]K$IZBL*IKJ,(/H4[&7!+)0+H+I5!&0$)DL9P%DH*!GR"@*P:+GU %/[ $C:X#B@QG\A3$FYXAF6[&J[K U6JN%70.&MKM"-S-"-Z0Z!+,Z*!B84Q$KR;BE)II:)[I[TXHC^QF&*A"OZKK[OJ@+9;!OO)A@-9"\VKFE 1("[PPG0IA$1CKH0@#D@*0$LC0JVXAE&9F+ABETAJD2[H*EK%J(X;LED51EJ\,E^>)EAFE<Z3,<^0E#RSOBI,I^DJ$M\ILEO,C8B3N(59DC''P G);GQ-*LA9WQ3^Q,RO<J!&0FIL<TX,= +:M$+6V$!]?" 0H\$Z=P2QG=8@, P7;@K .JZ GM0AXP0_>4,A4W)9@D2''!LS]T^Y]W,,Q;1\ZC4\Q:?B)D$9\:^Z=X>:B]B*@=KJA18;Z808$.HQCON(''4" 17_ $.N0_Z>I@ :H&\6*Q\:[):"A+F @"*2@@HK!,DT3L@JKLDPCLANT"TG#BTQ[BUSB38 3M7,XQXW+%:Z!B/HI0UWLM?XRCW@ARX;8+]:P+!^(+ LS ^W,R''=2@\58*T>Y :-(  W(".PLA#GX+-."]?<@JZL+_?LDCJZ1X"X8BF!P+B4,J*\3S"N:D.R+."H;._X,B;E3^_?7%@BE3K3H,T"KBTO6VXO&6$ ''"&Z,"88#*%HBJ[?%(UK>"DKJ TE;@D[CDL8=,\@24,KY6FI5OD-U&VI+@''H"B3HV-GH!J3I0@_IRMM8U%L5"T0^?TB!?.%,X)E@\IIBHH=C[''GK\!E AH4*R1L- CDXWRP[4(1F(,\)%QN%,,_!(L-;L((*)OD W >''+EF@-,-HW@)Q*FL-CFL8=*^^2".1XH(8#("STDVIJB@IYD53LF=D7-LODLD,9%L>:QL>"4MO=CL?>]L^=-L?>?L?ASQ@BQQ@@[P><0PDK\V%7 *D8.(69 (M:.*. "4*ZAH#4]JH""PKY"XP%L#?U*$-?90AL)R(/HK-R*Y#K^)I@#F0QS/06330QWT.Q&$TQ&44+?B)M)&"V>@M]40P?$#HI3-$A_?E<*S+U^@HWX2RI0R.B()QI''HP.YY3R''_DA0''IX3QB:Z +(*8+J<FHYXZ#810$+=R"SL74MB,HPM2CMV EL?#*SND4S.U4S.&4S.74S($,AJUC^B1,[W1":#PLCS!L&-B&^3J"V[PP-ZQ"TSGR:]PBPMQ0SK6/5S0GDR0"O!GTT ?4O"?5T-FBT?7ST>,3T4TUUCTUT>F3UN=O^A*5G^5+ZX!CO6:SW*32T7ZS$W*SH!C!3 @D0$(SFIVG^Y1''3TZOR(75X%(JBHO-^?<BYS)53RC8KOZ0L1MDH .,2R5V)R40T-Z^ Z=PZ5__MM0\,!BOZ] TTEBJ9CIP*5HXYBOZ]E_Y5SKD)QC%-UJ2YE;/5U;+%U;9R%?;%U?1]U?3EV@+)UDWESI 8T/X8&: )$DI\)$P("@HP-\R\/;0R-Z:)26.ZU64BQWQ"%,9X)J6:$3MET=M=&QQE&V5+1P5=B60X\I&-ZO*9PQOTD S /I.J2"Y@%62Y[^V]B]*,JR$ ZP"!"&O=V!5)K&^*7.P)CU23"(Y*^J>]K,TM#70+44=P0D=T25X[S:6]$7QH607Z6/IU&/M-&3Q=&3UM&7Y]&7]E&6I2V8<0!X$;B7?NAL0?5P0\ZO*?M@0^^KC*(H2/#J=7$H1#23PE)ZKNB,41FWAFJQHG-\N9UU27?Q]HW]2KY]2K;]2H3]3O[]3N1^!OL]YJ,U)/")&$4\Z*T9HJZ+KPJPNR:\I@4T$N"<XO<(CX,PXQP=)^]]G&)GNXF*RMN8"''AV6(KTZ0> ZK2N?% F56F]M5?P!33PN;5Q:25Q:+S]%-W];<]QB38I[P28:KH3=GMS=&(%<Q,V.KGX-N. )0I@2''^UK+OUP(%]M74H3F%]]N9A;>[]?TQYR5UPA6Z./=@-5@6&L:,55_[J6?@V7]AT-]*9 9@!!^DJ8R$(AR((&#KY7N] (FD9H?;XGR\=/R2\NJ=]&Z*5DSZ4BA[23K]"BU,PVO_Y.2,HV[\(5(;03[M-"!6N8!778!8D8"HU8"LNVS5?CYU7E ?24I0@5GM:F:/;0LN5PT 0#BYKCJ_JK:=!J0[K7TY&LS8$8#LU8#L%X[DECS7(!BT#7M05XI[C,J%G0U.F8<''XQO9#. %PCVEO"FM@,G*P!G-X,FS-8$NG*DZ9T=TXL]JQ*R\[''^J43^Z]5WY3B$I" =:HW^,.6SDV7:=2#:S)KK\#6J4VYOT!9%D69%EG9%ET9%U%9%U69%UGYL6M3L9<BKC6H_A?65/[6HB.VI7:M_P5UQ[VMB?$OS+?V_9D9&_>UNT:1@T3T,W''-:]4@PE9:P-8X>V4F0(3$NH4"U-?ZZGDC[U2,8&]5($%GJ(LO#)CUVU([S%&''L.J 5''V%U+.,!G-()U%,8P=(XY?9.Y?=^Y?M Q[$@Q_$ QXB&)?= Y<_0A?<8Z@Y>)<!N*H%^*H).*H->*I)HZDM6!3PJ0 R=">3L&<3[HR\VEC=]"\@M3FK 0% 8QJN*$1 N*Y%^*Y).*Y->*Y1N*]5^*]5.!X60W:H1CX]+9(!C:K$VC__S1\YQL!>$4RF<2#8FG_=>A]6]96-V"^Z,P@KA#"%$9E+2)G;[E)E0)B>D =O J_QV"9.XZ7S.*7]>*7S^*5)([3?,K!HGMX''M,U< 7Q/I7Q=L_X) *B<,HN''''\O$8O*0DS.1BQ,+)@4_:(%G!9('']CJA#]]&FY EKV<;48)\MFL8T2I(XXJ$(/R*+1(*N2XH''YZD#WB^%WB'')D- $ A)& DPF DP4JDY&.DR@HDLOHD[4@D]@JDY4LDSO@F7@\DSK,DS K,Y$A.8*4F7.^DR!M.6[_.7 _.7*_.:,7.;-[.;._.;/S.<0Y.:15.<*1,PK(DL3 PH5M#?'',GY>!SC$/( A#W+GJDN&TL*ZJXVG(DY6NF?F0$N?(D]2.DQ6NDQ8B@QG*D\QDDT2 DN+HDYN(D]QHD]D DM>/,QDI3B8ZCA?34\C.C@0O>[G^B@0#''<DR)\EDH<1D^<1U7<1VD<1&U<1&V<1D''</1>!D;3!E(+A?;R-PM(8KGIS25"W$P;"24)1MQ@*K_QXFH63FH>Q3T!;2$=O^8*$^:BQ6K1Z3<AZV/=L^C+"J36B_/"@FZ1A04V\GS*AG<J!D2[<DZ3AF  \1_%A0=T\3^F E_8@F$#<C4RAG08\3P5\4@''=0@5=4@<=4QE=4QV=4Q''=4P7]4RT]3PO]D53AG<:/@F?Y5"H654B%%7_BUD@!/33N"MZBL\9\00-\E''H\3!>A5SM\EL9\EGH\1<<\C\0!1G_@D@+\F#(A3.O\58F=5XD=6H6=D?>LO]&U_]&Y/]&]?]&CG]''#?M_#WAPHA04"H:N$&PQ3\-9"::#9APWWZJ\.0+>DM>^Z )13P+ 0PTXJK)6)_IB-=KQ!R"*($ "+Z;V=%I:'';7.SH!<BG\\;X\T_8P?N?L@; Q''>@@8:8\<''''LHM7A+T@@6XP\S]?M@YG\]GGMG%7L8G/^M-O]K!WNQA?,AK_.QEGNT1''!5NG.PG?QG8H\S%''NU+X1YF$#"^B6>YVF?7E&:.#&;.(V)L''V;3(NXBG,9UWAX:G@;>XLKU LC?8^G=V<LS/,BC Q''J(^%!W.PA_]G+GMKSWNMEW.1S''.3M_.3Q/.3OO,4QW^N-OR.\P''C?FX+[\[N:HB*N.Z15/V0WC6R:GHU5&+0WY&@X"P,9S2+^+3)Y)SIP"J&+WZ?K+?OK5^I#K,D[>@C%5?0OU#7%G30\E-0ZZA7BPY?%5S0PVLDZ<#3!H17"D57#/9;R9_3LC3;U;Y36H5;PVQ;7[S?7XU?7W=;U^[?625;C@VDN9DE!NQF5;+(''</+.=3)=U\ Q(!H2GALE,BKG:?3NQ=;M*17+DS3NC[0\H%36KS0Q@ DZYI7PA[7NT17<4U36WQ7>A]76WW7>BY7><_?>=U?=>U?CZ_?Z@ZK_KAR6Y.VY=X0V F!L^#%<BMD!$5,@4E!D<2W\QX4\4WS<ZCF\2I@"?;?<PPL $34?A%,>&6T/RL, LV^=@@@-X,QQ@BS=4!G/%91:W P45H$4*]J%SI,:_P(5*-R)/X;6. S@6*0 ^_+Y:''\0)!=;V6,B*C"2(4\4''PC4^V U8+FT,6CJIM./G3T@''M =X&_M&*!N8S+=,]YI%KT?4DR1^?2G5!=Q9^BT^2RJW2I&3O:I /NH7:N?_$VKK.47=]?U(5V/-.[7]V2?,E,?*"7[M&;Z,4__] 68]648_ DA<O_L34F0,62!_N,T*;&4!-Y^CKBP*Q(@:/J0[I9''&S4TY4>O-''[;TZ]N?O;@R_PH3+=N3C*1:>Q;\ZKI#<2! YA_^.$I%19://<]FJBAAR[H8G$BK."  .!IJNBDEQZH&C]3EIMWE/#T-YIB",RUEAM''+ZVQ]RB-"FJKJ*JTRQ@/0SSKVCJMAQNN"C"B$49VGWMKM*OH(X@\&J3"1S=TK\%$$48>BZUR]0C0"DDG_WTPSVV1QMYYKA*"$ST@EHBTUXF$!L!]Z][US2L@9OMGZO''-A9 ?_R C@AGU!MFIY[)X0004_?Q9G1GL*HD_N8&H0!)><RG(F7B5@T\)Z8=YV.%,&[XF''JV8?XT_O>1 1T(^A^VEIX1OMPN@_2%ZAFYH>PC A%OP@NBHKW"58<\1/^Q1226"CA,Z\Y\2<8 1];"R)3J\(HG^B\S?]UINNIQY@<\-%YG&6FNWL/(HL<E9R6:99):K[++*+,,..WAX020ZA/V#IT4"$!!QP1LA(EE5G&WTH(-*"YP"Q2Y= ^X,Z6Z9T''H42WRPN#4*1YMO<T #5@=E8Q-%119?CGISU&EU)WH-!TVS#VZ!A]IZ[TEWEUI3YZI0WU,!X.(%\;ARW72@"_-HN\XT@LQ:''Q ##"M(%@O@_.WD>T?P&BWB27)@$7XZGJJF!%)>+$VJXF1 G2"6[VRG#_[YX6-:G7+,(@O@@7$\UE^*[#4%7T#V^UP]I0C\(157"M1,23OL''SV! +S=,T<&"T@#22N\^JLHL8''\$  ;%E76+.[V?6RNX@2#6X^XW9^-I-)(*TOJ^.../08;;HF]U1*<E\7;4%!>7B/2"RS=K''C@F1T\D(05+TQVSFS!VOL,ONZT;4M&DNNAGGH@=P.RR([L___^\<?*H78<\5AW]IN%YX9^B >0&FS:>MBYMB^WGE$5\ A@@CEXBH\,\H!["49<HQE(2M0C1K@Y@@!0OYES3FG>$H#DOBXQ:BGN::21M]U\Z(N):RA)O,!ADG(0!BP\(PYW81_;ON(Q+JHE2.1!-42,B C2<D"JX''VQVXGBU @@1R2XX3M;9@D@M_C_W1PCJO.H  #%R@P\C LGU!AALLS8CQ3PDK''QPNLK[N%V.[C5L??E F-,))ESZ\0(I?NT<X1*QNLZ48 :M<)1 0#2A B>4H?$L@D!8>L]T?[5J(A!IB7BV9G^BFXRFMUKKK%;R_$L8(^[PB<)E[,^1(XB@JM<[9N\;JSHG$J2.%2IN\<H@(8^M(,SDYI8K8NK3AIVL1''U!Q,@8HLFJ8"_NQ"CMH@QQW7NT LE,"D@7.#CH:#4"G@@H@18\$T>DAN_PYSAEWO 025TZDEK&Z476.0&M;>93WA:T9.3*P4?#NMB 4CLHL?AV:,NVS2M>B4Z _M!J[5B+9"\AXWHF 43L$RK/2QBML0(A3,(D*26FJLV_S#LKZCAC BPP4N78DL''''O GWWC?(0ZG4HT+5OBX_<AF!R\,JP)OZ-JT(''R%J.4TN49CA@@T(4X09MK.@CB")&CC]24K''$>K-9ZT1FQ02]/RR& D$>_A32GG(I8\_"DMH2GIB):,*%V/6(,)UP$QAW$$#,JB(?VAIE[.J5O<:IJ27LTD_;Y$#Y26^HH>\@HZ<BENH,K1BF4]A B^R@QH!PTGD0A D>XPAQ<@0H884ZHV+F"ELVIZ+[NA\92T''Z1%1W%Y6; F''^T#R?%T9QQV.^)_I^''W+F*5%E.)02B^-]$P*QP[IZJGFW=0 2,D$X /BMQZ(*@R[K3!"RZ6(!NF2*,82-FI0;9KEJ30Q!!Z,]",=AN[?25%*WV+"=7+(-A--FLVG$U)E7/,XBD\\2T A0&+''?XT''.D8FE4FE8RE6XVO*ISXIIERR^3I8P^YKB=V?0/ *X2LR  Y9TEJ^\*U6]AEF&&%QE:YB^Z5IBZ4[N,YQ1NNZAFABB_PQSS<4X%?*LDU8W*DK /:A0W:1QW&BI\5[-F6Y!"CG<QQA@@F=Y/JX''[GN.91Y%4ZF,:*<6S-#L8;W;TQ"6@G\D/Y# ?3,D;8;/L4 KEF5CB*B0C\0!F:8@L-I%PKZJC!&HIPS2]PL!(@,HH>#?AF[YJ!BCQ@TAED0CE(;&N^;N)Y.7/N;"NXM]L_G$Q7_%2J"R+"("R+]7!@5_<H@LP R?.5M$]GIX-RHZH/!42/^$PJ"/XBCN)PM2%<MR(_O!LLH#>H-TW-F9MYGRJ?%*R)I''&(<@$QT9?O2BH@4Z"FJ91I0J1,9!ZKBP0@1LTL1F8&DSW0R3!\\]F;-"H++#FO#7''<X61+N88GD''H^V),G4CZEUSVDI1)"MX ^</CI9!LH>_Z)V]5@@E%*B@L7<FRL>-PBC*5H<Q?B\]\T"6,N /(''%_;@# J(PQZI*E[S<LNY><R;372.NLWE!''G/5&#B=,J)_>EB$U_!\MD_NVQIS)HR7S6,I&."''1=(X->%UJ0J<R#RK8""RUG+_N^// *U1&] !F!)*CIQYT??6WJ7A>-$Y(LC$Y[28BX %B6Y#8CFO==S-TFA %>]&HM -*L^B@I@EHV-!TBYLX_L-\WJ8ZIECVCS.V/KO]-3M9-[ 1M$@JPS2$QN^%O21)G*HA$;=F12C5/R''G0FXY>):QN8_@$GMR1MTNW0ACU.02<JW)L_>M&N(,X.B#V$&M>=#T<12G9#\PT&A,Z2.N,/?''*T)":&L?56PP#-<]8!>%V<K:S0FE2<%IRR4''V)G46]_&%LO0QH6[!^OJS*AZ+2_O+T_4#8V(.%UJ^/R?0RG%%]/]Q^1A)%<G5F!QVC]<N Y3CA:IM(D.DJ0ECIL::0''B#B\P-$"R%\/>1"G:C1?0 P]D0B*C*6,STG6CTH2CTK*H@M&H@O2H@P.CT''=P#^A&;"1!R"-S_''U#B''1TN+IRM HQ@J07",HR*@0P>BD!!?0@>U T274F?<DDA4$&2BTPMO81]34A*[94S%\D4)1B&1A7-CZE5.EQ+1T!]>4EU[ P.9)1S:D''H\ZAD@P7H61E;.EVF-EU=7,WH+%7385QN#@ETY45?UY8Z M&AZ0QW8!CJ1)FB+UC0NE#MKA4,,XW38@0Q''83Y?L@]*(A9(8H_,P@RNHR3!P&6HPRU81WUAT0/%T@;L,@''9PDCPX@3RIQ"]T''^YRG^Y]Q+&IFSC=5%>11R@A5Q< 0Y>06QJ8_=$XCD8WYTE*WPK&JH^&)LHAV@N;?HG<%@X8$@XULH(SQLN%-E[ "E]>@D@]=XL''O@[!?TH+_@HL_AONTZD42"D\/IA&NE]H5 PNUI(R+ES"AYO/!\</?L"P!U_HCI_%\X%P_BER,\D Q@$\%A3'')X$Y6"O(ZYUU)I8CLMGYCD8*=XQ+_X>4PM+]DD7"O@RZVI+B$I#<6DM4^@JXV@N&\LJ''$@N-7DK?%@NN4@D;O@.X%HN:AE&,EE:R5LN-T@KZ @L3OH.:UEF5) :LU&@LC&SL$&SM6&SNU&SL4&A7#X8(U FL5Q.M8Q$N,QC#)AGKFDKNHLB*XQX#OHXIG$_8^C?A-; B@M%C[Q0B_''@C&+ C_: ^ZH B<!5$)P1A4+DC0A0FY5@B<)@C%&)YX<AF,/5PW&FQ-RH%1PHQ7A Q5<@B7''1P +ADBHS!Q(!RKF2ZEYHD.6E\)&PGIB4\&T!D09#C32RBD,1@35A\4D1EA-3#9?)RPMVL(>DL %V]B0C_GKHLSNCE>Q#B2"0CEC7I+PAJJA2F7A CH@0A7''BCVE KY5@C @0A:HPPN-1SI+S]W@ Q&+9FI6@A>JPI3X6Y.41KMU)'']^I'']&)'']/I'']6Y''\1P''^FBE_9PM54EE" !A.84G\SSH/LTNM4!D581B8G@AG9@DXD1N8+3E<ZPCL)0?0,5<@XA<@=?<@^ \@-D4A[HH$@Q5X,@E1?GE#WV @]%\@/J\@KHYBA-D3,[2*D]J#,NL#+,L@^50BG"-4^8%5M?9C,/D(8?AW1AEVD3T%S'':H<4$ _-*GSL!0$5A77[@9(?6$''!@7S3L&#(,33\=2U!@''8DJW9(0!V3,@04$P]L@@"7@@SC4#ZYX06E<P#A A#ET@:L\#*IT@3,\H/PX@XGH!#? E2GLZCT()0QA@_PL@[[@ 78"Z]9**];2*]=:*]?6*_H!Y>"0B()8@]9TS\F S@2EE*- "HY4U:&QR-G:Q5$XP,G0PQ9DD2S8Q!*"!>K4P$ -Q"X<S''L @[>DO<''[ L_2@J@;AB&''O\H*3(,''M\I#V@_G)QA7,IKH,R+I]R+I/R+0W()I9 Z\9A?2>@G23@8K1DDSY""IUHUT^ /$R*N!TP0<XRE ;L&$X%J%H&#DEEI\"@MN+A_'' &$90(2( %^6U]JQHUJQ)^Z_+^Z]MDRXCE([)HO-<DO^1"@''1L4_:@F&XMWFTY/\MA>,*@XS6L_!!D.E8T__4@DBC@,-M@H.*A>8N&Q&)N11NDX1LF1F:.1G!.2F].1I@.2I_.1H(.2H9.2''=DZ1"DO]CL79%M$_7]$E9E$ZKA$;6$?A''F)GAIL($AA7KJ+8R@ZFT(-A_RH"PF^(?(X4OHH\"[?^"_AMK) C''?@A>K0%#3)F:.C$3/9-VF+$6HK-  BF9%PB6#PB;Y0*SQ5T<?:!J*4XB>R&GMKOLI7E<1#_K*SH3!*EY/6B?E@+-%S#>!*.BFS#515L."(IVGU_R@!$C4''O6/BU\.1CE AAGO)FFHI)0 ''B);1AV,YFIRAA$@XNU?PFH''UFMYPC/5 TH?@A6U0E.J B>V@A"4FF&N+.6B;.6S+.;-[F>"TACK;E\>@ T-A[''Z[W&"P[#.TV#7DD%7!,4''PC&_AC! [JZ)AU>CYL3!HG9,KG::ZQJHWJIKGC-A G6( C T@@NK@BV+0S1 GYI*8[Y.8X5PV*7[4C;XP?0"04IS4484%P!D8J4#U.%8E\7H0X)+GX4(.=1H1)1Q''4''1#"D$9]; W3BS*R#;;JGR&:YPKM#AHA3O3R#O6<1W 8P%<(Q;''$Q#C<("X8X^A U_:TUCLP@,ZPZ[&@BC!8@>>)A#0\Q."<@V]LBGHH!>''8:EI+LQK3E77XQ3&\B*04A1A8A2".AR$F@:B!;NDU4>SY!CKPA9D4A!#L3*BLQ"J8!''MNQ)*%6FLH+S FSR^\1.AXS&@LQ+DH(/4*<_6645$(0ZMPJJ/NP.= C1/>7DN<X4,X,CUV(:N&SH9 ''!EYRO-&F&=4ESUL8=GD''4X3L%OP&(&41U_<UWDA9A+<_==@:%<AT$3S2\^?G,I-_@J9J@K.$@N9F@L9J@H>Q@M-60L-:0H''CCKXQ@F1-A%",@G.J0KXQ@@''G@B-<0I4Z@H1)@O(D@N>Z@H"$@N+J@H.-CL==CK1#CL8P3N823N9T3N963N:X3N::3N:/3M1F0LI0B]+BAJDGN^JSEC(7TQH>^A3>,H:7R)\3LK]$PQV''XVA77PAX5PAR6\.:%%-5@KY<GPC:7PQFSPD87PFZ7QF<7QG^7QG07RGA7QEYDG30@K]MM1 ?&D4X)( $RE"83@1LNX":(;#(PPB(MJL&.Y&J&Y<\BY?FWA''R3T''>PPICN$;F)JQ''*ZHO0"* %!<BW? $GPC<]PCTS$4@%-4U M@AE]4BM=4LZZ4U5=4QIM5!D]SBFM5&&-5&,-4P:-A$+IAFPQ&3T;"#^;M5%<"''?3''"V]A51%NIY,GE(]5&^1&0!%4A9]4V]!5!5MCH''M5(<M6Q6M4\H)''H70AUM\DC<Y0FT"-2XG47_M''(8&A( 7NCXB2P43O'':;_D:%@5EE.C8:5KG]*H>@_TA$)L]W2(Z))J#,R*(L]DCTC;6PBO/ B]T@B&:@@N(@B-TPCZ> C(8@B/_ BN+0@-W A(9PCX)PC\Y=C<*M7^* BH; BMF@C*@PCY;07MD 7*A@A-\]CP8@7Z=07R> C/U-7?^M7?]M?=?5/]?67]?9;]?8?]?TK^@A#-?SCP+SC]6:0@2= J!VD&;87J!C.[09)F;>7K[3\ 1Y8NBAH@D"JMC.V-H?-D^&Y)8N;$!9QJP0$P]YDA;8HMT0MC\#FAXBCQ[J45$4PS>3-.L:O#8<?.L>? 1@O.QBS.QGK"M+H$PPT1CK0@,N7 <8T1^C,=$>D(US&BJP>-''WZ(VI%AI)MQMJ#UP;L#FT5AMOMZ87Y::23^YL"!U*FL+LLUS!-]Q0:CK2B&FV6 ?XXC /[#O- @"''Y#LHZSL2P+0,(RN"!C94P38%?S@/PS9O&"WZV#NTS%R:T<"E+J6:,>&Y+"N\+#B # "XK/?*(/9C[U,>^RA>/=HO_"@A\67ER$FJ]I/W!Z^J-MH!WMDKT"4PH5!<$#$>$!''&CR3,CDO,NP7,3D)+2U;,3H;,C1O,3<9H3P;-2+9N5[8%>$ #24B_UXD*8LD&6:DK!50UJ<(QL+7%KN)(J)F7MIH22]O@_/B-Q6DF<P T4B]=[Z;/N)FO@)5="<P%S)&$4KJ$*]2$;F8O,\$RSF@K2=@O2M+$A2GEKIDW2(DB?),W^_FWRY"D@/E2"A+(JI@EJM@OH9<WH(>(/_9C!;+2/W:(KM?*K*?2L?=CMQ?3KV?3NT?3M;?3MZ<\8\D\C2<P8^G''D\:(8>Z(A%MZ9=[OR G?CC5$J%0Q&53AEWL3Z<13UD5W6.+XOE-/T57/-)B,=VF_=UMN="01=''1[=#V#=&!/=&4O=''C_]GRC4:X28,, Z@>OIPU!C$8H+^_%K>!U!X+9*G=@4> CRX74\,''CD)J4EAJ<(0( EOUP!/..;>(J2+H&D?C5!$_''5GQXA*U&L?Z*A-U0BX5@C]10BZ)?BZ7/>*7_CL4@>8C0>+W_>-3@CX@ >;A?B[K/>;7O>;=/><MO?LU/?L^O?L[O>)\@BN( B$&@@(GPB5L_8SAS5>.I,4B5,8[''BONR<J0UE#X3:CUS(4QEH>YO:TVE?.Q??.5/?#UZ?#PB?9T.?>2_?.4_???*;?;5??884+M)@!BHY-''*16QV+7;=B@;,Q0.@H''2=ID:$JAD[@@A(4HS[6ED#Q9@]P8[<2MD#Q0BY'' TQZF>V22@.W0ZY%Z^&H0CPJ/Y"L''GT+V129D"K=6.U%7<;%R9%6-S)T:!Q)T:%V%U)L0BO?C23&Z^_3Y\50=+3 7E$12<\+PD((KR''1D@)Y35#.GAYMX13LAHA\H,XQ(27@@<&WM#0X\RIER=&7M 1XSTB>2E<=!TM CIQ,\+[ZMJ$HX:C@HA2B 2@H7-9%.V1%P^,0J5[<0QY.WI6;RB7Z^_^[[,7[=2:_P_?KQ38[.OI"2<''7!09\=:.V<;*M3AO+?=@-,SZ)F/.HUP& $6"R_-Q9O&3HLM9W/?%3>ULYF/:"Q%3U''6Y@-W%[C(J JP*]B!J#!><DN@-*1IT\DDFF7QPH P/2V(V,P#"K  ?XKJGI(0:N:>#S C(X2HDJS(&IY^>T,"5VQ*)@QQ?>IBQD39X@XHOHE"1<TXYA\F11!1?ABIHOE"Q$YTG!AQ2RBZ[1KGIHY>T$,$''([S22!.15KIJKV<4L$]VK+&E%.3(X#FO2<RHR$I35-/HDH<4B B@ZI)"P 4@5I$EF5#<XLJOYR L*:-YV,*04I\NM]R%QQD%"=EDHW7TTTT#)USRR"]-]EMKN]V4T5@Y+X8:B''MK"H''?X;+J018TAGKH$X"\X*IC=CR*M[6S\.4(I]((]@&&CCN$2ZX \MJ)H SM@F*T^ARP@9N#$'')0V&*+-W[AN2YDQC-BC;W/TKM*AP6M-]+ZB\F8L$E$6B0(]J4Q@OJA 95GG*F7GF/(-ZZSQ4SI%=9G?*7W&(@KY*\S_ ?.!A5+BKZWW# J9.^Q"Q$&.NF@F=YX880/1/!#$B?.^FNOL3ZY9H9G=) _#;ER81!< ''@&BF:?RD$3@L2ID851M<(G@C^\ ([NY[QS"K+\?I!EZ] (UG))):NF^.*''*9[ZZ**/5#)+++G6^../.0Y[Z*)S,0WP,7/1P2DTEOKC''5+*-IL''??GH>01W/D/2[BO7[EZ74O+D6''@.Q8M8!S>&?IO$%:DX+<_@D*>U_GKJF810P*8X.+B%0C-4$2RNP!0QP*UN3FQ)QN Z5P=![&F%D6/ 8@\N#NDPAVB@:=W=W8C9XX\]TV@W GYYQME]=8$]O/9!9''=7?/&G:WV>>]>#%=;:::/WW''+()7?>^##&+Q^]V,2)3*-Y<J''),-F_X-OMNLNA$1L@;!DZ@FVL8XP\X83!?7??=T>@?@- @P%80O8!4H@IYN@BGZ!@BCX0  >TX@T)^DDD@)BAX^@_I811@''K<#11X.\]5RA\9''-BJIK]"8T#6-*/84LP%!7+M_PY''C6N]Z?<"12BFA>PPC0DYAR&UH6HQ#T U+C1"ZU5QRGT4QBFA!N,,9R&WV2ZRK,K9XSZ.:X^DG"@K]#B#GOVRG\G4%P#.7P9!.7LXO:@A!3>T(11? LLA)#^1 O6AFZJXE;702CA;CV19=#J#< @IRDHN#FF@]I#4B)%HQ@X,]82,52K7US4I,\HV6VEB4UXCG911A''8=4<#ORMNTQN3%LZ-$YR-]>T)XJ$X0@I#CKV(YFLDX@31544#O1)T74LE/O^E0C73,D3#;\D])Y@''BB1B7ELUUHQ;RXM0O@GB H6YS&4Z<''EX2Y:F9XD!CGL+H97H%N!JUK"TBP]P3-#HK[ B@C=''?B2P =4 ;MH)QENU A,O8$X!N_BDQZG D;V@''B%G4D7XQ 8C8N#EFD8 O]8*,Y;8*VT&KA-J">++^QS.Z4X$R<''(V7V  :5%I+NA"VHQB$8#V%CM!&(\#=IL[QT(TBG>D$A2\F@QO^?)S'' XU*DLUZ%FI^%R#I!V)R5U*T9'':UJ]F=Z$;7Q<_UJD?S.C!C5B9RC''G<=TV_ !49$DQR9[6J1$*3Y7.1ND3S2 QL?S0!<:B%!^,,D6<9)U2RWRIS\A9JKZBB0B8F%\U]P W.XAE">:RDA<.>#A>AR<L_W@EDU1ACVO<(QOLTLP#D&DM "X"FM@PASG4.B?TO$J3,)B(?3VX<X\8R 2#HWT$IF4;T]2A]KX&OZPY]T]O7(H4W8@@@B4H-Q5PP T+N MIV(#IDTO<KF!K"]T2=G)];E(%C2!DE-42X#^MNC^,:X$)L_? -6CQAS?4FU1X&.''V''R!NC+<@(!3*\T7.Y%^?>;W)QBQ$#_-8AY0[<-[ OG\VDK&T]C,1GV3(X(<M9PE^<:P(N2KFCC90 1[,XLT#2IFIZM@K@L3(U3#J@X]N!NLQZ% 1P,LX#$P$-A0I:0RJ5SAGNL#")AOU;RH5>%D_%9R$PJ9([$O:8177R4KF+P%K6__RM(7GU"ZY$?6X $H&YC''K?NW29KYL$U"Q:BB2$*JG)/<<7/"=DBT*RZ-<6C(S@.\!!=6UB@==*@M)3C]ZW^Y3''7.QKRV2BI3G%H=K2!0RJ+KE"( =7P3[Z8=8N+Y:=R*G/5HICXS5011_(LT!;GVK #:"GLG 510Q> ]+#AF LE[-/) A#C?<S*@K>>5.N4)[7 XW-1/5KZ97KUJL''!R%1S5.U9K;%L6X>V9(DL4)&VHK''/P$/7:&]+VXXH.7_O''J +DU^O\FSIDH,37/RT&A@3\8>?1*3$.Y@UC&V$7HUU/^6.2&4% JY6DY6J-"QPL:E52QD9WA''X%Z2X["BPR@Y[R_#9"CF,B(A%J+@]VCXT\"WDFNV&1"K[<C@B.T@PC?H(R!XY4PAQ@J8H)[4L<Z-9N(IGM=T^DZ6^W@Q#KMN[+[H''/4,QLSAUZX_J[5J]!=L@TI''JP<)9+ZEM(<"_Z49?74A$%@25M7">]4A^:<9R(%J3''T(@(''%&KA]<D''$,Q<81FO.-8U:&,/X!J[WIN/>L*F/"+KXLV%E$T_E#-2^ZI@<!C)6!K21FF(QQ!,Y4YTO0H@(!B@<PA0BWY0H!0@PM \/JFFS-@OC&"D1"7<XP8>.JK!?.IW1XJ[<5/7VM\:%;&0Z?/+;CWR9<TN^"Y01-1.=20\4*U3"ZSM].ASQ]+W''  >_J?C<M ]S''3C.%#K6;]50''%X)])P(_7 3FL-?2T.6\AD,12G7>BG74G]]I_&0&''.VQ2X7?9&N(OKJ!="0_LVEN:]Y!>!8''/T(!ZMBLNM8" J@E@C6)$<_=B]1[LFU?@G_6F8!KHFPC@FV ,C@FBX\OB-WLN-1;+@CLQ@$=K@H0L.5>L-''3LSI1L:)7"_E7(SMJ@_J4,''LAL?FEPP):NH+)(2,F*>+5H3>A \6N"+,G"''+% 7E>2E] .J(]B3HX)AIUPPPONC[]&N,1H\0\JUQCLWN-.;45&&.\.C1)H^_"&(6!DEZJ@E\/@D[; EQX@1U=B''DFDFZO"C(TD1@KBFQB"F\,"J 7FEM8HCXEBCV8"5FG,,C!QDXROD#/<21@;40NT91@:TG.J"!^$#''I[JCNTB@EEZM%,1)TU[P$:4%.P[#?J0F13,COX0K6M2BSN9C<C!PW\Q0/#:#5<HH JI-4:,1ZT /6>JN:8[)?S[-;E"O>8J.I&0"^(K  &[.X*3!"?8@%ZHH8JQAV!P!D0HFL(3-Z2(E5FX 0N@@5BSASP@PAPK!#%$!7H8!%S"%3;Z,^31''.Z1'''')ZQ>5)1>:Q17&,Q4%Z,%V)C0-ID2"K*_N JU,LRL&Y%^=2HY%BL7:CHPJ3O+( .K%31_[+!VMXE#%P@KS[L8GL2H)0.<TR,G8(LK(;-H8(+K2;0''SYDK[J@27:N7$JOG9@@5+?THM0*CGM"+T?&LA2.@TUZ8V5VC!6@HAX&<L?PBN SHP?.@VD>PMY0INHZZPLSLR''3I5Z&<+#$T*)#L)A.,+#>ZOY:1UDHIY).I%J1K7''^":^8S6MSD,Y=B:NBLW2FLU!>#[(N16ABA26(($7V1_,:8??$BY*,"=ZUL-N1DV?4,U4X0%2<)A_EK) 9C.3B(-''8H[9*1#%JP\U*0T5XA!>$PUZJ@M6DHD9_HRM.DXQ(14"4KEN$HTEJH]?:HQK@HINPCU#*@UW(9 $04@NAJ9[B4G\5K$$6;D?*"Q<OB-SZZ''6NTF":3[3VLG:DT3''+@*BCK_%GLVR&L84:YU_DYYH?8%D"D0V.Z(/HYJV9;QE/**P.D,T>0 K$V1K/KOB?&H4=(*Z6V","4*^:-DEX  CV&@FZ5@CQ>BDF N@A8@FMXBFMVPFMJ*A_Z$A\&PF[+P0]- AX&BE\-@EU9!@_Z$WOKIMC,T#C=UPDN7PDO5PDR5QD#7QDQWA8(*I SL6,TP6R5R6<NHHSQ1OF9TU[+NUTDR3XB+EX"(7Y!(\.% W0&FT=<(>)U@\1/%KZ<J&F8WA20D0T1$0&*!K_UOL\6IL]QHCNGLZ 0L@P_@(_-@,_''(DS.BFV @9\S"A>>PKMHBC''A #Z2@BZ/0C36JFV$CP&PRBDY CYP CX+ ]4)*$DO?-''Y4;UILJS 855DQEUDY=5I_CB''>0CQ<43''8,N#M#3*Q;T$:5BJ/SP["<N/!X"TQAEO.04&G)3!7:S(-L.49]P$A;.4EC1T)YS99)345LE?ZR(TQ)G_)3&H")E?.3LEEH!B<XFNO1+HLQ!U!S 2>@A''ZP!]!!!''=:AFZ(,X*KFGO( ="LQ7+:5-,LU7@]U7D-U=X[0]$(/9Z2/[G\&[]D@;Q<5R^U-+)YO+N$3&FR*[&$([&HC[DX.IZ@RH*HI/B<K2^U5;T#/88TGGF2(\P4);4AQ''U*-FV"CZU)!E-@.DSTFNO9!4^PU''ZH+TWR+IA]&G'')U#-MAAR;U!M3!QO?R@Q&X@U*T@PT>27.(Q:\7Y:\/U&];U&^?]&]/]%GHB9?6BI5_SJ(PDG4J@<VQM!N#\;*1MK&"=*=^[<(%LI>UR=W?K5^"B,_Z!X$ED>''A[8$$ 68,0&P''LIZ*\I\SZ1!YJ\I29:J,Q7K+KG[JY!>X(_V;J_X@Q!*+SD0,)_ B]%A:HM[.@U1:B04Z@W/@U*_7U''H_U3I]U2^YQ9;&S4.P*8W]X)$\9O%<8 ZGU,[?TP]MP''10$E1L<V-,3^GGZ_<&HW=PMIWWI2!4@G@OE#Q+S[2L<?M$[9^%E+3$E#7>1-,NI)Y HU^Z@P"L@ZD,$5F7]AAP)8TKQ"]V9 ?<A_/?;%[D95^EM5^;>5^<NW^_.FW%^,D]I@''5  D]6$M)A6:JKOAW-KT7A5]E^J7TMUA@ABC6L!NU!2\HAP;AI''H(J /U97_-X-U(@NKI5K[,2AI=11BWWVI5V@MB,FJO*@FYU@F\] DC*ZFC/9 C=8DC09!D/9 C#9!C5XFD]8DCS9!\W#!SXC!D99!F*9!F;9!GL9!G\Y!Z-# E^XKZKBI_)B@UP&D][6=<7#W7PLZ@1YL^+V;TO0%G-T[_S4/HGWXH!6VP=$J Y6HH.0>I V?I/X3<$,M<=-EKEZ?1V2_1%RI&-"P!DBAX0 CV<J%VE*LN 8L/["EF)"%/+!#P@;$0_>(AU(0 5&P@G1PBOX50ZYPV%5A 2([X<F,0P<9,:&UTQ "5Q:<#:9SK9\P6K\8A"?H!O&RA!4HS4&^-;KMC[ #BG:]EE-%V;6K8OS\+&_8@(S1+]RL7$FBW%;>TC=JK\6DGU%@FM#!YVQNY&U^Y&YFY#=BG''[("S0P"FUH"B@HR7ZETY59#;I4K$M(-%QNR=K-)V>[8''0%1Q?MALBB0&HLB;4TN8+XO/*")%D(DM0MY?4*8<J<DOQS821%88%M$XQ(CY-  -1 @ TN (P %^(XEUJ!$HZ&#(QX:H\&EU-P:J=(CR REKA(:H''>:H NZXX^ZYD.ZYJV:H$&ZY@6:ZW?VXY*- VB4@;5YP6B9,^$ST;%-AV$0>^L#D;8"Y<)S, 5V8%ME(.J=U]U%X "?BG[QVV^;#M@";B3%[-/LSR;Z6A\''^U26869:H]@ZJJF?&+.2@N.\H62] 7VRF/ZHF.6_(Z/[N/YVN!>ZI/2F4XR_@Z<3&.<Y&.>Q&.?=..3M&/A?&.>7&..NN/CS.2>Y.,U\P6X]&$?,HT$B@IX\N$<<I-PH$W+EH7)^&)N_NHYCV5<_S8+# ?VOS_<<IT#;P> :C9''>XWG.V_O3*/]=R.F9Y0+!]# 5]K!;UJV CYO:(__1([ATQ+#+ &K-P5DTTW&E)P XFVZ- U7X(6A\4JV?5B+9<9.;UYE&.!.;/9.COG.9>Y$<^Y.;Q;/;#;/9:XO;%:[5&"MZ,:N]) E=RU"&&%_9FR.K?"H<(BSRI;-0[SU><W$MW,''R2EF:,LPNXL/+#VC?9BC\1BP.!I[@L<.-7-B+2@VC]GBVL9*$22+<8G(VS@GS>BFQ+"CQ& DD>\F[+"DE''=1ET?1EV<DE7_1E6]1F&_1E[_1R>#1G_]1F+]1GQ=2E*?1F3]2IC?2F7?1IA]2H%?2I6?2G*^FG&<DPO@DY% F!J:IYT"MX>M\&G(OYP.M4Z#03<;Q4K;U(C;''XT)''&2ARCV=]Q>%"1NJ>ZWJ\0CS3[RH?:AXKH$WU7??U;PPC:NE])(:.BP''AIS?.B;4 #E,JCE(B#C4FN\J(@TF>]D1G#CPE@G= 7["35I,&2?7&F3&!D35W0!(42HLDM:G^PU9E*4L!LJ4EXB=6M;@-XJZHETS8->/8L*8E#1$LY;\3X7C2..24UP].6=MA6>W&0%)PAA7! 70H@G?HDT;H!0^ $RCIA2CHA7>H$Q?I!2]!AT;0A6NHDW> AV:WDRW!DW[O$''ZO]2>Q]7*G]7./]72?]18Q]1/! 4Z(AW?0T,7MY#BW!9<F''W =]^A;X. *7_,%[_RBB[C30Z*FWW">H C /,X!$F.:,/^<#*U[.''?3>F"KRL>N4 A+H'';?9$U?#%#^A+#(:3+) A_X5KFJZ5AQ8H]NB@Y;4SGWL!:XCQ##HSG#$[#+3Y]K:);/ Y9 [W(2^/)2IU^)#7**E1=+-PZ_(8:A(0%[0N=F3)$OL[+9ZT:EE3>(+VQ3!%>-X3NAXJ\M\_/@VWCYGS-7L>V"0L!E6:9^Z@6I"@ DBSOW^H) 9>&23TV4WVB+)$HO_4<,_J]!,P=8LP^GBXY<FT)IT!88.MY)W[%=LQ:BJX]@XCGM[LC%6Q]^I!''TS77U99"T<Y#UK1"G6Q!AT!:T4]BKNU=ZPHS38P+K6M2&VJ:CU<;PK_.E92U/R7/MQ&]2>1,(AH,8!8''C&W,D&XG??>"FN8_-OG]AD0I<"XB6LCL>*M#58 M0_VY'',6+''%-=-P(_9>G@T,@"B^G*]YA2X&_1F$"LXXI"WUO.CM(DC_"(G F@''")4ZY.0^CR37:AF;! ]#NH0(\RKE"!LYQ,Q(<ZAC#P4=\)Q(#Q4\!(<<7ZK5+M>,OG:^(\"CA$B_W#Y/8,Q9BX@9MNGB>SRD1.]PS B"9T2*]B''S)$:_P(4*]R+U*%Y3L D@XN!O''5>@D T*M"3Y,&BA@, T9I$_^;M&HY(U1E9[._ZB-GTD@E+ST[^22X&''P@:&UU;>L\U7$8%M1+70N^8U.U^^OD2YSK:*^SO''3$+/@G $M4?K_*RC3O>:&1+.KJ5#11(BZ 5@@ZVQ@:UU''V]-O9Z <34J5>''Q/4_VD,F15&%G''38@WG''R=^!O)1N]?$G;T>: +CJ"(B5$Y''J!\&/#E:H7S'':=^/W&4[>O;=8<?__04=NG[27??/''=89/GS"_,V@LH@KS84]H3)\&TU%SM@BAOU4M1!TY,>P@@"&\[\-"!!1=V1Q S-0@@5E\TU/ ZVZ?=]IYW_72QU%53([[ KF0-RM]]K>2UUFR#@B@II._D(8L\=Q2FET:XR]YKO9IIHI$-R=)$VVX9ISF%G3:B6JVWSTV6$6"H$FZKZZ&Q5!Y^B6+%$8)D]TKSX$(]H6M*\<7BT!:W7LK?!3U0G@QGNXBJ$$ X__#SRRJ]$KLIF]+]$$ 0''_31RD''C]PJMFX&T<<];G?EC4"N!#,*OQ!"!2%F**X[T**,&_^R*+BCQ2)EAB0G*2XF$;]YRR3OUAMUNOWT%%%ACAPC@OU<26:233>ZDSY- .[&"-V=VR1QQ.ZF&89517_%R ''+11YPY_<&!P32?2OFCE0I 1] ,/]1220IZ5PH@X9YQ=%"UUPK@5IMNO.HJHII!1(PD$SFACYM\Y)X0-AO#AND#[-''"Z9)/$^XVWZ9Y^>I,-WE)D6:Y>N*G''+-1\@,+>0$(7(CL?DMDH.D$X$ ";O!S@CR"@M@IF)6T]=A@_:#A?9@5()P3HA2",F]_^12MMAJ,B1EH8G!Z[;555U)S#[WRWYM=$MHGA\!PI0[R0&.^/L:T2XL<SX#F"TPM$"GE^?O]-5P#[%W!"[E]Z:6[Y?741Q=1$8YH#W; 2QZ^]?IX;%J8Y_MKOMHXZQ!"UCXVLBIRO#Y/HBK6X(.HT>*K>$5SA,DX@L@ KHD-$CFI&^(OK9U00>5\:W^GXXX66"1&$/XL'''']A7-)V1V(KUI3AM)%T''Y&@R5^NPW@C@A>*X"SJ KVD<Z]I(#@SI0@5@FBMJ918$4&\9,V("3H@DLEGI;H0ED@Y[M0"VY<*&=">I+Z,W\5+Y%- 51#HPJ.YAHK L?>[*] A([Z51F4M()9S!%T-:@$%VT QG %K2K^,%B (6$)Q8\Y"% (AP@2>X$68ODZ#.9@KX+4@$"QFHX5_LJH[=PC@N9"BBE\LCAES,($R<SD%1!2#R+Z(!X \X2T&VLXL++CID2L3,G8(Y(&1Z1 >>LT$2?RCLT-<SOAL*A&K0XUW?\!X !27F+^@SDVDF9%-[''J2.6"/^\?(7/_FY!16%@LM"%AFO(BQ"D\XIC/,.DT90#D[A2QBC^0@@C.X$P @[LH\\M@DI2/%#5+8(13$\DWP4J@*,H$- [@\60H%*DB,AZ!*Y;MZ5''!9- M.#V5.4=!L2#@7BXG%[%7I&1/?7N#LY79HQBQBDU]TYL4U-R!E"(-QI-+2D+/ JP!6AA\"5MF#NZDNM9JP 3R:HP5L9N@0[Q1QK>YE&S1LJRW7R4Q#%.FIV @!XC_I@1X[@PA43@,@IOHWD:X4A2@P Q*M:TP#WGDKTSRTE8A0QTC%%X^MM"LP-& G0-(H3Z\P31+FP59*6GH''5T3+S]JST6O(IBN7\N1N7N S>D0U''HV\ @".4HT.ENDOZ?1CC]:@9BO&D@[-!BM(ABJBF$SA-E,<(&''\JM=BE@D@S7Z"D39=)]U,R[TG*$*B//2%@6OI0J9I4F0L Q@.TELYX,X0J!:D''%^P)Z2S@#Z0U4D!";9@?:E+W+NZE])VI)RWF&>*A*\K()E^>I$S1T#FFSFLQ32* H(*FDX@$5%]K;Q2/;S\(!]56(L-2$FDX6BFD>S(AQ<D*#+I^JHS/SBGF4Y!B8ER!#G=ND(/ZNDI&3S@F,,0!;9:0X%NANHD-\@FKG+!BTO4@!(A2@M&A[,Y4H1)PW$8T;]PP<9),UA$MEVR2]H"%9]>$9A!N<" $I)UO+"!F*90QP@RLYO#0NDV5*"TMV8QC*,B F&E&$MC0.FJQ4CC/>U(W4_^^#TDM''B-Y7U+6V3Y8PZ^I6/B''BF/).D *DBHVB!:CXX49M4W0= 64:SP,QA[.LMUL12J"=.LULLVPO?"JR;OLJ\Z*ISNHK&SDX3@!B&<DH2R8RM -"AS:$YG#B 2XP;5;HT+0-FK*L:KLXG(ASV>XAMWLLDORNQW$0BP!6T P!''G0D\P>''TK3K"9E;!9SA@Z,X1%3LH[.3M)#M''[B3F-5E^H>OFM/)''GE49/S-YKR53(L$O.^R>V41FTJO9T#"=4*!3&EH:@/1NZ\'' 20^Y Q"4V4"$"PJL$(SGHG6!Q@7;@(RQ.=SCZ^,5!W*N''%+8VM"8QZF1M,<N''(- IA.<J-?U6$F;EB(]P]H0F(8204M+6++QR:JHV7-"EK.IV)U$3%0P1S7H9Y@(O@<NH^KPK@E.0#VT@TE6[XE[?@ D[\99K^9MZML$1@)4ELV0B7NCR:2YZ_(012I@I Z[VI#W E0B<@X@9A@03B=/67DQ#C3&R9"4),7QL2ZI^$!'':YCPD9D/$R4-J(X]R@3HNMA8<F8RH0!V^YL\_ND&4(H$BY;\XF#.<$X AP6L6_7"$^R)80KQ]FLPS3FT/K035>RJ[00:<8CB_KS\TQ4![]2LJ!)+I<[L;T9*AH13[[V3M[AK%C<V@TU),QBLR6>T-[*&\$Y.$S%?<H/B>"N\YD O0_ST)2''W6=7 7XPZ8MI]_^YB[G63!CQP$@PA!;!\>[!DHB_PC8K[ A"^B''''EW#G''O&I''C%MZ8N;S74RY"?;)K&\:$/K"H"44)YBF\(F7SSHS\[W]1.]YL)YA0GHHV9$EC4/"!<44^!A''-:7R\_%*";YSC:KK802Z@<N%DFBL40XC$J:UN0@$^D"N>_B/:YX&5XX/M)7O]UP[12,F&CF-B7;;6W6G/_1HRE#V-FK"9TKX$3.KH2H>!V:OYDIBY$>W\QAH0BP<A''.CU@3D A.=T324PENIMBSEXR^S) #D8U<C T>+4 #+ 0V;-0SK4@./%#,G%@3;0PQ#XAAG< R6@P./,@P&F@WB]PCE$U448DZG9G7#E4_GL4R18T880(_M<DEG,FA?UUAW*&Y5TV+_8@_F%13<(A2!8 3F(0_=6,HH''\LIB3HD?I@H-5HJ([MI0,DL-GI59!@XZ-DH"5@H-&HMRI]!0RD60\U"H)X\ )(]\4UH!P!4.''X\ N!5G\I6/_M2/''M!S HZJGYZE@DVK?]<&= 7 ,IBK.E74:M>8XP<S"%_^7Q5NH\J:7TPQX(Y&+YL\H@M''8XBS%]Q"6LI/$X!V$L#(7HH=YY1$(HL+9@L53@-&HPHS[DH-!EP/0DKD=P,>GPH_7HH#=HHR&XL27@H4BAP,@HHX*DE*X\Z^7HH2#I$-DAP''J V"2XU)''@$  QL^^U-_"\TWQE+/=HI&(P1N"YOH]P<+HDSL>IQ2?HD1\@OFDTD#FDL80@G?(6#EG<1ANQ3G[@!D _5CRX#BGO0IH+GB?["BU:GA_)SD*;1RRYY_*;@J+HRDR*9J+IQ$5ATPL50P191FE#RH7I"TX>S_Z11KTQBWN J%,;PC2@  @W8"M&5K@WSLD>K^X8TKV?A]=T!FL.KCSF#COCCBF''PCL% B@HCCSS3ID%6IX8"HJ>Z"1GQ X4"L$*1QK=P@$4PI:" FO/GKKA1^D(2Q%]"BAD@FTDXFGM5U1,"QM35!G+ FW5GKH=BFT?3Q[#2C,9F#HB1J)8%OP;*RMSA$I9!AI0AJH/BCMZ#A7O%CLBRB8$!P(RSBO10]Z#JC]''PBL0@CM7RJ>H2JHRZP?:XA$2EZ&E31I"A&W]ZQB**,A9?PP%0DW3<D@S]EQRUV"3K9ACMIYUAJI8^(7^F4BOP U""FP3F A_BIGD:-!-/DQU3(QYGU5C 82S#R 0_DP0K<0!) P#Z4 @"P3&NL&^<DWD/,9VP8T_T45E &3N6PI\U]DQNDDS(.TRZTP1900,ETT^)4(LHX8[[)YO.0AD3$ T7N4G^>QY-\8%''X(>=QBPUT7F 49U7,1@O<@SM8D/?<PR.H#3&DUUJU03>D@3. @UOQ@!0,7XA4  &$C3-@@A*H@!($A4O<@QF@0&/RP"N(@=O=PV<&64H4H#C95E"Y74%F''YY*:Z"P!7$0@?> @@H14HI]9]QMV.ER;NQ"TY-OY--40"&G]E.MFYZEFBV[Q <Z0D!7M 8S.$U\7MAZBB(+>)E0EPL1_LHZ]@@KKHHL?D@%VDN?3HK+%ESD*L:[=\KBOL1Z>(#-NLZXIXE*.TI%>H UO\KCMPLX?X.S(MND1-!_E >C1@QK/LU84,!!,$#/_Q5S*E0^6H![-DW7@@DDJ@QBIH^@8N#QXN[QDX79ALNA2TK:0DD1PLMA?DF5%(LZALKQOPH+"@OF[XH.2I5D)(6)UN%ANI69Q%6:&(R:(&.;0..;2*.W3"-G0DDL*D\''OFK0-TP1UM01:^$@\(UQ.E"\F.1&UF]QD&C?M%7''S20N=@A@F]1I(34NOMYENPF@Y_$ND1#BB["AH1@CH[#SH/3BK(QBB>R@H4QCNZCN/WF*ZQ!(/JRN;"0IV^I''J3IADKQCE6EQ\O$J!J*QST0*0/F,*;;*S^2DR W&<[RD)^%HWG!("-S#O_XQ"ZILYYQI2A$TJ2SGPA3G]DBX^E!C-R9D#$KR3 2GOJ2(4BS]H:CA#(X),#;B J@A6%*CBR!GN@3"_^3-G>;-70I.8@+.8AJM-[JC+#R[GM''BSG"CW$%[&4K.TFCH&19,9X[H(54''.EES06X.X;&T-0!Y *B[:KH"X>PBD4@H"]AC@20BB3BBB21BA''#AF9BH?9X- <4F''EH8$VHD*N9,Z.=.[D9 KY-IYQ9TE9/]E*,2 P,F;7RBE;*987"I7OQ6ZH$,3 (]"6K^W5K !#_8")8 @ +LP/^P@9TJQB]A 7@H1A>H@''W(K_> 1:S\JMNEZR-4D''V8JH2&3:C #JV@3T*.)D&L)J)@3T-2:YX"<@E+ZY"FR"J @?45S,_DRNMBAWO.W;W%024( *QY[ ^#UMSZSP"G1]-5K#UU63[%Q.:)<A7)7R2T)5I< R.00Q3 @SC8@"MT@P''D@0# @#G$"3W02:B!3*ZVCLL4+0[^AOOJ)XQB1.,-TSHF[[>()]F^W_]\SC-&ZG%E8/R:! #TH?>J_LE,[N>\:NNF$(Y)FD _%DD-5HH+^DL-%@E-.@HQDDD[47D&5H@7.LH\YDH&7HH+GLH-"LM>-WD&;M\\5DH&5HH79OD"Y0H]^4O<BIT+,KEP5XIPV_H\D7H&V3H]^3H%43D(]?H("7H)W3H)'';H)^?H$5<H_L3IO=N''1!N<W7HHX(MR!<\PJ%XUQMALU^;CA$"VIFEZ]T,#XX\,%E*VN4Q7JAB*P7XW2BF(^UEYM2T,/?DLYDLLJ3LB:?DH7$L@(<@@=D@OKN($YS\UXT VS!IESDJ=#XEY7GZ2XUHXDIFLPI@D>HR#VN(9B!\L_IEMWKD:\FAL>="(:9(DD$N+?J2!TCQBCU##4/R#TK#(41 E@/"!TQC/4K%H4>6 EQT,4Q?N"P8/4RIM4RY/4RZM4R*-4RE\4LFQL[<2QKPRBH]3BFL_KS*#ATA@3MXT_JB@1Y31(]O82,1 !0421ST1SO;^HO3<,05(''07K.=R:31( K+03/<HIB)CPFW6JFO_SBE1P@@C!@E>QBOO!BM5@BLW @W3ST*(HIVTYS79WLWD?''0% FM]QBI0SAL(R\RO$AKNB^)WV(M8!BLR#N]''JEFZAAN[!BI/BNA)I(H0QB''%Q^O<BBK_0Y]8+BE0QB''^+X"W1AL_!DLX 6F)B6S-^)XW.EXXWCMG3E]''J''7L5]?3BP]#?W-&3[=-3A=!?P]&33@''].)1''$]#C ]''G_=''GGM''K''=''H[]7K7)A''$8IP,@3Y =)O<PQ.''I5ML2PLC0W[>@Q1<0Q^0*F 3@1?L0P YMU77#,4>M ^''-5R>]72;=72?-U+ZM<K\=>?&M>;"]7?+-7>#\1=!U$N91!\X@&.?]#@(U#!8,VK91OU6AV.+M @(057Q4HW''2_R"!C''0B6YI8FLD@2@O BD8P1[02A0H@P(J=UA+F5C4@(R0@&L\!FSL.B5XUR;^.E[$]C!DFF=C@;DD%CH0@X/ZAO+XAL5P0A0P R"(C$K NAC[0-0E0#JX-%,XM!LZ=%(LQVU,^_<^!O]K&OY\FC[$WK$-?DP.''+&Y!0NZ+;&Z,?&[.;&Y(8D-ILEPSL%/60R^=8J^<;$P;+&_=7&^@?* B;+I&E$/EHLY#EQ(O58 #M$,PNPVRQ(Q30H:IM>J!/X_2KX?E8L17DH56ML:)<8R9RHW$S)>R$2)=;\U2#^+43]</;(Q97_%;$8+K8:&O?#""CY,@8U*<;))56%(=7(8,F#\,DSFG@<>/LW-/@U$,@P>/L@\PDO+VTYKWFO*&LD!JD,8U@L=3D9P>3JK&9@-]MH,[@H@C@HJ=HN"/E$''%LL,KHM2NO)>A@K;PHM!PXLZLFP"P@M1,0IMIHF&X0ZKL X3EO[?1S5B[;#;L/QC4<2QN]@BN-JBF+RDO? CN(JAO;"D1U^<O?!AG#0@FJB#O>CA2KOBX5)<DNPAG#1@Y_ CJ9!<3N_A2<,<3\=<2=,B2;,<J80WG?AA[? <4O=<O0P=4P==4RO=4R.=4CL=A_AA@C@<DH@AB"@BG51<K>@AI9!AG)ACCL4BG7BBY_AAO$#F6J\N\;&LLY0@,R/BBW37BR BG@QTMS"I34=IO)A=K>B=S^2=7)L=D?Q=O/AA) HADF@FJ;BBSRB>8"]>K22>83_>84->9EL>85,>9E>>9J> 86<>J?"CS[B<SZ A;_SB:H,>:Y->:ZO>:)=>::.>::]>?>1C@6N(P[''T/$50"$6TP3$82]OTF0BX@SO((ZYO?G][OA000?D''/3< ??G? \V3:@O @X=B_C"PBA08.+,G0#FD5YR[19<Y13J\@@@<@FXH_B>0P0ZB 99:0260PTW30:RK>=''1B3\,PC%L6T( #9;DT\\D@SD@1B48W=C@XXX&7A>E_<+U<L[D%"4&DF7!:7T1DHAM2>39^SX+S99%-/(=:=\OX+<<?XH<B6HO9D.PHV_Y2#L+9+L<KXO =NLSY$6RP$=FKD*2''=F$RIUF''KV4G0*[?V(20U''1J+:,V6-*;\(U:=^/_+B"-K(,YJA^RO.M;KT,7J5FZ27&.Q#2[/</_I9.4THSCL6W\NDBC?;BZT:5VQ\''V&RB35Z/19D''R8Y<M;I^2T0$Q<Q''E]GDVZEA"2X=6''S*4*-Q,3;=^"J"V[E''.98XRB4S7II7:<;]F?!/8[2G>2Y>7O_-PK^OPS1&I''J0\A\A@CC3A1(4!M&?&HFV2D21QNCE$1?O2<19<]BJ_UF#1%B-N^E*0!DP<Q<\B_[X<_OC''95Y]@D@BK6XXRXOI/++1P 4YN''EDG*HHDXV^7)9Y#OIK-*P008=?ACDDDT\$\PRSS0Q1QPY49@[@J2YYRSY@+G%FQQ0./EF@H!A:H</BDH(GDOPJ@^@SDB4*A\3@N!#&RBRV &''OMS?NLFMZ$AQ11MGPGEDGR:=5MHQK[_T,,,./R0S%B.9]NOLM+/\<$4/8PP33#J?MCOLOOW\$<<>?_13S3OM-OJOPE@":ZY^&I''#% 7;VUECCQ,A0I2C0/''QQ3SPLHPS@MSIR<UHNPLQ((,"L#V3T=]J%UUTWU75UUU%[QWVV/MHM]Q\]^60H%O-"#ROV>[8 3BBA-L4HVJSIT2!0QSR%E!(?= THSQ.T^Z#OKK(A1D''.[W%)8#B]PP@ZC"S@D%[$%#QG%V*H>X^B2D]]]]:;[4W770?#J0Y@M!AJ8 ,[@EI)Y-@F"*O:(@L#L]"N0F #@0%/&"YF/*(BZR''[(WED0A*?: NY@A.*^:V$TL>F^VTPSYY9YY]_!''&&FVN.XX9*(O&J$R2BFJYTU@@8H5[IY:HL\:(@TBMO8*)]+A#00$@@E@2?J3(*(\^%V M,<9Z7:9MQOH2/SHK&>213[:([KSOE%---BU CD%:N<1:E)DSJ*XXY''+<P&<_ERH(&A99ECI0/2<-9.8/? A@FR"],._V)53J@(WI*0D DU8''(&0\];LYY@8/DIBWL::=O!755N<-''X%K[/''WG"]/&!4&6V[9K8"Q%4[#C2F''UQ2MQ9[<$E8%/^$GE(M!B"JOS^YP19 0(-_%F''KBBHNS:\''Q!Q1#]IE>^>>%''=:X;WT1A/7/1?>OG''767W=??_#[/19=>-N?G/?<=]>_??;=15<U9UM_H6;AB);]2 =IB@HS8&H$.W&HL9MR@6BP)Z%*\P(@[-CW5#[4PMU=DHP!%D15#GV)!A@FHQP<U *Q!\I(K\2BZB RB!Q8D)_XDD(!(T(^5HD41$#DBA^A1TUJ(BL<#BHR=@@@I]ZEMQD>DX("QEJ?2#D;%.1,E+GCBRH>L)*Q@X<0X^R]HQ;V!1DE8![^DL(2DHDM9CDADK_ 02LRXP480DDT5&AFG.4H!4:0(1OVD@T Y_FHO8*"GNU A3/JDPL8FIH__6AFI1;1BF-T4)JX-@X?KG%I_%#C$:B,)B ;N\+?R;K#$(= Q2UW2T)L-%JU+9R%J6$Y25+B\)VW-@X 4PD@_?Q"V4=9R#=$V@@O3*4W @B@OG W)FLI!#BC.LV''H-T544TQ&=$\$QL9MKHVZ *ZJE3!NLTYQ#D:;P,@NM;,Y/FR''/0DJC_*HSP81A%.=RHI;*(DE-(0#A%DPP1^F@BG$JQM 19TU9\@0BN"%KF[^LP#;]R"0)1V+V*U,XLY/\ 1@BBFK@YAM$DI0*S8,L!U\+H\($ DF"8)B3"T80.]RDP.&\FOQ;K#PI\T!Q*,(YADEMJR N1OTFEYR48> )MIQ^)R%])T)#;U*UFE:%R%*-QX+)H]?SHG%A)*B1&Z?;EDB#WGB#\%S#Q@;Q8HU^-Z(<"D:)0S!QT<I=O(>$6KD*YHV.0HSFCB3*CH!%2YV5F*=*@#]7R!F>M(P31J@P<@/BH/33 &V2&+SW+5":D4V\%MN#JZKO:''[ BXZ;DHX04@EBAD''L%HI/3 3(2A)DT%_PP\S(&F&_;AF#K=04?YPU-Q]LJO I3$J)''1T#Q@(10J^PP3"L.LQ8"BFX%0+$4EHH+Y".JN6LU#]+&;W^=*E;3]C^=71U-^<''8W.:HPAR]Q*\)>;XB+$ENICH5TH*4.+I2BFTSTJ--_?=*+]M:4(FC.R-_QL&6\B@E^$S#[$9/HIH,R;P$"2DUORM$%AO=^HHXERMBFRL  E>$XAR8&\P-P&JD3&/''/"+N)THX"8"ZHZ&^6IA1Z?AXX(9B*I$\3DVG[1R8OB VBJ5]IRVX E07?^DP8N$D,VRS"B;S803<:L\%GV#H\%(QCH!SY"WK@(QVI6KH AQ''HS%QJE)5H,9(;0VX7+1''N[X;3&>U\Y3+_^\99[''M.@8%IU"*TE".Q"XN?Z"J1$-T0E(QZME#\ZD^OBA,TK[BAJW5 ,5X++4G8BT1@:<;?_O(YEY9WK<!QGTI$(0!2HLD"9$DBWE1@00APQB<(TMAG7=*ZF;+CP,O%4L=>%*=>^B.%BV1Z5H)(-]6J<D5 +E@>GEVU0 W?K!)44P!"5J@V3WADPS(QC^C*L\6"B@\4Z,BOJ#=B2^38P2S! EO_*''/I*(QC_. =[7/WF=?75''^>>[5/_?^;7>4&,#I)DZVW[IZ>I.(WL<UH8CC&H8N8%/"#&S@20 !I0YZ.]@/K^Z$^EP&4C8;]W''D7"8=P.E0Y/UTOO^BEB%2@AWVHQ3,RL@X/^J@:8,CZYB_^\1V96NP1U,&LH>)Z",;5(,OSL[58#ION6 8QIFW%G8VZ66!0X89MC(L20$AIU:SY$L,M9B(U>X]J4+[H*S1%H-Q[C#QP4,=1%?/\:U97.=?=;"Z-)B T:(>O2.X#W%T:"P9]S ,J*UM)=_''"?2''+U AXDMD[O;A]3U($%4@=0#792J_[FU ]O6(I/I"GC;30B6$,0 T$6LBFPWJL2O2J<[D74VTW6%^B$^S)0@;- X-=6-S6X+T9.Y5ON@B@N^92%9[<P2^B$P$5O@HZSN:DO9RQ7HU"U0B]2L<_N?FEQ9SC.Z$$[/K%''T%AX/V*:T_?>-W__/Z?7?71!?=RTY%UW4[)0_L-$,H)-SBG(1C")D[6A+B-ILO"A 2E(D''2CB3AOH:>P"9#YLI6'' F";DGTU&RH?J@W6,DW/F@JD(@E$JDJE D^/F@Z$,@"XH0@U3B,E**]!N;AZL3(G*>"3B+G*('')""R$M.5FX .+T/>I.]P@C(!@DN@ FFSJ>1PI@F2FFA;AGCHAEH" DZ2!E,*!D>Y@DYQ!B\''A.ZHKBE2ABH" 5K9/>X!L?,8P?-HPC]^P?SBI;0B@E#1B^TB"4E)0+LPHVW:$T1"MA_/0 11O+&AH89@NC2,/D53"]#"-H38B)IY-E"9PUCX$GSQ@B%" @:*@A[+!A;3 CHB%E1CAC?6P=%8LA''=-+39+6NZ*=88-QHIO@!7,FY2M%UI)$HX$F&*AEZ@!#5+AF*ALCPC 21;!C;J-D5"AGPC C4PA@I)A)/J!A/8!DQ:AE+3AG1JAD8P%GDA)$]"PF=WPF;,1?-0KC"''0I%"BC/_O/)[?:Z;""#@ S(MBDQ:-:P@I+C@V\KRZ!.LTI95XB2X($ADU,\Y&XY82J&.R0A:<@@YTPA)> PU\H@]<@@SV)Y+"LQPU:$VXI2&FC*K>,R^N+*8H80[)IP_9\_ (LGYZQAA(*[_>JK\:1Q.8 QLB(A0R YI*H\/4BF%TRQ%3B0@^PL4VI=(.HP20KA).P[)$:Q.SD!2U$"%_RY''<0VA$@>DF[4PJS<FXA/D@POD($"-7I]I(TA@C\Q@!;=K0J!M"Q3Y>H*S\*[O:B!H5*!]&0@,$XQQTHA94P@86TPB:L!S)Y]\^PP9+X. (4KMNLQW)Z!U?K? Z+A?=0M''V#9FXPP7P?8@YEH$ON@D]Q XHTJ$F/D2U9 @NX,BR?DTYHP@Z6&6!%,<[(/F''8LA_S*$)X7L)Y1O=>@F+@D73+$K?:(-D;*.":ADM@)@/!1ME@.31DAC2;GD]3\(AFP0''?CE#M 6"7N$UT*8U@R@[_"D^ED@N_&DU/D@@^H88FP?(_N6!YGB"ZI@P76;0QGIC^L16_.J!<$C*X(&SA,&R#(,![$(W"FB&W@PM@J$J;Z SZ*F''ABK^["FO(JDO5N@K%H0UZ"A8V$$6J9P60YFU>"WPX$\>AZXNBX=RH*>B=''@<R9QD@KDP1UH9T1SS3+HQN^6#=N+W7%KG#(DXOD@N9N@.<=HK?/?AQ@]0EF^BXC!KN(O-LA..-G0O6X+D9NP3X6KQ>M"K$''I+$S(AC8;A7B8)$F8!PF>AF\1AEO8A@LIT%\ZTRJJKGQI!SE\I\23I.P:AI\7PP._4P-M0%9:R6ZHDA_H 8]JQ8R8H!XSDGW>TTBG(@CGN!EY4N_O187*,9O3*,=#RC5;@N$E$A A@D+RSN;4S@LJ3T@\P:BAGLFN0:MK31&20O^FR(30*)BPJID#*A5'')$VQ!D80!.RC@HL)AFZBAB)$K#0B@$!Y)SE5S#1K!C>Z GB;IDQP!.Q9ADYB&B>%TV..4?Y[*CPLMIO)J?<@JQL4AHSBNKMD <S:UW@FQ<,[?D.''B,$W3XOM*A2]\J2!"!4X!1TY15OQ4XAS*0P., E1=3"=]$B1<K_\LT;RHKT%YDXJ@#4$]3KM^H+Y&\^?D3E$7PQE8=QD8(Q''8@K]* P?$@_)\ [%"HAFH0@*[\K&DI13 0A3LPQ%X(Q38 @"<P[7Z#E*''5VYU<"''_JT+4K6KR\Z2&1ZK*TS#;5TRML:;(TPGQET"X\<EX:3%[*4&_C%8GL$QF@UN5L19> UL=M_[$Q#1C9V,MB.!(P,YZH IM-PX+JBQUE^S"]]AZYL!,BYAXBQ+8XAL2XP;&@AAXPYADHQ)\@PBN,AZPB08V90?X8Q[L3#ZM,''A[ QVT8Q[:?0@T EW9-/E&K7\66T-CF=L^A(YON<+P>"=PXT!\@X@O"]YD/5IEEWW2COG2H&2/FKX- 6A^&\8KL"EGER@^^MQG<\U+-TY+/AX[ G^2.H$ N:!TI/JAI(I(R&_)+*&^"E]D(O]DQ-G6U K7"!PUB5XUC?ZLL&446CHOX(.=(&6QA.&%&HGO4B6($#UM+X,]"(G;UD%KY6.))%R''5D@\8$7<Z-Y?:_RU>.TA5EIH<6@Z4KD7%:&":+D]H095"7X^@5U)B9F\OF8_N>5,H60#?6M^NVPT["D[9D@GE-H;0SM,#=]9''U]T&I^E*ZZCO(NEX5"F]T0/WK 3;@H"&+^ &O=WH&R8^U%''H%]$H-*AB[BA^T''%"G\E:I!''UH&NH64,[YNNV=53(=+6JTX"C3RP NZ(GB3)#3C)*B9I$'';+$&I %PS.G7+QSU$2A(I0-03!DX!@E?H(@K#!ALY.$KAJ#7W2D\2X#24W_+D*$O=X$JNM$@>YGPX9$P59?V33.[RJBVHLKZS"P:-R]IO3J,_5 T/TWOD0NYVV<"((44:Q+1#616"7T.ERRRR!N><2W?]5=$*EL3KCJ##DI''8EISQ$LW)AW/I"%3MCP7BX&4HC&C%CZGX8!44EL)@D=''9%%3MDH#HDZ4@Q>G(!PP#2TZ3"&''-!LT88^"<B,;J(E@\V-H;?-E(R$5PT-,^X12Y&@S]&XQL@XAC,H8?1<72#+\?8@[,$"T*?[;&:&L&V;<+B@Z^^C0#DPVRZPQ]:*)N,@PI@:ZD#N+]@BZHC2ZEA2ZH#N*L1F*H]^*L''>*H!8JMG.*L/6++ #!6T"P=NX!% 9I[;MHG/<I3#J##9Z9NG46 O#3%Y-0F[=#1[R:? 2Q=? &)AIBN2@QO" XQW(UN=^UQ (T"H@PC$(0?JXSX.()--XP8V02;TX''!;XX#88BH^YSN^^X!PIW&-.Y(,8/VX I(?TVJNPX#.H ?"Y)XSA_[\^*3=HP!T[I&9&TOH>*(/8*5S1BI_LBOO<60;T#4?$#6''?9!-3=HOXLD&JIBH&<DU@"BW]@&V#**PN:%Y8^696&//$H>'' $FU3H9JKS*%5@.O0 .;?"F=X''.;ZC.=;N66<2N7Z1.7-4.7_Y.7YW.6;<"Q=< Z@JDV3NG5<"A9-#U4/YT]<X-D[9(8TSR:UUS#GK-EH<)]\V_$BC OZ'']C+IZU9TA7^[]:5:HW@@@EUH0Y.BD_,%%:^8F,<XF,[XDH[FFHG(VVZ4J=_<T&UB3@#U$2MOC5E NXRTJ=58J,5<H.UHU^6A*.55,3R(T2GLT1E&LY0E)E?#V3R)D0RU''X.A\1/W]IVR/0MLXV@*E_L&DS1DDYM*DQ1FDSZ-3F[?3EZ_?<1Q.AF)RAF+!AFSK!D)S!CWY\GJ!!D2:AF6*!DQ(!D;"A16E\FF[<1YTA1,W!CZ3<2*6\2;_\2;,\3K=\3LN\3L_<2+N<1,TA1.LY@E !DFI,UFE:QA[NZSBN0K9 /=:Q./&28(8SZR]8TY]VG9&49H[Z:T(5BJ+S0$C$ ;L3G+J6!K''6QD,E@I+7T_9@GC($&5LXN*09L B &(G/0.<ZH2Z<E3QP+PD<L6BO&)% 04^%U8 &J]RZM3[''[Z+FE @@&CT$L0Y[L>+Z"GL#UHCNC=#))1%Q6S02Q]^6Q/LJIF0GI8(!CNP#YC8&Y(S%X43F6&6&E B@BJYZYN[ X;;?_VUD)!Z2WVQD)&SV'']7[7]7_G];#W];'''']7S''VQ. P4R PK:HP-B0''L+VTSDJ& =VYO7/B.?D-CQ-R3)"54?3V6U;QP_LYVAM4$0]QS.]QP6,W\''?R9@GX\Y(5G:@PBF\@1:(QY&FQ I:A[J P''H7QTPPRI"''QXZXP:(PP4X01TR0QVH(QY.''-Y76TW:8A[^8ADX 1T&!P"Z [H/(!O"JALRXP8$ @&N ^=*PQ1DXRITP$MF9!ZH T%,@QG:8QYP#@%PX@9F)QN<0QT.P[BJ\4L0J3@7:=]ZER[L&[QXR$&IQ95[J0-F89[3(A!"0A*R=_-@^*H_F.8>BZL;0]2V?4.''T"&%D$D@.,0ZSDB&5 .$20DB4 ;1OQ*$KS*PQI*#Q_?0P]?3R7?4SY?4P5?5V5?<<, J!8 I ,@&''NPY9E1D-L))8,(PF% @CS8^#SNF:KG7VS\,&=M)E1-6IR16JGWQO>R(M[T;/5OR->&*PQ7,;>HOM,D.9,@Q%.DK\!60Z>DN4J@W(JEQD.[5!O Q/HDY,@DML,DZI&HY9](\#$]CII0HWHF! *DV7!0 5M0)5(.I&6R=?O111Z3_H0@@B*KIUH9INBJ"^&'',%Z] QE/(2&$4],,_$5;V5CDI<(#H''5:2"C22-[F&3Y,ZK0E8ML-^''%&6>/4L<,3O,5%A;O<E&PTQS[!0S*MF;PR 3<ZSF+G6N X T<=>QO7LF!.$'':5Y^](E2_N395"1,5C4H?,,#35, _+5:)\W![5%Y]_:,PWK%)>U\?L,,=V.''523RRO;QJ,T4UF;RW="-$09\6_N&B]//*1T]F''M%T%GQ+Q,FZIY"L8&&RV4Y1A[ZJ+"1J''SWMQ0W:R"P\LIPKS]2IL+W<:<._O''4JMK''7:5JUR(0*??7*9=.5O-WY<%GV/O#364RE>W!>4H@KR[V(<Q<2AGS#0]\''98>^_<)@P&\<22TS-0,IDHD42E,=D-I3EUSA8''DTESQB\!T%@/-%CSBQOK=FNNH1O>4UD/DVWU4T''?@@AAT2=*UFNKJ<_L5,,2+)2D#"0E6PKMKQ!V@46C"Z#T$X@\R] KJ6DT1@44&914@!847YDH!M"TP0Y4V-61422C(__S^[BEVQ9D6&''7AUSV@EB@\(E4Y]QZX>U1U!8(HH!BTT^!-QQZ&-''RU!9KK\NDK\/H2^^]^Y!U*B57K#LK!7JV)]\,EL0F%5!0#_WV))%*^"&W&''+JZZ"[ ))))9"B**(-_$* JU!Y@MVOTG''$%$%33P@ 35OCNWW&]_$@@@)51AY+;KGHI(/%KPA@Y\!00IWI7[S_X\_+E7=<4]U+"DP6R5D=&[_TTTF<8E92L0@ 2R?1JBCGK:,@HHAV?<(1XZ<-@K B@KL@@LJLQ/"8H"BJV@D  TY9TO"QQ(=*9@*DN]YBLLJ7@ETSO"P6OJ 7MFDCSRB [EK"FAR02$SBSNB##E9:4Y!UPR"644,'':OQRCCULYEHLD86<%OFH?_@8''T:OFOUSTDO!"YQRS@G0''W]PTVWU25Q/AX@X[96''''''%.%Z]5.N^E3_WVXXL-I-]"_>5556N[''_[YYZ=MM--*(6767F>+S[][7GKI-EI\#:_-+\35=).50ZDQ@@C7JN/889AGK''%VSTG[:;SRY/:4UEA5UQ*&:6$6E+]A-O\^_A+IQ=</4. 0R#5^VMD_S_ @\AKF/P!(R2B=7LI;DO?87FHK1+_ 8:\-\4SHD\0H@0AKU/!2%G1V!6Q%;4X_)QTDD_6D@4@3''+B2SDRHUH11D$G\4%DM@L01A0C^6I8G1&2Q2ED^MR+BRR?>.MDKF4R"DLXBTP,,[RQW#3"OY,3R#?WL#T3SN!LZ4+R&7Y2DJ9%P3=KF9P]/E\VCGVSZA4TXP :B<HP$QJDIT<#BE[+0V2VD(P)%6DK43EA)O>&V[TI''J50A0A2<P(LDKS\HXT7."D!L(!J/T!A&XP\M! ""9*[8-N,@90>66!LN7RH.=M#C]L$Y![+XEX=_0D-^=DKN/Q2F"IXA#B[84$*I;M\<#PB@]?T#$#^N<YM^8JO?C8>J"OEHYH.SWL<##?ICK0K!"%9,8!GO:<TWG-ZK@."%HHD0$#^^U<!%6BR@VN''CG?*0$%G\0!CJJD ?%FF1QSY2ZE.BD=JP(#Q/VT]ZT=EM5^KSE[JD:Y^/BQOW C$:/1VS&LLL)#JMRT1! &&Y3#3&L)L)SVYB<3WTQNY9.FX^,Z3''&=8TB;"J\);1SJL+3SF\9):5.NL,<Y70#B^1,ED91E$K\=PJH.],=[%-6,X(05PJFF4RG2=$(#8J"D]>=,N\P]DDPXK$"E[,%[1#XL!64&NB!PC4$0I"SJLX2 L N(D0V##B$;UX$R4J^AKF=@H[S@A@QQYY#''7 221L?9A@&#!R#Y&"XJ^=<HP5LCP#Y^A.TA^%''2]N4H2N6NLR[%CDR_I@#PM!*AM7<FM?C+ %QA2-M.@,F0R%ITDJ*''FQ/V2 %=BCI:N,=R]-/\1Z8?)VFF9Q]GPUGU0AJ%^<<+V.^Y7+W?U*U;\RM+B"<1[S/M#A0_%0U4<\H%RJ:C=9V/Z2&HVOD:G8QL-Q,S.<,.HUA>^5)^PA(F@*SVQ^\R;$&CH[\-BAML28B"?L*:D]4X/0B($!7AF/I,R FEE#:$\"GHL M&FFM3(A"7B8X*"D-A]1CR%]BPA @WC(!R#DPI@;A@AA5&C_RW!Q@F/X8 /. =@_CLPDY#P"DTSZ2O<,>%@L@]EBU?Z BQA. P^-?,D%-$ DF;HKR6, IUF3MN0F&5[E_T*M"UW#B-[RP99"T/OB3;R&!#E,8P13NI( 1*ZGQ=3!D(]802PF<]*>%AR1ZJZG!_,!9;*#NLY%=,X8!&]L'']X+Z746''5J4WE_B4#V2_B9\,1!(S[Q"A''V=:3:/"553+&\/6?'')^+/KW_G6X+<ILP<@^-$A@@8!H@#=!@&2.LP-1BDRB<7!_"QBAE[.EUM?D@D@''%APK?:@C(!0(11/E!@3]MJGQL1QENJX02YL<#K)O.CN6@ GF7"''$_FYXW&6X@\5>/VHR69UH5(*V%5$AU]#[&6,62&+&,;?"$E.#''A\B2ZWBFMMZ0?V^-Z63#V.]05+W_^Z5=>:=Z>GGV1_6;B\93$J513K''E0A<S(R#JIS #V,GE/;6(839FY9I^5_8QNWP[:"+]S#+R?9SSI>TD]+]=N&[F@"G+L=866[8>$U''XSR@FO"Q9&''4.,)Y''$Z<QO3-CJVWU8H@I9$ %88QCUC''+$WA9//<!H%4X7X&</448!Y\F\SN^T;#\8!V)3V:(_DL.6VTDOC 05>0Z*LOF9E!+''LX4;3&].<9#"?.\933/N]>;1+R0F\Z&_A;NV(<7KPB$917H''-)#.]N#-NN-I?CM**Q2T<KSX&8OHT3F\(F\J=DJL$DJ)P??7093%>0%!<DWZ!DUW-YY:<$OT.W!L8G!K$E0*47B?4TZ6HH1<]XT\3N!G_"Q*R(F?W*--1M2J<I4\+BNPSV,AZ3KV!6$1(V#W*V%8@-/!2JZAG".!CS?+Q&;;4*C>=:%OO>-V;//V0_;7,T7>T["G[KTUW#+OQ DWMAZ.2S0>><I_C!L5:=-/87M25,-UKE=LP[JXU";)O-9/W$)F6-''V<\$:4DP/!!E9X(X''[==;1-:\,NV=&NLA708365FHSZ(@X0K7_4N>##/;PD[%WRT5K-=+RZZG%XK*D^F*WK11BF;NR @*8 @3X @;8 A@X QH8 QQX Q[H KNPA_6  _=3 QRA @Z7\B.<%Q0:@U%PMA3O4"/E4S#C58H.VAO4AHAN$XH)RGW\\T=W%4GOHCY.(U!.$P]_]2EX4VRRL@*,83)&=8KO\W_RYTG8<A??LV_$!6MY,!M"LR]^9C]6\7%Q(V(U-FP0P1!U,Q!,TXY&^HY(&HY*.HY,6HY.>HY0FH]2*HYA@G+"TQ]20 13P@Q !1N;E6QRPV5JNH!O)64<]#''7YHMV]C#X$$U\H!%YU1:JEXP64V;,8";0$''6DRG3T%UNNM2#6  ?R5XSAI7J#M$-0H&-K WJ )WHC"G HT ,@,@%D$@&6^H.8&H.:.H.<6H.>>H/@FH3BNH3D2H/?7.@M&XBL2. M2 @@2$@XQ/]C N ,1FD\&7"M5;Y#(OT,"#!#ORY$&S@Y*RUN*NUM)[M.XJ\:=WD_B7U66F!A!YQSTV!AD<UD!9]#$K\SB#Q9P[EMW,L5W@ UW&!AGGDQDLD/DIFPB+&PCM&PC/&PDA&QD#&QEE&QE''&Q-7@I8Y\\.RHO/?D+7"ZH;3"R%5U<!2!-76"CC_XT(>T&,EE.1*Q :XZNM9D.:=H.;1H/<4ZRR1ZE(*!=9@]>)Q!KW3TTI+^J@@!.+# 52QDH,1@HY!@HT#&UUE&UU''&UVI&UV+&UWM&UW/&UXM&UEC@J13@+JL@;J4JB4-!YRK^BOO&V?><T]T?T""(Y O_TEW&PI6.#V/;8KYP(]>)8!ED&N7A9ES=I"/PH"$A9X?''HD?,7R1$&M D)QI''7!V*4&HT9"I4(!HOD$SH6#\<"$)$9&,)""L9B#Y2E_I''#X4JDQR:)P2MTXTN!EJ1E_S !]-\''[9!I&/A1RB:(D0Z6E 34D7?5_2-)NR.GD<[C\[/I&2?H\]G8[CL8''UB1]L99''\Y"WX^(&,"W\#$8Z''(SFV7#A9QXT@\%A0''U#- 9&%T(Z$TIP7CSMI(3$LR7''&<)!TC9!4!7NZI)''?;YTL0"GH#H''S96S=''"&)$@E;LY]HESF+L0_VE4B;@%V="7$??9%"IWU??E^WKM<!,(:QQT$T(WN*I*RP/P $-(0@_V:I,$>)=.E5N7 DW9QJBG\:HR!GT"]&9?P1;''ZI-B*AED>FR.@3.D6ZKW6I"R%2#&5$5_L5ZI:AQ)0)QFN*V9P -42YJKL23MNZT$^W]6-I5SQ:OU\*KC<P^F8B[I1%[^=II?@:GH4V;/E&<:.ZU\6''P"M1&(VD,\Z#&^]RY)H*I56*KNA!WA4FMOQ@8@(@!2A96AR)*GMI'']>JS_\ZO!F@P"Y(9LH6H="#(Z<U)$)9:M>(K-2RUX>BW!IEX]"*I(<AARF*+>VZWJ!8IB!J#@9:*NB(XP4W,4N$W!%(M?,$E=6U]?.QG?M!%[YYRIE&*+02]2'')^''?+^''/I^R6AJ%2/*_N&F%&8M4><VBKE*-=5$PLQ \$T(-2"].[ H.OB"^79Q$MD$512B!&B@G+SLJ>%F$7-)4#[&OBJYVX]H7$0$-5@*F=5*XS@B+''HME_?@T1&AD@>.\N9Z0(+V+!<NZ6CH89DYBYPNI9.J#M"E6,SU[\#B''CS.TOBEK-PD''B2*_ZEB(I? [HY*VT3"2;6 8'',VR5W$H")@G6AB3L*.YFGHV&S@G1SB&]"&1NM 9&R@^REYZEHY$9[$Q16AP/6@_>IFDOX.OWLTS^D)9ME]O6QDN_?B''G''^5OF$/54*Y8Q@L3/HEW5@L?<UQ@FF C((@B*B C''U;-7Z[-7";-7+[-73;-78[.H@;.HI[.HQ;.HZ[.H";.H)K-7"+C.+ BK(PCZB B;$ZV#[[#VLZEP]*J=4D"TV6(A,[(]%0"S&)"VQ+[\A9XLLI+JZEEMX1X67;A>0 "?)2$A^Y.;*;.;3[.;;;.1YYB;]0B<A )"6+GW<0BKHH/L3[/L;;/MA[$\N;K>?##L0PC,W@CF!PC@%;_C=F))NJ-NJQV#0JNK<D!CQ9#6X -_JJ!@2U.)$5*,6ZAR3V+>PQ.=#![V#@C/D3/P-9B3T@0LM[@<PP0@E<B7L 0@A\0@*\0@]L0@X</@8<0@1<0AN<0O<Q#L@JCLDM/LDUKLD^''LDW3LDVGLH]?L@_+LD(KLH&WLH+7LDC_@,ECAF5$@#E$K5<>!QX%@=<$@>B4LO9$@<A4@QCG@ABSLQD_LQE+LQIWLQM#LQF;LQQCLULOLUKKLUU''LUX/LUT3LUW7LU ?LU"?LQ#[LU$SLYBGLQACLP<S@;F0@&C0@&\T@3AT@1O<R/#2)K Z:Y(B%BRTT1-=TW-V!O-Y*1SV5/IB+>WIWH%-5ZVZ!/E!+?Y2 3YR0GZ"27WB24I.<% 6<&\?L&^GL* OL*"WL*$_L*&''L*(/L**7L*,?L*.7L&<A1U?0@1&\@10 B5?LKS;E@:8#@[ZV:[?&E-E"3#L1%3L2D3L2''3L293L3O3L3!3M3S3M4D3M4Z3CZ*/IP<,L[H,-7C.4L&*411^>?KPT\8M.O_"4*SL_T=,:(J+H%-VXQ#M91H$^="/I0@0\0\F26SJ4@-$+"QOP02GPP#SPA%7PBD7PB''7PB97PCO7PC!7QCS7QDD7QD%7QFG7QC-7P_1@L8**+5(IEG"6#?:3QE''7RFX7RI)7RKK7RK*7RLL7R"XNH6[3K@?7O7R/N^#0-P5YN1>Y*IUR[:CIF=7GH*@/O&GV''Q<MV65QK$(2=>S0\Y-CQK@&1DI.9VI7UV+7UWM7UW/7UXA7VX#7VYI69D<,+BV/GM76C?<EQ,\FPKS(]57$,57P=57X]5;J:NVZZ27B /X,X*2)Y+(7(N^LQSNT!Z633%5"!#/IZQ%ZK5OD4*"![F15$30@) = Q3K0@3AT;4CM]5*@]6*H=6*Q]6*K-3= "2?)419/L/[5''6+@]6;H=6;C]/V$+$EAA2W>PK]!;2S,]*UH$$L2WB\R)"&_#PV9Z_VN$@6TT,& D6T&=IZJ!)&ICK!@!PVYB4)1;5Q7M#WW=7W\]7.@=7''N=''?(D54(I[SR6''>3]7.;=7/@]7?H=7?Q]7?Y=7>9]-KO,59OJ3SV*._*M-F;UM<!&3.^&3%-!TFS7B4PJ7YF]-]72''#-$8H!PN_=R,\,,&738/^D\7.D^?.D S-=Z;R/QT"53NY]NX\^4/^H,7.H.?-,=%-Z\5X"<-=&T/KHRJ:B,''Q-%(F2*NH9J 0#-$P#(H*GW9=2I;NAJ%JFDU\=.LQ[;0'',P:=7"W^W$_^UV''.UX/.UZ7.U\?.U^G.X:[]X)+"4OL1;\MGSO-J% .M!^,C+.?M!J''$S2.4CD:X?N-2?S@A1ND\8!?.^@G."BO."DW."F_."H''."JK->^S@1=\E)<R^DKI, \R<!N1(;HR*]3#"2L?I;#*2''%EJBY[NH/W.*&_.*(''.**/.*,7.)#/YIPX\^78@*0$CTUI&-(0>XB60/EJ)#5..''?R(R$>TU2C (W=''C]@0+&2";&3K;,3-;,4O;,4!;-3];^TZR=-;@I"VU;A&9C,CF:[0(@; Y/!?3\0K9DH&^2\BTX*QTD;YN03ICI+#;/=E;/='';/>I;/,$7U?I:?WTE9::E 4J_X*^M$;EJ5;7/.$2O,B=ROCQ(Y@E@K5%@L[P.6"7;1FI?1F+?1GM?1G,?1&U4CW D(,4YNK6%WBC8CD#(JB B2"J3)B$<]*:/.(*^E^L >0!.=N+?3OM?3O/?3PA?4P#?4QA?1@B@N) (6&HJ.7#R,''V+0YXPI\!;3#8N$3\*5X[LVFUJ=D\</3OK5>1K6XC?6X%?6YG?6Y)?6?6"?=&+_=&3?=&8_=7@?=7I_=7Q?=7Z_=7"?=7+_=73?=78_>H@?>FV_$F@_<^>SBZ1$FDV1(D7-1>CN[.KNK-J TKW%#%P?NZ,;^TE@/44[AG80J;PQBG9B/7M1>+RQ>*"?>*+_>*3?>*8_>;@?>;I_>;Q?>;Z_>;"?>;+_>;3?>;8_?L@??LI_?LQ??LZ_?L"??KI?> *H G3BE&ARX[^WV$N^GN)XQ(30B?A@CLGP!9%/KO%(CY''Q.''X1N$\1E6_!I2!@G(@BJGV1E/C??/L/?4QA??]/??GOE/''O??-_?0BQI4>PY0LED#P8,FBP 0,[I$SHTBED!1L%Q''18,_<"Q(,YOWXD2UG$Q)HTEQJL.O@Y09P!U6YTZSE&R9(3[\+DVSO''SY4=^_;\F]R''4HMEJSI<&QS)4#0+G]*BJ!B*''7:3+M*3N,.OOS=V.>[QF,R^6BBN@DC+%U[-6%GD=LVS=".^''A?D>BUY&5_/W+9=?_8EGE#0XLJEA3^;5V'',,5% <?!![IW!+I^N>?TC.7TVH*.\M7\F?U&4Y=J!R8<6''Q+5:-N-U[-&?U)6[M*0[\>>SY/+;*9\Y>7>/KV+Z=?E^1??#]18\.[K''R.G7#3:\>''U*U>_''-6:].2?+7;7''!2+<NC#3V/53[''*9\.TC8:]I_Z*Y*3O2GL5"7Y-D"[?Z,4@2JX[.A#)9(]JAC@,PPTWYKAAA0G+K:5&@N#D,"B22H0,^>3[[C5[''.''G%&U&VX=DD4-D<TPUT6Q1QQ][!OEEFVN$\TX[Z<S1Q!539GEGG7,D<$\! 2Q2RBNKQMKE+KHZJ0=[<.@L$XG ,8>,7:1<Q)635()P+UD@$DPNNW9AQP9SMDC0PSWWYKOMMR<A0A*+K@O+&]6B6@)EV8HH,Q<M?10+TD@GE[QPP <5MEEDE5V4TTX_]SQRRB^UUEGJF,OTLZ4X*1N2SCF%MMQJQRU5UEMKQ_UTP2&C3EO;W&4UU,Y ET" 1&2)2+.*#LOJL;CDJ6-K/_CA)1\3"IG$?8!_D)C+A0A"0J]KM:^%-%(6.9200,%,2QP%76[I8,''OP&2L)*X0\,* ]I]2ZM5364T7W''#''UU_^^.%%=59=<>W77W7=;]]] ^4M&NBJ]$KH)I[^Q\&)!!=6NFJHI9Z8X((/-#!##C_V.FNNO8;X)ZHRO.$$#P4"ZL''6SMP**5!_Y 2QHCAE1B05)FV"24BH<^@W''5&P@0]"R%D+Y6.OQ#))0+K%ECOLV+ZUL#:Y0AP3^<A2C+7^-O[.O*;M6=)+,X/;..2102[;;J;SY!---=]>F62899Z;[+O[)''/+>ZRCK;34.MZZM<FEF;100 <7OGGDE5^<\\X_]3122B\7/O<99+[6CCSI ^OL2WM+?[WI\1M-M[0<3@JF20"YRLJLNRQII19FFJFKFAC2$JBWI8-U.''__#<X6S!N3&FWDWD]/ZA%.Y5%^5$<91QS:N)>''W%O)K;V>>."5''39;;;_?/''/011^?_N3IO=?<:=_''O''7,?5S)8[DP$U%PV\WJP=UT==^?_?;?]9QSHCNP>E1-_/"+W:(>$3&0$DX>BR3T%SC%A0S\P#>=XHH-4-JOXK@C@ BP .0V,X!8''@H@^NCGE?JPL0%H:7\/!BFC.''RIV32BBPO)WEL>D0SN2H^GAX1OC;G2P1<JLX!@INHP#Y#DH1XQ"T.D8!N%:DP*J''FJU*3?X!N1.DT-])FIW81"E,D(QUC=;WC!.]I)/ U@M/+O#V6DX:H643\)XXUIR=ITG#>%Q3;.4X==#@>I\ TK,9!#K]#HP2?TLP\@@J@E1F@AH1K@@![T00^M/HT+D,&;FG[R$8VIDI3$IICJZHYC)>0JJ%&5R%V6L)V/YBT,WQ%KV,;R%+KDYR53^T-]=)JW/=1%LG4I2?D0I&WF1MRR%N$UN2;S&\>DY#R%NT5*U-NZ5<Q&M"TSF&[N+FU]4VX8(R$%V2EBWF[Y ^:"%YX 2JNQE3#EEW:1B34\XQ9MFD@%/@F@ZE@-KR;<YD@EV+R40H$]H=)T^O[4#HT6%B@NY^!C?2TZTX)B5JHS/V!EL[)Q#WX4(1?%JD =F%JR#-R$H$U)R__4HZPX9WMO\(1QJ/L$&''*.)#^5ZT91.%N]=)R''O?U)THD:UJDV%Z!G3Z&?[ETY=;CT*D=E:$>=X!TNZP$-?^&OG63AAC9H !!JRDL<D-@M@/ BEZ70@@CTLP/]XWB [8W+N+$A D_$$HBMXT=^=[)W//[U+7<E[F@EN5#BE-Z0!4U,X/%JH!Q$([G=^N1#6TN"KJ3G,\QS[FX5.5''N]-Z0 $QAO13KU<%:U+F?8QSVP@D@\3@!]6''1 5(48XMO)@@U<X!CGY3QB B0@PG=:@U08S+\8U:BFH?@Q*8DN_<''4CWW.\>E[''R%N57*U-^:5<U.]+V;W^9.E:YN\*+''.#-^<)[W/N\5[6L<)=:(RN&;:MV.YCK''A5CTH!DY;LU@]J_AINCBB9V  P7"\PL,_HHXJ<AK"M)JW@Y;D!?(@H@''#CE!\!"#0!Z^\HX5/FDN]=#CG0Y1"DT<X!JW6LP''Q''FJUU1"\%180&DH08Y]/FHZ5=#FM<Y1#''V\81X[ 1LXG(P+KF 4_F0U*;5H #,*(PD!.B@JKPC@J9;!U+\B-LEWQ-(2EGDK@CB2$U<F\9#EOFX2%=''LY4Y3&-V<Y#Z76\5/!''N\9S1''N-_Y3''_FL9"9''F\>$9''K-X@FI?OR#%;?1BDV!OA@KB)A"FR()U"6DCRVIV4-+@[CG6DXQC88$^%<<BD_''0Y5*DT=Z%JW6-R''Q''V*U[5*U+_Z5Z>F]Z1K3X%<CBH@&ZZ5LS+M!4GP&!V^I \''>NA)V,/Z6L]F]+JU+V)N_5+XP@@BI<"QC6#''H=!<@HJ5O97)Y"=;5Y/>]@C80@( ,BHPZ]''*W&CQB0/<E0BV8MJ$9S5/^-_[7 5:]BI/>L>\V_''^?6YSO;*T[''=;$ ''DR#\F?\6DNOQBD7MP19PKC''BJU=3"E8]!%1IITB)#7NNAJ_KF?8$HS$[:+_6^.E(H7P=BI?3#K8]93FTN(P %D!F:Z8_QY+93!W_<?1!F42CG+912CM(C >N@ML>U/''R&6;/_>XWJA''W^=I_''SHMR2$.DSF90/$2=K4^&^-#EO/YOF 5JZU''F.+G*][K?N4K)3$L1]N]5()^];W*ITGJC7''Z>==7/CPH:O/;P"E-P M=5?3.C>]NKYTP#D;S8Y>S%__^,F3''1%<]<9/,REVT0D+!+Q;3&76(OS0C@F=@P^^ %+W+Q-=;5N3_:L0)P"4Y>@:C=ZT_.][=;7/_^=;<G_/BEO73"E=?812\>D0!MZJ<[GXO\&@L!@B@K2[M><*?G_/X=O/T.G\LU\1@CHX"AA5&PH T)DDH9 GF@@;P"D^:G??/%G7?:3=?>=_?G??75''7?>;=???P_@?1O@@LR?@4 D=&.?@4"AU"BE@2@ET#@E@@ E^ B@U"B4#,M@;]O@C^Q@M.F]QCJCA)A@DP"ER"@FK? /X& $K5AAK0@@E4SAE8SAFYSAF(3AF:QAFG3AG]QAE>SAE[SAGO3AG!3BH"SBH;QAH?1AITSBI!3BG''QAQ/HAP-@@@J @HRBCR:J>B>3@K/SBK0R%APN.P@ EX!"@O>"FU( ER<"@CG"BP,"@U:@BIJ@DR, @N<SCN=SCONSCO_SCO SDO1SDPBSDPSSDP$SDP5SDQBQDOJ3CU5!CR= @R5@BUY AVM GE1RB-N@$:0OCS0SE2>/?#;5C 0G8!E,8 550 W"H!2)  U>P!&:( ''"X@SFIA15 QU3LQUZ<QU;T1U;\1V@D1&G<QPW QVM$QVP$Q%?4QV$P1%>D1&NT1&R\1&TTQ&WD1&+LQ&+$Q+! QS''P@S&X"7CT V;@AE>X S+(@!\X!"9( 7-0AP)Y!,LK1W*4Q>7+#1WBA''4@ E+P@US@!E7(A$[P SW8AQ6(@#%X@3&P!(Y4RK!01(ZD2H&T2H*L2H/4Q(-<2H#L2H3DRH.$RH+TRH7,RH842YI4RI@<2YT,2YY$RV''P X"LRX]4@QX @US(@%SP 6EH!5F@!W,@ @I A6281:H42-XS+#00!E.8?0UUXH@4:@D]JL\!>@TUV@M)N@^,ID!)T@D]N@\E@L./CD.0U@B1KD.2ML.4U@@UND,E<L*5OD.6UL.1)L.6[L.7;D))VD./5HF0;D.;''D/@7D.X%D.4!D.=YD/DU@CEYL3C]L3DWD.,WL2F!DUF$@YT:HYEXHEE2HU4FH\68@U^*HE[N@EOOL+SQL6X:0=?ZJPHLH@[:@YTTHE_VHQ_.L(*$@X9<HU3"@_^=L7^AL;_EL; IL;!ED_^YL''!UL;"CL;\#D"F]LZ,YL#%)D;"#D:IGL;(YL;*9D; !L&,#@-)<@ULHD#Y:XXY:HX.2@H,XHL9TH[T!L?8EC/^PXP]VO>@F-@DD\BDC= EE("G1^0F@YDCJO!H%3SP%8SH%O1HAFUP%N1H[8SP@&7P"&SE%YSP@<UP$7QF\L3MW3 GQ."@^N @E> FFVCETX"B=!2CW(@D>WSQE54:O+"EF-BEKGB@E?RAR-AQCR@FCVAAG"VFG!URK>AQH#7AH$WRH5URH07RI&UAEHQRJD5RH:WRJY7RD83RKL7RI[WRH75RK^UQHHURK"WSJ"5SIO5REC3AI-V@E/BA??JBRH DK9"CN>"OHHCQOMWS;\LFU @@U3@F:\,FI?WAF]3RI55SQEWTQD5TKWWTLU7TRFUTQW7TR)7TR47SP7UTSNUTRX5TQ?W?@RF-@B>0 1@P@1>8 1GQHJK\45Y55W''KFZI4I0CHA@C8@")CNX83S[2S/I''[5PRI 7ZH 3 X!64HT"=X4TQZ5U]%5&X%K"[@!%WM!1)()E%8N&]%DK1HB7#(KTC@H@7ZN60U57G=''XO+AQ;*AS<% "=PO&"EU!Y% !ZUU1Z%57&557+E57.557#EA$# U7<E6G U6H@%6HD]VG#UU70%2(W-AXY5VD  2''&M6G2%6HR56H)-VF#E!(7]6JCC!0%IA"O8PGH%6YIMF''O%1@O @P2D!AZ-E)]5VW-\V@5:ADG ![PP.Y 56Y7%6SWQ5%98A-7+6F"U@F0(6%?M."+C?2A?WU*0Z1M[ZBF%QU*(1RB##]+_:U]:YPK!8,R6P-*^A].]WQ4.WH,V1XZ/GR9V[YL.(S2%T]..B=.8%],E,S+]>S0W4#*!(3I=V8-0Q[_U:PUDLA*\*[:=0E,W@":?]ZE"$YZ ^;+>^KQ885TL7C"Q&=/K1U2."51XXE/F,=2"4RBT=S)A.:FZT8,UV(.QE[*=@52].3%>4=Y"JSJEF3#CG\THXR.\1]&724B5.C&]<=/LE];!;Y59QC_F24^5"B7^"YCW7U/]CZ:<0H^M6;-YJI["TU*WJ1+F;PTI6K*5B@K.!]:5LM;<:%+^DS!G456( M2=B57<4 /TI];9)_?_-TUY_JB@D* A=6RE?/"<4:V24D7Z5L6/#PL.X@@@X6@+;X67=8W[S-SU.SND?^*EX.E[)3W_+!T.V0@@I7FEW+AV<2U]>O7V>#W!D7:P2-TEX5 !VH FUX <Z\%]@D:KFPYW=U5 U6BF^\2YQAH.4<U@-(K W-T8156FQ)C\& -W9<4KWI&C8@J@RD.7=LW 7$W!J<Y"0?4''V2BB=I4YYUCV--*;QGHRS''3W884P[]48I B@''I''!3?UX<467F^9U@H[_+T4K@F@^+(W\5;I"+>/ W @@3<V SUK\T\3"QEY$W)W]W+ D\&C_APL@MRBFR7"RW/ CZ,"DO(@<1 W?A@APA!/*A<S0!FOP7W:X@29K"6M(!C$ @:8E@B@P!''M3*4BXDD>8NP;F!U+(@3U@@0@ @''[HH@BPA2I(AE#P(DX:!E;HAEMF 41X7?1*AF=  3?(!VJ(A%](!BQH-4G.!4C>5$@@!E* !''7CA&J AV<P@3VX7$U694WV(E$HA6(  ''0(!4_XHE]P!C18 F!H"6B^!W0( JA;AV/(AV" !%''P!S=  $P8 [[R-5/PM4^0!&_X@TTX1U(0A<N3A^\K 6K@A#VH!&SVAS<8@F+HA5-8!DDV9CBXAWMPAJ"[ 2\9@WN0AWLH@<]MB4\H!5%8AF?(!V!@ 33(AG?.C9X^?>PLR*Q=8@M[>HM*:H=YHD4&(HUZ^N^+/.I>V;^6X \>*HX9ZHT<G(W D OT3]4(Y"-W6K_<F(E^PHE@J@L3S.Z4B@P"NM\<:HO4IPI+_U=[:HMBA.)Y@HA.Y-=AQ(Q[NK\<@N)DR&)Q8@YGC _33Q%<*HT#,0U%2C%["N/<H HM& N<"IB<;(U@VN1^*HUKY&&,U&7:#YB_V>-^>HI["AX@DJ:$M M2.@Q%<FYA).,<A&T@.HVC42CLBN9$C(J#U+ 6]"-D2I5F(+7JY&N7R&6$K.Q[T@.VO Y/V@X0C-0.NXR-B(PUZ"QF,&+ F.P<VHCX0"++3&N6X.MQUF:4W_?->][SCC(7K#X;44:KF( -V3!-Q0ZB\@ G%/XCM(C(W"@B \"/BJE%%$ZDM9#G_+C+QE(@5@6:;\Z/#U,@7)'')H+,E8J(E81W-W/#,QJHF7JYG409WZ C!/ZU.V8@E-!JCBU:F/48KX/ ''=*[/GY_[0]V]Y(BFX+"9PU@DVC#''D? B_<"GOHXCV3"AV6"LW @EZFBBS A*(^8EO+ D.3TV7.8OQ0@BV;@FS5C/PV;''<FZBD4BCW @BS7 R%,8CNI>C?"AMV?BG!<9#ED KO;6Y5LTGX6@FEFBGSI EQ4BCPG DS6#0(>[ZVU@DM^ EXE@GA[_.X&''#3>W1SB?YCNK?GU#XAE\ !%/ @>AZA%] AWE0AFS.!T8X 4&F<5$@AF?HACL $VZX@6E@@4A&J6,5B4=XM>YI<OG%AS"9ADM *0''/!U-@!GN3[ 8F!%)(A+)N<5((, E7D$F3!VN8 5*8!GA @$S0AFJ A&XP;$D&[WYZ-79(AFH@!M2M[ 1ZY$6W];EUI<L-E&T_8@6*X@JF.+9MIA.F7KWX.E/UHCIN"=_M(M>EN4(/8:HQ.VW !-#"B70(7;[:XT\C.9/[.AC^]3R^=8?GU-^*7G?ZNG1 *?MVU-91;;THAI5#J0W?71S76%+&"??E]MUI;SBFN"X A30(783/6<![8I9K-7AM?3(B%&>PU7)P3JB<V8/\CPQ*C?* 256.5W^S+=(EC=]5>56)CU1>$=1^WZFB?>/4AW,&&@MG$C-<2A7&3X.[34B]D;&-R?*%-?,NMM\VV"G+W_C#SS(_;?!?#6B5R;^-O-9BC*8;A*TL4*C/1[.6/_/HM>GU0R<YO+ Y?$A,0@-Z9-8H@R; X):C-=. @8.N@=3E]:/ -]6:%?3V]=X^#-8U:(=%8E7*7W_YI:#$-XV[J;J@;;''C#5;#)W3B<OEM !CWQ78T;)KO%33 8%.5\O\1+NG$1U%W\M?(7_5^TOV.[W(+?(/<W''&8S_;1+=<[B++O@29,XM0V2,_!G.: 1?>**AO$+V*G2GV!E"H8-4<P?540</_?K@XHI$1:9^$EHED-PKA,=_)3BX@7M[5:6P*$3),1X+Y,3_HD@E6/Y[4FF P0)5^Y_+7B@P"G+U &I/6V*PO 2\3H"RP''<.3)T>]@! U?D"5*=B#R)D*WL&7*=B''T*EJ''T*5*=R+V+E*7\.7*=R-O [5 SPS0*!\+Q[5&=W&4+A2@"X T-Y+%RI$DI"]D=W)0X"RSOKNX.LKGQA^+W$A\$^,%:F</Q4A:4PJUD;CS''_#@\.;,>SO(4JIG$29->#Q*''40X% 7D$H5HQ@VY5LHW*I\2O41&@Q!X)!^>OG5:E S@,I0:I);B^FK?\*''\RF?=U''.S0G)'':.3Z-7O/;/4;>OC"%2J"FK]WDFJ=2"/29N4V2U,DZZ,D\L,> LF[I99\Y*-VK;_$@P@"++A&WFB=+RXP].L9>BBDDT(8HXTUY,U \\_44(<+D2$CSR](@EBPK[@A59,-9055&Q%3$LPMG8804P '' H3U"3"A3MIKKP06ZBFPPP(9IIEEF+$U@K+48(=Z/C7R"28''#\PIDKN@X&@/*/3Q#1+T#LSP# @,,5(X@@A!26J\ANTIKX+)H-ZQ\\(9I9556*'']P@KM\ ,^@B"B3226%LLM@O''LLY5 #_SQ2X"6&MFLM=2XTUAA>OS#B@B[G[M@LK40?0L@F!O%$P\:@HC21X=7*+(**:6:>.*N>B 3DZ@TD[P"PQOYP%! D6&8G$F,=QR\R&HE&)MJXU76J+OMN/,,-M/-1HQ19PDF94CEV]MKMI9,1!EE0-H:9+J++R#P[MN&F"6;;[+;K+1J;Q! *+5R1DP"7!BC3!^X9[GSTM#AR^.-N,U;LLHIJ>0.R\JR%J^-!#D!0UCVA*3+QC..6>B:B7/<L\ !R2#P+$\YA)R.GX.<L,,-.30! 7&)+A*CX\7<L,89:;33U7&RWN>2P@$<LL=EF7444$79WCMQX"5(\=IQR3755AA_V9ROQAMMM]]]^=622#]?OS[YYY-=M-))*1J=M---.?467GGKOS_]]]/=UT@@N0@a')
%
classmethod: R4PReport
imageTableCellGifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 671;
			add: #Height -> 362;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '"9J^ZJpb5J`&p@SeDfO/+ro\7oUMYO>[h6^dcAX3oE5d8`7!-d1,M[&</d!!XKo3DEcgsMoXr2njPnr
	'')C19)S/a"8jQ?).<b)=;''Keue1cdb;g>iqd*6d-l[(=2''JrIXjbTnYhjRW4jH@2NgXDDWfm-<YrR
	[hUYoL);Fh!4M?&fej<gf.H^''I#*8]"ID+r7''H]N1-OF$3[`QbhVLM,VhJ=6;5V_m$*s''CiXp9cI;
	t,''J):5N/ESdo''Dm@XPDt9''G;*R;I&;Bm,[U,<*j,-pAAH9FO\aor;->=JqOg^,[=qtgu@t-[_*:q
	/0li19MSj:g?c2\b\VTYUoLf_lKk%0Ur:-o1bq2Kip6dL%+N_TN09EIEc`9B,V1Q)9MSP4*ZWthgpFf6
	bg+GspACGPSD<E-''G_\2f%aeB>U1XI`4inTrqt:&ljO406paSccJ/O4*''?DQN/j.po(;#2r+p7mKSG
	.*+scgKm)6o^6q0T*ioH&QN-p)-$5+%P?>Rj2e.`B,o^;@IVPIa7AB39^eE6B[2DeB6<4''*@r;Q?+b1
	%+H,U>!!9Nks1$5>m''<''3?,bhhD#*ZTifN%?S4I"$IEY,k;l9FDUU>[CYT-7lgD]\)*%Pb+\``lotl
	4?Q),eE6+fSs45<Ho+#TN/ijmVQ:II''Mqh8k2u#=DKMt_[-[5IPa7OB%Li"''oE5@$N0^!plL\n1Cu"
	INeCNFQrqtR6oZZ1YS=ZOp`lp7t4P])c"9o:af$nebFPPa.h!Y).7SG''S,in+K`59G\6:BVgjQuI0Un
	XYq@V"!JoZYb51bpme;H1$/Ztsc)r;-%")C-RX1djr84?>UKg>j4l*7W]t]Xl&*b02Cd/F;s*I!U1AWi
	V_ZSCHEf9M/"&dbJ)2>T=q=]XkcJqYZSHUqFRg/2B?;ip<2(Pf1ks$k<t.3^i?&oNY^5r:^&AJqP*f,\
	1M''[''mX6lKjauPe=`K~>'));
			add: #Length -> 37527;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVMtDi>WLH7/EHPCX5cS9LZ#^*JX8E-JbeilfS`N)&Bc&$#aM#R$AVj8]DrJf@%?SIXK=Rq4u1]._s
	>&pFi>U:fd.Xl8pGWCs/=i"ErYoJZ>"hc^:iVj/D`D&''Hpbt<-L6/)\LYtksHi6s$rV<k?cT''(OZ.M
	g]J2^rH19s(G:@f7TU<DWRbs3K_(?\e1%n<+9mSET3b#N"8!7r`*&$"q&%r=sapoL+"iV`,JAe:<UCn$
	`i(+/_<OO[A.ikjh;l2821_!hC#R<#=0n_lBNVp&mKZ*T(+Kr:m\WIVnni?i06P=2W3@4J&1[30dr#O=
	Oo??^@X?o\ae6M_C/n_ok8\:u9TBJO]^:8,mc&:QKoZ&X(6FEoHflomu.qXP_oFr3_[<rn?<+]t\mC+5
	?h1rlKg8hnR@<M/g>]q3/M+kapDkl:SYfUYdI4o%36de#0A7Mj9COL=+T+PefbIXMH<KF-KN([N"%^cS
	jc!ZK/?9lu)8pK;1<`\%ZCn&,`Rr,MXJ''i_m?QR0<h!$f6WdNE1J-qgV/NoBZG>[BKPmP64UO^Ai?8:
	s-^aa]!g&M],R=!7AZqXYjLnOFgn7/i\8?qS_j<YD16PIg=JH[084&pB\<GmIdS7iUP$T_7;_n4<[8cM
	iHeTib`He[QlaHL$=U>qoA^(+8r8?rc`k]+1sh>9n6McOk<?u$r^[d5N&p<Wb=)e=j2E8L&eoq!?2/12
	pr''>f;/:g>:C9EiKh8gUVYGISjGi&dmgp6iU`XmJbXs62T.8j4_T&SQi6^GeRLG(1E%\EP/iDZ!aET/
	J2HOl[ILt.8Y54%s2H,qs6@Apn6c+]5:ZXMkjZ0+YY"KQ5b:5W4Jm]Q"lUcRA;jK%;+\9:V-lmcGbeY*
	FZ_ur$[`7)L0$J8''N[eB^tgRsW^@6+5;"a;)9!Dl"nBFId:29^AoB];jJVO+E4hIHAn"h7I"@g))S@:
	1;Q7=E8@>s76GEn=:(X>clhkPZ+91X?^\LZSpQtm$Iir+bpbi*0qIL<31mDq]KEtY02A/SN8>(49Q?Y6
	S3+''0.g]53.BIl/nelMD-.8R43VPoU96UpE96m\sn+5(R@N@aL>`A<-i&]FO"38R30CPp7!=t9B4''>
	.a_#Gc&r9TV3[[6d[F^I)q$n,2`@r8T=-nNZqus4r2''`MKD@rZ1rfC[SW^Z:^''''$BHs#;%Z]2hNFr
	/m.&$aWAXl>i^WU2j!.50>FEE#KpUeO8\d3pps;bZ6t(f''fgt&<9#!f(XkI,eC*mphID,QI?\%1Cq\m
	RX,htq<:g=\9QFOlu(8Y4&6V3jlH!U]GrI=dgJ+C?Zq1J_1HiMb\K\WF%i4?<PZ6qDOj&c]ig5)Y''n%
	fTLPr4ZZ_c7<c8dR=Pplrrg^1?L0el\P\R?9B([TG]i(l96%IZ)bJD7q]?,1EuM6&s8`YSij0>Z3!&S<
	,n%\u^M28U`$T.&@jHDt8hTIc90iqa\Euci1n5p%Q2kYQ"9?c^bbFU]W5r>$NO:<(N:2+tk3:Hm[m.&:
	p3&0-MX]L=D2r,"]TSU.dbn#e*ha0sLjOe[(hiJY<3F6ci_QM%%K1[Dkaf,t/QV>9^?O!ZeHifSiLm\Y
	ca7!/N]Bc[ueXeG)0QHf":@gA#<1oUl-Y+8jSPrlKZhcBg`bI7lfoRX88&kc+(VMdo5%biB&9D>_``_.
	n5ca>Hf<@=jal81<#;RailDqIkJG%G#0l;u:.ur+,g[NYm1"fe;X\7`4H#^--M:;W!c86rC%O:pq^t''
	:Ch6)u^ANrSp-;#=&%XM*LnkT1CKGFhhQa:sOd]BpE.e+/2[[1m]@a0?''.W\baD''A''73VLf!$/##l
	#CW/$g5c=/LS''QmDP"V"ase4=RH_5ENrPX6iGX]19[7j])NMgKi''V?rV:P,J.O&2nNte52ti.TW?er
	(s+8H8I__L[TWRd=:\M/!#/FSUZX[HLT=G)*C/>j>nN=''T%TOg,SR(mKYU/cWT8D/?>&'':.PB^nIkE
	d\8-FYnM=\.A:;dLk-NISM2_''uHZ`Q$>FYT15/,ohS<[lDW\/1@&@(^Y@;-=B]/3$SG#'')A,i:WPkZ
	8gRpJ9]BWsS:+KC%9''I$GXf/`r:ZBqW`qk]cl&W>*C(Y/SU<&mAaqn''=n8;M%OQpq3EC]_aZe-7s''
	PH^qZ&W<"i<6kc)p=9]Vr)&o66kdH4$%As!dX5d-jZ''n>Z6?AdKgne^ggAb''Os''&Xsr(ZaQhg+kXb
	))aQ?TR,-^0UA97HGq2`,VbR,,AfO5]R&skgPtB`.[p$r2ZHS-n3#^EBjVD!L=Yh8T"pV;:e"`7>S+=U
	K;[,f0mSfh]n7<NFfKM=3"s9F''%:O,Y056$8])S5n0g3-g3!q8@<5?]^k8f:0dVDGFhJr`"HhQ!=*cW
	<2VOt#rr\:X!J,*aTH,UP:''!Sk''-aO+kW[0l2-\LYe$V=RmQhrXRqA;b"55)2#9*(bWaI#AFsR1$"g
	EUorjD%aRnj&7A[:>1a>^=YqTSo0=7PO"]Fc"mO(2IqnuRmR2#9]jrVF8`cWQLl!bV7DR-\A''lr_]?8
	),-S6hR+L[.(*&/P091\\u=HAUNV3i01g2gU*kN(-hec$GCrpJCr&)>e([glRf*?6-dBDP?C*2l-8Fa6
	,)!d/lQ0I.">t?XA$Mp''HgDs+e]]:FkQ]Z3Fkdb%%&g%-5\.39Wl*,b9JicTC0L)03"ub`LPLg"Sqn+
	e.Y.^lmju]g`n*?/d6SHC5FRHXc[7HJpY^nS7q,g\X[@7U)9T-sBFM''FRRI`DRSH7l;cY^\mTDa-.`p
	L4Gj^F(\bQM+Flcn/A?P4$g)*;ac3`1QLQJP''PF@A,SSOB9oRLa\F:MTS0p9qeTo>R0I?-;U/a2?''.
	(L[Z@kfAg>$c*El,Nr(<0#,;o"8aV4+;LPVP:nn:@#:D''i7jN/h6kQ43S4cekc]bZAnQ7JP>Y^"m.JJ
	<Ue4/mjq<-l7;*f[4Z/Ge?TQA.:e''@`98e@O9)[jNScaA>@k,C-\FSF).rZl9*ZBJLDkR83t1.`\EJ\
	K27/ga>Vc<$''gQBKZ-P1lj?eAr"X&"VtDi?Au0;l9ic?S5a_qKBk%@''_=N?CQoj1C*sOrf!X9*iFk,
	iVoS(8]\5R3p,"N]`S*_$2\a1Sh4gZOl!7JRlrirZnjCdZ^H40=4%:dk=@:oci^W8HWZC-i!]g#Sh`",
	h1/NdS>,!>&!MONU9qrhg(5oD[0u"F_Keg#/*0-.`mZcN`NF;,''7T8.Beb8OApIY!4>YNlG"M+qE?^m
	n?6,2Q2jTI:3T1t@.Y2Wf=ad52r4OQOU-CJbkITRaN@/$-,Fh^s(Mq%?CC$)&&),o>u5eWea[&npM,]Y
	5O3*H,kk3;?3ems3SYI6[Gp5Gi$.Et!^Sk!7$.Ui]@V&m83m''SL<Gl,mF8Wl5f30+)o^"La]B>bIXB]
	Y]#bY@[.<D/j,2d3W+al9MC^ukGT\QALL$e(#"d,A0DWM/;`;ZALF,8T%5Jsf#MBDg#s^[u:<f\<NGKY
	:J''r4Et,QGbp9=i''=RgeXU*rfr?1)RY.,j109!(/3t''3rVrkn2''9RhlM(2+*ckT7u4XAA"FK@[<"
	.fRGWAnRXUpM`p+]l$/!O<R=4Ub^%9gUmE`.&''D\''mgl63Y]Z>!;%-8P<H=]l;:[)qm&A'';G\;46B
	paS`]<Cf5@j/LhOl]lN1pW6mD6/DkgId?(R2uTC?(gARtrk7=6K@#\6CmBY&g(6PCC$neZ)nV=AU5@%G
	mdYb=gAYpCeJ_/NP!fGY`?^9a=#YH"jX8R@DQ(M&7_fM]eO)?aVkr%,CSa*7C3iRaL4Sq^@k)(S`e+CT
	S)o)*]qAOKImU''oZcdZBmTI;ee`/lC[JbAg:P+iM/BKbAII;sZalp!V:7TYDSKr8aDC/``7DM%,T:D8
	G!7o2(XH.p-)ka4sNAlEbo,.-DNe-i-ar]b-Mj''B",@/pGV,>7FN",7+p1N6"BJM4S?8CPFg/4*DPd7
	dp"6q5Tk8lC<`ZgOI;fRC5;1gcoMW>P%?r;bhG]e^pQ7]/!G`C0pYEhE''jgHIQNl>[''k>OlN89-eK2
	ORsFHR@''Ecsu(7@+(%(<(9a2C,S7!''*0;cVKiE''/cO;kB=,mGW$S+a7M&@P''KL`BP5db:A=n562)
	6kA''i,jkPQET1DG4m;.3;!(aja$e2T.-)k`Q+kmG20]=.c,QB3Wi`Ag*iL(S$A2.MFgU@[WrRG0e;$\
	S(KgRB#r-<cr40.EQ,XD!5)JfsCPmYMPo!>M_-&=_4[%.1_(]`S.:<[4p8tGGQ381nuX$Kg\Z_^)LA*;
	N+Qgii(6`9t"\,UV.R1_bhc0=I`S]XN;_kiIY.^W<r:/BCageO\H"?+u]o4T"ld5nmM+(?4EOsJ8*mZ%
	FD_mW`,l7[$0.T?J0DjI-uss4Y>oI07)54mo;s<#ohqWX.1aj\Q#nube"q.k@FDDD^a-Aj5hji?%Ns5i
	q+bdS3Zq(YsMLh`*AqS3''NjGT=^fD;q1/EE?%Z!ot$Q0k07MPM$(uC$%A=&/&u7#D,(ksmUCQ$I!.k`
	>kEEhbaF_B)G>"IWlf''22VC:gY1H34%`U4HGrSksD>BFYi/Q>26[3l2oO"G:iRR^+N@?.69sPJ>"?dF
	KBqg%DWRn''l>j%c2''/,]ujfdem8Z#QS;I[@qo(&^MaWC7g?T#J?S''R*WPO%,MOhZpQP<$bO?sTt`"
	Qm7m?6^?MJlW3ckUF&r,Y8<)h91m+a*oN)Y''DmPL''(b$1QS1qY%ufbJs981h8;hX\YiaY9;q9Eefa%
	RWG!i\W77%R3AJd7QRU<o--fSdk@Z>lG&KY%gS?<P7''i&P\]N-<:V9/!NuPuH1dXm#ZR''WMc.l;ZG?
	$3f8EB?X9k;^b+5OXL5?e.&DHW[CfnbVUF''"t/&Uj*D!Cb>QCEs6W!JLSr5E0c/\g^5#`$N@LVMV6dj
	s>cERi,8aX099)cd,^7=7XjTo@eOj"tPUH+X5f2)`>sTg\j''F@bao8>CSrEVskU5VP`okU?7IHA(<+Q
	<LP>d^rrgrg1iJmY(IEZ:"\/&l9-`m8-D\T3LtY^YO6B+(II]Z&nm<<T"<X0_c_0F(s1sDlNqusR2=_[
	HB?_a.29kP7(=dI:KQl1*T>gY2Qu8D5`^*6Cr/mO*3Ii:fmKrob8%$hhTWZBY:j]c-L`3P,&Gk%.XC(0
	kWX2U9?MB0H5Go"Wg\g"eZrO>`Dq64^cDRp1tb7''Ph!CHHFmG2%,FeVackP,[L`''MM2WTWhbeI8PDu
	SSWD9=q(dk,dNo\Hce_/W''ZX0isULnDp[%#=ESo]PHXI67:P.hiAPh7EV1hYs$4H4/=)(`hteqcNa%e
	BcWK-c8k9T2\TFeI=<:Ikeh1pp5(''M7uGXM11fHY!EWCC*7ML,9/"o,;=BLb&TjRlM0<d99BjJ;_7`X
	lB!lLu)#$"#F9fBs\(`R@b3&]j<Z-lXk4Aki@GkB>3%f:JGu3d*fWaUZT*[<+GnJ-&6Ns5=G/I+Zi[)M
	7Q*sPTVDG!ZeOH7mbcA[0cnor=8;EDbgsl-GCOFi<-RXp)9c%M\LK3#;_]"(7TF[n?G<S\d=P_XBRI)>
	d`M@BCek>89VG0YN;sVoiLAQicr)@W;qC/,tfL@aN>?PUZI:-g_ncjUfdHHrBl%,"(??Y:_,JQkTKOM-
	Q&lToSC"Mg:MTN-Uh:/5]0Z/Pr.A9[!cVjO_eM-kUEkYe9c63CS"\RfhIs[jC_,,(LAIFB+"<SEY!>]5
	C#JpRFC5dG-3`=2KK+)0)Gea5QuH@o:XK+U?/NVGgG>4pE0/X!UdIh^U5;j9LnBs''rgHq3lQQf9MlkT
	F<oojo+1a%-%Y+i`!(P<''omJ#''YN_QS>9pa6kRW'')JMlu=a/7,E3Z"(-I"mVa7^s&0''>9>5tS)<Z
	Z\`mC"@0`;u''Rc]P$@l].Y.CJ+UpFSkA]5g.=.+3[eHO:S0]L;?;7%4;TeA530Kn-rB>&FN*%Gd$sZ\
	+"+b3`MtmpQ=s@Jlj*sb8F_;l-CSn$"2D%OI;c<O.]]&;7a:k]C*$?LMMTj;WohTgP<8Gs(%O<ZZXiH[
	#oi1*,K2mJk3KhIWs=auRXKY:?NAbX''P"p:>\OE5aEd4M1k6qd*s#hm^NeQ>k3`oG**I"?5#?e8U+=T
	Sq`;9#G<ciJ=,KFNJL_^:+c@sNh3!s;mlVS(T-NO5:LKB7G&tZgA?5%''Fe,$3\^.t[X_Zc>>ckI#]60
	ocY*#E`/gYf*J6V3qEPu:>>b>f#:JQ4+C:W,IAYjjD]PjqfDgP!u_dkp&bF-oW6+q3]^(i;8aAOL7$J;
	/KXQ#nu*..J9R88o!leK.UlfA#t&dR.=al9BdLDq]OBF>)Z]2J.aDjXf(DWgt[Ur_lPWB6=<l26/`f.T
	A8h-6N''h4ghEe]k<th),''I`;hmQosZmG5gKh#n&"%SlD@je^1RLbGBA83f8(V\2_sS''HF]H9p6tE5
	V>!/%"Ko9:>.e-XP00l4nuhf(/!(^XF''\l(r7o-JU(TE6TYDNU9_5(p41".MS4-$8T(Zh]GGH7rO`f:
	EKg;V5@.tN]8!8MB#coBP+co09IWl/6)>brrY58M,X]00tfmn^a^?TLC9$F2\`]plmQ[FR-s6`C]:\hH
	)5J8*>K.l,4mkj![."^)t[uB(V\mMTP;^J%Lfb9UFdnN/X.B%+rD_B2qB4+*/*g/HomHZS%Hag"f=.G\
	"27&^-''0/;jB0r$"]Y6CWUc4=i;p(];=RpuIUS0kqc@!<]A`(Q\''<)TJ6A.g[>!2>9hBATsVr:_(Ec
	7N0^;i<Z:_&R<H"L*44jO/IZ=\@]3oTPe2j@?j[eV8K5&:GA#*9,S\JbHiWT)AEZq=?VaNY?8J"s?bf>
	pMLT''C.`Gu?2\Fn>/2ak-.A=ui*39BF<nh>G''881pOK:C_`ljYb1$5Wh''_V]8E?[*%%,B1"]6Bj1=
	ZUc`k7"5l\6[_o&)W#3=''^4GP\0ds%kHkf20n=r217DAqPHdsdDY(Ms,9%]dM@(*IgrVREU5+.!I2%X
	+\8!sCh(LIPl:NaA<=(2k[!Kob$,t5@MhlXV*F,a-*I49N[]g_M9&[.WRjRT*e4u[h>gmrN$;''BRaF7
	pR0[p7A_`@6D@q>:8GV<.cc8AtX*(4>77H5:kpSuR".O]3$?CBU7k;?l5O*?`k8TuE1f(;:(T=S+&Uo6
	X#0*UF3uqb)IA''ZeT3@],APFZ:KJ21O*,D`R;e-,RrM"C^Wo?;?rZ8qN.k-j,GLT[Q93''7BX`X30gW
	T^6A$6Vf[Q2t)$(c7HLf75C2?QO$`_RD-.-24Jg3=WkmljB`Q1T&%#M59P/-)^mFk[?$&=#Jj\+^>2^e
	%oJVFH7IG3kAqH0)K"S0pH>/p/sptj#\O=l<0XH]_>G>0ht[`H^S(a2rg*''Q!$QY?fXgbf^''''Enje
	@3Q$\$?TYBUWS-(6n%S0#cc6ruaJ^C;S"@OIeVGe+pb],s6n?6`,:''2*cA9d%)PSIcUtk\B;&H+.N@S
	0XAhplm\g=)R2Bp/a3!R+4%bkd99GVdFi<\@AkW4pfq=M(PVl"FHc8r4^gKV=MrSI\a?<?G!+KWnB''E
	/F>;N>+Rf(/9*C!O"3DTMMijKi*EI:k0EYhHELen([=Q8*-P1Y9/^)5=fPC"_$=!#o>L7(?B<_iRF,/6
	/Gmu(/RaMA`G<T`jKCX3W=hE_9@Fu>m<rTC5-L)&a_/sV%pV1G<C*XdeXJh)FZ=_,T6%&e>W&.Wf(2tD
	22ngsRi*]s*sf^SZkCDSk>OOX.=>OJAq!"@rue-(g$=>d7t],/g!c+l1mjU&SJWs)>o0N&KgI2f0\=ZB
	M3`As(d>V;Ui^-Z''Dkf+nON=''?ZuK''JMK*?K\Jd"ocB_N%nsq)o>T_V23:D1Rk''tD8ESk[ErCe$:
	t#<n?#SgL%YpeqC''F)`h7gAMW%H3SZ`\&-c^=cn(gNCDR2k@jRBMM#\QlIu''T2WIQTpJa`T090)(Bn
	[.X$**r@D$=$9c"oRQVk0.qP`)gP_I05gTDqSq77uT=a<''324`)JfE''5N3cd#-8FK^".+Zoe+1&&8f
	bHa$qrnGC.I"leUbBD#T^0tC12MH=lEuPSMYR_\;B^-VNH)Ym<a,<cm4Y\JWTN:KsBH%m[b<!p''5B&9
	iD8E,7Ro&+,&\oOWRR.Jum+r''Of#+f;jKiPPs#22FU6QfbICn<@3e,`=MWRY+AS3OlcXJ;kMlEQM>>C
	*GDfWII8,u%s9+[JO2BcoE<8"]9I(K\1HdT9UmLua5o>E:&bH2G<"J)i`Oh,%X[WG:a_57.=!g+B!1rR
	o!r"C&"q).:;\,uYtse<`2=!UK`BOQeFPmq9nW7&$6lf:TNN,n$Q%<S(V,S%]NS:hX"So/N!fP!nuCJ9
	G#),d4/p$,0WNH)kCN\R3$b:rpu%[>JXWfHmo#G''TlWaE#s/#B?dbAqn.W7<m_)9=Pqh''nO/SoH1)$
	+[%j]O''($ZKZjtY%"T2n3";34''uaV:6VEo\.W+%2d&E["?G"V8`TWgXboPqOE1J^*!:&#pg^.X@HHN
	-Ju3eSKE<^Z]ApO%cV^k@*KW>3c"OT!DW''9cG+5SEDMG30GGYSPG>iUhEY%iemsXVt_1.J]BgrbYdX9
	#WM/p8:<IJp/\+*jml@4%eoS83E%B<&m/*g;=]kXVZlSN%T[0P^n4iW=0&=B$a`"EN!cHWb>j,f5?=tf
	H8h$T@4@gI/E);tWm+[dX_1''Xd/`.3=8#Tb%jM!H>!>I%F4\#i)%!-_\U#%SoH)e_.e+aWBUhTc8soB
	b3lo,3H$sVZ*8i\h-g7Oep([(9,$*8\U<I8t7CkUnr*?O%OiDZc;ATrDUR[b%2p(6RbEm_Yo&7IsceLX
	(6+l(CH/1S-LVjpGhRns:EWBE)0:]RuoDQkQ1%<0cF`\^t];F@PNMO[)Hdk0%?0?O#lr$$NVb*Br9^O&
	UMYoWL7Mh24dO*\o]K>\''q9dRp74Jc?)JO&V8?=cTYk;UQCZXX*b0&/\k2qgFEIuAK=P@"_g"]hpX5S
	-aKob6^/=ijnD4YP)''&bC.HX%`C8YNslX$0T1dfoC(c.3,um_,p(kkE4f8_O2OUV1EcB/C>dK;&1=<+
	9SIYjkI\^I#-p<LT(70T^@/`8k31UQ"OgR6++0O`''@S1epGDkQ*.]3((bU''CAFj0j5puk*J;7I[gfg
	W"IU/+P+ks[BJCn?N/lj%iug[iO''3YQe)PcjUt%sGJ''h-?Sg*e8@AU;]q;q9[-?rR0$<L-$5MhEU4!
	'':VlI.d1IH2$[MB!aeikY8e//=rg"BMu''18m$SW-]`X!Iu=\0a/IfG@4n90lfIj_\!rdF3JJ\N,J,.
	''[+cs''5"$;^Gf#J6>*r''`tC#]iqW-f%tW*4_<''/nf!MKF+:''<mfGkA7-&Q`Tm4JX>b_Eb]e8''\
	L#-DcB69;mr8%JfqB:>gQug[,:\hgUL8+Qhak##1D-SDHqpJY.`n)s[Oub<)nN8ft@<"J%_];V)ZC,V@
	%C&R>7^Doolj>-"jX7^Lr,/^#@-qXR2Y/hiVkQsdcnMjF,?cMA`3BZ!''kTnR#H:[`+O<^_l!n`W/J90
	XDsA=<nIDNt4C)416l`:K6b5cu1dc.FT6iZAe._uJ+O,>OC<J-2O`36-fsA3s4hK]Al6o2VPs`uS5ZLq
	;JN5q)B9spj%heGt/E`&gZQ1B6@$4\0qLL/P-''dR[+W(pEcm*mH?PPrj\P)kM-Ff]FY.dA.Y7[)t;WO
	5a@aVI=>P&KQar+)Qj!%@^i7uuqT]]QLPB`''l8??u8RWKsC9V9FO"^sm?YZ^;=!?oiYGt.FjFfb<,\M
	KPY1/,9s:3]"t!FDNRH91-C9_#F<#+!Z]F?NYQ$MTk`#>+Vo"S0e2ae!sui)t09;S7"S,e/.%qm98nq$
	Mt,T-''<(OP$cbH=g_aWek?"1=''6Zb>3(oq-L6:AaPH.8-n48pKeU^I6+hk6Pd.''#nn,n)LENZ$Q.''
	E3<tC''H^Xd.I[>[gDiu<5_Tm!DPhEmg)+&^GLs?MLIn0.TV$SVub+<H%5u6/OG-_"UZc6tVbi^!eZ>8
	ElIdZ0:dC])VR;og^#GegQ61P.1pgD3\M`o:4RJ]2-+?;eib&*cR''4Qi0)-uCK?ns\''`TRP''F5J48
	[Yp`3BNQ\C4g"8Fg^g4IR<X^M!O?>o9:g+CVZ&1#4;.Ot7uER`FDZkZWL+BB6(UEr3(U^@V`gLGqctWs
	,+Bl%."@H973PVHc=)`Gd6V)Xd%H$tg%4hYr9T\3Bk$R2<mT1bg^b`XG.gA>VJ[/4o15JAB#MuVC^,,[
	X#oAH(+t18BqULV"qfpAcj!nGc)u#O5u6d<Y:P/I9M;3,/j*TjrEa("hthYO''`EZ/-G$i=q>Fl#;''d
	K%5UjA6P?hD,Va;D6=kOIASWLLU;D\&cP)I1:kQaUAJlTuA$+Uo%/k\<GB=F]MMK9+f;o@7"V5d>5+E0
	.oh8<,R`EU&@6P)QVK*EU48#5"5)]C<#qV6eeY$p0=Sc!s)fa;k&TBt6lT)L(9M*QLZU?@DEAVPRCdnX
	!6!Zk&-9`$D5oMckb67e@tC$`_TdH/V+e9-)''[-lj>FoWVk3smd4q>C6^T!EtPlL0qncb7qG,_N=UU8
	P.14[s#=:uCd*]osa*J#]e!n`E*2k>.<@9RrcC4XG.s,X=j3Q1`SnOZ0PR;"60)jE][67A-RL)=;;8Pk
	qN4XV0''Rn:''f)f4^N5hM7]6?7Ad@/FcpEV;/8+''FJBo&$YDgPG"[]:HWKs?SB]&RE--/%*PhS7*AQ
	@E\JRl]j!C6h`Q*[RSMG"[sBee-dUtIm;(2n/\`5fr?YA-:b;Ia!u03`j;K`HU<a@Om+G@p(3F;g:YO.
	:17ea4VKYCC(PV''U,hMqrM4S"FHZNPd4.<@4Up`D?AbV]WT0(dJ/Y>JXPGr/)kncTBdLQ@WFD&<o,9@
	K]oLsu9M8Z:K3Ip=]=1ReBCEu8b,B''n6b4M8Q>93`)$%sI.!usJ.RX76U*,PB9X7*%a)G/Vg^TZ$qq:
	mc4,ITOW$AI+*2?R7:r5N==3WBl:bD9i&(C<U^8EV#l@%laZ`LY1$k''eHW:[XkJmVS2%m4<Ou\c4%B]
	@EQr\bNs$B]uFnZ=i0<cSbHrmqHH@c(,3pZeV=d:RKIV\CW.dA!Z:35,^$?W:)@%LYP,.H*6GqQRespL
	PSIn,''eo^lfWCWqB5E(c19$?/ilSBMhJ_Q*;q%8.e/^5L&d"T90Q1jd*EMO*pE''cIHQf?NCM$2Q_5f
	)\S!bnamA2^nJ_)th##G3Y$(ag7ok/:T8@&BhLmnNfrE,>i207ZQm?05`p%+o,erO2H8_#SR?R((a,0.
	3q0VVU;ak`H`0(-I5@+RlAK_:n;2*GU[sA%CU?U?+6$hO3cg@(^DC#lf#b`I0BORW`m`)3hl2-775Pt4
	.3OrPGUB%.2/tX\4ShKo?E)RXV&7*f?H!t@1FkVVCM<l[bjg>4S`KS;1%lBiW6N).P*UY``W38hg^VIJ
	58]u&9]peh+e\/`$OX8O_&M2h2k^Mq_?DG1kG@?OK!?flY*@UVC]&dGb##3l[bP-LZqZg$mCb!''[KoA
	!h#JLWHj&db/,I09#&tGPelW>O"G8osmRI4_)$q-`''6hp)8Gj3pFoD/@Vga.8)MufJ0-I(_FnJ^eo\P
	%"PV.5oijPm57UR*T:fl/G=mrr)SXLs"[e+X*3h=J!I`_U,@;5L`I5`1K$%0HmqOQb`"bGl?WTbhN76?
	/CXSP5^DW-6L22-&:-,j#BgI=cn8_fLg+U1JK`4oKp.q74&<ZEpr9hC?j:O17]om&Y&gb?O[9S[EC[D7
	%m&b"''TqTCK.^5(>OnD@6?E:\N#5.%d[/M,@:KCIQS@7/$507tbK-VYN/]6M(X\JHhD,@W1Y9EU9:&[
	$$7<V#<G6-dCg<W'']<:oW6/mk=Ho,d#s&N)huN-2=K!HE\R"+`D:fOpp`s%C[U2r-F0NdijjsCj$dr#
	8=]2J73pA!S?*538[1Sl>?Otp:29OY2pWLE#Z2m9jr.eTQ6sr@^1W/2Ib@ZOCLlQ)KnhT/rp%`9k9%FQ
	nF6.hKnZ#R]C<Lh(N/:.]ed-9+27]X41kaWEuYrJ$sKVo0AKeT/%nLkM<oLk1%u''DD*-T@:"NmTf7I!
	@E*C\B@uQ_`*lOSjhEW8*!]u^h_.ZAAdM`fi3M?nN/p?F,\R\?V+g\B''d&>O2Ns9bjA&96(;si44j\.
	=lN+)H[JP&M:dGS^6YIl[e\OH\JEt^2L''DIHP31[qO2]?j.U&hEXj_,OULm`^.8tVh?g#''T44o''QL
	U##T(`*X,6Vr;RW^$EA3=Vj6-\OLUYM@\t!+J=,4a7I>0C,%iIkb<k^J57_ZECH#`(km#`>hHntBZTd:
	-cpCQ&LZBD7c0N'',=Nc#I"Mn/"&.YIg1rkO[5V$*DkFXtpU!>D<"\%E:-\K*lno^>F75;U"aRsjK569
	n^fQMjSDF8IL[qE&eL@CuZ1JMB"6BaUi-c&4kg7b?ieo_+dnsK:R*8KViSW&:EKg$WB570/OmR6;@L6r
	&*-nLAM6g;sEQrDX+@&bli]''uWe>@g;BnHYp+/NG;+<tmX/S;E\eHRLb03j^bj*J+cGi@4sp*cuR<Rd
	PTM0cW#Auqu5dN7(_Ug,.kKc2N&F76/i4F''-b`^i:;E<MA9>DqNOi`5S10ZFa2OB''GT;Qq&EnS3Sm*
	!97$X9<&J%j$aBVW/2Tj&eDIGP0T&bpUn=]Bu2719Yhqj^Hda(ZI@)A#)r7k._e_^^=O_E$:&?]aJG4r
	*1nkDZjb.?kSOi,!FOTap\T+!:#Gc]USa_oGf%2dun%4,ZoN4oA#+#W#K:A"S>q#;3e2U''_k0/l&=pS
	(ALj!kQKC/2L#(!DL;M0Q%-@IQ#Fmq,>3C:4[8.m4^6AoJa3"bm3ob4kA>H"ed:tK&9_Llrj='')"Pho
	Jf&tHZP!EfU,AMb%R#][jD&DA^GAd0OG''CK@%CJTSVM-!TI)5upk1q"&H_)9%ceT-]7]<jj^X7_t\W?
	)7dKOqT[!4''T(JKoU61-;/:Dd!ne1p#Y^b''%R"*6Ls@GbScSUIKFg$&e2oG)$%EEJd[IgkD;&%?oZB
	^U\JJ5SXj1p3,A]#I@DjRXKRK`#.3GY:T"0`WXSrCE]HIg[%L.ol;cP8QZnGJ0+GU"rMPX7VFIZj:`F(
	lc%l\YhXc&TWkE6_\rq@cq:b\IY`;[tj>''k#;g!P[-[P4arUk24[U)k_g7r5X?!MA6dRa*?+SG&"2nI
	iXO@!fc28a-E,BDHZ"oKd"=Tp%,.5j`M0_D<Cg@]9eR9j$un%IEMf`:81Hq_''Im86P_I5X2*''!G(=b
	/IGeeX9^S!s)10l,j("EBJ!I<PW/UO''1`TJl=HX\L"95BUCci%6uNV:oX"aV(ZJnI8jdRN9U9!Ro`*s
	Wg2E!9uSH+g8ecgdkZ,6+Q!b)^%9Z_+d@m@L5^R@5go]nNoA$"Kju5uH91dGP=A^''Z7*"''nnCL;tpV
	Fg=D66u4@sZ(6K<GZg1\=\<qNfCtGAArS''^i5R01`3BKGd@&gZH[_c>m]&B?6iFi[k!"&SpV"70\H1n
	C=uH^j^,0+`''/K9T\(\X^JL=4/QQ;nhDiEAoR9H6cT1%A4!V2]-!5p;#UN]>b@_Y(g1Sak+MQROj@OR
	PMb^1]BbFX@QSf?`612b\%''UX62lYdDVbR]_c&74s\''!6:%8&j&aGa]U=@1l:!L9q_0ah''Y%RM=tI
	obiBLmi!ps7+o"^XMNY,4o\O(SjRVo+ao>$D+KAlai\\^UI&7DSpF##9-=UmP_pN.5M"eb^+jF!ZT`nM
	ahLbXe_uCMG[0fR%;We19#6m2L8YD+>Jb>u3BM):1o9H7ZCT&H''PR+>*<qo.a^ODDg[7'',RQD)S;9"
	V#5!,Q=T6TjY9a-"(2rd9T%iE::;ErNQ#n-CpTZ/RJ@C12"7OIJ*^%PQHYjSdpCYGei*''B37&*UQ.F&
	16\p5/ON,a$9+-*%ui1mrXWHW6q*h4&25HJal/c$o/l#E/fq68Y5:TUO7&8!k/Q$TbFSHO/.1@$Eqc2n
	;H*@O[)uSQZnK\*&,_o&6mON6cH="aZ@+I"+Pr-_b7T!N!g@i+6;]8i8@[)Zp7%fnFM)>.P"fFb,cmr!
	r)WU!o5$Yc<9&Vmb2jNOcco4L%lUG`''1fHc^>ITDO/Q7Yh0s@];*H55,Z%7To?t`.jRDLAiCV;%-L*q
	V%?Y1R5ka+R+8+T^`fnO,@J:GZ_=J9*taeI>:94NCJfCL+!*S0W+qIfN<;m5Ym&<cJ#ooFc8,P!2MkC^
	-t;%na?8''4@cDtSQq3^3Jjg;P^IR#)*J=''Wnd5/9>f=.g/8PYPcdlk3%\-6XrfgqN#_<>O;7''7>)M
	&JT''&;M6="I/HD8@[E!o,ujuSSOW9433krfGe+QrRe/J>DFb4S#Q=4$`hX==I:,,N[.FT8H1?/0cjq&
	5/;6/hUJETY=%fKIV5ha"SSK1AIWC+=nW2;4WLBCdG4!I7r@2eHsW*Y:Er(C=c#,n<OF-f`Ddc\S#k;e
	)Wr;RbMj7Z_4dLg%K\qbtF[FsE;"h<3W3$U(U;DiTC_^FG*M*]<l\HL[4=R#>LqE.77uHA](%$tM@*3e
	kE8?9RUR+=NMCgm[ks!1FF`4U9_);>/Kj"40u)rKnM2&ujA/kLq7;.JK=m]K]%BHY:$]H2s:blN"%5fK
	5bc&[@R_dZP4,9PQG%q74Ol[:B\L,9K0J:2#1Efc8,sT#>s_c!<jJFL(MnpS^nI8G5I:6p_IiG\mB)Js
	p$*G=`90f^um!`![sM,DZ415S!9g:kF%4aFL6Ic9+a''R>RG0dbK@6SVG@:Boq[Ad-`@0;okQS.Y/M62
	ai3p*X=2O,1<V\hE9q%mucou^^R"uf%O%m@m-LOq*&S^W?=o&khP(Q/UQW8jVeSgpLFRupNhfGcKDoKK
	t:QY]57A[@_]kS&t/RBd!7lF$nnjV(#Ps<D.P.$lTE$]LQ=sPb1RX'')?!o"56EB&I@''Dr\aQtR$I''
	T&:WaQ)1N%!XS(:4i?Ll0%-^AVd<24beDu/6?qnE-CJscKB:mOoIY.2m)Qns''lb>HmcX^Oe(j2%12F2
	W<!q=pt_q>S)tk]7!RI1gU\p8_#f1j:*iM@3gr\2[c''Ib]d6q3)LU6>-8_j;ag(TJ3"gA^k7"Rl8]/A
	n@$]AX%IuWVS.qX:\1iNPXhh\(t!(Sh.=sr^CUu(gsf"4j\sdPlj62b0G6=,O>2s>1(f,Mh0h?0cNCjJ
	''(X0.k!J:G*1rIH1$[!YE=(.q^^0%3Unk)W9ZfH-I<1cKWk5,"+$bl.H%\VXBtMY+:.lKEjiL9qlC''
	,P93Rt=Tl.+m$2%:V<cP]ftI*L:S^dci\hG:]fj*c''*VGTG21)n3E"AZZ++t%BdU%Fegn3hT-JQfZXe
	]?KO(K4i!iNLkqO\uekEK[Xu19Zp&/&W)iC5^pGGP])JR=>dL!*/5J0>=+PF(QHlEM,&''4pf6MtR<qm
	FU?kN]F#R@Lb''if344&lsG?j`i?W''(5&L<bdIL\gHYrVb&O*<CC*n-l(r+VVP28rKd)Y''1+F#5<(+
	_N_#^:(&tkf!cfnPFg!d<Z:\UYrZn/9enkh],0*u]OZ`k^U$UGRluO8NYE>!%&i8>$4SQ%@Q_ZrR07(>
	[2$8fkdY$)LQk1@<2"mstac&a2]5I$4N2but9=4?1bt=,`6+_J^)QGJVf5oGdR].cRL&Y6LpAl[!.a7H
	ng-$AZPNOP4]a>)DTS%@pmM.7%OnWL,(*E/CFSXX9rU8''oF26=e"pR33)25\(7IM_kj/gR#R676Q?DE
	YZ)JN/5]a@.4jgh"NW''''h%/(?$?''H.:n9eD)7/(^eMLUH_nBt[DI3T\9DY<D#!^3K%]SKgcMK*I9I
	mYSoo3dZ&+37GeH.-#p,7tU&''p)MiTWR)<TfS5-`G7[M''S1^1g\R#AHV.O]SSAj(i@F0k6`MGqPTfn
	.T/<j`IEqK%\W?H,NYiGJ5bF\ULHR@/2/9OIrp!Qp?l]jVuGM_MZl6gMqqQ0OMk3Fn-dm2MQJ1t6%i6E
	h9mDhg_/a]iA)i\L8^go9Gkuni\LHRKgiHA#NLo""J@G%2Ij$PQmJManZkQ),R,t"kV`b&?[aHZ3a!/f
	:kdD<D"RT;W''qK.pA``Z)kT)J=F''A9^q1PbP3A$l0poeg4Uj;KK>[''2ff:laDn^.!1+5CFbHWEoYW
	GcZN?1Wum;mOjIt//@A_F@.=F7blc^Q2FXG`@t+EU>suk*s1rO>QdG4,4$0<U1+>:C"4''S7/a.2I1Te
	6^e[egJ]/Trlr4&&;eU<ckL2;e22^m99?Ec<Shep=831f;5;TgXr(C%`/rQ;&]jPTJ-P/(^0Kt?A&s$P
	Y^q1T]!2K%S;f*Rc1g4Cf[I[PffF>[IQ''^3<9A+AK]-fot<?qXD_+S$oNu<&>I>;8K?kB(LPLBN-Cj=
	lg@eG7KIOqOJBD(emRr97&btQjo4\+sVe4-Q?$>SkHqX`K<9N7V\L_]21?6c^&>Y*''`UToNQd1kA+/i
	Y-_Sm%Tc$oMMmc(e5O:-^=$AB_d9jMe$D=X*LS15(*$-uE5$Vj''=JC2#*h@GDTma&[I7emgh7#5o.(+
	:tFVSO^"!UJlOC+`aYu->j]NL>ocF55,Zd)JMcE\C@dubNJj_7a&R<''''1&)ptY]1Pe:6!=m1T\3jrU
	?NCJX#R^,_/BZk?mao[Q_DtasOVoD>ZOR4mNRB#^M&Y;Qe(/@]''Ye!WaF.$qodE1l#a*n>dDe"UTBV\
	lXX!&2''DGNtbffo-n.b"p9HQE8!#?NZUEmH>*J4Pr`(F2#b;O#a#1t]SZ%%*psO=OSKmM+5ScS/?Jo5
	k]MMMAr?_dq)eG#&+_(a5%u%T!h^5VsT"`)>-87D;leG4_aBQD>`5i>\SM4"*4o%JRF%gXf!88#`Y1]C
	XfR>^>!Pe7E2e9O3T3V1Btr30j#tqA[D:_3.d*:FjHk?p_3XDK''"-1!59hcuZ/A;\XU6@_ENb),sK]E
	"e1cZ<0onM:>o91O.3o7$hh5GiTFVLS3fII>u04VXjD''kIFrFo]RUF2+f`MfH5?HS<RI(e0&473<@]p
	9A>Y/q]#hT*"<T%d4qk\loE_dR@n237rX)j$6]kHlmAIdBp''Yd@-H&Q:h4lXb^oiNs+&o-I-r6Q26pa
	*5slMd#PN6=PnR5*WX<l3G#''G//2?Hb<3g6d.&iT/5Y[37D[T30RjFK;NTZp2XPron^^t32U?0G%HV2
	O''oYRA%`u(^R=a3aM''o&?^:lY/eqWSD[[KgUF7l_=`@Uhs9r()6qUo''uf/GHlMR_ik4nY:rb2<qAA
	2oP.+5cLT+-S6U*0_[E05qmLUk&2N%`5\]/in#)Bk<e3X$G33$:CtpN<iO)k''[DlJq2C(jEK+1iFh`^
	XID#EJ'')6Z@bJ-23kW)tP&Y]_OQT-qUqM22\T76g<0SNp?hOiQUJbbk-[81^kn(ucee[8ENoQJQh^:I
	Di#@XR$bAk7Of4`+:7a_Mfc(@IT?R(]rrpc7(011H)_*3(9AL$@5_]tD(ot4)6PcNuL"^J@qZ/[tnH3V
	$l]Afpdfb^?9+0;cn''UO!hViUT=f(X!b6.,6iF+:/=e7I"UOQT?!Sg$*gSNZZ(`f7r9?Tio5_1PZF*h
	5HEj2S(iMs"OJr8N?-[hhir?+f^c''T=`9H6D@AH5s0K_5YDW<lgDu4:W9WC0\C6!\1aphU"6]f_$YMn
	kS.8e?U=2:psT2gF@l!-ZYu&hQGUV+`[rC%Y]KDqaRduCMY_fCS#i#LmRLFHIXuWV8f)jp\jt-WFg*)@
	N-X-luV''r6JZ%]-Z*7.=a$.Uf0e<446(-9*@J/1&8nY_%BCIN?hh^tL^L#k*SEEnWLS=8EVCsQV<:p''
	:oCXE4KdN3!['',H%qChtj,(/09=A>c0ElK=6ef!hpsII6D\_UlT5N=*CkMheYb.:!K+gH''i2A;O2b@
	>?^MWGuEuu^S?1i7\=CVXop[:<BQ''\R=>o#S7=rO/ojDfjkBfcSBAdGd''eBNU*DSob&m3nLJf3pJ`D
	5:i#;K(3d<eZ.np7I5GkZusr^ha\d6F(*''Jm"jYg-''qgM3n>#TTu8H4/#lg<,`5B''?rtJ-ZBiD=RF
	UBp=4K4FR3l7o@rj![kj(J,Yr6X=3mD,,''dYcdic9c3-jqtgp3f?\,.Z)r?e3#M2r`Q?EV\+$?\_q4b
	^^L>MP`=;K9->KH7S:X[:KSKs"\]+95;[8f,C^7o!>pI()b@gCcij-j4(<6Feeu''_*r"Lq''_`WT$]B
	YRtZ3C\M(AU;jM\:D!Z++YY#;2Lq2'',RJ*l8$*=>6l;34V%#@WiI;gG"[]0_438n;`g^QE>QNn''1Z?
	f$4@>^PU#M]mi0dm@Ss/5V+/qS4O6/''@hT#(;ZA^IS\k^^/6cTY\%2\6S>VeZ\6RH0Cf:RS$[OiVF)o
	`S<''$`$_CZMHHOYag[QkgnC''\Pc"%gJefC%m!GdH*cK2NT<c3C.3d4Dl9f(7mK.GG:$/RUssU\+6.m
	`+XqkDl6!kcrW4+1sC?#c=X="a2^5fA87-4pXE/V3fPHEq7D%B-RPk%r.oc\1@^rLc&Bu3a!T6+*L=qu
	gc9YNbhPDM''O$j@qM+5K,%mc:kL`WLJJ2V&6<<fESD'':W_0FFn5S<FmUIB3c@W=Lo[bZ"U_c-8c;=g
	dMJ]\)C!ht%t?N9j/RCU&%&+.8R(9OIg$4C/JeK7l`E>O:,p]o,j."j+dghA>-_`0N>qJ49THl6Er=#`
	#f]sJO>K]>k7-snN3X?1r>+gCFdqWgWka''$0dODg8efeVqBTP:pt+#q?siPkp[''t+9lDKer3<LCdr8
	gm,n%$9fVq&EG6@ltgameICt1u<a1PE8TrCB-fpDjJNbQgEhY1\J%TNnbE@q8a#gndg*S*jmC!H+IQ[$
	.Rh?T5hYRL''t;haKmTP@VO?+7P\,D>=br!F2.Ff'';2p4A7),2.NsF[//]Wqh=q%sm7YNc5b4iD''dm
	).Q_Vm?>HSVek;\enoeh:R3WM/$&=IM3DJXMsnY^`5&SM:Yq$''t8Ntrg!ci!p/''p.1C\R,DWF@0J-R
	VA#pAluQ&''=ueGC^PI7jR%q3hJj:SL%n3M+CqNCANQ#ZSoP_qcfuk!`=6=Ec[BoIVi:@F_XNP6g%3/:
	06)\,&RpIrc<W1YQ#O@nfYqu$1XIQjD#3>mhJ8T.D.;6oEr7t$GoQn.Idl2dESbbi]P''Oa/''Hq]:Eb
	!]&V3#e]OsU!jkC]P7du+STTD3*J869iCtnV`j:T^ZaElsD)1!56\\D;>jrL,NbBKl!Fa7ONMOo>F("p
	nsg-T;.Ak3XJNuNF7bE%bJ/oqG$TfP*IkC2OQ$-Y7F&1p,N`ffQ]!+N_[,^CS[Cuc(u>i#I7+t>NgY@W
	ILI!I@8gqp7YF''J&pM+i8uEq,pt):E8CV9`e$-$NCQB:BA()XS4,fp;0mJij''n5s)9>n#q,FS-''+8
	\h4$UgcXIDIfsf!mG2P<o:ur/(H7D)ZrS>Yh6uFop<9;Y[_c\]d$%$UlcXp.mN^g3$(I)-!&,O!?D-JV
	QT*Me<dTOa=6PL8<thrsR@1sZDsfS\k#OaZ.''D0+-@qYqZ3>d>JWP+d*Aig5`,lXZq`-t+.-N\(^8EB
	"bn*%#a+iOd)o7Y]$^^&.5]E]=jDFH^d,sr8$-5Q5DK!D,s,/[YZ[??h:''DmE1e9d-d9ZkGA2,)SH]H
	P+kZt=,P,cn:5BlcX3\d>=7O-$H<!F&bKXoj2;g7$6V:TVA?<MPYcBGJ;pH\3V8Nj);W*CuO7s8sm.@6
	MG&P8JHc\4W\:9lg)m[<6uR8Pkr`tQP#CZkPAh4?]WpGuH\4-dZQ-8D"OV?uE`gbA%;Z-DM*\P)$bPE[
	*"?:Wc!F!+0P#4rU8M0=0(`p%8L[#Ib5\j=a7^Op)7?IfI+(6]56HBoP5&q/6cW.tQ''dS8m+Bfa5[SK
	kRQQAN68<e*ioMri#''J5#XDBi$\(kJZ(EQ9FtupCu_%''A:h`J@=Lh$Zl-pL=nV;b77+M*.#o=A''_u
	$7U1/_,__;0jVOrb(,>+T[pLUQcNu_m@7/_3[eV!.R3*a$?.LfM,l-1u^YNt:I-u>\W#JXl'';]WL!U/
	d&80O!2g9,MI.;[c#Pm@BPM7ZY=]1oI=2]*+G+e\M"gp#&/:#Kk]LZ%k^^IFJOpW.Do+F5e9Osg/^@.;
	ni!;tu&WrRt@I:R4sS?`H++EXMcAn2_:=+Oc&KbeEKNbq;48N"B.JqbR`nD.)2`p.?h<n3Mb7$R\,V2<
	?9+1FS-BFffXjql)NWTJ_=D\hMP-U.jYV[FE*%<jT8*<N_XBPP"+X]GB?Yl0Q(cmM0Sq8f-U+f+>TfM?
	SGX1maO7Qb8U#OSJfqGnarJ(f#43DEtp2CUQm/JlJArjuYR&3"MYKD[i-m#UKd$(''g2;lB8gfre;6q!
	\W5,P<ol@IdS?jI\k:]eiT3mX&$6)^9VaWr\@mb+<:hhC^p1_OE''r\''!+;oCqY!gM)(]#''bA`>+o6
	T#UA8^q,XQ%2YY*(:jdh_dGXa=?<Qp-,^I*_Nnnb"DA\=:F:-;1K*$SWh8j<E^U[1t%bDI<7rt.]^tda
	=/Z2Tg>=G4@D:5_\[HmS)2<%&%_$3&/H>Fmm4h^3=Q_^iWEiX2]ABt:%mN_B^_R+ffV^(^5.JH9S:K!e
	]GSTM0Yj;VV;ZXt2O"lBlNkWK:Y^WTCB''p^Y]$>_%F=W@R8/&O/Z/>]O[%bY;`]Z(uL?.`=dsXtY*Bf
	PX7C%R!-#AXf*(0(a&#gsd;pl,9?soHUrC532?X3?&^\6@pe%FV`32CFR\VL6M2Tp8pe=YCP2r=//+!r
	1j"!(Ihlnsq,+\l"<<+E\=n)6,B62$Q1i7]qq$/`*K>2I/I\d?N#hkNNo@%TO34D7[lP]t%LZjFoQ#TW
	bWkYtb=])''S"BWJ?l.k\L3U]lqG''@UY1gp0l2pn(^nHR!o56ONuBh&GGDA''ts0l.gN,cNOY''b:1p
	)1kB:/aF>%OBf^<4B\I?+Y31"&P[2FTh;5B2Q4$k/be*B/S9:P_^b4o1OW42*/WMH0otUha%!)CI;ti-
	^55.85SER+p@ofPG!nQ#k*73.E[o)Y%c*2#nbdhcD[$q&nF+p[?,r]Y.:?7dch''kFo3V0^bGO=a.+P+
	+S,0^MA5''755?`1t@7:WjdR@;%[Se$MYQd]\3:#%K.kdc;dRbJGk>PW<?mJoc[X[Z/O[.EFMC$&X\ns
	2:cms^Y8gUG_ol,=A,A40(U]T;aaN''1t"mi1_`e_H/`GYaIbrq;2jb;Eb(jVhRA0rBX0MrY-L?Z+q2!
	c0\<pPG@A_$oh7Pt.D=P+C?H,BEt.1i>bUr-O+P]A1PIfnJT6H&>[Z!Hi0JOZn''G(t\1i?saDdO+%/9
	B3h1.d).fbFVhD-:t-Lfl]sulV__u:2s0\YL(hhfH_&qrSu[Y2&^TeJ''9;hX-@*I$X&FB.\aL@''R<I
	^`)jEK*eBe)+05H9Q[ceOCl$9QcEZ,o/!F''^$SHD0bM;aTi<dh*<03>VHD`;kOI*0OmF!.:[Z50%=m,
	p3Y_H;)@CiL5o19QUBhc%''BK./R^i?mM[:?]8a<.<grBQ>bYXp"$N$P[Bcd-1l-7(nO/+iq>hh1$JX>
	l=9*Z^!#4@X`ZC[/=TcNC6i-Oj/u0Gglps7t*:?\ON@kgu4:%H"jPS^&+-LhnQpTs30MmlhB#iF8>Cgj
	2]b)c_$W?J*-=lf9QrNhd=*CB)V7RqiB''n\UX.njhpCK?XI/)r`ho40!?A27kT[bRcESA0;(00S&)F*
	b`_TR)3YmGJrO_;+74s>-IPXPCrQ26A9#KTjCr+h1,^i4IH8+3WlKdXPkc;t\\G5V7MaH^C][GFNNFSi
	m@74$[3?um`K<YG.m8%(.rs8)R!]9k@Er7cDmNj<GYd%d)"6_@:8u!sr8j[Jb?f+MY9t`squ<j28bs;<
	Lh0M`in5;,!A(DEe:!>5Q)3`(`9mreI94A8gE04%?_``o:ST(b6Y!n/2s5*dGlM\F]Q!c1pDtqlq"_eE
	[r/sqE;RlHgD>><nA<''0LM%A<DEbRLoDHL[RgTT?^A@iGc!Obdn@eQ4hKl\YGY>`6BkI&+WS1naIKb2
	qEns71S^MN<4VtW,?rK/La/).*"@ogD`''i*WB7cno)nUi%nHRQ2O3sVq`GL-1)-V^dc"OQM]J]Di@>V
	]Bf_m>q!M#!(YiY1qI>uZKc+`bTI[]GKj_$5\Kt91hnsr5YpBfoh:Fq_;Qn["l:ML.U`CLoQ[RJ(B/Aq
	ZugOL/9<2Hm&G8+=8e0sWqW[aIC1$*qnkq\1-0\B:<jO;a@''T=""q0j;*e#9IhWbBY[]$6kh]HQ"V"8
	J45KuYoKP^nO=Na*W=pDg$5O_P=YA**eUHb7-h>]''<,EfGU$",_elJiGIBM5P/)]>MSlSQa"6nTZpWV
	<_T&i17@:WFJj]]\kT+PBomDgW1pWa$K%feejVU0q&Y=qo1f''A&`lpOBeFuCVFOc8.5lfWWMs0"1ulk
	?F7u+-jS*/!/b8;`Q[#d&A.+=4Z,ZX#SF1l5P!Bu4SP2L6J[!u];Y6[GC/lHXh0L8(R*gt%="LKlje7.
	T"9L=dkUZC?3C3#WJBohWKN<Q2Q#;O!B>6a!0F+[SLF]2p:''iKk%,&Dd=E8k6>&LEoYDM$pWg!Vq>Kp
	?gIYk7#Y!s>4[5ID4SLuge.([*N^`#3E*K\oSKZjtXCC]$L,fX2,<"Q?55.?dcQA=(2IImJB0nEW3Dc#
	GriE#Rh3P"mb<\h\.S1Z-diTc\mM#''BcMH<b0&3UScaP:%TDNlD7W%3OGZ2p`?eCd=T3n2)G\$fp`J[
	6H^qr%nO20aDQl^hC2_ND%?i1lrL_\#chu/eCm)C9^pD^PQrZ_@0U:41_%44_u]fWOU;[:R<h,uUW,EP
	&G7GN:"2>:`A"mJMkS-09DenZ[,\#3%[>3HBRQJ^<WXsH8Ud7,+0h#83b''3&1bmg=2)o%`Ia<0Y^la"
	j]t''k5+om=pbER#.!k!g2O$42OD=#dZ!lX:d.DQQ#5Y]/se@[b@,OP!-a*)^dSc>cje"8/pu<+(k8[)
	/oujM)B%(Z7cE/0e`(E#*gt%L2Y"X.d"`Gjei''"\OKV/n._/A:tts*Y-)^c?[6?9e1h''4FOYd=be*(
	lKl&C7;K07n%jXshg$:]X0,;>sDPlu_FH7]dKq]t/9Tc;;''P/a1p!b;1m"(W5pinp"CrPV.m#2hN\O!
	C%0;C<hNB#^C0e`@opXbJ`?XI,(N/iqq2os<ZiooXM2;57;OX=io$(467Q?H>"IX;-;N<W@\)^9VI-ni
	PtjE$Z^HHW`)(`-XkSH>mfiAdjC_"L76CphL(01R31dLOO5>EH0@2,R5O.\!4t,>j[j<a\g.N+'';YFE
	>W")"A#A[n&,\^T;qjj4QT=-oW6P!BgK*3]&s[XSYA*NV;[R,UG9Ek#7M;e_]#FC\7/?lnoD]e/N+n];
	-Sm4Wr*tbAGe)E4_''/)=0\G%d\KE+DZHJONqRn^KsI*-c/QR\kLYM.&e>`!d2D`CHC,e7bM&@)s$l7P
	BGhS[<me;.IGp*It.9HjUXOW5!\0,jlQC2J$Qf$r!h-DfA&m^CXc+*C<@12%AUTe;cCi1'':*DjTL.Ir
	>U*Z]U5NPg"\!ssko;pu1:)":h/qppluJK&b?$XXCM"Rme-U!@\V+K2ekNIs,g\/L-J57Dfc!I38Z)C;
	RG8dm9,PcaGrB9+46R^$J\Ej<pP8P!pZlVE<ZP(+N]XPb>ua3FU@)[TjfD:67Ms]oCS2Tj.hF8ap%c48
	d7*s3d`oi[Fk^cA$;T''AZfbRPW]/+=]nKWf_3kC;J#Q(NWlerY1du:f%npSD2GDRVdgR=B;33.G2pXX
	Ll#8U92agF_*F*!#Ret0SNF[Kb6P`kR$p)3gp[S,!WTnl4%Er+2PM%NH/<^O`M77M@hm+I;FS+-aN0eW
	l1Mk`UVcgIQd&I&lN1sS1;/*i%&p4:Kdj.=#Ap3EJXhn".WAjN=cf''%CNmr#5!CZ*s9!&!]T)81<b/A
	Xa[]cbk>5gHL.bk0[4Uj;EKt@#3\;a8U]->QABr)m(`OSXhgf8"4$rOs(at*3:k<]eMjPk;;F$''jOpW
	2YNhD,Ble31dn''FDD8)"pLp"(p$!atd2M]s05:m^@K->VJkM!8jG:\t2?6I>km0L8kkqYs,dtM"_V-I
	?NSQl-Kc[V[&upW2d&$KErU0<B+;F?>qX(,IE=)9<Lu)UZEhBSDr\$ZV1_fBZ&TXRU7Mh\Y5cr)AA5hl
	35G+o4]qR''NN,5hq2$(m''h?,l>\nIi/(5=7h2_87?Fe>HRA^HK`DJSF2E@n@sX,&O:B[k>u-adgGU(
	aC&bhD3IC\F]\.>I>TmpH*<8RgL+>>NZphGFSluB%I[if71Yg7#D0s%nhQgM%Rep8uJ''C,lpWQ6S#Q!
	TTqqO^.CEtu^TS$G!:`N;9IY)*+@sE^=J[224ee4Eu,jMX-_u.O.rUZN8R:^`@hJsW7h_6u[_''*6tg*
	3cUb!r)o_`?&]C<9WTbS&iIo9j_\$_"]CMT-KYm^bVs_cZRp7D,O?*a(%`8F1P9bCAWEQSR1V4UfsXqu
	\d".2/#b>je?>$LLJ*B9qnG31pso9eEjLb6?5+P]q[PYECSSe]J$<.;''U9F?Mu9cY(kG+$e''krsg,,
	c[E[OF.Bs61/+8oBjtNRC<Jj<m`SG#DK*7$_nb-q?.j9\^Ga\c$Dq<&-Abo4e,BWRe(G<f,8-GVpaho+
	j-Z1q84!*(](fo&ja<6-.qDGXJPG14+-.*(3Dm^T7UID%b/A2WZKfV@YL*LLgdeA@a&EYC#0<Cjdacub
	g6K2SFIl-@mmV_]KQb($OGZRPS@N/lQelC=E>FTpB"2.RSU.0cMr!Fn)/h^mQ?4bGSo?@VcZARJ\PF8m
	Ng/]Zl[-''OY)u\GG1DnNS36PZ/(>$P28YogM7W6rD2TO5bL_cXe\eqOAZ%dW[kY@s;QlF^l-PiY>4mI
	`''r!?g]@a^-6:@<NUgq1r^Q`Jd=ZcN64lK@dII7arH]FWZDTI:p=Po;O6;J2uoTE7RHWLpU7(Bg$^+"
	u7Rng#%0AimnR<s^O;J/P75W2eh3!4c2Bp\.NHF`8[q7FW3a03&$L)VEZ".jK^AV;"5VkGC?''SkC*n[
	;2uloAB:GL7/.X`!<>YL2Fl\aNgtmdsoZe-,[D1Jc,afW`G=m&8j.F''*BI''JFFMBJ-b+>m^u4mBU#A
	$;n?ADk.%,oXWM`QChhmU=qeS:Ecu;]^.Xn5BeSD6"Ios`HKd6R]q8/j&N%A2''_(uc[&*GM$GPNfh^s
	-@:ZgnN,C]O.R^3cg#[faGFGY!_r^a6>A":<-K+J5?fD1"_I3Z4?K`igUPY=#8*t(;X!1IWZ,F"8&PH2
	3L''i(6@F^JhBLc]85TP$tEa,3J]-pb3UZdP5a50%h?/Kf4Q`uf*OR!-!4n%5F49&1MS;:+tR\])+C$0
	iaSu8E"1uYb''LiuPpRsX99>raW:rS5Go:hR$p0kY=Q5<H33Sc4j%f[CckeQu4o9H33E%;";Mkdf7DEZ
	L5eS-NSrnrbCY2cOi7VYQRsm^\DFq6"Cg-Br[h:o5SjQ3ZP9=gI4P$?ZG^:Fm<7%B=?.PII]%^N],$\Q
	e@LSZ(.#Z10lhEPHkpm_SeUo,;N&_f).j)jJ<sBY-)XhGY:"?OFF.ku%$,L$"E&M+&*tr=M][SZ$4Ief
	N#thS=FhEBI5_mYM!J,8NV_M)XgM$7X#\[97[O''6Cm3[![@pqrDJZ,P<o\c!C,"7Gq(If18:\]sAuAf
	M%=lW=>t+2pgtA,T<[AEXm7t[@Ks6+(+JHo%;/#pC(L199/T`Wa2NnNJ7<fdX;[\K/aq=2):HKW-S5la
	d5.\l>+<V8V!VFg[82gIu,2&5F`^cH[FO1pK6LelPe;N,g__&1lSDXN4[#JZH-Gbb!kHcW7dn''Md8dj
	]6j5k`Jbo>d)>ME+F.ugeE<uhf-XbrCl[0"aASJ=+\OAk>nK^C<?8WPA`GrJQT[36+-!c5GRH?+6Fs$''
	d%0\91+A/#Y(4ooi_ts-Pc3dmlbC8N2;gdk\7B7udmeOM6[k[&2YiLTn0%h<$j)&''eQ.<1:nIP[''>Z
	5\dIIr&H]D4cZ`Q''.g"-K_H*>gj[Q_2V=dhDk\O)kV)JKu]"!5DMGmB?``!CI!1c10>At!sQd5n(Oc?
	[=#n7IEO=U*f+19b&Z+f=L_=@dX9KA:"UpacPfM15TrH&)s5NXRj@)RE48[-Xc90k6h<G8[D+j#pO@Y*
	$)<P4+q9pMWi*`0)FXY)Jfb"O(+kkcOH#c[PiZft1nc@N@->@2>3Ti]Oqo%OPY!&N>Us<X)2%H(b#T/7
	_h)p[(*N^?sM2in78nlN%8/1h[ELgm_njc2QY_n@A"Ok:UM[-1sFU]=^TW>(31iQF(W]e1DOdWrdFl`M
	B*5;U(Dga<"@d''*X,%R=ja"[;nQeO<$\F$Q8:g&kjr3hQOTq5541FH1%EFP4/Lp&u*!LcWtha*T+?eF
	t-DZG3?EKR(TL>?.EURKt8FQm]nJ,@cbhQMt<!:gGs:oq=3X6kE!EqOiKOFoCGK7+(GV]jdl(8q44q6=
	;sr"!LlmJZK&n)lUDcN="YP^5?lfL1=CZFf-n>)07hqXf([rYc^cNWUe7!;n''(<-''sZ8@;`LI7`sc"
	`0:b@?NTr/$4k#4`OfO-@]oZE1aV!^!*L3C#h;*7Wg<h3.XLiYdN_psBgU63<F;85\R4)9<$&aB-]Uh#
	.7/0Ko)e=q:pHaf/@H`P#UKoaC-I?Om(d@\rmth(U+tC&\@S(]r47R;tN?qk0IGbf:<0p_P;qb"s0?W_
	98?7E&<a-W;:>#.hfa;TnC)E@MXI`f%WR[QD:_2C/EUD+jQ0-HGl.2])1N_''"07+.O?\LPK,`e+rn''
	j:hbnCj7O>RD6NQ.(**2tgXAb!;ipPJVY"d`6WWlT8M<i?\USUBTJ+F@]XGCc6H-4r@[VUJ2!Q4a1(Ph
	hP/0l"m96Mf4"P)E(77ZqIHH.R%''qhKkRCmeMP6EAt#gh<i3fl#<FAS0;UU<s1W+)H;IdUb=k;#rBC]
	XQIJZ!-hM&D9tbI*--u8b]d=aK,:)HQ$a(S5(h,UG&8mPs(aM,u9e3/]^iFljS3S^iBE^=/ljImS%pEp
	8op-(8Np%?RLp%VIMiq_f?oB^3oJs=ZLub2P%b2P7=i5fpCQ:2h:d_Qoj7bl>:O0]Ya"b=iaMU\?kR5P
	JYN2`_aLN*B?uAMQDADe/6Wckbl\D%sYZG\`fIO\``S]pN)ZbG7dQ)Yp"&Y''^mC+q<l#<)G>rMF[^hl
	+8bmX;4p87Bne$sEo6Z9\U9"#Q=TE#Og5(e[''/Vp[>!c2c"#C2TiHp(o''qo*:>*1-m]0sC.pmEmO=;
	4)fpFA#9Ncad)k_''f5.7f2;7d.%]u<-Yk@X.Rg(AikaIhWa)2=j`gsrBoIL1Di9$#dPcX$8*&@,r0f*
	DW7R6r54-d]q^c[,b2+?FOeMI;=6,X!K/Qb@E3''Vmc8nF]PcI#4SW:T*92a>Hk@mVK8ane@R2:o\r[]
	,69b;n&<rra_n]/>"Ob40NS4`5uLl3b7S(8tU"EM$e.S(HO#"R4Sp^WhLZi3_NmZ[k:T"`9sV%Qo$B/<
	C5[gdIPP"8bU98H\sm?MVV2fj_,J5]LL?4rq;15c(Lt05e7*RlnBNnfD&Uk-''G4mgt-Y?r"(:G&FU_Z
	l8nC-7O6_MTBMm[H>;,,9k/kQSsVa%\ONks#aRc:.`Xmj"PpYAL<aaLH#q(NTq*EpYEPB"eF6gln+XKH
	8D)N829Zj6l7%^+pEaLM"mpVO/4KHc=;%MU"I2RQ\T`s#m3R+ZY&K6R''??''f;b-!LiQ#$6rKEk9^Zo
	Pt.tEZrZ?;(L3:''THnDk<i8=aE"V>fh=.[h]h["J''"%>_;W"TVIcepY8:Vg9t%,M<+B^rNT@8m`h0R
	)9hVSpM<^nn5*iraFK_7iQ($[@/97eacs5U3`5tgFE+o&t/GgAZ''[#"ctV,7DkqP;EF1s<#7$''Q!g(
	HDRK[H:OUR`#0NOJ`+q3g554sdmQS.p-e!aDKiuRdWJe#L^2r;oT7$Rmhg4IBhM#QLn=U20hcm^Q%64%
	=I-$/lmSp)saTLZ=O.b$[qo''d9[pTI(m#''ukr!0-#Ndsg=kc''gd-2u#-Aas''<Vj''=``[I:f+hF<
	kH6tM8CqFE9LM''X??Tte05+1..Ff9Q?Cc)MI/i.ROg[a]sF8%X*nek=Y4,uq>9^C)K#qd[nfe#9IX;u
	G^RK<._7t,Z<m&@WePOES5o,+I@VefH`#W6Jp$::Tg,P(8t8&+Z9^[nAGYU''$(\PC5/#IjI%H#'':Sl
	c]&$Gp#oDo&h$Clhq>S&[^iD^k((^qu5I_K.Wb>AZ''X[EbEoeJ(ie)NBVj@PN^6<k\BP\Zq2]/Ps%q:
	ads[l-8")6R+Eg@:[H[1qmTX-?7Y1FB_#JCg7d,??/i<JE\WH?/+uFmh]ahN,?)o=;^FH0ZP=)XnG3R7
	-FoR.eWG>D1fI=Ab>5dsrH*Kq]4[kK)f=\XD`f:d4m;^*O2(Y\n,=Et>YT-PL\TZ!nFj''9I(]e^]$g5
	?K]2@uXdnj`kf-4G>;6[-=P%=/%J[K3l\*+kqAns>&?nn.I+>!nm"mq#L+(s8?p1X0#KWLB=ZFQqP0D-
	:b7+i]>g$qH,Yc;eh$mi&J?c^jonmUqbaFHB9n;i)CX_$lK&&q=.>Z]GRBHOQjn+GJVn2p=mp@(Q1$:C
	1kCi$/%o79(<Eie,\>sjMD-+p48)Jc;i[a)>9ZVb\A6!+)+Th9A,j-CQ<hrb+Zg=,cKCJ-A7hKoNs6:5
	u_PLu%J!1S''YOq\SmrK<8Sg&h&:9)Lm#AW\T=_=J8BB5FF.ZW`n/T%TA+gG<sV-_/f928"t[orFq::+
	.6c&IgXLImd*kQ/PlR4tDHpX]!''el5>(fsa"_>$L%WUs#@-S*Dk5p,&fuHSu^6)S+%[=\2E6''KuK@P
	q,r8c#:;e5BkBC8;V$DljsU4OiHQrJVcu\dunY=c)IH>9<$h=8r)D`cgNKu9q]Ol&u);,(V*A$q"''/7
	Kq<.`S#L*IkaCm.:VVgZOqL[;*Jpn02nua9gA*aUH_h2jO^0?ufd%](P%H4ZD3ciCkDloij`P$>`ELEr
	#h&.?2]q].p?qhEQ<)l1(@U+p1WRGc=R(Y>&!9!iF)QjAF_P^g[[)Z$*7]c*0$*Wsf:FC7&5529Zu>A>
	"!U1>]Y*Yc".:@\4oC&Lp%s;IjO2=A*d?b-qu2Pr0+_FhrH8-"ip,L&^N%]jnE4XnqKcV:k=hh%D:X?2
	VF"''Aj)^OnWbit8/D9`\d(-Bk51bZESmSqni_Fd$K)S7!fMSIbL;;I5+-e?l>B?8:PCM%\+hNMpg\l1
	b2ic^d.a]ZeTP&T_H@EOXH_,ZmC+Kfc,YhlGakSI+XIjhEKu4u5F$K3XNun8/+F?^`;<OIj5Y0)m(nrD
	)iIdE9\!VuXe''D$3Pl+NW\E#+=f"AKM>C>bJ[fLT]HKs<H%kUS8hY!&Xl165t&M:3*<7G#J&ZeUub_\
	n,4a$cpU3JW*A7qPR?*rG/WFJ!$A$4]@P*rZBc%:#/&@:!rb9;_td!JZph-sKB=r4B+Pqs[-$N"L?T/6
	!G,tQ`oA7=\7>/_hiA&K"SVfkaqVk.H0BpB6O)GRU*q+1cRGDVl)`&s.El_5l7BYg&AT7XT]6F?A[T)?
	5#pY5`?-\)-1OoJY<0R9N^379,$0Gd=k''nN!V\g-U2r\Q/J&n404L_[Ea"3KK"L:0sM+6kt$k,rIB$b
	oPt>QN=q\.[%SfZr6Nm--%4-fu$b4''hKm=a&TIFUCl-$@sgBZ&mCW3*.t]Y*O.+ct;''Rq+3T"C#*Sc
	ocN%-R=Lpa];ckF(II!I]q*f@6@7*aZ\5+hCUo#[fhD8;U>8b?9^EJWfi1=''8Z$"*.D/,D[,Cr`g\_!
	6[]]MAlXZS+H`]FP[ln^IYKbj[E><]#a)tBCLWHuIlu(/#DEb-##Z3T>M_\%d#cG"H8)ai;7g$''<kBK
	j;<eR3_C$Xg,0V^#NQRKSFf`L/*5R#-J(k!`NK-JBs%b)0''/crJ-d_=UchZEll+=W/Q!Tr540(AB"#i
	P6$htQm>o6%XsL5fVU\Pr#]@0%+VHgfsL-9q8&Rih[/%''Z''cM?I?B(g!SoFNpl[+APah,N2KVK$[4f
	bCXdHrF3.L`qE"eHt8e;C`5nEP-^*`4524/$[$&:[EuWH=)cKtab#''L2L&ShO?9jD-?<);?B.H-7&*i
	\$<QgfT:XGkq6`c#)#].^[!7ojk4<(UKWZQIbd!k"?l<[e;ou0t[aiODeZa*6_#LtsF$_Y]ini`N:UOn
	)lg(-p/WW^eW4]Tl/(Mq&R@ng?H-2*$Y\G84D7M0_H\`"ZQ2KA!5-k-T?iK-&*!g=I`@WT&&kJ1^AQOt
	^-ou34;@SJ#0cbI9R:J]W+!i9d''r/7fD+$=RH_T?QcEQM6FgH5#VgO<OE$Q<BQQ;KHV)+q=Y(Egsr4-
	:Dk_g1`>X"=/U@=bsa,Xc+rmYa]f4Kqp^/-)QO.QfA6cZT6!>CGlB4@>]=2/eLX>R9;f7Uk$UK*V,MCb
	j;gbk^&IQQ-7),''WR,NUi$U"R<]1"hCCBKJC(TjEt7+<6$g^+c()lKm:^PMc34a+&7,;urt>_P''AUd
	BZ,bSD(MH[&PudR<gJoA.uI$"(#b6AJ5P0W(^h>S(kg(M&JtE%cUb\P*a&/gbW;mHNJKPnI-SV`r3GM0
	c/4pI5[k`J/EF$Y%JrtW;_gCM>Ci#Qp9^<"rB^i,p2L"rB`-*YKEoHH+rElBd]@?YpAA8^6RcYr18OaY
	^3;1A+%&GZJ?"33^R1BNIl>OGV"eBrA1C^-PbO9-c,e-pW&oZIc,ci52>a_8HL+''<,+C7<T[j<1G2a5
	YZGsL_@gaAG1Zq<9aiU^11''fhFP+LV*(C6/,Kk$1r$O>?E!B26*2qhf$^_rFY])[k7ctAY,?E%4k+fn
	_(=Rg+,\4tq1g^A7T(0R#b\0j&rA2iQ59J;OBREnAbNXKK,,Wso4W@T\rl`:%gPU?4_''JNQ,Z!08=/V
	3g1A;1(WnOB1).&re=/Brk<9[<^-)]$OCG.Go_C4*K-Ot(mn\CaJ?T[buI>^>eX^m%ER.OB.qP:MgP0G
	kbgXDu6-''pp:/5,9iB\CQV->lFNh&/nb"3Z+j$ro''<4?ok2)r^gZ>@k?$l+a^[?8D"P+q\0Q4@X4JG
	BG3>Ncltl"/DW''r+Yb-4J%@i`oDWgBO[ZF+gE`Te[5AfYWL5ILD:lipe![s\o?TV7V<P:Yo-p4+;\R&
	4V,#KbsuQG_anoH''6MP923"PV/4n9k"n6\3oBd";qS6%0/6NLb9ZGkMXpf=CZ/dQ%ZoC''kp:_>lmG0
	&/FpqC!H=F;MND.`+k>aS`:0PA''[2pP\B<3[Y4l$AGK<Q;''%l*[EN?bERmeKu8c^)-i*7Y6ErY8mc,
	\8_!*>L!,J?8)peOriTmSLMiVkhu#lji69f=AiMiLrPYT_>!<6Mc*''7>5`5**N1<Y$iLB+">cW4#RFL
	mMbAX;Tm9;1XF3"83BJ8$nGNn;V2jW9I.rm18Q;U]p8N6B>2(Sg=`YTn"#`@4''P"#l^e''5L3g5`&)W
	W!"tK''*7+@D)IMh(\M6ID&!r#rUV7]tq"nuN1Y7MdUc<L,;GE\_^@0O2!_:.XJ53<<r)KN?]]d[;P"b
	2-Q$Th9nh!`F9(T9H]b%mu@2Pm"9$/].ZHSkuIo]!S6XnJ-)-Fu![29nX:Su406-/t;9kR9*#-Csssk4
	a0XR?TAR+#pYJ?s*(DXn)$gn#ZJdl^o[[4!kkc&8BueYCn^$;JC7qFaW]dBEI%Cs#HKU2OG4S$$''Ss2
	D;",[r1uBWVFpa*;lk.^I/-qWq(EnjiI>7[N29?W!`rra''p#9!M5LING)7s+<!>`%*`=9Lr%;?]fXRU
	j&F(ZEf?p'':th6c)(Da$9R/d(T8t^s?>hO`96j,YDpSUTnU-q34ajutkMtRPPdmS.nd:NEjd3WZ?6^-
	sL),S,Z;>QBj6`b*Bc1h];`DT#o*n&8l,9X#;SGt_iaOppGO:TOS&Kg7-RMO3VZGh2.2bEX1''%1hQif
	J61Dqm+/?j.(?<4C+/Yi5:JQ\"E_?LZ^_0_e$5Kpn4/pY,JAAsub?-F.@aM)gJ)I_lD_+<a`Thm?Xl6*
	E>heZZV+.8\J`lUKTVGSI5.Pe7C$0[Zi/YoL8eD_:)EH\G0?rsLMp:FC.nB)L$%^J0GQa82h@#emY[>4
	.[PJK/\5OW,f^!EeZId:2"h>OgYXj5g@&,0Gi@aA.04&6nTqT"ue?P4&"^Hg$5''AN)GrS''1/[f-2#n
	qM\0j''l^o<)5!9qaF_%-`,k`6<s9?A''ZYlm($&N^=TPuc;L#rc:C6W6:"`Q(__SpAr^0DZt"NQs2l(
	+VJ9jIY%0;3mdY''/Noa!p%u$a3cu_)_9''>hg>0j%SOZP"GCFG''A"oWp7D8CUi,VCa>5g;??je'')Y
	^r''<.@BcR''Mm:O&l1b&P)8&m*XjF<qK3Xq5V<*.SNM1X;dbMuK(<&N1?l95N^,mqpN%6''3*L8?ep+
	keH:94@Nn@>+-&)_R*^0bLW@9Sm>:if1Ca&m&;n9K(G^s?Pk%$lb@@Iok)8h/3onMq@MmYN(tMK+cYM9
	-9q%$%m/]M&bg8$?Q[%5j^.^sT?,;Dk@$TC1fM.PHlG[Fn]EC9udRfgp$2;OO`^_G!C1;r57&_*fDaE;
	p/&^Zo_f''e]eb`M.@BFGb)c_eASGppBLg(jreCH*ej&:#++Pd;R?mS<;rBcon`&''us2b-kl8DF;Yj[
	h=>-q2]G;7/0lNookjTC)b2M%*^2?,;(f(-Hj$?p+nm"nm4[,\j)Us@Zk''qFU&OD<,?dXp-AKSE9Gp@
	/f`DmUj?c>I+Rj1Oa2l47Q9_il]_l68n@t"**&O$%''6!@^kK:/DAjDRVV,T@h7Y9l''cX7@$:3]7kXu
	QTsA#C(8=B:doJLSAS-GYbun?]^rZUuk_P5Jafq7.mN#>^LlRX''''KN&p$H^uq*-=Rr2_kjtP1^9"W<
	3sK5ChI:llYQO<)Y)VCWhFn''VNW:6/kZ-@$8\`VISm]4ba>cojZrG#6I1&%Z>pG>fOrDe[''1Q+oYHN
	A__657g)b7,DTK=HbaoL23WG,\rP_PN<&>-e#oqJGLe\F".8S+,''8>WD#B04<rls^GB$)6=(M7gAPd.
	:bD9o_Nr%.6\Wb[S0EZ"1EkL)R;Gh''?BV_g-$b:oDnZF7bV!CI02pZ9s2$-[)''5Fo6`]hmI5.T$JKC
	J)^7+''BbG:,J8c5j!RU;$[?g2SlM9''F-i>?j>Dp`i#?k,)D]>rBuYe]*h#$g1R9<\m$k!t$4i0!SW<
	%R$;Gfb-m?Ym^!c"Tg[:>>c*lS>OO<c>i1*r?W3>0E`>k,k:C@S=r]Ed?H/Zs8P-8sd481:9<4[VM5`^
	$1)d%8oKM;MYWVjKa?u7rFaBX7B1EE''r-KndB?]X:Gr`CcdbJA8,(sU$WdA)#p(b6;X`PR7V4aL8S6r
	i#?cHDTqES6q0eL]i@R&a1LF*au+I@FIR3u,''1O5*q)2Va3Fp\^]44am4OMsF`2-K?Hg$8N?P%A$,)J
	ll4@_r[[bG_(8s)]]fEKX,L#;Q4Aa7JBI9i9u''m0-9tl7sdF5P<^o7)]`(u0P"/eC84BqfbI$Ne-sNj
	:MdXsn/ls"(@gMYU],j()Slj+bC7-lalN(r?V]&o8$UK%>#_&$SHM!AT6-idPE(#V#%UkbKY%)%a+,a=
	WOXWJSV%QLC)RMK:_s^e^*`ku,l-&a3W$M90E0''(a=F3GB:G3:<on\NFC^dEh\`''4NZ&(l8f!gHO3)
	FV85[Q!,=%)S^J+YY7Pdo#L_kHF"\lo"ND)g&AKi#jZMt"]h&l7Gn/?pZ[d+.gqui0qW<#>V;b.=;EF8
	$E/:$RUh5\u8bCmHiVc%,[f.--_KE*b+>YjpgmecpKJ,.jVJ)OW3_`Z?tYsFhKHeK<0SuaLYf&WEtZ4%
	:k(lXPO]B,NRR4\3:lM7BE58s&5e#iatq@U85chSf>@^P''C3T?+\0QfAl]oY5m4W;#]2<QfhOo=&eO2
	P:/UFVkd[l)NpfC)78ncWO<UI(lr$CtTe>Fp`DKKSqi^YO=q1OjK''[skn98bS(SbG(E8Vp&7@(eZ?NX
	tdqZb#fRI:a5*I*?kBe((GPchfjGrfo\6Sm.3_C7K#4h_,u/pj)QuO?e((gA^+T+@.p.gkOkSLlD?+]Q
	+=P/(3=^=E6ia:6ru@QOcIBUWfG#b&>HBGZHM$enVcJbStGrFm=2WjksM*1\X/D!<&D#Q0"GWGiLV"94
	\#sX"W62A=m$hX,)Th$e<tbKWpQ8.("M>7UW?7d)m]P:#kB&SXMk,g7UiWS0=kk7[nrKPNK>Yg*R6Ro4
	dH#W762''bLJWa0\#?,@8=Go6iNY2)cm/JW?i?ILHR@pIH/QE/8cFsjVKmSSE&6aM;Mhil[6`U^@mVpj
	oM:!",qqY\Mb]9*l*L2!oZBin,C7T6J`p*56+qRfUCcZFZLZ(t`cc1I)S2^7$+f:7<=7>[p[fr)U?1o.
	fBu2rX.Y+ogDtPFVL=aMmPuGBimMU*s62o*s48LAr1N5bh.711c$eSO#^%*94XE]k8QKT[&7Co8T>0\b
	ri''Q6aK\8;,cP,f4jl3P9,++OrA,^<5,2a7Saikco0b6F;-i''9!`^Wl#X==f8&KfDW_-o>F<o/8gD(
	25:B^k:Q?lZP1r:d<rHj!?qaV^\IeJsg^TX`8E3id:%fG\5FAI0hJfU!pOhr-[1KO,_:*mnmA%@qI&T8
	a?piS0.1-/_e>)\u17[?*<7eC0u=!D-d+J6glMu)QfZHaJs)_FiqP`U9nUP+G7FC];WKXtaN;e]sSH0"
	``<J[TceFV#TT:=X8^oos"Ze`l53`+@p@q0s`4T1qSZT:IfUg3/5<''J@D_O7Z5$B_9Y!/!F%qXe6UjJ
	(_\kYVoc`T"rkYiJTJOc6G]-?Z%sF@Be$9%0_&p[kI`pT^#S8'']^Fh,jA7_hNcc.&eh6[Jel&l?VfOq
	\OHUd#jf#UJ[>hD&QtEWd@@RL2:fsTnF,<KhS*l;^h@#]krqX@]gqZd#.oUUEi*uGKouB7+\.@&Nt0a:
	[D8[9j0.cV.3(@WUmOqT!A?*BhS4aI;AN*<-@V3M%bKX&-7r\4*)hGpZ<apeGPq5h\C$/e7aKG-UK[-6
	)H2WYYW''can]O+OQjaE?''->Qq23Qf+m4n5Egl7Eo=>g_bmsAkYNk-g^5Wo6DjYl\+YiiifZqK6,;i\
	2pY;_)1G!N4m!dWa''ci./PP=M%/\1&p(>tdl;54TbT%Td4k,e3%(8EqaeV`d_^)YD^d@BGOR[FPgX!D
	jg09TFgA0Stu!<OTM\u@n7[DCi]CWS$aNm2t*)uDc''A`p0;%DO8;p"0;i;G[_q)Jo5)aE*5cP#gP5V/
	,UJ5,s!FT[F6/Q@%#_+$F_?T\\_U''P:ruM+l;=6hi;W:Hus4,)Eb)&+;/bqL6kZ=[o@E''\AM-moaD$
	HeS6eGXboTn!RP''n&_jB,fDtKX2:0h?/^<\:<r6OiGWLXC,W?bB`oq`UCHIZ*XUZ_.@mG?mLd4Js3Ub
	uS3D,%?csblhpH)sRGrj]Y!Ot%SW-1sJ)S8gGbMlf+Vou*2plGCVNK*JOlX%[>Q04QZL?''<W;&Lb#:(
	QC0jfmf`>.mKPDb?_S&d`C`EDLHEkU7l?''F/%..>4iU*Fk3!^T?=D#^3V5[a&*/f$M`-WAF2Cr*:eX_
	;HW^R,VJp@A!;3H,+gC/3MWgIUV$''$QLt_NpMo"aOIu@k9J?:s?-4H\SA%QQfafeCEiU4eoW5hnHU\m
	THM)VNmZ_3\RM\7$^dM*1/''Z6`CR*6Ntl2qHg5iGhaFC\iOT5&`"+A?r3]9N2=]U(1OB/04]*#fcmS$
	a;#pS]pOZYh#_YWl@,D#-mj)VqTaYcPhT0H4HS`saS&bbD#&:>>@)&\>Jfhh\:L$Za(_FhXGRoVg_8F[
	btZcqghG:_*,HpSUJk,nVCRBqM.P6%O<]?$P%7^X,@<X[?7Xo"04Eb@WbR''=EW;K+d74)+<gp@f(c)!
	]!ZFNXIl"!<R2D;]&''@qh=*&$^JKaYnVV%e8#5,.ZZ.ppD[RG7&b8%,((`j-I`Tqc5+Ep7;"p1=3$)C
	^NUQ38V]/<+%l^eS?a?"cGQ0qdQ9%Zc(fh7pk4h4ZbpQUpaZLltgQQ3%4/VPc#YV6CRlr4!#XYbGB@;>
	7-H$NA@/onmEP/2O!rtIAeQqa82N)4^l&JVVa[Lm:Tl9UShRXXZDeiam`7OWpIaSl^gOEG$q/^.JBl''
	Gn//V=aAcnbjLYf"DjHnj*JGFg&AalD$X,F?"M=''VKP+7lY6g`mn"Z>Dh_Q&Fm(WI+>C<n`''cVGUDS
	V]"S3cohX"bP#,SVt,#jooq3*ec":.,OoO)''4+Fqd3M9d"q$)/)''1J^`''RoSb<Yo(Q(b![CmDfHUe
	PIqf]GG?(a''8\d[c]E0^L)1Ys/\O_?:*G=$3JiX"6gY581f6IW2\AC:#/?ZP8qf//u5i:W@<%asO2Jq
	ilI;G(>:,M+TMU6Q6uUpZ88k:CaEgEeTlS-,ub?R/a2A]W/D!rb-c_nBO]Y[l8:-r\JAbU#fTHgM;)Lk
	HYJ>NL7=G?q8!K''f??%L`0]7Nr$=7Z;/i.Um!gFA1`^bTV!u5hSgjY1h?bDkF&Cq<OJWcT*B;5TirS[
	j5<eKbAF?q8N;[ei3IhK=%QA&jOS$:mgUgl<f%`37/)A`k?+]$3_;dOSXVVrFRINH3!V+^%@s(NI:d)5
	AEF3N5''QkMaI0und--mke)t?B*0*3A"3V?6kN)7-02KgMf?G9ZA3;+L+<,JL7b89im-JM)h:''t(k$H
	*s.Cl<apP4*,L$kokpJndPFo#cT69?P/@oMMa01Y8d9k?_K9:Eijk)a+u5m]PiQVT+YLffP?$6&otP&s
	AbnDS5mQm^\")&1gV$W`jsTt9ALlCPiW>,1Sugs]XD^*g<=&KhRd=&mpmTE@iB3tV-:6+UI`Sh=#?CaS
	XG6jVP+T`Fu7a!5e43uR(uRbY6g4*/V!o8L7R[GMYhOAY0LX/_pee4B-scok`$^ZR*7.FF04P1A%r(@c
	Ts<[<cs$:C[nTPt4t@?BY;FiT4<Vm6"Q*f26XI?''1VEWbG;3p[n0,$cQDJGQ!ec-](TND@M`''''E_g
	>57&qfocGO*k'':!6/)M2D:+(48>Ms@N\U:i=sIPU>,.WX(np:o&*=3Z]c\''2-n:B@emF%-IQ=E<+WS
	Ss67h\Y!i!t0,I,F:@,W_39h(uN0''lcCf"8*Z&[CYCL$42_*mQlK[ZTulCMd9$k!%FR3\,YTB"R,(.u
	QG/M<i1:.I]*/%gV0m*F^3HSG$t)FI.7#-Y?''q?,[''QeJC"4^<mpsjQ*\XY[@b@X4S0=NN#=%of\]U
	oF$-tJm(9Hl%B?-)_KEa=%q:K''Y`VpJe0<R.obR^;NK\2M5+?;hit(eW)9i_-oh]s"Us^[LWNdoFH!\
	5YVXHpo-$b/+fi;@p/i@Oj:aXH4dNq4r0#TJ=^6MR@]7>"r=Z;F]4R*OS5+ju<\_/pfiY&4jXr?TI(h0
	GF)Rk$k&%j\Z;+EJ+`h_B)iV=+d7+4OX^(E]*h`e[_km`p4%Y!?h8NqmR4o!0dnuRB/e#W=c#+YZD)ha
	`":FIAK?''n%6(<g3nb,i,0nHS))mf#gOc*2mrX:u*I$^L=Ae.bDD%@FY7Mq[Lr8,"^,De"e!Su3LiC"
	+*:5sEBBnL\A^,f3f8k,9?1qq;iZX@mkApZFQ:jD\9Q8%apQFZ^p%,3J=D*+\&"k=ZHF4Q''oJ\]UBA[
	+_9Vi4TaIS\42A*uO[H`I&F[O$L8>b2DEgsK1pii"U!c%&8#7qW@K7U''_-l6FnZg1$Qq(0*]7b=k:o]
	s8d57tE=40g?U<EX8W;ZZh%AcNl,K!4r;a[<#*b,nYQ0Ue]Cd''HPW[o*oeMR>q-9_<eSKgf6nsG-8O!
	gEg)(E42TsWd:!6dob-[`o`jsUEd#GKY3?5.IHe98`f?\Ael>E\RS>gO_CIsIYWRo&tZB8Whrq]N.[/C
	A\1#AG2NKq((:pJqQpnL!qHM''g1BBfai!.Q,/U3S(>U$qM%1.(O9IsR)6qlQ%p>Fj*tq_i+<FU$^<.X
	afKdr"?,*9ek-/(H^-@[[k1e9$(03S]NCJ@m-VJL;<q(GoE."Ztg8K@ES:&YGS\%i>hGDF%7O@)''?ul
	tGM%`<uC''+,D,_X)>oX$<s$rJ''"J=j]0K\PEa^cd;o''GlPs#t2%2i`:/bL-V%X9F)-j4:WW,3-[\-
	TUltD_&5P@).^LUpa/Fh5eiP;Re@XN]r0VbN5hf&ZSHeJ\`\/UE)2`ufCF(P.em#YCS>Z8eAuM5=#)ZP
	F/PO\EJ-MI("&]U3\>%[GU-sm!G3V(?Ps%`\MjlfH>GPm''''S,hG-Ut.R&?_[VK:_b3O(P\b5mZO"*V
	&hL,b\*]c]H_;4c`&i@>CV439m)j9tkDo1qm7"/8SoJqI`5CU@Eta=>m=bHh/cqn+LT<)e9#\^B/&pJf
	ad?d%VZ2Mc''.8Fq";%cj#fBpS&Z4mU/D_/QZ4$/2,lD(Nk:fHQ0p-$mphDS)cElH%19F<VgQ$"TQ6^.
	XHKdPc-$?c4![M''m587]lh!qpenlg`$MQWe%.7)2]8gIh&7_EK%P/ihFoW"(.jHBftsr?Yi?akY]l-Z
	a@s0hTn]''TdnGr8%@<&:V0f:olX&;K7)!mQ1!mV3l]g[7Ote+f8-uT8gsBS7+Ka?b<W4D%M<F$=Q1"T
	B`hHf1[*3uo3;<tpjc<i_c-foad"K!(sIEqD*lM^![,N>,@hpK\j>[fi6W2XUQXKe7HX3&LL>=poO8p''
	ib>]"[i/E;Q!,EQk#kp_%$mEVY#5G[hab<OY57\F#(B@OC4b=51&&=CO4F&t21A>$"R#!?8>7&^\]_F6
	1B&+gVRUBQTGhFB?L9;G)Jq2V"3E5:eccbXQ73khVg0i?Z"O5OM,>QlW=8@H/Kk.r/ac/N)k+@#mci4>
	Z1YUSP37@hl3*Pg;3j<Cn-52jh=P=fe@BqX&FNPI:UJoRmO<;Pliptf89T0FL/;=NjlB-u(+Z[OJ^epe
	lNA[=G2<g20^(in>@O=O12;V;CC"+5.4WG=W9gD$),!`,Og2@VY/p38FMiHoBZ[!@:Sg#$j.8H:EZ&(F
	hm7;LEg>N\(+Q&''ZW!#gA1TTu\QA"#.XT7Y0BUg''.[K=U<nT80aZ6D07l%t-EN"piY_Q3b8Lp8(cpe
	mG<=!`,/U(X\''aBND1cDmZU1*DqG$5`e$MhnC_.u+FUM2P5].a#h6Ck_LPr8N3iG;*E\2]%6[]u4s%G
	sFX2R/X8gM!&cMLM*79eA]&&R<M"7id7YO9&%VCoZFp*h(%ho/m]M&iF.8Y&h()JP04@,Ol!9SjAD>_4
	^?_LYn:VGL[XI\Y2*^bEqH&[XkPMW+3i9gf&Bl:QRqLUJ3W^(.RDk0LKII`!Y>,6Cib2Y'']<u^@_5Dr
	16e\PQ"rm%_''mTH*cekOW<-0BMHC!MTnJkPC$>#N?n7'',AIA8Ek^idGM9RC%\"]MVI>BgkDi8rnu\F
	eAES<nNmG-3Sp#N/P7\4[(38q.WH1R$?D_guG.E/ZcLn]FVntUMRbUia"s_V:C;t8f$e(El*ieTSjIh1
	)dH\,/Q-@b_oNA/7=5al.n49>-pSQ6lQK5G&-9!.$=%8/`C?_0)921>@X.!Kqe`3d7mQ&qr[75ekb\LT
	(U#k]@gf%ADL5hDpb0@&0?V;EQdBD1F6Cf%#!jA"XpWBP^mXcqNO=FHbl^hrge>PZiHW?UuZh(aR[QI%
	2Y+s:U3UY")7&UcnaUo[t\hQr4]B*8.;H)hOcC$MHBkf0(jV>LBBpIS^;4W%INr"0`jCq7M4<Gk*/UL4
	Nm#EYJB"i`PSs6\?=i_4\@VT\pK\3tXCm:$S*RLn,=E=>2ojdDaaZXF67R<=<91TdeFDbZ@6n=7\jHt!
	O@E>%/XiEA#%$6R.''PNjcn''_IU3jPIEDX1^^XV#Vm;XR\cf#Y*/]g//FX\><&o"HQNV8[_c+"q`H,q
	3r<8kiZhZ^?B*>dEl_AMlbjbT2@`9Bi$J#^%Ym7POHMkFR^q53VU0DonBYm5ZF[>GI.e,4Qs?ZKc49fR
	/nmYOHo,hQb^*\gOPr4Z/^UC+V$m8#1*J9SuU:LS?)[:J_tmAOk`LgsaY6V.lar4bIoZ37tD,H^\k:^K
	\_5>=A3jNZ-$nZ>^9IXQZ7_''cG=hBc=gfq6YPK6n;L0^ch:H:H%4(PtK;a.`88nkDfjk@(G`D*SkdZe
	4;''*.Q%D]GM]@\.J#\C_^OsmH[7oGDai\WE,LU=;MmI*lf*9FC77YX26=`^mhq[4/St5\-#-Db;*oXF
	R/t0QL@H&@3#!B+hL''c%mi1nF;e+n>>o35W3HDAnQ6Ysu?G&7Fr7$kh(%.L9YN,@VM"5^ES["''!R]Y
	,.UCoHTfL5e3$6VdoS''m0#&gpDE#L[iB9_3CDd)hC!p*Eo4,RAun"ULO<jAQq''8o*56aROUL4sNM/C
	jrr>h\3Q`c,fXlBpLiL7QY0#n?Bh]FiY6&I!im:r;W_q;[4jc7oDMSRX,o2c"%%jBI!c5<8D>YDlJ6N9
	DqLV9=OgG/C"1(F:8#p-2b=Pk>.''()]MFEV.A^gA)5W-H`BstBhGHr7h1Y?d;n.#3gRC;:ksihZGOg4
	:MK?2;OP-QTQ=bAJ^H62&(aMD[,7ou.49mpg@#,AokCi=fBb$C:fHr"T(6I*?E]ZH%BY(Li`a*p*Lh5`
	_QCuT7./_h\u10W(UQJ(%#91Anm4!''8=DJknTV&NDiM01WD.ZcC<tYcqik.d#$^7AA;ge-''Q5]o..q
	:iU*H^*@L$,O;H!D7WdN,5Xi*BM1r:qKaUYpEg_9*Z3:1%^LZ(^gCMSldWNP>]<Ih2s07$s%1p8Ri&rZ
	=O8<N<6qD6<?F)1Y99<)"#JtA5KcrKn5]B9qW>=\G[f\@-Y3^[L1g"b''>HPPV!LA.uTrXW1JUbJm#P$
	Z["a;6:"CROe0?Todb`\R%i]kA:/o6!?bWr8M%:W$\#Gj,67e^m@WSui]J=4f2QFjCY/1E<Th,bo''O&
	K5Ht[Soa[_i5#0<);@u<JFeC%k#n"b:D0.c$4[4"t1Y<g%''(c_bRpYSX@$[mlB&V),"Dm/ajqsKH7-X
	/N$P)M7:c4(@kd''Nqp=4s.mfc1>Lb^bQ/Ht2i*(?Vh$NpJMg9M,OYp4a0IVtPkB!oXG)F$''f>VOWm8
	;3\>PndM8g,gbR@rQIb?d+nje.KW&`rcETS_#jElVso0''!uA?F8IQL]Aph6UXeQ4if$H"lf[USS1+e<
	0o-+Y+uhG.fs2k:ibnfi!8p=j2Hf!-K''@8<Z4*Ng=ZbfRE*bhMb5I$G0)c(<DBX)Vl&?l6]^aA''$-u
	I*\J0&X6-;Ddtm:GA0rB&5+u=54aknYUK4%Rn!1>l=A)I?H,mo_\?W1$%T.Fah-oGANWdfE^_'''')3r
	<,#Z?GWa>.h9f''fSQNERiB8R]fsq10fR96jI=mb)WlVpC^o(c@[I5=If1XE^oP:^"u?L"kco-YemHjE
	mWL''$N,^5nYc;T]?TG+Qo8p*9<_!IXbqp"5oc7caODYAC^LI*l^t\A25''#r<;#Rknhk]''!>AfN6W7
	[aO7%G_Mk<P?/ao]JU:]<Xhd.d[!Qu=N$Z@TZn0*%EOM(IZb*Ta@O]5qgd"8!FMQN\$RO,uEQ_k9I<"h
	nj:@DP9jRpq\7a)c;X)8I>2_$&/iPb*LXT]Rk/kJ-Ma(f"qPW%VcB`e*3MM@3<jfg5?+&Z8S9"lF6-E)
	E''pS&?!DV\#f*_95hgsb#\GMK3bMCFh4\qNoP"1h-+qhH)q".*9^%0WR2)&dq<J!OASMFnZ"7Eh4U-h
	g7Y"$rV#>UR(4P;A&pGB[%5c_ujWECtj^1Sa%WEXN-Gk::]gjuf(4SXJncon/J(@L`o6_G_b>Fr6Ig6J
	*YZD`=d!t-&V[bNaD<1U9^/#L_^aVT1$krg,%);S3q27)Kk$^TikEt??=K`U$#Mih:?.T#Ue<B[rtlU5
	9@LO=1Zq$Ij3K5h9EXL0T,<r5&''A^QupmaigN1qEBQnmIlo"S_PeFC`m<:heV))%$di<7IOVX$0/3Xe
	Nl[rd;,^ag.M8?C(7sAHablH`gQ5)ot(FpOf;G2HS:D(fP[s_k^''oNSn:pH10hTjS%F0.etHdq*?H[4
	L<*j<A=a6DHW#hiLDH"@MSP`fW(!1GH3-4f\B<>Y(pl[U"%SMqYfWnP_blbh:SWKJUJZid9p?;-PC>YS
	)#R,&?OB/h4[Tth&CV,qhk[hJ8+Y^XA*jnC3%K<m^eqn.[gR5);_*%D?)o''l<W);O7Fb\[QA;[_eK^R
	(S#2\Mt/Fu:55&Er)!1I/DTGc<FC`4dZ?V`.L-k!A,aMB''^jU-bF0p;7=KUR^ba:frYaC1fupC"Bf%&
	[NgMk=*k2:i-<Y]4Z3T;gGuPMeMH;j/eDo3Q7[''mE7Mr[af^WDhbA"L+>mudYq%L[,!ab;?-8#K.q06
	rWJIcVTrjNCO@`DP,j7Q8\)DNZ[&C2o;eQ1@n!jo;8XBqtH_334,_db*G/p3>Me1.Y=KMgN[_2oK\T+E
	"0+O<-&r:HW[Qc\^odBESpDgq36!]`]/-?dX,ngpY8S&Y>hHa6jnjlj=B((oDU*%bFk$?&Jle;H0OR-f
	[S1+n(U2(d6Sb,V=mQmj-<[/[\tDu@H2LX,mOf3Wk#??4H/?J%uq4lMD^6hIUq@MmhSP/FNXGIHtIe1+
	ho`7_lRB_L>#[DU=t?`Dq3NLs%iU:"2d68$VC\D0B;J,Jff55+!q(]X!.?7(a5QmJ452mqO!WR"eW@tJ
	RfJ__)%=+%>3L4MQ(+#jX8qi<Y2rbVe!,U-V40ot6tFFU@Si4o&RMnegE?VCB-\$3AUC3[5YgZ7j\''.
	;)CK''Uu/kN*TLg8/,G3:S54^12,Rqr6`hV4u1Fj/;=[5''YDO*r)juP?5''L#5YU=2^?#n,QThuZ>%/
	>2WmF?ZPEB8P5+#qT=Xcd#G:icA57_81XJ>@3`B+''m9:L`VkDDFkSROXG70r%dYXh"rdA9Lj8DX6a85
	[A8+VahnoshPG)i,#lKlIH$P0aoh_gra*.:K[Gis<B/2n36&3n]dOF1&a(53IZ\c]GG''9q&@*H1X[i@
	j<E/VIiKb\;4lP($:+ESt<WV9S-26nit_"YNT=Wui95M]:l(Zg(l0,cjY^gsd?=6CS[''1G&<tQ=-7_1
	!F?V^0u!\R_P[F6,1I`/jV>IjedWL$p;"R@&K6I0QX1nHp^k9HcFPMlqPT]=BLCOK\]nF''f6Ma^apeu
	Y3>K^n''.:sT!u4&''$Wn<]W;R1G72eBQ;KRUku4@t2FCKI,u79Arn2[OF#F[S<i`H#*2UP`@?uaE[H(
	T@CsG[*ZF;.dq7kFi8sbudcq@bMX1P845OqM+.<>&m\/JTrD''G0L=i''XDUJE31*<!mt55(tmlaP^DE
	hk.NgEO,CDnGCH07W#jDg^S@ak,Dr041`dIW`[H*+eakp&6rHqFA;L0\Oor0M+f2P4[\<p0+e%*pUMrE
	2i^plQ+kM!p&=<mNFgjWS`%>pu`st,&UEKM2jqrKYF&UTQ^%jDgqAs+8ufNrPPi:kC<5kYC*iO(L4`Ga
	u@*3eXu[&DnP_h`8C<N:FY1ab<<<_Gm`F6^LmKEG90jkHt<-8gt>15+54DTEQ99+)k+6;c''nCheirg$
	Fl$=_iT#+MfSssmg#l^hkRToL^gDKOo081tBNb],)VeP\4rKpk4-J7diq)%fHXOnW-?hSXqfBC4jB,Z&
	<R5`$dokrXiuD-ICuoRb?a]m=6V\I/CqoGXcb0$CJ+<IR-@SL''/^CsEEbc-m1s\J:I-DaO_8"j3j6XO
	K]`3%oDp>t<@)q($45RRIp=gP4qsereHG7/AEVTNn3LFaPJCcmJIa3ta*VWW!F)LA*pX\^sS!.muk$C1
	Ibk>\`rVc25rmb(3b\\uqgr^&t$6fHE+9''F9([muDB(C"aO3I!?mB+A#Qea>9^=M50\_Za%n7&q.rtp
	-%DL$T"AT#Om8=m"JIpg=VJ[5lXrQNREGs!e@?Ge@3\VECh8&l^$o_F"5SaD?Ec^d52\GJLn05YLdJ&A
	&B;e;N#HU+]JYi_:(i]4JpnuW!9=o?>CB7G2HnV,kH4RcMDXEt\kp?4FsNpllPddutEK*Uj:hU@\$-^f
	X5:U6n7.S!:?`u^3MX;kW/''aI^pa0/+<I_Y=0n>Yp355oNI_4E<0\L6kl''d!)NDJf255>L2cSg&(sc
	.W(hS"/as4fcYAA&gGf+M$>6XZ6;g+QoB`q6*$Wkeg2n%5(o7BM28OSJU`+1.o276:dYL&m\.=]bujM%
	=dFT07EAKhtoWW07?kTiKEA)B:a''rQT>C,>/kuHnARU7O7,Mb,WGa?Mj%dVGf!p[FF!*s3Y/=a=o,5P
	cpF@q[qDII`FmHI0$hhtHT1_J!e-[[5a(5mj=``''[S''HPZ*ig00)''4tVPNOa)mG!f(%83:%$]"jrh
	W[2EOqJ5efQ:mZ^3:M!mWXKgZI8Ql''b5=>b8@d3ZlW$m,K&!&,J`L[GVeF,:B=REf=ushS/Y#.''/ab
	XDr]10DPJA?[f+j`:MSj;7WI`NHd-7/;I8><AKXek*0)Y.3`QG7G''5^Zu((l6S\;T=SAAc=67T&;#.T
	^g2]jL6^XoS\mkmW6W0=KDPa^-*''ga,Rjq="AW`3S76A`Hk>]Mq8lPLQn/.H*g5SUe\Xc^</,+UjDo3
	ni/8e\SJmnK<Y!qc=''dG:Z-qu#("d/?q13<5YV-"j=jC=F.Od9aY"("6T418-jpNk!*+:m`VR&!h''k
	!N$*kD7&&^Dn-$)U+K#m\lH<5Utm-J%d-]1JWX8''3,cWF/1UUAGS>I@8oO-:e=j@SCgjlj]cg<5mkg+
	o4UR?_[[f5:)K$""7M)PUL>RhGB^QlC*8-kIY,t(;_M-)#n5u;\YZss^bZ.TB7OnuL''4AuX*o4->q-1
	O2)@D&XB76\,e.?o>6//pZD5cMb=94h4l*B[I-4d#+!+pH8P8'',16J0NFC*UIZs++eU"$"HD(`@KP*]
	$?=\LS:F9)u+>DfJi<?"8FK.oSff`bP4o87;J?4V7]1l]Xej?+oD5THRQ3Pq32:6,l)6[O/5RF/!VdQB
	XOA7l''>,/j+Mi:i>u8EZV#n,l!)al+pElYe''RqL#*>83(snCM8;-6Cnsfe&"$n2,1oA(*;''qlAbR8
	2gcJDmYM(m6[KKEJO\gd`tf``/%_+Z4),!:7fIM];F(QL1;9C:GtNmqU9Sej;!TVl0mhV6Y0k<KUb>#.
	R)?K$-h[Ws-SF1+GY*]sWVO8,5`Ea9\"-jb3Y4!uAR*jur&;FE[L<#M/hU;iE8JqX`$lddSe=ScYYqtG
	rk>"?DZBkt\''\[lpM.45ehj*78>4$%2ZbW7`Om=p8"#UmUhkiH_LJk@-JChJTr@-F<1tr''@0</3F;\
	''3g]J[l(PG)9,m"`,,&Du:lp9l1@qW_N*XS=%''@PO*\O8H*)!g0*d#D!d@#5F9SDa/ImVp2Rh/aKo*
	?i(-R1`.J[GLL"1E_)l28ThY6VIh*U;]eO;%N4]$@4,3khG%_"A-C%`=d$ra@s!%]oWG`2N6q`e!u.0,
	pY_gntXkY5r=DYJHi-^THLa`EUKbO:EC.]$N]PrLt7S/#]21`3qIWiXl/5;Uj"O?rK/6pE8`1m_Wla/G
	;-#!VJ!-\Kb/HY*?O-MDH\6HQ:qnC,87!$O4Z>)$#01)#0r0n]VK"#@>IDY7="sj8.%ecmVk[]E(>b3E
	]+XJqt<h0-b"B)/-4`)q)6]0ilE[1U:McL7kjAaMgSA1GOjNt@!i(totV*Iq*aCSNq=Z2J-p?:^Tn<:@
	_333Fp$ZCq*_*n3,/m!6Mb+9FPsQ??lAk=`9S5u"u*k-,LeT1gel6XJa6#MMC6Yt9;LV.kD$h8Ndd;d+
	fQ?+(Ii`\lX\r"IC;5Y&-a.%LF7bk]Bg+)X!;.cck+8pc#<s0N0Dj,-\9t!D2C$o'';i!hq=Kl-OZX''
	p:7fa]VEdu<''V5V>%41aXZU":LqQtT4gp''3?X<:Tn1=2A_OCF/>^/9''$l<q1o-bTis+_>trlt=c&:
	3d\=1''L$7W;]&f_Xs7(jh&ZLjXIF`2jC9KT>G#kSVc>Qb.4:%3nk>Z6rF(bSRCq"l.&UP&u%.:K/#IE
	l/[(3@>15t^pdFR^-IABF95aso6\p&N^e^;E=-drE''$?R&\bhBat@PBA!M#2Fs\N>`_]"D@AFgZ8HlA
	\DWs8c8!9/b0Y,82_G1N<npgtM!L<G2H>R%LKe]@.''/X/u*Wt@X?s!VkGNmQs[H(I+D3RnmChL6Y-WB
	9"198J-G0kV''dd`".s+$S:?B:O''Q!#4[p:4/cNpG4YA[[[mm@\%.ei)^'';0\Fm<5r7X#"Rq%Yt"dM
	7,tR_jU,tKdiFq<=@0EY!<K#Co9;8+A[t0BN1`OmYsGC/dZ_5+pM$,$lV]+2je"/F]&O")VrWkq9<q/V
	*OUuW)Tq8PEGh9$5cYPJ]oLZ^*@Ms+7b''RQEOt07aHK3"A>$qf+LsD`N=.h;>J`,BYrNWZo[7c.A=Fj
	hEWaabPsQ^>\Dr=?46CEK&po0YC*R8_%u;u_ET''<KF5ns">@?-=+=&''NZUCDGk>S/*8m`j!HQ2:Bgb
	!2\a/2]]R,d6B0tq!3"+%P/;>''l@)oFIKFB_Go\IGrFK>u+,ppfOHT,1kg-H''l$3-M+3mO@L#,''UP
	,;s4nV0F;ahGT*&PdR6)@AqJMT<''2MMC]KLsp<#h_,!EU)0HDSJ\mSCLd)KA)dN(8r@&COISg8.=5J1
	A*adHPI-S.PW=:C%0''+elL&1/X\\C*b_U"J_XV,)<jAEcj?3DKr40#YtMe45na^duY%,15*R!db%=E2
	J,P%ZTiZA`-h7&..V7V`Wb)6&pjRTR.;W*"N$4Ncc71J4mVP=ad2FqAY$^.GmQTH"VL59V95C7-AQV38
	O*GE)5DaL:N*+0,qQCL+EKZ0SP5cU4i''nP7)#L0dEZ6%?n)b^10cGL_!T;H^c#F]%#u)pL4u64<r=?\
	AVR4&tMs(Ai)+2h/"HNR7SMJK1jOf&AkQ^E6$5dSsTl/_[_:-FG&_oWP`;`%7L#RPaPRJLm=5T&5'',.
	@Eon]\/t&ebR-aijID?djP+#*Yd]1H<5+k3rl(8j;0''^(b!;:jjtJ$5as?)Q6^_1Y#LQhY)0`a&j@OV
	3[?^,m&9fSM*R-j>1(e''I:04uJ7l*\Z>/`WT3,s,$L.e0,GlPml$DWfbl6DX8\t_9aZ\D''"$F.27l&
	Oo2h<Smg_]A/aLHF;M%X5G#*U`\7*<.)u+g0;~>')
%
classmethod: R4PReport
imageTableGif
	^(ByteArray fromPackedString: 'Q4%FNC%!! I!@_\@@@PBAG2B!K3B/CQBOMS"9AP"II2"''N32;@0RCFQ"YLSN7B02MJR2-MS"=D1RUH2R%APVIOS:=L3Z7NS"9I2&-A0*I@PFEAPZEL3R5C1BSFQ2]N32?CP:OMSZ;I2*+K2:/IRZ)@PJAHRJ#@PRGM3*=M3Z7N3*9K3B3BP&MM3*;A0"KIR"+OS2;EQZWBP*MDQJSL3";JR2/I2Z''GQ:_F1*[LSV7BP2OOS:?L3Z9I2.-C0:QN3.=@PBCDQJQ@0REH2R''AP^INS";@PNEHRN%L3J3DQNUG2B#C1BQBP"IEQRWBP*KAPZGMSZ5O36?@PJCAPVGM3.?NS*;JR&+OS2=M3";GQ>!LSV9A0VEK3J5K3B1I2")NS2=KR2-MS&=N36?CP>OJ2*+KR:1IR^+HRN#M3^9A0&KIR&+EQ^YCP.MKR6/FQ*]CP6OOS>?L3^9JR./@0VEA0^K@PNGBP.KAP^GAPNCOS.;OS6=@PFAHRF!M3&9A0&IF1&[CP6MEQVUIRV%O3>=MS^7GQ2]LR>1MSV7J2.-BP^IAPNEIRN%EQNUL3R7NS&9I2*-MSV5C1FSMS^;IR^)@0VGNS.=E1^WBP.MD1NSMS&;J26/G1>_B06OCP>Q@PFCH2V''NS&;HRF#DQFQ@0NCN3.;LSN5LSF1JR&)N36=F1.]FQ&YAPZI@PJEK2>/M3^7I2^''F1.[L3N3BP&IEQVWM3&;KR6-J2.+GQ6]@0BAHRB!C1BOM3"9A0"ICP2MJ22-M3"=EQRUIRR%O3:=MSZ7MSR5OS2?C0:OJR*+H2J#@0RGNS*=BP"KI2"+E1ZWD1JSMS";J22/G1:_L3V7B02OO3:?MSZ9JR.-OS.=APREA0^I@0NED1NUDQBQB0*KA0ZGM3Z5@0JCA0VGNS.?N3*;O32=G1>!L3V9LSJ5LSB1JR")N32=M3&=OS6?C0>OH2N#NS^9BP&KI2&+O3>?MS^9APVEB0.KA0^GO36=N3&9JR*-DQFSM3^;I2^)B0.MC0>Q@0FCIRV''N3&;FI)R@Q&@BG9A@@@@@@@K@@@@@BF@&DAA0#?@M5A&2KP''\FCBAL*WL"0(\NGDBMJ''D"1(,VKFCM*7L"1(<^OHDNJ-@#MXD&S4D*^GL&2)T.I@07ZV_&2),6[NGO*7L&3)4>_]%C>GD*4(ZI^^B85.5S+D%N''SY=J#T(U*-V)U:-"7Z*5Z=Z/WLE:CT-6+E&1ZL.&OZ.6K].7Z>N:%P-7+-6:^N''*/[,7[5.'' @L7V=)LWZ=/JP,VWS14HA8@@K!A''$29,.WK&CM+7,29,>_O(DNKG$6:-N''S*EN+W,6:-V/R]0C\BUGNINO[P6,ADJY4\N?A0HOOBD:<.OG#2IOO&JVT^SO''4I-K_38=N/W+5+O?S''9\N?[*68$O?6<6//1 <>SO*4?OG+7;=^?[09<//7;<>?RQ^=<O?+-??/?5A>B@@!XX8HDDH&  ]<\5-5P-+NPBPF448VZ!SR^E\<\%(D1!11P !"#""BRVZNJIJJZ(8((,-.#""3CFJNNLMMY(88489*####2R>NFOH(KR8PR,7HEO!Q\&:UII%0A0BSP_G#BE%@^X04JUU489AP-VV*''%@U1^6VVWT''XY))%('',$%&NZ0B^XTW,[I@)132$''''''W[&V^^^^GK9YY!T*-''&%NXP*.V!T"HJX*IT$*%%&H=:JRV$$4*:)ZVTW#)'')Y-*F&&''&XZJ:Z" R++)*UN"2.&W%;KZYZ->1/=Z*J203&)++[#R*..-.>[J::>>A-/+,L@RJ:2- )9I)ZISMAGAM1=J@(@_-"%)KT,DMW''IEA6FBNRGH!XJH+#$# /"''A9.^Z:IPI++K+ ^V&&G/OR2LJ>==]:+[;;<8..//.)::6N:PM*;YYP^R&&G0 0''[B:<HAX:[<PQ_2"..JQPCF^<F)-#,\XY[;311BI;WCJHHU?L\\$_"912/A=OSGKHJ]-Q);(6''>,1%3[/;GOOPOL,=L=CA4744TX''W_SRRCN-]MMPO26545R[KGB:@/=(1=XGPMN!G[0@HLV5YK,TC B3%N.%"C>">6F7W;?+H[3"H)3(''#?J^3V<9VK?7___\/,]NN@0S4E38V.;OZ;D@0/.H[(GO0:.0SYO7+[%$)=;.^ZYQ58999=;[+C(&H]N>^"T,;479:L?_GF;!L\>>N2N258;;[[''#//.-?^.>>L;Y<2B<KC;BL(<T2Q312)%M0<R$0@4,3CNVB,L,YY[03&/4]9.''S7,U''-),\E3S/?;>[3?O^.:NUN_-<V@E>>88R.K"?VH5;.K_8''97:=:8?,C8O<BNB;2O^:@_CMW2B@GL_S9K''4P_J@DGT#ABEY0_''8S50D>MK2D@P$]A4 F@EZAIN^Y<BJ:&TTSTAX"^RV*TA/$F0_Y%Z VP.9>;WJW5]YE"#''54G@>CBHP?8_802HJ4X!DOFK9G)Z5&8WHXA,<&I6PVLQ5YZ9''G!1"<A0F1N79ZVD_>.DV)=]E''6UQ#C[KFA%;:DVIQQFMGU0#/"(6L#^.;H][B-$LJ=^/__6+#8C<(2C=RL!@E''JP!$0$H!]92DXB$&A2:1/[#)\<@A" !B_L)DRZIK6;9SA 87+[,+8%NB9M88ZL@"T[+:YF>$7P +A4I\X(-+''>YV56R;0^80Q8MPHN<I^=AJ[>!N&?XNZP\JLC''0=)F\,KN+NY4G2%MI=9PB?=,G70R=6OC"@-_F#2&1X9&?PV)2:OE\1>?-O ($#9/R%DXA(_&$XD.!"1_0UL\]NLY.3JY??@''JD3TZ5,YPM)Q2\/% 95&2L]:ACZ.]L]=JDK!Z!BQ\\:C"X0\"OS''?2(2UE=^#R_HN6(K.OU.$SY@Y:U/BP8U?(P:GT2\E#[@[(:5@P;5IQ[$.PVOK67 099CQ(Q@LU@_A$"OT8)R$ =*%J#2MR$M''V)S(6*= !H2''R!B56)ME?CI!YGD)%LY[J$IU"?B+JN5X>NXG5YGS/V,%&^#FP&T:,FX<X8"7(K*5I=*%;32%^(>''V/_>4+X@\+6LHF=+BDOZ3*V)%L$65ND+P8D$,''69@TH.>@1EO''7AP6-2''THYK] )]OPVES(\XM#6GD8T\;6%":: 2#*XW$QTVTN+_?''Z:@[\O-@Z=(6=32U+_X/N5/!?.98O(6]D1,E?''F14H-[''R54@5)]C,*W\E-<Z)2.:D]]/B!O-3A@IPM+4H8B\K& *,N7Q(WMF8J"&?LKZ\BHU]IDJLHC2''BZ0R<J ^MV%7*$)F_U<1(LH5Y3FDN6L@HK+BB><] ?#&X%0"6YSG?!,?)V+"?E?Y/L6=(MY+%4+G1D*E$1T/",:WMWJMS(41HZ8_62.QCJ[&/Z]7)-P9=017_<LWVX#0PN=QA!)2-VPF#U+V)D_%''SCPWN,$5NWM&+4/^*:_D''/3E;A4*2%^6,$&5GD)0[]''KWU9T&KLL9/^J6\1$O''NT01Q%!@EI?48WSVL^9=3AN //3''SF,9733N\=>5''O N93(O<,:DHS>-BCS#R_YQ,8>;&M$,)#G(''C:=I95EA"\>*)J+:1P#/T514%@MD7N*PH]0#UAN9(%(.CH*P)7E UL9&@N>++*P$OV\LX]-''C% -I5U4O25(SWC,IU+1!A;-\1.8,,(/M[FD7^=''N;#VZ2Z''&IJ8.5;#N](Z7[U7,W#NWW]6&BD$8Z_F&DA6:EY\IR.H=T@!D5#L1=X].C@)3)E(U;& 1-O[]X!2;@16 $J\]4IFR(.86&]0>G3<-.#Z7Q/E[.[Q* "EV-7@M,>H-/K#F4;%1"''_\8!0O^[%LR+@%6))>6/?F],IW+/K[[R?#F;/!.E@Z:WJK%9MXYBH<Y@LY2T@FG/^5@1%<8RE(7FAN" @@@$I=X5> X9:_@LCVI*@5\=Q!5CQ.5\2$2D!EN+K+J=._?N+Z,&CWK\)_''.E;5^8=-*>926;O''-/^//Z@-]7.]K?;7NTN];1[>VE%''!9'',ZYDZ5N1<H\7ONHW+?#FI?;1#H^<82MO^V/;.-^57I&HLV''3D8*3W@.\P&Q$\0].7NDN>0C@7M01CR!M0K4D T0DV@<MS) @QIFYI3QLXM)9.NLF6XLVSENN/-Z:<$:Q 1RV/53-0E\/2RQ7//R[S?4+O;?W4Z=>=*4?_^1!F^L!^&+?''E%N_OJ[77E*!JHU(: 2G7>(9)5_*T.'')=>JP\L\$F%=.^+[(W2WXMZPDW@-Y ^HXP^&M047Q!C_<@4;YFT9<8C&]W8TY$WZ9EZ@]7&@98A5Q60Z^G\\.F0]BF0[*CT!VHH ^HHY"HJWE7E$=FC%57H/FHL_9R\B,4N^$28 -G''115JV1TH1@07_X@I6HA.&)  DNGB&9 879P:\@@+T0@7\$&?1%WP!8@:L<F8F4PS3@BV#]''=&ISS@X6Q"VFQNH6CL5T._MF2!9BO0L'']O-HYV=G].FH]-BH]8=8Z]QX]8JH]6NH]7"C?E)#OZQSBIUX"FYX"H]X"J&H"L"H"N?;"H;;L:$3L.$KX<N<!R9EUC;''H@S^@ND7A:0E@RHNHKD3@J%A@B>7@G^L@HKO@M,ZDJQWHGBS@AV1 [*VXG+S@KR@@@+#@K >@N/ @-1XU9T0VCDYQA0/X.8GI6@8U%GI 5_4^B4I!@3""MFS"M6G"M6/"L66"MX[X6VCLMA=X#9E"N9''"NH7IU0*ME!_L!NS!B''G^IYFM"%0UF''+L540@YV0LJ".A.MC@YHP@Y2VA*$*DL$AE5<*@HN7@G''3@E4E@B2''A:CK$[+)Y*-AVGD(!KP?R@9$A4P(TX+]"Q4OAYI0TJO>X=@@\J9[L5S]A*;=VR(\VRK.$=LM%%M_&RL?<)$3EYV#-9$3RY$31)$4C)$3()%O6P]S,0C>X0@^#PX *8W_MD C7FS(1F%UYY%U!9%U*Y%U29%U;Y%VC9%U^)Q"@6SF+''HYA&R_D(#4$2_>C(H0\@F^%"DH) C_A@CESGA]0@]D4 F_@ B()@AOI0A:30CYA1X9\P@,K@A^:PB''20ODI2]B=7NGW6]Y[)]_7"R.B"Q4^IW?''FKZD4^5L6K*''FW^S"C ^0T:RY& :H&*+)HZ;Y&*19&+N9&*<YEKTI&;&I&;\Y&;SY&;/)&:QIQ-;#C"0 UKNF%%M@]B46S6^VR,0B'']K9''MPY'']T9'']ZY'']"9'']_Y'']+)'']3IKM#?)S( 1(HI445,>T6:LPOHY#KKH''(@TDL%@HRJD@-\H@''28@P@PE*%10TG4@?(H@.<:@9Q=2GY@@BL8F*.A A<HAB0A4(2BEJ5\6N^M ?(XHQX\0LJ*&1SHHR]5PP,D@WH$37(DJKAU*IQ\JH&6%$(**H)2*H+R*H1:(83N T-B*L/J*L9R*L;Z*L5>"DXV";.]UJ%9&HO 5IIE*EJ&)GF"CUFETZ<U"\-82D4)9[)&T&\)IR0<"91*W*$ZXS)PA&R$V>Z\@_+TF*<A0AW$GS\<BDSZW(<]0_BJE,&A3=L2#]K(93^00#M8@]AHW1DL@->8IBJ8@,7P@R,H@NDF FG?9*(!F*("J*(4EB(#R*)%A*)#5*)&X*)$0*)#-*)& **''G*)''4**%.*))1**)$*(PTDD ]@K_;(5 ](K/8\X\RNM&+"$SZ*+/J)$U*RYU*MMTPQYH7Z%3&M"2NX78JJOLBY+6@@@<V@J(4@D\.&J]4@OM>@K4C@AR&^X]8A;@\$M8\*PM7T(FBMDC;!U:.(0V,V.:=*.;S)!>MT,",@E]2@L;,XIO?XABH@MM3XAC/$A7F@K?1*0S$B0''!"0@5.04BB0B@.0C[.0BQ.1C:.0ET.1CN.0FR.1DJ.1D>.1G\.1E+.1E1,K''8B/2V$GG?@I_C@M//A?:OV ^Y!E#="H"?=X,3!;,3)+,33;U@X#TFXY!2#%W^A%+L7#T.T%H <WH/(H&%C2B]0@CO$&B)CQ" @P@!<0*T40B-3@C.8PF256(H0P@Y6(BJ?&S(1VN;.$*:(EK.86AZTV@RU@CLSPX*Z%B(F@A59H M.P-4O5KG6+-2@B.G8;.M<P.G=;.HW+S((+.H2K.HXK.X>;.HS+.IV[.II;.YEK.X6[.BTPBLS M_@DB"W@B''9:_=/@C:$@IX$R%^#8.+@[.202"VJFQP[3U]="C#)(-L7S 8?SR&#))->J''EN B!_@CZ-@AH:I^/\UF?HPB7T@A/D@@KVP]@B@&O_:@P]P@*Y0A75@Z1S?.J,_YTX]Y@;_LG,NNPF*TAHS$AH&\@N-90;4TAK7YP;426MS\@O2^0NSN!C:":7=&;?;F<C?2;?7I\@@_L@ESL@C+L@M;K<O#L@FCLDISLDSKLDLWLE]88&]ZJ/">D;DD@JR4K9(6VN\IYY!&\H(/LH*7LH,?LI NUUU%R9:$40?%#2V2K/M 7NZZB;S0FH!\@\;\FO5AP4"1@6_(@4@<@%7H@/9^@_IP@5JA:7=.LP;L@V-H@2T<P''0L@()PU)19Z;,R(YGM(YMT2H%4P<DF@/\H@3B:VK_TE?^\6M1O@W3,CU4WE=7SH@8),]87L]6?L]5/L]9GL!<OL"B;L^G?:3H!T3H N3H"K3H!-3H"S3IM_Y7E&$G9M@B1OB6KXY_GQJ^( 2^)O2])#3J)53J*K3J)[2L@OQ+LLLB8;ZVN+0XI-Y)$BP%))XR$NDK)@T-;$@D$!A5_D@L/P@@O^@N@@@O >A]S&J+QC O(K@N)''B 7D@IG8A.I%B",-,#N.ZP*"@K6,@GH@HMTPP%\*1^GV(NV#(S:=3N@6DB;B01<S3OQ__N=H3O=23O<J3O;,3O>P3P>63O?43P=]3O@%7P?_05+RX$>SY*1>M.-BX$)^WBE 7CFG7QF)7QFC4)SNYS'';1=UO)>NU3KR#I?KGX?WDHO47@C4@JD4%1/:MU:AO?![(M0X0'':-$IU4;KFBTD1D4ZXZ"_9(CIWH.#T3WCW]%]LV HAB+C&$JB@27$X@U@25UU-YUP][CR&5U%-5U1=5^CR5U M5(MK5%/-5V =5%>=5*Q5T3O!Z^JX[3GQKL;)#D!=57!-#-?3M%D0)<<([)EE22Z]F=ECR)Q#[7Y@C0XQAZA@\G=K J8V17RIX:C@/=,<@R0@_O 6C=0UZ"46@TD-XW&MH%YW6''U ]ZPE& )8UN9EV J([8.2T>4U6?,H^;L965L"6;Z-6;PM6;%]6;6M6;^=6<C=6;8=7LEM7L_M6=;RA@_PK\A,3%C-4=.EW#4<6-"]7SZBW.$BCTS?14Q5\<NRTMJC''RP8=6D"(*47L@D&4H$&LF+.KHB7BK_PHKUSL@6:GF?3PLQ]80.)M 4F(UL(6WXD;#5;WN@H''.@J/.@D''%3M$&* 0E7->2D-*W,^0%8D >D?(.DW?#XY;.D[C.H]C)(!S.H#_*,\[%L"+.H&3.H(/.H)''.IPH%*%M W= A")A*K.L.LL7.L>?.M@G.QBO.QDW.PLO$?N@$<.''X_EYSI=@@CEV-8V *0QBB9:/@O.(@*]ZB:L;X&.WXB,]0@V.X@G F?\\''?-*0(,M--6<96/T"Y<("]4@$N*SI6@X"X]()J1%&([5M)6V2K)ERR@O. $D. !X.  ?8K(*#4""-;(!L;(#2;(#M9*8) =>Q[T;TUZOXVO7\KJ*_3)''!;*]P;*(2;*3IK(O<0-ST$OP/U7>ELG:@Y?T(8[%[Y$VMM?PTDPTWBR'' V[Y>9.(:ZTAB#MWF"Q4L@B4GCE?D/"SY&6JDJU:O!I$@Q<]N0,*SXPJ?P5[Z[-7<O-O>K-^++-]^^T]4_.8%;.8F;.7S;.:];.;C;.G3!/"EDB:!@K<UU4NZW]>+;/X/]$TH7,@&G#7/L*DK^;,88[:6%W^<7P=G@KJP/[V<O*V+#SS&&!S+''SG>%.P!#G<,P-''7UJ8''!JZVH&92JN)77Z2YH&:)K2KD?2U92NSQ&D0/=H DJ5PZ''-$$JB70D''IS''?7O#MKS.?<4B/<3?_<4I/=DT_=D%O=C2/=D7O=DO/<4</=UE?=D9_=T%/K#WTW.]+Z.* CP''@BL CB(''RZ,-2=*2B=&*_=&2?=&;_=''C?=''H_=7P?=7Y_=62/-N"UZ/6P9<_S;K;FWS ,ZP^OF3 797A7X7 PBIK@B:ZP@L&PCIH0>YR_@IKPA9H >YI?>Y$?>Y//>Z[ >II/B)9O>ZYO>Y+_>Z''?>Z"/>*:?>*TO>;K?>*]/>''4@>W7@B=H"C<+@B0&@>Y /BY[_>,I/>),??JU_?LRO?I6/?L''O?L]/?ILO?]L/?\;_?MQ??]F??M[?''?7I;?$4H@&T$@0=\KJ$''?''QO?/*S?/+7?;,??;.G??0O??2W??4_??F#?&$70_IP@LFLOL@L^W@EHIS;A <$@3@J&#.GC:DFEG"QH(UKU;D&EG#Q(8\00FXMZ> 0YGPPD69@H@[@IY7V+I$"XC%)9\0[])4^UL''3I09]0K0>UO(TJIE ^(LZ%S)T*YM''S:E.-L''-Y$-$4[E&%W+U*9]X\HC(F7BPV D;Q08ZF_Z0VQ7U''VDF5_.WK)5K3Z<AJCYVXL,3M%!\WC EI^3Z%5BWJ,VLS2GK.E)!$_])U*=Y.EQW@.3Y,"]5CV;ME$3Y\VTO8OF@;%Y9,2)C>LIQ802?6''V&&/[/!P.\V''E&OFP)%7[=6?E8RKW:+S9M6!":(#_!!9]>''S*5Z5_198]N99^%9*1V-5Z>7#29\6_Q9?>M.YK,7P7.<O-@KQ)A=ZVM],7FR5<C^7>A3A@@^^*ARQ4Q#)(B''LL "XB%$P)(YUV"I 0EWID@XXHH,!IATLLI60EF@$)EHTQTUI!AA!AI-RPP!CIZZTDTTP!)0PJV<QQP&AD@YGEGGDDT$PIP03QQP5GMGIDDGF\$YETV(FQQ%FH@BYEC,$9,,\$%=QR2R:;/IGKLL\T,40233P3SSSWUOOKH]>$<$L!V9%2R3.=SIIMMK><$4<<??PS3C[5GKQPP/?7/AGHE"]$YA@AWAJ,+IO&L6"G 2Q1Z4AMM>U44;2Z&V^ AMNZ0)6SWA)1Q0)K&HABQ( (PYUD771312I/EMEEHTTTQAD*"VBD5%RGAVYNH7L5U- #*VR1VFF_''UWND*F\TT]F ''46V6679[Y[[;<EM511!8Q10!7O[WY\]]]%-55777WWQV1Y& JTZ]2)]1)P<@W,(C8@:J=S P\&VJNO="HHKXDJ,&LBTEX"P @MQ\S21@0%I*KZU6_EL\TY94P2X05M7IAZE=><4D Z!T2T12EOM#]#N%&\4FUEE142U7MAE@PX\%I$&\Z[EU52ZJLOMSQ))I]V.."#&RUQ5R3?'':YZZZ*/1/+*))''&6.)EXXZSQ7J@\\$W$4C91 9<P[ECK[X2KS!..P''FR2>>DBS)&0MXL%^@X#>T$X!5V/& AAESRQQHQ#!LTVH_W=9Q%R\''ZDWV&8.MT4.Q!:ZS1H43IK%NG:.V4LL)V6ED%P<(3AACF;NFOW[YY:^=]*N72Q XK IY)_U%^[P=^NFGI;;86#OT$RU7[#"I[V VI@$0!PJ^.7++AR10%%EIV#@0ZN18TLIWY;RQ"4=ZP_EUPS:@!7T]IUREGA"AYVS5G(,=E8@HX<&2RC)E6\Z+:BP"[OW/YB6C4(80MKZZ:P1$\CI :SA4+UG@*&X!D(T@8KUA?096$E4OGIKPFGFIN2@BP;&#&P]U.DHV-)AVM1/QBU&RHG^(!PV JH,]PGF -+3%^#<D(%0>5XRACBX0AD$[JEPBOB#YZD*L.LOFO,@HI+PB@J^[$XY(]J4IWR!ERJJQ@FPDC2:4X!B(R9&N")TFK4*HGA(TFYE@-*0L*XIQI])P.Z+EI"UM[F3@RHTLO)FK%>G,TE?[&/DT.\ 05R%HTBJBG31Q"1IIBT%[\5+U.+YI+3FR$VVBU(Y6AAP\="U:4T/HP/0SQEZ6,"H_4Q;C(F^P;+DD$[$SAPA" ZG8TP @9O!FJ$BWB%U\Z604T(T,]$PNU\QBEZ(PAHX@0H R0O<(E)QC''80PE;<H[V BG3!W+*ZD(P>4X!/N(&X98%^!CA''N#A=Z40E]IKY2@H@O-.(\< #50*Z94I?,B!N\"NP9J$''L Y0DV;\P",%=?-NCS@,Q/PA3(K>,1R@V;ZD+M[)Q]=PMXZIJ&B''M0S\RFR%E7@@EHG?6L0@0XTZC0DL<@H@DOJP,) BP!2_JBR-52NLB!&@E@@#W"%8 XZ[D\AD@<@BOJ9"H&U0@A BH\XU8&D ]Q!VFG3PT@''KD8 N2\DT,I!RK^@#3RTPP9/1@NZX\.[EFF5(U%@XE0:U94*;D89JRZJR#D"D+!Z_[YN0XN-!87-V. 6JYEPE04P!L(P;?]#BGRMKB .&-$*NW?VFADEXPE.R''%$KMT(Y\1@4.AL-U.\PRH5[1"U9<(A2_"DTID"@O?,VBE<((92*(TP92+@H,I)KAG\AP@#30XQWUA@A.YUD''Y#H".ZK(AQGTPP\.BJ@YE>!DB[CQB6@&@BPZ*$T:]+X+M\%SS!$3P[WDB[T4ZP-)C(V/.LXDODQ^"VWKV%X?1;UP?/X(/"(<)HP$>!^3,J5-FHT[Y!U,/T<I1"@N''$I''Q0*@ OYQIT^A"SY8)X1Y:H XRB"AM)QYO>^FZ@4F@HX@QGFIS7AAD]#@08$XDP!!DGL!UXIUBVJ!.3/ P5XB2@T1]%2BPEAB@CS(?0NE,''DI]''1 F<% 1X;FY2I=4,1HU0[V!T:HIPV&:IAF&:-!1U0;LR7J-D@KT^-^5,^:3(:0[4;\&C?IQ6X)-&5S HY_7H[J2";X37JCYXHHL$/HQ)"$,2IRED]F!@<(8((" (\,A-\!^UC(@3H Q#M(@*EO2JIF9O#@MC?@#X1=*G5DDN*IG*^JN70@P,8U)" &LH+6*XLR+_ACOH@!C3BHP!!HWY2DU,SD@?*7X5"JD1Z)YB,>\*/Y?8V6,96&H<Q]BDQO&)H8/]SG=*:K$7C6[;P9>E;EF(Q-''C7P''#O:Y7Y72*LN/+N(IF1KDMT)U;P(P^+$Y2I.)BHUD; CVO=,P PYTDL^:\BCKHQZ@ @@H79UR+UL "J9!P]N XK A.)NHH(;YK,U,*AFBPQ@:&PDP!RA:HL(-NG1W^$LRI^L8_-D8Z.1&_ZI9]U$&^V<\=$=;''7NJ DWVBDC''\T03/$=$>4XF,=N<''2QZ**2QOD\XUJ]QW**[G_VM:UYPQN$,0_9+IUIMJTP)LG_(BMG2C/T8,+)RAU4$@DFQ=4J@^1CENC<>AUE$_D,")[UQH#E32ZH(/N]$A(]>-T]1"XOVWR#-''RP #2&!JI,%2.!U+; $WZD.OE!RDSD3..72BQ.4$/;Z1R2DAD. X@$"?[J6])S.9K^I&9W&M2%;>B32=7?K0WY0UHW_Y,O-S9<@CWXK4]T$M\I\&#MY7/%;,2&((G!BS1PJQV#4DX,;*C@U.BAF<UJP@IT8SL<TA X/@ DLMX!"''KD 4X!6IDS"1QUC1E!$&[<0BU2PPQUTHHO_I@QTZ @[NBE@_(PG*F/%4.!"\$6TY@A@AB/Y_,X''LNZSGJ:B=RZ''X&#H9DA[F F8M&+W@D3-!J^4A.=(=L9)CLSCC0,QD&*1XJ,I)"B-V@A2Y(B]D %:"F>GY2KPIN%3*H%GP(_D"&]"GHUKWN.''>DB$AL2\( GO@@F[J@DF"FFS:BF0NNB^K"DU-B-S:@R]\"F!NNB[B@FQT"C!''L*1G$R?>\"@-2Y@GV8 7HPAS?8!EG @%E(A&:XAU&X!/0C D-@'' VDOXFJD0(Q!ET@@FD(@U$XA@JZ&-@K%</C/T$L-392FQ79@FXZHHV*QM$S+X?)&@7AI4JD%&:#1GGCOAA1E#,#"LYBB4-)&<^Z ''W#0U+$"G][&@QYDK]QH (SN4.S/GFZ@FBH!T?0FTTP!UU@ !C !$@ I"9@@&4@ G!80EYP!@<0@F0@@F+HB<$)@WV A&68 %''0M5T8/B5"%R:*@EV8000*A6:@!4>P06< !4,8ODTPI#@@@B''(''L89.)_C+1/9DLIADP*Q''2]"0XS\NV8;''Y+1OC%BOD#"NSG9DO<,$YA-FA6E5L#K Z@P<"TXYHDC@[;((R2@,R1[QL&K8K*Q"K@B*9_0(YMAZAKTHXPXPQ=QD@RQR0TYDPRC_JYT2DQE>I''C Q+O&1EY(QE_@R](D(@OFLZLLS/G:[UX&ZHR2LQ&N)5)FA,0RAE_DZ[YL<V@V!YP6AB$=I5NODGZ@;_>X,.5IIJ"2SZJNQ5$VSZ1!DR71BWP&QX- ",7DQS70,/@CB004)2OUA HR0,DD;:TYL2JZKA\!CB6:\U_PY59H IPPI0PDRC<@QXOFQ/C4YA2.!AP6JUW$Y! @RPQ@RQ "XUW2S^9.)X/H ^Z>1$S<QU,N*MTD@AFFL[@H2[?Q3*=(0&YNH&5#LF6U;O@''/.Q#M1HE!0]\1FRT@ QFRF\J+F6_>R\8CG@$&H =''+N9-3H_ 1H*FDIFY2B1#(%]@#IUFJH1''SO"_CA (B^@8.@ZQ!BF.F2$ %@&Y$X?''F#B]D Y*($''<F_D=*<<WDT41(DVTCFC/DXTFNV''_R2\-)IR^.PGU&GWR*VXO&"[RD/5ZRU $R]3)DU.TJ6=:DU<$IQEW7KEE7QE75KEYUQFI7QFJWQF;WQGJ5QDO*[F]D2TGN8VB,:G]VVGXWQ%8DP\&BB/?$<1EDUELUQDJ*UJJURH+URH;7RF=VUNT&,FRJU0C 7]T.0=71OW@P)/2@H?12ZSC)Q!QJ((-OJGR\JE"TM$Y>!$DD0J0TR$QK($@#Y&WT*E"1! *DDKF\A%"VIR6YY''M#4FQNI21$Y)=*QMUG()Y(Q(6-!G_CT5NHA3"3!$V7 @#0(!0''XL]QI 2+)ULOB%-TQS\7L%[GY5E#=''R4RF[HA CB] ->SPZ,+RQ4$4?_LG)D0B2@4M6#XF5?L&AO%''9>Q2TX:M#-EGUGJFDYXDY>AI52Z0O$Q)\;1$L9!I">R$)(CI0-QEZ^Z&ASJFT%<(P.905#YRU#Q,+94TW\I1ELT-7(]S!$1AS+@!%QXO0&9TGUMTXG-E()!-MPA%&I1MYG9&WVQS!4!VG.U+??/!MPP\X&SNL<BZ9,=09SE?EW7[CA34KNDNP#I9A-F$Y<L:XM,(@XD$H]O>@RV%P]-&E&Z!P^[%P]9:HXK.M&Y/P@$LC!9LC!8 @\$:E%8\@V"%U&"EU*\!X^Y3]& )PZ#%P\$>@R$5PZ$_U*!?]&''/U&"5XZ,/]$K4@Z2M].2QU.4/XBYEXXD.CQ;0!I,*@BCL;"3O].4I].03].=CU.=U].?-U/@E]3@I]3AM]3BQ]3CU]3BCU."/PIW:K@R:HT>.@QQHJ.0]PU*.M/ESU/NI]0%(@L>0@[>$Y U20V&+XBZ9U.?S]3V=]3W]]7X!]6?K].\QXJ0*#$;N8!9DM''?O^M]_3GI#2WS#9"A+$.>$[R%SITPK* J''G I%>"I&*"I%_BJ:(VJDI )I."EN6@GR)UF:K4I:/TI:@W_:LT0:4W_=B4J%:BFS0 AM@@''/7-@@O"D(K C&*A^(,C^?E6J''DB@0!&R(NJF*7A^(Z@J=TW +JA^PM2+%\CU!D&7N.B!LQU^%CSSA6/I!S%VE"''HC@J@[F"EBU"GY7*&BY"@PUBD[3A!QUBEB_ FU.&G[> GI% GQR AQQ#!BV!!D>Y!GU:GG ["HA["HR["H!;"G0["D!BDN7@EPT"EPV@D@R@G%RBB[6A!U_"FE XF!8E"TB"AD)"BXWS!F ZEM)6@?5!!8SG^XRM&83Y683^F83_NM5YIDW5\@8^TG3? A$>@ETT08R1&ES>.8RGF8!/FX(]!816&!4FP*UV@J2IPA&+0 1PF!Q<. Q]NX4ZX@![.A17&XU>Y@%W(A4Y @QRN85MF9UPV8![>!%YHA&8@QABYHPWIL0J+.%8=2P*61V@]BSP=,HN@RPDEMPB@A8#HYXO(*FP.%VQ6"HYX9%1.M6"FA$W !$> U@&E($>0+BYX)VV.(RZ@BF<F98!@Y%66B3-HAW,J% =QAS=H1H_0#7!VF8,(%XZ@!+J "EV:Y?<@B5&0(14QA BP X;JY_?@L8,XY6<696Z&@YBXD2<%B_>C$DW_B;:EY,2P?S+%V9 )RM8$@PM.$H_/8VZQ]&Y%''(!9[.ZN"(B2.F^B-.\]!@[''2(ZJ7I@YF.]07!^GFN^CXNZG,@M''!(Z[!&ZK'' !.I."N&.I8ZHU(T!T0.@L4*JEUZ(I/^H!:S&''K6.^8&HIO8@[>\QEI(HYQ:F&G>F&2!.^HBF)#G.+FUF!(L@VGE!H/C\&V-FVDTH#6IN)Z#D<B&<=3(;^(6PY<B@D$4@"R'' ",I%ME.@L$"I!]\X&31"G?LF*56VZ3%(#/R^&<;(!\8 LS*T0A^D@>0I^W;(#J3("V](UO@@M0&!I%@HA^LFZH.A^HJN.,].%17.^V5&W?;6*F''HE(,M,3L_UX3X9&]? T7,5( ^":HTRVT]#F,S;**YNHY7:H^2&A#&JP(KZC$]9AFPX@>D.1]^@BI%@^_JG-6\9,=B;)I%@[GCHU*$Y)8*:H$80E[-@5Q!@:_""AU^@FYX!*]4"[''6XP]0C''7M;'',''ZH''M:H^9ZGN>@B?),RW%"H,UX[N8 @\J8WY(;''6E;(%&8HRP"AV, ''^&%E12JU7<U%>RZ>@"''^82UVGPI&B .SI2FG L-''?<@@_W@IOJ C''WZGZ_"F@]^F! B@>H[)YO:F&R(YBVD8@F#&-H[/&M[F[I"E6B8UQW@HI7\GKR<U!U[1"CC2M@ AU;"P?5+ !)Q3Y5/P[X\ A5:PA:''JZ\S^\)O A.">B7^P1#-,DT?HEG,6:!(B!#80AF)PA4T0:(Y(F0B/(S.@A @0\GMV"B6\5-?./\_:9U&$8B??,<_<N  [#DMKE^]F  5?</^>@+DFAVEPA:.N"BQWF4Y7:[U&-5YX[ED0@@CH!_*)9"YP\H!PATNPAV @A&EXA^%>ZS3;AKZ)<5H1\$4?Z6XNMVF $EWP!9)2[&1H9''4)@RS0@;[I!UU ;3>?99?V<#*OB6*8@?:!UEEPA&8XZGH.B67(=!JH!5!0''(Z :''69Y2^G]U%''3@<G@AD?D)[@#0LICH-J"134]6]7IU#"7_>Q6LT= 4$&X%99NF1''C SX?&%QXH\DM>=UV"1(*ON2?''L^KHD;4@X"*B]!FJT; F_KB \C&@HY[HU\^H!/0F2*] ].Z@"^W7"F]8 O.@M!2JIAYPQ"@@AJXN<D#00B]0\.RH]$U!-VY7S0$X-B\G@^6QE%.HM7#>4=ZHZN, L"2@XZ0&:1-8L;&@I8>N&_S4''/.(P,(_PE,VWEAG(?.>@D@\H":. 0T[!B.G"5$P] FN6[]8\:<@P@<HPI\@_EA''AX57B792!V 0\:,\!\\/''X'' \;0HTO2GX@?6%P(@QCL@VCT@T-]?$5/7.JBCT>XAG\@PX?.@O?!&]8V@\N!8[?N,BF''D*[I @@=-Y2.F#0A<\ +!?.^2E,''SXI!:A*X]@F[A!0I?_93B9S"IBDO:RYVVZAF< O2_*K$,S+5\\,E!\41KP($''(\P,B)_N8([VBD!9#*''B:EU''BGUP DKK<C\L9<3C;*GUS,%@\HTX)Z"PHF@HB;!N:ZJF32BU)B"M<"3!KU[5VB!M4&\(LFDZKB$BIG$"1)<"SJ%B)WI$15I1^QU.SHEU0E@E-D"M0T._,F[X(;#>9JCY*@4Y4]@GZ HVPY,!@@K*6&D%E6Q8[H"^:(L[HS<!/PAJ+,+NK%EQSB)$FEN&7+=*7IA@@.DX''Y:.@TN7&'',K@CR*>]ZW*S?=5YA_\08,RJE;.=AJBYGQ[&>$:)+O]@9XN"X''I.IP.@/IHRD[ +4QO)P'']77D6Y$@4(-2''P;/!,@)H1;)WPI B 5 )XWWJLC(9$:@9!7*C.ADN#9.:/<:Q^;0"=''_,:])Z)O,DDA*0U(5X6=X''$I''HZ4"''0_DESURE/T0CVV\*;D6,3\J.FQXHB9S2/Q@+]@T)O+"R$''"KT^KQ ] 6:IP$@8Q@45TE6@C[EC!\B]$@2@N@3''8L!"#""R^D@LL,<9%QVFVY9YS#EPWTQHT(+LWDA&&#.3LLNH<,EYQ"A@G@#9F+GIZSVV"Q"-1,@<O26VR+C2Z\\RC3%@ 1RG,''"5_=17@29$3._ADW]Q4*Z&U,*@FR3VU7@_FBSI2KEP0P4R8F2""<I_WHPM_D%5AFHJ\$SUT3@"FHUU!D=5\*A4HB!(3-CG(S\[M@<1MZY&\)5RX418ZUWAG3Y,\L<E (6AVG;Y[(**8%A9A!$_NV%%8*2,''AP#Y7F=E$!I$GDR"\I,YAFOA?I0:UV<A2IV)Z-J#[AG]+H5D>.]2C)EUC=,NHGOTGI$,9G<D0D$"<HUQ*(,>&Z=LD''_@CCBA_[@+OJGY9TM0T+:!0XR3+(5CDEN="&5%PH:JIT''5QTJ[NO*!@AAX4)_"#DQS)@6WF,P.N&)V:H5$EHU557JUWYOENXD1#?XB^34NFGGK/<,$"5@C@C+R.2LN.K&''VZJ2D8$ PMP<C@T<8T*RBQ"%]6IDBDGW;X,5ASX-*FJ\0,*PJ@-JJ$P!T@YB)T9-T_ IJJM(Q<81T"*+#S21+3;HBDJ$1UU3W]G=3!B$65TMLGDSY!H5PST73C3R#_I@ FS>9P@ 0(WLQ39EKT/UWHGU3PVFLOU75=XD=7#MKDK=PX@EKZ?YRS#X?T&Y\$7[%-&*/HE.9UFVT((5)X:;&7B*-^M>,5QX-?0P!@2I5=E))IR0&P22](>CDEP44\P@$@ZCCA4F)LS\@:5[*OMI0<8 FP#*D@WD)%P+ZI$(.Z"\;JA0B9"HHT?9''2F^1=)!<@0L^L9V@S"B#<I!I(_B@^R@A@NP8DB&!P !-H&@AD@@@_6;!%TE1(T5T4%1OT7F@BA9QG()Y"A63\@Q/3\4\?; @#ZDS@P%3B76D>E+KX&\0W(#+YW.3@HPB,8''80?FE"SHR"V/7NL"3BU\#0<1%%"VYA]]IKS(BF*R%R2X^:J0E(2EFN<QFAGLQ9XY%@4(PH LP#XGQ"P/Q"QR@:JA[#><C%7$TL@EBBR5MDH=B>M T2+-D$A;3 ]4SAB18ZY3$_L\DB$]JD)R22N(@I"QO;2DYE.\LT\?FNJDQ1DNGQ2 95PD]^O''$;UT62%H]95VL.,9\V:S@31O>+4T2@,X5X;LLU<:$STD9R1%5ZZH3]L:T;!,LMQ  "EZH( \ "T$#%[OA+_O0HV5"8EFBJBI+P\BL6_*MMT\# C)QP"A6LD<%]_&6QT9O$5531BVORBA#J@L@(A$#IY,4''%4G!D!%[>D-*# PZGZKKY&J''E7R08EPZV)&G4L%O_,),E$TD7((B@30$D"D<][FIJ>Y1R1\V4TKMYEYPV!#N.R''TYUM@R31>L8EV)LH T;(M :29E#MN\99SV6"C+NOE^IB#ER\ BCC0@PA!?CIP>5PISHTBC0PVAC"^8FG:$AKN\3[!K>DDC@U#V*]H8%P$"00IXV*QR^=T"B>!N%''?GU18*%Q5-Z4''8Q5DYX^YU :OL7E4(8FZ^MV[S-D+5"0#S). BBE-Y#L_@@X7U/OU^TJR$$\-*U-52T17>BHU7N@CH;[Q4=?XIA4Q*X1S3K$TA#%EF9?8 DWI$P1.I@(%V/4ZV:@86\"FAB"<.DL-Y/")JXB2K:SJ2<%66CKZD-]D,U)QQE]4@H*J !D4R$NSX@-[:7CU"ZOE87R=A=L$PP,^;0J*I-VBG)%V,S+&7Z94"[,X]N"O)3LJ#0A,((0A#/F\)C6*V9* CW!^#!F^.HODM)^$;-5FM.$C4C#1V4*O''X 3QK#CGQX9N9LAY*68T:=[TQ&+.[J(0<NCGSB6?1ZKS<PCOQ!&#MB4DP-2*DHTKV8RB9Y9X''VAIC$,D@T7\-ERLMR"GLB8!C_-&Z$))NLNXBBB(TRACV74H H3]-@TSOFIY#0)/I_9"\(.MH<]=/CIWV5(Q2D:.>TR+445@ XXC,HG^LB#F?H(!C2PHH<835''N]I83''.,\Y3/_N\=>?#N @27(PQMZ4M)01UKEUY\(HWY?<''BEM (MZC!K.-JV/#R&L55)N[,BBZ;H!#36&89V%H@U]4@DD\@0)F1( 1*]A#R_<Q3)N<\Z5''.>\30NX,0YDVFP<DBCM+Q1@S$# ]!4M+Z1WQE*[W :5I)>M+R_[^]C16MH^J#QY#Y9(_>[F_QE[OT2S&D%ORLZ,TTTITH*"J@J1?W)HABF<JS"K^=94;/^=+87//N-[73/P1;BZHT_1!^KP\ #AO.6-;TN+/BEL;3!C$_8I7!A#!J@ Q^WRLT(_@@@ 7_-8P?''A#3*0 !CAVIO5)HT01O.<YV3?NG0H@Z:ORWAB0EW212"1WCAST4!?.8@*%0QMM@B J% ^1@58(H,0OBAC<RBB;EH@1^";''R''1:K)UZ^:5[EN=Z5[?^)^37+U.<95+W==:&G''N-#K?/V0,74T,.BB@H!PC%\[B!!NY;+Y<096/_M=;7;/N>C?K/#@D7;0! ><5:L^":T7GP2-TDT+8L"HD!C?8PM/#?+[C:=9,3^]:%C?."1" ^R)%H@\G>@BFL@P^*Z+O^>\9?/+=19;58-]=+V''?]"3O''/]B1;,D="L\<-: J+:)VP.4(,$K*13T6*X+#7OB2!<7#TZFS6SE]UUXW&]?\I._4[[;;;5.Z==;6,_?M<__?#I_7;1+1?<L9*@@L#1''P/NJLQ,\#?>4Z???OM???;/O0C>''0@BHO_MQE6H@(-UW''OI1@],@>1,40AFHO.)W6FU G^LG@K6E2MX%O]M(@^ZW?&!7?*E(@^*''0%>(@"R(@*FG3''LR@.2HIHI0DFD"(.8#^1D!%;4PTH-G3WI3GD=EGH%APBH  BLGMDU?0(S?HY7ED)8_H\SZ-LSQ"DTS*DTU"DUW*DUY"DV[.E79D)''(IY][VBML@JRZZDY\"DZ''*DZ)"D[+*D[(*D6SREM%@@Y-,K/<Y)]EDP[;&DW@$\T),J.JRDYA-PA:.D[G"H_I"H"K.HU1,P %%U_?@U%9HVE$QHOS!I\\U!\!]M @P@0/NAT_BHB''.C=RZ@)A"@*''&HD] Y''LDIO^P]A2D".#FH*5*H*6"H.7*H.]-?8SXTQY%I!KVD(G.@.&*JL%@@]T,VLTDU@OT$19"H4O*L4Y-<XS)26&\3MFID]*@3KPMX%) /OIU^K''IQ^&@=8NNG(HYM]6DTW1&D_!"H<./=#OK:#OMX#O];#ONZ#O^(#N,*HO9Y@."5#E5EEGOJ#P^8#P.I#P!:$P#X$P3;$P,)#N?:FJ6[_D*8#&;@#QC*$QAK]UIR"M,$"L7H$RV:$RTY$RZH$OE)!GL:PH\#G;MRJYUS"M4:R!$$ODN[EM10@T0A@B;BBIBQ@@ R%II"BJ^2ATT)BTY*BTCJ%T2[@T4X%UD:%UEX%UU:%UVX%U&:%U$Z%TW8%VAZ%IMB@T[HBT=J@IN1ATCH%U;Y%U;(%WK:%WLX%W\:%WZ;%TB9%@-@@J=A@G5@BIT@%VS[%U=Z%XS)%XY+BV!(%UG;%X#K&XTZ&WT(&YT:&U8I%T-H@K<3?@/VLCL*$%\?%AP. @9]9X462B) %U52A@% \4I:866.:&<*5GF7V9,(M"\)YB0I,WM_L)&7>I''@F9<D%''L+Q@WDR)7A6WGH.I7LJ99;@ 9_D 2I@W=BA4*3P9F&2D^>0$)!Y!#/HPCO @P5\P#OLP#LD@''%^@",D ''''N@''.6923D)73NI77V)77^I7;&)7;.I7:V)7>V9004@2-\0"W, P::@ 6D@2,4 0X402VTP''''FY2ADJG=V*HU^JHY&*H[^I8VZY7$R:BTDP#IPP!>T0"WHY7$F0(Q.:G:>Y73:98.&ZHSB*G.2*H7^JH9.:H*VI06D)6E@35=HX,)DQ#]&?2\PBUG *IJR2,YI>\.,\ARTQ*&TS"&UU*&UW"&VY"%GKQJT $K -D\+\@L%=HL]7@@(5@EDVIVV+"&[-*&[/"&\W!T:.A@:SLMI?XP* HW,OF&\=*&_?"& A**V\*%G-ABASDM?>LQ3:@U+3(8''CX[2FZ''.MI?0*NYIJTJ](LZF9AB''Z$"''_**''!"*(#**(%"*)''**FSLLA6B&+2 Y+?(P]+HL">DUZPXL9SDLK9UA(&"*/(**/="*0?**0A*.''P-PAR@X(,@@''P]F,*L!UZ^N0Q".1R".5S*.5U*.4 $I%HB*"_,MD"EA%S@AGI]]@BY])R**R>F@Q>U3O#]EDMO<AB72C-"HW/]Z+/]8+/.Z+/.8+/?Y+DD+)EI#@M;QPO; CAK6*GW#+<?$+03Z,038,1C8,3Z7H XARED2ACU''F.L)$1GZ,178,2HY,/=*ARK$P=D2AH&"+U91MYPAM_5!FV(4R.,HPWN7);HCE*(8JT+R*-***''_[,*);TEO",4AH-4O;,4@[-4QZ-4"X-4!*-43K-43X-5EX-5Z8*JGA+5''))^)1M_33+WM'')4H;-4)Y-5I*-5T:-6$(-6:K-5Z:-6<K-6\8-5M;+MB BH2 B^;BF4IH[W<!-6+X-7[:-8@Y.7AX.8Q:.8 8.8BX.898T*0[O\31LW$P@-5(H!07?EDI56\3*S# FHX-8!KZ*+JBV+.$B*)^2I(G0!ET)+E>\K.3FK.32*P,-$D\H3*OZ@S+L0070+.3>K/@F+9/NB(CH#,(6:.S2QXZTB/QA*"U6;,,4W6!26E0]0DR0PD=4)<!.K?\6[B/)$IHFQVWTPPRHU#!Y;#!6+?*.[?]"+]XBAS&6@#8 3P8$:4%-+]"2+?;.K?=BKN[>3 K)1Z1B00;80 :T[<:R9%:PY)EB[>,4UIIF5DN9 2*(F:%-H@Y'',@Y/L@]7,@]?L@"G,@"O,G^T\@&+0,"E!14Z(QJF8 "?L@3G,@3OL@6CLA42PP'' \GMM@CB0@!4( 2A,02?$?7@JL4DMG3DRI;DRK7DM0:HQX#@0(CB#)MTMZ&3L_),C407/BD?:+.)4< E.M*\X#3F=1^Z;>RXY);DZ;5/B4PE.;.XZ1;D\13DZG0P%2N,7$L(MEMQAMW@V0811!Z*ENL11NA(<W@D<BE,7T@L;WDF00XL<ICJ"K].\GWJ(TTN$N_J2AY.3QU*0^WH'']3H'' 3H)#7H(^;H)$?H&'';H(*3H(F4J^T\L''7HD!TLLU]HN0W\DS#GH*=7H)?7H+A3L*@?L0B?L*E3L2G;L2_;H1L7L2N;N0A],E_CJ+WXB+!Y(+FLHU4HE)2XL#=?J\(WH8T;H(#3L''%7L9.?H)<3H2,??3LY\2IX,3NL<3OMM3N-\3O*_3MG=2,B$[OG1BM.BE0>#E0E*F-4W*G=NXN<AUWC7)AOPG5(!^(_#!UF0MSW3G4%T$M[8WDP @\F0M=0E#>H%4A884LIY4RI.4R*\4R:M4RZN4(WRQ#LB"Q79GA;*4RK]4S.L4R^>4S?\4TI?4S0-5TJ<4S1L5T)-4PW+$[0 BL9:#APIG>^%4TUM5T+M4S ?5SRO5U)-4U8_4U8\5U8>5U9L5VIN5=1'' 2LWB(K"CH$3C6U".W?AV#IU&P,MLZ-K+;12HY)B!WW0$NP3D2L''_R%7 2LE"QQUDBQPD9I&0X3<6YD^6YD<6YU]6YK/?X''!X5AG"(QIZ-&]?M&"G-&"O-&\+(3>6T4DT1BB&@#''D7S&RM&3G-&3OM&7WM"0:HSJ="1_I!<?M:0EX9: 8;57CCN>(*&V4"OU>00PX''H0,XD>)@#$HP#L"XK(!HD7 XP,ZHB6>HO,U5 -2-?)=M0V:HG&O]G&CM03RX''^#-40,(19.-(NY-717=7#O]77S-77''M7;/=777-7;W-4!FHPH:54C>A!4Z2''U/=7$/.HH7.G 7NG>[-U"OMG^_]U%_.HU''>HQ+NE%?]?[1V&/G1A5< &3 4-C^H@-,VSK 7K$NM68D,''J-RM[2[P!00>W8(?\!D0I6R(-Q''!:FSFX=?6GH(NM/#A9EG;&QI3$:K''&QL3&RN;&RM7&SI?&S6GP+B@JA39@,>"FR]S&W_;&W!3&X#;&X%3&Y'';&Y)3&Z+;&Z*7%FZ''&''TE9G*2MM-B@#7#$UE(*^R?&S<7&TO7&[,;& A3*!+7&Q+2OQ5Q4,]$7J$&;/GM<TIM?3.''"&MA?0W@!&,II&_H]3''VNT3BD2BTIE_XB\10QMMI_<DW ]T,UDL+V+/3*,1;*,3?*,#=9@-.BYDS!S?3V-=;*/?3*0A;.0C7,<+*MM[<LG[D/);Y)_W0:1O3.+0>O(PS.5U;.5=?)_2:J^]47PZZ.J6BB'':&CNTW*+)FYNV&<8]X5T-??SX_5^BN0C[=HAD$ B]E-_QX6\HBA8^@ G#/^;/?<;0@^<0@><O>9LY5  _#BBKO!5\4>H-'',!1KM"1D><1E\<1U><1V\<1&><1''_<1G.$#C WSU1BBE@BN_@0^A4:0Z<<2;^<2?\;1<^<1<><3FO<CI%6D%UT(44D=@T)+H(RE)O;;#2F@*/RNDX@V-1AJ7#4I4;HS@2]JD1@Z:,CD"3MA(8UD\QBY.>L,5>;50N;+S= R3EBJ/ AM6PCSQ\<2M\<3[\=6;>=6<_=1.-J<Z0#D_BCL-AE^3\W#_C:5?<=-\.=8LO=68M<<XC<Q1/\3R1P28IE3RGT.@-=) "Q<^''?MWKE2L4ONST\$3EQ''!?H0<"!5"\:52CDW54(P-3).GOE #@N/.,S/-5/=$_J1AZ92>W A4G:O^C/O.=[>>F;> FRXX!-T>/C//F??.C7//J/_\^38:^XP<''4](TI->R/R/MMA :IVZ[#"!_F1NU0 =Z/UH.%P_" % C4 "-D"3*@ #WDP.Q94R!<Q3''H03_L1OG_O?H+8<9LAT4L $2SF$B4H$^$ETD"A1D&UK"PXTNGC2EF%C"QH,JB+SA^EDTNH3AQ#HA]KD"0%R"CE4>&QK%RYT.VK57F!C&3X$6[M27FIAK2H @@T>1L"3A%&)4)<8@BOY@L0B)(;*AF%S*U?6)U*5^1Y-V:%V-W+59+@V!&]D+R@6WIS/FYL2V@[0HD#R(8:)N(D"U*!P@SB007]Y9:)RKSQ5: $.*D$P,)$3EM185%#,18TASO /QD"V*56BR#$1%A!1X=&''Q)4:]Q)5Z=V+QA! VA0QT@[OGA$Y@_9<Z=V;]H5+>AA9<<T7H+''4AY(I-20L8NN<>KS$%6Y=UW:=^1Y=^>_^,%,WYX&FNQU.%2''1 MZ-8)B(@@H*)JBLHC I!@U[$:)_J(K%\ F--":X>]UEQA9IKLE..M-0U;.06#3, 12RNNOO(HFM-\04''CCS'',LBJTE@K)H4X6(84H3]AK$\DUEV2QLP=!9O?0LY9<^&:JF5 @J#*#0FLJ'':^8B5KHHX''LK!0@Y"E/.ZSJT ,@"10$@ A..OGI%W!$HD\UTVJ!I$KEX/''D#62H$DT^Q^@Y!X!85EEEO1[!]IF868BI4JBAR-+I(1RE:=OOO0EUKRVBZL,",1L.D"CO3T1RT\88HV404D''?#N2$89)C2)0)1 OJN^&(J5KTTT$-<"''/&)%GOJCHN>.@<3H2:\RP;.@(EUO"8VJB#X@)@PBN:H%0''5!\DRTYU,!IPAH.2ARDQ@0_#Y[E8" K"SYQEC$1%F\;23CF[<DM-2DGP[M6F29X<RMA3*Q-M=K]1H7709 .3@!SE*@Q#<^$;E#?B(@_R05X8HF9B&,F(3X-:20&''ZQLIHO.ZBVU_(@1QYHOQMIF8 <T8^HSQ#8!A0@0P@G#D5'' <Z!NQ==5%3#[[#-1,9IRH\H3ADM*%MJ]^^;9MM]"%''" R:#)X:^BTISY9YZY=,7'')7>^,:^_=/WT*G&"&:8: +''.&&-T#\+Q1+O:MP<@DGL.:P;=ABAGDE\BVT>TWFX)XSMV!ADEF6F0RXOD[IS!I\KM(F7Z<I]F<& =FPAHA;X:J;-P7,$);=B7!BRS@Y-&5BM''HAMAWE)A$ Y]./KSLV?)L>N(M ^)!W$4)<^& OSZ=-.GOGHVUY-,$+33(ET\F@AJLF&BU#:@?4\?$FPAXIPOYDABA&BZPYJIC8BQ9H8>EFFDH9YEO324$3H+*Q].\ EF/9K6!M+==0MUBS4RNRKI><(HWY<$<I-V_G?[O%HYZ$TK_ U,CY?PH3%F5@ -ZJ$C#;HVJ-1M$HKVN]U7%!R6!K$J^BSQCF5R PC[1NH=(8 G%8"PB''U4P18!H@X0YKDJ[\""U?+!1 ]" S7T!T-V9],S"[I@#&4$[&X;MNHQ13TZ#@FCDYD+BBN6%@)F^LY[%L-H?S*"G)8 $X/_P)*T@F@#GG5*W?5JA"4@U$D5+!D+XT&R;<KS0KH@+2P%.IA&5!DK\!Q/U;FX@OZ&(A! 1DHT 3"Q7_=RXQ_="NHC*R#A[@1HJV.EQ)BJDXT*OE^!Q3(-$)74IF V(!$R&\P$&^3V^#+9PRJP07/5N!GPO!''KO-F&WC$C0A6J4"&  @H]4CFJ=+[FQ&DN415 26AY1L[A,5E&%PVIDF4X0PT&!HQ#!AQ <R8),RY^"H)LJM8#.: !O&GHY+F 9TDV08!E!)N])6NIP&#CQETL )EP+I<QAVH[]Z*,W JIVS,AN*;TD^:VN,+W@9L2C>YL(P??*!41HS)A7ST!XVWY5N>V6P+O-FHP(2S%AI;9NT]>,CJL+IL"AKD(2-#%\B5=I31-I+LSM_D6UDS\07YB.)TLI7FEB==OWQ+?&-+DR!RIWL]F4I&"\VA$*S$%''ENWR##GMKU^''/D_"#9R1G)Q%UA]%V*=0N)R,SY(_0%<)''G.0B>4$CFB0X3(V;-60[G$ZC1!T9#YQ)HY/^Z4AH,LBRNX(BURE,=ZLC/ZSI4%PE$J9;A[K@$&V;FN&!DJS6W-R@I!$9+FK)Z387.I=0;B!U*T@9O4Z9Q%74\PDQ&$+8-BTV]!R1*X \Y.= +#\*@1!T552"#(XHFOG )W8P+LX@!+D.2ZQD\;U*XD*<CCKE!A#D/ (Q&3BLP%&$G]R?P"DKN(1WR''&=5KXK\V6W5.KVX1W/V.%;7-]^=;8Q-_>[+7.<6PP &6$P+X?<!@G^LU;70AGF@AC9#@ASZ0^+>[8D-<U16#J@DQVGDGS02D..+(;7<OWNA&YI\IWRUGL1J,8@2O&LP%E/BF1?/]2&3TI0,MV5A8!EB-CY_F@3N&6B2JGK0242P(@0@<_AK$DESII7_0BS^LWNP)AS''I-032$:D\YR%O&\)U-/JU"60KT_R"\Z7H #Y.&VP1R7''LSEZ2&Y.LYSV/&\5-]+NU^_DQF_S!D.RHAYB)D^T4''=''I_G:3$Z&A(L3P@L%''[+JQ=43&M2>Z4X6^<!688P^NCNPNZP7JTL"XEGS822$5=/R(]L^;)NSHT7M\I!FJ9<#&\\LTBUBV*2V!+EX''0?<T). CCVI-,PR4^-Z;)$D2ZLBKU+.Z6LT6=+FQ''V1%K9/Y1:YAP05AAC<@ @=D6HZQ^YFLVAL[6KZ.]SI6_^-VO7,O.Y[DG'';=[UOP.-''-]/^;8Q7/Y,]ZD'' C CC4\4]QLB8DB^!CHFY-"&B3@-3F7/Z,V]DG  =;7,*" T=*Y!]QMCP]+78EJ2P!"U[''&-XE77XB$#FKO.0:5<W>.K1Q''/I7 ;/V-0[3ITRB*PL4 UOM^X8]YB3AS>>\N7H5B-%2/JQW''U*#+R AE>;@!6%HAQ)M+57S7PF$6-$!J$>AN,>+G''V-P<LN"/!DOI+XQL<AXA=V&PITZA95HB5]:5G?[8H;;FA5*E0]:0JK^2+.$K\<GVPU]5CF6^E.=[]OQ^9V_<+Y72;7*$!];^; @0A$,\*PJHLZ''XY;D9:R^G\@O.-Q:X_V3<;9-(>>;%U7>!PR0H5Z)J!FQ!&J;JZAEC/H;+\NK_7-/>IFIPD=.T>RSHRZQ86+PE71(0<.5WEO%]2.QVILYDPH@N@NTI0=-<"W_%S,LG"(\E;;TPEJ< ^VB CD8;C/XQ0_4M99+&^%>>/7"#0 #44"XDO''2-<>>><O?*:X@ @0''9*N;F*!4BD: D''?CM@*TDUU0,MFRN4,P@EVD$P *@D^7L;)+NK0\&/3*&8J"F?.-.8CB0<DQS@D?4%0AD60AKLN:-XA@E3AL5)IR!A@=K0OENXNG[ZO>*X"; IO>4*0A4?0A740BHE0BHWP<C(P;;K! 4*"E]PA@C3A@9^.B[;A>5I0J##P>''90;)).B.BAF-QD-R#!C,)!<1!/B ZO>+#NBI,@B:DA<=RN;(*PBNT0C.%P:)+NEN8 GE@BT:[@E28*(QZ*(]K(@@''QGX9$KG)G->S(@W6/HORKBLC 14!OJ BO;#1P>[AP?1"AF:;@''8#@OPC D9X.@:_!:-10G$ O>VQ0;)B/>0(1N9:BGN8 E9Y0Y''+!C-JA9(H+.K"BE;N"DJ"!GAQKFPA@A"XQJ&!PJ*3OE5<1J??6(O="#&(NPC''F :6LX,ZZT_=<[%W,*#2FK"L<089F@PC$ _CTS ]UHQ6P8AOTHP)/TO+N;!N^H/+4+0%T@PD@(HZ>HQYJ0L"^@ /-8A.^K/>4#.("8N5@8R&& PY]LQ-;$^''\8PL@@@4H@ 1(P@YJ L.T@_,RL HB30N]3 J?;2&&<B. 8QO. @/P 0!8(R''Z;.4RC>*&X@(YK04CS0_? O"X,^:B"?=*8UJ(9" TT]OF(?XF4RE1385DCS0RI"!LKZ<D0BRPC &2(!V:PQZ&@Q"00P>XS//TX/OB2@@''@@@*8DP<Q:M<(/F([ +#;!^0 PU''PP/_;.0&@B+($Q;?ZW@U$_H+O.@NS  X<H@[>"@UQ DW@P?9AHDW8H<XZM@R ^PNM.<S#%D+9NDN=N"COJDX)ZHIOAK.V*DO:L@Z5DDQ7O@_ESK*P,@]@B@$]3K9.D8R(OG?=&T<].QS,GD/\\>X^F=!%H%NBBHV@J@[+&K)<B@P(BHB BDW?)G-ELD]7.8N9/D@P0D@,"DU2 DW:0P@!J?8(JHD:@@L7@DX4 D_+#C+8.8SG( [MK@!\]L+ EO/U$DX"DDTU D@W*@N(FK)R @I?@@T9"D^<ND G2(M:UD=KP#>O,ABQBDL#_F!NI@E-HD_C*@D8"DV/.D97&8JZSC.(&<5I1O<G (6?5_OHA#HGG2AJL)(B#ZMJR2/O^-N]9PDKVCLRYK&H+"@GJ>B:-3"@9NQD\HPF0@/>"J@G/V/G;VAB@3 7#3"%$1OJ,KA@L10B%(!E:IBHM\RJ 2TF3+PQZ>#K?OFI@JKDX A@B!!CYD1.93SG[  GP)R@8E$M^6@RK>"D@B@B3ADF^;@FL51C9(!:*Z@BKJAKNZN>)8#+^@!;%+S@AN ?6BFC7?NC$C!9''KN+[#T470. 7;.+(XN B:"^\+Q?,+BDK;!;AH2:"H@F!H@FJZ GBQ!B*F3BS9AK>-NE^: D4< B1:DRRG2NYC @;RNA"<TF$BAD* !@XABEP+TD"/UJ?<$$ =F8,F@0P?<#.'' XQ4 L.+*@A,J0QN&C1(@X@7%=O4,$5AD@T<)5Q6V#"+SK1&''TA"4@T!5LO*4%ODHLTR!4SYZ#4&BP/ZP882N\%%+3H6T(4%VAPB]9BT<]_#\@P$<$"[O[.%\H^)THQ88EN:8HQMKC1(T8P:NUAQLXE[RL"*4C5XY+2R!HQAD8Q-VHPF 8 &&L@R6EFB9@.<(T"CZA!#*D1.44A68(S$ED 7](QPXXPIVE.;B2E,S5R+ #0-TBT?5-D*! !(X@P-G5Q4RPAW,XAU8 ^(^TCUMK6%/[5D/(UFCT#''NP''XL- A%=-O@I#14BV4W1"]N*5LA(AC?,@HT-DEU4]T]1-@.(R?O;CI01UH;U5H1P''X3@;\,-J8((HDBH5W89.E(M\DB57Y&O2DW=JS(:%L_),I@(6K)#@H^S%TUJ @(:AD:+:L2;YQQ3IT*P@DT!@<(RCH*; AXH7[38DDQ*HD5$QH6=[AQFF J!$J-_L,(AAE,J5^MC#D-.-G&F,Y!BHHP\GS1S TO4''PT5*AT-VE/L7@_C @Z-EP[11H^''$*J?KG3&#M=@ <ZYHG*/IUJ)*P9I]L](IL;$=\*1B<IDPHXO*@>''5@*2B<MPVDUN/LS_@I0/]X]=$EY-:HPT''H2''K[3(JHPV.G>Q G.^O[I2A@:9YFAW5E,?:O1NU1''RQCJ^V?3_"O*T''$/X^"J#"0"XZ4";%ZPB>0 EY@ E_:QA(#@GVR@S]4!AM:NG$O2="Z@X5^IG*[F@:''.;O(!WY*S_XWA:+RAI*D"S*O.C''3A_%E8J#; D?"@"]A%^/+ND8!/B%!AG[X/E''I1> I5OQLXY%LW@@!)ORKX@:''OEK RJ-[4;N1@G*2OI#,TJ^55[OFUZ%"@(-RJQ?3%W;]8&GR/];#Q^^UVL #"Q#>5J X/E-8RBUZ!K)-NDR A@GIAEY(3>*@ABQR!:RH@TT/OU2;@H?R+H@@@Y''6!B)D5E]9BF0#!F5BQD%SAGW*AC71!A9A@EW0A@F8@^_>SE>=\HTI*(V"HHF^36C%''$!-F8Q-4E02Z414(@Q!@ P.(UCV)C''V-08EW=4K*.N7N;!/.XAS,(@R(HX4PPQW:0PB2XV>!44@=&AAA&BQZS2"&8HF\00;&(P:T88P[&Y!4$5VVY@("=W''9)G'',]/#*D/%J4!5N]Y6_8/N$K2E9-?PVB@&".WFV]E9EE.6 XV:(T4=G>OG88@S\8Y"[[#W7NZFA,=)D(Q20XQ[(D0A4%.$>HA:P@@C:]"Z! QJ8(Y#''4WS]<B-T%7SRF^+\]0JFV!;4MN;, O8<PQR (!?R"*ZA #4ODCXI>R@01TQ5*81(#4VUNZF;Y''$S4W\TA''#?%IAKNG[13MG4LH>AU]$@A8D\>?\N,DF]4-HW;YC)N#O*_BFYV]DND#.!.2<MF ^+]&H>JFDC*>9T(T:3A7VH+>NT;8@PT*PG''J]6R#G*/"DY53C;5#@$^TS96!++&&AQU:<#_HK+((L6H^ Z:<>-5<#''7GY,2.M A2U!]_H\69C+DATNSWTMU=,(P+J)!<,N''>HAV3AB3F%8; C3&-(BL4<G7R;.MG.H\<,LX=-E ^P=;VRZ:MO/)I,5NSO1=#+*,&>9,ZL2Y^F4P!$_0G, *=#, JR5K0=CZ1++6#@*1IY,%RI_KH)_3P"M3-.7@>ZQ^8_73@X!JJPUIG-;-ZI6?3"[EZ,00"M*^C8A5RZ AKC''\K=B..7O+_5[?F#Q_5*A\V2!R[5B1JLB3D"H#%6RD $<-:ZO(VTW+7G3;\ ZY(3,(U=L2P&0-2U\#QY:=992_2(C!#TO 5$ST]?;0MU.+9N[I1//F7;LD\,''KZ''.^5$S/)M[.[5\I?4;(Z5NDFYQ(8:&E\(A@OSA:YY;/@6/@%&Q/B=PAM4A2BH/)$H9#=.OJ5Y;B37Z20=0:-3!E_(/Y="CZ*[!QC=EN\CC^F7/2]$H+!$Z.W[,S%*!^YR3<]K/Z3<TJT- G>S!ZH@!EM CY!GO44]%B$S!C-C @=C)QG(!AC0!D2''7D$T%G.Q8B_>IPA$>(Q] N;:M\H"W$YU]UG.ZXRW3=\T^2I]@9U1-7WZ6LX9ZY\(Q(#K@8@:( Q!"ZATL(A0L0@?JXQW O];UG]9%XAW<PM;1''];U?]73/]?=?]<AO.C='']7W7P@J_!Q&8QN4HQZLS!R6PVP"CQ?VW^C%W^K5_QW87PC&''^H9/.L=?.MAG-=%0@?*G^O=8AJ80WETPQY&(T7T Q.0ANO#/]9KW-7+_^IC7.K_7P@$W!4L&AB(*E0?PQIF(^!WPPX2''-?#W^$1/.RYO-85/.AC_.*)W.AGX^YW P?6XQXH!WTP9$Q![5=V-MN;''XH(OB''4!R2>$RR,YQS2CA:L#L#?,B3QGJ7.;W;J<,0S.B%AX8DNP)G.;3;0AW?0603R;(@V?B9B&(D[%DGZ* S:?H30(23O/IADMJ(%%232+R3M@E?2OY?MCA @5ND"!"^L^(/T4A[''^I/[2=9+(#1GM$#\I\P#5$4Y4 DQIHDROHDR%DDY^I<R^K?7 5?8^;?8 _?8__?8!=?7&_?7&??9''S?:(W?:)[?:*S?9$5<RZ$DT)"4WR$@UY(DR^LDS^LG9"9?9)=<^$!?84Y?=#_?:+S?>8W?>9[?>:_?>^W?<^]<S;LDSW(DR@LIOBRI>JE4"P$PRK59=%EG29EBY1H"TJ%I4VMD")X,[N6KL>EBRJWJL?1@2F*U0X<L^2''!Q, ]Q(4VIGQE-%L!+($^NOC7:;@''4)=B QB$!T.XI$R]^2\B(D$V$EP@@].20,FM."%T;V,6!X9DL0B)(;,*ZOX,6+]*5[M.:_P,7+-29\<LA&CU/"%:-T:9N@PUM4Q4@+TPMSMV*ERM2($P1Z$TDJ#CH%JMZ+(39,.[L''C];; 3:,6#HBD.WK$0NFIEX@HR5H#_(@2MAIT_[C(7;-.;\/G_;;''0:Z.E5!T.PIDJNLTKR/Y-+M(5PUFH"**UG/+;<-7[''7K];)30=,"!U9@P%G''1@K5\VN?#Z*\L56Y5U]N/[/8<?/7:7YB<AZHYN^%#-)YT]D_=LLUT+*QCA2FSJ@PM!X\JA%5"EE%:HXXXZ[,!!!192^E&ED002''"#&1PHLH:JP*NJGK+8HX8023$##!^R4H(A#%3ERP$%P5T #]YMM54*DER8GYIIJK/$"UC^*@ 09T4F#7!S&,NC^E@FF!P=Y>74IY)!"8%_KWW''5)]TAWD71SU<IL"!\YIMYE!54]-:IY99:;,%''''79^I4(*M7:PFCAORQ\^UG<.2&"###8J*Y:SUR]J!JT)F&&##QU9JV$SY )**JK>&Y (G8"2C0B <MUWU.+AM8U<=H5IZ:664.(_ @HR2AT(M703UYR[-,JDX)DUVR"S2"8KH7ULCMQ )UC?BZIJJ!L\16266&:K8Y@W%"PHUJ+D2Y!"7&X;Z[)ER*\Z)<0UZRR7<,:;XUP)C.YN%U]^6YUUWG)9Z<@BC?2VW[N,ZRUWZ4[P!BH@\DORPL!IJJ6"#VE''''\VPZX311-E9_F3GH''ML<,\#&5122BP;&Y!90UVVL\(2*3133CS_[GONHN-<L,8;?=0331)CQ_SFE)KTFJH^F1444B$''C_V&O4)]*'' W>=145$<;WSOWVF?],<]JF25HB@ALT1VA^=&A]*33DP177@S7=9=UV&FI)!6*,@CMI0C\L]! T05.=.BFG8988((/3''####<N>UQ7>B78GRE0@?#_$V?N^^^^?7<N^.S\H@:88JE;[/+)**?N^..CH0@@OH*,.Z/BKF0)E,A2;<8;&FWN4(R+?^(ECSS&^NJ79(N'';''+33'',N36@>DJ;<<=Y_#77" (\0FNZGL9==>NJOG?) 5BB2:%?*+\%6_F?7C''?<=>U*M2&<* <JO]>8D0T4U[$CB*9@P45(J)A;*(K@AB)0 P1,(@L_BLDHL+A@B23^EBI0KPB$82?(@L0T&*B0KHU0!BH,HP%OZLHT(''BEJ&0!B5>(0''& X19+2 ) <"TKF,!"DGX0"?)<H\D R''@JUNK[C^@C1B1QTH!L[JHS''0!EAAK1K>;8A"!Z$R?5)T5!9/@W@K($/?<0"-D-])''A&+BTM!ZX017P@HT;].BKB:)-#%U"EQ7/"L\<:''FO_N2#GTO(C!B2JQU7TDX]PCFO_@UP"W9,)BL_B\%HO%JA/""^GQS1#^I=X7Y,,2H:?B_ITH)2%JP,)R$QBH))@CA_HY3GM;XHJ5&M\YZ4YJL;:N\*_*4IG_RH  &&4@<;/MJADX""LX>I3@XVD8R THP;K $MU[0QEGW(X%:R"\5,Z/NX^3GGMMI#PQM08"*VSB@:-(''N]J)3''TG<(O> ,[<IGL NN8#@*$JHN3CV\)?2>=6Y4L"UJ1%/G&/,(RAQ^D\XJ-RECE6(P5N8-"$\8IW0-BP: O"MJFC?*X,Q_Z!GF0+R#8(4)BYDH@7WMC-7(FNJ"&@UJN9I4)GJMJX4''Z%MZ6)BMWH%O^:8ASS*<A\*,R>V;>N''TW%GO0J:"''6>:N@4W((0-T5#[WX@XUQM"UV,G)QU1Y,]TN5@C2IBX19T*(L;0FDG\N!E+U%-*5/]&)8*X^&Y!;2''O_''B@+R]ZZ5/;Z/Z6L)V/0+6$T^400F&DXD6M$EM][!A6;+2%[BL9Z"T#Y/A&*BW.MI.@%M0!8)RHX,O1FJ4)B6-ZT>K6-R*]+V,[Z5+W1,KK)QV-*$XQV1#<PEY!GZ4*^BBKF@K7N@J];#DKR9)YW-\6_*V-*K% ''I1Z=3(R''^:?=R-+''Q3J5+QD.FYN2B(K6C:G/_M*+K$-QW]&&EX/ZA1K0B\ C2V);#L$V>>);/C:OYQN-O] Q[4;Z=?G9^:N>@W//<-,HGIM9!\*JH/M;CCO@JH30O<*;0T+%VY4J.W)_KEAA*]B!?8$H5\"G#DH/80G7I1X!J+^LT#S#FKW03#FL-81#SNQS1J_FI!6M S.T@BG52!C3: 8\Q<VHNKR53#IBMY2T^&\YM''?FP%I?''F<TACOJ;,"%1 H15H8@X]WBFLC:L82%H.,9''O#NX4*5''MUC;10?KU-,H*#J##+[B]/9ST3A)0("@D0@THMZQ3 V1B2B)4, QMGP4%N$Q1H/<R^A+=ZD]C^-JNO-J9D@4^TYQC 8UI4ZXH?^ !1T''P$BG5]DR](TQ[.-HVT/V)V[7*VHO:5[D.=Z1+_V!8Q\-T1IADKR)EF$R]Z=RX(]B+FR7)YD]:6XR&3*S''YF0".[+T*S;5)40=:&-W6=Z8O!J-K>R-27CAEZ*20:+L6!W7 NVK.+.3.>5#,K5FMXG8/X:BJ(T\Y9F#AMYAT''RHI!1EUTX@0A@EN^(D\H/UBVLKU3#CG>;0"@M*8''!"#*XA$H/EQNU*DJ]80"_><Y@7GNP$Y;#H@V92$*M\8R_/NL,)W''H!P\%T5I[X0S6N\9U7_N\R93''LV892''W<\8TKGS-G?!7;4''<]\6OZB2%Q^6Y_6,H*^8''675^-37 B-17;)K]A R@NLPU2GDP4236,NO"\^*\YR%TKZ,Q04&YKLJ]C4>-C\$8V!NO6HBE$0CADDHJSBZH#.5DXV7UT=JVQMF?FE/7.,G^=8V$N><II/ONT+IN+I995H8V+EABXC>J"$A-^D[3W!S[4,1&O>4J*74ND##?^;K5+P+:_6-@,-^="7O#0!\/(]>FJNZ(;0]!KN7]VOK9__HV3^^0E\!Q0CK<X<:T&OVYCAT9F1EC5IEN.0M92"W_],8U-B=G";15@_?/R+?4!4\/54F-N.0%R*8N.//?7%YQ+6&6X*AEQ#W=#W?55.T6_HQ8A+$R.K1S9>HUDR=S^XTR:-(@")(@*-H@.*<PG$,@4STF*O,WD7@ 0_P@RHXR?S\Q1SX7R#8" &^HKA0RBJ("I(A7>5\R\NI2D$U8L"U8L[-7@7*HOKPXLJ%8L<6HL*NHP>VB]A>HME"B_P)1(B GXM$!5D>GC957L(RHTK53EE87BW0(UBNGH::HTOI2E!&HQ^:HVJ &+/M1W71B=S%2[IP@/:UHA3"AXFP3,AUT\HP! [12''T<PD!$B(>(@4@D@>W0@W6U!6KI''\S(1"B0!2>HQ*Q6A69 R\E57Q;98D!L8''ZHX&^4X&Y<X''O@Q2#:H&# W^E\''@%@O</=#ZF&>"J6_FH+2"K;^^I)@"J-""J/#D''%&E0!#XU^JM'':0L+$%AT]D"G9=U5Z;M^A?@&+RA6F;\M 7@GQ\HX#G@IRF@JOVI00MY6<$\DIP@L6?@A\U]0^5],&2E-%XY,:Q!)-0DZE8H$$DDN"#@1\0I?9< Y.LD[2_X);M"N7%Y,?<"O@A$"@&&P>] -O,HH,\@J/_@X0/X#>+ [3PFPD#&I;? Y@:&QA-%(?Q"PGS&P3^X)8CDU;GL?H]PG;FZLJ>$N/8MY#FP?^#DUR@HU8BHJ]0@LWM@2*"@K5B@^Q"I6AK^P!1H%0E@B,+BHU*"T>P]>15HXB<HDA#\!&KK?%EU)%U_I!?CWBWS@B3)RZ%RI%VEY\VII%-AAHZ*1_0A IZ3"_5(AB",%R21)#G$VU0R4E<0H@@E''"QW2BRT F>SQB!>0C9@A#'' 0"A^0B$Q0AT;2@W]PC(L @JNPB6N8],9&^QL"^:.7ZI(I[\%2^9N7"98B"?0&@:7QK"RB](W&[IT9]8&V^J69])02JYY1]>42^>92&;YY&;QI[[+Y&;3)&,@9&;X7''K'')Z.5''#79 B<7 [BGH[]8R&9GG&*9I^^!7^^:''^KKI^J>)''^FV^I[''&<EI''K"9&>LY'',UY'',XY[-!1]2W9UPL"P,T''!7II GZAXW&XI#J9!368\Y*P?1*&<(7*\@W#4 *W8@*+$@+<0@5 D@>5TGBAD@J3L@ %P@1<HB7QY)/BDW?1.JFXFR\^F!:X=:D!F(>0*HNM,V'')@@2H4V''$^V,">ZD[J&''60& 4>& 6N*J6FZH:2!0<.!0>"*L;V*N8NZO>!!B*\WBFTRRH Y[-50+#8F7>E''>-=":VRZVSE(?,NJT$")B6-!%P>(!A6*MCB*QDN*X7Z*X?.(:1^F-S0UG+XV9ULP=-D9_4"W3H.HB402)8FQ;C= $_,@7_4@*1X@+0D@:1<A#Y4@2)PPRW$@;MH@$M<$WZ G6R,J"FE!UPB)0]FY*U4Z&[J&6_*&)GV'' ,&F#;E/>BG2!7PP"O&*F#%+%8+V%;7KZY,@)--.*)0D&+-1*^.9*+,8*+-B$\XI)*%\H#R_(Z''.XXOD(!&7&K12Y-K6NV>&\[/C&^"PF&)D)9(<*+.!*,/"*,0M*+76*%6[(YUB$U]4AC9)@WW]P6VNEE$6V''AJ!<AJIYB:"G1:H4)_DI 3 U<$@I/]@J*)@F+R@O"EDB6B\VEU@B*<@G*Y@M,)@J0+@J#@BBF"*RK$(Y.''$YGK.18X*#N *$4MDXG'' #C_I<3&ZNXC*%FL-,?/"2LT-)L*-,L6.3MX.3L*,Y&OJT0PX!\&H4_U !6Z*1O**1:E YSH&V8AE?L-),$IZNFW''?,3QK-UM+-]G:#%J!%"F$W"/UO H8+0UHO7&:T?ZZHETZG]0@ Z% B,IPC(X1FZ)@FC_2F/BPB-4PB0$ BTP0B?[PB(XPB>DJG,Q:-UV[,8W[*)T" -SX]"5(ZLUF.LI9+Y8:''+]).[%W.Y!;.Y''K.Y/+.X9''I?@"."&2F@PWIZM7)O:F,8HK-]G:-E&Z''RW:*=&A%.6"*Y*K.96[.9>+.;7+)M(JFUNQX[90IU!1SU\!XWGX[&G+[/FVTGD5$:(;GW_0@]YBA@&@CZ)0J [''B+J "$S@A](PB?HPB=RPB"T0B)<0B*90#0SY)3[ZK/DK/?O+K/I[/?Q+/9))+B8S?1VD0&?FL*3\]*L->:2)>14GOH,I#KR.A02@)3G%,)^0V;,FOLD3V,F_D+*F]6/.\+,E:R;54*0JKLJ?4Z2"*KU[)A8;(CA-P80C2K16!(3;\(]7^[X1V! @,A#J0P^A KC$H@2]XB2WD@?@0@-!I"#@,FU=<H4GA7ZUN<J22JWQ8V$P@&6RHWA9)7.)U+/71<T ,(ONJG!\L@.+PG=K^:/ )6"7&W\<R:PX_B]MNRRZF*TABV-]WBLEFYFTTYI\4PS,,QUY$)K3>\I7U"Y&)H1KA[59ZQ+VTQ!7\BL%0F= \@F)<@G_P@PF@@@2@@Z+P@5>\K72X@**LHF,@@BRLO>AX@^KZF3G47()9@@%@,@H"JD\#B25]K3@T_/DBJ3K&;"B4#HY3W@G"EBB;?\X<J"O\52J!OZ]Y#&"2@1+F?2Q.R3M),!;(BZ<^''D V(D6UUDG\E&L 71''X;,+AYQYMR0YQ4HGHW #BBDCJ(*TQM@K<$@M<J@N_"$C''2@@ (A=9Z@M9[@15&''GHI0,*7)1_C@A75"Y*Q_PB>5>((Y/5EDN%E@K@#@H*HD(X:L-"01*91HJ&=\!4N3QCJ4$/I!?^70@K?&5B"N_20/N9^V<AIHU7L0U4Y,X#<F( Y$XD2@@QTH^*""AI],OJLHD\-H 0C@A OZ?>V^*Y]$''RR,^?.[?A7^PB<B@H"UQ)T''K2JI)($D;3U6=2=W:ZE[](,''1F%%P@--0@''4X..!:FQ2##>$BI=JBGA]#1_W"\D6ZI6#:5U8]F+()%EGB W/LE>%APE=[)25MXSDLT@VTG.[0I%%XGPW''2.DR ''KBHD]23''H"B=]"\LVLHVCI5G<"Z&P'' Z=!,T-ZG]UL,-J*5ZG-6)''B = 759\]#-W;5J6]GWPBJT$SF]IW$?MG],96\6?<(T:)#+''=6$-IF_!V\EERH]\,Q72AV_B:[/JJ6AW&S6.II#G]M%NAJD20[0Y''GNLR7([QFC3"H:*QB''P''FUD9F^]K-3X(4,Y<HX B !X;FTO-?9TZ&&/!-''$^?B;3WW?T\\8[60**^GZJ<[4@C]@.@(,S0BNE$ *OTX:B9;QM*]F0V9DBG"OV.,:$XSYY("ZKYM#__M7$!T-(%AU>$Q8,DK7KLX;75-$L\''@S BINJQ0%P][ ZK(Q^]/O$]1;D&39U7BC@BDY,9B2&,1\JGA\RKJ8G^R/K]_1N</+SY''72B!W/MPU%1#$,BCT0\SH\WAC?W_M''NE^3MYPG.VK$&%NNQ$FI;1\V2T*OVD''S&G6.W0Y),43+\"XM1C*X@*AX@*CO."3P@&A$@CI$@B,D@ "0P+IX@*,P@M:.0^,$N",\@$LLP,I$@BCS N&4N&DK.*#S/?*)V;*)8;*)!;*''B8I$,@J_&A+*"DC+L@J(I;*($8C$7;+ 1;*.>;+/0;,0R;, 5;+(B8R)-@L9ZBJIKF"0M@L$O;)1>;+''68J5L;+*];)(\;(^K@.I\DE%>C)+-;)5$;-''5;.-%;-.W;-5M;+0>;.;7;+MB@I''98@^4@C^EBA(&_R&JT&;<+BI&;''?JSX[J%''#$4X4.GJJ@HFY#L:2RL9C9<:(?L77*L9''3@:(0L>A?X9 <DNMR$^<E@=<YW1F$?2''2O1:^@X788O0N@G5@@@K,<9><T:]1@K:;4MBWD8EA=__3O2T:G3I^<<!WLHU%2R1GMX0<\U \3R@[=O6[7?%/8''4?%:JR''BAUM1BY7PCN)PB:2 C,P0B>F@A<50B[OP=\Q0B[V@A7!0BVIOCLRP=Z[PB7 0B;V@=&&OA7Q?=7R/=7/O=77/=7<O>HE_B9\PC(QO=9]0BU9O"H.!/XY"=Y^P=(T/>XR_>I6@A8T/=,5PB>E0>H#_CHW/>XX/>JMO>*U/>*]/>("O=,5@CN*P@G\PC< 1B9=@BZ%@CG^PCV)?B[40>IL/>W+O>^D0>X />''=O>W*_=+T@C9*0HJE@AA=@BPCPA55?=(E0=(M??VO_CM-_B<R =)Y/=6#_=6_O>Z!/?.]/?)*?>Y^PB7]0B^]=$5,;]QBFC''PJ<D2??4<)/&]:+(\["2"2@A@@>K #6@RZN6!MBK*[8$:!.V%6DG9+2EC"MG__C#J\X-C 1(T!QX8$V]K$2YKP(M&1<>4N-5Z+@JP#4 (@ HH(UXHT&U@"0)4(!P8%V-S(TXPD_<XB$H-QSFU8QITC8J%!P8$*$9:L4LRNU*LG)<$KTX:Q*F6"JN5[IUG!U+@[(TU\*MK+EC-MH-"I0AO)W<AGPT81AZAVJ2K@QM5$^ZCI%@M6H.I%X^]@L C%?@[&7M''3Y=@#00FXM\?\EMR)<XJ2L>V&JLP5XWLAHL?]5;)27T4 NB?$0[$!IT:AE*E.ZNQBC:+"I ?XJ&84[^H\F_R61_>LH+4RAI95Z7K08T<^ILJ-@!E2Y8$0:#WSU<F>1 $6A@;V8\>C1LN:$0]@E#@"@%PF@C>R4 *Z"!#"K"]E7IG/-97:6$0<B$4*;IH@Z;(IK09SZ4625#A[)THRR3SQ)HL.@ZBY5 ;(LKH)VDC-) A#*4$V@@(A:+"UEE1HO=S\@XV;[1"R2;8S$ULI%A1[RTTT\%(A9*[_!%3H''J8R9L:>?IK2[,H$023*(LW"XRP1@H&PBY.M\NOR199:MD 9@0?*#8/X E%+1NNT6.4-TB+"3RD#V_)FH0&QEIODA@@HA;EV)$,-@-P>9AB]2>9XA\1EN?V4*E(@&JF5R%6,]H_?5&"DMC[Z[@LJ.B7+ HZ"!PY-DA+G%EJHM2P9?QSE 4* 9 IA @DF4#/.FN&1)AS2C*>DBNK-I8H0,.9W[D$"$1Y!#ATE2)#.<JP>]3CJ;-2P&E5PJU<!]J^PN?BDS\<;YM * "KU?\8/,V:+C:Q6,0W,0&L#/P''FE.5 [[U9)!C1N8D#E%!EE"E;DSTWVW -T%E*1AFIKPF>S)QWDJB&%:PN[ZKH%Z[8QD!-A/TJM8&K& B@S0+.FI!$;[OR''W&:+EZ$''2I:*:,>Z<[6H@DDD(@H6AK3@0A,>-)WYG=3T#";''86:L<MVJ@G@7)7^J%_AH$FAY$#/9*S/J4VU?*3Q?<L"%]QRC3&4KA%Z<IG;[4=GJ445C4E4CX@LZ0*05]12 4ZQN6JYX)A8"HGSPG\@NH":U0MVV"UU\C8VZ 7/"E,"\"PA8HI^:)!OH",%>$P"3_75GG@RAQD(2%XXNWY*M!F: ZAV%NDFG''U.<:4'']38Y$#*3"TKBO;.I8@T@O!^,:@M%;''@E9]3XI$#Y3O?E7[LKE[>[0< &Z6H2RSR=?W3:?4)1QS/LX\D\%"266I2[PD)=NMJQ- !B 6[4I 7)2@.57EDB(NG$Y/T#B"N8\PE2@LM7)X/K_M3ABF6L0 8%2@T^_,L.N>@DF)33D0WE%H(;9HHHIP@CJ08ACA%TY_<!PULD@C9 !0%$(1[,((>?MI]BKQV%O;IHCFH((Z&RL@H^G9"FJHSQB>ID317=2A1ANA\7E0:%TQ"*4XYZP:''I_@!F_PB@7<K81,>DZ!Z$P,7>J%VGU@D Z+I)A^NX-3Y7,JLUP,)IT(PA FP(!Q.*\@\7. [G.#CIE\AHQX5,X+*PA@$T%9AAP:[QB''4 )CV=0X(J;=@PKTJRQA<HPSZ"U@-8=DDT*;!C<IHRC&H(8"B)4L\<]%@$C1H''A@S!A!!QL#5YUD=,=*+KMO)B@3=X"Q3)VL"P)-D X#9/WZ($2! #)RD@+NX1;VNI8YH!O6:&,2 TZ5FGJ#NE412.Q&C?H:@AE_HI![3-N)I !C/TDP#"JN,[KI2_7G"&CVALSQ FH>"V9IFJ)D@KH_60AS^D\PL.& 8Z>5!HJ-VY''@?\XP6J<XL''K$DN&ZP#^/BX0DE>@$1EXNLS_ZAO;U 8I"S\ X$5D%/6\.MJ !RIU4IJ117:(CY77DD!-_.(F@5#-?W!J(5:.40[&7+U$ #.LYVB9173*C9V5PX%8\P\,:"#B&(\9ALMXV(: ;T:%L8$PER*3!R4 ZAL.*LN+BACAFP0B6J2YGXJJR!V"1KR^KRBGCVQT''.TTQBIFFH8B2*RJRA8BQ),#"D7KP(2Z0J @_%4H]M8FU0JL ,&.HLXMK M??$8Z5"Q#AE,#NG*Z#84&WM.B+YX/Q>KL$X*F_5/X:7(6H52%AJIHF@B>NFSG_(C@F; YJA^[J)JR @@^B#6@:H8D56/,1V\CL=YB7F$K9)0A656\RWW6&5 C,HTO#@!T&&ZV <:: 98A@(;[0E@$[9ACR>2=V1GVZKNA.QFVATIG-D2EG$!<85,_HT:&F0/1EPWC,TQ0X0GPEC^&''@@]KC XQV>Z* *5%VK,VP2M@K[8,Q*PF# HXE_8XH+9*LM&%D7!U^E1 QB HP3_XN8+^AFBG(TUN;D 0#P&@\:7EFN"W2BF#]1KTJXV% RU0\ZG0A@M$#7KT[$4IXD4TZCMO?29M-L8XH7VZ-+) @JZ C3JL"\K1E:F"]7W@DCVXM(P?XA#74@ A*AJ&XW,10R[2+N#N1S6F5C!L9C)9N]>,MKZ>JY,PB*3; E#MMWUGDGLM AFOJ@\)D(TZQ&^NH[RL2\@5U)WTGC9%-3YRE!L<HJD1H$E#Y.RE(]0&GWGDQY(804Y5K1BVE<5%!D2JEJP3JKV[@,E/D82G?1R!AEEK)6RQSJD!%+9<0,BIA6ZLX%$ JF[BREF%M($IT@4BA.EI, E?;&7Y09!Z&*DSM.%C\$AV\Z5PSWC* :7A:G;L_*E@PXRCCDI:R0(2[D80;6H@]5>1G/*3HI(^(1FO''.R1C?XF!#EUM(!S1$,T>H+$HY[1;&B+G\;9BPP2C "%P*YNHI8>1$(E08@AE\(Q&I<MLNXH"GYFY''ZJQL#0/_/#O&C-HJZ(CAE4PPNW\$LT Y)HL8NGEM2KH,V4 11%K$Y@$^GPY)&EN0->4$E_="=MU/F1]$,L)M# $I.:E]S.>PWD''(8HF^BYBC.9<()'';2>(EC$-(\+,.HO[#QCX": :NFG)K!4:8S]6R!JUHBP2AVTPK :\X]WA PF-PA$!I8@ B.JLEA.MDPYY''KJGQV''M P#AB//C_1[,RHH/#PYP Z.+S,+V9HUG^IU]DV6O&^P(#7__$7!&)T^SOU0F^D.AXS?<FO/$HRXTU&K[].2Q6_!(^75#L%3)&MO!VI%$M\^)1JL8PLDHL>"-0!<7!4[AY4(HP P"?.P)(S(F@?PDH20\"))VL,,SD@@L0''/M*N-+$M<N.0''W"9LJJ[^%.4!D&#!\$-B:0?LVF''EJ,##KDC#PD@@_JX87(U9)$S\>DI4W.KC1PX+P ]Z( +IV.EN9"<)FF?(PDX8''@ +Y ,DDRI1^@CC_HCY["D*["C15HJ2?,MZ%DH,6FI[]HI%\")''X(T60O@>W"?##*P>8(]X3H,5Z$EX4DLCZ2T!V$MSA&1H#2__>L*-2NE5@@ O_*6RNFBN9";L(R>%U Y;F(E**@IT_>8@0A0B67YCNCXB;% O5?8P1BD&3HY''\=*C6DPD%!I"D]\QG]@!6,[D8''(#5"("R"9G-7RL"YCDCBY!/69CR;2.-4:"@R8@>S;+HOA&C_#J#,0N0>LP<AYN4(30XA;L1Z+L5G@$PVSQDA4A4W8AF5PARK8 EP(@RI@1JP1(O\8DF"8@XC1AV<DQ.XI"W%X#F0 +"YR$5+ZB(<R/8Q+Q6C!C?? .@FAL*QP"PP1OE=@!4"\0[<A"TQ#*HW1G;5)O!F3J''DDGC&"(7 "EQ^) 41SKLV@$#.H!8QLNF!  -)HI&=Y#KV:P''F+PF"HQHRX!79T"T LF&YLN9B0@7K A#;?JHGX"AK NXB-DLJP,A*DJJJ 2[$1TPQ6&D]#4QL:V@U3HI\::YVQIIW\ 1.V LEYSA<)4[A0,!R<Z[R3R<VK+A&J>[BB_B^L"T!Q@AAQ(@5[1@L<.HQZTD.6WD.6_D.7#L-K L.9%D.:/D.;3L.:7D.<9D.=/L.9YD,<*LW2<979@ A[I@Z?CL2:SL.5WL-2T<-ZVL-Y&D27YL2?3L3E;D/N5L3N7D3O5D.4CL25UI5\VIVZ$HDP4HYZTD25''@WICD24''L3X#D0Y^43RAD65''L4K<BGXRHT4DA-^&D24SL.=)D4<^L3$WD,< KYYT@_J-D3\?D3*CD7]+L;+KK_%?;0D"Z.E,USCN;BLR,LK!BGO9=-J)QD\N (.N:24ZS!F@H$TLH".:K(I>;1O?L1O?]1O?.1O??1O@@7P&<"F#$$E\("RJ^LFQAQP?D2VN:@E1G1PA)5PB*5PB97P>/RD8&HD75FGJ_.DBT6V>$SDAVWPN0 A:H(ETR A76F#YN''OD*V2E2UQV AQF[5PGL7QDHV.W.B]1P ''0%DM\4H;=E0Z]:@XSL&_C/&P?HEO@Y@RTV"FY$ @F*!RF#BEJ=4CT=!RK.5RK?5RL@5SLQ5SL"5SL>WRJ$4FU#@EFT EQ% GUT"OV&BEPIBDJ-UROCTEK\7RJ;5RKN5SMM5RO>5SK_<=T4L=UDQM5CMM CW5T5MH F[0@04B 5K0@2^YT"9-UCR-4$_E4$J% SW] 4[%5DK=4%K=T23E@?UI1$B84 Q85E_=4"3UT5(-5EI AQ)8UU15UDEUUE?=5SKE5UC]4&YXN(;IQUM1$^)#@^_C''"J-FS%*&D+Q''=[XG1%ASK,A!,TZ''UVAJ&?=U''@MU7D]U7H-U7LMU46[K<RHD"IPAUE8$+D,+,R@#WNEJ+@!26:--7S]U0G*U7;=U7<MVH@]VHD-6@1;"*&DF$!!AC28 &S8 @&@F CA5="(G''.=U;AB3W:UD* :%+G4%#S<E 4*&LSY6G*+-:"IS7@-&G)-VY_?_U$A&+W^TX2N/TX 5TU^IK!__MZI0Y?H$AD:&(I/6HD)NEFR3SBZ%]\LT]+DZU*''_U*(#U*)''U**)]*J!Z*3$IIT&I)4J@GEFHQX6@[E4DNMU]*N*Q;RJ],L*="5''R^65[R6_],TYE.9[U.8)]-9.%.77U/XVB1;)ZQV8H]4*HTO^H)/"YHLV-V<%U&NF[J(,QE??[["H$/4^E]KT 0C=]Y4?R[FP%%4II7DX]**GU7RK]6''3QOET!3.@%5D#H2 ;Q==P4"^3QZ-P(58& H;MDD/<"GI''[WDLE7 CU;!G]:'';Y#GIP\06I5X>JX4IE;''_U;(#U:(#]3DJ@GAZ0U)?70J*IEI8$(S:YWZ>S*VMKPQ.O5^<37_4$U[1"*VJB&ADM G5O F5%BXMOIEH)7]QU$;E3GAZ$TMI=LX^FBB"^WX[BW  !E_@5YW##5 ]TU A$9 BG9 BU[ BX9 B&9 JWG MNEP@P <UP@F+>TX^&UYD"9 ARY EB;!D5[!EC[!E%9!E6Y!FX9!F(Y!CM[ ;2SK0#W^L0DP!DW AP;"AGY D2Y"#3W^VZMW3VW BR["HQY"I(["F;Y I89"HKY"B9Y"H>Z.,YP9@E@[/O@JTF!C 31O?O4UD6MHW REN! D1B0GVX!#NRXDNX9#N)ZEN<9#OM9#O+Y#O99#PNY#O_<V9C<^YDL.YDP&YDV.83#& # ^!U"PASB@X3"VAR8XAT#& %!0YC+.9C76YEC>YEDNYUH^YUL.YUP>YUUNYUY^YUTVYS:N!U @ 46NYD_&@#C@YT%V9C/.84G>X4K.YT[&@$6VAU&VYT,69&Q6YE<F9&[.86\V9%;>9V".86%69&.F9&0NYTQ&Y$/& ''JPYY-K% [*''3U>MJ4<849!)?7-$A!A!5GD&_)DSG*VTP^=Y73N93/P!FS!Y0_5977.Y74^ZHH.ZHL>ZG9.4@!EQF:HK!SE&Y.H+#/  XN.ZH.>ZH3NZH7^:G.&L'')^4BH#4^#ZA0T%TYONY8C6Y8@NZHG?Y.$Y/\<XA]A;U.&Y?.^V/&&Z-.&VW.&T/&&W3&&\=.&^].&ZY.$H-\<; @_;UN).THQ@ZX6HHIWXER55E!O5UH72''@HS6@E^2@Y*$@]94HZ0A&.1E&.0O&.4S&.5W&.6[&.7_&.8#..5M..1Q H$(@X$$@]8PHH%4HY,.@I9P@I84HXK$F/CO&3DS&3EW&3D) Y8:@Z;W IW$H\''.@A-DF.;)&/FS.1*BF2;Q(J2K..17&3RK&7S[&.3/.23! _O%(],:HNLPH41!-4!S^^*O)G^<$(:L!W=V ''I&M<X P2+KD"+_B^<BZ; H$G%M&97C%K''_&;(].;!I&: KR_B?>DH-R$RQW 36N60:O9.<G;.8P;Z<B9/<3;/<,Z[:1;#B@@F]_ @W=!ELI[*  1.2*BL=_0P(D7.R".''81YRN4BG^P#0I#M''>>[/9DY./SG.?U;05EC0A:>TA(?09W[0B*_0@2=F?G:;<Z0TL[XXQ''+*@8!*BV=V_+--LU''HD%1RR%DDI%VM62U/=I91F*=1=HY]/V$IJ>FM[3BM_^RP [Q1HQ=2H"?2:OY/IE>RW(@GRY"VC<LU #Q2:K9J*WYER .TN)A2K]=2F=?-C%^X.;A.T3$U]OYGEO>K+!336,)1K!)AN''+1<RY.II=3N+_JN+=3OL=3?<;1<WYHW)Q*Q_=HAPB0AV# AF%M,?;V\4U_]C%_]D]?]D!'']A''''*%TSATS@ 8XX8>3&1TS_\2YE\#;7[>X&Q"U-#L*0;$"/<?F$;UU?=D;W<5]O]T=''"]L AS.BD]Q(@/6"BD.Y!/K\F=$=<2R9:(JDDT*!"@MX-R@G6"&( 4/3G?9]<MOP''6&GI3.Z=&.7]&*75&7/]&;?]&<O]7@_=66O\''L6A8,@!W:@HVU(BX%(#W$HEFV5&G&/]7*?]7/O]73_]77/]7;?]7<O^H@_^GOX7?5EA:E-CU@X<H[X[,&(-DB9=&2''=*:"^F2/](/W=(*_^H;G>H;_^H<O^Y@_>X,''^X4/^YP?^YW?37"V3?XCI9THBAQJFTEN10/;=R$0XXDELT'':D7X:X^_TTGO9IX:.(F=^[D O>_ LW7(HY7X.S6<!+^?F$OC K!J!!PYUVI-@$U=J^?(#+6>/C?//A*8%[Q"5LY2#;@"E@V:*Y?+>3WA"YO(K[?+;%''.;U7AR[?B<''?,J#7.:!?.?C72>=7/5A/J<FFM<;OBBMOE,IAR08G&B"HK!HP 6<G&"$JN-&.:>>N6'']''O"[.8 #WOQA?/QK?7OM775K/05;1A3T@REVKV]UH$CBIQ)&HH[NG7\Q?7\A?7]577_;77 5?4\S?RI\N))$O''>6]?T?71NW?;_#?O A7:(E?;(?7?3:,_=*O?4?(&@@U\X8.!<2BA1+M1Y''''B JW"O)DA?"Z!<21<J(A?3?^4GT@!35J#=5M AE "TM3->TH"''/1]; I "\BCA  XOH$Q(#.ABTMC\Q_DE2(8]QP8EP+L3LRGG#%LV^ 0)\"SI$ $OECPG:--D]>:& I+B\&CL#2YA&,2)\2_O''#8W3+,1QQGE WZ&GC7Z9D@2@J,^.(0ZEQ+T%5M\W''WGQ"+W+%:?  4+]"3Y,&[O! OP;B#R-&3][PS%C-)D"''XEQ)"G]E:S^V0M6 4,^CC!0(XOH49,12=E1$726(T6@Q+F%Q''-POM%##E''1X0I>06-^CS)4*YOT?=,D''"J7"&>(I$3L\7/MAZX)=!^+O-0Y=F(CW_>KY27:\?CS>MF1>K[WI!HUP/T^CSY''Z]S0VZM^,ON6^;^/8LO;?7!I[U6T@)D''?;**F2_8BFQI5>>M&4W8,?GO7<??_7Z9O4WHH@CB. _ PXV6E>BA?87''8@MN,"_!@;R<Q<5<;$B#70W5O\  0L*>FFHH9KH''8\QK''!""ABJ.BJBHK*(8(XA6"^OJ0A*\<VEM,(CC87=/\!""3GB>NFBKW*XYIDSR)" $"<RBZVQT0*)XH_51U\HMZ:8H(0,ELU$E4)GJ]^T]U1%=IIK9+#33P9-*.%RAC]T]U6]8M4IU)7PQOC?DEUU?R&^(F[U@,@,+RW%%!7=_EL^M?@@DJ&$$]I2A26Q7)D)@ITB\D^''&D8Z**"#$%***Z^"^.&%*HX@@CVQP-*)):B"Z"(--6:ZZ:6;<-*++;?2^ \5,78J[J:+F/.+*)/^"".22TH[+[R#^-(*OI:ND%5QQ-U1%BSU9S&ON46<NPED3533"3/(QCGUG=CP"VZ ^IYEUY<.X_X''UWO)&>^ ?4)UW#O(*C^E^!R9L0,@%L RB1\OO11KKF''DH ,!WD#LARDV2=I1153H@+GHDC/,<L ''&8322QF/#GGKJX<L<<,P#8K1J@[8@T@6C7?0P\\U-3032SDSW_SJL ==?;SPJ"/]MMMAI2512E2$T/T(MOSQ#@1 _B014EBCG[[SX)M]--%''(9566J-<L@(X%@C Q41TFS1MT.X\5P\@>GPE#[!W2[TCII?0XD>;DF47ESQM7M@D/1D8#.^<X_WK[:AP>P&09%>%]Z!@)KS%5%TJ40ALJ82HP P#+X "B"O@)HH:DZ4P(S(00MP.R".-@DON:[N?++((-;,>>>:8;48;;\X''O7/-2^?.OOOQK0>=<,=CK?752)-.O^67-4HNFM30H\(G-IM#./F8D8D<;<87[336<#/O_/OQ07<?=/)O_77?6&^/^ G\G?TH:C?(HP=9<$,^>E@W"?R!3''_*$??^@)^W0N_U+7[HF0\BY7]A: ''0_]/;W V31;8O#+BBF&S^A(>7/-%1$HD0]BDMX5!CF(+P_^9+AP_[)4K^G^=:=''BJ- 1&EK-M S)''$$)&.LNIGYSAA9M@@PC,PXY5QTTQK)DKMMBA.[$04RTO8T8XJRLPI%9%S>OBB%5\X"97@BL*3JDL/3Z7N_JXA2T,"H;A;C@M!;@B@JZ W^)DPX;!*V<P*AL@NX  B D@X7V)*57:0-^JU@@#E(30WR0\VS/-3Z=;@&1_?JH''2%BV<''.)IJW7%$_A=+T.$.Y+1PRT18 RXGB@)-2%J)''GR..-TIW88>T(S2$?X1HS%\LTY#G?U8%L9YGP%Q D7 PX<S1+^)I;/QP%LU,93N$A,G:#IBD8V="?>55O_^)$Y":3%;=R?.>]70R&OLO)/T<R4''&:.58O''OHM=BP%@$!!@\FZ0+_+.LLW77#HCYP@@C#@H!D*4@P6(#DWW8 +S''I$C)*<8">WLB\;]\QLE.N4TJ28YA)0ZX8]V>*RP,4BGW1$R5HN@H5&7HDF/AMEBV9''3]SE; MD&H@*U)\;8J6.ERTXW %^=5MFNAVHN\3_<1P8SJ-VC:-U)Z(G*[*;FK9P*.+#JS@>T@I4_,A527S_M@,H/Q9>]W-,/R)WQT ?.=HU&%/UZ5_9"-^,5%V+//S_I<&Q?8)CD+I:>HP+?K3JP;?"\G<J-N)$V_ <F5(O+,W<8PD#Z=%%"#BD[O6,ZN=JP1=*C7\S-F\+%DEDO!KDC-5J8!4L$B]_.HPSO+AFDZ"@!P6\8@%B(@Q7.NNKN/CK"6=JJQ''I&C&UQ,U1T:A[O1RGEHP6!3,O.^9\X"H/NK$TX@J[A7+FAMA>6DE!B]!''>E)GAEU00PRYAHX(IOY^H#P2!I8$ "A8Q5?8;WNAO.1%MD&YPPHO6L@EU%<4G=-M;652^Z''#WU)1BTQVB%"]A7X0BG>YS''D^VLDIY/BHO83 2Y)X1B NLX%QKL1SQ-I4TLT&?\:9S^M!E,PYG*]^N01B]O=^](@L=#D4^;''#>Q&9 AMT<# M_FPUZ!"A?"T1;U2;B*R09VA8X8EA>8[[_,PCBL?@P!,"\PH/!FDROICCM8:BV5>8B @<\DT_2@FU2DW%J)F[''C,X8P99.DNK @K#]AWACSAB :L%DI\65CSGU( 1/@AK25)&Z!RY6BDP@IAD?U:''5EQ<H%K\.@H@\''FIVGS/]''5]J.)T8T"^"&HP9O@4Y6-MYE/7NM^5[.0''O3&HP$90*Z);G?3@"%T _1N+&,T5,6?-[E4?.=''PU.0HV^_J8QU2^/Y3,FL1RM$P6?VDJ4R-.A=K;+,&NKP_7GY_455Y;(G;7^7>](GCK\<CV![?^7BSPQ5T("42HUFIM4@([.W! 4 5X!''"<LX;TL@CS]0!GR:Y!==8@M5)3JHTQ0D#(*M"A32K5A4@^JO%T()RTC NJ-NM@FX@8@;(L,\U"/"()L>B1;TTKGPQ$L,,\ )DP1HA$,@@0N4Z2X!KELHTSW45>&Y-5$6Z;+>N%I6A55''5[VK=:%*7N-^37#5<@''!7*2NGH":)Z$MNFI"77''(@=])5-,O=;WK7N-7#W/]5R!NZ.I.5KF[AC=MM$'')CO"[V$S5-F7/31:74<@>=N.CCQ3/2$I><B#F\/@''+;''%0<0MH=) (N="FJW/3,#.4@PA\<L@F96#CK*3@@2G0XFF.?<G-M50N#VB8H0IH8B#G81R9@:R<8;[W*C.4J<^9RD]MUK$CFQ7"#$<<AK(5E0=LZW*@(50_I&4N9G+U6 )U=D8T]<#$; R!B%%P@>7 N94(/$FNVK .EZ^#A2YE \''F[ ??>,<???_/??:''4>KED''6% /A(TO@(S0>I6;''-''0JNF0C>W0QB8@QJX@URXO;-TJ?)#"(4@3]P %)MVN:P$?448@"N60JRH@@V6PT:H@M*E_: 8@''J(@/NX@3R8@6VV;;!W2, @ C4 "?X@T88!1=M114\EE_ U)+0 @T,P0O 0@[00CN(R9/L 2= 5C84@S*8ACAP01(1 "\@ CB(5O<''ZID(FDH+6@D#YLOK#QD@1HH<@H@,EE<S-HH)\HL''S@MR!D@[XPLRW@H7\MP''%DN$1HKH@P@7)MG4"X_@K@UGI@0@%@9>?Y_2\LL6)@D0*HK<)\G8JQT05@H\2$L''2DHIY@LX%HA!7XDL/D8,ZLLJI-8+0&H,2"H=<](I$\L*%M(&/T;+CL<,>.H/@&L0B.L01&J/^U@A=(H2WHHOS]"PD^L3P*L,,%44T"L18!O+<E(+0H4:8LX_XU&"EIQS9@$].\LM@D@Y5@@TN@HNV@@DQI<U.(N8.I0;ALLN>HD$ZI\)%LLT+HH$P@LMODWNWHH;%@L%PHU\@D@ P@LW_LK?UT3ACG2@N:!CH@PZMU@EC_#AM6!ZU@@@J;"CEM2AN: BMZ!B;2''"]7SN98&N0SRA0)!B#^4NN]2A:9#O^:''CERPUG&RCG8"BCE0@FIRCL/!AJ0#BI\@CJ<0ZO0!CB%) T5: T8JS N&NJ@3"T&[^@4K%T6)%U''K%U'')%U1ZRMSFBTNENK7[%V_[_MHG]U;H%V+)%V6Z#J>6NZ?&ASMVEDUGD[B$Q]#2DI)1BA7PA:0$AN:@C''.FKI$2JIOPCUN3C] EZTDHCISQCN$@CM*PBF8$\SC1_G+(CGSSGI;#DON:^R@H@''70CM>1KZ]*AZ)8$/P#L^X!N("BER5IPDEEC?1(0P-''EP IP02UH$B/LP.>PP3!8 "IH "%< B(@PC%<P.5P "$XUU-J95/.E0Y!7 ^$ +B9C%RE8GQ:I7U>Y7#:70H!D.5XD;0I&G!NI7"2)7*6I2#-''3[FC_[E%%-0&S"N(;!@03%V@ 8,P1/0 CXD@3R@03]D @$L7A7 ''"C$P &<!E1\PEY(@#7X@S28017,96)&C''L4YI.8GF[LR  X0%0@ DOL83_@P>RL''D-< $NDIN>5)''_@UAODY&06 2AU%SP!@AIH"#34PR?0C*+I@7Y.T"1< "3H B\@P7.P@1>@ P@(P3''8#''-R*_:!&>;\C/"@ "_QE5DFVIVB:_=;!"%!G]V+NQK+FIJ''>YVXY*W>#B&[/"$E4 >V4*U^9BE,Q,\NENDRLYE6ZTHYMH@3P@D,ZHHJ"ADKIIP]?DE)M,D48DLBNA\5XHX" NX4BLL%RHH;HLH,^@J>KMQ>6)SH$]G0/TRE/ P_&@,(_N 7 BY]6M8<C%*L%,U+#$&M6!S)=D<!\\MP_T@B<@D>3I)?DU56*,:0\$L,/LH,)DH =@DQ0@LW2D:\""$CI]Y!XY,D,T9O.QV\\*.4^*\77YK*CHH*.Y\E]V.[..&9^.-;Y.L50 4?XM=_I@)9&\&;^@TK=BTP8@@LKLL2ZLH1MLF>4HMK,@@*K@9%B@LXNFS?@* CMJQBL%2EN A@K]#A#S;EFWFG22F$''2Q@J4CCJ/@AYRB"GT BLT0CJ=3A3E7(FVF(N5ACBYB#+JJE^]Q(%+V$HA6P>''@C]$8@DRR@I: B*!6RJ<#BAKPNNW@C:50AN]AA:, @D*A_^*(+]XKR<J#B>;BO<9@CD3@%WJ;+5;Y''!?$NN_#N;CSR?G3)5JZ%MNVR6++-]<Z%=-C%TXPIW&*DSB''QQ8FCR?C@D+QAE8!CF5"@B QCD>@YD8"\80PCNHABOD1CA@SZDLWCU]"AJN2*N<!$J:#T8%1E"X[&F-''AF-1AOJ3CU\CC] &CM!"@23$NM7CGMK#\EB"C)@HL?<6E1<4EX\5FA64BDG4A B(X4.;0PR@\4%$IP274X"7D >+X@2W0PQ*P 2*8@#[4 R)<@IV=KU\BDN-LJ^6$ -DJ N+TS"*$KU-.J=""K5RVZ^A=#220 "*&F))&;?&6[X?M;?5:;P#M+QE]V%K\)66M(1 A@A@40CIT0#K0@C-HAV9MAY9ES/A5''D YY! QF''[MDQ!M@:B5R\0Z9,+-">Q<\M?XK%#@%E:(YD450X6>YO2<6!4XG^)H0QW43C_D0")002*$  10P3''DP"-<R3N,3#)H@"Q"&?+&W;/"F;VA$!IS68WES3O6#%H933X00"CT+?)^<QD''<QI_LQI3,_>U[+DR 3DWL7GT>Q_/?APQ?JD''XEHAFWDEQ*C#,^5ZY+E[^+DXZ3DY->4XL?D^J=X;FX=+=XJ<G$3@NPTC=45(@$F?],D;<@@\?L&[8T.$9Q$S_QE4Y\TWOTS0XP97M@EG_Y0[.(L)BDO1SPUI/Y1T5HF^#/B>YL8XA^08!''A74J+.S$DD8N(5$-HU(F''T+TH:LDKQD(D:HLD''2HL:<APQ%HLK)88@1@HR#HKY)Z,JU!, E9$5C5X6-57^"V<#"\K*1 O+%BT$1V\\.6K#.Z%[*WL5W7L;Z?L:X7L:,3L88\<$6WO.@HLA6LL2D%H#TY$.+Z@DJ%855:<7ZWM:S+O?GLN3N<-3OB?4N5_[M\]%ZK6@C=I''?6;YW/)KQ&&B//H+ KKCKE,2FA&_U>2YVPS0U[PBM0B@L''ATU(B\''D!F/H"EG;&2''51F/'' T^GRNSI50^-3,B&<+E5 RX#FBHL1N;A#RA7CAM6A#:>PSLL B;U!XDGESMVH5-\H ?(3BO*RCIU''XVX&UU,NXUX=5DFU5V*-58(%R5G4];<#_I 5U@Y(''V$/YFY.1KE:5O)V5C@JQFU<5@;W5V!L6LTYR=APR:63C?-[-&AC!E@01''5J0W5XB/2H0K+!D9O1A;&U6Y9-$)M7IAE_EAJLI]7BTE*5TZ&/WA_,M(%5EQ''DEFX&Q?61CQS=@K!,VU>Z -C.<)._U:F3"JCZZ##=_4 SPE2N\ @''YS-CNS &4<_@0 _OPVI&BGZ=U=7V+YWZ?4''Y3<2^1;9%F4%#"55H]T.(LG)%F9_):=5F!M=.:-3F>]:?EM7Y3M73[-73_-5)ZV?<,''P>1EYZ&G^YI6EEM$#G"M14[^C#US;$&^B9MYWVFXHOW=8GS=71+-8UO^GYGC=&6S?C\#"BH #J$X+36+65L ];X5%]@APB\0#H8P+?2 @)$C&;]=!*M=$;7SRI"Y,"ITZI=LD]M@5Q05NJ8TZQ-[&^?L%QT1D+QTUY,+%2X9L_IJL7>]EJ(,HJ+#><X4#ZP+_<5;U78FMTD&AT$BXH?-0="2[\[\;M\5&Z[,?&[H7";:+JA2TIQ?X9O8QK9H''!;):.@6S^\;;E5D=Z .7&\D3* B7*!V7_>VJ,,2Q>EQ]5617$6_]Z$Q7S3UAI]X1O?3SF[H18KG;*!J7*"H7*(>;D>6]T!FX?."O  S<F]G(2^4)Y$!1E%V3XOU@L%&3H=X-\&_;Z.D9=.=057YHUCY#K$S$G,@ B1Q4UHY$UI\,UU_DLWX YG2\TTTHL"TF;.>Y'']!L]+K,QO@3\M$J^5XV$*?A(Q%HAPYY;.XN,!*\=S"PK0--I^*=46C?S?7C,:<_,=3X=11<HD4GW0:$8-=S/?/+="XN_;/"<<0..;03O<NT\<OP]P:)1''@Y+NT-T5M&:3/T.#/<-/_"$U:"!UO2^^''=?;)1?<0:?<1K,<1@NYLW$ZA@''2Q_NQ\"1ET:R8U3@0";/8_<Y8)F66N;AIG]GIQ=G\0^8:<WFE80C_F&WEX+*DJ9S$GRAE(IS&.!"_#$/W''%DFRW4(S/LAZ+\I9GQGR-+2=Z&0=0;O^:WB1SQLJ''QLJ.@01#RL35PLFLPBFNC=JJQBKJPBFHBLJHBL03PL2BC>8T>M8#<L81\>8#?>8!->84=>9EL>9A=>9C_L7OLWH<SB4#6P1MRL9S]>8W,M9DNL8:->9Z<>9+O>:;->?>1K/.1_?.3[_.7#O.&G3N9#SB[F6"TM@!\H@A"L?-R0#NG3/.IO?,_\/,%,O.IWCF(M#20D_"3TCN1''O,Z\?.DCS_H''_.:7?.7+O/ZCO?&O_>GK@-]TSLO ) _5$1?P[UD8-*3''+X*;!BX,PVV;NJ;W4Q?TNCP@!CE(T=0UI@#M#)6B4@(6''JKPC,NFA"]V[O!-()4)A@-"ANC.64^DB><0YC#M506GATDU!N!N(+.O;/(U''C[SGT^KN1,2/@R &Q4VC8%*5C E6"0@)( PXPRLBC%Q2TH@@LCMZ%X@]>9X;])5:5^-U[VB5W(V[U*5Z=&6]X-U%"H"*%)=BOSI[_=^/W/9=/W;U2=X[Q>@2[($X5,*O-(@M4X+;9H(H*6H2IK''=R) ,889]?[KMY&(U*5DJPL P>NA#Q.M,)!7HA$-@3D+R Q0Z)&#YPQ80HG)22TKI5XKYT[*+"V@T_B8 QG@^EQN];K:\JL&0=4<O+F&=6$BD5*D&M?R0]OV:+,"YY?8HI_I\N[7N#IG.PH@)&L_@L* _Y\99Q,;!IFGF@@X6&^TB*!YYP(D++ICD_A8&-B]V BX)P%3#B)*(0O,ZFZ)2ZBR#AE=@G@%E0RP*@BI^NJQ)1A-XH2''1U2P\HTZIJ!YH1-W.$DQBRQ>9JLP^^PI<$X$%U2R2RZ]_AIJI(??WEJ^MZ0ZIQT"O"CME@@.XJ]HILH,,$!WWDGBRA&''SAKMJX=\L<(89Y2S3#+-EAMMMJ6J)YUZ;+AG%@$.\8UFU82D<\!B(''0S2SN[SGSIWJ2"PQQFFJ&+J3:P F_JL14MD%D<PS7REWZT!OONUEV54=MM,<$F@BQR Z(UW C0P:M)G%JM-Q44$ R@UV"+3Q5MS*$D!&VV8ZDZ&K:388>BEHD&BG^&>Z:?''I[:Q)Y8VHFF"4<X&$@XX*BQ Y-1!WD''&1K@":@IZD1JPAY7.M@F) R@&TJVAM218:M/.LDHI''\F(VXVTD[Y):UWQGE''EEO$%T&!PO2@A%"PDI E&EF(?4G*$8L(GO&''(H[Z\LL)&%BJ!,$$(42T[NRQ9[#/#)NN(RZNT4#Z $G:E3%9&8!79JJMO!+)!_;E9)M25HFGC<$(@TBJ$\NS]="^&.B9O?FR?!+,,LT6F&,>@IAE%EE:ZHZ\U*#9AA"_,\::M#/ )]/(JR9A0)POABD-E C"B]C''''T2*[VJXD#+\IK3G_/11?0+2NA5 R@M&Z%2''<G@5%J_P[3Z^ ILIM4\*V]X7!JC9H0K7%GXG7(HA0@ Z@??E2RU7O)G.#G B4X%1ZE3ANZ],EEHEBW^Z.@@#Z)BK*JA=1@ME&2%@"T]^Q^Q)2D@;*@G%&=)+@&BB #:J0OZHV/<BN10@Y##J<=P>CKF)U*@"0.1TW)>HM):U%( !DWD\9@"X-O8(AA+)@H@:Y@B@WAA!@+XZ!_<J4(SXJT=14"EV0TX".PI>DHPE!@X5@E@NUPBCGIX#@#3.TK:D1J-5.S.\9N3 G8[\KF$VR,A((OJAN>BKM#\,6DLBVCB"IT91N-EIBI&HMM*H0(EM,]3T^*DQTAAEMP''Y0S2&$H0;BH,''D#EVIWS#BA;   7 &X<";(@U@OC@J$V\P$% D JB6@D^7>&GJFQ@BQ[2SAU4HE M"S\E\WEDH54!XT'',X@!97XFC9#LHIF,G@KJPIG+\.=7D@F@BZGCCC. P&4>@X _?CWGHJMMHB AZQ)+ITHXO]=C_7A0"2"-2QBHZ*T SH"R!03TQ&C01F1!&YS=QSJ4\D9''FQH:33LK1+8 LZQ4B 5%MZ5XDG-S8@AERTZ$,T^LN:<C P/0COQ36)HLC''A@^%&J94\ B@DB\"@]#\$O?%^=_28RFL959SW=NR@@@8@M)Q$OE5J2&JB1@A60@DC+C&^<TLM@M[5Q D.@@Y189*6@.(PEI&JQ+Z=1("CI(D@-U@D@!/'' EI_[0K2D")7T_&TYDLCJ<PT1DIBF0F_^2IUI7X@>W;)"@RJ=''!1KP808Z<Z$].BGJ#0"P$E>34B28FK=]+V0)("FBU/E7A7KP??I5?$L"SEZSNHIL0Y%X>6\SM2JLN3PCFJH@A#A"XR,?/F8 OPEZ@G_:,5<^YJ1+ER3$(HFDN<"BGDP(@RM&)X50.#R/+%/"P(CWDN %#Y44(@1%.@B@;W%PYL?K"R;IN)G50XQ H1&,XL$A@FEP)"&&4]1L;\B+!<24"5=4GDOFVDX^*BBMAZ,CB:P3CWQD470:N0AJXVH  ''@C *[@51P&D@=P)DLUTDT+^F"@G5DLS1TIXLPTQ"FL;0B@HC"QE5M9A(@CSDLRI_"FEMIANC(&X8J,(@YBTJ(P$]+!B.9RI8UJU%-TD.T!KGOYJ?DG@GK(URK2B]):93EN]G(M&/>AWV7XINJI,86FDUGQ#01"\%)_"EJIE^DYJA ''.PE/FLX;JT8,I!NUR#%VEYC]ZBRUA<A<?.24S+PPT2P#" >DXG B5BAKMD(;QKYDH"1^"N*^FVL&P"LU@-4,DT3C#8M6[ +3LL]+X.MP"83.M+&! "L4(XJ^#F:2NX''IAP+FC_>H1A6+:ID:Y&^GYJR"B[G@1$^91!I%?I@HLNFDLJ: C>8@+K$0NR5N=%FPD&BCF0N5STG:(PP4+LI6%IRIK:A!C0N51HMHZ1>FC(02DCF%1*L! *S419O/$JMK+%AG8 X"S-/HB0C+T:.UN[0P[IRPGEJ)%@P/.1I2IBN^_''C?9$Y0Q!ACJLQ66BK6-(-&G3;AE"($EGACV.FIN5BCFJN52G\>$Y0JI,4NO8D5Y37;SDJ2XA)<>HP6<EAZ!B23ITL5G9Q]2&5 -#XWAF5E9*2H1X_X(P:?B![^1K DL";#G[<M+#-F]9H!Z%A%I4E YRO;\X( 1C<\RSDGX5KN?]&P ''JF2SHU4)H)0LN%M2,"QZ)LHUHFIX,GS''BH-&29^@A@DMATV ''  Q) <JG_]:3MSG2:O8NC3QOT0DLIV,DHTYS@E&]+GDTX(X5Q3DLU<T"7YR^B/$!''9N)1W0",.B@JP!1&L ;J,TS(@XATPDLQZN!73"^F%N:!E63PFOJV?>F9O]*\]PHW8HH] AFOY/"X+@0Q:QQDP,4W2=5(@W6 WA]>J8608B!ENRW(J@;Q7NR&3QVYA7C<89>Z>M24?DO''R)R9R?_0SI\SV2?"^N;L20;$D1= 0RU84\<)64R/XKMPTE)]%E\3^CR-(OTF>8NG\BC0@<J@!#&$+(*Q0LL#LP_=4Q#BA5*L ,ZR8WHI&W''VR?"AHN  P#*$ 4"L\M9<C,CB5.?*;BLUV.DRDH@WT&DPFFO\(NDRN&DB/.LC4&D#BDX:H"HDB"I]O&<"E,=>"JB39B&CA"HPK"E[ULDSDB!<BLY:=$D!R,H$8(0@00[!K*_4*& J].@!VHC<[/=KH;0H#LJ(VH9EM3KN3RZ"86B"V"8B<0(G"TJK''.S%F8KJ18AO7WS/>Q2GZESNG\@@B[!AG1!!(1RBZB3B@1."0L#O* 9 %R %5(3N0W#B/?Y.IS[B0?( +6[B]&*P?P)BG>:@G4BMC5@HVB;&7P)FG)INH^( I!P!GY0 GZ@AH2")4'';IC:6LH^2CB8#@C;B!E"+%@ A 7M0AG*XE]#)BV+BAC/+ HO!P[GQ(,4P!\KXGC]5AG&:*HBZ@HI0)G^:@D''ILI"H-CSM1P+JLC>)''?''SP-!HJGU @6,2,H-@LM=(@=)J0H ""BYXIJ^1@''99P1T[F>GZQ&''QO"BI, ?S?*)<\A7(HI*4(:I^J4Q5V+Z)X 4LN@J.X@&Y$SUHD@@4%8!O: RG&8P[&XW5(PAF& A=L0W1*BG^L4V)>4P?J0XG"Z&+@"GF&PA,VY2G>31SL30>Z8REDP%3>A2I!CA+X@PC*S()ZPQ@.P1"-SPAU1!7&@2ML QD $@Y"*#?(BF1>@ ;#*+M,$Y% H&P"2XK\0QR@@Q+J PX^BS9<I-UN<& DP\-*KGN8BAPRX )N1 9<I[^F<AZK<NH2+ 1T(" K19X$YO\*SF''"3G<T@)G4*-C:A1OCY&/TZ%#$D_\JKH.8L&T"PLE*#@ ,Q5JR["]TA!9L@LTZX I@HRC]PQVF?0\@7RE]*CH"S<OF8,(4$$$K<\0MC8\Z0,^P4&.S @X3U4-^Q/DC9N()Q L&%TX^[(=8Y@H#/&E7HF4X);I( EH4!AH@B"D[F4H[Q(H!ZL\N#L\]E@FT7HFNY.H!T/M+$EG!YN/K).BT#HJ!)OD3H<(Q/+OMX,HWN(8!($E5,*T!''J?0(*>^0F*P^F:IA(H/CT_I . .B6"*4@GAT@$T&.CUYH7A>/F!7NDBI*CB>$L]5(\[Q/DS;*@&.BDB]EL:"\?\?D@T/$F.#"%XI"I<B.JAB":Y(@D2RV +3@\>4OHLI?R_XJT5QTDT, PX"$LXB:H[[H8 $.$Y.VDI-,HPCO?/F2AS[LK!C.Z-L.:@JO''GC+P!11AHE!!BDPA $S:&(3Q/7UQ4YF8PJ44O+WZ%\2H.KG^KKB]J81*"0$!M+3JJ95(.>+XFB!,"OW8F<[ FG$6R,J:&''*H/!@+LJ0E3MPRS_#Y+MH8NLR7B^)(!G@+F@ISAOZ8M5T;3HZ6THQRHG;2.[SR4GGJ)XK:AE_*MHS;@<R3SIAHB4"BM5:34&*C!:FC4!I9"EB\ I%"!FW)IE''IAH[9!/7S"RP/"C*S,Z>RM0XYR1RP"DF)!E<$A^1 B"JJJ.V2G2$2UP$ROL*BB8SX''=S[$FZLQ[:!Q3\0HF7''/N8IA%E0"Y6[0W8J,9RS?X*Y8[''=(X1* KH#*MKC&46#Z!?)R;1;3$SAC4F3<<ZET8QMP(1WXPQZDL[8 I!9L-G[&UC(''Q!!.IZ8*%QI4244[@!"4 TEDPQ.8X0_\PQH>0A5:0QL6@/G:4E''?*S!RPR)Z Q26!CDT@W((\RUY@A#$@P1N#FLY0P::9T@"AL=:EP@$@T@[#<]2X (T Q+JXP+J[!QLH!&HX@+J0Q[:L:&F\S!K5"IZJ19(!P"&=X)L2R.7,(,&# "K1^KJRAM0PS1G9<9TLZ1$"B6WSLMPK"]":O_(-N]FZR>A:^\LSES9]N!ZI-XBE^&^JR[:PQC4P1;<0FZLB!G.(E4T$2A4?0(<8$WCL''E"J/QB9X(H^NDNOKM0V"K<O@-)N\HD^HESG,02SYP&+OZZRH 0Q*DY2$DQLKZE$B UEH ]EK\!R,CCWJDDFHJR''.)+E@<@=FV3 CU8VJHD,(D[$BC]4-TN4.DS,(D) _RS!@4CVW\",IZGMIPYD\)ZX^NKQ&\,4<14.CV8 LM?4","/CDU0^@S8%\U-&X_U$D^WFD BDKP#ISDPHHO*DD["@C[XHHZ0 EG<@L9ONDSQ*E''I7R*T [L#NK:XB,D>?E]=:$''V*J_3&+ITF,JV$]^1JM2Z1B@ED FQBE%M;]3!8T#%&$*JSET.9@"9EI;&8#.V*DY.DDYZO<LBPA ;;C%N=X''*.QEYM(7 7!3P( WZH?WRLD#Z=3EGI7)??9E:(9OW$D/I+CV\*QUR6''KA5''CR:E-EX*&-:# GVIO\T1 H]1%A3PA^NREF=SAG\!!@6VBEPZ!GG+@O?"@DAQ&I"QADDC!@9K,/>:@E[;AC;S!F;8!DO@@F&# *\1PN$.&B_:2[3,DW6T->5IU>I1X 1AH>E8BZL)''_]I*!O5PH''0!DG.AH$^O\2<UC\.Q9ZY,VM)1@F/8 4ZQB1"!#6)!U.0#[#(8$/J*\V1(6@2H''WZHU+# C)HGD5T+5\)I"F#2NF7""#NQ^87I(CXG_AMJBB$NO$(''M7:+HI;?4Q5X K#(8H6 5B(&8 V0 P/2"/L00 92HX"W42!!Y2M/JB()9#-<2!YKPE04R#)W3U9W04MT04<5Z:- I!P$I]GHA''D0KB^& V:O^Y 3&''D4&*L76*L;&.WTJ&I;@]Q^"1ELH3*PRF H\*0NRJTO=JD>V*YA^*Y+&*Y/6*X=.#6Q]RTU##S,88O''"RM51&+B5RRC9?E:8V\Y"3HB11WT[233JRJN(5$]LQ5K=''B2CA-P2DTQPT.;5!2<D"3ENE).$REP@QJ( @)6HQ)8(@1X0!^HR1V! P](Q=/(HQ4\*0QZ !/(XS''/(I^>PQ5L82N&8CK%<34R5!4X8P:>P7*Z?:-YL_O''J-%3D@*+FK()9F([<&].N%,K[9D&E8JPH/.ZAT/83LXOO*@D,*QV3 Z*+X8=5SHBRN6T\W%,Q#DV-,%R&(H1UDE$;!L&4AP&[,@$!O-+_@D(URF1V$DV.LDPB$9.9JZS55V4P>''9+C[UO$A0:*\U=DLGTR>B041<17 "2+*\7ZD7''LDQN,@Y4A(57\DD;@@88I"J(V=ZFFDU. D9.HGE_HD]N$HYVHD\A@8T9HE0@& &'')-''FPH^\@/1+ORAN:QCM,QC?+L0.3\[P A5CZ^%N=,8@^!N+SP"OF0U5 EFR0<L)#*LOA2DZ:!=0\*6+:2GV7L[)@JE8!^J^?=,=7X<N4A\[C+!Y>N*ET) EDJ@JH>/ZFIX\QBH_GD9!M7!@>@!G$I0Z;VTA8W"A>4@EOQS#M7A/GTOGW*CB.8AE+2AA2CAGV9 OE$@NKXRXP^"I[:AE?A#N_[I$AG9<#@B.+9!C4P"@Q8FCI9[I)K#NC8AH2R!E=0A$)7UK>4Q(S_"CQ_L_, !E>HXYUMA@EIA43D=EWH[YPV@GCP=6TA]48%@@C8 E%I]@K )-3= 455=46M]5&^]5&/]5&?=5ULA5#W=@<!AE,2&G@;P''\H.DL!A@H2,454]5$]]DCH=43?@5TG]6E4=5GT=6WD]6;M]6;^]6;,=5%G]E^:@DK;?K-R=K)-VXY.L7]L5OQYP5-!EO]&(/]*_G]]7OQV80AP>8\<KT1P:J1;@@HU#@]%''GXT7WPA"(QN-?TVUO]9#PR+,7],#W.K+O[]Q>@L.8P;"HTN+GCV8N[1_@5 L(BCJ6+17 BF,(Q*"0QDR@QY80@T& +!\05**5A70@UXLPQX*,QZ40Q90M"]"P]?T8QM*[&):(@PT@QS463%-8*&& @? @P24HRFCCCO)EW;,<R"6\(J[0"&H(@\@PI47H27D? ;6XSOD7#OR7#G08&22IM&HPC<,"R68H"/D(.;A@.7UW.?7_.5%XZ: X*9 !^7W0"30?"/07"=HUA=]L''B,?<JM>,K0LXO.08K/J=<-1M9A!FEKI"L++R\!^DVK ''AC3<0$M @@HH@D7.D\( D@4C1\J42M=V$G&,CM-]0\!1F@-AD!DD"_+@W8@H/GI>AQ*WI/.]''@#O>C%VK1XXXH&"D]>F@M(I<O!FG:>TC:*S?:>R@W+I?;+S<W!HG:*7?;*3?;25?:+S?:,5?:5S?=.[?=4_?<5=?=8??=8S?>$\@S\ D[2"GDUHDI @D "NW"$44XM#8H5?AQ*A@!''50C\0%;NC@^QH\ML2[\.IB#1(8 O8+4RCI$2YD&T:K<&B-^L%FLU#W344)T@%-<8/DQ% :$,I<OM0:\6GF",HX]O?<:''A /''"\?0E*E6"ZK5<B\A3\21I!T&LVHDH''.)J UYEJ.RK^VWW.V+]*6[R\*MN+UUS1!,4R5H-IJFPA>]*X\&DI8R.CC(BPALNB.\^L?;*AA\03@!SL*W)310LSF''Q57KG05''"_Y<VSST107A!B!BZ#I4C9O''''KZWQOU45QC@6VZV+$):/+X,QL!->+#2IL+W<:<>_EZ@FXEM$38LFEPSY(ALLVW"B#/#L!==49^5G_39;6OG4>$?W''1;,.''''5>>O/7;=/O#77?^OAE2+P@C(BJ)%LBEW%FU4D(+ZZC''HG/-Q[" ]7.99==>>&VH8XXZ],#!!?''AQ9< IX#BB#3?BX""B#''$-E_S 0:JJM97H*8W78W9-T]NJ$P@09^BQLSB%7 <O$ ^^SNNU6E7?85W(8]P ""%!3"&$(((0FQYP")Q-^_IGZ,DM 55 1''F@#)SI@O@J,_9@ 5$5@@@PCEOTAGMNN?8HB\@O\3##"*M)V[[\ZV]E),;6/C&6V:7^^YNZ+T5]--''2(FQCPCI@ONY(L95:.&''3U4BPCMM%C%]XWX\XL\,6?D5HQFC-LHHW5GU&&VOVR:X9Z::A!!UJ8L0T*./.QH[(KF6C,-+,,\.">233.X*;[F-$OM+J;4@$L:B5'':@9Z;IS%,,+^AB*:28:I:+[+O),+,.,?BZ>2:4?<5R.6BI^9W#BQ9\@LLH% E>J2683@X(;E;"4-,,DX2TPJD(KK["\HADJD"0+-\.::N/^3D2J<N3Q--./BKOV;J;JH]K;J=D_ICJW''+5]X\_ P46WVC3;F@GB6*V 51);EB#BPC6QAOIE.>T8XPS@OC"Y16OR$+(\T7L= 44T=1&ZFN5GX)\(90F:(8".HE*=-&_!!N]]XJ5[T\S7="!WX(>2'']!_7_''+S_^^:.G''-==F2&8 8C3[[#_=[%X+R"B-C\J@LI<HN,DC@?>->VAD8;98Y,W;''''''(F,.>.VZ*7\$!G*)T!L9*/3[X)NH#989:S#Z:M=^, 8BCN6\2=8;;?>?329<<LQ?#*N/J3J2E2T@?F[GMJ@L!2)52X@Y*S/3>DJFG[$@4D "YDRSAR%YK@C@D[1IA$7U#W+*]VSW(05?AN+GK??=ZD<&Z#N)D&X*X^;81#Q&0X5Y8N9Z17HU;!SX''PT-4HDM''AC.IL#@BS:P !B4X@P/2LDHM+BBC 1!,ESTL$[DP!EAZ-E7I%[AE&[P!Q"LXP\5RDLX3-BFF<1!CU?H00T"\DI: Y#$6FL0C%)P "ETHAH3FLH&W$,=@LK!C&V(P2%Z,X\71FHU-^#CI''K+@;]J@@C<8K=)/L4003G#@]SD)#YM1 ;_.AP.P/FNF3!CC0A8 S/PTZ#R L+?#?CCG:D@J\!B2 <:?I-^&P8@B,N0R!I\H*J+7KL$B$WH$!GR22T5&\%J\)JR&?R$JDMI2$:VL#8RR&UMIMZ"V.&%AHIXDBM8YL)Z -JV&K2%K''OI24?6\)R8=JT0 [''KX_JRO@/BT,M6A;L#K\&X0RS&J*_I,@'':I9NWOAH6M9%MBF$S%],LI3#G>\52^/N\7T3''J^N#(AKI !J_:@U54D ]:5Q/E]^[SC> ](-<NBDS4VC@D-;@A2=4A%K5>:O:CF&Z!TZF(QC=5O9*M#7BQL@N(ABUM+H!C6+@@1;].H@68JDM$YYTF2]M*T%W"%JV*+R%LG6)SE5J49#V]JX6?;T)RT4*C7$<PQ/T$@\<\CGR$RK!@$#8!D%12-R[N#V''SXW*T:MJ5Z%ZEZX"EV%O9UETZ(R@F$!@@%^)PP5-\KV*T$4+R-^JT''!85J0(9^)U4T+W.])U+W_E*T.Q @A9HLDU\!*EGVJCQ%O)#F\@0L_5QNNXF=3 E@C  AK.(HSF-NZ!$*&Z6P#I4O(9MJJ UX;Z)NN?^ 9FL-< Q #61M+V./Z5,H6-[F]K6=+Z%+Y72J5+O;F''N<#I-;\M+''BGR=3"F+^6/.TF\GN+6>L:=;_O#Z94)4/\5\*IF0B@Q2<FZ95FR"<0"#E@O+$6FT:0XQH@\LHWH+BE1 R#(Y3-%O?=B+&5>XXV(/([ETW''VY!/DDXU"%B^131FLG>-+LCJJ3B8D*3 KCF80P=V\HPQ_FBBS]!_?"J0 06&B"I,(0S[PJD*UEDBXJ "U!.6,H@M[FA!IW#EK9XU#H/(X!*3VL@3[/FMY[U##?D81#WVLYA3SNP_>8+@K''X0H0Z!"%Q\X!SDR#JFX;3#J%NY2!@&,[AJ0HPF^?''KX@:3&L]L9#J+NLL.G#@0R%@BQ#3*.=T1SA4B48_D#-\1''WGGK;; @DB8 04;$M)=A47(PRO2Y/:;FTZ''(X!)1J64[X,4)L,$:T!S&-JVK 2&E:'')0&PZ4)O.-J /;V%NU>^;%@[EAC3?L0E(JFHJ(O"FK;81@[I9>-N73+V.]<7+W//:5<@N-+A5OQ3)^S):7: EM1I Z5@86;/CG/Y )-46Y<M:F _@]+R73^5.^3/Z1$ZU_3E*&F>(X):H-(L9@''MO>9;&A.:@!S''X\HL<E?+^>F[(Q@5C[Y-MHPI(]D\#BXO-LW7;8@!O^J$!GY#X/FXJZP AM)( O]#8U>DX3;#F-?6?8TCC/>8PQQ=:0Y/@Q@Y:@=?8EL[D<)V;Y$2KU+''LY=9-TA"\LD68S-Z0Y&1+53MM"SVM[!8ZFW#?>^#9S++RO0V[V(Q E-G#MZVKS_V*V?7*VL>:5+_N=Z9;/^*;=$34ULFE?698@-*LQBOV5T95-!_[;]>5N-0OH?^+3?7.],=;7_\N]*/C/K/ETPQ6//DM]?P#^#ZO>=\W3?#FN?;1$H><9LG^ZX3NP5U,YC(:7@R[)W/><<(I!G9?[)5M1=$:*B^=:%OO>-V;//V0_;7,X==08#S!((P=0MTZ08!/@M2;C(\=''N''N>.F+''_SF_3/*$<?<8#,_>\=__/R)49+XSL_&;(#@AMJ''BDTL5-&()_7,12?><)O??NYOO?+W+?;"09*^);KCC.Z!FI<1G_S833?Q(WNI@KZM6,\7GY!VZPP8 @Y8Z0^8\@((Z 0HZ 9X @ X [C6[APHZ8PGZ8<RY1FX O<@6F$G>HD[FHHKA8H#JHH^ZHJ%QXH''B@))54#O)#TB5A*/$XGPD@G==''<8^HL:&HL<.HL>6HM@>HMBFHQDNHQFVHQH>HN(T#U4!3VJ\@B/,BY?IGS:U8W8%%>A<G;D=''Z#E&*_Y&) :HU!>HU"VHY$^HY#NHB^M%".LPV+1 "''!UFMAGB9Y&(''VHI8^H]:&HH("HM92H]?:H];JH"9M!34L0V*L@ET!7#Q 5DWQ7NPFH&RNGODU17^]Q/5E4#N@T#6YXV^"C>'' T"G6G(+=8BS^H(I57*?!7/X\V6C)XV(FH.2"B+;QQ"EX$X_!3VPX(-*NH.>>H.R^B+O=#[$A +?]SXJW_NI2(!?#ZH]%0A3ED^LY4Q54S!95''"M6A!.!T!8EC].]E^MU0^N1RZN04FN4%!59)"N8["NU*^N;\"N:@"O%D^K+!X=(.@GG1A@3/XMK@!+9)"M@A&P@#&P#U^M!''D[?R!0BW@G(:BI2?"PR?\Y]''@IE7@I"!@AEI"Q3<YH6ZZQG/&QHA&RH#&RIE&RI[&C3#XXFB$X!9\*72A8D6"RL#&SME&SM''&SZV\*\! Y^H@@%A@7MX"K4LAHM5&TQ''&TRI&TR.&QA3A-,_Y/D?A/]%@BBQ@B/U@(DI&URX\51DA]W/&UXA%\0@T@/CVV3QVVZI&VZ.%Z6@T@2+VV?7@Y%7J9%&XI@MXUI0C@AP&%%W09ZK@1B(CU%''L9&HSI%'',"&L#5V&OIV(/YV8+9&J;U&M@U&YCYV)HI@I^Y&YWI&IF9C=0 &K35%(T9&*QY&,DU@-0 C6CP%:2Y[=OP^Z4Y&<,1VEO8L<%!O;!9&;))&;09M[49]K=I![89''K_9M%1CF;JY''L+)FH72ON9 GL,Y''V 3GL@)''\,YM-ZY''U1CO=TI&=+9''_$FF977Q%0SJE I''.#)TG84'')#U''.39''.8Y''?@9''?IY''?Q9''?ZY''?PYRI68'',@YJU-C!PAZ'' K:''0YZ(@!J(@MJ]@):(@6Z(@0JO>-CJ^''3''AJ:%>"Y(_?8= 6S,);1)ZGV6X$ *(29(R #.)206S8''.*J!IY:C1JK_RR$"B*MJM1$*J"$MY2"RTX:QPS?V)3VC=ZL=.*OFVXMD*#T>2*MFJ*QK**QA**S*(:QIV*QS"*QGN*QLV*UX:''E/UC=A.%CP,@6]$@+XT3T_R*M(^''>N$*[YNZM,V*M>U@I>X*PB2)5DQ:]7Z*^F(*_[FZA8.*]>2*]C**BB>*]=R*"A&*"H6*@CN 42H@>,TG!/N*''7P9:T*)1''^*%K=06"(@7:PE:Q0ZW+03U_Z"!_6""1(S:(^**$***-*):,F*);"SV.J*.Y=Z*#Z*.9&**7**.+V*.18ZO3L2''?#TDD%L@O&)*,<"U4[**,3**)>$PN]>@JAA(Y$ **?[B&#)J-)"FC..F-7^(Z!BJ.*$E11"D[$ F]-#EXA?B-9K*];=*#<U*C<0*.<F*"</(Z!#JC$7F-.-*,31*0@#.0ZU*+@]PX6NF\$7D@7H*\@#\[''_\M7A*1D?-PD%.^U-LXPV)8$+);@''^.#,F0B''*1;T&2EH.1@UR1?Z*2JX.2I(-Y"''HZ:"(6N$*0M''.3NC.)FO!=SD@6WVM"T"LY*(A=_+Q& .XNQ#,(#YF4;YN4%OH''IX@\G"L))5DB DI1#,F4VQ.5R(.4WM.4W;.5Q:.5R1.6-<E&&& GSGZ[@I.3?6;;-''B+%ZL:FQL@@M) (9LAC@BPB8)2F80@@M%P^J_AAH@+*YMA.HE[''(!+.M@@CG\PC8(2VJ4@.M&JM^80.V"@+2T@@J8 *Y>1.H)[.JF[.@<E.*T+.*]K.(^K.)O1-?E .N9@C/BPB:/V-V4[-;"[.;*;%W74I4"@CU''CMZ* CS70&(81@_J@CZ+!!$#P@4?K/L;+FMC;-GZ@/LB+F(*@AL)@/RUPBIR MX>2C/K BZ :/]H;@\7;/N [/XF2/.*[/.\K/>4+/9F!B-7P@;BQF-? B+$ $_R5.0@\0@I\&8:QO+W1-@HG)ADJ&0;[O 4,/Z_!0F@3JHQ''F8XQJO<F;B#TZWK&^[PO7LE]><FOD,DP;LD$#J:!6!!#8)@@>5&K,+DCGLL2GLL6^+:M(P"(1Z" T@HHKB"Q8,OKV:4V*!/N&W6$&!)SPCZN@YQ[@<SRJ<QNKCXQF,UQL<UBSLJ6@[..1+[$AZCE5%A\N,M"OLZ:::<%PJ3R>1$J;J?U6\Z >*ITH2#PZP_(DA''-D5<_&)-0SI3B6Y76H1/J(Y8T-98+Q9- _K-$''L"J#JY/=KP2.2''$.V*7D[1=CL NJ\P?\1*BH!._@SV54R!&%A0HCI.3DZDK;L\D",B$*!*[ *>[GL X.1,H^92S@Y6Y.,"8''L,0J!&BL U>T''"^+KG?5!\5L"*!1:D(_!HY''DKJ;.&?NA)@-TD[&:KJ''%F$CD)HK.(X.VF)$GKMJQ2!*A+MBO.%TSOIC3T)D-''M*TF,)R28)Z3K<A3OKC(Y+3DH3@LO_.@N 0@N;,CC +,Z5J3B2FD\''R\(EU+C9[%*70@@TYLZ:@@@NH2074@MNF2)S73QQD^]Q=,5%DH)[A0)& WK30$MBEA8CK7L3K&.*1E@%CHECN5*I \@3!''P<%3SM(6"/A\O>@@J]^@J1D@(!>DN>9CQ)1DA*=19''6GT>(.]#XDN<4B[2R3P%LKP-<DM$?GR1O0>?G&''Q</J-@F '':0[A106%JH5[<SP] @@53)X?7T<22D-\@_+C 3-C)?PFJ+04(!<47*=5?(''23R C)NAC(3@A;RA1JA:A58<JCHJ4-]\4D=L*? JCZRQ5)PQF7Q (.RI%Y*L,&<L53]*GD(L''@!,2P4]F[0!47J-3J\Q5TP''3U.C6 1=6!!U+[_L5;Y=6>E9F-$@CN9@C:''1-ANPC$4#3WLM@EM@C:$-5;6@@I<0B,A@CW\@A(HB@K50JVA@0$ @JH(@@L@@CQO B-@ 47CDA=P B<X-57H"5;H C0@ B8[WFGW AH7I2,)MT+K@A]+@C^PPZ80@VM+PQ''4 B/M@C)Q 6MU+6([GB=S@A6'' C"Z@CY=@/)QA\WO]FO=H8FYD@@B"LIVPJ=RM4P*TP@5>\@U2GR^RDPJ2\B&)8LN5#],.?.HQU\,L[P:Q0[TG@@6R0@TGH@N&\MV)L]\S(@4BA0CD,@;%$@7RTP;\(LK\X@+.4@-5?Q&9(P0F4@?D @B38@;D0@*)#P84,@+ 0@(LW\_T<B\@\@& $NG/SA)!,38XFM84L@U H@?D@@+&?Q%HH@!6(@:ELE"L0@;3$@4%$J*B0 &R4 ] @@) H@3P$@AD8@:+$@C)T=2FH]$N,I+M\@]W6P,4,MK3\@''!L@U"/-JOT$BL<@E8'']\0/.*,7"$@Y0^.L@F3E#V#R 6)(P!%? 4H\MTOAPBR0]7?#WDG683X 0T@40@5UC4Y<?@A2^@N''$@C2.@N/B@E;,@M*TDG >@N#D@MS3OW>2B8^E58*WDC%''.'',TDMHL\M=H@57T@)]Y@Z-C@(+J@O$_(($00N)T2-:LCO"CIX"%@H2$0G*\DM*D(D:^@N%G@IZ>C,,)@[]>@Y5AB571@B/>8YSQ@B+Y[Z1MG*GM?1 )Q3$8DD6]8DM@8F''/30I(45! @MC:5Y1W52/ 8M0P@@-=DD/!8L8]4>0_@M!-@ORM@O:L;P:;C2;-@M,<GP0]@D6"A@//:\S]<OSS@M!+@''U2@''-^0N8A@L-FB&>5@Z1 8NT7@HRQ@''#QDD]:4JY"45?:=)<5*SF0C@@7P I;''QAM00C?N09O4:A]Q A=* BOJ B-PPZGI=; >K6(!-F9<02D5/_Z+.<X;?8*MZA0%@Q*P1B)<JCS-@A5@3A]2PF?/088E"BMM CD6_7HM/F4/>''E5/F:\!C@K!C#(0B8!@F3S/C''^P;QOMQ:&=@=S@+PC@+XUW^JRAC#''''FLE@>9Q!C@GTCYFQB:XPB=O@C[UK@<J0A>90?A<+JJ.FAJWAQ4/N+_#.C/D.5>/\FM!@CLKPALHPC \_F:$OC:/VB$H>AY< $QVN54705(>???2OGLC070C!K!ZZW.8LX)LB+Q0/ 0@Z$''O''@M8TZG_\PXLFXO<JN& !7CV9@\BNGW\V7T511?DAMWW.<G1R!=I$JWU6K F@Y&\J@EC.GCX4"MD R& F7^40Z*]"TW^_CM)1"N;SBS-'';$2Y(,(U-GP_RG83""9(''5#PQ.U214.TN79<CG:#AP4\M7AL)?"9D<9]KW&WCHI2NBVP.#+M@M30N^G#''YEPK3I%Z%S2YL*UKU?F''E''3Y,:]OW<FGU+4ZMJ%SY=F''U+5Z-IE"7;P] ^I''8/.:$C#P06IN7O.-OVV9PS@J)NG^3)5Y<^I4Y<>??9M9XP\*EP@,/A6RMHVD"7\RA&$9LL\ !.HKY*K8@;1^/TG!"I&\:\7JYGI9XF2$<*VD33?<M2AR ^CJL!98S<_)""OCQOT&:HE@KHQ1X8(OHFG#5C\@X.ZZY2CCI PO''@''B0@&0L(][(*Z0)9OO$A P4+6.R"DC]6AL[KHVLL1Q1579KEGG7<DL$ !TXL& )3\X^D"W=2Y!<''I;D"O-?QHHX&M9M  RS4;6FLCLS/XPJ&<?:149;,2^YOL2!-HL%ML]:IPK4(6(BEEKOW.!L<]M+9A;D:#[+CRC"(M@-P J=&(42@3PQEK4C8OOQNT;:X@B2VTO-J)BRYI:"\(ZG(**"^T^#J(BQR!HV)HUU]%-UUWW8T5U%Z!ZZII 4*P[D%7_@F+*A,"HEOOM=5I34(3#P+6?<= >1R+-<''X\I[P(OH42-$/'':K,TVPI#P@1ZL#,L;4>I>.33>?L;L5NXZEA3MXY$4).L''U+*<45V^?EM5===>V7W1<YLV#A$;JDM2V#PDT3L+G,CX-]^(L"JSJBO8(W+O<(.9ST9A2><4[JH./)A(*X8&!Z55C"BKJ 8!6*YT*CZ^KR"B?R5JA9##R*9)T==-!_''7<FN&"!!<X12:K*NJ%%%U4+R%N!U(Y:YY6]+"5U(\QB"YNGJW(XZ)VM,%G''T.7P]DL[G=/989S,@/.=(B2^^CI[(=:Y:Z9;I#)//__&.>=UH;XW&"QO,-//VJDI)%PLK2(9U\L_!3122R_OT_=,V&-CZ (Q* DQL&<1@#544T\''/WSSS4\=]]UWY16Z]3G*QQ-BZ),FJ\)/13577W_/6)5[LNS''D0B$ PX]1MAY&O\^PT=\CP@4DPEM9Z^''/''++YW7Z(HWG,@X@@O#1L=%:10>\_OOKQ?=<=]M''_77768^_7!5\68DWHPC0XY"#:K:>_???A2A([,P.]F3ABF? PQ&\THYFMBHC&S!BI#J!#2MT4HHW3L@EMU#ACF8P 1;<H@ 9JLHQ$+BCH#0!BEO(0P\^HXNYVLLKS,DCO-1OB'':"3A<B.DL^=-A?6].DI''" A^_]S1O^80DO''*\II&+B@$5<H!N#:DP(U%FJU*S?8!V5&DT.S-FKVO3"E,OXQSBV\X,<DHHPM^DDS]1!D94P#":P<#-7:EBGO,Q#G/T8NZY<80D<4LPCXODNDJ2 G_U(Q3.$0PA)RLLC;U"DA<30RD%:(IK-,NP$LS''IR6ZR$)3T)B\7JT)R )JR''SP%I%EI2%U6,)V!WNT(3Y@CA*1 AYZ81S!&8@L@3B8(/$#OG_\8SFHV45=L(X\V@N&AQDS"GKB @ ;N<P9''THDJ4W G@YZ!3VU449/_AF\81S%N\)[S''N]DY3+KBPMG0JJ[[U!FL=>1ASIT0P^AMH"Q_NDKXQ+S''?<DZIATY ].@N@L;8 FKI21#G\,095T^@]B?1/*BH(:X!$V+^!EK4)Q#VXT(13=JDX7J%JO#+R#HCU)RD=JT)RV5JT-!R%KK8(C#H*CAN7DPRH$\@,G@D@_9,A[PHT:UJK"Z@ >D@HH%!FJ\9C GM?LI!&(8@ *MH@J2:!*U+E*UZ-.UZ-Y;R)X+2)V+8ZU*6L=Z5''Q>-V4$-V-[XT+V>U*UZ): 0R;6LUE/^FHZKC!GSMX(1[FU5SBE-Z0([EC@E@A@ABP(@X1LFP"D8&L] 2#GQR KFX)&5%$[OZ2''V4GY36+6]A>]+R EV5)RY-Z5))V-Z!5[V-WF5/Z3!Z3#0@AL.)QC1@081FQFD\@@N@D0X;+,L]E;&F?!/< ]1CB^=H(APAF(LX$H!D@R\Q.]+V;W^965;/_AV=81S-^<*HBE]%E!SVFL@P@KJD]W$!._NVKWEMAX1CC.I<KMEFFCOS7BO* X@Z08\HR]-C@ATX0 QU<8@T''&LDO]''BDF31!BEN80F.HH@T] @(!JLDI*F!GR*H5W1JW^I LR9(;G''CDN33@^NV9PU@M^8O0GMDC3#J."WV<X0@V;"R  HX:''*@I?Y''XW+M(+2:FMVH^M=''I/N/Y)R;WCF:TP3H2O*0L@H@OZX%E[D<F\9!3!39(;KN>VA:*LO>P)_K8@,5"!''N\IR^TM0-U&K:XV97%/F\>9<7M_=@3XWVEDU46=]''P!3;&%^-XY#]''#<RM7&^#DS5)R%_ZUR"6]JX5/V%N]=+S''0Y5*DT=Z%JW6-R''Q''V* 1XP@C,b')
%
classmethod: R4PReport
imageTableGifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 646;
			add: #Height -> 353;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceRGB
				number: 255
				bytes: (ByteArray fromASCII85String: '"9J^RJqUeR]JMr;eDfO''+s`\QSF#h5$k`tZ@V!F&gak3;Up@AWiq,oB</d!)''G_tdqXi"cguRJUS>
	NY5.O5-$''GMH"bgP!''6;1p1FR8Sr1cdbKg?b?0XLu-WPa\BN$3Z$>N!''b5h!4Oeg>)&[jLi@R,UPA
	''lKfK7/<lm1o^_?J>$N=&1djr0bi7ubZFl]rS90()Chde)f%_N+4S8LJbhD-JY-^;P7-s@6"9K!"8jj
	(#''TA3F''HSO\ip6dX''SML2bf\-t:/lUmN''''Ep,U+L5;HtIs//0!BeCrFMpA>%#$ksHsh!Y+$lKk
	=DXRu)Ugu@[BIY>Yjj;\pL]ZSa:_Sht=Uu_+=[''I''gk4J$6r%pT"XK&A/]#9i)XFuUmgtq*3-7kCYX
	AiM:1bLVl[_--cF>Pm<o`"d4hW1K8]F5Pi*$-X6%NH`H//0-F''FkQDmd0T>oE5@$KSG08k2[L/,]%(/
	1c@0i<`G<LPl1OQeDAu&Ecf76`73AnXKJoU*[DF0''U4WB<)-E:dbNpGjIE6WeCN,37SM=gm$+*;$l0i
	Dmdsaj>U1pY9MS:2k3T1X]R50U/2fUI5"#MX%#gLBjQ>f^L5\Mi6jsP6m-a5YcIBeP`2''9+m.U''fDK
	KE$A/#q-":>jU^:`7Xgmjt+Ci4(5cHIO\,ZIfLgudt1[^<-=XE8W-$j$QbJqQ6Q4P\rW*$QXb1,Q)<[+
	s[5<)Q\[P*Gn@o[MUMeC)k=o)(U24J]!YN/EQ&&ffutoHYV3S>*)i>$O`n9[r(gXKoKdH@3rfgak3Cr;
	-?@g>o''4[.NeY''G;*"*[D.(''MM8$6pa9m.P)PX*8K8t$jmC_(*)E''r9W_8r:9K-IY>qrjM]3j`59
	HgU87/Ho\AU$o_/2F5!4daN9&jV,UP(4VQB^pr7''Ta''G_B6/h@t`*;oO&m,mB)WjG/`9\eLkS=ZN-/
	i5[S6js8.Pa7j+k3QPk8-EN)~>'));
			add: #Length -> 38728;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: (ByteArray fromASCII85String: 'GhVMt:M:_>[P&V6dFDD9`SeeG.C@0f9g(\e(q^FSY&Y]\>jjC+(<q@F?JNLVZY`,<7b3@^l$c?h.j6=
	Nbd_]1I0EbOl;H&DhIj''8"6#4CJq+IM>HT-QR8b?a6_HZgd#>0eSJ)U''<O1_(s/@JJh=!h0.^Ma4R1
	YQ/IcjLs3(HQ?V#%:cT=me/.4O@45&MK0`Vtc(s*mo`dc#.GX`Aoo\m+!lrn$k9E6HDlDKY^LrHL<,&A
	W7FO;@Yqnt3X@`1&___^^.QP&2$b]D._YTC''@2q:S5[pB.kqaq+L@B,@iOE7K\[4/]mg_!P1kP(V^=6
	E&C4Y8U5,s/Z''1/53gO"9C*UHJ]5^&kZ.M<QSna?gSJDE)^Si$bn.TYM-5BBY/B3.q^g9n,%"ioEP.t
	h]g\'')gueNRK$2aYJ.]b[b6/h?LP,H6bdb,**m:6b5%FP7C"VC*17ucdbr?ZTl''b-Z^($nV"^E''7a
	jT8CR<n)1RDV-K,22M>E6"q>?B?W>?A&RW&aY(I-f,P9RgU8^YbJ5)%7C]/&MjI%n"+Zc=8T"$7.''Nh
	q]OW0XU-B;mN+_gb,GaTSX-a1W\dA0ghE71FXLce!/HQLEE`dO6J$oT7+gZ];C.D1t)p"L:>)_p2a$U$
	na=]9W&_ZAT[,p,fO[NDCd#%fEJ2FTspbNQQtr#fTZfuj:&lXK/U''r$_)^dr!8,`WJ49&BF%Z]6A#JK
	R<dC[)K2#Vrg=jD;U%IqdXLSOBoq"GCAcY%!CeLY"YrcP$([>k8<,6VSU$TL861Jr1qZC<b*quG:$(k9
	k;4J?f#<2]L)lR@=HPMmE.AT"pAHr$@I9NL)8*kY@n(Y.r7Kr''7]j,e<VTIQH*#].,s>FXo"AV^LrVN
	nQ>r"6CIAe9q@LbYcs,*%ILQ\,-UUO\"q*59A-`W(1@ec&!Mr]+=Y_+V_E-5nbg>=X"ti)Ib^S:mO;Sr
	uX5.>kWEV"A.i+_A/kXg''CmWRr%R3Up"5$.FD:>]Xat8eFLKH1a:&/:''o3BOjbsQ1aWjBsc[sXCeB&
	8VW8''*0-BRaS5Y\biEO>M<JrTUre>qR\)_I,FCF?;Ri0,k@lgsP2)4&EPAk;KtTR*(+g9#ag:''pCC''
	"!FJ&*]-_FS-l2''A3Q)f;+(Lpmeo)^bd6KU3h!P_gl@EMf$]Pg=atPRoQumPe.ndF^DWIkd19sQi&B,
	ahqEB4X]47=>PuD5_^lUql7PQ.b((S_q?C?bCc]!+6S(c^X;mneY6?9%edib)BbqWg5Z:aa$a;6^OB%<
	&EaaIl>t-%HM''rd^98rp3&jU,Pj:6et1=d/7f=au0&Bfc_3=BMBelWD.ia@^kYpBQ"nOTZ^<[b#i+9O
	@A_ELXGeFI:qQrtp1SKk)\Id>#0L.cjTMiOcM<-72aAJo[XYS7Q$&''=<M@;7GH)+?nA(,-NBY\-p-TK
	&UT]F4i[X*BOWr/.d_UIF>SG-t71eU-%P;p7m'';''^M`#EjR@n#;n^Yb01EcsY\EI0jF5/HQmQXDNok
	p&Q+"gi.1Ec!5)f''dI*>4O"aH[R@$FH<^Z]UWT8e%VS97nhW_X5a>#W8usgO:_g"n[^tCkeGc\7gU8N
	S%/XPf-1.;#<qF/p=aFFbF48WQ/OlM/S!XM4DlqDcgS)!74QQ4Q6R*71Q[^=_!a21HdX_@LEY>FGX_8^
	T11p:g<3-R^AtC#8BoL01e=c?J3\m-i/30TmGU]31H];OD#I8!lc5Eoca5giL;PTp1\*[^-414m!*I2>
	''d+A`Nlj17aLhGKPO"-=kitdH&=%]hd5'';Aq[gr#:7MmWuB!c2\ACRPWQmpVt19\.4\pdb^#\/T-ll
	L-qBD85''m:eV"RFBT"/t5];FqhA@3gR1lJd=3gEoCnKSdHmmo3.a[P/&kM:!!_sW$[2YiV9mS;>>X4b
	0N=P4B5J@+iWdmk)i<-WFWn7ZqU1jX@=jRDhp-e88A<9,JfSYeT@-62D8j-6:lV?k)kXSQH;$.9s##V$
	4WqCNiEX''1dPc2-#7eN=A]gRD*,kkR</?#_pl5[[DZ/pQHXQI1V&joL4e:9$<YCmf<%"Cs-Cqlf\''7
	,ME$YZm.S;7NMT%_T/#?)TE6=qTaFbe;>&[OY=/Q?:*cm%$0@r/W62$$`IVj''dRRM?j88,_D;+*sdlf
	qOg!abE)UL[[^G-Y(Q",-sGpKgt0[/6)loK0!0/Ss>PuYZlKN+1R(%*VI]_(@a;XBPBWoo?X:8LGpA^S
	LS=kgH_[UPOs;PlhMrHpgb5H`5;@(BZ81h.?+\!A19_<)U[ieS</Jt[n:[^?n][nk7I1O#!<Q4h3+">l
	45&ua!HUeD?j:^&GV6V*3,<2R;HQ-PEQGr;l:$;8t0&"PmUm0W/3)TZ$HiqA\."beRr7E0Qh"BnPO#6>
	k3rY90S;lB$"+Tn\hAlEs^!_I.K)=H+cQL$@gHYM\lC5cEM/p.;h*!+<;.TQ;RUB^r\dlOLn>NCTAg_4
	T`-6uXBM<u/qc?h''-E2AeEWg5h69t%>Q^;$BTKi"ek+KsKgk#?HFluUFfm[6q5<%3p/JeOMUn<U!Egs
	l\J[:MPlbY1$o7^(eEWS&O]bA9l+PF<%nd#O&8NL@bLs$6_K[:XH_%bJgkWY%]B'';*9$-M''jL1iiZq
	5^uIS''J!3E4fmijHj-Zj;!0X:WK^%?5K3S0l/ujoHVhneJ.Mf4=&a^;ca:Z."PdbYgr[%V29:)\%(T/
	nQ4$L5\f!T=$Wr4(:gq[h)/G-Yp.sP''MY\kHT3"RA;;fGG&!scFN1I6@IXk[:''bs+UJ0''2aJH>YCm
	`#<nQ)uS)l6P`fG/#o-4]Cd.qcFBkDVS;p@dUd97)n"QW6&lY%Gt?mpen+p%C8i)3fhK_m/Z67$@;1dE
	kHe?"e(B5n%1)SZ9UU5M$O''Q+>)6Y>1jbFN''jlcn8e!,V=P>`+&T2bGh[fGm49c:qC#AJ.Z:DnRf39
	]j4Vb,hD]1G%q*>pg3dtb;6rQY.I&.N-^QS!c.)cfA?^#tI;3;ZNbltG5_\F=`G-uu(al*nPDQ$8Pjc!
	YU--9oi5k`"Wl1o4o<0_WBtlks2Wk=&<`Iq$n:1[n%-fe6,[;gJ!ddQ2"5c1!*_<Z"3i`W^[\*If-um&
	`7mg97</5X@j"/fjo&4Qbe>3(tAs+tNNpD81WbD?/CW-H''jCLc+jJ4MWm?h]j[!(GG(N[m-GJ0/Ra</
	Vio_K*.FZ[l!^u)_&"Xu[JdZj#Y:^^eQhp@5CMYcb`!;ALP7p,jGDUNq(_8Kd`eDif_AR<:RZ8@n/.p9
	XWiLW-Qet&1:e-m1`n>9[Q8+Ig;eb2SZ>Ak/Ci?%fZ**32>S"e:gb)<[&4&Q8P7IKZtpNH#2"''DR)/8
	hi''NKOYF5[6T@9^bb1?n[-t1LS^*P5kL]TUMR0>;-+>ZM!AF/8;LB=`GL.W9o:M4*YdF"]>u=''sJNc
	oYh`:IrM%XSK^S5Y''ZrOc\?]01jH;''R''`0:L\.pj;NKJO(.b1mp&00/8!"*(X(\T!oX"7*q]ba,O9
	@gjJRWU1S>))N+'':N?IOr\\k[Q#>"Ytu`0\eiL`a0M7.C2qL]B5BGanR[ug9X(D!a2O-Z1:b2$h.(\r
	K\meMBn_rbb/5B)q(Ypmj5p)1.;;B5Tg1:%*GQ<PaCa2p&V>-@5&Ug''Ntm+=O(P83[5acl?3:pO0b6o
	j''OdqlV;W0i#Ji#fQTsd4ODr;H(_H&Xsd:IEH"?qG$$.u@<H]R`>jDc!`7lH5F(rDGm2o.X^]Y=@N\3
	HCk[a\mXk0%m)h>e&9%c&'',g]``lW%Y)k@kZ=[[,;AX1$=OUDt\h7q;t[/b!,<skRbeRrlPI&RF6a(R
	mK7@Q;*"a(.r@nppXB;Nhp]L17M:>,N0e3m8jMbUGXJ5J>J;pYVci=#YJU@qLF?;OMk<=Gm2fbdX6)j-
	9gn8>N^Z#tCKEt[)[Ibfi<\Pj''bVc:@2B+''/0VukmT/=C88i0u^OeWDSq$aQqeVV&C9?*^_l`-#sBY
	&cN@''<D<l=:(<,Uf\Jd0mu@b!%Q@6-`@k)IK2`U$6%\.7UZK3QgI+`*jY59fb#C[m,?<p4)&=;I5mXq
	W;?=k?3eW7kQ9\_VelXDU1?6O,ti99?I9D`+mY$X7F+H\HIqY4l2D$)W+@g<cnZ&hRif8@^%F0l0_2%2
	]R`]eR:O5(>[rH&2@''m@W?clqTCf-<4i9/4p;3G)G8dSdZ/E7oNUm>ml<l`>:XMuScr9QhJ\samJbk>
	EQ"HTlZa<kM_I"[&*b[&uMPAsq0Sc=<S/;+-7&R+2dNON5MpX?`C)u"Z&Ia;bR=jl.p3,O(%H9@NK''3
	$kr[C<1_>iSNr2"$JAjMc2Kdo8K[$3PJL>Q=9RAM&(Z''r;0G].m*iPS)i`YYg35(o9Z/a=u20&Kl^\@
	??>RI[TJou7dYc)Km>dW0d6iUqPUjCIH1piX(]gf@eB_Y36Kji+''OIokCE`p3Rf*;Z#)p2tUBf,n>u,
	jq-!Ag?5@BUOhgkU!q=ajV1PT;#hh3UDF`IQ_LkpK-4,`r>t)ZhkT&S<,n_"l?23;a2K''"]1H.h"N5L
	$gk$N*]G.n^_?sARh+Z\=*8D)mWisHrl3aUjsL#Nc0bMuq/-q]RY)\u<]?Z>mT/;ih+_2;ju08]:ENOp
	X./g@FB<lo2(-0oSsoWY5N:k(]5sIV_])R!Wt1fJAcKL613^LE]=QbIQ;K''Wh]#@l]3[!8S%0l02E9T
	5gC&!aEk2ZXjpC!A''Y0>F-="&u&otelHkuK''pNbt\Run''^5:;T2H/c@1$iQXS&cTguEmk(;_nRChs
	(26&:%V4@pHuG):Uhp&Ob?="s6Nh7Rp5o!\CokNj0A=54g,?64oIOQ*n<=;n&Y0?NHkY7"gpmG6b.0Wm
	=hIKFR=hj!"3r,]9-$2,N!Y/XsP&cJAq`#&]kY-NehRR>iGP41XggT+.K;A?$iOMCJK!E^H4kk+.kFbU
	P*]uW:*5D;A)?P>ZX!*R0"B=>t>51RV:6jC%0QTgC&!aEm@4\X:Vap4,.%4\3(IM#__/P>gl''b(Rtk+
	<%\0(ceS_[`tFh)E-WW9EUbHR5p0/KEIAE?FcBN5Od<(eMSr!lL%F-gdQP[tIbUJrGJSS+''cQ:#^\lI
	Cmok$9kmiG8-RZbU>;?In1q0a;O:,!f?(<,BBOXZnpij7F+t>Vt$FA%N''B$rAqP3"Noe7j`p$A@Y2eC
	+De&<;0\Vlisna"dK''_4>YD*Zs(&NIPKL#WY7Glj8!\Zm>P[<^hn83tWn&>H<15_6488&/GuqHBXmUg
	tSa!hKJq3poZn$@UgF=Zg^6qi0l4![<D,eAG$(W=gRnBL7<*F8j,0bgeYH?fX[*-70"l!0M.KA#;rRrX
	Aelqfc9M+1:$:(X1:4q>i)C<UUkZFM6MhB>s,FLAJLFY"TRGQF+X*E7jQ3AWo?1iUhO`=pS`5EH''P(!
	5XQleVG?:!?CS36oI!_^<gP[N&gfL-FsrfRM,d''IKBHN^,uV5J@$XYER:fio`?6;''uQ<JASOr:5bK<
	kXPL9))83k''IZ/@AZ60h+(inUggti11/E*HP5@0J5(BZjQVe6&ln0I!:q/t9H%Cs06V*WN`FtY#I9u&
	&HK8Y''87O3gV7eFU0mfTL7olYXteE]$o$[jCZ)MKRH*?Mfb"OJ\K''EKHkpYZt/kQr7*c9WII''J9Ib
	rKbI@[.b$#K$9T%OEk6s5QTk%jj33Gd5bS.8[s!3?AjM$817Y,''Q6iDcT1s;(#5B[qTKZX0''Or.dPf
	7$UECAATN!oljrEL"81PRlBE]SRr59sa3PRFq9W$tQoO*/h],8*(o92gSYEGh$h4H%C&)[jOGP!ng7X[
	af#G3+8r:r2&??"_WUetB(CIE(#3#!Oa@WKK$&>df`!XK2kmmP8X-o!+q9Q-a:;[JlrHsq,D*$F\Q@tR
	:gfC,`I6_qS2T$FY8km5@a]4m-//K,fJi^gqqedO7jaQE]EUQsR3i^''^*N1Vr^`k@pP[''Fn;X;hhE:
	O:W_\bQsf+ZmYci+5L9-Th7?qu"X*Z/&''fEus.5ic29_\sq$Q*QP2!h,oJf)OGpX]NR\$3"I;>KnT.p
	4I$&4:ojL`X*Q]A)a"bIEE?4GC@kD#FCH;Fpk:''FPK>B\*8)63+r:@''iFL<LLi2Ws!;/IAG4sOA?h?
	o8IeAO7j42jWjj>%Yr>XL"SZE+SFe"2Rn\23=@s9"OKQCX;m.,9XH<gDAn]A[K!#?B(/7CUi1Lc")%Jg
	A2+Xf(ZGRI1R#RT<>C",Y5J-JheFL%4k+,@+ZJ3s"3"^<)aif`/]iQ,1T*-V?KS)#U464J\^C+$`"X:W
	1IU\,F#aA:Ti3;Hsk7^uB$:Rn4Or*mi<''B5/?P)+l30^;u,4$ErS0AJR.T.`V@a5Z.uhgPRT(.C/,hG
	HtJ52mrmX0"5ZKS$rB-3f*UR?R\7&:uA9+2US/1QSj3q85(8gXJ]"U%"&"+r1B/+iiQ_aFu1/$a#Qth>
	RZ"Y:#1Q>@CY''Nq==%41TS+Ci5e3%KXO;!%@D6%:IaSpsS+l9K-1<s&8@EUKcVs``hfJ)uR]u^<\Au(
	*-i1Y[j7YM0R!n$R/+7hZ=Gn6,Jl\l7#483A[_"_Mf8u7^&T4],RI$4Qq''+]&Z30b(Wf,Yo-0O*S9#d
	KmHmA2\lHM9mdPIDOg&R+g9-YFa,V50.0%n0=IU[Qb3Sl7Q>>F:I1#BHYrpV<qr@qMh&S1L%r''BLm.K
	rJLpWY5Fc[Oo\mEX1K3UO1)O7RFO<hi:S,<1-3f$Sl)^WIKJ7DHT3fRhV''G+2m//Oe\Z]55?9@FtCX-
	6clc?R_IYs+U>\fO;$WRm8KpjDpZ-OGA2AkAriqAlm_E7W(&$R[TCC)^Z?5L^''EQ,U1iWVP+k/i;7%U
	,E/6#bE@p1AEI=M=%OQfdHOc@>EK?DOK0buA!c(*YPhD3CRcU1XbhMA(V-6;oOGYqMPn[**Gi&J_3&7E
	6LtZFep$c:kY="kUB=J#]''(Bp4fWLH_#>AhP_"kW/aiSH''4q"]#,>7^^&[M*J>\pRmWgc\Y.b4F''c
	XCEt^V<R?qFY''BqjDLL[;K?=uupcJl=0_1(t`%=+G%erBXc4&TID6,[S<DjPPJ!t3->sBV,T")<t[[m
	(q6I/iM''dj2AJ''fuKlD\`"9^33+kLgR,Ek]?bGG-<h&2X+IkO_?_bu?mo:nm,Jnh^SRa@7>U[BUOV&
	[qio(E/<5498L+$_gK38VO?A:UG"mopP&mE]cpf9(A*8A?Q@"T_!PLs7;4(X9/i*I&@Oda\V1XS<"\tN
	bSe]:9&YL,o"tP5Nj!H1D@mpXjC)^''gpl<Op''j_Mk5\]FCJK2:dfgML_iqZSuBSImNma^$uGo''SZL
	8:9l%/6<_"2;H_H:F9JmSTr"^D_jSpQI5VKk9N.hbV#nN_9%uX"*n^5*;cNs^NX95f/\4''903E@[:Q[
	]U-\^<;&e;M$C&CQ*sBI;`-0.Jr(Or:U;YTRL#DVujaS,hZSc''Uo^$X!@TV+s>?L6h&V8=UA-?+''J?
	]-,6;3+o3DP''">Ph<9WqTHEK%UM[3c_%Wb$a0OJPULZ"d1_pF?/B$K]K[B`_i]Hk4i/)E''b7rpG":a
	e2A"_&''&95S[)ntY5;%i[gcmXu%W7F-\]nIlG''np-]!,<.tq<Qn@RE$#?-GYml#`=Z&s,ce3+PG,u#
	T;Cc,N8-j."#m=3Tk&<SW7_FFC6;o0BNfD\;.KSR0OegQ&mbAA/!q:F>sX!a<mj21Z-]&+\r`D7I<[K''
	j/k&dH/gt"$AuPF/&VIOENmi]ep"9F)/J+6gb&Q(81t"`tq"CTsO_"IC3E=!&aj^`EBqjOZGTmaGPE0_
	`GAI-cT9Aq,P!>?1''>>c^VWR5Wo@_3!#fcNQ:j1^PL`eWlpsoWqMGT`F&,A4lX`8HWt@S;dqBE)QP`u
	Y7*m6*Z:=''"]ENt]^B%T;g6sI%t1+S7Nqk*\J52>Tb?[EH<l&WlC^4r`4ZLoRN(`Meip6b%7.NjE(kY
	^XjNY_\K,i0?Jt$)iUS_D8*mp&\M+2(:sklYbT.B9d#p%IpNkH_I1UF]B)j<1%S6lX*gC3o;*f\Y"Nc^
	"0j;$Z_I8L4BG-bCPUET(TXT!lE"i6ecQ&MR"R-IO%Kn`j-;^O^\O6Br''Z.95''Hu.Mj@&Z/HUB*r08
	^=*6t#58lXi>,ijD@?K[5/o)*4o&d">fW=Z=)Sr;ZVp.GO/uWu\qPjeM;KaN>ZUrO-XF#3mI&^Q+M-73
	$Nn\LSMGhhQr^Op&fCH([LAOmP:%YfE_Wl]du$B&l:#Vb(:o?@TW^kt/#NGjf2CA:OX?#e#q\5@&m>rh
	\kmPEW\DUSDQ$G4"MW0n)+8_k.pbcVO&fdmke7[A1-Xpcj/;"<W]uO,ir(mo%aFhbNmSa,?-l5&h\CA.
	MH''qB''T\7qYTak6pVOq:\=!gqR1WNT:!9[Co5A/n]ZWcH6+4Qh-7V+?&iV?0IQGS9206n(#-oE\b>Z
	^e#s!O\a^XUqkHQO&PX+Q:?;?qP&J5*htN;*H&IHfX''n(4o7sS[u4lHaRn$[hHtMUhqUfDFk6j>/d4F
	[pOTG8hQ''BKZ7''ZmBG10Y[(Q*9b9tU$$2Eb_7?CWG<=D9FhHK<[5#rXQ<!E([C)N$?n8^JqEGDS8N6
	1sq??bD''dP4347thb%.##O?8mEcDl@mk>]S^''%I8Ou60Br`9LG&69Kb#d/9]r:^Fu-emO!UU``g(j5
	XqsJL\E*sU6fBGB;$lM__5Uob?IT@2095K8HuY/D8rHio6L2)G<!n<r.T%Y-W2)YYa%iHP(P;''RTm#>
	L3[Q@"=L^G>$^E\r;/l#K"eSi\YB6sJ>ZuX=QYofL??,2$k[CMAN-7PBkYj8MEGA0`#+^WW@j/oYTEuu
	7Eb+B5(WtZ,.Vc]=%R,1MfR9Sriuf07^/re@Q15m_TibINUN0Ct5nt/uGou3Cl3h.We/#O]$;g((8\<_
	%-+k=^om.nY2ORNBB)eW3>uikcl3FV"-6".d;B8o\/f]R)2=6P9%Bku`fXZWMXV8h!''*!&dVgZQYQcc
	t:;X-IGG]K$4H)rA3''''Qokg]I)K(pu;1:^:-(4X''hI.:m=_OpO./DPHCaReKIf4o?P5Uo/%8V*(q>
	[+<Z@_A;n6F=OJ@c@"''b:pT6ndEb?9!u5#>_U>."]JCJl:/$NfZn,A+0(J$"M<WV_Kp?4.L@qiD''lV
	eH6^*j,oT]N+6?#"umS^L3Y7)*SW\ZYC+g/g+"2tRt9YL\TbHH=AeQV?D&ZBo/gs:H!9W:l&0"dJBHga
	O''NK`0hY<1-63%F5h:(8!$[$IGgenFWh6h\rjM]bEod3a"&3_E9S?&<OrdFF%[OU32N?HcY#`/\N^[*
	Ppi-%I`Qg;JAHC`r\.V9q=>rljYW3%M*OUn=#/Cc"tZ1e$G7nE/Ep`1(c4gq?3Ybb^\9GmmfEEcon[8C
	sAGEFKU;Eci!J>^&NBN]d70$Z4<AMhS<W02B0TY-t>0p,A&Vc/Q6O`qUhQV''<!2:E5rZR<B5\^^a6c_
	?:F>oH049)p/e%74Frig+Ra:N(Bo)^R/+rph&hQ?QahT@kG686667M1*SJe]h(uG4(JPTXLC?g6A[gXM
	<eO;4g-!3=,^Sb4A#C%Kb$`iPU\o8_V#N(G>ba<a>rQn(hkOkQDqM+&G5c=''l6*1AMbMKVEE+/HI@_6
	2pFQ1GPu!O<_forrq3i:gFEQI=tu2JJ@-(3''GePd,hm4Do!c0j%XE_:Jo[_)!d8daXCW&u65UH>o?Q7
	b)D85bS^sTJ+d27H"O;!3!<*!E#bVIdHEn''3#ZsVU,ON_u.0[8$7+RorLr.t/1="Y#OHU.gdMqo]SG6
	HQrtn?R<47l]Q$^q7a4keKa>P[4cG]FST?o?QmNTbG":s%GYRRl_Mr$AT3''6qd/:g''ZK&&HF"Z+I9c
	:g0AP*,@]r5tMF+;6inA4dcTh?B%mI>m+t''TO]`4F4sBd[Y;T2.!Ei$ak.51@(''$H_L+:=XRQf<63R
	G0Qr1Qc/''eV$_U7nfX!n>%skPL*FUpfaA=$dp6>N*7?''MdPJP]!bEUQ9H$a:fA6:,K&I,9J$f6oHe^
	99h-taWmgl>O*Pg?6oPRB>`g''?M7douDSYQqZ3+/)@qba=PV`c))`=>a1d9^W8ae=7"smO+2?g%qCV/
	@;H1IZn1`1;nTJgF8d3s#Et-2_)s#"%a>h[0LQL!#G,4W9/c/8nY]3*t_nRgu,A:`g-C!XIn8*\h`pGC
	:@EP!ZW*4fr>$);`sp>>6Ur<,FU''RK8!h?VfbohKLY>(>*=]@)R$K4H_>*c7G_eEkm\EV&b2jb??]j,
	OUrI_@(i$:j%hri7eII0Ntrd8eY:KMAT.@KOp/.HShu:"87p?t``1f*E_cjT%j*fN^k#Blm4N\kq9]QF
	S,T-nJW8''P/Qr)[73ElH>V''^Ld]X&eK4%Iurh6mM"+4GX;=UTjf,sBjOZ^T/)jV^EFlLo0J6[n43+[
	2BpT.VUr:)F(FM:u\%S9j"jeW<"O%c`BKh&Z0.Kh5c`ZSM`G6Q''YEZH9U[,@Mj<E!qR<(J(*VCRNSjL
	5''IfEI6[=P6TPL3(2B$dKJe:Pe,RP:<"!.RG0;RQ=GpiYdb>=d/fI$OoT1il_E>:7[D5r''I/''CNE3
	!j2`7O&:_.b4/GPV*!6Y?0[4$dn.@?1.Icl(i9a%V/%Dpo>&2gke`t"q9!1C2>9:cY,e&T''_?%f5[<]
	+q/r1822Ng:1I1E(&G]l&>s+46%,8EtDO+i<5^Ke$u-Fpbh7l/IV`Q&IA*gDC(pVG.cL`G*hKch!HX7_
	;$$T05*)'')jq-f&fO=c&mU)GZ&]Z#Q*^r)K#U#AYH"pP:<aA01n86nU39L58nY`?<t9:!D*GF`m^cVc
	2&e[''0uN$]?([$hpg9+;_nSb;=+mM(<r).S;:L@-/:1,*SUGRuSK!J!XUf"^UOkq-Wk[J`C\6Y7;TUB
	CTM*ogRQ^=P0$3f''D<c`*Q!Z\s(WYI7>hW`#H<mqrIl2X)`*7r:2=pLQW@9m#d=pP7nMASdA/,d^%r>
	jEJ`+8b?@toAhMo1U![mJa1</5Bq:&C[D8hWH^O"%WIG40"%PaT]^o?=FLWJeoK`F&R(R5;OAdYPLu)c
	OnXY^oua$tjr*Kuq/\9seBmn)d,8T],G2D]\e?>XA$lNgHCZ$uJ6l#Q0l#i8V+fu:9ea7oRR9i5"7cl6
	)=406Tu=UK%]aEgQ-fX+A:lupl>Z2&aAQa]0_I,+''f>s!`_Q[OMMU.7/,7"/SFBY]ZrgqGN&F#HBVna
	9k)nVCbG:$=.u<^KCnkq-W;%0DmDSeL6uJ0D-]OI.FrHdrZ`Q%aaB1I[ZS(lAbJ/mL=nE,/3>HPXojqI
	Pe-R8DjFB8qmaiSBoE$U.kq+@EAL9"f(lHhLki_b$XOTXZ]&+ZTZ;+?<PsKnsar%\[fUcD.83R/-jEC4
	F@Au72re8Rra<3WJ/Au,,.qJ$(,t!lhR\b8%YmHdh3TZhS>A-9^,>j7`@<?$k4iT&G5m=''\bH`*%''g
	bL5E>&4,io5<jh;ou^hJO9''X+!GH\.J(FZ@YOnkUE=YEklejmrI,9enZffCP7-"D;n`JD7.ZMcoc=eZ
	Ra!]A4$g3f#<c>ELnlr^f3@IgnQJQ=%E_N#o,nP1L8\q6Ztrk*#K=*qc;@<pRbhU<tJMs!Rj!"HC:<s8
	Gfe=7K@q*c6INYRuFOdccJAXLT5^XAnJG7e<V$&Ob_^u\BD1jVQ!hAI3*g1jhacbAKm71#B:!V4pH$!F
	")?(!o9<U[TYUib(*K<97%W21DAk3Pf"\Q+YJEg>9Q)Z=HG2.iQ6!W<VtheVFnAc/Tdd*H\1r88"#^t?
	L/sX>"d*E)V[BXCinb3BZ7ASI;1HbACROu2[<@oEO=UolFbEa;5lYdh1G5V-/Q3PMJJ[dU?X>DQTH(5k
	$>kiNfWo&k6G==\m4!L[!&0"OQcn''N1T0dHQII-1QP532ZnB:?6a"[kibei?_''l_F>d''p:St$c\it
	USfj?fb7klX-&hPkgZF58.Dn\7M@$s2]RpLGhZ;7&\cDpVuH`]^Xn*dAQBE''m?52.N#^VCZ+KS1a^g`
	o>(gu#kW^[Abc:A4F[f1u$d?Z5/Oite''#YQRus0%&fPX?.jM!_hd>ETSNglSHSk>g`-IP*>S#a`b$VS
	i(DV9`/ktg;,,5XkcX?C\OjfS\HNcmo>(mO\XJNUBW*R#2YlF#Oh^lD[''tCKn+l,)#!`Y2@gjoH<!(d
	Ug&sCZ[T#YCs4`MR"5>9#6(#rkJP=\Vrc5#ba<a][jJVPBb^6$0(,Pd$.XQP>`7lEOo(/C>_bLFs,lp2
	,j6f8Y4VugEH@<&bdD6Pd%[W/qU5=&:29m6!X?Qh*#\D>i04^s:=(O&T#l0hj;@E3F:2R]q^130f;j8M
	S)`k`Ks=$61Dk''VjKIB^n1Urr+CfGm\m**$4iJd,:stDqQ8Zkt!WtrdK&Z+SEP*s\pVq3I2glgg#f6!
	Q;l<PROZ2>]5u%9o<)''6W#B.gFq..HE*A];Lm8`fCAL$XYI5DJn"7oeAU5OMt+iP)^5h-2gMknXjq]i
	#L-KHMng]U5+6&@TDFS.B\T>j3>/junV;6^pP/7diL+X5>-_Scp7QhM)_D"pLB,CesN9KM4^7>%:L>9%
	(U4,m`:QqTp:FK4XfNc+u&lJU=YZSpGPp8FaoPBP&YHQ@.!R\sFPZM%n/@FsP,G&..VdC4(G=[Y+pe38
	j`PU6eR(VH/.@$XA)!`+ReG$Xh_:Q.+FRrI^tarXBM4#i8prL-7j.%8R]YSWUHQ6ERo-OKV;X,H#uMrT
	`=bJ.@i?A./0pi:jj=]o!9]Y(ni\ibFen^i]ZrO_%#^9A*Yjd0%%?XHtT?bVJAK(9C''dUt5&j$iRai$
	t>lkN.%09?<6HqIUTL(*<$KY*lt"CF:j-erdO+*NmR)R!GK=j)]W@-3e_HGDP"Mlh.)eqSITG''8E72a
	AK!\>eu5\_'',X"0oqht3AP/\Ed-qlFa%?i7Wl2OH=LH/j2M;9or7<;CNnfO];sKIYHO.tb>22`s0(8D
	5N<2ko^/gM;D3FPh5FAT2]aSWs6$E]m`(;hksjAV4<&dQO)GH8p,DK<QZqUn''LbBKSi[g=\Zeb+h_*,
	T''cbLs2Q0M3]:_i/4_cR^@TS.aW<.7jV9BG.rNJiq8aA,-H;HL,"o;0?4:>+E#\*-k$LH$Wm2G\Dj<4
	,8V*RO*%''"pI26?h]B!+_q-)&L1HVn4f/<fI5hPP1c(\PY!fqijg_C70.dG)9-d4(!ZiI1rp2M5TQ@s
	f#iD''=&D)uA>c2W%(9X.(X&@oY/#!?G!.LG8ZXcEgTPJCM["fXOse?t%W:WY@Ul5Gq_"<K4jU]S5HA2
	9W?>J96Jc.Eb"2lr[rgKm(D]R-''iR8hn8^J0Inaq)pF818&R0JcP<fLX*>M?o9q<4Y=p9?eLU"lEBhH
	U5Q,''41mqiiUS-,:^j=hLV@>cWu[Kq+lA.=6W+-8T#$=@5rj`''<C)0IOH<bX>aT\#+JjbM0af2bl''
	9^J6SB3!S1n1j''PMgu&0ic$Yu@NpYmn<MR*2k#>c^OI*ZRD_JSOV5Y0=4tFC&S@I.Leb1:856&n/SMm
	mAuJP\>SSd31.OHP9@.7Oq/-D,''-8^t]E@,9Re;*LRhK9NBoN7ufg,,&F7U_N1FCDc1gM_S>U"pfF#e
	%eg??ouinq:]AnT$<=b(%t29E-:6mHX8A$JAN$pg.*.*n[2.nn''ft6`.+;sKE:/\R@C?(IqVtL08gWf
	=V)X:/:9TmO+=F*oGG8HSd#BMAk"h"$5MEmEVnKim^;#`.En67)lOJ;hZG3kuni>jjJ^=1XmQ^:;CNL9
	IX*J(@IK$/C9:u71rTrDXo:BR9H+\W@Ds_(bmH`*5goj#em4V5kjo*D=s)!8Z\%UM]`''jM"CF;W[a+R
	96TV2#?l&bdGgN=lI/-I^Z"9I2rkk$om*JP<k+t=?_<O_\O^dOj4%FNMI''_c+pc(Vq[A$/SCWEgCs?<
	m=OhE/j*`(fkn!&!F?j&5J4Qg,9/E.>kfdMIlVU+4D)''P/&1L0PdRH"<Zq.tqtXI$:[m-PbkD6kn!H%
	Rml5e''g?_e0EB$i[939S)hWDh+M!lM/3r1N5l8YhrS9%L.MA8TV,YOKDn,ha;ip=F,+i_nsqUS>_ZK''
	(FF7b3)"I":MunJ*)AoF<3&1F&iWCn4F1@UnGRlIHPE/H,Ia)dNFV1+DUsXP?9Ia<HCP:34&;c^J!]QH
	2;e+ooFM2r[amAMa,F)p&nb%0g=LZ''f:A;>'':cdP.S;''lg`cd^%ZheQf=Br,"JCnW7!nL9g,IA<(4
	Y,3CG)E(&)[#)R^LoJlf)559csq2EEm_3_6g%qqjoFdDRTL7+8"\1)*^Y+,<0-VnLB)JraIR_&Qj6DD/
	/s<nb<>`+!mTl`''MG#DRt;E_.NhdTq>_U?sg68dAq)`QpE^Km)XEgRpbo`MdD=7X7i@!]&4hW[MjRlh
	lgINkZpEd@qo2j9n4LG-\2WZo[H?O]fte&C91s5_g(.3d.1"1''_oU>1-rXE?EBoFidtuk''00$Vpq9<
	LD-[&\5Tesgqc4Qo[O3`rARttfnQ_Ve8dYX[7eA!RGjd,!Uj7#Y(c7sU@Si2>J-Bu7@s2Rsn1^1Q+jC8
	1MoLE@J.e#u7(eV?4eEc#$3WfKD.js,NHir?*7W:s''RmR=HCX7(C!,[uU;:Z%^u''Y.<*b_mIGQ'')m
	d\(I8&1h9#uX+11b)ur$N>aW?#aW-ClDQmgZ)$71?$*,08cDSqu##IijtC0(Bd4"2fM@87mCm4.jXrn1
	+t?5MJJR@?!MA2[ms0;''d9^]rV5NhSU>`Xc*#?E#K1fndT/%?EGB<mpbY4@Ba,,:X5Okh3re1b.f*ZL
	f=_(F_7qk%@RBL=khm^]P$T;mTuMcC!\HNT[#c-8(KW/ZTPass^+[q)_YZZS^$"cp*$Z1Vo,&S1FQd5F
	>5uGr:6!4Zdj>]gh7OE3GA<eICee5N>\cL;I6[YMlR+V8rd6d>;6R3#)en*r[(4i>lHb/6-lp_9ei%\m
	9F,E!)Ib*WK8*5A3NdH0=?&/VnI''3J''=8XS=GVf"3>lo(cHSB?+A1pCIhrIkr9fQf:E"Fqr$AFo;rt
	Y>A>h3ef3EpiK;#/iiEe2PG&HaV>cNjY!OtulLDUBE%M>0PR9h=>2A5W&G4$W"7P<2M_E9&Rm<CCco>Y
	+R''@Ea14Ef=P.S"^<j6-)3oEG\BmOgeCQfrtR-lcQpXi?6!6MuH,T>L+8nm]H46A9j)6HbLh:2*qQpn
	&Iar5^dtYf9#9XBS)!&r"NL1BjF9LEt8k$%LW1it#4Tn\pp:mS,odHbts=]n#l-0)rVWEo`.t6E?tN[@
	Ud?''>7LTk9:gR.-sDiXM3i<Z<\B&geV(eP:jD-gYq3^i@D1G=N$9t;-9pDeGNK:Xp*EllUQPb,jGgNC
	''/%c6QeT%a;j^X2Hq-2(nYT2\CJ:YXsQI+2nVt3p8VD\irB<[7jpU-Pm`S;`?Xf)@P#(#Fp#XFhVE,p
	Z+A1%D*Rm6NumcF9=,GC2``]4Xna5f#,<#[ZOj9d_,tBq^IX?JO*37&8`EoHc26F(`nUe"Kl5Lj;_1=0
	9d`]\b4+T1bNsq[''hB1[iGKeg*ehHIlf:f\EdCXMP)ZM2Z05JsprQ%\>dGR^g+gA"\gp*''8J+B-0,N
	UhSN8H:&r[7_O\hi-o4''Gi;aVu-bmaLMTJ4ogN7$APFTsV5(3sHM>*msB>\L4[6A1uJ;8]Nn3T^c0I/
	^%-M$IL<AUduFTkG/[`L]=18tbB5(&?61Os_0%7H@c3(tqlHK[7&!>FgP`n%LT&U%M>SRc7LV]`9n0Lr
	*=cmc4NL_pIR.@AS,s(Et[rBUa7o8/tsoNC$Lh7#2&^j`2^U3=m>eL<eGlcdSdHY2JT%i:R60\2/<uh@
	-B-Ts*Kg@^XRNiS?d<KpBSPKVjLF*FsMUV_AJ0`+tgoKu''Ik(G:_9r5)>Y(Lk^c09ZtHbZ,5i.h/ip3
	h4fs&?l2-aAiJdV$EBO:7_.hD&Pmr$*@^f"WIk%.A@>jr>%''_p#OpP/P=A!/efZ#@1j#O^JB8mJ.]uu
	XGL[TgcdS4Vaej`m>bEgq/1;ta0<Fn!43#`H0?*Sj.]''o2^998eENBp!XY>d`acf,ICICcW0J/Xl;mo
	V2uWKM0qOU0Ccpc3G[fjTrZ&c:C9KpNs"al2loNPlcdse,3%=92k:S\TkOkt/DA^qr"BT=\''rF%_.*.
	)g>11-HR9XFV8dH5YYDE+W8KEF@o@/m8%2]"<T78srG)ZSFa8c2t01%K1-XB]5rr:5!#,.Ug2$**hC`_
	*RSW1C>2M$bgnFmDFd>@Vu,mgo%U<)Bu`I''&eC9:u%N0:m$q"kA1ASu#N6$7''hj$n08DRXrl`l;V5%
	R/3=X1Ok5F)r:i(g3sgA[AuF&''g@Mk\K`L>^`c$_.D*0T&4PC<UY+jcL!fFrZ:adh4;08:m^Oc<U`<W
	/%`DqHX&$&<2c@S96pe\G#\#W"%t7XEZoYYO3!GieiM26Md0I5d&$]jj&O:Z+8L?U/Gd>7)QqLM)p#=#
	5KF#OpUEG5]p^d<aPN?(^>9BEJk,NR@3ND@l87Y2MYF@j?>BS4f0)q,e6!aeE&@,1QV`:<p-OOic8oYk
	@q9!g>^62*o()IsM^I"-Hl?A+r:''8?pj[5)jm1uCGCduM:HW[di?EaDnFH/Cj^ksLoukPJIge0Hq!6+
	"NJI!AmsfmhYATQk&3Up+,afLaIKOrUo"ES/kmt!C<j/^@?6)#s1*Ifu0\=E&q"QMCMpR+''!3!ImDuA
	S6FjlW<3^#"??-lsUBs)\W2p7SJOg.c>?YFQBDL8''!3A]O)c@]c9"e+5)=^Kf''V*m/.D''c74Nd]8K
	P(^JAs$\YNTtX7Mj>/@Ar5+(Y`lM]SD0)7n@)^lo-G=Ep9YE0>5e/s:YK\2hk/k3(LhLE3i<N</iid%l
	'';]%"O#6gjZPMN;U>r@M1P5qL5%^[/SdrH;p#_`-23D51Zqg>+)J?[ndq,VYK0eBQ''YZdtCt(992Gb
	_V[`N9$T0Hn>l^NtUUGCo]-cs$#J2GreZXWK^^KIBHZL36lpS\IKUf-jORC#c??3VrS]j-]?JTuE!r:\
	!+oX2+gZHb_4B<%-.,8g9_4#JaXB)G8i,3-_X;HS?5epJ:Y]]@Q6fh`eXGmJbcd_1=9XEnm@++9Qa]:n
	t$]3.PJa_e\-;jCiS-F0`AL''5+mKQ)%?G6^.NP''<S*cpH>uWl(@@pj\K%>F&j?BmN`qIJ7=AWV01YD
	hbAP*=[@hc&Z]'':''(Qpa9?_Z)EI)%$EC\dqn<465,.hScdB6ak)rm]Lg2h-DX``_c2C`L3:OY7a^$D
	sin[$ZMhpeJccYlrqp594p%$Yi!SXf%aCP"j+CcSnVM/B$ab;cKpSb/TZ._e7?g,+^\3j"h&I:mGrRl;
	[GuHL3N6\A:;N[PNUKmh`#UbQMN^omRp+2LM2MU,Nb:3KO$3ieK(8QlLQYpp4nt^>6BBa#ZbTe<9YXGI
	P_/XkJq)*''pX`sN\$=9cl@Nk_5[N%]Q3kYF+;YF)6Q''NY5VI$Fte=aunC$=YFXX!.WjJLIiq&"<g]]
	lSk7=SKZMFT#b3KaChYk8J9$fiZPL*Z<r_8jD,`WuOq7q?H=6+@?!=AVQ]">PT?Wc*dO0c+o5lerlQ):
	^]ioA0e!&6R)+2c%iTH.[<"2:1s^2II[<Z94Ws?2sKQSl_![,:*HO:FOZG7.D_4=R=+_RD!QSFRn@$3\
	nu=29;Dq+#VU(%AiXT0(^9"X;)T,?8&Q[rLQ+]&%lO!!L5&J\XY=V.IlbO,C/7CX3Ir9<_<Ts)?b0CQ0
	eg(L&Bi7fF-!s6?%8KID0NrO@eYfep3L!6AjP7I*&]7REEtWJ)m-[7a+-TMI@pl\O?.[46t''AP"d/;1
	>:TZbn72sZ7HtIe<gg5;*8>KZB8`iXJl;T)\BP%;+.7.&#mqGp>&Nc[c#)&W<i@m''3_9\>r[]fJg3JJ
	QBZ-''?!6g.6.*B7p^bFY.MT_e*/oH:B3IXK8e[?Rgg9c5bhu1hN]3A(a00\QhK9q+^h=kc9#`u>`;3q
	c)CO''W%3D[Se7$3a`*UM?/C\Hth.SsH64@o729+nc$HUD15n=`@^qt,oGHA(;I3/@Z(K"_A:dZ=o/o%
	\QR1,DT`[Rr%6\U.C75D''a-&DeD8([%i7;<O,IIu(WJ4`^''T*g,oUT`uYqV[iX>$GWI''ZXs8rt/h`
	8p"iZ+0_u,`\qZee6-K]bo47Rp+JPX.WWXAN\hgWB;c@\2k>`l6fh=M.9Sl)I_o8OP7(q*Y`O@sE^<62
	L+CmJ,6A:<2gDtjY13K1e\j_X0_@Ul8]DCJ669ioDH-\''[42''Hr;orT&a5&Bl>o:(d)RiZ6>4</YBp
	W]o6?h.gg_fV>,:Dr@!Dk`%fohe7Rpc)M=nr,%>V`];_%^Fo;c8++9`h%+djI",ZPS*Vk*1J:7536e9"
	6?WNaM*OUBa43e^*'':NDP)(tS+aHs&[;+b.sJZn/F9HBachT25YWmp%@7B$Odm&F:ja5LcSo_"T\t''
	oshkrQn:N$_O.DO7r@pgWWHiOg#YUH":O<,9o_X9a;''"VlD-Wh>%d>jaEsWH7s?Fe[p[9!]`TNZ-`6$
	2:,=P>lc.^>\?[g\nHVf)F!_C@Ng#Uo66q=JlZY8a"D$N_$[-R23O!=QflcD=(nR)j[/qGY9Geah#W''
	>h*GG%8PdoJ^Z7^k04\\fdjuGt\CTn,Ef$+K)SEBrRE67UL5")"aD7,J2>KqFM)%8H?B+%JE9-(AD-g%
	>p4''K]:f27])ouo(gXuROUpi3XmA)Wnr0c),Z/XS/nu&P7JWq)!aoQq7[''gIPSN*5d2fMo.8qhf[b@
	p"+Gt&N1@@B\ZdS"3CRU#''f`_o_b\*7S-7lEE6?5Yt=FCb,g*?"W^it7\c.0/%$QH7m."aX!?="sYb:
	WUm+QNRg"4ii<318](l-Xf-Gs-rd_/_/DMq5pX\qA-UsaGp\B[=iJd?$ARe"S+4p\901%ls6Y?[3U>A:
	u;R@T-Ji^3H>?4WJ\ANbh5?Epnn&UPK1I84PB"\CIT*U2Y2HP;iI7WJ,I0DNZ8mrX?FVMaG$pM2HuD\o
	"^5pWEu:",C:chpgb9aoeXF@d=TP$J&h#@a7''doX3?e''T;jIe,oNXdfHBc<5BsQO[<@/X:ct<r+<8j
	40"%SBT]^o?=I*lf$T!g%i%>DdjCAnV$@Cjik"<>X.("`7hO?_=(@$2+p6O9(7USCM0''f=M1feBTL#;
	Cn]4''RAC3B>2!"ek*(:V@VEQ*:cLH''M+\ZMEt_#)3T/)$\NjTj3cRPql&cV-UC3O5HfR1Cg#*K\l2(
	AbI*.&Xc9i)''D7EX2X%"DEhu\0tkK1;f;Xl5J0-5uesRRur_<.()j1Si[gDFXQQS)OhnVe@6nCRV>Ne
	7kRHS3rU3J?QITkTe&F2!npZeO>T8=\=CJflRNmOQEm*iL%i66>tJa5%]X;`mNgcZ<P_bLS[*C)JHk;\
	g=ZadKI3\im=jk>C02\;1YM;Rb:Hl])1cYR=oWe/O^((cWQ@_np=lL?Z5u%!0@X$<2ZYW1K''^5qpuW#
	''>!LU[l3W''l>c6Ko1J>u._cr^t^B[#Tp_T+*-Hs=Pq;_:\osWT@P6S0shB<)PW$XFHDl^4ibC6mrCc
	HGLGpG*8aMDQB$W@R;RURGE+ea&>Z;s^0"u(s^kZqS!F"_K(_ei)-+ueR&F_%@iZ;V;QV1+4=$`+g_--
	D_6?!Pks70hmlV?75TCBgIERorP*c8fKU*g(SMdCM^];GYrs8-M!]%HJB3cJFL$VZ)9K6)*AmbB43ESK
	(Nc'')"YX`cAI.s+#jijK<Ve1>USa9:b\F"@Ih>SqE%c8:Q=8/<kLr/>?b^^HZ_-WO*JuAJcDN##9g=e
	F''6o?TDIg*Q!d(RUl@pZkg>\S-#oNGU6&P4<edI.I4i;Zs/H(hVNnXQ''G,T5$9jI1$+UaQ-#;FPtma
	#J$45f3V_YK^"qA?m5H5E5=nC]iVPR;q7XN6aoPQ%PEl-6aT]?36FHJ;Wip>''Y<QqG:ZC3(&sLrYDeh
	;]o3g[Dmlu)(,JV(m-p+iU4*U+o9Obuc$2XU.dN7U9L%b8@hKO&Vk?i+7]LD-bdl+I9%nFHB#Sff<`C>
	m*!&AclaH9X"3Ih9lHfVVSr??0-8SUVURau8"J+.Uo^>%@](KtA/I2]jjp?q]QjPM38_V$kpdHb.ohrb
	h#6t>1a8PdTk`-UJa/k''Z%YY`C5?%0<Tm$ktjgIjUi905f9^4Upt%6d.an+t4gGi?*@aVW$(VnTEXn+
	OGkoHrgVs12]X\$OP9gV(Mj*]3`e?Z5tfdXpL3/:jn;GjjM^SYb(T&"(M8m6B>B/@QF^0f>)CB+`j<IH
	U''BmOeD1[V;])a/*]nJ''U$LG;]D4kOpjDmle<a2tP*5%^rc=aP)D:+2?1T$DFbVXEZ#MNZ78VI9ahe
	(&a$qPD/ejM/XDE4%aB*rL<ci-[s2^%Rp:!F9^)*-DPst!1IW/0+:L+O`sK2a$D&S_Y>bVj<j05>1>3"
	d:(''^RHAs5)`QA2@U\Ks!opSB1q9aDf75PO$Fb8Nhi&JjkI3-Ud]er])WNmGL%"g)Hc.p&R@[\''`m#
	NC,Wq$q<Yq-kejrtd,ilg@''/^8q_Us)h@(JeYjH7et0)Jj4Afm%j+H4l3:K&ML8\t0u3n3m#FTaW`o6
	?M^d2I6D:gW398Y?.e,/B.f>^0r,KtrhS`.WpLM;`41l^>Das#DZ:ERhnT,(dEclX:t,00G,(g1K6Uak
	]"<aTSrG^.VF%Zgfk+*,9dd%(lU[F5K=i9uK@d_tHX^44/-+1gdDI<oQ2#HNFa)3-T5jC+\K<m9M*V^=
	c)\eDIn:pcCC%7j\mdic>4Lr5U=R(*,%<F.5W[KD*)qXp%ZsKflq,6(0?/;D]$V$XlE*>VF2;#0jGCKS
	dO57;=ljeO7S"CeUuR0@j+aH1+\6@;*NUjJ]r>`%8&P3?7TPGUA_,_h<tH[VsdoCsI?"3V(8KCVh/?,)
	!O$GU?rB)7%Z[j&P"6-D9&AodJn;UYf7cO9XU]lK=<b''eh+YAfZCC9`6Bm<McMe6:.9ki%U8"bif5F:
	(L..o6DGn?,+%$$YS7fF10r9@?Qr;Q!(Q7Cf''$*(:\dciZM,iKrj<A36ZF%cV.klbVc))TVf/:a\5cE
	PVUsb@V0Ho<BteKkj?RuVht]Pd9U/-cZ2ZN<f*Z3.uZ@Gi)!`rS_gRH5nelQkWh]o)[Uj7)@NrQM&q-?
	o?ZVN(3bL5^AfE:ccRf;-ruIQ2&4?Dg$,07iuBn,*bjb%6D:%M[`$Ol#Ke2/PsWZWa]K;KPYL7l;6h"4
	:+tA"D8fHj9C/<PIpOHZEt!I$O8<gR`BQ#L^.p>I,==I[;QMQp#je[fUR;:MNQS;t=7rTn/S@AJ8Fo%T
	]3L_52JsMh3XhHCO:bhu_g@gEM\fN:#L''0O=j"I2K2Y?Se\AG?VtAk"Nhtk(g/u=+On^c''PaY9AFnV
	b;I;L*_Uq()D#3?ho^!ah(48e]kjYXc&Y!A8u_t@JK?tI?jKm2IW\;kbCB@\"<>thA4)Dg\U[A#j"''H
	<fGMU7L)L9&tVBmV?INHo&>DPG*:We/)Z[nX^YP]aiYOdDZEaa&@Q/lN4l^%=e>:kSGS>Gn;Jlni+F+;
	aH$)p";lc[K1EA:FOJ?!HISkO6^RIC#I<h1q8*]ZK!s]>qDL:=$-BD>ol@_dC[''dnStOoZd3^3r\72n
	aY>h8''e&:?`n6U5P2,+I4nFqq7EH!0Ph_JkL&cPeBtkSks$dVEiI]Z4Z7a\An"b^VbcZT?a>]!n$ck0
	&09(8IR''Jn>kjhe^PH]4W9eD7m[AFtQu>4e)k1%0bUnL>''(uLl`<V<j3k6fN[<9g&q42lJhPTUTgN%
	;2ASt\Bp]%++%U;L]=<dhS5fcEM.35a1\EAuMe(i$C)ntr`_DL>j9jDZqENEN''SPR7^;*EAi-874q\$
	,P^ZL^++"Ha&[q?bH^\n?NFp%u)!2uiVlhfMjKh$[''r]SMUckLRuWm''W.lA#nLspQ'']Nhf$_Pd2AA
	ZM7%P9RL=&tr<o=-hs/b5N-XW&/T*k,HsbZf,#".@\\9<8B?oT_n[\A$8VCEAk''iEDc.V\nlRMB3m^Q
	bO)rB]2@Zo[f&/Nrmke[nYp&:ZBF=5o1Y#]Y.7g0Ka@I=TPNSn++r/NquG5)A%T^,?e4Ye#&5/-nQS]d
	i>\iM2eD9oX\A0..M=9sDJ(R\6g,2l*mLWtlHQR<r&D$V4Io,!j-r#Xu]a&ME\rD&Qcp?oZar5__^M4f
	>A5#j5Rn@7BT5[NWtT2Fdf/*UT''J;''o=/f0i$_,Vk)``TC8Q7<O2nOcoEeqI=%SePS6q3\]KM*qs);
	!>r`B`DTA!D]Wj[-TnKO3I,)@$b@I_<sGVY5Kfq:?igs=#%=YI+)@S3.fQAN^:(#@GIaPYMIKB,2W:_#
	9?V-cVi)ocAhVP%XGB1n*j''''eZJ3(Ee7W#3]JT0qUc4r^G)uugKa80qepK6-2IFdEa50O?4piV$F!r
	\#5#goS95Z5E:CaQ:G@h[H8FPIBY@g6SJI"Qr1JE8L>e_\lU7*I:\2*[4Vk1gn$Yuf2f2/o3hs$>=J7P
	"Q8/h1lH"n$D_K&/U<V&d"MKMalcC^hSc\Yee&r6Z''I\h]''r(9WLaPWB)1[!6G3Cc^[abZ@3-Z[=FB
	:M>_a]fHN_G$En?q/`k8gE[]R+tO>q;R6!.2JcHJd>bGPKp.o&dtfj3R\=qj&"H@(jlu0t3=>NoeDEe$
	%AYHQK^km?I%J-4c[hpZUu3M4#d''=^U&BCG12ta+W?K_hUW>l\LE?]fmX:6Zbs2h5e>Up%"ZI-pW.+e
	DG\":To9]rMW`Gq[;EIZ#V")V[H:&E&[pm_OP5\2Hg`.$[TWlbpiL^T+kX%J:DY;jO/q_3Hp[]GAq?ac
	J;$tBmccBAubh/^d5GaD"reQ4,6UpD7J(IPR`\mC0p`Z;iJs+rFZk`4rd[gS#_ZsiK(T1gd5kQ''D`SZ
	3dOuKoOIO)h(_i@bth7A?_ed=[&lf9mR=gKD!:0:%(!a-m=`Dsa)]Ga>#UkYN_;h7InBhs]$BonlY][-
	>7(>)FmXEPn/E#[\"!]p5cPUBdUbVFAZkK@OD:Ii[__&C#NV9YE<MYhFZTekPj`.+?[\Mbqf@k!RQC7@
	ic5S=6Ob][`d^`4Zr=rM+1j/Rq4.HQ/C,+*o:OM2prIcZf/JDFFrI]FSMitaY&u@VQ5$m\#$!0;M5/)]
	dF3gl4l/nd>CErlkR6t[I`N=L''5OKFF!_^<gO[r8XJ*fm,nVG`)i>hC+OfSDZqj-aSf5W8#Ico&Ra+e
	k621r:@?f]7N\<.rbegtl&\:/0"ije?ZHnpodBQ:^dM(=+:"5)IB4G,c[J!?M]"3_s1ZYfGe+eXNX%$T
	mWu$h*&(^F\+X]qWU\YeS8JjH2-S%E$<p%<?lf%Pp?Z`VOD@R)o]^%kl:7%_RQ^F&(gslb2`WtIGXrMQ
	jiDK7(;hjbMR<=%K*JDMn:e;FbR?__C!MsX)/!i+F;pu.()SFO9&RG''RUMaqirIA%to<pGWCND^k@!F
	XDlV=aqeeo6<$K8+c&h\)EE(<<=&g^D>*//lWN%_a]IuV0[\Co*iT0J\q/EUrGD31EG-a(eEq:bJ`DJ,
	7X("l!)2`j4Oe]#7pN!YUl643t#4ntsK:<6DaNWn*,Kl3bB$O_hq]_HQOX5;en3^5B:Dm`4DLo`H*p;5
	;&U"Wm_3`''+c(`(os8+e(!e_7SJ\MO,BPK$qmgo9+46\A!dqCF21mq*UR0Kra#*#1b0M0pOrA^Aq&gh
	6!"c,?T17XNN?''4]&''N9NKN3h<!>WLYWAUmj.+<;dV1a%O9IS@s*4pI(@%Ws@2CaG)UmIl`?Igp(+j
	pk?pAaT`-tGiHZ^0L>:hJ/K`h^B=>2*fGC"o3iga4hL-6`0ADg*uIj(lS<RY_%6f!dc_7V*If(A7GWtX
	mQdZDbUc-rh#c8?An+8b>&Y^+es`!*<-7rEE6Es11DK(s/>GOmYir1)eEpO4#%QkS9P.I5J5Via0tW1s
	?MH=Y,AX>XMM3DS''DI[2EVQq7@52pi+d:MYp(lHRPN@*R=s]+"%P3$A2nGD4b*0CRo0\Ja`8<E;T!]m
	U,%R%2>8)P]]A>J+<J`!`^KH->5%fmp.9DR.j$5P.Fa$q@Z1+h?J/h^)+DYF]d)-I;O''LtkKuTh%rR0
	6fFHb%h]''nK?ffONJ''IR[+2a`%uJ"YgNojIFO1Xir*!LYUNNp-31Hcl_J#ZRSt(_D!(Q8Zb`''6.KP
	^Z3DpDC=/_n''@uAgWqc\`QDRsY&%.W&&5g)b(X[ggM#_Er$W.*VaZ<E8CS;#$L8fR^1rJ)a?>5*''I.
	$k&)HFoV>tF30[9RO"n/U-#iA>c4-;hiW:Nq$3G&h)W:0%fqgF\S;=Y+Xa\O//m?u<UMO^71f'']e^.N
	8:"m??5bMIYHR`9X`M]b5FpU-8bl..O&hY&t4CXjMr?XV\sO\Ya4R\Y?V=m1[8bDfRd)=R[PnP42nnOR
	U3W"O-bAi,%nqrNI=h5*0q92"U&(Wd2kX@8N/3Gjc1M$/-u&''LPsJRWj&=^,9RD:`a]gS-PdkYFRmaU
	Gs4>.#Oa*cf*ie!+piSrEUD2Q:JbtG\RjG[V;$X*dt+''-`O_^kHO1nP/7\$6k^-k<IfnP8YRVUZhbMn
	*,/GTFT%qi!,?q?.!DU@3\([M-7U=A8D6F''6k29S7_TP,;2`@''\gBlMpcXQa+8pl"K@3o=2B-!dkK(
	XsT''0TEdE*+f'')"X4i"!FDEu7>E%k=6.&9tGJqMAEi(kBJJIdf`F^jP0MOu_''?`+6SjDM$b4-W5%p
	&REuF:7P%f4/++k2`)^?(#"3&nk]Ru(#%."9g;5D9gHU0F!4pRRLkY#^$<1=fsIS=V*hQ$Hb-%$^''re
	uOPrcC[2u;mj:i!HI'';Qo?bWe(ARjuEpquXP)iMn?&!p?NO"^KYFo:35p"oA$naY`&bKJqh+&h+anaX
	T;eHP!50Y+dk`W\#&*e34?5*o9Zal;m^mXYIl!f./p&##\TYQ*mnhTj\b*''^R^Qijqmh3fbj@W?Nqs%
	NIhZL+p?QS7B;0DDn0QP\W?^V2h>%RVYLg.<pAK=<fAEZshkKfhum%<66:ZnQ9H?AWHm$FY$X#5hS=_$
	@0!MeE''Ac=EFeS[k\CTo4Gp[El?I;uL#Rm#eu&.fBH"VnGPJT"''^@3aR"t]A(j#8,FV=q-U''6C&^M
	f1E(:Jkktlb&I1s73jLXgcOa"Qh<smZe`RH$T$H_(2kE6Y:u.Y2X6s"14P5(rI"-$D''0a^d_>E@"QKl
	ra;g>K0fHA4`;D3nggpB+$pU"[A%t4b=]@G_3rN#5O;gcJ]qHU&Id+nrQa1YtAfr-KV&%or(ea!*->:l
	qle''4e8#j(<=p2"39C``ORkl0Ej/s5JGU!mmYod)Pt)nW_oRh+Y]^XN-n]</Zt]*@Xh`N"-q.Z!ZNZQ
	''j2C(<a%0%BL6D<p_Q`eAOjYB1gRSS&SaK!h(Aad?OX6.Nn!aU0hGIWS=XN//3qX_<DN1V)MgBE$RU1
	08)$92>M^49DjB299&f>1o>(#NRds/n!tP1i2tC4#]T<o46jrk>1m$b8#cVYt9+YC[17r&gR%5o.2PA?
	>i9!a_#(fYeuTC*gmuV[Crf8c6EQsY''@*j`A<^+&R/=5lI<1>2K@%W20k+,dWbB8M-+Ip.dW6=6\:Bl
	B2*uFN`<$0V5AB>5B1])X+-a(gO^p9Fc34kZU1*A;>K@B-Y+6WJidMZA=M)JQ)X2RZDsjNejc(rrMWoN
	=h:@ag''M43)%$=0D)0Vp#rVj+`M`Nn)>9^ul\i#ai2]uSIBRT]b(+jC3dD@kbd9.@pNF8E])Y^nk<Cp
	]P_t*`i!LU)(%MXNKN))C1)O)*o:eiEFXL](k;NDL;hSV[!2(1ER0BBSM@:W9.7SYh`lV/cmNf1Vg,Lt
	Hb0\HZl_Rc`=tVY?<Q)a2''<%/%RMB<C<g:foJ$LhWGfL5\5>$UOft2?)HS]o<q6#Yj=4T2u)9N''cPN
	06R#/cT5(&hWG#kWRNnSW.%!\P$7$dM?T&PZ[(NV7b]c(LVHad^4UjQ"OHA]h@h;>2<_mP8CO74QN(m"
	N9T`kl.1_ergs@kZmi%*1Jtm[t+ir*MYD]+7DO>`Jh)Bh\d."dA/Z#IPtARa<[iSu;%5=EnR78+V3U_''
	T^W87G1#$k(+sr"p>"`<(=L2\;LB%F_tT"[J6e3TgGZ,!T3SPLtS#h$f1U"dI&i<S"t2qD4kc5q_NtE_
	YqTJ,"J+[?E6C6C-=52,De*BR=%-MV"N1NZT*EKF`aOVL#$i86JG4`(:g*V:T3,=X9@%cY@#>^s=lmG@
	bf&"8X@4a/.B3>!S*!Y:(.Q[#U_.G+-in/PrXM;l36PFYl_J,F0QA[hX,"[JO#(^d/-s:Lmgo*Tb-dP7
	+dA;E"6c+bddnIj$UMW99<9$g#6ck&kqc_!-biXK;5lUJ;-R=7>G5[k8sD!5VPVWo(tV^VcSRF/d;3),
	ZQm^c7DgH?WZXg?S\NPN9.Vk6`ZD_W+@lNL!EfJQgHBZ#C^2gm:(A&c\f^jP0F.1aMr*WL''Eu][p9<d
	8''8^HIQVTiQu%B]''_u`(XBal,GtTeT`,QI*bhCe2L,RhTiG0rTP&m#+DW1N%`4fd.]<JM-`LiMIm`o
	?''L3C2Tf:=do#6I+8[NbbSZd:!o[nEbTH[D2\''q<YX1VD,.+LA>N[tqi8&j[KoCDGRoN(NV9,l+_lr
	k;sqnnPsOi>2Q=DH8hnO''@JFSQ_.@k[2sO]+"dX''/X61&s\"^3Y6\8l*F%(TN<@_V5=SCHJhf)QaRA
	KLRgro$ngdgK.]1JB=-M%?M!VhQI&h\6Vg0a>87rUB6=oW1Hs0eZM8p=LmLP;)RaC)nBo6PrTm5GTJ8T
	.X:)]&#Ad=aH9J0W3@K_VK4+s*dO.7_Qq8uT426<NgpiYX^nBq6*Jo75U)r9lU9*^aX)>a(f2@RNp]PM
	*`B>Fs-TG6!;''[#"r`@=+;Gqf(bA#/-Tg$Cpbais^K<''LWd5OSOIdth!#FS*R[qYjf;tCF"ddoPWZG
	DGbYsCt5#J$8a<j5EQVYk&bCqqs>qDO:c?/-->fbEm"glWCT<]-#@+@NpZVHb5+\,C-jM%<SD%)0B?dZ
	;#A6rJO0//Pg^D+l_qPHR@YNjY5$fpb5#Tj3aFX=gl''!$7Q%o-TApt+UoeG4;"I\\=F,7k+u6VcL4\b
	ImH&_E5[Y;;D3neBap@UW"M*P%]h1&cr10!oW#F6Di0WFRut$aTQO)F2cc1!L8odsBSjKo''O$NBnJ"9
	h;''AGZs[jQ)BE&5fpOE7u2m0jF@?M67IiR0m]+)Io9LQQ.iG"`8uPZZuq4e)TXQgfSe9aj?hh&*#Zc@
	kPl!n4!A"rbG1D70;''oM7k@(,P.^$d37MuBR7D.!Hf5o)Z3:*)He4WE3<8VPMUh.^(f`r?HAI9%>fRI
	8*#[FI]Sbuo@h#"K#QSEQeNe_X@WZR;''@sU39sS^cX^&B@&M)NoL<AmtEM:h/-7aDFNi]XP"0:7_h''
	sCLHOOh>["=_fWMijIWXW''tC+%8*7im,id**`$d^bRnF^u`AM!;*Ys+#a5I-geh6"##h_bLW]^@8\aO
	d33jLtH(.dE+F!7%mf;TtuDu_SMLjb]EEI%p-j>4IYtQ16Ze2%OC=Qm+O10U''^F=\T#PbiJO2U;pjI0
	/U&:$/k0UH)3m[eP^TjeUana!eWFL"?S4/rEZ67h=>*`;G@IId&I^P!-uD1d.QdKR"\Y$/acW&,''(4@
	TdWu-p<SSoK#OP[Pc[Tg1.9,T/(.Ejsg^!]*2npJZ,B<+jMPI<)fG?>*jHn3_p<IN\k"(U(!1o\CF(?)
	dd)^oPke@>/k''QI-aujt9kbcjU:5)_$XJYLqJV.&U7(I9(m=f-n=U1spU9D[mZC>]7?>8Q+RQZh[N71
	_]42Ou#^@>ehY%5WFd)K;@ee)c,!>Y8L#:C;h(*7O@j3+9g.2D,^470d*FmZQf7=;87Ad/M3'')BJ9L@
	,G:q:&*q.>GM`1%SR51bERD3BMG:ZJgVl$dl)rb(ZgTSG&,;q.s67ZkeFgkm9n7A1q(/a)9tObf7#0KJ
	M$e!6OS''R2VH''aN;*a(NuhV^tV0*4Tf+uRBdkcN4mULctV3?LfA&1AF5GR>>Va\MIeq$fb\d\O_gb)
	#jHTEg>#KAX!=*ZA;:"!6b/qf`^CB*nYQQP^EQf@-:cGaH00..YfnYhhAdS@*^p,>5?ic/7Ks4YjB+pa
	''DKVW$FG1^[e30AGUVf:m/cUMiC6C\YI""%l9ph=h4(7L@]r`Q"@K0.4lj]"<qMnu5Wa8\L3t$[K3E%
	",qsA5E;;OT)OtBA;VSCe`R&TfU<^m^C.io1at"AC\$!fg<!^Yr-]]Lc!.2\3AE!Qr!M]DU3S4g&1+HK
	iW$@r+IqlbrD&r=r:s$pZ!us=/BmS7eQOEIKZAJ(CUU-C6-g4Rn*=ls7aK+B44[NL>;PP*EC6''#V>B"
	PmAWBLj1buGq@+(Kf!"0J4WQEgn!4I&rK8EKsb5<3AM6-UYW)*c)o]8\L>%dM-#)$#2bfMAXo''`/0BY
	JY6m8G6-Ngt+N+dWS[3GLn,g,Ls9JNobM??L.g\nPOMg8ap32\+OJ7aVohdg+_/1)8H\[6pG6%D=I;SC
	6!ug5W^5d#k_\1-)VSM:L>,bF#<>iX:g?>''%<eF1P/eePDN&O[n@H9B;[N+f9_h-d:P!EPJc`nUK9i)
	W3TXo<;;V7hDSTh(.3`ShrX;odLmdNDg$1g_/$\b-jbkY?^n.<KeO33jkD)U*cIlpl8:*:e=9ZI9eWe!
	uEh)o@=9ep)UZ!mO1+c/0[_T9d?r_SpZlh31[?QFF]F:oEa5$W[eE4rP&$_mMucB&g.ZK,k:;?Ir>;C^
	A[d&s1,6!Vkmlb22#V7O3>[o,gKq,9%J/akR3g,(-X<5$me?^A-./rla_"Pg+cL%P+,f23\hb;5>0uT>
	.,2as!1VIZFnA+''bf?KULG&agTY6J.;#*;jW.M=pDFKZ"HRje\R(E$EDeo''Ea;1]r!0C">MkHGDNe''
	FnV(DX$4;!eShi73ZP.u#4%DKH^*2^2SM-KOfor^J:RA.=V[ag;m/64+qS8q-+F.TOXrKgPf==6n4&<u
	am@5*aXM/T]\>cHu1/qS''U(bGuYm[,#7Qu/kY(uuOXLb$dc@[lb9_L1.]5qU%73C=;a>Zk:mMn7;fbm
	30[j[Uc]/28uKdq-KV%Y;MTFS,7X4rb::!hQ''/=P-U8R.NCnm#\\Za>EhY(61#dS@r)A5SJdp#]d+&]
	7b?12pN>G$37M!O$)UI.2i''4(da)H(;aW"%JYa^Fn!0Y.''3I6*Wfd(0''njZE/i3Gb"VV\6d?V`dpN
	gWT!G6cjNK2"Z8TT[(`c(WD`99hb3"A4^IftF+Rq-lW+0:8:6*$XA]5^;Ts3BAmWncQIOLm''*9(%mCN
	3k=(YR,3@:[TkunWIpYAJTU6^]l:aJp#MGnYDAX4_AeC?0Mj\9<@plZlXG!.3Q>*SD''aJmih$*Q4]l!
	_UeitS"=:>2''f3.-qb(XC1G:TQnh6D>qA;Ic<3Fj*Q*O\Vm:oTq?.jLpPu''A#(EU9&kW.lhJc0I''U
	]6()Sd-*:-MG.4<IOB-I3Q.u7n=)qf_Y6%n4)7+BCLVBuejtQl;V(=[Kga?&Y,<%/G*9;(>XgiLRN7RQ
	".J,@^,ffsIg.Q-5L2M:J/GnT.oZ*.1J=J$RKZ6E5<d=AqdUB(]_&S2i@lTB=,<$RM<\5umQfhJ2=!GT
	FPaTb(D[(XK<2_H\p48ta5*M9!M-q6LT?p6aqK`#LX8]-MS'';FUZ@+N3\8^p+E2GSCX08s7TC>l;R6K
	gEoWYg.[kHK;fCLAY%7BNKi1LK83)Ug2bl4_JI\O:''/$D.#aeVDdE<&;%S-3^64*"P-Scrt,aW_$J&F
	jB"K`%iAY^Ah*6p+.Q''dpsE-C[W%IO*bjhrL[&+(iLJ0Ir<e`OJUM<At%TLI!A!im#tH:%rSVEk&&i5
	Ogb$b`S5*2E]P1$gJsTD*,&''r?MF6Y.`.Uk:;f!I+2]?>=&77HLa4a\Xr[*dgoK*jpJ^M>GUBD?LI''
	gDliNO>m9TU.`UMp@<Xi,:tV.T$qIXS(Rp&X=/HK7T2M?&N3\p<?RTb=;PpoXcAq"&RH;#>d7#-_2?Wb
	FD0/h%/4]fGOqq/aj9]7:PS?Uk>atU7604JR](-UTWERngV\?KZnLd<Z?so):$G52hW_#j!;hqrJ%OI+
	W3sl<A0NP?n\''23b@*u1^6e\4@;mVM''qllN+^Ma''`k6^jKeh9ICJJj-]R21;\gCu:,\/FYS$59]N:
	$<OY;KGL''D:@[M@>O_;AtngrZP-nP\(CSqfBnoBSD.]>NjfC.[dc7UUMY]]/>%>#26\u%Y]1\].Y%dV
	a-`AL-nKQUkR5YDn.)\_$\ASA+q*?TpbZ^.r4Pd&Hg?m[,*6ip&h3Bgrm`J@+@bV\cF98.YO%M46hQl=
	\V#\O!EIT;f;^FRQ9#''eO"!ZT3n2/b>\DH.l.52"e$sOd52@roMKkOSf6l91geD9K<_g1^6@ISM1L!]
	f7M!^t(*GKKZZHt4NRJjCkdM79%0X>T$`WoHjthP(W\#m8H(WbJb[JN6!/&e(1(''NQb<\p@]%d:%Of@
	K)BZ.oTEGY_''Tn0psOTSAa/lsF/5T[*o?`5M=e@-lfQn616Z[+(5;9aqKrj#N;NK?;6''#NTI^7[YcF
	5)F5b]^(R/Sm:EPP<"q^^MDk5Kg%U\uDA/n!hi3TVM=u>^''-FpUUT9j&EkG0LqJS9YfMTdQg_El3e?N
	WPArC5p7<5K$"#:k6oQ(@7FYSD4<:TP*^.eXQaB.s0&hS4[@HKKIHm/ElrZs9lm[W<Ms''_oiH+TO@0H
	D-gVfq(8e/,Z:4?28d=4"?!hbkl)M``4G+p)OX_PmDG-U!''MiK?64&@o:QeI$-Xp34^BOC!VYG$Gjor
	lT;?-i5HTQ&*>QH>?;.+1oXI@dH4>6fG-=LCP&A,)a1:usZ^>bAiD;U''Q7M.pbe0$T[ngrUSD3e575h
	)ldY1^7^p65-YV,TqYm,&.tiD`MR+TMLY_/7Hc_E.pCafMcoZ==8G<W,U=D]*I`pp5D@5+\.Dat;-i]*
	D!ZL2F&2VS%BiPpZ:m?+DEj]8Qj.CUeD--(S1Z-`\)Vlhtl6=f%nKi<+5-;/NY\f=D0C.Uo95D)K&3NJ
	c]ZjBRaDJJNFNKq8JpF/@7QWVs<CKgmq;iNY]q&+d'':8nD(td!59(j\#IX_iCkYqTCJ?3k,Rjc+>H80
	RXLY2HH60$L#L!?*^iuFS.^Q)n3dIkPDkmnk\;VY`m^c.So3I`[8#_QPl!qZ#X+j4I)gl<qq#q04?T`<
	=?+BFtCqu<?;IQ,-NR0j,B#7;bY/^5=CBMJ`5irFDXLbe?dl%eM>b7B?@IhQ"'';tF4(8kjUQV1fd-m+
	GDkXNn''NW],!5e*\q,k)2%8\RS.&''U-&L9*qHU1''@I=ltidj>k5CXs$%j29Dc8?Ro)^2m?g,1''QS
	''%VgK@\$YQJ4`M81%)q_/,tb>:/r\-MmqiH366WDmhT@k]G0U^Nq+4_9-_8B)%Ae1>U\(NFpQ=TV5q3
	8(CE$:6]J?6F_oU#ed3:C:Nl^e=Jk6GNkX%d[''.Rgbp&kmMdDR+JEa(1DnEgj`9H_JX%/IIPKOSp&c]
	glOkaS-ojedVh])AZ9%1_>cbL(Jr3''I3frJ($9LBeCOEXP@sKLrHB_U)oK7<1P&$aMYH5<meN7%SU>g
	pqW[hCP+"q_ao%[,MZ/:DL7`giXbD,XXp1g1mLA#tZAUjjfk?92-<F3H)JepCBb8\?-aHSdp$Xi0r[/-
	M;5@''?f=''hpJm"N.&S];JkW)YbKo\j[i3^,O"4Q&(SU3.isej"KC_I(,J%-?IP-\%VE;I*"r+mt"Z0
	_*],&)Hihs+n>dBi,XJ\,jC.QYZ*/<DIs:V9tYZFO4G-r^s`.G6q&QH4&HGf?+nQ:mF\jk,6:VX>a?ML
	NVXUjm4B5c480`:<rn`,rapsXHK(#[>VGFYnt3u8f''fQIT[m54lZPGgX3u(<X2iDBDjXA\ml1LP''XO
	&^#"?mREuQ$C:A/^h7Po($SV,iCoBiC_R&T2Yb6EF-1o^=U,DSbJl<S1Km-V2&6uCsRZ=CZ%b&&3W38l
	n&[Wbf[M''(%p:9D`3;LcSTE"D;M_>-.<Wc4<pD;<9)IWA%Ys)6C2%mDKbC<ss$sR\^<deDEL755&4r*
	O.4X]B;kV\s;U.0L`;U5!%jgm1eMcmR3JuO.`6M.O_"akZ&rBAD+(TnU7WQ,Zp/-QZX`c_C>E7K#W9qf
	rGl.jAQYIWTBWoB=B-*3FPTYf=GMo+e_KQs\T!''T/S!5iM-(>aU>LEMQf''mn3N[?:\"l1,$@f*DW*4
	cd*,:aJ`gaW2:u-A;0Z4b)uTpAMTFs2e''n4,D""-4U$''Q2+a;"[-rG;m+P6@>Z(#<>l1d1h>SY-4[n
	Wj?[@QAdNfb_&$-`Um<iI/?mT6-b9SM*61VEhW8<2-^-:57S++*@_a4kXH/=6''>[>D?L/C[aJVps9`V
	^r8GAiR9]MX(Hr5*L)rf9b\EjP=Zu7eH/Ki6&AHZS+6eCT!rm]o?&>(TSL?q:`&T7R@V5IGG6J*OIDXF
	F=2,%X&R-Jgrpjb#f&.A(os(DB"d4bIZYPn@UmJ]mFUD%)<jMUDj6_uI?L!4&1\NrkfYhG6UU+&kPE<2
	^b<YYd@aG%6A$LY+^$^L8]0KF$LiK''h^kO_%q5*Wm]U9ItJZ(/1P"lt`1/C//okUZV_Kk)/,*WX5;L(
	(''@\R*b''YsuPd:KTtNlgtXkSL_m7@pkt)q8NDk2tX!K!B,<?f4K7:7tdD.ioZOef*W>Z(iJ#&RAsuI
	JQZ36agrg4Q_qiuEXs=#\F<RLH0$,jT86jk+r]!L^jN]AhdEf<D(mcq.[''Kj*@(.[g+p?$`V`"i<Q#e
	Kn.t3q-16fGi\XPDf-qS!4HF_nafpMk*/S+g@]Xrp7:T@)!up1W=;W.AY2pVBXoVG.gen2U:(K?__lX/
	,aTL*RZF[Mp]tQe^&j4I&Ps,)Kk;jM\)R&Q]KDH?RB\!q8*F[GG8P<A276`Im;6g`U,L:,>6sUJ5L''n
	CM45Kr/#EO?.hfdo39a9k#"=>V]k<u,d(Lb<?JAF-B-NPMZofje4@ehYr[>pctJLoP\,^H^YNiVb)`%2
	Pj[=*3*-o*PiBs6U9]o;7u8S@We<WSW904c+Rj]ZT!4=B6m3USg?2&qjk;hbEkb>Y?Y&Ot/<K`A2n4L1
	l[fmk;@:DF*T9*LqK9<S7t''@^YO0TV-eV\^FP-R5hn>Jl)6nT3dX"I&OJ>Kb*=B@d:OXh0)L_N00<a1
	MB<NrZ&W'',/os-4%e$*am;T8LAL&R1RYB0!hED%O;[MWK8YR7sCbR1.U#8&<JR=$l&eTP;@l&"Ahh(#
	*N6_03$fIW2Tf#=RpmZQQGkK.JMd\OD"#UKiGdPND9/d8u_S,bKCUN*]5#B9&^tZ7`i.D<8IY3F>pr`1
	"f2;B?q+1pgSp_>CDc_Z+D<QQFP:<\F;9@aP#:<ZWq-57Y/"+4S3^pqUe;flu)n7"&rCAgp6,:%NG^Y2
	_m`jO7UXo5!Xk@bTKD-%QVa[P>oo$NJbo773^''''kCA%!''<I<pi_siZcDT:6(S7f$b\/e.PECA10?&
	M-7ZI''%#6?b)S/uYCm-:''#5N!S#RQFNThORn58db59J7b;h1?hA"Cu%tE.R#Tle;>FiH8grG+]S;Sb
	U8@n2l)Bp:mM\-kRDWAENOK.`JJO4`32B$h0\$-.q++%5blNm"TYC"5^Y(kjua`U]#68!jPGRD,+54c0
	`^1.9SX%`4&''t<a4aTsLlJ_uo7]%d,RK/SZJ3VgCaIl\cMUM\ou9aRV:)Cl''?<V;Bekl/N56+,''a*
	V9JXXc=VIK>I/^)fKn>DF7TY+tf0u.aH(bR_Pf/;4gpqji(?ZLmW$[HDKiT-[cJ+UUQh1]qJ''gH_?Q\
	_7I8&f$?TPYL<aqJ]u3>(G-&QPt;n/u\1N5l7Xr=g5fA%`Y-c;\gWd`,HuC\9?^o2M1\.$W3dg7:CrH;
	kBFFhN`8M5I@P-CN\2dMnc1p%Ri55>uT,nYd`XoME;1""XL34Q<l?%Y?0ILCA!CS<Dtc(7nT_lo3u''_
	''&te=J`Q3jm0Lf''>RiM`''Zcc]mp")M%@L-LR;J1''5lr]\.^`MH,^LhOdd=O/j=LU,bR`$F4/''>A
	J$+t@t7D0h_&_q90\85$tZ!R2:QqYN!nB"n:cbuJL=8Ng7pb,Ios!]Pa><''_h)SWoD>FaLCY8D]rf(Q
	Y0pIb.?K%TWQ^3ERgt*/2(%,L;.SE$a3`D+)`lS?P@UepH''bKI''R[2dj9m%?J><8qo$;gXB.dFKn\6
	d*p07%4\%''QEA0:;i4"p3T$WL@XB)j<=Ef:,&kd+hDh46/?mH:iIg%"6HVrA"8iikSk76\-qo81s50''
	3PlSH*UCUT&EHkK.fMjQ3X_4(*c:lTA9qbVAh`AFkQ1.LP0[BiP"''dTlXe2;#d*KL<DcP$9DE$6>tO?
	9O;YoV!5L(ZP''L;Io/Scg:4s(q`q''ke?N5?i@#qJ*b)99OnPr+c%Sm]jf=N+sQP8R0Ue8,U?!%2^^R
	c5K#TK=InB4q#''GWbQ7#h=h"S6.5I#1JtC;pCM1Z#/mgTm^_a"TnJm_3U,2b''92<^%`;H9@jpgYD_>
	]3_l*6/%dS[1o5R;$>ZW\6GG%V%6@7G"bVsS@A7995k:l*Afg*<rJ.X\a(+DuF@2OpOBdRaOJGXj>m4c
	n$_s3V3]^<lh52X.3@*jUksTXL`?J6.@Kop=5GU4oe&[U^<C7<rE(\$.oSS>Nnhs2#<d[[C&l7[=S%M3
	eV^''gD:u2!S^CMkd77`L2[34?d>eY9,-_NoqsKXB)p6[3*P9LO#cTc^1`.`q&J"1\0#cjY2`/g<tb6-
	W?Vk''GE.7>(aK!/7:!(VJB<EbpO,2cUr3tqUCVSM)p_B5C_Y/Y?pJm%&OD6d3f94"\oHJN_DkAP=g=a
	/>_U+=bUD)aJ>h(_j!_]L]_uA?Nba[kKV3^rgUf>K8^d9^<k5U-4aQ$8CnfoA7aMTb<<tiALD*6`(Q,q
	,L\I8VS"5-a^)m8%914nL6`82%Pf[NC@CY\@?:[thC>_:\m>Q=&AR''BDdtp`V`q:tF+(E"^X]I0S*AL
	eB,\2''2e((?GIp>,V1J+\#_`:36p#F:1aJ&\U%lJp]AH_DV7<snkWlhXH/jMp''lK/(pB)?Bc6BoLE<
	QQ4P:hYn3fGf=Y=5!h7A$"pqEd&%]e2!.Nb]Xigm$obU:ib5/EgUGKZZ/]7Z^pD`gcO5P")e:X-[(J%V
	%dc1cP%VW6\f5Y.mk=kDtfTcqCoMa_4K/b7\.9F*5d?V8k%XV\f+VBR`?mdGE`kh1fQ6NtW)l?WtAa!\
	QTA(]=IE-bfCfOA<C%,GF=@&^f*12#.''>+m@Fd^B-ao[8WeN*e^*o>j?f8,@1hP;"#kE?[rOFYfqG-L
	KMM3Fu8Il*LRVkF&VItA,W4sO0cK@#!aGfq/S\C&QgUqBq9%VMPmU@>k7ibAOVSVbX#SWD\4u;$lM+tW
	M=n?<d1[rou[^V(WS([+]!=00g[+em"Mc*O/fIP!XHFI9"_;=B$B56DT$BOI'')4jnl/2L5X"RlF$@=9
	-6m`-YOrq/:e8<Gi>=i=LUdfuN<mc?enQ7_d*SM&=4N0;q2!^O"=mJ?fTcB<\h''C14_$`B6?sVE+%14
	.<FMig1AM:soS%SsA\MlVoMVZA.3npG>-?6oNU?4qPjm@j*&kq\l7RAk/"Qr4LKDWq/Jg&"=YUI`l5?Q
	4''''nVn)n=I6H51`^T`3#Q;Li5NT''J7QGsR0p]qM$;eO6nMn$2MI%4Q3$2QgIFZ"1#FgQ(WYF9cg@o
	5UGEc1Qbr%iQ8`\ZY<D`4<[-:kN6`MR4NZ8d/!f)+_mp=PAuP<>R9JdrZ/[kN;m;q6GH]5(W!jLaB_NQ
	f/;gbK-6/::ok%[csQI&W7RUeJ$JY,tD,Pd(db<+@A(C%?6uL]9bg`#NE&L[P-@NBL_Z4pkgSh#)`t[9
	iCJ<`#M@.ST(X]XCIsE1J6,`(HZ4h^g.dPa^+7B?-hFe*ko(#CSu$2kt@Uj^5aitG@3OR%2VcQ>khtkC
	9<%0Mcn.<G&b5q]>A\RbaL`B6t[GPs$2M"=B`Wl#"#"QYq4^<''i(R%EfEMS*2LNc5CrBJPL,U"4#5e^
	(U0$7Uf1(]Ai$E&2Dpr?e>1K.-JOmOK@t2nPNqJY3j;b9nUn.eRE?g8F#ISLYqeQF1SmcW37pNg)4D&I
	]&B^FaAdUDl''-8=9c`WWd>+8"_S3=l4j1I`L.BA_OiM$q+`tA5WXnME4DdOlQ"B$>9(JuH/a>5V+WS\
	L_smMIG>#YHgQ<Fo+nC?]!A6J=\.G=rE?rX'':K1m^9HG#N+_;jZH5GhAV6u?*BG;OR%,JeiR(1Z''9Q
	[8i--)s[*<P^[:p8NYSFI6+O@t!FE#''b_L#"#Q]+\^6,sf,jh1Uk*#);,kZ3_cC7LplgQ>(;bKrC=Yg
	@J4B^Tbjj.mlk1ZP?UGB:4se>JK2O!1e/UP<Cc@\YImP_Quo2_Y8/%#2K^llaWRK-_[HRrn2l3cP?>#''
	Z_tY9_M:;^IM9#1U(ET!)VW]RObnF8=WKL/"6l3e9:m&*$]m9q%,QI]HX%5S#e^_gTM+eh>A\7P8&RJC
	o47!47_`I>UTu<(tLcZD#DSM^Y:P/?[8qRH#gW3I_9;EhiiP#HL0aunXc>=a82F0@*6p&Sf;P^4aVjDi
	KOpsSIr0nN@-5>o(us/Cog,rU8s41]Fl3ihX*nI''Hn@g9`/b*]gBa$Hc@P#$?g5_rtc,0Y]61p:<0Zq
	BUn_h?t.Y@[81a$gr\1fFnadnL6n<tSgHVaqtV5k_E,F#3,-YS!k^30aU.qS,%omR,pRk^rY4fqEm[)H
	4Ps)U\X8E!5Z(Lom*(@D!mEcqh''j3!6?G''Fib8=u4abcq^[I!;&M=O=LBmTofRE!tD`CO<E]G5[#$b
	h6^/,#S9inW"eX)S8>*un>Y7*\cZ"uPcVV-j>@rU:_adL=nrJiU''5#o?@;Qk1$&a>dI%BIndHjn+X_f
	nnAgjIOKou%Z[aIG\fh`i.H)2TDSSICq\=ZqNdGYNB4CG?%QA;co;\;''UA:lh;tC20/O%P?cM^O-''%
	+lbhMD1s,X.%(WRE.Y6p>2bWtkPlLmLT.uicGq1qctOiT4i6"9MnH9VEaOfF]0Pb/c4^qg_joh*T9BHC
	GXFk%:F[la0BERk[P"hL_M''b:bNL=Q3pfeCs3[O?XW''<S/q/X^hF#;Bp?0N$P+p0V1+%]D\WmMo$$-
	4?VrYrng;<pT\Tk_obiBB''o<&GK19"TT9e":]Pb"T>64gs`*ue-^@8fN7B=''[o["e<X?ub^V<''(M_
	F;!dFpK!XML#;h!1#NGSZQK$a5U<S4?T0.6$9GJ:]0WBd1W=sPB5e?OqU:KsMi#$+,]]!*`^uc?3I?YT
	\:P(g>7-:O@m`-di)2=aN$)`FM\fOl)Kt1<50<<p(V3(c4;GO,Zn-$nN&aQ@R[FEj1bEG?Ub@Gl/;''s
	sE)+#Sk3DS>l3TrF[JT:Gk42B2e?"msO2!=+%_3T\"$DT^7C1hj,U4_?Ar27/;hSls),8eEQ3G*G@P,D
	7bI&@d+!Z%8lYS")*k?hTr#+Q'';.#+p]W-@a9UYSVTSF<a3>>oJo:;:+:tCK\+8i4QEtCAV!*;21_2O
	5G1,92E<XR''$r(I+rc-(FuU:huAGnt`Wc7&V4MIe4doB,nd`HGdR:36VVJ%VS.:fdkPomPf-\0TKZf;
	_a3PYT5rY]*hOQl1VO''`)Ij#Uq"&''uX%U;5M2#qE=@babX(KY5hD?A9D`N6s_u0m5*T%$9P*SM,^jU
	+*YYB+af-3$^;of^,@UN[$A]XWkY@s,fa"kZ^ROL^kEleO"1GrTSp@d2W,12[K5Fi<MlL([#Yn(D4e9j
	V%JC=Tl])D-_)oRhJ(Br8J?ag6.@Df6k@5Eh!pd3WCt$a!)@rNR%;B@7R6;m$MIlo)LPstUV9R`6;e.=
	0ZlVq"`dWQ/35n6Sa8D<Ua%6Yeo`R>FGT;Bq@QGd7?(]S5VK8[5;ioYT-P;R"h"d.Zsl?qNW2_lQfR)l
	BYrs%1VhU%L(iB+-4a$_LY,)YdLaWg:anD!``l`F3sE,j`Zr5;ikk!I@Mr>?L&6n(Q`H@6Bc%1"`%L7N
	d8Va#UXN_>M9fL!R@/t`+dEPX4s)EpJq%q6_07ZU5&qiA,8(iE#0/>3qcuX`(I%hfUHmt(@Zq,t2M'']
	.k''#f4^=ERDj2`$k>]4>0N*V`C218cE0:\f`=c7fl?R.G(P2fZc(MZs8.(qkQ"$)e9OlF9*MP3q`gqZ
	>%@jIfNjRP^P`U:imrcpq3jesGAV5.d<Ce0O0ikiKGeWK/n((Nrh(rMj^5d/OMO#KG&CW2"fMB^[*OUj
	"N,Mra9n;>/<@NmM@8S]9!="MAn>d%dk.a\sQE2)0*\2O2gd+5:<4+K+@*"39(F)[$o9RK3sq+*?''4$
	bIr4?6;\nqkWY@:&t"6%]k40_e)a3kTPiIk^"g:@-^\X)<n(X"[dQcT`Mo71nW_*is5^7@kj>c4`Ql%/
	3L@6d4C2.A$=ZK0g%uZA0:j6_!+&*5lXnL%''3=GP*_0IYqoq"0`6g^,Qe.`eD2ENh\[iKYi`l6[<`7Z
	GM;N_mT&H=D/Ckm#\"(+^rOXGL4>W_5R''s$,$S?fiYsnS0k!O6dOl29t=gXVPtS=RY1`ILhALS^''_a
	mS`I*_L2"0YS1in6=s_nq5<CFf0:c1m$)52.!@l73-1u^B+.jE)*?-FP""m9X6[(_\3a.WTO:QcAh%\7
	0Pkss-0YSL_`TC@V&X8E)Q#qVN7Lp"12j$:\r[5bs;Yqq!:400hL6f`2>"7@!;r]OS(LHA*E''U+j-bg
	eK,.n[WU*222)?PT<V_p=<U''&P+*LPF''DfUOeqg+.g=L_N^`prBJO_*=)c;-;Nq*L''gMJ4*9:[(Gk
	LeE2]n<O*KGUNLY.-^Iua2n>5%Ru^.E8:UO6.Db.0>WhSk2L.A0up]52+DnOJW".\CR:d:EsWQ/V,u-b
	`stt''Rn<4=ZCPZbcB(YCXpW=8m1dsaV>+G%Ea<3]D?fl&6h5qhAE8cIG2u.\>U\(Lj8c`cjNd&kV&n%
	FjD''%7UYn"^E=K@$"1httM,GrKe/Ek,nV,Ls.XLJh-poQ*ac;gNG&i.lC0)/>F(^*=3s%@W9[^c_IOR
	H76cMu%1TFHc[(mqb#:)9ljo\Nr\_q!+1,c;0=)S(-7b-7U$;b7Mnu(3g6@St>G/WB-SIngq`-G:LY7o
	q&HNP[LV^:^7^t(unLrX-h3>u0pm3/pB*Dt8,+NE?=@WG31a9i_Ie.l)BEFTklZ+,8HeW:s(i$``W9Ih
	cJkHMU@AMP=d8%t^BA@BS2=uolI/&"jRd@@gA8Z_<&E^?MA,^Cs3(.''1t@pg.FVT;@5b!hf4NP#c1#$
	''AQZ#Rn$3T!9"%OR3V,npDE>6lVknV*U"ecjJ1N"?>%KYf*(:On%-!AVF61_Y_?e70%%8ij)nKd`LFE
	@8(YmAD9,_IJ*(n\9ne$$ItR^b1l`MY#k:S;GNrXh>''E.%j-1G5:Z<!>+hf@?*.df30Cd)rj\8_LrI:
	iYcpI=+NHG%(NOOM^7<jU)7=7FcTasN"7n^,B7/-S;\E^T$7OY-hE#0,mQ]HKJ5>[7\0f4FI\TS4\;I$
	grhTrX>?U+;7qt.T*"8q\!''NLf],k!cGi5/bR/&``e!KA5`8"$+tbFtg"fiT(FiZlZQgQ*c542baiII
	:Tif,\&(.+PM4#pU&l"KR%iQmi"pFXgq:KgHe-b3"[2h7Urt[U8BPISd=h`)Qe>mP_43drFIYsiALFG`
	NQ>p<!aLdhN(MtO))9MeT#@SN>+g[!%43!(U:%=4l(\2hQ&=<Z<\;R`S78HW(KZK00iTI%6OanU2gJ,4
	q.H:RG)-Mm8B7Q/0k+VeYV=5U*n\eY7W''M:=IH)-uOdklpa7^P&dYS;Q3EK7025SO86LboMq]$""?1e
	<@KuA=`6i7;4Z/uFl=KO&T^_`^g\Ne#dE11lh%s-XL!_,-j''>sE6?cZEHWe&GjOo\?FTmC/;7J$q^e1
	"qH??&(WcCUgGFn?VW[-^G:\Qhd)[m1SIs7iD!_lNE(Pf7X#A`pV6RLl:*b]b$UMUL"Q!]Pu6%(-G[Yk
	ir;B>/5PE>H4-[+2[*h@":r:"_''A)-b,=/^Y@U47Skl,"jMqn3^+l$abIaeA=07oq;ts^^3"gIa&,0&
	eM&&$X(_t;T"23X=kPR6n/;c"W$u?Ho_Ij+r.`;hoonfhRZ1=NT]:WqMM_;:6Na.R]l1+DQZm#qFQ4SK
	ai^P4GeC%;_n`X&9!oERCYfC(r02uG*7g''UeQ@riD_hYa;@(hX.^)c[:@\)T''$]`YY3)P&,6RLMmfq
	N[2]NZBp,Z`OYLKMHuUD[qU?o$hLnoW''8A8_7._dhagB0mN`O.R1IL;6]Wqcu;h)Mhi1%XUYE%`Ai/U
	PZLKc%cT2E>R@IWngL^Sbee3M*OoPI6mPq)_QNZPhII$*mJ)JgcDSN%J-!`0`$:>g/]@H''IKY!<00"P
	kISS0kr;<:P@UkWu>YFM>)h.uarE@OQ4QEUs2Z6sSsL!ace):JB8-''6BEp]G7CMBH_uj)AfuR,3P?A(
	ES[JY^TC/$hmqhH"tu;0StY"-PY:p`QP!_)frk:S[f^-XoA\HTM6Cj@RnHA.gd%VjUW&Qs6r>2$1D7Pg
	miJgG[5>@bT&!M@7kgi-s;DK"cVQ\j`Z"k\X$Cf-JjTOSO&Nj#_gF$&/^7Ub=M<T`p''ULO^;hLi0ffg
	7_F7j.Q*CUQarV,SKB+=0bC<Ch@ku&(''l);.gK''<4G[M%!*gr4Om>AqQ0e_M_K(FqkR`f;DCg3h><d
	/5(F/+!0A&7uG6u*L,#V7_6pteSO>$^])bY5.MW^\u,HDH15akPK,@,&SYE''?,l$F1,1=08oK[@Qenr
	XH''0Re209Y/f#19>om\67@e38@KiN&''6iLc&OY"^TQc<WHB\6!hhROk^L=N6&Z`j''l>$e0!4NHBoP
	[;Qc-*A#sYQ=\`.#V,VJCV;1qthInN>jJ#^rSBoaWKmP*uI(X@#DgcW_^@/egr=DfVM%1]"QF_L$nASU
	u$LsK8MJn>>*5jT3np,a\MD?ABTbId6\1+RMd-ZRq!Y%_+=K?=S8gqWnSX90b(\1aLWB/UrR[R/TI99u
	D&B4@,JS6TgDX(BU!;:OBJT>:1"!+ub(M9BGk4gTk1_k0$PT3[A=LCdS/85bbUbc:3ghcL07<8R3!O"6
	Mb1jKY>4-,WU1><FD2/Z=G1>X?dTJCQ.$d<rB>/TV-bf>W01qi*/iEb/-q`sUS4IU7oOVGID*IIo!9*`
	P-$0m''8@NTgcTMHa&NKEn*2B;egl]`pNO:*"?Ybo7_3s.cWY`5Kfc(sE1mMn1CWuQF1AZ;h3XGJbRUo
	K+07jb]39(qbK''6Q4U<ZCXa"&I"0s.^ZR21J*Xu5[qf>qsXEFZnu.p>`d)\A<_K:2A2C9aX!,,0/MkU
	S=^_SD1j3P0epX<GK%pLj3uhe2qoqhq1a@\R?6SKAHgEG>m]\NV`1a)f?$9nDe52#P[O''XH1"I8<F!+
	H2FTqVq9QdFf^\S^-cOF=FlLf@ha7(GD[YW?!F$4&VJ[WjJOc8sSJefFcA=1-5p>3sqJIG=*s^%ZQd[r
	F@7/@D:VGLJ''_!-sePVYBFF]>TKuQ".#VkGYI7SYID[2A]E_*33cB(Eouqf;5:Kc?9*Yo_S2;.5:%;>
	kb;:5eiT&MW@W;cI826O"?SRh*6_*[!P:@:pMc-dabdI/a&/`K*K&0Q-EFuZ(d,Tfa2o>7&L&Lgjl.rm
	r5E0<J#<+@&I<nAX9N"gJ5:YS9qKN(,(?EY[KWsEVE]>dab2fJmZ&U[QSJ7UK/sn;TBe!QW%D2h(TMd/
	MC.)/N,2V9h;kh0nVV''AIPnhLJW*/eZ7/IcE@?Vu%3n]+%R,lmf/7qrR=8^1&+aX3P@VR%kci_%SH:D
	cA\96*6CPoF`l$_<^!sXG<G6T>af$^?9h7?$iY]oV"q1SViu]XulU+a&n07ZPf"nDE]?n61Y6c%#:f_?
	^2\7<7@Vp3Ob_;WnRL$9aehFFd$[(PYF>JjYfod=bl;IX,n<nrjS3><X5MiTtpa#brM6$2cq1ZqjVu#?
	NSs/1NB.P;+$1I][roqO_H3_"(pc)LgVt8s/i=R%#*M7htYstVq>\E6/qrdi(EBs@J-fp=P!Y[^NPMlt
	m%`:O&o-Xc*pZjJDT7];ilYf;=-h16Wa5OJf.#ot;I"Tm>eX+DRZ2)MjQtWDSf,BXT&f40S:]_4(l0n$
	:[I88<O8Zj2!_!=e,35sb910u$c''>G`L)FflKis-\jCd]<6]=HSXl9^U>c@30U:7^6dOB`$]<e7KFG:
	o@Xg,>-`]rb<,qLsC8B#lq+NcFjIFuq!bR:0i@=is0qkk,Z*/Q\Rk&re"^s\VF,A8A`!,B7ka]r-''Y^
	RYU=GhZoCq(c=AZ35''qj$5jl!-)*MP[]jU)lps!mq86N>\i,j(*O:g5us[=[tKjKbOB(!Qb+eIWF>(Y
	?e[DE9H%WOUb=*"g/(1Hj/#ZEnaR=1dMJPRderg9j[<mUV54N::/lhN<Qi=3/U.<<B<^I8MXA9R`WK\7
	kMDKD9rI=<]:JoYE^eLm4>f$92S2b7o+d8D8C^>E7F_YCK]LJ6-[WqK>!k1GoWAY^XTtMqV)k''6!fr6
	c82O=->tEV7!P:8$\qJhN0?cl/\+dY[Oe1">n<N]&PYjK]^p.-;^2lJFYDltSI''EODra]U[pT'']Y?Z
	A:rqjfY<H4ZA#Sj]Irpr/#H!Ne&B>?N*D-9D_kck<f67F3JLJ8lc_OZ^bhJFW*<csUl:,V%Hg5e`4@i3
	l:X51"MosrdKVac3E<:)%5VR5HL1sW\p:`)r`(9tgG;/cDfmM\G"^mN.6C4g&OZCK/@>tq9*Y"jA5NKZ
	#CrmNP"daPu''2p''+e.tO1;W<J*7C[.%HiJb7''09eRBJthaUVi.I*o%DlG,tqU7qp\''QlD0E<Pt;g
	\&r^=:rDL&+XERuI(_&_a.P!krH:h[e9!@?_OK$a%5e&b_lX:7%;A>5[:,5)>:qVXNYoNTVNeYCWo;B]
	S^4)pEHiKL]7$K]MC''1LC+j7c3M_o''%mY!(XU!6&FL>s^Y-P07Rhi)]Hel)=SNZ5m3SZ$!Zijr0*h6
	+;oW=r5nEF$Vk@\1eMX-pM6*fPghFAk\cFVS-H(`PFDqAfZE,!2/S/''2cBbn8r%/f"qQ5PTa&J*"Wk\
	aR$)Ir2q-nm2Z"f0aXp4)^Cu:mKBrmm:UsOhu2d5<&?-Y/4FP=(.Z7ne.51-O]amgiq\.F$7-]P47*"?
	hl`ir)>-\kYckR^8Il,aQEqr''ca4,>^d1WY&$SBn_8+tV.Y`p:b@3i<7OORn:s,F/ek%\l^Qu(Im!^M
	SXKaSnoBanf*''"BTYjj=m0%jS.Hg^%]/Lm%>G*`EC0_`$>:G&,;(D6>YL_I@gXn+NP]LI`''V[/sB?(
	*-=(UptlMfZep]#R#=*,),k$AoUn:hRgFo"EV,l<h'''',K.h\!/FWBpip''Q7Ym<#1fo`nP\_MKTT$M
	]_#&[grG#\aR+tnFo<C7Mk.c]d<'',:fN"K-''DG\fn.*o%bDUecgZW7K+dJ*A(:+.o:8?7DlVN*>j`J
	6KJ+W''qr@_X[?Ep3?[FaCGAa2VC+m`GXkA.u@.WuoXf4.spf&D;;/m(22:tqW3gCo1imB[[)\[c.`R"
	)!C''09YkU[3<I[HS4gCZjDlD&_AN2=i8LhYLq)We+lM`1;]:n]^A!Y5Mf[1l2U.)E^J6E$LaAMo$<K^
	o.FR3G17J&+@5G_D%]o4l,HOHc6g+JNJY4g<sf&ZgI''"QW#!Tc2epG2P]I#W[=%o2Rrr`m''`-RA?GX
	R''2S*M`/%Ut$DBCkG+Z3GrTJ$Xf*uG&D/a*nFRRC#AQBPXXp9&ijuGlQlpra;S%^NkCk/M=?8k_[&!b
	hu2V-f%/U\ikp]b=J\4/PbT-:#4L''KWtK?6q7a?m@EkbmN4OH\fDWhVdpF64&ki$2P?O`jsu7"TF5#1
	8FXS*CXo-7C-@NsE+WPtN2<KaWGb*"ottBCo-Lm+;M9D;B^aoI7fV#-:m1D4fPA`m&?ca>Wb;"SNrcNV
	\n18/o7Q4uf.2<7Z#WB`%`S\5;[s!Jr3L+/X3+=5LA_hr)*$Zt[.Vg5n\/JNK]djcb[Y6o>_l:=JSi,X
	@E-Qu5+"\nYqJiY<oBQi_89K0"a*#r%URE!f&rbT)u(=MZi(/s,&u\o.Q\K_O''SoV\UT''BGT9G:pm&
	_N21^FAOCM%5S"pm6bEOD](%[&>q3NL4C''Fi=s$>)4DKtSP\JG-L)E:jfQ//\OgkEn6e@e1M7g[I,"@
	3+OoOJ-C^Bc`A.Vh?._7!_h)ei(mLkBL3="P,''I7-ZeKNo#.9nKXc5edg''4irS"Hpa]u^`96HLP.+I
	X:_F3nP^![s2?ff*&LK>@^\,1942/@KWofe"B[0-SI0q(5NXM''S?:rKabH8WWi0(@0C`#<%Sh]f[IO)
	]_,N@X[5)=Rul&)um"+p*7;^oD$jF;8+#/1DJDVbJd)GfWm*<26\ptb:0bUCT^%-YFVHWk%LVCYEIY?h
	.*QZ]@Dd.[i2*!n=!8BVM%>Y<(9b7q!$Aj$[E<>c8YFFOR"^pS_V/o*#gHAB<)pZ/b>dmSE?<@>g$0]L
	TT%+S?TIF0%gS\:M\"+/+Hcu6qtplJAO#-PKBbP@i;/jM!-j7=MN]TfUoA<3''$0-e0J,k8Dk)ooX0p^
	RQ!#H$qnZYh;2ulef\#DX;KPOfk/M3g76nr3TZ0g^[WlaLKisOpRT/_MF0dZ,]a(QoLQWDICV@VZ;+J?
	>gmsd7\rOa#e2Vam]?JA2m%b@Z,<qFMN@#\o(<BrTW;l[.#HK]S'';,gF-G^-Y0WVAj_`U1mTOZ<_m9-
	6l3cQ12auS@/#1/+(r?rq&X9e=>>Wj!D4)ON?su@7b)LJD"GVljK!1&1fW%&r4C2(T=m!6[bqT_:&!IK
	"_7)+:^YN(n/[R5]asRf/*I;F&.A!GS<>*K5l,<Trjp60FPr2`IQ:`qM94OfYCEL%`g_@#\]IZ.5hKli
	XG$@IiLgdRUU`ditG\#T!E3RD?QKK$U^c<r&bdeO^QRR>f1;/4;VHi\]P,u[EQn2.Bb]27*>@_''c969
	.\@0)haFrM!(6oXA%MjUW6V[I^lXQ/]5M\8gopY=#^bSb5`(7.G8H+=uCEY*ei^0WfKD/[T;GJ-"4^O3
	N7U$du$ZfQnaD90B/A.rdjQB6LXk\JN0!lh0>+"s&*"B),dI:6SV3@[hAcnrkC\AiO?S22Il"(t/oq]f
	lb7:nR:IXJU=,YBHb!gqp$&!V6\N%"cNW-5k!Y3S<IF<&+W''iZM9o9I"(g#K4o="n@2DX^%7CD\>lAk
	XMX@bY:8X#6jlM<REf''W\bE54TZW+($QO"ERM,-l8s9Ur;*!IC\C-qUC0;=^>_D$&,DE"fFAXXj(Z$$
	?%eDW%a13rC=CLmUN9&(:k9"S^+%IrB\s;@cBXLcn`X.5S@LNBNecGQe0,[c,c`e75CRbL=83DQU9#s`
	j(fH=)M5e^'',esX>!ZYlmG)U@8_$DX8_X>Fi<"]SCZUjIZstD19!+KMW\F$@;-gd6Z7Nm5Gq1Y1or+H
	".QkN_BOXk;g#9;\Y#jh:%aRkp5Gh_pm1MBgMUfFKf2.V8EL,pd8r_p8KA6H0i9QnEQ.J$Wqa.fY[.#l
	<p2&t"KJ[(I"DU#;]8?>G,Pg+03f8kdkkE_i1`mt3I[kZ88p!B&[<_i0V&9GPk,$q!f"7flmT*oDFda8
	k''q%d(jfSQ7_i\=''jB,8LN_@0+;tS`:q6q&WsV+[Wngu\AJ1pmQRG8[lWN\SV7_"/hD<%q.9HtT<9?
	N5AG2iR>aCc0<7OTh=iE6I7%.pjjBE*$D:*WsMmCl/\Z@n#:H5Lg;O(OZCSJoQd$!*"jO''2$\B8@dHc
	"HG[UM''\#c+2d1trog!OF,i=+Ji^&\L5rPK6@tW4o7rI4/0nSE(I@_/>?I[bK''P(CDnaM74``.,$>:
	kGSM56Jmrk.*pb<*;8sn5)RP4UebI.T!V$"O110i_O05QcXJJE63PD28/D.H%c"6sQP>+8ZZCh:DkU:=
	I;=cU7YNh_<(hiLFK?MADW/-Rs23j!Mb-lp$Xu`,(-Rs;Lr3]n2BZYk%d"!;''I6I6hjp;b.CsYmR((f
	Z:uiT.ZWO5/[.31iD0n_Pk>6iIp4BNjo<6J6T4TlJ^gP+nEAe+gPnW?L=Eshje)L1%YZnYZoT`?/''6t
	^c6af9+Y,nWI1>G9W2X.#cQ07NcTX(.6m.-@!>AmK=@pBf,q^R-hTg$),P4(6GkYBnRhZ:`tn*Lcen[&
	.0-^*KSd&ZdQO[PTQ:Kp]%%>2,fMbl=_5(OHM\LI)K,kAge;mCRL&(M=;%^Kpk5_e>0+9MoI@.XVBZLb
	_RmSfiN`FDDlQ*6&I!$>9jKH\ip;piR`cD/SR^eP[&iq-eI[g/jV(9m&&G>@t4%\g-Ld#NtR.mape:HS
	rZpbCD?KkBA.(`[iiSBU+`>d/TLpq&OeM)TXfA,fYAqr-?\bln$HkGU(I7Ge+G2NRqFVbch<0i`((_/f
	U8k6T//KV.9-8`i&5WJ-pt4:''$35K1Ub*/3!cB#06c"o]:]gf$Iu+!e?K?9.b]an\r<;4W&frmuXbLX
	Z5hR\Zc.!W\YK-Cg@a"R1AoOIS"_eS5[Z^1a)Q&s[QfYN._8+NjjB"d[\4Rd`otceRmmG\[k$;]OFM5c
	];-*PqJ6Fo^`3:0a%uR6Ho5''>c%-`6,_>;WXt-qaO7C5;)s2l-E4[b4nimkK=.;Hh5#cOp+h&pct%dZ
	Ej0ab2is/7//DC5!G.O(#+H+SGXpS&U"Y-Bbro4O(DWcNY6<kZUg3UOLb/aUcJ89,lfm6oN]"I$\B^n:
	pLXEa>^T%nCD:p#Rm-Y4:=du5K.9,PAj#aik9CIO<Y<&>3<GNf`dNEejlP:0^m1l(''g%i.B\0\1)g:"
	7BqTN":o?M5mZ-o''eUnT-Nuh43Vem*2r^(!a7uL01)+3>lpDXS4>*4H`4PsJ_U%dSMQbqe#3)hD1R0*
	pY)/g,0-?Q*?%TEk1X6g^U1JI3[bFiE::Q)qi]RAt5J[0f4Wpa=AjuEB1(#-n\nh[<pBqSK(u!D9pQt4
	\q''@a''m$[WlcS$*,(1Qd)ffQgW!%qn/U.]pg!0k(5rqNR>)MfiCfIZ^_q[^sYqpOAKT_&h)ha>#ep>
	71-kW^EC%X[-p>lWCV1@mM3T@M[:''u*/j.7IMfI6/=[1FEC1$GJSLPY-!9$?$Y%N,U+)Y^UmuWTPPY%
	tO:/''Dhsf%g#096]g$Z"`#6&F:!</&5V>1(3eXHgMmrdL^-<a*5%n.>o"=ipP]!!(Tp$hK(D.Mj;e3?
	S_uK>%0,U_j%g''[BoFI%H:sHQoMUqT0PEr<!<,qe:Lhd=::DD9QJr8ap8%$R"FSUIf+lslU$D^kOcZ8
	E9kZeT+B;GR#li(nQV:^=pORIq5BKBY;FeU-/(,a=Iecc-!N"N*KqS2t*`nXh_Ki/4i@sbD/M!>3,aI#
	"[q)gu\4Zt=#Y[C(6p@sJR?rP=?NpcBN%1N6/6Z,&&S--ZC5Hp:?hGI"ns!G.Jle6Q(Sk8XR0Skq9H=R
	Ecp).m?QJ5Z<os0=U)$h@%>qW*PJ/UR''GpsDLF]]K>rlrd`F*H$#.[AE(;?fmDfm1#T6j$D?T"6#I;/
	foRh2W?$''j4iJTe#[e=*R-[0Cb''%2#f8&DM*3%/_QDDhiq>c$[&!*.g0>"Qc(Jic_rQS#Oql>jN3t+
	8gC`QR:^]eWfgnftnD9<8>+aHu:/\%CSJ4rjtbmI9u=SjmjbX+FqCOij[!cIh7E"g[2EL67$^ZrZD,kO
	EUeXG-_f3MS1_"qqSmGSj,\]pGPISNA)OE.f[Z+r9`p-mXOF.?[2TP55(8F]^rD4^&MR]\GR(DrQ6?,q
	.=jN^#W+c/k8%<rL@a9D"r?)\psObaM$X;+l1u8"L]E,KO=Je-b<+/Ij4l=?b;s.iEkFU!!9W<n2<Rfr
	qj9&]^M7=+8i)ss8''u!^],P9s8E8[r,3\PNurT[IWDk_bP:sqs6N7)lDOXY+2%<RpQ-c,Eta[Q0YBQ!
	n?$''c54O3MJ*@3am/,uWY''b0F^)*uckJDQuCdS#eA''\DinE>SPJ*G+ZI/;aUF5hU77/4^=2AG!P0Z
	XDJP2Ee?i\TK(E94qm(\V.Fo&>oBa.Q]tG%"mY(QNnXIXh$aA'']Ppo7-WJhFl^nh%ie(U\9W?h#"nq?
	iRrtIXh^gb''Q=cn(>>rh0/mK)`p3kmk6oth="Z0p6Wg+gjeEu:YbungG\*HWNj5./b6^grS0X5WAh*\
	l''m2#k25u5q#9NI)a4.j&,tg#s7HWsp[<Lo[D,!-#eRMec##h0d;o*e+lE;(q1g)dJr!+35u?F&p8r8
	Tl!OAJ09Z4hr3bqTF#u7Co:pe_0DXiG&&&#An,M$%msOL#(K$7&$/\6:p6Bhcp>0`DqsjQuoqj-URnmo
	DCoDEm]^Gu[^TZDrD6mBMY/,H[O:TQmD1[-s6(nEKRpZ0UM*Ki8DoHF2h]I^\[OoV1&RQq#.cdIo.aHQ
	B[u"r4q&ps!o$(KnCqHej"!L^8R+Ubg?`9]J*!9-N-FT0r[k0RW[Y>9cE2"hjrFp002SgKm$;Z-Q?\aL
	*a)N''(LKlqO''R<@4+.>IO?t)22dWX@HSEb:PDVK$?D7Ao@h6u'']gMYb_rS.>WLF=B5~>')
%
classmethod: R4PReport
imageTruncatedGif
	^(ByteArray fromPackedString: 'Q4%FNC%!C@@L@O\@@FA XHB@ HNC ;>?/</K2<;N3,?O3=KR4/[6=/3<?O???0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@B0@@@@@C@@L@@@HQ @UJC! PJAA PTG@@@08JA@@@XVK"2HT@ED"QLUQKR(PBGF@@,9Z,P(TZPB@"PA&EP  JQHAAE]*$1PHJUJ%P8]W+P)LR@@N0@a')
%
classmethod: R4PReport
imageTruncatedGifXImage
	^(PDF classAt: #ImageXObject)
		on: ((Valuemap new: 8)
			add: #Subtype -> #Image;
			add: #Type -> #XObject;
			add: #Width -> 12;
			add: #Height -> 12;
			add: #BitsPerComponent -> 8;
			add: #ColorSpace -> ((PDF classAt: #Indexed)
				base: #DeviceGray
				number: 255
				bytes: (ByteArray fromASCII85String: '@"<mEbKeG^pAFpjzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz~>'));
			add: #Length -> 62;
			add: #Filter -> #FlateDecode;
			yourself)
		externalBytes: #[120 218 77 140 65 14 0 33 12 2 113 85 84 254 255 96 219 37 26 231 52 5 82 105 80 134 170 64 181 131 8 248 167 246 56 8 33 55 65 65 186 220 216 213 30 215 119 125 242 230 171 191 251 3 206 151 100 3 201 78 2 120]
%
category: 'user guide text'
classmethod: R4PReport
userGuide1

	^'+Report4PDF User Guide

PDF4Smalltalk  is an implementation of the PDF specification (ISO standard PDF 32000-1:2008) in VisualWorks Smalltalk. Report4PDF generates output for PDF4Smalltalk  using layout and content information, coding in a style influenced by Seaside. This is intended to be a programmer''s tool for building PDF content; it is not an end user report tool.

Development of Report4PDF was driven by need to provide reports from Seaside applications. Using a generated PDF is a good fit, since it is seemless to serve a PDF document to a web browser.

This document lists the layout methods, as well as examples. All examples are coded in R4PReportTest under "examples". These examples are used to generated SUnit test which check for consistent output (see R4PReportTest>>create* methods under "actions''). A separate technical guide deals with the design and implementation. 

A reports consists of a report definition, containing pages, with each page holding content. Content consistens of text, tables, lines and images, each with some layout information. Layout parameters are used to position and configure the display of content. 

:Layout components
*report - starting point with default layout parameters
*page - a logical page of content that can be split over many output pages
*section - an area of the report which can contain other sections
*text - string content
*table - a layout structure with rows and columns
*row - one row of a table
*cell - one cell of a row
*image - a JPG, PNG or GIF image file
*line - a line from anywhere to anywhere on the page, typically used for a horizontal line as a separator

Each page has a collection of header, body and footer layouts. Headers and footers repeat on each output page, with optional include and exclude blocks (e.g. include: [:page | page > 1]). Body layout sections will span printed pages if required.

>imageLayoutGif

Sections inherit their layout properties from their parent section, with the report providing default values. By default, a nested section is the same width as its parent, with its margin and padding calculated as an offset from the parent layout section. Section, text and table cell layouts can contain any other layout. Nested tables can be used as a handy layout technique.

Sections are positioned sequentially on the page, or can have a fixed layout. Fixed layouts can be positioned anywhere on the page and are not included in the sequential layout position calculations. Layouts nested in fixed layout sections use the fixed layout parent as their content boundary, wheres variables layout parent section calculate their size from their nested section''s size.

>imageLayoutspacingGif

Tables allow content to be positioned in organized rows and columns. They can be used for column based content, or to define a complex page layout, much like HTML tables. Each part of a table is a layout section, so all the layout attributes, like margin and border, apply. Column width is calculated for each table column from the vertical collection of table cells. Since table cells are just another layout section, they can contain any other layout, like a nested table or image.

>imageTableGif

Table row height is calculated from the tallest cell. After a page break, cells that do not have content that is "tall" enough still use their horizontal space. Table headers can be optionally repeated on page breaks.

>imageTableCellGif

If text cannot fit into a constrained space, Report4PDF will render an "truncated" arrow in the lower right corner of the parent section. This can happen in table content that does not allow for wrapped text.

>imageTruncatedGif

Images can be sourced from either a file or from a byte array, which is handy when an application stores image content directly (like Seaside''s WAFileLibrary). Only 8 bit grayscale images are supported This user guide''s images are stored as ByteArray methods on the class side of R4PReport. 
-
'
%
classmethod: R4PReport
userGuide2SimpleExamples

	^'!Layout method patterns

Layout sections are added to the report by sending section creation messages to existing sections, starting with a new report instance. For example, to create a new page, with one line of text, you can code...

@	report page string: ''Hello world!''.
@	report page text string: ''Hello world!''.
@	report page text: [:text | text string: ''Hello world!''].

...all of which generate the same output...

>imageStringexample1Gif

If the text needs some formatting, like using a different font that is large and bold, you can code...

@	report page text; courier; bold; large; string: ''Hello world!''.

Since each keyword method returns the layout section it was sent to, you could code without the semicolons... 

@	report page text courier bold large string: ''Hello world!''.

...which is really...

@	((((report page text) courier) bold) large) string: ''Hello world!''.

>imageStringexample2Gif

Sometimes a section will need to have several nested layout sections added. As a convenience, all layouts can be added with a single argument block, with the new layout instance passed as the value. 

@	report page: [:page | 
@		page text courier; bold; large; string: ''Hello world!''.
@		page text string: ''Goodbye''].

>imageStringexample3Gif

This will display a large ''Hello world!'' on the first line, followed by ''Goodbye'' in the default font and size on the next line. If both lines should have the same formatting, we can set the font parameters on the page...

@	report page: [:page |
@		page courier; bold; large.
@		page text string: ''Hello world!''.
@		page text string: ''Goodbye''].

>imageStringexample4Gif

This will display a large ''Hello world!'' on the first line, followed by ''Goodbye'' in Times Roman on the same line. Strings can be concatenated to a text layout with different of fonts...

@	report page: [:page |
@		page courier; bold; large.
@		page text: [:text | 
@			text string: ''Hello world! ''.
@			text string timesRoman; string: ''Goodbye'']].

>imageStringexample5Gif

If the font settings need to apply to the entire report, we can use...

@	report courier; bold; large. "set font parameters here instead of the page"
@	report page: [:page |
@		page text string: ''Hello world!''.
@		page text string: ''Goodbye''].

In practice, report code would be part of some other Smalltalk application, so the layout sections would be passed as values to other build methods, like... 

@	report portrait.
@	page := report page.
@	self buildReportHeader: page.
@	self buildReportContent: page.
@	self buildReportFooter: page.
-
'
%
classmethod: R4PReport
userGuide3LayoutMethods

	^'
!Layout Section Methods

:positioning
*align: - horizontal position within a section. Valid values are: #left (default), #center, #right. Methods are available for each value.
*left
*center
*right

*verticalAlign: - vertical position within a section. Valid values are: #top, #middle, #bottom. Methods are available for each value.
*top
*middle
*bottom

*noWrap - do not allow the strings in the nested sections to wrap. If there is not enough room for the strings, a "truncated content" arrow will be displayed in the lower right corner of the content.

:fonts
*bold - use the bold version of the current font
*italic - use the italic version of the current font
*boldItalic - use the bold + italic version of the current font
*font: - set the font, default is #Helvetica. Methods are provided for the default fonts: courier, helvetica, timesRoman, zapfDingbats

*fontSize: - size of the font. Default is 10. Methods are provided for larger and smaller fonts. When used with bulleted text, these keywords apply to the bullet size.
*veryLarge - font size = 14
*large - font size = 12
*medium - font size = 10 (default)
*small - font size = 8
*verySmall - font size = 6
Note that the numbers do not have to be integers. A font size of 12.5 works fine.

:layout
*margin: - space between the edges of the page or parent section, and the content box. It can be entered as a single numbers, in which case all four margins (top, right, bottom, left) will be set to the same number, or it can be entered as an array, like #(10 15 5 20), corresponding to the top, right, bottom and left. Convenience methods are available for each edge, which override the previous setting.
*marginTop:
*marginRight:
*marginBottom:
*marginLeft:

*padding: - space between the content box and the content. It can be entered as a single numbers, in which case all four padding values (top, right, bottom, left) will be set to the same number, or it can be entered as an array, like #(10 15 5 20), corresponding to the top, right, bottom and left. Convenience methods are available for each edge, which override the previous setting.
*paddingTop:
*paddingRight:
*paddingBottom:
*paddingLeft:

*origin: - sets a fixed top and right value for the section, using a "x @ y" point 
*height: - sets the fixed height of the layout section in pixels 
*width: - sets the fixed width of the layout section in pixels
*widthPercent: - sets a fixed layout width as a percentage of the parent section or page

:section "add" methods
*bullet - add a bullet section
*bullet: - add a bullet section using a single argument block
*cr - add a text section with a single string section with a "new line" parameter
*horizontalLine - add a horizontal line with top and bottom margin of 4. The margin values can be changed (e.g. page horizontalLine marginTop: 10). 
*image - add an image section
*image: - add an image section using a single argument block 
*line - add a line section
*line: - add a line section using a single argument block
*section - add a layout section. Useful for positioning other sections and for wrapping other sections with borders.
*section - add a layout section using a single argument block
*string: - add a text layout with one string section, with the string value from the parameter
*table - add a table section
*table: - add a table section using a single argument block 
*text - add a text section
*text: - add a text section  using a single argument block 
 
+report
*pageHeight: - height of page in pixels, using 72 pixels per inch
*pageWidth: - width of page in pixels, using 72 pixels per inch
*A3Landscape - width = 1191, height = 842, margin =  36, 16.535" x 11.693"
*A3Portrait - width = 842, height = 1191, margin =  36, 11.693" x 16.535" 
*A4Landscape - width = 842, height = 595, margin =  36, 11.693" x 8.268"
*A4Portrait - width = 595, height = 842, margin =  36, 8.268" x 11.693"
*A5Landscape - width = 595, height = 420, margin =  36, 8.268" x 5.827"
*A5Portrait - width = 420, height = 595, margin =  36, 5.827" x 8.268"
*businessCard - width = 252, height = 144, margin =  0, 3.5" x 2"
*landscape - letterLandscape
*legalLandscape - width = 1008, height = 612, margin =  36, 14" x 8.5"
*legalPortrait - width = 612, height = 1008, margin =  36, 8.5" x 14"
*letterLandscape - width = 792, height = 612, margin =  36, 11" x 8.5"
*letterPortrait - width = 612, height = 792, margin =  36, 8.5" x 11"
*portrait - letterPortrait

+miscellaneous
*pageNumberPattern: - string to be replaced with page number, default is <page>
*pageTotalPattern: - string to be replaced with total number of output pages per report page, default is <total>
*reportTotalPattern: - string to be replaced with total number of pages for the report, default is <report>

For specific section types, all format parameters are supported. But some sections cannot be nested. For example, you cannot add an image section to a text section. Adding an invalid section raises an exception.

+text
*copyright - add a copyright character to the last string section; create an empty string section if needed
*trademark - add a trademark character to the last string section; create an empty string section if needed
*flip - flip text upsidedown; useful for creating folded report output
*leading: - set the leading of the text, which is the space between rows of strings. Default is half of the font descender. 
*string - add a new string section. Output will be concatenated with previous string sections.
*string: - add a new string section using a single argument block
*text - no action; answers "self"
*gray - set the text color to foreground: ''gray''
*red - set the text color to foreground: ''red'' 
*background: #(red blue green) - for example, red = #(0.75 0 0)
*foreground: #(red blue green)
*background: aSymbol - where aSymbol is the key in R4POutput>>colorNameTable which translates a named color to a RBG array.
*foreground: aSymbol - where aSymbol is the key in R4POutput>>colorNameTable which translates a named color to a RBG array.
Both background and foreground color can be set as a hex string, like #''339999'', which will be translated to RGB values. See R4PReportTest>>exampleBackgroundColorHexTable.

+string
*cr - add a new line (same as newLine)
*newLine - add a new line (same as cr)
*string: - set the string content of the section
A string section can only contain a string, with font parameters. No other section parameters are supported. 

+table
*header - add a row as the table header. It will be rendered after each page break.
*header: -  add a header row using a single argument block 
*row - add a row section

+row
*cell - add a cell section
*cell: - add a cell section using a single argument block
*cellBorder: - set the default border values for each cell in the row. Note that you can a different border setting on the row itself.
*cellPadding: - set the default padding for each cell (space between the border and the contents)
*cellSpacing: - set the default spacing for each cell (space between each cell)

+cell
A cell can contain any other layout section, including another table.

+line 
*start: - the start point of the line (x @ y)
*end: - the end point of the line (x @ y)
*lineWidth: - width of the line; default is 0.5  
*gray - set the line color to foreground: ''gray''
*red - set the line color to foreground: ''red'' 

+image
*filename: - the full path of the image file
*image: - the image from an image reader, which allows for images to be stored as byte arrays (e.g.  (JPEGImageReader new from: imageStream) image )
*scale: - sets a size adjustment for the image. Default is the image height * 0.75 (image are stored as 96 dpi, PDF default is 72 dpi, 72 / 96 = 0.75)
    Note that only 8 bit grayscale images are supported.
-
'
%
classmethod: R4PReport
userGuide4Design

	^'!Design
Report4PDF contains four main classes: R4PReport, R4PLayout, R4PBuilder and R4POutput. Basically, layout are added to a report, then a builder is used to arrange the layouts and create the output, which then uses PDF4Smalltalk to build the PDF document. 

The builder iterates over the pages in the report (see R4PBuider>>buildPagesX). An exception handler will write the exception information into the PDF output if an error occurs.  R4PBuider>>buildPageSections processes the layout sections on each page, with each section using double-dispatch to send the appropriate builder method (see implementors of #buildOutput:). 

For each layout section, the build process is very similar.

*check if the section fits
The height of a layout section is calculated prior to this step (see #calculateLayout) so given the current output page''s Y value a layout section can answer whether it fits or not. If it does not fit, a page break is triggered and all of the parent layout sections are notified. This allows each layout section to add content (like a table header) or adjust positioning (like a text top margin) on the next page.

*render the background and border
Rendering order is important. Each rendered output is placed on top of the previous one, so background first, border second, and text last.

*process nested sections
String, line and image layout sections are the only sections that add to the output (aside from backgrounds and borders; see methods in the ''output'' category). Parent sections pass the build sequence down to their nested sections until one of these ''output generating'' sections is reach. For a table, a typical sequence would be: table -> row -> cell -> text -> string.
-
A lot of refactoring involved generalizing this message sequence and removing layout specific code. The intent is to make extending this framework easy. There is one inelegant hack: the report is linked to the builder just to support #includeOnPage: and #excludeOnPage: ... all other message sends to the builder from sections layouts are done to a message argument, as in #buildOutput: 
-

Output from layout sections is added to an instance of R4POutputPage, which holds the current, minimum and maximum Y value. A page break adds a new output page. In some cases a previous output page needs to have more contented added, like the next cell of a row.  See R4PBuilder>>resetCurrentYAfter: .  

R4POutput subclasses are where Report4PDF interfaces with PDF4Smalltalk. See #renderOutput: 
For diagnostics, you can trace the output generated for PDF4Smalltalk using R4PReport>>traceToTranscript, #traceToFile: and #traceAppendTo:

>imageDesign1Gif

-
'
%
classmethod: R4PReport
userGuide5Note

	^'!Examples and SUnit Tests

R4PReportTest has 65 examples which were used to develop Report4PDF. For each example corresponding "output" and "pdf" methods are generated. These are snapshots of the example. "Output" is the report content prior to sending it Smalltalk4PDF, and the "pdf" methods are byte arrays of the generated PDF, minus the #CreationDate and #ModDate data. SUnit test methods are generated to check both the output and pdf content, and each method has, as comments, code to recreate the snapshot methods. 

Class side methods are available to create all the test, output and pdf methods.
SUnit code coverage is 60%, with most of the skipped code being exceptions and bad data branches.

:This User Guide
This document was created with Report4PDF using a trivial markup syntax. The first character of each line defines how that line is displayed, with a default of Helvitica size 10.  See the class methods of R4PReport under "documentation".

@	! heading
@	* bullet
@	: bold
@	+ bold and big
@	@ code examples, using Courier
@	- horizontal line
@	> image filename (file in aFolder)

To build the report, use...

@R4PReport>>buildUserGuide
@	" self buildUserGuide "
@
@	| writeStream readStream report |
@
@	report := self new.
@	report portrait; pageNumberPattern: ''<pageX>''; pageTotalPattern: ''<totalX>''.  " patterns changes so they can documented "
@	writeStream := WriteStream on: Core.String new.
@	writeStream 
@		nextPutAll: self userGuide1; 
@		nextPutAll: self userGuide2SimpleExamples; 
@		nextPutAll: self userGuide3LayoutMethods; 
@		nextPutAll: self userGuide4SUnit; 
@		nextPutAll: self userGuide5Note.
@	readStream := writeStream contents readStream.
@
@	self buildReport: report from: readStream.
@
@	report saveAndShowAs: ''Report4PDF User Guide.pdf''.

Each line of the readStream is iterated over. The first character is checked for a formatting code. A new report page is answered if ! is the code, otherwise the current page is used.

@R4PReport>>buildReport: aReport page: aPage line: aString
@
@	| tag string | 
@
@	aString isEmpty ifTrue: [aPage cr. ^aPage].
@
@	tag := aString first.
@	string := aString copyFrom: 2 to: aString size.
@	tag = $! ifTrue: [^self buildReport: aReport newPage: string].
@	tag = $> ifTrue: [^self buildImage: string page: aPage].
@	tag = $: ifTrue: [aPage text bold string: string. ^aPage].
@	tag = $+ ifTrue: [aPage text bold large string: string. ^aPage].
@	tag = $@ ifTrue: [aPage text marginLeft: 10; courier; small; string: string. ^aPage].
@	tag = $- ifTrue: [aPage horizontalLine. ^aPage].
@	tag = $* ifTrue: [aPage bullet string: string. ^aPage].
@
@	aPage string: aString.
@	^aPage 

-
Diagrams were created with Google Docs, exported as JPG files, then cropped and saved as GIFs with IrfanView, and stored as byte arrays (Seaside file methods are used to build the byte array methods).  
In this case, a GIF renders more cleanly than a JPG.

For support please use the forum at http://pdf4smalltalk.origo.ethz.ch/forum (due to change soon)
-
'
%
category: 'accessing'
method: R4PReport
builder
	^builder
%
method: R4PReport
builder: anObject
	builder := anObject
%
method: R4PReport
font
	^font
%
method: R4PReport
font: anObject
	font := anObject asSymbol
%
method: R4PReport
fontSize
	^fontSize
%
method: R4PReport
fontSize: anObject
	fontSize := anObject
%
method: R4PReport
margin
	^margin
%
method: R4PReport
margin: anObject

	margin := anObject asLayoutArray
%
method: R4PReport
marginBottom: anInteger

	self margin at: 3 put: anInteger
%
method: R4PReport
marginLeft: anInteger

	self margin at: 4 put: anInteger
%
method: R4PReport
marginRight: anInteger

	self margin at: 2 put: anInteger
%
method: R4PReport
marginTop: anInteger

	self margin at: 1 put: anInteger
%
method: R4PReport
origin

	^0 @ 0
%
method: R4PReport
pageHeight
	^pageHeight
%
method: R4PReport
pageHeight: anObject
	pageHeight := anObject
%
method: R4PReport
pageNumberPattern
	^pageNumberPattern
%
method: R4PReport
pageNumberPattern: anObject
	pageNumberPattern := anObject
%
method: R4PReport
pages
	^pages
%
method: R4PReport
pages: anObject
	pages := anObject
%
method: R4PReport
pageTotalPattern
	"has to be replaced after all the output is created"

	^pageTotalPattern
%
method: R4PReport
pageTotalPattern: anObject
	pageTotalPattern := anObject
%
method: R4PReport
pageWidth
	^pageWidth
%
method: R4PReport
pageWidth: anObject
	pageWidth := anObject
%
method: R4PReport
reportTotalPattern

	^reportTotalPattern
%
method: R4PReport
reportTotalPattern: anObject

	reportTotalPattern := anObject
%
method: R4PReport
traceFileStream
	^traceFileStream
%
method: R4PReport
traceFileStream: anObject
	traceFileStream := anObject
%
method: R4PReport
traceOption
	^traceOption
%
method: R4PReport
traceOption: anObject
	traceOption := anObject
%
category: 'actions'
method: R4PReport
buildNewPageParent: aBuilder
	"stubbed so that layouts can sent this message up the parent tree"

	^self
%
method: R4PReport
buildNextPageParent: aBuilder
	"stubbed so that layouts can sent this message up the parent tree"

	^self
%
method: R4PReport
buildNextPageParent: aBuilder do: aBlock
	"stubbed so that layouts can sent this message up the parent tree"

	aBlock value
%
method: R4PReport
buildPDF

	^R4PBuilder new buildReport: self
%
method: R4PReport
byteArraySUnitAs: aFilenameString
	"remove timestamps to get consistent content"

	| document |
	document := self buildPDF.
	document info at: #CreationDate put: nil.
	document info at: #ModDate put: nil.
	^document bytesForFile: aFilenameString
%
method: R4PReport
calculateLayout

	self pages do: [:each | each calculateLayout].
%
method: R4PReport
closeTraceFileStream

	self traceFileStream isNil ifTrue: [^self].
	self traceFileStream close.
%
method: R4PReport
end

	self closeTraceFileStream.
%
method: R4PReport
page

	| page | 

	page := R4PPage newForParent: self.
	self pages add: page.
	^page
%
method: R4PReport
page: aBlock

	aBlock value: self page
%
method: R4PReport
saveAndShowAs: aFilenameString
	"document may not be nil if created from SUnit test"

	self buildPDF saveAndShowAs: aFilenameString
%
method: R4PReport
saveAs: aFilename

	| document | 

	document := self buildPDF.
	document saveAs: aFilename
%
method: R4PReport
start

	self calculateLayout.
%
method: R4PReport
traceAppendTo: aFilename
	self traceFileStream: (GsFile openAppendOnServer: aFilename)
%
method: R4PReport
traceOutput: aString
	" cr prefix for Transcript in case there is already content "

	self isTraceToTranscript ifTrue: [
		Transcript cr; nextPutAll: aString].

	self isTraceToFile ifTrue: [
		self traceFileStream nextPutAll: aString; cr].
%
method: R4PReport
traceToFile: aFilename
	(GsFile existsOnServer: aFilename) ifTrue: [
		GsFile removeServerFile: aFilename].
	self traceFileStream: (GsFile openWriteOnServer: aFilename)
%
method: R4PReport
traceToTranscript 

	self traceOption: #transcript
%
category: 'calculate'
method: R4PReport
calculateLayoutLeftOfSection: aLayoutSection

	^0
%
method: R4PReport
calculateLayoutRightOfSection: aLayoutSection

	^self pageWidth
%
method: R4PReport
calculateLayoutTopOfSection: aLayoutSection

	^0
%
category: 'initialize-release'
method: R4PReport
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.

	pages := OrderedCollection new.
	pageHeight := 100.
	pageWidth := 200.
	font := #Helvetica . 
	fontSize := 10.
	margin := Array with: 0 with: 0 with: 0 with: 0.  "top right bottom left"
	pageNumberPattern := '<page>'.
	pageTotalPattern := '<total>'.
	reportTotalPattern := '<report>'.
	traceOption := nil.

	^self
%
category: 'layout'
method: R4PReport
contentBottom

	^self pageHeight
		- self marginBottom
%
method: R4PReport
contentLeft

	^self marginLeft
%
method: R4PReport
contentRight

	^self pageWidth
		- self marginRight
%
method: R4PReport
contentTop

	^self marginTop
%
method: R4PReport
contentWidth

	^self pageWidth
		- self marginLeft
		- self marginRight
%
method: R4PReport
layoutAlign 

	^#left
%
method: R4PReport
layoutBackground
	"no background color by default"

	^nil
%
method: R4PReport
layoutBorder

	^#(0 0 0 0)
%
method: R4PReport
layoutBorderWidth

	^#(1 1 1 1)
%
method: R4PReport
layoutFitOnPage

	^false
%
method: R4PReport
layoutFont

	^self font
%
method: R4PReport
layoutFontSize

	^self fontSize
%
method: R4PReport
layoutForeground
	"nil = black (default)"

	^nil
%
method: R4PReport
layoutNoWrap

	^false
%
method: R4PReport
layoutTruncated

	^false
%
method: R4PReport
layoutVerticalAlign

	^#top
%
method: R4PReport
marginBottom

	^self margin  at: 3
%
method: R4PReport
marginLeft 

	^self margin  at: 4
%
method: R4PReport
marginRight

	^self margin  at: 2
%
method: R4PReport
marginTop

	^self margin  at: 1
%
method: R4PReport
pageLayoutHeight

	^self contentBottom
%
method: R4PReport
totalMarginBottom

	^self margin at: 3
%
method: R4PReport
totalMarginLeft

	^self margin at: 4
%
method: R4PReport
totalMarginRight

	^self margin at: 2
%
method: R4PReport
totalMarginTop

	^self margin at: 1
%
method: R4PReport
truncated

	^false
%
category: 'page size'
method: R4PReport
A3Landscape
	"16.535 x 11.693"

	self pageWidth: 1191.
	self pageHeight: 842.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
A3Portrait
	"11.693 x 16.535"

	self pageWidth: 842.
	self pageHeight: 1191.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
A4Landscape
	"11.693 x 8.268"

	self pageWidth: 842.
	self pageHeight: 595.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
A4Portrait
	"8.268 x 11.693"

	self pageWidth: 595.
	self pageHeight: 842.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
A5Landscape
	"8.268 x 5.827"

	self pageWidth: 595.
	self pageHeight: 420.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
A5Portrait
	"5.827 x 8.268"

	self pageWidth: 420.
	self pageHeight: 595.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
businessCard
	"3.5 x 2"

	self pageWidth: 252.
	self pageHeight: 144.
%
method: R4PReport
landscape
	
	^self letterLandscape
%
method: R4PReport
legalLandscape
	"14 x 8.5 ... 0.5 inch margin"

	self pageWidth: 1008.
	self pageHeight: 612.
	self margin: 36.
%
method: R4PReport
legalPortrait
	"8.5 x 14"

	self pageWidth: 612.
	self pageHeight: 1008.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
letterLandscape
	"11 x 8.5"

	self pageWidth: 792.
	self pageHeight: 612.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
letterPortrait
	"8.5 x 11"

	self pageWidth: 612.
	self pageHeight: 792.
	self margin: 36.  "0.5 inch"
%
method: R4PReport
portrait
	
	^self letterPortrait
%
category: 'printing'
method: R4PReport
printOn: aStream

	super printOn: aStream.

	aStream 
		space; nextPutAll: self pageWidth printString;
		space; nextPutAll: ' x '; nextPutAll: self pageHeight printString;
		space; space; nextPutAll: self margin printString
%
method: R4PReport
printOutput
	
	| stream | 

	stream := WriteStream on: self stringClass new.
	self printOutputOn: stream.
	^stream contents
%
method: R4PReport
printOutputOn: aStream

	aStream nextPutAll: 'Report'; 
		cr; nextPutAll: 'page width: '; nextPutAll: self pageWidth printString;
		cr; nextPutAll: 'page height: '; nextPutAll: self pageHeight printString;
		cr; nextPutAll: 'margin: '; nextPutAll: self margin printString; 
		cr; nextPutAll: 'layout:'; 
			space; nextPutAll: self contentTop printString; 
			space; nextPutAll: self contentRight printString; 
			space; nextPutAll: self contentBottom printString; 
			space; nextPutAll: self contentLeft printString; 
		cr; nextPutAll: 'font: '; nextPutAll: self font printString; 
		tab; nextPutAll: 'font size: '; nextPutAll: self fontSize printString; 
		cr; nextPutAll: 'page number pattern: '; nextPutAll: self pageNumberPattern printString; 
		cr; nextPutAll: 'page total pattern: '; nextPutAll: self pageTotalPattern printString; 
		cr; nextPutAll: 'report total pattern: '; nextPutAll: self reportTotalPattern printString; 
		cr; nextPutAll: 'layout pages: '; nextPutAll: self pages size printString; 
		yourself.

	self printOutputPagesOn: aStream.
%
method: R4PReport
printOutputPagesOn: aStream

	self builder ifNil: [^self].

	self builder outputPages do: [:each | 
		aStream cr.
		each printOutputOn: aStream]
%
category: 'testing'
method: R4PReport
hasDifferentFont

	^false
%
method: R4PReport
isNestedFixedLayout

	^false
%
method: R4PReport
isTraceToFile

	^self traceFileStream notNil
%
method: R4PReport
isTraceToTranscript

	^self traceOption = #transcript
%
category: 'accessing'
method: R4PRow
cellAt: anIndex
	"previous cells with columnSpan > 1 will affect the answer"

	| count | 

	self sections isEmpty ifTrue: [^nil].

	count := 1.
	self sections do: [:eachCell | 
		count = anIndex ifTrue: [^eachCell].
		count := count + eachCell columnSpan].

	^nil
%
method: R4PRow
cellBorder
	^cellBorder
%
method: R4PRow
cellBorder: anObject
	cellBorder := anObject
%
method: R4PRow
cellPadding
	^cellPadding
%
method: R4PRow
cellPadding: anObject

	cellPadding := anObject asLayoutArray copy
%
method: R4PRow
cellSpacing
	^cellSpacing
%
method: R4PRow
cellSpacing: anObject
	"copy to allow for #(0 0 0 0) which could get a #at:put: "

	cellSpacing := anObject asLayoutArray copy
%
method: R4PRow
indexOfCell: aCell

	^self sections indexOf: aCell
%
method: R4PRow
lastRowOf: aCell
	"only applies to rowSpan cells"

	^self parent lastRowOf: aCell row: self
%
method: R4PRow
maxOutputPage
	"used to hold the outputPage on which this row's cells wrote the last output... i.e. max Y for the row"

	^maxOutputPage
%
method: R4PRow
maxOutputPage: anObject

	maxOutputPage := anObject
%
method: R4PRow
setMaxOutputPage: anOutputPage

	(self maxOutputPage isNil or: [
		(anOutputPage outputPageIndex) > (self maxOutputPage outputPageIndex)]) ifTrue: [
			self maxOutputPage: anOutputPage]
%
category: 'actions - add'
method: R4PRow
cell

	| cell | 

	cell := R4PCell newForParent: self.
	cell margin: self layoutCellSpacing.
	cell padding: self layoutCellPadding.
	cell border: self layoutCellBorder.
	self sections add: cell.
	^cell
%
method: R4PRow
cell: aBlock

	aBlock value: self cell
%
method: R4PRow
image

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
method: R4PRow
image: aBlock

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
method: R4PRow
line

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
method: R4PRow
line: aBlock

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
method: R4PRow
string: aBlock

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
method: R4PRow
text

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
method: R4PRow
text: aBlock

	^self error: 'Cannot add image layout sections to a row. Only cells.'.
%
category: 'build - layout'
method: R4PRow
buildOutput: aBuilder

	aBuilder buildRow: self
%
method: R4PRow
setCellHeight

	| cellHeight | 

	cellHeight :=  self layoutCellHeight.
	self sections do: [:each | each cellHeight: cellHeight]
%
category: 'calculate'
method: R4PRow
calculateRowSpan

	self sections do: [:each | each calculateRowSpan]
%
method: R4PRow
calculateRowSpanCell: aCell

	self parent calculateRowSpanRow: self cell: aCell
%
method: R4PRow
insertEmptyCellAt: anIndex
	"used when a cell has a rowSpan that affects this row ... spacing is needed, but not the border"

	| cell | 

	cell := R4PCell newForParent: self.
	cell margin: self layoutCellSpacing.
	cell padding: self layoutCellPadding.

	self sections size < anIndex ifTrue: [
		^self sections size + 1 to: anIndex do: [:index | 
			self sections add: cell copy]].

	self sections add: cell beforeIndex: anIndex
%
category: 'layout'
method: R4PRow
cellPaddingBottom: aNumber

	| array | 
	
	array := self cellPadding ifNil: [self parent cellPadding copy].
	array at: 3 put: aNumber.
	self cellPadding: array.
	^aNumber
%
method: R4PRow
cellPaddingLeft: aNumber

	| array | 
	
	array := self cellPadding ifNil: [self parent cellPadding copy].
	array at: 4 put: aNumber.
	self cellPadding: array.
	^aNumber
%
method: R4PRow
cellPaddingRight: aNumber

	| array | 
	
	array := self cellPadding ifNil: [self parent cellPadding copy].
	array at: 2 put: aNumber.
	self cellPadding: array.
	^aNumber
%
method: R4PRow
cellPaddingTop: aNumber

	| array | 
	
	array := self cellPadding ifNil: [self parent cellPadding copy].
	array at: 1 put: aNumber.
	self cellPadding: array.
	^aNumber
%
method: R4PRow
contentLeftOf: aCell

	| total | 

	total := self contentLeft.
	self layoutSections do: [:each | 
		each = aCell ifTrue: [^total].
		total := total + each columnWidth].

	^total
%
method: R4PRow
contentRightOf: aCell
	"for cells that span columns"

	^self parent contentRightOf: aCell
%
method: R4PRow
lastRowBottomOf: aCell
	"for cells that span rows"

	^self parent lastRowBottomOf: aCell row: self
%
method: R4PRow
layoutCellBorder

	^(self cellBorder ifNil: [self parent cellBorder]) copy
%
method: R4PRow
layoutCellHeight
	"excluded row spacingTop and spacingBottom"

	self hasFixedHeight ifTrue: [^self fixedHeight].
	^self nestedLayoutHeight
%
method: R4PRow
layoutCellPadding

	^(self cellPadding ifNil: [self parent cellPadding]) copy
%
method: R4PRow
layoutCellSpacing

	^(self cellSpacing ifNil: [self parent cellSpacing]) copy
%
method: R4PRow
layoutHeight

	self hasFixedHeight ifTrue: [^self fixedHeight].
	^self layoutHeightCalculated
%
method: R4PRow
layoutRepeatHeading

	^self parent repeatHeadingSet
%
method: R4PRow
layoutRightOf: aCell
	"for cells that span columns"

	^self parent layoutRightOf: aCell
%
method: R4PRow
nestedLayoutHeight

	| maxHeight | 

	maxHeight := self fontHeight.
	self layoutSections do: [:each | 
		each isSingleRow ifTrue: [
			maxHeight := maxHeight max: each layoutHeight]].
	^maxHeight
%
method: R4PRow
nestedLayoutHeightNotBuilt

	| maxHeight | 

	maxHeight := self fontHeight.
	self layoutSections do: [:each | 
		each built ifFalse: [
			each isSingleRow ifTrue: [
				maxHeight := maxHeight max: each layoutHeight]]].
	^maxHeight
%
method: R4PRow
numberOfColumns
	"includes columnSpan"

	| count |

	count := 0.
	self sections do: [:each |
		count := count + each columnSpan].
	^count
%
method: R4PRow
rowSpacingBottom

	^self nestedLayoutHeight 
		+ self spacingBottom
%
method: R4PRow
tableWidth

	^self parent tableWidth
%
method: R4PRow
totalRowHeightOf: aCell  

	^self parent totalRowHeightOf: aCell row: self
%
category: 'printing'
method: R4PRow
displayRowIndex

	self parent isNil ifTrue: [^'<no table>'].
	self isHeaderRow ifTrue: [^'<header row>'].

	^(self parent sections indexOf: self) printString
%
method: R4PRow
printLayoutOn: aStream

	super printLayoutOn: aStream. 
	aStream cr; tab; nextPutAll: 'row: '; nextPutAll: self displayRowIndex.
%
category: 'testing'
method: R4PRow
canBuildAt: aNumber limit: aMaximumY

	((aNumber + self spacingTop) roundTo: 0.01) >  (aMaximumY roundTo: 0.01) ifTrue: [^false].
	self sections isEmpty ifTrue: [^true].
	(self canFitOnPageAt: aNumber limit: aMaximumY) ifFalse: [^false].

	^self sections allSatisfy: [:each | 
		each 
			canBuildAt: aNumber + self spacingTop
			limit: aMaximumY]
%
method: R4PRow
canEndAtY: aNumber limit: aMaximumY

	^((aNumber + self layoutHeight) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
method: R4PRow
isHeaderRow

	^self parent headerRow == self
%
category: 'actions'
method: R4PSection
buildOutput: aBuilder

	aBuilder buildSection: self
%
category: 'initialize-release'
method: R4PSection
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.
	" *** Replace this comment with the appropriate initialization code *** "
	^self
%
category: 'testing'
method: R4PSection
canBuildAt: aNumber limit: aMaximumY

	(self canFitOnPageAt: aNumber limit: aMaximumY) ifFalse: [^false].

	^((aNumber + self spacingTop) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
category: 'instance creation'
classmethod: R4PString
newForParent: aParent string: aString leading: aNumber

	^(self newForParent: aParent)
		string: aString; 
		marginTop: aNumber / 2.0; 
		marginBottom: aNumber / 2.0; 
		yourself
%
classmethod: R4PString
newForParent: aParent string: aString leading: aNumber font: aFont

	^(self newForParent: aParent)
		string: aString; 
		marginTop: aNumber / 2.0; 
		marginBottom: aNumber / 2.0; 
		font: aFont; 
		yourself
%
category: 'accessing'
method: R4PString
endWithLineBreak
	" If this string has a line break, and we end up word wrapping it, we want the last string to have a line break (but the ones in between)"

	^endWithLineBreak
%
method: R4PString
endWithLineBreak: anObject
	endWithLineBreak := anObject
%
method: R4PString
flip

	^self parent flip
%
method: R4PString
fontHeightAdjustment
	"Y value is the top of the font but output rendering requires a Y value for the bottom ... 
	fonts like ZapfDingbats use the FontBBox and  individual glyph bounding boxes.
	If there is a mix of fonts and font heights in a text, use the maximum"
	
	self parent maxFontHeight > 0 ifTrue: [
		self parent maxFontHeight ~= self fontAscender ifTrue: [
			^self parent maxFontHeight + (self fontDescender / 2.0)]].

	self pdfFont ascender = 0 ifTrue: [^self fontBoxHeight].
	^self fontAscender  + (self fontDescender / 2.0)
%
method: R4PString
lineBreak
	"If true, append a <cr> to the end of this line"

	^lineBreak
%
method: R4PString
lineBreak: anObject
	lineBreak := anObject
%
method: R4PString
marginBottomAdd: aNumber

	self marginBottom: (self marginBottom + aNumber)
%
method: R4PString
marginTopAdd: aNumber

	self marginTop: (self marginTop + aNumber)
%
method: R4PString
positionX
	^positionX
%
method: R4PString
positionX: anObject
	positionX := anObject
%
method: R4PString
string
	^string
%
method: R4PString
string: aString
	string := aString asString
%
category: 'actions - add'
method: R4PString
cr

	self newLine
%
method: R4PString
image

	^self error: 'Images cannot be added to a string layout. Only strings can be added to a text layout'.
%
method: R4PString
line

	^self error: 'Lines cannot be added to a string layout. Only strings can be added to a text layout'.
%
method: R4PString
newLine

	self lineBreak: true
%
method: R4PString
section

	^self error: 'Sections cannot be added to a string layout. Only strings can be added to a text layout'.
%
method: R4PString
table

	^self error: 'Tables cannot be added to a string layout. Only strings can be added to a text layout'.
%
category: 'calculate'
method: R4PString
buildTruncatedString: anOutputWidth
	" Answer how much width list left for the next string section ... we assume that this string is used in a 'no wrap', 
	so remove all the cr"

	| stringWidth endIndex lineString | 

	lineString := self string copyWithout: [Character cr].
	lineString := lineString copyWithout: [Character lf].

	endIndex := lineString size.
	stringWidth := self stringWidthOf: lineString. 
	(stringWidth < anOutputWidth) ifTrue: [
		self string: lineString.
		^anOutputWidth - stringWidth].

	[stringWidth > anOutputWidth and: [endIndex > 2]] whileTrue: [
		endIndex := endIndex - 1.
		lineString := lineString copyFrom: 1 to: endIndex.
		stringWidth := self stringWidthOf: lineString]. 

	self string: lineString.
	^0
%
method: R4PString
calculateNextLineAt: aPosition
	"Answer X position, to be used for next string"

	| stringWidth lineEndIndex outputWidth | 

	self string isEmpty ifTrue: [^aPosition].
	self positionX: aPosition.
	outputWidth := self parent contentWidth - aPosition.
	stringWidth := self stringWidthOf: self string. 
	stringWidth <= outputWidth ifTrue: [
		self endWithLineBreak ifTrue: [self lineBreak: true].
		^self lineBreak
			ifTrue: [0]
			ifFalse: [aPosition + stringWidth]].

	lineEndIndex := ((outputWidth / stringWidth) * self string size) floor - 3. 
	lineEndIndex <= 0 ifTrue: [
		^self calculateNextLineNoRoomAt: aPosition].

	^self calculateNextLineAt: aPosition lineEnd: lineEndIndex
%
method: R4PString
calculateNextLineAt: aPosition lineEnd: aLineEndIndex
	"Answer X position, to be used for next string"

	| truncatedString breakIndex endIndex newStringLayout remainingString originalString |
	originalString := self string.
	breakIndex := originalString
		indexOfLastByte: Character space codePoint
		startingAt: aLineEndIndex.
	breakIndex isZero
		ifTrue: [
		endIndex := aLineEndIndex.
		truncatedString := endIndex >= originalString size
			ifTrue: [originalString]
			ifFalse: [
			(originalString copyFrom: 1 to: endIndex) copyWith: $-]]
		ifFalse: [
		endIndex := breakIndex.
		truncatedString := originalString copyFrom: 1 to: endIndex].
	self
		string: truncatedString;
		cr.
	remainingString := originalString
		copyFrom: (endIndex + 1 min: originalString size)
		to: originalString size.
	newStringLayout := self class
		newForParent: self parent
		string: remainingString
		leading: self parent leading
		font: self layoutFont.
	newStringLayout endWithLineBreak: self endWithLineBreak.
	self parent addStringLayout: newStringLayout after: self.
	^newStringLayout calculateNextLineAt: 0
%
method: R4PString
calculateNextLineNoRoomAt: aPosition
	"This text cannot fit on the remaining line, so either insert a CR, or indicate that there is truncated text "

	| newStringLayout | 

	aPosition = 0 ifTrue: [
		self parent containsTruncatedText: true.
		^0]. 

	newStringLayout := self class 
								newForParent: self parent 
								string: '' 
								leading: self parent leading
								font: self layoutFont.
	newStringLayout cr.
	self parent addStringLayout: newStringLayout before: self.

	^self calculateNextLineAt: 0
%
method: R4PString
calculateStringRows
	" remove any content after the cr from this section and add it as addtional sections "

	| crIndex truncatedString remainingString newStringLayout | 

	self string isEmpty ifTrue: [^self].
	crIndex := self string indexOf: Character cr.
	crIndex = 0 ifTrue: [^self].
	crIndex = self string size ifTrue: [^self].

	truncatedString := self string copyFrom: 1 to: crIndex - 1.
	remainingString := self string copyFrom: crIndex + 1 to: self string size. 
	self string: truncatedString; cr; endWithLineBreak: true.
	
	newStringLayout := self class 
								newForParent: self parent 
								string: remainingString 
								leading: self parent leading
								font: self layoutFont.
	self parent addStringLayout: newStringLayout after: self.

	newStringLayout calculateStringRows
%
category: 'initialize-release'
method: R4PString
initialize
	"Initialize a newly created instance. This method must answer the receiver."

	super initialize.

	string := ''.
	lineBreak := false.
	endWithLineBreak := false.
	positionX := 0.

	^self
%
category: 'layout'
method: R4PString
contentLeft

	self isLeftJustified ifTrue: [^super contentLeft + self positionX ].

	^self parent getPositionX: self
%
method: R4PString
contentTop

	self isTop ifTrue: [^super contentTop].
	self isMiddle ifTrue: [^self parent contentTopForMiddleAlign: self].
	self isBottom ifTrue: [^self parent contentTopForBottomAlign: self].

	self reportError: 'PRwString>>contentLeft ... invalid layoutVerticalAlign: ', self layoutVerticalAlign printString
%
method: R4PString
layoutHeight

	^self spacingTop 
		+ self fontHeight 
		+ self spacingBottom
%
method: R4PString
spacingBottom

	^super spacingBottom 
		+ self fontDescender
%
method: R4PString
stringWidth

	^self stringWidthOf: self string
%
method: R4PString
textMatrix
	"Y = the line under the letters and above the descender.  contentTop is the upper edge of the text. "

	^self flip
		ifTrue: [
			self 
				textMatrixFlipScaleX: self layoutFontSize 
				scaleY: self layoutFontSize 
				positionX: self contentLeft
				positionY:  self contentTop + self fontHeightAdjustment]
		ifFalse: [
			self 
				textMatrixScaleX: self layoutFontSize 
				scaleY: self layoutFontSize 
				positionX: self contentLeft
				positionY:  self contentTop + self fontHeightAdjustment]
%
method: R4PString
textMatrixScaleX: aScaleX scaleY: aScaleY positionX: aPositionX positionY: aPositionY

	^(Array new: 6)	
		at: 1 put: aScaleX; 
		at: 2 put: 0; 
		at: 3 put: 0;
		at: 4 put: aScaleY negated; 
		at: 5 put: aPositionX; 
		at: 6 put: aPositionY;
		yourself
%
method: R4PString
textSpacingBottom

	^self parent spacingBottom
%
category: 'output'
method: R4PString
buildOutput: aBuilder 

	aBuilder buildString: self
%
method: R4PString
outputContent

	self containsTruncatedText ifTrue: [
		^Array 
			with: self truncatedImage
			with: self outputString].

	^Array with: self outputString
%
method: R4PString
outputString

	^R4POutputString 
			newForMatrix: self textMatrix 
			font: self layoutFont
			fontSize: self layoutFontSize 
			align: self layoutAlign
			originalWidth: self stringWidth
			foreground: self layoutForeground
			string: self string
%
category: 'printing'
method: R4PString
displayString

	self string isNil ifTrue: [^'<nil>'].
	self string size < 20 ifTrue: [^self string].
	^(self string copyFrom: 1 to: 20) , '...'
%
method: R4PString
printLayoutOn: aStream

	super printLayoutOn: aStream.
	aStream 
		cr; tab; nextPutAll: self string printString; 
		cr; tab; nextPutAll: 'stringWidth: '; nextPutAll: self stringWidth printString; 
		cr; tab; nextPutAll: 'lineBreak: '; nextPutAll: self lineBreak printString; 
		yourself
%
method: R4PString
printOn: aStream

	super printOn: aStream.
	aStream space; nextPutAll: self displayString printString
%
category: 'testing'
method: R4PString
canBuildAt: aNumber limit: aMaximumY
	"text bottom margin + bottom padding should be included in the page break calculation"

	^((aNumber + self layoutHeight + self parent spacingBottom) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
method: R4PString
containsTruncatedText

	^self parent containsTruncatedText
%
category: 'accessing'
method: R4PTable
cellBorder
	^cellBorder
%
method: R4PTable
cellBorder: anObject
	cellBorder := anObject
%
method: R4PTable
cellPadding
	^cellPadding
%
method: R4PTable
cellPadding: anObject

	cellPadding := anObject asLayoutArray copy
%
method: R4PTable
cellSpacing
	^cellSpacing
%
method: R4PTable
cellSpacing: anObject
	"copy to allow for #(0 0 0 0) which could get a #at:put: "

	cellSpacing := anObject asLayoutArray copy
%
method: R4PTable
columns
	^columns
%
method: R4PTable
columns: anObject
	columns := anObject
%
method: R4PTable
headerRow
	^headerRow
%
method: R4PTable
headerRow: anObject
	headerRow := anObject
%
method: R4PTable
layoutSections

	self headerRow notNil ifTrue: [
		self headerRow skipRendering ifFalse: [
			^(Array with: self headerRow) , (super layoutSections)]].

	^super layoutSections
%
method: R4PTable
repeatHeading 
	self repeatHeadingSet: true
%
method: R4PTable
repeatHeadingSet
	^repeatHeadingSet
%
method: R4PTable
repeatHeadingSet: anObject
	repeatHeadingSet := anObject
%
method: R4PTable
rows
	"to make table methods easier to read"

	^self sections
%
category: 'actions'
method: R4PTable
adjustColumnWidth
	"factor out fixed column widths before adjusting the rest ... truncate if not enough room"

	| totalVariable totalFixed ratio columnWidth | 

	self contentWidth = 0 ifTrue: [^self reportError: 'Cannot calculate column widths because content width of table is zero.'].
	totalVariable := self variableColumnWidth.
	totalFixed := self fixedColumnWidth.
	(totalVariable + totalFixed) < self contentWidth ifTrue: [^self].

	columnWidth := self contentWidth - totalFixed.
	columnWidth < 0 ifTrue: [
		self reduceColumnWidths ifFalse: [
			^self adjustColumnWidthTruncated: totalVariable + totalFixed]].
	columnWidth = 0 ifTrue: [^self error: 'Column width is zero... too many columns'].
	totalVariable := self variableColumnWidth.
	totalFixed := self fixedColumnWidth.
	ratio := (columnWidth / totalVariable) asFloat.
	ratio = 0 ifTrue: [^self error: 'Table column adjustment ratio is zero... too many columns'].
	self columns do: [:each | 
		each layoutNoWrap ifFalse: [
			each columnWidth: each columnWidth * ratio]]
%
method: R4PTable
adjustColumnWidthTruncated: aWidth

	| ratio | 

	self columns do: [:each | 
		each layoutNoWrap ifTrue: [each truncated: true]]. 

	ratio := (self contentWidth / aWidth) asFloat.
	ratio = 0 ifTrue: [^self reportError: 'Table column adjustment ratio is zero... too many columns'].
	self columns do: [:each | 
		each columnWidth: each columnWidth * ratio]
%
method: R4PTable
buildNewPageParent: aBuilder

	self repeatHeadingSet ifTrue: [
		self headerRow notNil ifTrue: [
			aBuilder buildTableHeaderRow: self]].
	
	super buildNewPageParent: aBuilder
%
method: R4PTable
buildOutput: aBuilder

	aBuilder buildTable: self
%
method: R4PTable
fixedColumns

	^self columns select: [:each | 
		each layoutNoWrap]
%
method: R4PTable
fixedColumnWidth

	| totalFixed |

	totalFixed := 0.
	self fixedColumns do: [:each | 
		totalFixed := totalFixed + each columnWidth].
	^totalFixed
%
method: R4PTable
getColumnCellsAt: anIndex
	"a cell can be nil if a neighbouring cell spans columns"
	"no #at:ifAbsent: in base VW for OrderedCollection"

	| headerCell columnCells | 

	self headerRow notNil ifTrue: [
		headerCell := anIndex > self headerRow sections size 
			ifTrue: [nil]
			ifFalse: [self headerRow sections at: anIndex]].
	
	columnCells := self sections collect: [:eachRow | 
		eachRow cellAt: anIndex].
	columnCells := columnCells reject: [:each | each isNil].

	headerCell notNil ifTrue: [
		columnCells := (Array with: headerCell) , columnCells].

	^columnCells
%
method: R4PTable
reduceColumnWidths
	"not enough room, so first try to narrow wide variable columns"

	| totalFixed originalWidth index count | 

	originalWidth := self contentWidth.
	totalFixed := self fixedColumnWidth.
	count := self variableColumns size.
	index := 1.
	[index <= count] whileTrue: [
		(self variableColumnWidth + totalFixed) <= originalWidth ifTrue: [^true].
		index := self reduceColumnWidthsAt: index].
	^false
%
method: R4PTable
reduceColumnWidthsAt: anIndex

	| list | 

	list := self variableColumns. 
	1 to: anIndex do: [:index | 
		| eachColumn | 
		eachColumn := list at: index.
		eachColumn columnWidth: (eachColumn columnWidth * 0.90 )].

	^anIndex + 1
%
method: R4PTable
variableColumns

	^self columns reject: [:each | 
		each layoutNoWrap]
%
method: R4PTable
variableColumnWidth

	| totalVariable |

	totalVariable := 0.
	self variableColumns do: [:each | 
		totalVariable := totalVariable + each columnWidth].
	^totalVariable
%
category: 'actions - add'
method: R4PTable
header

	| row | 

	row := R4PRow newForParent: self.
	self headerRow: row.
	^row
%
method: R4PTable
header: aBlock

	aBlock value: self header
%
method: R4PTable
image

	^self error: 'Cannot add image layout sections to a table. Only rows.'.
%
method: R4PTable
line

	^self error: 'Cannot add line layout sections to a table. Only rows.'.
%
method: R4PTable
row

	| row | 

	row := R4PRow newForParent: self.
	self sections add: row.
	^row
%
method: R4PTable
row: aBlock

	aBlock value: self row
%
method: R4PTable
string: aBlock

	^self error: 'Cannot add text layout sections to a table. Only rows.'.
%
method: R4PTable
text

	^self error: 'Cannot add text layout sections to a table. Only rows.'.
%
category: 'calculate'
method: R4PTable
calculateLayout

	| count columnCells | 

	count := 0.
	self rows do: [:each | count := count max: each numberOfColumns].
	self headerRow notNil ifTrue: [count := count max: self headerRow sections size].
	self rows do: [:each | each calculateRowSpan].

	self columns: ((1 to: count) asArray collect: [:each | 
		R4PColumn newForParent: self]).

	self columns doWithIndex: [:eachColumn :index | 
		columnCells := self getColumnCellsAt: index.
		eachColumn setCells: columnCells].

	self columns do: [:each | each calculateLayout].
	self adjustColumnWidth. 
	self headerRow notNil ifTrue: [self headerRow calculateLayout].
	self rows do: [:each | each calculateLayout].
%
method: R4PTable
calculateRowSpanRow: aRow cell: aCell

	| nextRows rowIndex cellIndex |

	aRow = self rows last ifTrue: [^self].
	rowIndex := self rows indexOf: aRow.
	nextRows := self rows 
						copyFrom: rowIndex + 1 
						to: ((rowIndex + aCell rowSpan - 1) min: self rows size).
	cellIndex := aRow indexOfCell: aCell.
	nextRows do: [:each | 
		each insertEmptyCellAt: cellIndex]
%
method: R4PTable
lastRowBottomOf: aCell row: aRow
	"for cells that span rows ... the next rows have not been processed yet, so they don't know their layout top, so we use each row's layout height"

	| nextRows rowIndex bottom |

	aRow = self rows last ifTrue: [^aRow boxBottom].
	rowIndex := self rows indexOf: aRow.
	nextRows := self rows 
						copyFrom: rowIndex + 1 
						to: ((rowIndex + aCell rowSpan - 1) min: self rows size).
	bottom := aRow layoutBottom.
	nextRows do: [:each | 
		bottom := bottom + each layoutHeight].
	^bottom
%
method: R4PTable
lastRowOf: aCell row: aRow
	"used for cells with rowSpan"

	| rowIndex |

	aRow = self rows last ifTrue: [^aRow].
	rowIndex := self rows indexOf: aRow.
	^self rows at: ((rowIndex + aCell rowSpan - 1) min: self rows size)
%
method: R4PTable
totalRowHeightOf: aCell row: aRow

	| nextRows rowIndex total |

	aRow = self rows last ifTrue: [^aRow layoutHeight].
	aCell rowSpan = 1 ifTrue: [^aRow layoutHeight].
	rowIndex := self rows indexOf: aRow.
	nextRows := self rows 
						copyFrom: rowIndex 
						to: ((rowIndex + aCell rowSpan - 1) min: self rows size).
	total := 0.
	nextRows do: [:each | 
		total := total + each layoutHeight].
	^total
%
category: 'initialize-release'
method: R4PTable
initialize

	super initialize.
	
	columns := OrderedCollection new.
	cellPadding :=  Array with: 0 with: 0 with: 0 with: 0.
	cellSpacing :=  Array with: 0 with: 0 with: 0 with: 0.
	cellBorder :=  Array with: 0 with: 0 with: 0 with: 0.
	repeatHeadingSet := false.

	^self
%
category: 'layout'
method: R4PTable
contentLeftOf: aColumn

	| total | 

	total := self contentLeft.
	self columns do: [:each | 
		each = aColumn ifTrue: [^total].
		total := total + each columnWidth].

	^total
%
method: R4PTable
contentRightOf: aCell
	"for cells that span columns"

	| index  column | 

	index := self columns indexOf: aCell column.
	index = 0 ifTrue: [^self reportError: 'PRwTable>>contentRightOf: ... starting column of a cell is not found in the table'].
	index := index + aCell columnSpan - 1.
	column := self columns at: index ifAbsent: [
		self error: 'PRwTable>>contentRightOf: ... column span exceeds number of columns created for table'].
	^column contentRight
%
method: R4PTable
layoutRightOf: aCell
	"for cells that span columns"

	| index  column | 

	index := self columns indexOf: aCell column.
	index = 0 ifTrue: [^self reportError: 'PRwTable>>contentRightOf: ... starting column of a cell is not found in the table'].
	index := index + aCell columnSpan - 1.
	column := self columns at: index.
	^column layoutRight
%
method: R4PTable
tableWidth

	^self contentWidth
%
category: 'testing'
method: R4PTable
canBuildAt: aNumber limit: aMaximumY

	(self canFitOnPageAt: aNumber limit: aMaximumY) ifFalse: [^false].

	^((aNumber + self spacingTop) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
method: R4PTable
canEndAtY: aNumber limit: aMaximumY

	^((aNumber + self spacingBottom) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
category: 'accessing'
method: R4PText
containsTruncatedText
	" for text we store the state of #containsTruncatedText because string sections are updated for truncated cases "

	^containsTruncatedText == true
%
method: R4PText
containsTruncatedText: anObject
	containsTruncatedText := anObject
%
method: R4PText
flip
	^flip
%
method: R4PText
flip: anObject
	flip := anObject
%
method: R4PText
flipText

	^self flip: true
%
method: R4PText
leading
	^leading
%
method: R4PText
leading: anObject
	leading := anObject
%
method: R4PText
maxFontHeight
	^maxFontHeight
%
method: R4PText
maxFontHeight: anObject
	maxFontHeight := anObject
%
category: 'accessing - keywords'
method: R4PText
gray
	"background color on other layouts"

	self foreground: #gray
%
method: R4PText
red
	"background color on other layouts"

	self foreground: #red
%
category: 'actions'
method: R4PText
addStringLayout: aLayout after: aCurrentLayout

	| index | 

	index := self sections indexOf: aCurrentLayout ifAbsent: [
		^self error: 'Cannot find string layout in text while calculating word wrap. ', aCurrentLayout displayString printString].

	self sections add: aLayout beforeIndex: index + 1.
%
method: R4PText
addStringLayout: aLayout before: aCurrentLayout

	| index | 

	index := self sections indexOf: aCurrentLayout ifAbsent: [
		^self error: 'Cannot find string layout in text while calculating word wrap. ', aCurrentLayout displayString printString].

	self sections add: aLayout beforeIndex: index
%
method: R4PText
addTruncatedString

	| outputWidth list | 

	outputWidth := self contentWidth - 10.
	list := self sections.
	self sections: OrderedCollection new.
	list do: [:eachSection | 
		outputWidth := eachSection buildTruncatedString: outputWidth.
		self sections add: eachSection.
		outputWidth > 0 ifFalse: [^self]].
%
method: R4PText
buildOutput: aBuilder 

	aBuilder buildText: self
%
method: R4PText
getPositionX: aStringLayout

	| leftX rightX totalWidth |

	leftX := self stringWidthLeftOf: aStringLayout.
	rightX := self stringWidthRightOf: aStringLayout.
	totalWidth := leftX + (aStringLayout stringWidth) + rightX.

	self isCentered ifTrue: [^self contentLeft + ((self contentWidth - totalWidth) / 2.0) + leftX].
	self isRightJustified ifTrue: [^self contentLeft + (self contentWidth - totalWidth) + leftX].

	self reportError: 'PRwString>>getPositionX: ... invalid layoutAlign: ', self layoutAlign printString
%
method: R4PText
replaceString: aString
	" used for bullet text "

	self initializeSections.
	self string: aString
%
method: R4PText
stringWidthLeftOf: aStringLayout

	| total | 

	aStringLayout == self sections first ifTrue: [^0].
	total := 0.
	self sections do: [:each | 
		each == aStringLayout ifTrue: [^total].
		total := each lineBreak 
			ifTrue: [0]
			ifFalse: [total + each stringWidth]].

	self error: 'Expected string layout not found in text layout'.
%
method: R4PText
stringWidthRightOf: aStringLayout

	| total sectionIndex | 

	aStringLayout lineBreak ifTrue: [^0].
	total := 0.
	sectionIndex := self sections indexOf: aStringLayout.
	sectionIndex + 1 to: self sections size do: [:index | 
		| each | 
		each := self sections at: index.
		total := total + (each stringWidth).
		each lineBreak ifTrue: [^total]].

	^total
%
method: R4PText
stringWidthTotalOf: aStringLayout

	| total sectionIndex | 

	total := self stringWidthLeftOf: aStringLayout.
	sectionIndex := self sections indexOf: aStringLayout.
	sectionIndex to: self sections size do: [:index | 
		| each | 
		each := self sections at: index.
		total := total + aStringLayout stringWidth.
		each lineBreak ifTrue: [^total]].

	^total
%
method: R4PText
totalTextPageY
%
category: 'actions - add'
method: R4PText
copyright

	| lastString | 

	self sections isEmpty ifTrue: [
		self string: self stringClass new].
	lastString := self sections last string.
	lastString := lastString copyWith: (Character value: 169).
	self sections last string: lastString
%
method: R4PText
image

	^self error: 'Images cannot be added to a text layout. Only strings can be added to a text layout'.
%
method: R4PText
line

	^self error: 'Lines cannot be added to a text layout. Only strings can be added to a text layout'.
%
method: R4PText
section

	^self error: 'Sections cannot be added to a text layout. Only strings can be added to a text layout'.
%
method: R4PText
string

	| stringSection | 
	
	stringSection := R4PString newForParent: self.
	self sectionsAdd: stringSection.
	^stringSection
%
method: R4PText
string: aString
	" since formatting of a string is limited, we don't use the block pattern here "

	self string string: aString
%
method: R4PText
table

	^self error: 'Tables cannot be added to a text layout. Only strings can be added to a text layout'.
%
method: R4PText
text
	"allows for 'report footer text ... ' "

	^self
%
method: R4PText
trademark

	| lastString | 

	self sections isEmpty ifTrue: [
		self string: self stringClass new].
	lastString := self sections last string.
	lastString := lastString copyWith: (Character value: 174).
	self sections last string: lastString
%
category: 'calculate'
method: R4PText
calculateLayout
	"  for layoutNoWrap ... no need to calculate word wrap, but need to update #calculateTruncatedLayout "

	self sections isEmpty ifTrue: [^self].
	self calculateTruncatedText. 
	self initializeLeading. 
	self calculateMaxFontHeight. 

	self layoutNoWrap ifTrue: [
		^self calculateTruncatedLayout].

	self calculateStringRows. 
	self calculateStringLayout.
	self sections last cr.
%
method: R4PText
calculateMaxFontHeight

	self sections size = 1 ifTrue: [^self].

	self sections do: [:each | 
		self maxFontHeight: (self maxFontHeight max: each fontAscender)].
%
method: R4PText
calculateStringLayout
	" copy of #sections because #calculateNextLineAt: may insert string layouts into #sections"

	| position | 

	position := 0.
	self sections copy do: [:each | 
		position := each calculateNextLineAt: position].
%
method: R4PText
calculateStringRows
	" split strings by cr prior to width calculation "

	self sections copy do: [:each | 
		each calculateStringRows].
%
method: R4PText
calculateTruncatedLayout

	self containsTruncatedText ifTrue: [
		self addTruncatedString]
%
method: R4PText
calculateTruncatedText

	self layoutNoWrap ifFalse: [^self containsTruncatedText: false].
	self initialStringWidth <= super contentWidth ifTrue: [^self containsTruncatedText: false].

	self containsTruncatedText: true
%
category: 'initialize-release'
method: R4PText
initialize

	super initialize.

	flip := false.
	leading := nil.
	maxFontHeight := 0.

	^self
%
method: R4PText
initializeLeading
	"after font is set"

	self layoutFont isNil ifTrue: [^self].
	self leading notNil ifTrue: [^self].

	self leading: self fontDescender / 2.0.
 
	self sections do: [:eachStringLayout |
		eachStringLayout
			marginTopAdd: self leading / 2.0;
			marginBottomAdd: self leading / 2.0]
%
category: 'layout'
method: R4PText
contentHeight

	^self totalFontHeight
%
method: R4PText
contentTopForBottomAlign: aLayout
	"text can hold mulltipe strings"

	| top adjustment | 

	top := (self parent contentBottom - self layoutHeight) max: self layoutTop.
	adjustment := ((self sections indexOf: aLayout) - 1) * (self fontHeight + self leading). 
	^top + adjustment
%
method: R4PText
contentTopForMiddleAlign: aLayoutString
	"text can hold mulltipe strings"

	| top adjustment | 

	top := self parent contentTopForMiddleAlign: self.
	adjustment := self layoutHeightOfSectionsAbove: aLayoutString.
	^(top + adjustment ) max: self contentTop
%
method: R4PText
contentWidth

	self layoutNoWrap ifTrue: [
		self layoutTruncated ifFalse: [
			^(self initialStringWidth) max: (super contentWidth)]].

	^super contentWidth
%
method: R4PText
fixedContentTop

	^super fixedContentTop + self fontHeight
%
method: R4PText
initialStringWidth
	" used for no-wrap, so assume no cr"

	| total | 

	total := 0.
	self sections do: [:each | 
		total := total + each stringWidth].
	^total
%
method: R4PText
layoutHeight

	^self layoutHeightCalculated
%
method: R4PText
layoutHeightOfSectionsAbove: aLayoutString

	| sectionIndex top | 

	sectionIndex := self layoutSections indexOf: aLayoutString.
	sectionIndex = 1 ifTrue: [^0].
	top := 0 .
	self layoutSections doWithIndex: [:each :index |
		index = sectionIndex ifTrue: [^top].
		top := top + each layoutHeight].

	self error: 'calculation error in layoutHeightOfSectionsAbove: ... string layout section not found in text section '
%
method: R4PText
layoutSections

	^self flip
		ifTrue: [super layoutSections reverse]
		ifFalse: [super layoutSections]
%
method: R4PText
totalFontHeight

	^(self layoutSections size) * (self fontHeight + self leading)
%
category: 'testing'
method: R4PText
canBuildAt: aNumber limit: aMaximumY
	"at least the first line of text needs to fit, or there will be extra empty spaces at the bottom of the page
	when there is room for spacingTop but not room for first line of text"

	((aNumber + self spacingTop) roundTo: 0.01) >  (aMaximumY roundTo: 0.01) ifTrue: [^false].
	self layoutSections isEmpty ifTrue: [^true].
	(self canFitOnPageAt: aNumber limit: aMaximumY) ifFalse: [^false].

	^self layoutSections first 
			canBuildAt: aNumber + self spacingTop
			limit: aMaximumY
%
method: R4PText
canEndAtY: aNumber limit: aMaximumY
	"if there is room on the page, leave room for the bottom spacing of this text"

	^((aNumber + self spacingBottom) roundTo: 0.01)
		 <= 
			(aMaximumY roundTo: 0.01)
%
DoIt
System myUserProfile removeDictionaryAt: 1.
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
