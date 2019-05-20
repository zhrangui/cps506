'From Pharo7.0.3 of 12 April 2019 [Build information: Pharo-7.0.3+build.158.sha.0903ade8a6c96633f07e0a7f1baa9a5d48cfdf55 (64 Bit)] on 20 May 2019 at 4:02:09.998794 pm'!BorderedMorph subclass: #RectangleStyle	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'CCPS506-Object'!!RectangleStyle commentStamp: 'count 5/20/2019 15:51' prior: 0!A subclass of BorderedMorph that supports different fillStyles.

RectangleStyle diagonalPrototype openInWorld.
RectangleStyle gradientPrototype openInWorld.!!RectangleStyle methodsFor: 'visual properties' stamp: 'count 5/20/2019 15:51'!canHaveFillStyles
	"Return true if the receiver can have general fill styles; not just colors.
	This method is for gradually converting old morphs."
	^self class == RectangleStyle "no subclasses"! !!RectangleStyle methodsFor: 'initialization' stamp: 'rz 5/20/2019 11:43'!defaultColor
	"answer the default color/fill style for the receiver"
	^ Color
		r: 0.613
		g: 0.903
		b: 1.0! !!RectangleStyle methodsFor: 'accessing' stamp: 'rz 5/20/2019 13:39'!wantsToBeCachedByHand
	"Return true if the receiver wants to be cached by the hand when it is dragged around."
	self hasTranslucentColor ifTrue:[^false].
	self bounds = self fullBounds ifTrue:[^true].
	self submorphsDo:[:m|
		(self bounds containsRect: m fullBounds) ifFalse:[
			m wantsToBeCachedByHand ifFalse:[^false].
		].
	].
	^true! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!RectangleStyle class	instanceVariableNames: ''!!RectangleStyle class methodsFor: 'as yet unclassified' stamp: 'rz 5/20/2019 12:29'!roundRectPrototype
	"Answer a prototypical RoundRect object for a parts bin."

	^ self authoringPrototype useRoundedCorners 
		color: (Color r: 1.0 g: 0.3 b: 0.6); 
		borderWidth: 1;
		setNameTo: 'RoundRect'! !!RectangleStyle class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 14:39'!gradientPrototype

	| rr |
	rr := self authoringPrototype.
	rr useGradientFill; borderWidth: 0.
	^ rr! !!RectangleStyle class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 13:09'!descriptionForPartsBin
	^ self partName:	'Rectangle' translatedNoop
		categories:		{'Graphics' translatedNoop. 'Basic' translatedNoop}
		documentation:	'A rectangular shape, with border and fill style' translatedNoop! !!RectangleStyle class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 12:09'!diagonalPrototype

	| rr |
	rr := self authoringPrototype.
	rr useGradientFill; borderWidth: 0.
	rr fillStyle direction: rr extent.
	^ rr! !