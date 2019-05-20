'From Pharo7.0.3 of 12 April 2019 [Build information: Pharo-7.0.3+build.158.sha.0903ade8a6c96633f07e0a7f1baa9a5d48cfdf55 (64 Bit)] on 20 May 2019 at 3:10:12.669842 pm'!
BorderedMorph subclass: #StyleRectangle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CCPS506-Object'!
!StyleRectangle commentStamp: 'count 5/20/2019 15:09' prior: 0!
A subclass of BorderedMorph that supports different fillStyles.

StyleRectangle diagonalPrototype openInWorld.
StyleRectangle gradientPrototype openInWorld.!


!StyleRectangle methodsFor: 'accessing' stamp: 'rz 5/20/2019 15:09'!
wantsToBeCachedByHand
	"Return true if the receiver wants to be cached by the hand when it is dragged around."
	self hasTranslucentColor ifTrue:[^false].
	self bounds = self fullBounds ifTrue:[^true].
	self submorphsDo:[:m|
		(self bounds containsRect: m fullBounds) ifFalse:[
			m wantsToBeCachedByHand ifFalse:[^false].
		].
	].
	^true! !


!StyleRectangle methodsFor: 'visual properties' stamp: 'count 5/20/2019 15:08'!
canHaveFillStyles
	"Return true if the receiver can have general fill styles; not just colors.
	This method is for gradually converting old morphs."
	^self class == StyleRectangle "no subclasses"! !


!StyleRectangle methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:09'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color
		r: 0.613
		g: 0.903
		b: 1.0! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

StyleRectangle class
	instanceVariableNames: ''!

!StyleRectangle class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 15:09'!
diagonalPrototype

	| rr |
	rr := self authoringPrototype.
	rr useGradientFill; borderWidth: 0.
	rr fillStyle direction: rr extent.
	^ rr! !

!StyleRectangle class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 15:09'!
gradientPrototype

	| rr |
	rr := self authoringPrototype.
	rr useGradientFill; borderWidth: 0.
	^ rr! !

!StyleRectangle class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 15:09'!
descriptionForPartsBin
	^ self partName:	'Rectangle' translatedNoop
		categories:		{'Graphics' translatedNoop. 'Basic' translatedNoop}
		documentation:	'A rectangular shape, with border and fill style' translatedNoop! !


!StyleRectangle class methodsFor: 'as yet unclassified' stamp: 'rz 5/20/2019 15:09'!
roundRectPrototype
	"Answer a prototypical RoundRect object for a parts bin."

	^ self authoringPrototype useRoundedCorners 
		color: (Color r: 1.0 g: 0.3 b: 0.6); 
		borderWidth: 1;
		setNameTo: 'RoundRect'! !
