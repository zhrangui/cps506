'From Pharo7.0.3 of 12 April 2019 [Build information: Pharo-7.0.3+build.158.sha.0903ade8a6c96633f07e0a7f1baa9a5d48cfdf55 (64 Bit)] on 24 May 2019 at 10:17:36.598892 pm'!BorderedMorph subclass: #RectangleStyle	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'CCPS506-Object'!!RectangleStyle commentStamp: 'count 5/20/2019 15:51' prior: 0!A subclass of BorderedMorph that supports different fillStyles.

RectangleStyle diagonalPrototype openInWorld.
RectangleStyle gradientPrototype openInWorld.!!RectangleStyle methodsFor: 'visual properties' stamp: 'count 5/20/2019 15:51'!canHaveFillStyles
	"Return true if the receiver can have general fill styles; not just colors.
	This method is for gradually converting old morphs."
	^self class == RectangleStyle "no subclasses"! !!RectangleStyle methodsFor: 'initialization' stamp: 'count 5/24/2019 22:15'!defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightOrange! !