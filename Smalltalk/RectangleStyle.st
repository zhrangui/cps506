'From Squeak5.3alpha of 13 December 2018 [latest update: #18222] on 25 May 2019 at 10:50:39 am'!BorderedMorph subclass: #RectangleStyle	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'CCPS506-Object'!!RectangleStyle commentStamp: 'count 5/20/2019 15:51' prior: 0!A subclass of BorderedMorph that supports different fillStyles.

RectangleStyle diagonalPrototype openInWorld.
RectangleStyle gradientPrototype openInWorld.!!RectangleStyle methodsFor: 'visual properties' stamp: 'count 5/20/2019 15:51'!canHaveFillStyles
	"Return true if the receiver can have general fill styles; not just colors.
	This method is for gradually converting old morphs."
	^self class == RectangleStyle "no subclasses"! !!RectangleStyle methodsFor: 'initialization' stamp: 'count 5/24/2019 22:15'!defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightOrange! !