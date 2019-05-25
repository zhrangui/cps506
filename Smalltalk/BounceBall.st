'From Squeak5.3alpha of 13 December 2018 [latest update: #18222] on 25 May 2019 at 10:50:44 am'!RectangleStyle subclass: #BounceBall	instanceVariableNames: 'phase ball star'	classVariableNames: ''	poolDictionaries: ''	category: 'CCPS506-Object'!!BounceBall commentStamp: 'count 5/20/2019 15:50' prior: 0!This is a example of how to use a morph. It consists of only two 
methods, initialize and step.

DoIt:
BounceBall new openInWindow.



!!BounceBall methodsFor: 'initialization' stamp: 'count 5/24/2019 22:17'!initialize
	"initialize the state of the receiver"
	super initialize.
	phase := 1.
	self extent: 500 @ 500.
	ball := CircleMorph new.
	self
		addMorph: ((star := Star new extent: 250 @ 250) center: self center)! !!BounceBall methodsFor: 'stepping and presenter' stamp: 'count 5/24/2019 22:04'!step
	phase := phase\\8 + 1.
	phase = 1 ifTrue: [^ ball delete].
	phase < 4 ifTrue:[^self].
	phase = 4 ifTrue: [self addMorph: ball].
	ball align: ball center with: (star vertices at: (phase-3*2)).! !