'From Squeak5.3alpha of 13 December 2018 [latest update: #18222] on 25 May 2019 at 10:59:26 am'!PolygonMorph subclass: #Star	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'CCPS506-Object'!!Star commentStamp: 'count 5/20/2019 15:32' prior: 0!I am a very flexible star..
!!Star methodsFor: 'parts bin' stamp: 'rz 5/20/2019 15:32'!initializeToStandAlone
	^ self initialize removeHandles! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ Color black! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultCenter
	"answer the default center for the receiver"
	^ 0 asPoint! !!Star methodsFor: 'initialization' stamp: 'count 5/24/2019 22:14'!defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightMagenta ! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultFirstVertex
	"answer the default first outer point for the receiver.
	This with the center determines the angle and size of the outer radius."
	^ 10 asPoint! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultSides
	"answer the default number of sides for the receiver"
	^ 10! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultStarRatio
	"answer the default ratio of outer radius to inner radius for the receiver"
	^ 5.0 / 12.0! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!initialize
	"initialize the state of the receiver"
	super initialize.
	self
		makeVertices: self defaultSides
		starRatio: self defaultStarRatio
		withCenter: self defaultCenter
		withPoint: self defaultFirstVertex.
	self computeBounds! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!insureCompatability
"The old stars had the point on the second not the first vertex. So we need to check for this special case."
 | c v1 v2 |
c := vertices average rounded.
 v1 := vertices first .
 v2 := vertices second .
(c dist: v1) + 0.001 < (c dist: v2) ifTrue: [vertices := vertices allButFirst copyWith: v1]

! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!makeVertices: nSides 
	"Assuming vertices has at least one point, make a new star 
	or regular polygon (for odd sided polygons).
	The center of the polygon and the first vertex remain in
	place. The inner distances for stars remain the same also if
	possible."
	| center r1 rN rNext a1 rTotal |
	center := vertices average rounded.
	r1 := center dist: vertices first.
	rN := center dist: vertices last.
	rNext := 1 max: r1.
	a1 := 270.0
				+ (center bearingToPoint: vertices first).
	rTotal := nSides even
				ifTrue: [rNext + rNext min: rNext + rN]
				ifFalse: [rNext + rNext].
	rNext := rTotal - rNext.
	self changed .
	vertices := (a1 to: a1 + 359.999 by: 360.0 / nSides)
				collect: [:angle | center
						+ (Point r: (rNext := rTotal - rNext) degrees: angle)].
	self computeBounds.
	self changed! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!makeVertices: nSides starRatio: fraction
	"Assuming vertices has at least one point, make a new star 
	or regular polygon (for odd sided polygons).
	The center of the polygon and the first vertex remain in
	place. The inner distances for stars remain the same also if
	possible."
	| center r1 rN rNext a1 rTotal |
	center := vertices average rounded.
	r1 := center dist: vertices first.
	rNext := 1 max: r1.
	rN := (1.0 min: fraction) * rNext.
	a1 := 270.0
				+ (center bearingToPoint: vertices first).
	rTotal := nSides even
				ifTrue: [rNext + rNext min: rNext + rN]
				ifFalse: [rNext + rNext].
	rNext := rTotal - rNext.
	self changed .
	vertices := (a1 to: a1 + 359.999 by: 360.0 / nSides)
				collect: [:angle | center
						+ (Point r: (rNext := rTotal - rNext) degrees: angle)].
	self computeBounds.
	self changed! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!makeVertices: nSides starRatio: fraction withCenter: center withPoint: aPoint 
	"Make a new star or regular polygon (for odd sided polygons).
	This makes star vertices from scratch without any feedback from existing vertices."
	| r1 rN rNext a1 rTotal |
	r1 := center dist: aPoint.
	rNext := 1 max: r1.
	rN := (1.0 min: fraction)
				* rNext.
	a1 := 270.0
				+ (center bearingToPoint: aPoint).
	rTotal := nSides even
				ifTrue: [rNext + rNext min: rNext + rN]
				ifFalse: [rNext + rNext].
	rNext := rTotal - rNext.
	self changed.
	vertices := (a1 to: a1 + 359.999 by: 360.0 / nSides)
				collect: [:angle | center
						+ (Point r: (rNext := rTotal - rNext) degrees: angle)].
	self computeBounds.
	self changed! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 14:32'!lessVertices
"Reduce the number of points by one until we are  a diamond. If odd reduce the number of sides by two until we become a triangle. See class comment."
	| nVerts |
	( nVerts := 2 negated + vertices size) < 3 ifFalse: [
	self
		makeVertices: nVerts]! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 11:07'!moreVertices
	self makeVertices: 2+ vertices size! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 9:08'!nextTwinkle
	self makeVertices: vertices size starRatio: self nextTwinkleSkip .
	self computeBounds.! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 12:32'!prevTwinkle
	self makeVertices: vertices size starRatio: self prevTwinkleSkip .
	self computeBounds.! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 15:02'!updateFormFromUser
"Does nothing here. Overridden in subclasses e.g. Kaleidoscope."
^ self.! !!Star methodsFor: '*Etoys-Squeakland-menus' stamp: 'rz 5/20/2019 12:12'!nextFatter
	self makeVertices: vertices size starRatio: self nextSkip .
	self computeBounds.! !!Star methodsFor: '*Etoys-Squeakland-menus' stamp: 'rz 5/20/2019 13:22'!nextThinner
	self makeVertices: vertices size starRatio: self prevSkip .
	self computeBounds.! !!Star methodsFor: 'accessing' stamp: 'rz 5/20/2019 15:32'!starRatio: r
"Set the star s.t. the ratio of the inner radius to the outer radius is r.
If r is > 1 use the reciprocal to keep the outer radius first."
"Assume we have at least one vertex.
set
All ways return a number <= 1.0"
self makeVertices: vertices size starRatio:( r > 1.0  ifTrue: [  r reciprocal ] ifFalse: [r ] ).! !!Star methodsFor: 'access' stamp: 'rz 5/20/2019 15:32'!skipRatios
"Return an array of  ratios  of the inner radius to the outer radius.
Ratios are in ascending order from 0.0 to 1.0."
"Assume we have at least one vertex.
All ways return a number <= 1.0"

| n  alpha  |
"Odd vertices sizes can not be stars only regular polygons"
n:= vertices size . n odd ifTrue: [ ^ #(  1.0) ] .

alpha := Float pi / (n//2)  asFloat .

^ ((((    Float halfPi -alpha  to: alpha /2.0  by: alpha  negated ) 
	collect:  [:angle |( (angle) sin )/
					(angle + alpha ) sin ]
	) copyWith: 0.0) copyWithFirst: 1.0) reversed .! !!Star methodsFor: 'access' stamp: 'rz 5/20/2019 15:32'!starRatio
"Return the ratio of the inner radius to the outer radius."
"Assume we have at least one vertex.
All ways return a number <= 1.0"
| r c |
c := vertices average rounded .
r := (c dist: vertices last) / (c dist:  vertices first)  .
^ r > 1.0  ifTrue: [  r reciprocal ] ifFalse: [r ] .! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!nextSkip
	"Set starRatio to next skip wrapping if needed."
	| skips n c r1 rN |
	c := vertices average rounded.
	r1 := (c dist: vertices first) truncated asFloat.
	rN := c dist: vertices last.
	skips := self skipRatios.
	n := skips * r1
				findFirst: [:r | r > (rN + 1.0)].
	"n = 0
		ifTrue: [n := skips size]."
	^ skips atWrap: n! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!nextTwinkleSkip
	"Set starRatio to next skip wrapping if needed."
	| skips n c r1 rN |
	c := vertices average rounded.
	r1 := (c dist: vertices first) truncated asFloat.
	rN := c dist: vertices last.
	skips := self skipRatios.
	n := skips * r1
				findFirst: [:r | r > (rN + 1.0)].
	n = 0
		ifTrue: [ n := 1].
	^ skips atWrap: n! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!prevSkip
	"Set starRatio to next skip wrapping if necessary"
	| skips n c r1 rN |
	c := vertices average rounded.
	r1 := c dist: vertices first.
	rN := (c dist: vertices last) truncated asFloat.
	skips := self skipRatios.
	n := skips * r1
				findLast: [:r | r + 1.0 < rN].
	n = 0
		ifTrue: [n := 1].
	^ skips at: n! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!prevTwinkleSkip
	"Set starRatio to next skip wrapping if necessary"
	| skips n c r1 rN |
	c := vertices average rounded.
	r1 := c dist: vertices first.
	rN := (c dist: vertices last) truncated asFloat.
	skips := self skipRatios.
	n := skips * r1
				findLast: [:r | r + 1.0 < rN].
	"n = 0
	ifTrue: [^ oldR]."
	^ skips atWrap: n! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Star class	instanceVariableNames: ''!!Star class methodsFor: 'parts bin' stamp: 'rz 5/20/2019 00:11'!descriptionForPartsBin
	^ self partName:	'Star' translatedNoop
		categories:		{'Graphics' translatedNoop}
		documentation:	'A symmetrical polygon in the shape of a star'  translatedNoop! !!Star class methodsFor: 'class initialization' stamp: 'rz 5/20/2019 11:37'!initialize

	self registerInFlapsRegistry.	! !!Star class methodsFor: 'class initialization' stamp: 'count 5/20/2019 15:08'!registerInFlapsRegistry
	"Register the receiver in the system's flaps registry"
	self environment
		at: #Flaps
		ifPresent: [:cl | cl registerQuad: {#Star. #authoringPrototype. 'Star' translatedNoop. 'A star' translatedNoop}
						forFlapNamed: 'PlugIn Supplies'.
						cl registerQuad: {#Star. #authoringPrototype. 'Star' translatedNoop. 'A star' translatedNoop}
						forFlapNamed: 'Supplies'.]! !!Star class methodsFor: 'class initialization' stamp: 'rz 5/20/2019 12:41'!unload
	"Unload the receiver from global registries"

	self environment at: #Flaps ifPresent: [:cl |
	cl unregisterQuadsWithReceiver: self] ! !Star initialize!