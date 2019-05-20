'From Pharo7.0.3 of 12 April 2019 [Build information: Pharo-7.0.3+build.158.sha.0903ade8a6c96633f07e0a7f1baa9a5d48cfdf55 (64 Bit)] on 20 May 2019 at 3:40:13.267453 pm'!PolygonMorph subclass: #Star	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'CCPS506-Object'!!Star commentStamp: 'count 5/20/2019 15:32' prior: 0!I am a very flexible star..
!!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 15:32'!updateFormFromUser
"Does nothing here. Overridden in subclasses e.g. Kaleidoscope."
^ self.! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 15:32'!prevTwinkle
	self makeVertices: vertices size starRatio: self prevTwinkleSkip .
	self computeBounds.! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 15:32'!lessVertices
"Reduce the number of points by one until we are  a diamond. If odd reduce the number of sides by two until we become a triangle. See class comment."
	| nVerts |
	( nVerts := 2 negated + vertices size) < 3 ifFalse: [
	self
		makeVertices: nVerts]! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 15:32'!nextTwinkle
	self makeVertices: vertices size starRatio: self nextTwinkleSkip .
	self computeBounds.! !!Star methodsFor: 'menus' stamp: 'rz 5/20/2019 15:32'!moreVertices
	self makeVertices: 2+ vertices size! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!withCenterOuterHandles
	"Add to our handles the center positioning and outer resizing
	handles. Outer handle must not be blocked so it comes first. 
	The editing routine expects to find the center handle second.
	The side and shape changing handles follow these."
	| center v1 hExtent holder |
	center := vertices average rounded.
	hExtent := 8 @ 8.
	v1 := vertices first.
	holder := {(EllipseMorph
				newBounds: (Rectangle center: v1 extent: hExtent)
				color: Color yellow)
				setBalloonText: 'Move me to adjust size. Shift move to adjust pointiness'. (EllipseMorph
				newBounds: (Rectangle center: center extent: hExtent)
				color: Color yellow)
				setBalloonText: 'Move me to adjust position'}.
	holder
		with: {#outside. #center}
		do: [:handle :which | handle
				on: #mouseDown
				send: #dragVertex:event:fromHandle:
				to: self
				withValue: which;
				
				on: #mouseMove
				send: #dragVertex:event:fromHandle:
				to: self
				withValue: which].
	handles addAll: holder! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!addStarHandles
	"Outer handle must not be blocked so it comes first. 
	The editing routine expects to find the center handle second.
	The side and shape changing handles follow these."
	| center |
	self removeHandles.
	"Check for old stars and correct order of vertices."
	self insureCompatability .
	handles := OrderedCollection new.
	center := vertices average rounded.
	self withCenterOuterHandles; withUpDownLeftRightHandlesAround: 6 center: center.
	self placeHandles.
	self changed.
	! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!dragVertex: label event: evt fromHandle: handle 
	| center r1 rN rNext a1 rTotal |
	label == #outside
		ifTrue: [center := handles second center.
			r1 := center dist: vertices first.
			"Rounding and what happens as the outer handle
			approached the center, 
			requires we guard the inner radius 
			from becoming larger than the outer radius."
			rN := r1
						min: (center dist: vertices last).
			rNext := 1
						max: (center dist: evt cursorPoint).
			a1 := 270.0
						+ (center bearingToPoint: evt cursorPoint).
			rTotal := vertices size even
						ifTrue: [evt shiftPressed
								ifTrue: [rNext + rNext min: rNext + rN]
								ifFalse: [r1 + rN * rNext / r1]]
						ifFalse: [rNext + rNext].
			rNext := rTotal - rNext.
			vertices := ((a1 to: a1 + 359.999 by: 360.0 / vertices size)
						collect: [:angle | center
								+ (Point r: (rNext := rTotal - rNext) degrees: angle)]) .
			handle align: handle center with: evt cursorPoint].
	label == #center
		ifTrue: [evt shiftPressed
				ifTrue: [self updateFormFromUser]
				ifFalse: [self position: self position + (evt cursorPoint - handle center)]].
	self computeBounds! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!addHandles
	self addStarHandles! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!withUpDownLeftRightHandlesAround: radius center: center
	"Add to our handles the side and shape changing handles."
	| tri  above holder  triAbove triBelow triRight triLeft |
	above := 0 @ radius negated.
	
	tri := Array
				with: 0 @ -5
				with: 4 @ 3
				with: -4 @ 3.
	triAbove := tri + (center + above).
	triBelow := triAbove
				collect: [:pt | pt rotateBy: #pi centerAt: center].
	triRight := triAbove
				collect: [:pt | pt rotateBy: #right centerAt: center].
	triLeft := triAbove
				collect: [:pt | pt rotateBy: #left centerAt: center].
				
	holder := { (PolygonMorph
				vertices: triAbove
				color: Color green
				borderWidth: 1
				borderColor: Color black)
				 setBalloonText: 'More points.'. 
				
				(PolygonMorph
				vertices: triBelow
				color: Color magenta
				borderWidth: 1
				borderColor: Color black)
				 setBalloonText: 'Fewer points.'. 
				
				(PolygonMorph
				vertices: triRight
				color: Color green
				borderWidth: 1
				borderColor: Color black)
				 setBalloonText: 'Twinkle fatter.'. 
				
				(PolygonMorph
				vertices: triLeft
				color: Color magenta
				borderWidth: 1
				borderColor: Color black)
				 setBalloonText: 'Twinkle thinner.'}.
	
	holder
		with: {#more. #less. #next. #prev}
		do: [:handle :which | handle
				on: #mouseDown
				send: #changeVertices:event:fromHandle:
				to: self
				withValue: which;
				
				on: #mouseMove
				send: #changeVertices:event:fromHandle:
				to: self
				withValue: which].
	^ handles addAll: holder! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!changeVertices: label event: evt fromHandle: handle 
	| |
	label == #more
		ifTrue: [evt shiftPressed
				ifTrue: [self moreVertices "not oneMoreVertex"]
				ifFalse: [self moreVertices]].
	label == #less
		ifTrue: [evt shiftPressed
				ifTrue: [self lessVertices "not oneLessVertex"]
				ifFalse: [self lessVertices]].
	label == #next
		ifTrue: [evt shiftPressed
				ifTrue: [self makeVertices: vertices size starRatio: self nextSkip]
				ifFalse: [self makeVertices: vertices size starRatio: self nextTwinkleSkip]].
	label == #prev
		ifTrue: [evt shiftPressed
				ifTrue: [self makeVertices: vertices size starRatio: self prevSkip]
				ifFalse: [self makeVertices: vertices size starRatio: self prevTwinkleSkip]].
	self computeBounds! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!updateHandles! !!Star methodsFor: 'editing' stamp: 'rz 5/20/2019 15:32'!placeHandles
	"Add the handles to my submorphs."
	handles reverseDo: [:each | self addMorphFront: each ] .
	
	! !!Star methodsFor: '*Etoys-Squeakland-menus' stamp: 'rz 5/20/2019 15:32'!nextFatter
	self makeVertices: vertices size starRatio: self nextSkip .
	self computeBounds.! !!Star methodsFor: '*Etoys-Squeakland-menus' stamp: 'rz 5/20/2019 15:32'!nextThinner
	self makeVertices: vertices size starRatio: self prevSkip .
	self computeBounds.! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!prevSkip
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
	^ skips at: n! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!nextSkip
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
	^ skips atWrap: n! !!Star methodsFor: 'geometry' stamp: 'rz 5/20/2019 15:32'!prevTwinkleSkip
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
	^ skips atWrap: n! !!Star methodsFor: 'parts bin' stamp: 'rz 5/20/2019 15:32'!initializeToStandAlone
	^ self initialize removeHandles! !!Star methodsFor: 'accessing' stamp: 'rz 5/20/2019 15:32'!starRatio: r
"Set the star s.t. the ratio of the inner radius to the outer radius is r.
If r is > 1 use the reciprocal to keep the outer radius first."
"Assume we have at least one vertex.
set
All ways return a number <= 1.0"
self makeVertices: vertices size starRatio:( r > 1.0  ifTrue: [  r reciprocal ] ifFalse: [r ] ).! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultStarRatio
	"answer the default ratio of outer radius to inner radius for the receiver"
	^ 5.0 / 12.0! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!makeVertices: nSides 
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
	self changed! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ Color black! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!insureCompatability
"The old stars had the point on the second not the first vertex. So we need to check for this special case."
 | c v1 v2 |
c := vertices average rounded.
 v1 := vertices first .
 v2 := vertices second .
(c dist: v1) + 0.001 < (c dist: v2) ifTrue: [vertices := vertices allButFirst copyWith: v1]

! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!makeVertices: nSides starRatio: fraction
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
	self changed! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultCenter
	"answer the default center for the receiver"
	^ 0 asPoint! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightBlue! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!makeVertices: nSides starRatio: fraction withCenter: center withPoint: aPoint 
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
	self changed! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultFirstVertex
	"answer the default first outer point for the receiver.
	This with the center determines the angle and size of the outer radius."
	^ 10 asPoint! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!initialize
	"initialize the state of the receiver"
	super initialize.
	self
		makeVertices: self defaultSides
		starRatio: self defaultStarRatio
		withCenter: self defaultCenter
		withPoint: self defaultFirstVertex.
	self computeBounds! !!Star methodsFor: 'initialization' stamp: 'rz 5/20/2019 15:32'!defaultSides
	"answer the default number of sides for the receiver"
	^ 10! !!Star methodsFor: 'access' stamp: 'rz 5/20/2019 15:32'!starRatio
"Return the ratio of the inner radius to the outer radius."
"Assume we have at least one vertex.
All ways return a number <= 1.0"
| r c |
c := vertices average rounded .
r := (c dist: vertices last) / (c dist:  vertices first)  .
^ r > 1.0  ifTrue: [  r reciprocal ] ifFalse: [r ] .! !!Star methodsFor: 'access' stamp: 'rz 5/20/2019 15:32'!skipRatios
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
	) copyWith: 0.0) copyWithFirst: 1.0) reversed .! !!Star methodsFor: 'menu' stamp: 'rz 5/20/2019 15:32'!addCustomMenuItems: aCustomMenu hand: aHandMorph 
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
self addChangeSidesMenuItems: aCustomMenu hand: aHandMorph.
self addTwinkleMenuItems: aCustomMenu hand: aHandMorph.
	! !!Star methodsFor: 'menu' stamp: 'rz 5/20/2019 15:32'!oneLessVertex
	self
		makeVertices: (3 max: 1 negated + vertices size)! !!Star methodsFor: 'menu' stamp: 'rz 5/20/2019 15:32'!addChangeSidesMenuItems: aCustomMenu hand: aHandMorph 
	"Menu items to change number of sides."
	aCustomMenu addLine.
	aCustomMenu add: 'more sides' translated action: #moreVertices.
	aCustomMenu add: 'fewer sides' translated action: #lessVertices.
"	Regular polygons can be simulated with the one of the skip stars 
	and it would confuse users to have stars be limited to Regular polygons.
	So we've removed those menu items - wiz"
"	aCustomMenu add: 'one more side' translated action: #oneMoreVertex.
	aCustomMenu add: 'one fewer side' translated action: #oneLessVertex"! !!Star methodsFor: 'menu' stamp: 'rz 5/20/2019 15:32'!addTwinkleMenuItems: aCustomMenu hand: aHandMorph 
	"Menu items to change the sharpness of the star."
	aCustomMenu addLine.
	aCustomMenu add: 'twinkle fatter' translated action: #nextTwinkle.
	aCustomMenu add: 'twinkle thinner' translated action: #prevTwinkle.
"	aCustomMenu add: 'fatter star' translated action: #nextFatter.
	aCustomMenu add: 'thinner star' translated action: #prevThinner"
	
	


! !!Star methodsFor: 'menu' stamp: 'wiz 12/11/2004 16:46'!oneMoreVertex
	self makeVertices: 1 + vertices size! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Star class	instanceVariableNames: ''!!Star class methodsFor: 'class initialization' stamp: 'asm 4/11/2003 12:41'!unload
	"Unload the receiver from global registries"

	self environment at: #Flaps ifPresent: [:cl |
	cl unregisterQuadsWithReceiver: self] ! !!Star class methodsFor: 'class initialization' stamp: 'count 5/20/2019 15:08'!registerInFlapsRegistry
	"Register the receiver in the system's flaps registry"
	self environment
		at: #Flaps
		ifPresent: [:cl | cl registerQuad: {#Star. #authoringPrototype. 'Star' translatedNoop. 'A star' translatedNoop}
						forFlapNamed: 'PlugIn Supplies'.
						cl registerQuad: {#Star. #authoringPrototype. 'Star' translatedNoop. 'A star' translatedNoop}
						forFlapNamed: 'Supplies'.]! !!Star class methodsFor: 'class initialization' stamp: 'asm 4/11/2003 11:37'!initialize

	self registerInFlapsRegistry.	! !!Star class methodsFor: 'parts bin' stamp: 'tak 7/25/2007 00:11'!descriptionForPartsBin
	^ self partName:	'Star' translatedNoop
		categories:		{'Graphics' translatedNoop}
		documentation:	'A symmetrical polygon in the shape of a star'  translatedNoop! !Star initialize!