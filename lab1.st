|a n r Sn|
"Name:  Rui Zhang
 No:    500736315
 Course:    CCPS506, Spring 2019, Lab #1
 Due:   2019.05.20 11:59:59"
Transcript clear.
a := (UIManager default request: 'First value') asNumber.
n := (UIManager default request: 'Total number') asNumber.
r := (UIManager default request: 'Common ration') asNumber.
Sn := a * (1 - (r raisedTo: n)) / (1 - r).
Transcript show: 'Sum of Geometric Progression Serie: ' , Sn printString; cr.
1 to: n do: [:k | Transcript show: a * (r raisedTo: k - 1) asFraction; cr.]

