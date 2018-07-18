# Decision Tree
### _Panji Kusuma <epanji@gmail.com>_

This is a foolish attempt from author to implement decision tree module with CLOS approach without consulting first with experts. The reason behind the writing is none other than to learn about common lisp and get benefits from it. Due to the lack of knowledge about common lisp, please don't treat this project as standard way to writing projects in common lisp. Anyway, no one except me willing to use this module for now. But maybe that will change in the future... or not, who knows!? :-)

## Related video

[![video-0](https://img.youtube.com/vi/0uKptCcTqCc/0.jpg)](https://www.youtube.com/watch?v=0uKptCcTqCc "video-0")

[![video-1](https://img.youtube.com/vi/8KHEwySzi9o/0.jpg)](https://www.youtube.com/watch?v=8KHEwySzi9o "video-1")

## Tests and Profiling

```
CL-USER> (asdf:test-system :decision-tree)

Running test suite DECISION-TREE-SUITE
 Running test CLASS-IN-PACKAGE ....
 Running test ENSURE-DECISION-TREE .
 Running test CHANGE-CONTROL-STRING ..
 Running test ELEMENT-SLOTS .......
 Running test DECISION-TO-AND-FROM-TREE ....
 Running test CRITERIA-TO-AND-FROM-TREE ....
 Running test CRITERIA-CODE-TO-DECISION-WITHOUT-TREE .
 Running test CRITERIA-CODE-TO-DECISION-IN-TREE ............
 Running test SIDE-EFFECT-REMOVING-CRITERIA-FROM-TREE .
 Running test CALCULATION-CODES-IN-RELATIONS ..................
 Running test ANSWER-RESPONSE ............
 Running test STREAM-AND-OTHERS .....
 Running test CONVERT-TO-CONS-TREE ..
 Running test REMOVE-DECISION-TREE ..
 Did 75 checks.
    Pass: 75 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
measuring PROFILE overhead..done
  seconds  |     gc     |   consed  | calls |  sec/call  |  name
-------------------------------------------------------
     0.004 |      0.000 | 1,374,592 |     9 |   0.000437 | DECISION-TREE:POPULATE-RELATIONS
     0.003 |      0.000 | 1,092,976 |    92 |   0.000032 | DECISION-TREE:DECISION-TREE
     0.001 |      0.000 |         0 |     5 |   0.000198 | DECISION-TREE:ANSWER
     0.001 |      0.000 |    32,768 |   194 |   0.000005 | DECISION-TREE:CRITERIONS
     0.001 |      0.000 |         0 |    29 |   0.000030 | DECISION-TREE:DECISION-TO-TREE
     0.000 |      0.000 |    32,768 |    34 |   0.000000 | DECISION-TREE:CRITERIA-CODE
     0.000 |      0.000 |         0 |    55 |   0.000000 | DECISION-TREE:RELATIONS
     0.000 |      0.000 |         0 |    17 |   0.000000 | (SETF DECISION-TREE:RELATIONS)
     0.000 |      0.000 |    32,768 |     5 |   0.000000 | DECISION-TREE:CODE-TREE
     0.000 |      0.000 |         0 |    15 |   0.000000 | DECISION-TREE:NEGATIVE-ANSWER
     0.000 |      0.000 |         0 |     1 |   0.000000 | DECISION-TREE:REMOVE-ALL-DECISION-TREE
     0.000 |      0.000 |         0 |    58 |   0.000000 | DECISION-TREE:COUNT-CRITERIA-CODE
     0.000 |      0.000 |         0 |   117 |   0.000000 | DECISION-TREE:DECISIONS
     0.000 |      0.000 |         0 |     1 |   0.000000 | (SETF DECISION-TREE:DECISIONS)
     0.000 |      0.000 |         0 |    27 |   0.000000 | DECISION-TREE:POSITIVE-ANSWER
     0.000 |      0.000 |         0 |    38 |   0.000000 | (SETF DECISION-TREE:CRITERIONS)
     0.000 |      0.000 |         0 |    36 |   0.000000 | DECISION-TREE:DECISION-FROM-ANSWER
     0.000 |      0.000 |         0 |     2 |   0.000000 | DECISION-TREE:UNKNOWN
     0.000 |      0.000 |         0 |     2 |   0.000000 | (SETF DECISION-TREE:UNKNOWN)
     0.000 |      0.000 |         0 |     1 |   0.000000 | DECISION-TREE:PRINT-DECISION
     0.000 |      0.000 |         0 |     2 |   0.000000 | DECISION-TREE:DECISION-FROM-INTERACTIVE
     0.000 |      0.000 |    32,720 |    15 |   0.000000 | DECISION-TREE:REMOVE-CRITERIA-FROM-DECISION-IN-TREE
     0.000 |      0.000 |         0 |     1 |   0.000000 | DECISION-TREE:DESCRIPTIONS
     0.000 |      0.000 |         0 |     1 |   0.000000 | (SETF DECISION-TREE:DESCRIPTIONS)
     0.000 |      0.000 |         0 |     6 |   0.000000 | DECISION-TREE:REMOVE-CRITERIA-FROM-TREE
     0.000 |      0.000 |         0 |   175 |   0.000000 | DECISION-TREE:CODE
     0.000 |      0.000 |         0 |     1 |   0.000000 | (SETF DECISION-TREE:CODE)
     0.000 |      0.000 |         0 |     5 |   0.000000 | DECISION-TREE:QUESTION
     0.000 |      0.000 |    32,768 |     1 |   0.000000 | (SETF DECISION-TREE:QUESTION)
     0.000 |      0.000 |         0 |     8 |   0.000000 | DECISION-TREE:QUESTION-CRITERIA-CODE
     0.000 |      0.000 |         0 |    57 |   0.000000 | DECISION-TREE:DECISION-FROM-RELATIONS
     0.000 |      0.000 |         0 |    69 |   0.000000 | DECISION-TREE:CRITERIA-FROM-TREE
     0.000 |      0.000 |         0 |     1 |   0.000000 | DECISION-TREE:REMOVE-DECISION-TREE
     0.000 |      0.000 |         0 |    26 |   0.000000 | DECISION-TREE:CRITERIA-TO-TREE
     0.000 |      0.000 |         0 |    74 |   0.000000 | DECISION-TREE:DECISION-FROM-TREE
     0.000 |      0.000 |         0 |    16 |   0.000000 | DECISION-TREE:RECORDS
     0.000 |      0.000 |         0 |    14 |   0.000000 | (SETF DECISION-TREE:RECORDS)
     0.000 |      0.000 |    32,768 |    17 |   0.000000 | DECISION-TREE:DECISION-FROM-ANSWERS
     0.000 |      0.000 |         0 |    28 |   0.000000 | DECISION-TREE:CRITERIA-CODES
     0.000 |      0.000 |         0 |     9 |   0.000000 | DECISION-TREE:NAME
     0.000 |      0.000 |         0 |     1 |   0.000000 | (SETF DECISION-TREE:NAME)
     0.000 |      0.000 |         0 |   108 |   0.000000 | DECISION-TREE:CRITERIA-TO-DECISION-IN-TREE
     0.000 |      0.000 |    32,768 |     4 |   0.000000 | DECISION-TREE:REMOVE-DECISION-FROM-TREE
-------------------------------------------------------
     0.010 |      0.000 | 2,696,896 | 1,377 |            | Total

estimated total profiling overhead: 0.00 seconds
overhead estimation parameters:
  1.8e-8s/call, 9.94e-7s total profiling, 3.8e-7s internal profiling
```

## License

Public Domain
