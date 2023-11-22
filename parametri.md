| label | decay  | parameter | min | max | neutral | rectify | synch | peso | rate (fps) |
--------- --------------------------------------------------------------------------------
| L1:6  | linear |   1.0e-6  | 0.0 | 1.0 |   0.5   |  true   | true  | 0.15 |    1000    |
| L1:5  | linear |   1.0e-5  | 0.0 | 1.0 |   0.5   |  true   | true  | 0.15 |    1000    |
| L1:4  | linear |   1.0e-4  | 0.0 | 1.0 |   0.5   |  true   | true  | 0.15 |    1000    |
| none  | none   |           | 0.0 | 0.3 |   0.0   |  true   | true  | 0.04 |    1000    |
| step  | step   |           | 0.0 | 1.0 |   0.5   |  true   | true  | 0.50 |    1000    |
| E1:4  | exp    |   1.0e4   | 0.0 | 0.3 |   0.0   |  true   | true  | 0.04 |    1000    |
| E1:5  | exp    |   1.0e5   | 0.0 | 0.3 |   0.0   |  true   | true  | 0.04 |    1000    |
| E5:5  | exp    |   5.0e5   | 0.0 | 0.3 |   0.0   |  true   | true  | 0.04 |    1000    |

Decay parameter: 
* in us per exp
* "slope" del decadimento per t (us^-1) 
