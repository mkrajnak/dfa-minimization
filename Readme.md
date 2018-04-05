<!-- Readme written in markdown
DFA-2-MKA, project for FLP course at FIT BUT
Martin Krajnak, xkrajn02@stud.fit.vutbr.cz -->

# Deterministic Finite State Automata (DFA) Minimization

### Usage:
  ```
  Usage: ./dfa-2-mka [-i|-t] [file]
  ```

### Algorithm inspired by TIN course at FIT BUT:

  1. DFA is obtained via file or via stdin by excluding [file] arg
  1. Input is checked and parsed to internal DFA structure
  1. DFA is converted to well specified DFA (if not already)
  1. State are initially splitted to two groups (classes):
    * Final States
    * Non-Final States
  1. Run until two separate iterations yields the same result:
    * For each symbol in alphabet:
      * For each class in classes:
        * Calculate corresponding group of Final states for given symbol
        * If the final states resides within different classes, split the class
  1. Obtained classes are sorted by length and renamed from 1
  1. DFA is rebuilt from the original DFA data and calculated classes
  1. DFA is printed out


### Examples
Sample input | Output with arg `-i` | Output with arg `-t`
------------ | -------------- | ---------
`1,2,3`<br>`1`<br> `1,3`<br>`1,b,1`<br>`1,a,2`<br>`2,a,3`<br>`2,b,2`<br>`3,a,2`<br>`3,b,3` | `1,2,3`<br>`1`<br>`1,3`<br>`1,b,1`<br>`1,a,2 `<br> `2,a,3`<br>`2,b,2` <br>`3,a,2`<br>`3,b,3` | `1,2`<br>`1`<br>`1 `<br>`1,a,2`<br>`1,b,1`<br>`2,a,1`<br>`2,b,2`


 [DFA Minimization](https://en.wikipedia.org/wiki/DFA_minimization)
