///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

grammar LogicNGPropositional;

options {
  superClass = ParserWithFormula;
}

@parser::header {
  package org.logicng.io.parsers;

  import java.util.LinkedHashSet;
  import org.logicng.formulas.*;
}

@parser::members {
  public Formula getParsedFormula() {
    return formula().f;
  }
}

@lexer::header {
  package org.logicng.io.parsers;

  import org.logicng.formulas.FormulaFactory;
}

formula returns [Formula f]
  : EOF {$f = f.verum();}
  | equiv EOF {$f = $equiv.f;};

constant returns [Formula f]
  : TRUE  {$f = f.verum();}
  | FALSE {$f = f.falsum();};

simp returns [Formula f]
  :	VARIABLE      {$f = f.literal($VARIABLE.text, true);}
  |	constant      {$f = $constant.f;}
  | LBR equiv RBR {$f = $equiv.f;};

lit returns [Formula f]
  :	NOT a = lit {$f = f.not($a.f);}
  |	simp        {$f = $simp.f;};

conj returns [Formula f]
@init{LinkedHashSet<Formula> literals = new LinkedHashSet<>(); }
	:	a = lit {literals.add($a.f);} (AND b = lit {literals.add($b.f);})* {$f = f.and(literals);};

disj returns [Formula f]
@init{LinkedHashSet<Formula> conjunctions = new LinkedHashSet<>();}
  :	a = conj {conjunctions.add($a.f);} (OR b = conj {conjunctions.add($b.f);})* {$f = f.or(conjunctions);};

impl returns [Formula f]
@init{Formula[] operands = new Formula[2];}
  :	a = disj {operands[0] =$a.f;} (IMPL b = impl {operands[1] = $b.f;})? {$f = operands[1] == null ? operands[0] : f.implication(operands[0], operands[1]);};

equiv returns [Formula f]
@init{Formula[] operands = new Formula[2];}
  :	a = impl {operands[0] =$a.f;} (EQUIV b = equiv {operands[1] = $b.f;})? {$f = operands[1] == null ? operands[0] : f.equivalence(operands[0], operands[1]);};

VARIABLE : [A-Za-z0-9_@][A-Za-z0-9_]*;
TRUE     : '$true';
FALSE    : '$false';
LBR      : '(';
RBR      : ')';
NOT      : '~';
AND      : '&';
OR       : '|';
IMPL     : '=>';
EQUIV    : '<=>';
WS       : [ \t\r\n]+ -> skip;

