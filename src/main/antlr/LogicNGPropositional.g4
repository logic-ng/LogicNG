// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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

VARIABLE : [A-Za-z0-9_@#][A-Za-z0-9_#]*;
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
