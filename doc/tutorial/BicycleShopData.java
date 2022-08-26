package org.logicng.tutorial;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class BicycleShopData {

    final FormulaFactory f = new FormulaFactory();

    // frames
    final Variable frame = this.f.variable("frame");
    final Variable f1 = this.f.variable("f1");
    final Variable f2 = this.f.variable("f2");
    final Variable f3 = this.f.variable("f3");

    final Formula f_equiv = this.f.equivalence(this.frame, this.f.or(this.f1, this.f2, this.f3));
    final Formula f_exo = this.f.exo(this.f1, this.f2, this.f3);

    // handlebars
    final Variable handlebar = this.f.variable("handlebar");
    final Variable h1 = this.f.variable("h1");
    final Variable h2 = this.f.variable("h2");
    final Variable h3 = this.f.variable("h3");
    final Variable h4 = this.f.variable("h4");
    final Variable h5 = this.f.variable("h5");

    final Formula h_equiv = this.f.equivalence(this.handlebar, this.f.or(this.h1, this.h2, this.h3, this.h4, this.h5));
    final Formula h_exo = this.f.exo(this.h1, this.h2, this.h3, this.h4, this.h5);

    // saddles
    final Variable saddle = this.f.variable("saddle");
    final Variable s1 = this.f.variable("s1");
    final Variable s2 = this.f.variable("s2");
    final Variable s3 = this.f.variable("s3");
    final Variable s4 = this.f.variable("s4");

    final Formula s_equiv = this.f.equivalence(this.saddle, this.f.or(this.s1, this.s2, this.s3, this.s4));
    final Formula s_exo = this.f.exo(this.s1, this.s2, this.s3, this.s4);

    // front wheels
    final Variable frontWheel = this.f.variable("frontWheel");
    final Variable wf1 = this.f.variable("wf1");
    final Variable wf2 = this.f.variable("wf2");
    final Variable wf3 = this.f.variable("wf3");
    final Variable wf4 = this.f.variable("wf4");
    final Variable wf5 = this.f.variable("wf5");

    final Formula wf_equiv = this.f.equivalence(this.frontWheel, this.f.or(this.wf1, this.wf2, this.wf3, this.wf4, this.wf5));
    final Formula wf_exo = this.f.exo(this.wf1, this.wf2, this.wf3, this.wf4, this.wf5);

    // back wheels
    final Variable backWheel = this.f.variable("backWheel");
    final Variable wb1 = this.f.variable("wb1");
    final Variable wb2 = this.f.variable("wb2");
    final Variable wb3 = this.f.variable("wb3");
    final Variable wb4 = this.f.variable("wb4");
    final Variable wb5 = this.f.variable("wb5");

    final Formula wb_equiv = this.f.equivalence(this.backWheel, this.f.or(this.wb1, this.wb2, this.wb3, this.wb4, this.wb5));
    final Formula wb_exo = this.f.exo(this.wb1, this.wb2, this.wb3, this.wb4, this.wb5);

    // bike bells
    final Variable bell = this.f.variable("bell");
    final Variable b1 = this.f.variable("b1");
    final Variable b2 = this.f.variable("b2");
    final Variable b3 = this.f.variable("b3");

    final Formula b_equiv = this.f.equivalence(this.bell, this.f.or(this.b1, this.b2, this.b3));
    final Formula b_amo = this.f.amo(this.b1, this.b2, this.b3);

    // luggage rack
    final Variable luggageRack = this.f.variable("luggageRack");
    final Variable r1 = this.f.variable("r1");
    final Variable r2 = this.f.variable("r2");
    final Variable r3 = this.f.variable("r3");
    final Variable r4 = this.f.variable("r4");

    final Formula r_equiv = this.f.equivalence(this.luggageRack, this.f.or(this.r1, this.r2, this.r3, this.r4));
    final Formula r_amo = this.f.amo(this.r1, this.r2, this.r3, this.r4);

    // colors
    final Variable color = this.f.variable("color");
    final Variable c1 = this.f.variable("c1");
    final Variable c2 = this.f.variable("c2");
    final Variable c3 = this.f.variable("c3");
    final Variable c4 = this.f.variable("c4");

    final Formula c_equiv = this.f.equivalence(this.color, this.f.or(this.c1, this.c2, this.c3, this.c4));
    final Formula c_exo = this.f.exo(this.c1, this.c2, this.c3, this.c4);

    // implications
    final Formula formula1 = this.f.implication(this.f1, this.f.and(this.s1.negate(), this.h3.negate()));
    final Formula formula2 = this.f.implication(this.f1, this.f.or(this.luggageRack.negate(), this.r4));
    final Formula formula3 = this.f.implication(this.f3, this.h5.negate());
    final Formula formula4 = this.f.implication(this.h5, this.bell.negate());
    final Formula formula5 = this.f.implication(this.r4, this.f.or(this.b2.negate(), this.f2, this.f3));

    final Formula formula6 = this.f.equivalence(this.wf1, this.wb1);
    final Formula formula7 = this.f.equivalence(this.wf2, this.wb2);
    final Formula formula8 = this.f.equivalence(this.wf3, this.wb3);
    final Formula formula9 = this.f.equivalence(this.wf4, this.wb4);
    final Formula formula10 = this.f.equivalence(this.wf5, this.wb5);

    final Formula formula11 = this.f.implication(this.frame, this.f.or(this.wf2, this.wf3, this.wf4, this.wf5));
    final Formula formula12 = this.f.implication(this.wf5, this.f3);

    // all formulas
    final List<Formula> formulas =
            Arrays.asList(this.f_equiv, this.f_exo, this.h_equiv, this.h_exo, this.s_equiv, this.s_exo, this.wf_equiv, this.wf_exo, this.wb_equiv, this.wb_exo,
                    this.b_equiv, this.b_amo, this.r_equiv, this.r_amo, this.c_equiv, this.c_exo, this.formula1, this.formula2, this.formula3, this.formula4,
                    this.formula5, this.formula6, this.formula7, this.formula8, this.formula9, this.formula10, this.formula11, this.formula12);

    // all features
    final SortedSet<Variable> featureClasses = new TreeSet<>(Arrays.asList(this.frame, this.handlebar, this.saddle, this.frontWheel, this.backWheel,
            this.bell, this.luggageRack, this.color));
}
