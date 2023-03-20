module logicng {
    requires org.antlr.antlr4.runtime;

    exports org.logicng;

    exports org.logicng.backbones;

    exports org.logicng.cardinalityconstraints;

    exports org.logicng.collections;

    exports org.logicng.configurations;

    exports org.logicng.datastructures;
    exports org.logicng.datastructures.ubtrees;

    exports org.logicng.explanations;
    exports org.logicng.explanations.drup;
    exports org.logicng.explanations.mus;
    exports org.logicng.explanations.smus;

    exports org.logicng.formulas;
    exports org.logicng.formulas.cache;
    exports org.logicng.formulas.printer;

    exports org.logicng.functions;

    exports org.logicng.graphs.algorithms;
    exports org.logicng.graphs.datastructures;
    exports org.logicng.graphs.generators;
    exports org.logicng.graphs.io;

    exports org.logicng.handlers;

    exports org.logicng.io.graphical;
    exports org.logicng.io.graphical.generators;
    exports org.logicng.io.parsers;
    exports org.logicng.io.readers;
    exports org.logicng.io.writers;

    exports org.logicng.knowledgecompilation.bdds;
    exports org.logicng.knowledgecompilation.bdds.datastructures;
    exports org.logicng.knowledgecompilation.bdds.functions;
    exports org.logicng.knowledgecompilation.bdds.jbuddy;
    exports org.logicng.knowledgecompilation.bdds.orderings;
    exports org.logicng.knowledgecompilation.dnnf;
    exports org.logicng.knowledgecompilation.dnnf.datastructures;
    exports org.logicng.knowledgecompilation.dnnf.datastructures.dtree;
    exports org.logicng.knowledgecompilation.dnnf.functions;

    exports org.logicng.modelcounting;

    exports org.logicng.np;

    exports org.logicng.predicates;
    exports org.logicng.predicates.satisfiability;

    exports org.logicng.primecomputation;

    exports org.logicng.propositions;

    exports org.logicng.pseudobooleans;

    exports org.logicng.solvers;
    exports org.logicng.solvers.datastructures;
    exports org.logicng.solvers.functions;
    exports org.logicng.solvers.maxsat.algorithms;
    exports org.logicng.solvers.maxsat.encodings;
    exports org.logicng.solvers.sat;

    exports org.logicng.transformations;
    exports org.logicng.transformations.cnf;
    exports org.logicng.transformations.dnf;
    exports org.logicng.transformations.qe;
    exports org.logicng.transformations.qmc;
    exports org.logicng.transformations.simplification;

    exports org.logicng.util;
}
