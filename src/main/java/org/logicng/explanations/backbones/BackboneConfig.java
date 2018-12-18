package org.logicng.explanations.backbones;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

public class BackboneConfig extends Configuration {

    public enum Algorithm {
        ENUMERATION, ITERATIVE_PLAIN, ITERATIVE_ONE_TEST, ITERATIVE_PLUS, CHUNKING, CORE
    }

    final Algorithm algorithm;

    public BackboneConfig(final Builder builder) {
        super(ConfigurationType.BACKBONE);
        this.algorithm = builder.algorithm;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("BackboneConfig{").append(System.lineSeparator());
        sb.append("algorithm=").append(this.algorithm).append(System.lineSeparator());
        sb.append("}").append(System.lineSeparator());
        return sb.toString();
    }

    public static class Builder {

        private Algorithm algorithm = Algorithm.ENUMERATION;

        public Builder algorithm(final Algorithm algorithm) {
            this.algorithm = algorithm;
            return this;
        }

        public BackboneConfig build() {
            return new BackboneConfig(this);
        }
    }
}
