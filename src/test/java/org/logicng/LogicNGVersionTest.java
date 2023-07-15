// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mockStatic;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit tests for {@link LogicNGVersion}.
 * @version 2.1.0
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class LogicNGVersionTest {

    @Test
    public void testMajor() {
        try (final MockedStatic<LogicNGVersion> versionMock = mockStatic(LogicNGVersion.class)) {
            versionMock.when(LogicNGVersion::major).thenCallRealMethod();
            versionMock.when(() -> LogicNGVersion.major(anyString())).thenCallRealMethod();

            versionMock.when(LogicNGVersion::version).thenReturn("2.0.0");
            assertThat(LogicNGVersion.major()).isEqualTo(2);
            versionMock.when(LogicNGVersion::version).thenReturn("2.0.0-SNAPSHOT");
            assertThat(LogicNGVersion.major()).isEqualTo(2);
            versionMock.when(LogicNGVersion::version).thenReturn("2.1.3");
            assertThat(LogicNGVersion.major()).isEqualTo(2);
            versionMock.when(LogicNGVersion::version).thenReturn("42.7.19");
            assertThat(LogicNGVersion.major()).isEqualTo(42);
            versionMock.when(LogicNGVersion::version).thenReturn("0.0.0");
            assertThat(LogicNGVersion.major()).isEqualTo(0);
            versionMock.when(LogicNGVersion::version).thenReturn("A.0.1");
            assertThat(LogicNGVersion.major()).isEqualTo(-1);
        }
    }

    @Test
    public void testMinor() {
        try (final MockedStatic<LogicNGVersion> versionMock = mockStatic(LogicNGVersion.class)) {
            versionMock.when(LogicNGVersion::minor).thenCallRealMethod();
            versionMock.when(() -> LogicNGVersion.minor(anyString())).thenCallRealMethod();

            versionMock.when(LogicNGVersion::version).thenReturn("2.0.0");
            assertThat(LogicNGVersion.minor()).isEqualTo(0);
            versionMock.when(LogicNGVersion::version).thenReturn("2.3.0-SNAPSHOT");
            assertThat(LogicNGVersion.minor()).isEqualTo(3);
            versionMock.when(LogicNGVersion::version).thenReturn("2.1.3");
            assertThat(LogicNGVersion.minor()).isEqualTo(1);
            versionMock.when(LogicNGVersion::version).thenReturn("42.7.19");
            assertThat(LogicNGVersion.minor()).isEqualTo(7);
            versionMock.when(LogicNGVersion::version).thenReturn("0.123.0");
            assertThat(LogicNGVersion.minor()).isEqualTo(123);
            versionMock.when(LogicNGVersion::version).thenReturn("2.A.1");
            assertThat(LogicNGVersion.minor()).isEqualTo(-1);
        }
    }

    @Test
    public void testPatch() {
        try (final MockedStatic<LogicNGVersion> versionMock = mockStatic(LogicNGVersion.class)) {
            versionMock.when(LogicNGVersion::patch).thenCallRealMethod();
            versionMock.when(() -> LogicNGVersion.patch(anyString())).thenCallRealMethod();

            versionMock.when(LogicNGVersion::version).thenReturn("2.0.3");
            assertThat(LogicNGVersion.patch()).isEqualTo(3);
            versionMock.when(LogicNGVersion::version).thenReturn("2.3.3-SNAPSHOT");
            assertThat(LogicNGVersion.patch()).isEqualTo(3);
            versionMock.when(LogicNGVersion::version).thenReturn("2.1.0");
            assertThat(LogicNGVersion.patch()).isEqualTo(0);
            versionMock.when(LogicNGVersion::version).thenReturn("42.7.19");
            assertThat(LogicNGVersion.patch()).isEqualTo(19);
            versionMock.when(LogicNGVersion::version).thenReturn("0.123.22");
            assertThat(LogicNGVersion.patch()).isEqualTo(22);
            versionMock.when(LogicNGVersion::version).thenReturn("2.2.A");
            assertThat(LogicNGVersion.patch()).isEqualTo(-1);
        }
    }

    @Test
    public void testSnapshot() {
        try (final MockedStatic<LogicNGVersion> versionMock = mockStatic(LogicNGVersion.class)) {
            versionMock.when(LogicNGVersion::snapshot).thenCallRealMethod();
            versionMock.when(() -> LogicNGVersion.snapshot(anyString())).thenCallRealMethod();

            versionMock.when(LogicNGVersion::version).thenReturn("2.0.3");
            assertThat(LogicNGVersion.snapshot()).isFalse();
            versionMock.when(LogicNGVersion::version).thenReturn("2.0.3-SNAPSHOT");
            assertThat(LogicNGVersion.snapshot()).isTrue();
            versionMock.when(LogicNGVersion::version).thenReturn("2.0.3-HUGO");
            assertThat(LogicNGVersion.snapshot()).isFalse();
        }
    }
}
