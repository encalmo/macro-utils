package org.encalmo.utils;

import java.util.List;
import java.math.BigDecimal;

public record Order(
    String id,
    String customerId,
    List<Integer> items,
    BigDecimal total) {
}
