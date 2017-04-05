package io.terminus.parana.order.engine;

import java.io.Serializable;
import java.util.Map;

public interface TradeAction {
   Serializable execute(Map var1);
}
