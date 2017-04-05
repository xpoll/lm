package io.terminus.parana.trade.fsm;

import io.terminus.parana.trade.fsm.Trigger;

public abstract class FSM {
   public abstract int next(int var1, Trigger var2);
}
