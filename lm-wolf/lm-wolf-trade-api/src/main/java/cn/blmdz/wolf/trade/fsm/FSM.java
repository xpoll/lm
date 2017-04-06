package cn.blmdz.wolf.trade.fsm;

import cn.blmdz.wolf.trade.fsm.Trigger;

public abstract class FSM {
   public abstract int next(int var1, Trigger var2);
}
