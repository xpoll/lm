package cn.blmdz.wolf.order.engine;

import java.io.Serializable;
import java.util.Map;

public abstract interface TradeAction<T extends Serializable> {
	public abstract T execute(Map<String, Serializable> paramMap);
}
