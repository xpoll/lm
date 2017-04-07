package cn.blmdz.rabbit.web.core.trade.engine;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import cn.blmdz.wolf.order.engine.TradeAction;

/**
 * Desc: 订单操作注册中心
 * Mail: F@terminus.io
 * Data: 16/3/4
 * Author: yangzefeng
 */
@Component
public class ActionRegistry {

    private final ApplicationContext applicationContext;

    private Map<String, TradeAction> actionMap = null;

    @Autowired
    public ActionRegistry (ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    public TradeAction getAction(String beanName) {

        if(actionMap==null){
            actionMap = applicationContext.getBeansOfType(TradeAction.class);
        }
        return actionMap.get(beanName);
    }
}
