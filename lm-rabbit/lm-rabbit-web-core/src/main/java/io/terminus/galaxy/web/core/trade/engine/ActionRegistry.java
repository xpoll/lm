package io.terminus.galaxy.web.core.trade.engine;

import io.terminus.parana.order.engine.TradeAction;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.Map;

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
