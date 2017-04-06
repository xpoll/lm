package io.terminus.galaxy.web.core.trade.engine;

import com.google.common.base.Throwables;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.service.OrderActionReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Desc: actionInstanceId -> action 路由器
 * Mail: F@terminus.io
 * Data: 16/3/4
 * Author: yangzefeng
 */
@Component @Slf4j
public class ActionRouter {

    private final ActionRegistry actionRegistry;

    private final OrderActionReadService orderActionReadService;

    private final static String RESULT_KEY = "result";

    @Autowired
    public ActionRouter(ActionRegistry actionRegistry,
                        OrderActionReadService orderActionReadService) {
        this.actionRegistry = actionRegistry;
        this.orderActionReadService = orderActionReadService;
    }

    public Response<Map<String, Object>> updateOrder(Long actionInstanceId, Map<String, Object> context) {
        try {
            OrderActionInstance actionInstance = getActionInstance(actionInstanceId);
            TradeAction action = actionRegistry.getAction(actionInstance.getAction());
            context.put(RESULT_KEY ,action.execute(context));

            return Response.ok(context);
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to update order by actionInstanceId {}, context {}, cause:{}",
                    actionInstanceId, context, Throwables.getStackTraceAsString(e));
            return Response.fail("update.order.fail");
        }
    }

    // TODO: 16/3/4 need cache
    private OrderActionInstance getActionInstance(Long actionInstanceId) {
        Response<OrderActionInstance> actionR = orderActionReadService.findById(actionInstanceId);
        if (!actionR.isSuccess()) {
            throw new ServiceException(actionR.getError());
        }
        return actionR.getResult();
    }
}
