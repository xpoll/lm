package io.terminus.galaxy.web.core.trade.action.onsalereturns;

import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * 买家发退还货物
 * 1、更新退款单为 已发货
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/10/16
 * Time: 6:17 PM
 */
@Component
public class SkuOrderBuyerDeliveReturnsAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;


    @Autowired
    public SkuOrderBuyerDeliveReturnsAction(OrderReadService orderReadService,
                                    OrderActionReadService orderActionReadService,
                                    OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }


    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        List<Order> orders = Lists.newArrayList();

        Response<SkuOrderRefund> skuOrderRefundR = orderReadService.findSkuOrderRefundById(orderId);
        if (!skuOrderRefundR.isSuccess()) {
            throw new ServiceException(skuOrderRefundR.getError());
        }
        //子订单退款单
        SkuOrderRefund skuOrderRefund = skuOrderRefundR.getResult();

        //更新子订单退款单
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_DELIVE_RETURNS.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        orders.add(skuOrderRefund);

        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    /**
     * 获取下一步的aciton列表
     * @param nid 节点id
     * @return 下一步的action列表json
     */
    private String getNextActions(Long nid, Long... excludeActionIds) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid, excludeActionIds);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }
}
