package io.terminus.galaxy.web.core.trade.action;

import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.ShopOrderRefund;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import org.dozer.DozerBeanMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 整单退款取消
 * Mail: houly@terminus.io
 * author: Hou Luyao
 * Date: 16/3/9.
 */

@Component
public class OrderUndoRefundAction implements TradeAction<Boolean> {
    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static DozerBeanMapper MAPPER = new DozerBeanMapper();

    @Autowired
    public OrderUndoRefundAction(OrderReadService orderReadService,
                                           OrderActionReadService orderActionReadService,
                                           OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }
    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        Response<ShopOrderRefund> shopOrderRefundR = orderReadService.findShopOrderRefundById(orderId);
        if (!shopOrderRefundR.isSuccess()) {
            throw new ServiceException(shopOrderRefundR.getError());
        }
        //查询出来退款单
        ShopOrderRefund shopOrderRefund = shopOrderRefundR.getResult();
        shopOrderRefund.setNodeInstanceId(OrderStatus.CANCEL_BY_REFUND.value());
        shopOrderRefund.setNextActionInstanceIds(getNextActions(shopOrderRefund.getNodeInstanceId(), null));

        //订单信息和子订单信息
        Long shopOrderId = shopOrderRefund.getParentId();

        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(shopOrderId);
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrderId);
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }
        ShopOrder shopOrder = shopOrderR.getResult();

        List<SkuOrder> skuOrders = skuOrdersR.getResult();

        List<Order> orders = updateShopOrderAndSkuOrders(shopOrder, skuOrders);

        orders.add(shopOrderRefund);

        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    /**
     * 主订单子订单状态同时往下流转
     * @param shopOrder
     * @param skuOrders
     * @return
     */
    private List<Order> updateShopOrderAndSkuOrders(ShopOrder shopOrder, List<SkuOrder> skuOrders){
        List<Order> orders = Lists.newArrayList();
        shopOrder.setNodeInstanceId(OrderStatus.WAIT_FOR_DELIVE.value());
        String nextAction = getNextActions(shopOrder.getNodeInstanceId(), null);
        shopOrder.setNextActionInstanceIds(nextAction);

        orders.add(shopOrder);
        for(SkuOrder skuOrder : skuOrders){
            skuOrder.setNodeInstanceId(OrderStatus.WAIT_FOR_DELIVE.value());
            skuOrder.setNextActionInstanceIds(nextAction);
            orders.add(skuOrder);
        }
        return orders;
    }

    /**
     * 获取下一步的aciton列表
     * @param nid
     * @return
     */
    private String getNextActions(Long nid, List<Long> aidsRemoved) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }
}
