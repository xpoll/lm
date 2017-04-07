package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.dozer.DozerBeanMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.enums.OrderStatus;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.ShopOrderRefund;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;

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
