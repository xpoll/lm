package io.terminus.galaxy.web.core.trade.action;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Desc: 卖家不同意sku退款单退款
 * 在这个action里面,skuOrder状态不变, 把申请退款的操作加回来
 * skuOrderRefund状态流转到 SKU_REFUND_REFUSE_REFUND,
 * shopOrder的需要做比较复杂的判断:
 * 如果其他skuOrderRefund状态为 SKU_REFUND_REFUSE_REFUND | SKU_REFUND_CANCEL_BY_REFUND | SKU_REFUND_UNDO
 * shopOrder的 DELIVE 操作就应该加回来, 否则,shopOrder就不需要更新
 * @see io.terminus.galaxy.order.enums.OrderStatus 订单状态
 * @see io.terminus.galaxy.order.enums.OrderAction 订单操作
 * Mail: houly@terminus.io
 * author: Hou Luyao, yangzefeng
 * Date: 16/3/9.
 */
@Component
public class SkuOrderRefuseRefundAction implements TradeAction<Boolean> {
    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static Set<Long> JUDGE_STATUS = Sets.newHashSet(
            OrderStatus.SKU_REFUND_REFUSE_REFUND.value(),
            OrderStatus.SKU_REFUND_CANCEL_BY_REFUND.value(),
            OrderStatus.SKU_REFUND_UNDO.value()
    );

    @Autowired
    public SkuOrderRefuseRefundAction(
                                        OrderReadService orderReadService,
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

        //子订单ID
        Long skuOrderId = skuOrderRefund.getParentId();

        //子订单
        Response<SkuOrder> skuOrderR = orderReadService.findSkuOrderById(skuOrderId);
        if (!skuOrderR.isSuccess()) {
            throw new ServiceException(skuOrderR.getError());
        }
        SkuOrder skuOrder = skuOrderR.getResult();
        //订单ID
        Long shopOrderId = skuOrder.getParentId();
        //订单
        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(shopOrderId);
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }

        ShopOrder shopOrder = shopOrderR.getResult();

        //更新子订单退款单
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_REFUSE_REFUND.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        orders.add(skuOrderRefund);

        //更新子订单操作
        skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
        orders.add(skuOrder);

        //更新主订单的操作集合
        setShopOrderNextActions(shopOrder, skuOrderRefund.getId());
        orders.add(shopOrder);

        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    private void setShopOrderNextActions(ShopOrder shopOrder, Long currentSkuOrderRefundId) {
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrder.getId());
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }
        List<SkuOrder> skuOrders = skuOrdersR.getResult();
        List<Long> skuOrderIds = new ArrayList<>();
        skuOrderIds.addAll(Lists.transform(skuOrders, new Function<SkuOrder, Long>() {
            @Override
            public Long apply(SkuOrder input) {
                return input.getId();
            }
        }));
        Response<List<SkuOrderRefund>> skuOrderRefundsR = orderReadService.findSkuOrderRefundByParentIds(skuOrderIds);
        if (!skuOrderRefundsR.isSuccess()) {
            throw new ServiceException(skuOrderRefundsR.getError());
        }
        List<SkuOrderRefund> refunds = skuOrderRefundsR.getResult();

        boolean canDeliver = true;
        for (SkuOrderRefund refund : refunds) {
            if (Objects.equals(currentSkuOrderRefundId, refund.getId())) {
                continue;
            }
            if (!JUDGE_STATUS.contains(refund.getNodeInstanceId())) {
                canDeliver = false;
                break;
            }
        }
        if (canDeliver) {
            //把DELIVE 这个操作加回来
            shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
        }
    }

    /**
     * 获取下一步的action列表
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
