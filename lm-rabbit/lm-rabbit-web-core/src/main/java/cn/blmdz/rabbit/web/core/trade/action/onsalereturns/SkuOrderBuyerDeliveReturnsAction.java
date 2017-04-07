package cn.blmdz.rabbit.web.core.trade.action.onsalereturns;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

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
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;

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
