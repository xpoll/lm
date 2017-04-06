package io.terminus.galaxy.web.core.trade.action.onsalereturns;

import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderAction;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.*;
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
 * 申请售中退货
 *
 * 1.创建退货单
 * 2.主订单: 状态不变, 操作去掉CONFIRM
 * 3.子订单: 状态不变, 去掉所有的操作, 打开查看逆向单的入口
 *
 * Created by zhanghecheng on 16/3/10. <br>
 * Mail: zhanghecheng@terminus.io
 */
@Component
public class SkuOrderApplyReturnsAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final static DozerBeanMapper MAPPER = new DozerBeanMapper();

    @Autowired
    public SkuOrderApplyReturnsAction(OrderReadService orderReadService,
                                     OrderActionReadService orderActionReadService,
                                     OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long skuOrderId = Long.valueOf(Params.get(context, "orderId").toString());
        String buyerNote = Params.get(context, "buyerNote");
        Integer refundAmount = Params.get(context, "refundAmount");

        /**
         * 获取SkuOrder
         */
        Response<SkuOrder> skuOrderR = orderReadService.findSkuOrderById(skuOrderId);
        if (!skuOrderR.isSuccess()) {
            throw new ServiceException(skuOrderR.getError());
        }
        SkuOrder skuOrder = skuOrderR.getResult();

        /**
         * 获取ShopOrder
         */
        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(skuOrder.getParentId());
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }
        ShopOrder shopOrder = shopOrderR.getResult();


        /**
         * 创建退货单
         */
        SkuOrderRefund skuOrderRefund = initSkuOrderRefund(skuOrder, buyerNote, refundAmount);
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_APPLY_RETURNS.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        orderWriteService.create(skuOrderRefund, null);


        List<Order> orders = Lists.newArrayList();
        /**
         * 主订单: 状态不变, 操作去掉CONFIRM
         */
        shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId(), OrderAction.CONFIRM.value()));
        orders.add(shopOrder);

        /**
         * 子订单: 状态不变, 去掉所有的操作, 打开查看逆向单的入口
         */
        skuOrder.setNextActionInstanceIds(excludeAllActions());
        skuOrder.setHasRefund(1);
        orders.add(skuOrder);

        /**
         * 实际提交更新操作
         */
        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    /**
     * 初始化退货单
     * @param skuOrder
     * @param buyerNote
     * @param refundAmount
     * @return
     */
    private SkuOrderRefund initSkuOrderRefund(SkuOrder skuOrder, String buyerNote, Integer refundAmount){
        SkuOrderRefund skuOrderRefund = MAPPER.map(skuOrder, SkuOrderRefund.class);
        skuOrderRefund.setBuyerNote(buyerNote);
        skuOrderRefund.setRefundAmount(refundAmount);
        skuOrderRefund.setParentId(skuOrder.getId());
        skuOrderRefund.setType(OrderType.SKU_ORDER_REFUND.value());
        return skuOrderRefund;
    }

    /**
     * 获取下一步的aciton列表
     * @param nid
     * @return
     */
    private String getNextActions(Long nid, Long... excludeActionIds) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid, excludeActionIds);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }

    /**
     * 去除掉所有的action列表
     * @return
     */
    private String excludeAllActions(){
        return "[]";
    }
}
