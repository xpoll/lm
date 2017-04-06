package io.terminus.galaxy.web.core.trade.action.onsalereturns;

import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.*;
import io.terminus.parana.order.service.OrderActionReadService;
import io.terminus.parana.order.service.OrderReadService;
import io.terminus.parana.order.service.OrderWriteService;
import io.terminus.parana.order.util.Params;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * 卖家同意退货
 *
 * 退货单: 更新状态为SKU_REFUND_AGREE_RETURNS, 更新操作集合(每次状态变更都要更新一把操作)
 * 子订单: 不更新状态, 不更新操作
 * 主订单: 不更新状态, 不更新操作
 *
 * Created by zhanghecheng on 16/3/10. <br>
 * Mail: zhanghecheng@terminus.io
 */
@Component
public class SkuOrderAgreeReturnsAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    @Autowired
    public SkuOrderAgreeReturnsAction(
            OrderReadService orderReadService,
            OrderActionReadService orderActionReadService,
            OrderWriteService orderWriteService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
    }


    @Override
    public Boolean execute(Map<String, Serializable> context) {

        Long skuOrderRefundId = Long.valueOf(Params.get(context, "orderId").toString());
        String sellerNote = Params.get(context, "sellerNote");

        List<Order> orders = Lists.newArrayList();

        /**
         * 查找相关的订单
         */
        SkuOrderRefund skuOrderRefund = (SkuOrderRefund)findOrderById(OrderType.SKU_ORDER_REFUND, skuOrderRefundId);
        SkuOrder skuOrder = (SkuOrder)findOrderById(OrderType.SKU_ORDER, skuOrderRefund.getParentId());
        ShopOrder shopOrder = (ShopOrder)findOrderById(OrderType.SHOP_ORDER, skuOrder.getParentId());

        /**
         * 退货单: 更新状态为SKU_REFUND_AGREE_RETURNS, 更新操作集合(每次状态变更都要更新一把操作)
         */
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_AGREE_RETURNS.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        skuOrderRefund.setSellerNote(sellerNote);
        orders.add(skuOrderRefund);

        /**
         * 子订单: 不更新状态, 不更新操作
         */

        /**
         * 主订单: 不更新状态, 不更新操作
         */

        orderWriteService.update(orders, null);

        return Boolean.TRUE;
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
     * 根据订单类型和id找到相应的订单
     * @param orderType
     * @param orderId
     * @return
     */
    private Order findOrderById(OrderType orderType, Long orderId){
        switch (orderType){
            case SHOP_ORDER:
                Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(orderId);
                if (!shopOrderR.isSuccess()) {
                    throw new ServiceException(shopOrderR.getError());
                }
                return shopOrderR.getResult();

            case SHOP_ORDER_REFUND:
                Response<ShopOrderRefund> shopOrderRefundResponse=orderReadService.findShopOrderRefundById(orderId);
                if(shopOrderRefundResponse.isSuccess()){
                    throw new ServiceException(shopOrderRefundResponse.getError());
                }
                return shopOrderRefundResponse.getResult();

            case SKU_ORDER:
                Response<SkuOrder> skuOrderR = orderReadService.findSkuOrderById(orderId);
                if (!skuOrderR.isSuccess()) {
                    throw new ServiceException(skuOrderR.getError());
                }
                return skuOrderR.getResult();

            case SKU_ORDER_REFUND:
                Response<SkuOrderRefund> skuOrderRefundR = orderReadService.findSkuOrderRefundById(orderId);
                if (!skuOrderRefundR.isSuccess()) {
                    throw new ServiceException(skuOrderRefundR.getError());
                }
                return skuOrderRefundR.getResult();
            default:
                throw new NotImplementedException();
        }
    }
}
