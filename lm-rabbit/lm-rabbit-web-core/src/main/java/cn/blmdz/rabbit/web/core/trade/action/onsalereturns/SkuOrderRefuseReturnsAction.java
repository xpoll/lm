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
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.ShopOrderRefund;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * 卖家拒绝退货申请
 *
 * 退货单: 更新状态为SKU_REFUND_REFUSE_RETURNS, 更新操作集合
 * 子订单: 状态不更新, 打开所有的操作
 * 主订单: 状态不更新, 判断是否要重新打开CONFIRM操作
 *
 * Created by zhanghecheng on 16/3/10. <br>
 * Mail: zhanghecheng@terminus.io
 */
@Component
public class SkuOrderRefuseReturnsAction extends OnSaleReturnActionBase implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    @Autowired
    public SkuOrderRefuseReturnsAction(
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
        String sellerNote = (String) Params.get(context, "sellerNote");

        List<Order> orders = Lists.newArrayList();

        /**
         * 查找相关的订单
         */
        SkuOrderRefund skuOrderRefund = (SkuOrderRefund)findOrderById(OrderType.SKU_ORDER_REFUND, skuOrderRefundId);
        SkuOrder skuOrder = (SkuOrder)findOrderById(OrderType.SKU_ORDER, skuOrderRefund.getParentId());
        ShopOrder shopOrder = (ShopOrder)findOrderById(OrderType.SHOP_ORDER, skuOrder.getParentId());

        /**
         * 退货单: 更新状态为SKU_REFUND_REFUSE_RETURNS, 更新操作集合
         */
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_REFUSE_RETURNS.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        skuOrderRefund.setSellerNote(sellerNote);
        orders.add(skuOrderRefund);

        /**
         * 子订单: 状态不更新, 打开所有的操作
         */
        skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
        orders.add(skuOrder);

        /**
         * 主订单: 状态不更新, 判断是否要重新打开CONFIRM操作
         */
        if(canReopenShopOrderConfirmAction(shopOrder.getId(),skuOrder.getId())) {
            shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
            orders.add(shopOrder);
        }

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

    private boolean canReopenShopOrderConfirmAction(Long shopOrderId, Long curSkuOrderId){
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrderId);
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }

        for(SkuOrder skuOrder: skuOrdersR.getResult()){
            //如果是当前子订单则过滤掉
            if(curSkuOrderId.equals(skuOrder.getId())){
                continue;
            }
            //如果不是退货相关的终结状态,则返回false
            Response<List<SkuOrderRefund>> response=orderReadService.findSkuOrderRefundByParentId(skuOrder.getId());
            if(!response.isSuccess()){
                throw new ServiceException(response.getError());
            }
            for(SkuOrderRefund refund: response.getResult()){
                if(AllOnSaleReturnStatus.contains(refund.getNodeInstanceId())){
                    if(!LeafOnSaleReturnStatus.contains(refund.getNodeInstanceId())){
                        return false;
                    }
                }
            }
        }
        return true;
    }
}
