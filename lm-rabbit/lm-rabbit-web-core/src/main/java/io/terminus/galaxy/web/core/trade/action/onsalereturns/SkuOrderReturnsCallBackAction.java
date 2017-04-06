package io.terminus.galaxy.web.core.trade.action.onsalereturns;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.common.utils.BeanMapper;
import io.terminus.galaxy.order.enums.OrderMoneyFlowType;
import io.terminus.galaxy.order.enums.OrderPayType;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.order.model.OrderExtra;
import io.terminus.galaxy.order.model.OrderFinishInfo;
import io.terminus.galaxy.order.model.OrderMoneyFlow;
import io.terminus.galaxy.order.service.OrderExtraReadService;
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
import io.terminus.parana.shop.model.Shop;
import io.terminus.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 退货退款回调action
 * 1、更新退款状态 退货完成
 * 2、更新子订单状态 退货完成
 * 3、更新订单状态 确认收货（部分退货完成,不存在正在走退货流程的子订单） 或 退货完成（全部退货完成）
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/10/16
 * Time: 5:15 PM
 */
@Component
@Slf4j
public class SkuOrderReturnsCallBackAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final ShopReadService shopReadService;

    private final OrderExtraReadService orderExtraReadService;


    @Autowired
    public SkuOrderReturnsCallBackAction(OrderReadService orderReadService,
                                         OrderActionReadService orderActionReadService,
                                         OrderWriteService orderWriteService, ShopReadService shopReadService, OrderExtraReadService orderExtraReadService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
        this.shopReadService = shopReadService;
        this.orderExtraReadService = orderExtraReadService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());


        List<Order> orders = Lists.newArrayList();

        String channel =Params.get(context, "channel");
        String tradeNo =Params.get(context, "tradeNo");
        String batchNo =Params.get(context, "batchNo");
       /*
        todo 待交易模块添加子订单对应的orderExtra时回填退款批次号

        Response<OrderExtra> orderExtraRes = orderExtraReadService.findExtraByOrderIdAndType(orderId, OrderType.SKU_ORDER_REFUND.value());
        if(!orderExtraRes.isSuccess()){
            throw new ServiceException(orderExtraRes.getError());
        }
        OrderExtra orderExtra = orderExtraRes.getResult();

        OrderExtra updateExtra = new OrderExtra();
        updateExtra.setId(orderExtra.getId());
        updateExtra.setTradeNo(tradeNo);
        updateExtra.setBatchNo(batchNo);
        updateExtra.setChannel(channel);*/

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

        //根据订单查询子订单
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrderId);
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }
        List<SkuOrder> skuOrders = skuOrdersR.getResult();


        //更新子订单退款单
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_CANCEL_BY_RETURNS.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        orders.add(skuOrderRefund);

        //更新子订单状态
        skuOrder.setNodeInstanceId(OrderStatus.SKU_ORDER_CANCEL_BY_RETURNS_CAUSE_BY_SKU_RETURNS.value());
        skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
        orders.add(skuOrder);

        //判断是否需要更新订单为下个状态（退货完成）,如果所有子订单都为退货完成,则修改订单(下个状态)
        if(checAllSkuOrderRetunsEnd(skuOrders, skuOrder.getId())){
            shopOrder.setNodeInstanceId(OrderStatus.CANCEL_BY_RETURNS_CAUSE_BY_SKU_RETURNS.value());
            shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
            orders.add(shopOrder);

            createOrderFinishInfo(shopOrder,context);
        }else {
            //如果部分退货完成（退款完成for发货前退款），则恢复订单的操作（确认收货）
            if(!checkSkuOrdersHasUnfinishItem(skuOrders, skuOrder.getId())){
                //shopOrder.setNodeInstanceId(OrderStatus.DELIVED.value());
                shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
                orders.add(shopOrder);
            }
        }

        //创建订单流水
        createOrderMoneyFlow(shopOrder,tradeNo,channel,batchNo,context);


        orderWriteService.update(orders, null);

        return Boolean.TRUE;
    }

    private Boolean checAllSkuOrderRetunsEnd(List<SkuOrder> skuOrders, Long currentSkuOrderId){
        boolean flag = true;
        for(SkuOrder skuOrder : skuOrders){
            if(Objects.equals(skuOrder.getId(), currentSkuOrderId)){
                continue;
            }

            //是否继续判断下个子订单
            Boolean isContinue = Boolean.TRUE;
            //如果所有的子订单状态为退款成功（发货前部分退款）或退货成功（发货后）
            if(!ImmutableSet.of(
                    OrderStatus.SKU_ORDER_CANCEL_BY_REFUND.value(),
                    OrderStatus.SKU_ORDER_CANCEL_BY_RETURNS.value())
                    .contains(skuOrder.getNodeInstanceId())){
                isContinue = Boolean.FALSE;
            }

            if(!isContinue){
                flag =Boolean.FALSE;
                break;
            }
        }
        return flag;
    }

    private Boolean checkSkuOrdersHasUnfinishItem(List<SkuOrder> skuOrders, Long currentSkuOrderId){
        boolean flag = true;
        for(SkuOrder skuOrder : skuOrders){
            if(Objects.equals(skuOrder.getId(), currentSkuOrderId)){
                continue;
            }

            //检查当前skuOrder是否含有退款单
            Response<List<SkuOrderRefund>> skuOrderRefundR = orderReadService.findSkuOrderRefundByParentId(skuOrder.getId());
            if (!skuOrderRefundR.isSuccess()) {
                //todo 查询失败如何处理
                continue;
            }
            //当前子订单不存在任何退款行为
            if(Arguments.isNullOrEmpty(skuOrderRefundR.getResult())){
                continue;
            }

            //判断当前子订单关联的所有退款单的状态
            //退款单可能存在的状态 发货前-退款完成、发货前-退款取消（审核前）发货前-退款取消（审核后）、发货前-退款拒绝
            //退款单可能存在的状态 退货完成、退货取消(审核前)、退货取消(审核后)、退货拒绝

            //是否继续判断下个退款单
            Boolean isContinue = Boolean.TRUE;
            for(SkuOrderRefund refund : skuOrderRefundR.getResult()){
                //如果当前退款单的状态不在上边的状态集合中则说明在走退货的其他节点，则不能更新订单状态（确认收货）
                if(!ImmutableSet.of(OrderStatus.SKU_REFUND_CANCEL_BY_REFUND.value(),
                        OrderStatus.SKU_REFUND_UNDO.value(),
                        OrderStatus.SKU_REFUND_REFUSE_REFUND.value(),
                        OrderStatus.SKU_REFUND_CANCEL_BY_RETURNS.value(),
                        OrderStatus.SKU_RETURNS_APPLY_UNDO.value(),
                        OrderStatus.SKU_RETURNS_AGREE_UNDO.value(),
                        OrderStatus.SKU_REFUND_REFUSE_RETURNS.value()).contains(refund.getNodeInstanceId())){
                    isContinue = Boolean.FALSE;
                    break;
                }

            }

            //如果当前退款正在走退货流程，则停止判断下个退款单
            if(!isContinue){
                flag = false;
                break;
            }

        }
        return flag;
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


    private void createOrderMoneyFlow(ShopOrder shopOrder, String tradeNo,String channel,String batchNo,Map<String, Serializable> context){

        OrderMoneyFlow flow = new OrderMoneyFlow();
        flow.setBuyerId(shopOrder.getBuyerId());
        flow.setBuyerName(shopOrder.getBuyerName());
        flow.setShopId(shopOrder.getShopId());
        flow.setShopName(shopOrder.getShopName());
        Shop shop = getShop(flow.getShopId());
        flow.setSellerId(shop.getUserId());
        flow.setSellerName(shop.getUserName());
        flow.setChannel(channel);
        flow.setTradeNo(tradeNo);
        flow.setFee(Long.valueOf(shopOrder.getFee().toString()));
        flow.setIsSettlemented(Boolean.FALSE);
        flow.setMemo("订单退货成功");
        flow.setOrderId(shopOrder.getId());
        flow.setOrderType(OrderType.SHOP_ORDER.value());
        flow.setTradeAt(new Date());
        flow.setBatchNo(batchNo);
        flow.setPayType(OrderPayType.ON_LINE.value());
        flow.setType(OrderMoneyFlowType.SALE_REFUND.value());
        context.put("orderMoneyFlow",flow);

    }


    private void createOrderFinishInfo(ShopOrder shopOrder,Map<String, Serializable> context){

        Response<OrderExtra> orderExtraRes = orderExtraReadService.findExtraByOrderIdAndType(shopOrder.getId(), OrderType.SHOP_ORDER.value());
        if(!orderExtraRes.isSuccess()){
            log.error("find order extra by order id: {} fail,error: {}",shopOrder.getId(),orderExtraRes.getError());
            throw new ServiceException(orderExtraRes.getError());
        }
        OrderExtra orderExtra = orderExtraRes.getResult();


        OrderFinishInfo info = new OrderFinishInfo();
        BeanMapper.copy(shopOrder, info);
        info.setSystemNo(orderExtra.getSystemNo());
        info.setOrderId(shopOrder.getId());
        info.setOrderType(OrderType.SHOP_ORDER.value());
        info.setOrderStatus(shopOrder.getNodeInstanceId().intValue());
        info.setType(1);
        info.setPayType(OrderPayType.ON_LINE.value());
        Shop shop = getShop(info.getShopId());
        info.setSellerId(shop.getUserId());
        info.setSellerName(shop.getUserName());
        info.setIsSettlemented(Boolean.FALSE);
        info.setFinishedAt(new Date());
        context.put("orderFinishInfo",info);
    }

    private Shop getShop(Long shopId){
        Response<Shop> shopRes = shopReadService.findById(shopId);
        if(!shopRes.isSuccess()){
            throw new ServiceException(shopRes.getError());
        }

        return shopRes.getResult();
    }
}
