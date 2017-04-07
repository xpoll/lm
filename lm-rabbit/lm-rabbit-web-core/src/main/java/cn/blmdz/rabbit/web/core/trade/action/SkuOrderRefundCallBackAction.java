package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.rabbit.order.enums.OrderMoneyFlowType;
import cn.blmdz.rabbit.order.enums.OrderPayType;
import cn.blmdz.rabbit.order.enums.OrderStatus;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.model.OrderMoneyFlow;
import cn.blmdz.rabbit.order.service.OrderExtraReadService;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Desc: 退款回调action
 * 1、更新退款单状态 退款完成
 * 2、更新子订单状态 退款完成
 * 3、更新总订单状态 待发货（部分退款完成，目前不存在正在走退款流程的子订单）或 退款完成（全部子订单退款完成）
 * Mail: houly@terminus.io
 * author: Hou Luyao
 * Date: 16/3/9.
 */
@Component
@Slf4j
public class SkuOrderRefundCallBackAction implements TradeAction<Boolean> {
    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final ShopReadService shopReadService;

    private final OrderExtraReadService orderExtraReadService;

    @Autowired
    public SkuOrderRefundCallBackAction(OrderReadService orderReadService,
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
        String channel =(String) Params.get(context, "channel");
        String batchNo =(String) Params.get(context, "batchNo");
        String tradeNo =(String) Params.get(context, "tradeNo");


       /*
        todo 待交易模块添加子订单对应的orderExtra时回填退款批次号
        String tradeNo =Params.get(context, "tradeNo");
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
        skuOrderRefund.setNodeInstanceId(OrderStatus.SKU_REFUND_CANCEL_BY_REFUND.value());
        skuOrderRefund.setNextActionInstanceIds(getNextActions(skuOrderRefund.getNodeInstanceId()));
        orders.add(skuOrderRefund);

        //更新子订单状态
        skuOrder.setNodeInstanceId(OrderStatus.SKU_ORDER_CANCEL_BY_REFUND_CAUSE_BY_SKU_REFUND.value());
        skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
        orders.add(skuOrder);

        //判断是否需要更新订单状态,如果所有子订单都含有退款单,而且退款单都已经完成了,则修改订单
        if(checAllSkuOrderRefundEnd(skuOrders, skuOrder.getId())){
            shopOrder.setNodeInstanceId(OrderStatus.CANCEL_BY_REFUND_CAUSE_BY_SKU_ORDER_REFUND.value());
            shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
            orders.add(shopOrder);

            createOrderFinishInfo(shopOrder,context);
        }
        else{
            //判断是否包含没有完成的子退款单,如果不包含更改订单状态
            if(checkSkuOrdersHasUnfinishItem(skuOrders, skuOrder.getId())){
                shopOrder.setNodeInstanceId(OrderStatus.WAIT_FOR_DELIVE.value());
                shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
                orders.add(shopOrder);
            }
        }

        //创建订单流水
        createOrderMoneyFlow(shopOrder,tradeNo,channel,batchNo,context);

        orderWriteService.update(orders, context);

        return Boolean.TRUE;
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
        flow.setMemo("订单退款成功");
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


    private Boolean checAllSkuOrderRefundEnd(List<SkuOrder> skuOrders, Long currentSkuOrderId){
        boolean flag = true;
        for(SkuOrder skuOrder : skuOrders){
            if(Objects.equals(skuOrder.getId(), currentSkuOrderId)){
                continue;
            }
            if(skuOrder.getNodeInstanceId().equals(OrderStatus.SKU_ORDER_CANCEL_BY_REFUND.value())){
                continue;
            }
            flag = false;
            break;
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
                //不包含退款单
                continue;
            }
            if(Arguments.isNullOrEmpty(skuOrderRefundR.getResult())){
                //不包含退款单
                continue;
            }
            Boolean isContinue =Boolean.TRUE;
            for(SkuOrderRefund refund : skuOrderRefundR.getResult()){
                //如果 当前退款单状态不为 退款取消 退款拒绝 退款完成 则说明正在走申请流程，则不恢复订单状态为（待卖家发货）
                if(!ImmutableSet.of(OrderStatus.SKU_REFUND_UNDO.value(),
                        OrderStatus.SKU_REFUND_REFUSE_REFUND.value(),
                        OrderStatus.SKU_REFUND_CANCEL_BY_REFUND.value()).contains(refund.getNodeInstanceId())){
                    isContinue = Boolean.FALSE;
                    break;
                }
            }
            if(!isContinue){
                flag = false;
                break;
            }
        }
        return flag;
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

}
