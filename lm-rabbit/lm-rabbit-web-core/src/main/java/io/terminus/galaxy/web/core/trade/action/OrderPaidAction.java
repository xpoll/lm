package io.terminus.galaxy.web.core.trade.action;

import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.enums.OrderMoneyFlowType;
import io.terminus.galaxy.order.enums.OrderPayType;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.enums.OrderType;
import io.terminus.galaxy.order.model.OrderExtra;
import io.terminus.galaxy.order.model.OrderMoneyFlow;
import io.terminus.galaxy.order.service.OrderExtraReadService;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.order.engine.TradeAction;
import io.terminus.parana.order.model.Order;
import io.terminus.parana.order.model.OrderActionInstance;
import io.terminus.parana.order.model.ShopOrder;
import io.terminus.parana.order.model.SkuOrder;
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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * Desc: 店铺订单支付动作
 * 在这个action里面,会把shopOrder的状态流转到 WAIT_FOR_DELIVE, skuOrder的状态流转到 SKU_ORDER_WAIT_FOR_DELIVE
 * @see io.terminus.galaxy.order.enums.OrderStatus
 * Mail: F@terminus.io
 * Data: 16/3/7
 * Author: yangzefeng
 */
@Component @Slf4j
public class OrderPaidAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;


    private final OrderExtraReadService orderExtraReadService;

    private final ShopReadService shopReadService;


    @Autowired
    public OrderPaidAction(OrderReadService orderReadService,
                           OrderActionReadService orderActionReadService,
                           OrderWriteService orderWriteService,
                           OrderExtraReadService orderExtraReadService, ShopReadService shopReadService) {
        this.orderReadService = orderReadService;
        this.orderActionReadService = orderActionReadService;
        this.orderWriteService = orderWriteService;
        this.orderExtraReadService = orderExtraReadService;
        this.shopReadService = shopReadService;
    }

    @Override
    public Boolean execute(Map<String, Serializable> context) {
        Long orderId = Long.valueOf(Params.get(context, "orderId").toString());

        Response<OrderExtra> orderExtraRes = orderExtraReadService.findExtraByOrderIdAndType(orderId, OrderType.SHOP_ORDER.value());
        if(!orderExtraRes.isSuccess()){
            log.error("find order extra by order id: {} fail,error: {}",orderId,orderExtraRes.getError());
            throw new ServiceException(orderExtraRes.getError());
        }
        OrderExtra orderExtra = orderExtraRes.getResult();
        String channel =Params.get(context, "channel");
        String tradeNo =Params.get(context, "tradeNo");
        String systemNo =Params.get(context, "systemNo");
        String paymentCode =Params.get(context, "paymentCode");

        OrderExtra updateExtra = new OrderExtra();
        updateExtra.setId(orderExtra.getId());
        updateExtra.setChannel(channel);
        updateExtra.setTradeNo(tradeNo);
        updateExtra.setSystemNo(systemNo);
        updateExtra.setPaymentCode(paymentCode);

        context.put("orderExtra",updateExtra);



        Response<ShopOrder> shopOrderR = orderReadService.findShopOrderById(orderId);
        if (!shopOrderR.isSuccess()) {
            throw new ServiceException(shopOrderR.getError());
        }
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(orderId);
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }
        ShopOrder shopOrder = shopOrderR.getResult();

        List<SkuOrder> skuOrders = skuOrdersR.getResult();

        List<Order> toUpdates = new ArrayList<>();
        //流转到等待买家发货节点
        shopOrder.setNodeInstanceId(OrderStatus.WAIT_FOR_DELIVE.value());
        //找到接下来可以执行的action
        shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
        toUpdates.add(shopOrder);

        for (SkuOrder skuOrder : skuOrders) {
            //sku订单对应的买家发货节点
            skuOrder.setNodeInstanceId(OrderStatus.SKU_ORDER_WAIT_FOR_DELIVE.value());
            skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
            toUpdates.add(skuOrder);
        }

        //创建订单资金流转
        createOrderMoneyFlow(shopOrder,updateExtra,systemNo,paymentCode,context);

        //批量更新订单
        orderWriteService.update(toUpdates, context);

        return Boolean.TRUE;
    }

    private void createOrderMoneyFlow(ShopOrder shopOrder, OrderExtra orderExtra,String systemNo,String paymentCode,Map<String, Serializable> context){

        OrderMoneyFlow flow = new OrderMoneyFlow();
        flow.setBuyerId(shopOrder.getBuyerId());
        flow.setBuyerName(shopOrder.getBuyerName());
        flow.setShopId(shopOrder.getShopId());
        flow.setShopName(shopOrder.getShopName());
        Shop shop = getShop(flow.getShopId());
        flow.setSellerId(shop.getUserId());
        flow.setSellerName(shop.getUserName());
        flow.setChannel(orderExtra.getChannel());
        flow.setTradeNo(orderExtra.getTradeNo());
        flow.setFee(Long.valueOf(shopOrder.getFee().toString()));
        flow.setIsSettlemented(Boolean.FALSE);
        flow.setMemo("订单支付成功");
        flow.setSystemNo(systemNo);
        flow.setOrderId(shopOrder.getId());
        flow.setOrderType(OrderType.SHOP_ORDER.value());
        flow.setTradeAt(new Date());
        flow.setPaymentCode(paymentCode);
        flow.setPayType(OrderPayType.ON_LINE.value());
        flow.setType(OrderMoneyFlowType.PAID.value());
        context.put("orderMoneyFlow",flow);

    }

    private Shop getShop(Long shopId){
        Response<Shop> shopRes = shopReadService.findById(shopId);
        if(!shopRes.isSuccess()){
            throw new ServiceException(shopRes.getError());
        }

        return shopRes.getResult();
    }

    private String getNextActions(Long nid) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }
}
