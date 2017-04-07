package cn.blmdz.rabbit.web.core.trade.action;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableSet;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.rabbit.order.enums.OrderPayType;
import cn.blmdz.rabbit.order.enums.OrderStatus;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.service.OrderExtraReadService;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.common.utils.RespHelper;
import cn.blmdz.wolf.order.engine.TradeAction;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.order.util.Params;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * 店铺订单买家确认收货动作
 *
 * 在这个 action 里面, 会把 shopOrder 的状态流转到 {@link OrderStatus#DONE},
 * skuOrder 的状态流转到 {@link OrderStatus#SKU_ORDER_DONE}
 *
 * @author Effet
 */
@Component
@Slf4j
public class OrderConfirmAction implements TradeAction<Boolean> {

    private final OrderReadService orderReadService;

    private final OrderActionReadService orderActionReadService;

    private final OrderWriteService orderWriteService;

    private final ShopReadService shopReadService;
    private final OrderExtraReadService orderExtraReadService;

    private final static ImmutableSet<Long> SKIP_STATUS = ImmutableSet.of(
            OrderStatus.CANCEL_BY_REFUND.value(),
            OrderStatus.CANCEL_BY_RETURNS.value(),
            OrderStatus.SKU_ORDER_CANCEL_BY_REFUND_CAUSE_BY_SKU_REFUND.value()
    );

    @Autowired
    public OrderConfirmAction(OrderReadService orderReadService,
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

        // 订单
        ShopOrder shopOrder = RespHelper.orServEx(orderReadService.findShopOrderById(orderId));
        // 子订单
        List<SkuOrder> skuOrders = RespHelper.orServEx(orderReadService.findSkuOrderByParentId(orderId));

        List<Order> toUpdates = new ArrayList<>();
        // 流转到交易完成节点
        shopOrder.setNodeInstanceId(OrderStatus.DONE.value());
        // 找到接下来可以执行的action
        shopOrder.setNextActionInstanceIds(getNextActions(shopOrder.getNodeInstanceId()));
        toUpdates.add(shopOrder);

        for (SkuOrder skuOrder : skuOrders) {
            //如果是已经被取消的子订单,不需要更新
            if (SKIP_STATUS.contains(skuOrder.getNodeInstanceId())) {
                continue;
            }
            // sku订单对应的交易完成节点
            skuOrder.setNodeInstanceId(OrderStatus.SKU_ORDER_DONE.value());
            skuOrder.setNextActionInstanceIds(getNextActions(skuOrder.getNodeInstanceId()));
            toUpdates.add(skuOrder);
        }

        createOrderFinishInfo(shopOrder,context);

        //批量更新订单
        orderWriteService.update(toUpdates, context);

        return Boolean.TRUE;
    }

    private void createOrderFinishInfo(ShopOrder shopOrder,Map<String, Serializable> context){

        Response<OrderExtra> orderExtraRes = orderExtraReadService.findExtraByOrderIdAndType(shopOrder.getId(), OrderType.SHOP_ORDER.value());
        if(!orderExtraRes.isSuccess()){
            log.error("find order extra by order id: {} fail,error: {}",shopOrder.getId(),orderExtraRes.getError());
            throw new ServiceException(orderExtraRes.getError());
        }
        OrderExtra orderExtra = orderExtraRes.getResult();

        OrderFinishInfo info = new OrderFinishInfo();
        BeanMapper.copy(shopOrder,info);
        info.setOrderId(shopOrder.getId());
        info.setOrderStatus(shopOrder.getNodeInstanceId().intValue());
        info.setOrderType(OrderType.SHOP_ORDER.value());
        info.setType(1);
        info.setPayType(OrderPayType.ON_LINE.value());
        Shop shop = getShop(info.getShopId());
        info.setSellerId(shop.getUserId());
        info.setSellerName(shop.getUserName());
        info.setIsSettlemented(Boolean.FALSE);
        info.setSystemNo(orderExtra.getSystemNo());
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

    private String getNextActions(Long nid) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActions =
                orderActionReadService.findExcludedActionsGroupByUserType(nid);
        if (!userTypeAndActions.isSuccess()) {
            throw new ServiceException(userTypeAndActions.getError());
        }
        return ParseJson.getNextActionsJson(userTypeAndActions.getResult());
    }
}
