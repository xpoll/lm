package cn.blmdz.rabbit.web.trade.controller;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Strings;
import com.google.common.eventbus.EventBus;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.order.dto.FatOrder;
import cn.blmdz.rabbit.order.dto.FatSku;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.rabbit.web.event.RemoveCartEvent;
import cn.blmdz.rabbit.web.event.StockChangeEvent;
import cn.blmdz.wolf.order.dto.OrderChain;
import cn.blmdz.wolf.order.model.OrderActionInstance;
import cn.blmdz.wolf.order.model.OrderNodeInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderActionReadService;
import cn.blmdz.wolf.order.service.OrderNodeReadService;
import cn.blmdz.wolf.order.service.OrderWriteService;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.item.model.ItemSnapshot;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import cn.blmdz.wolf.parana.item.service.ItemSnapshotReadService;
import cn.blmdz.wolf.parana.item.service.ItemSnapshotWriteService;
import cn.blmdz.wolf.parana.item.service.SkuReadService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Mail: F@terminus.io
 * Data: 16/4/14
 * Author: yangzefeng
 */
@RestController
@Slf4j
@RequestMapping("/api/order")
public class WriteOrders {

    private final OrderNodeReadService orderNodeReadService;

    private final OrderActionReadService orderActionReadService;

    private final ShopReadService shopReadService;

    private final SkuReadService skuReadService;

    private final ItemReadService itemReadService;

    private final ItemSnapshotReadService itemSnapshotReadService;

    private final EventBus eventBus;

    private final ItemSnapshotWriteService itemSnapshotWriteService;

    private final OrderWriteService orderWriteService;

    private static final JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();

    @Autowired
    public WriteOrders(OrderWriteService orderWriteService,
                           OrderNodeReadService orderNodeReadService,
                           OrderActionReadService orderActionReadService,
                           ShopReadService shopReadService,
                           SkuReadService skuReadService,
                           ItemReadService itemReadService,
                           ItemSnapshotReadService itemSnapshotReadService,
                           EventBus eventBus,
                           ItemSnapshotWriteService itemSnapshotWriteService) {
        this.orderWriteService = orderWriteService;
        this.orderNodeReadService = orderNodeReadService;
        this.orderActionReadService = orderActionReadService;
        this.shopReadService = shopReadService;
        this.skuReadService = skuReadService;
        this.itemReadService = itemReadService;
        this.itemSnapshotReadService = itemSnapshotReadService;
        this.eventBus = eventBus;
        this.itemSnapshotWriteService = itemSnapshotWriteService;
    }

    @RequestMapping(value = "/create", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public List<Long> create(@RequestParam("tradeInfo") String tradeInfo,
                             @RequestParam("data") String data) {
        List<FatOrder> fatOrders = JSON_MAPPER.fromJson(data, JSON_MAPPER.createCollectionType(List.class, FatOrder.class));
        OrderChain orderChain = preHandle(fatOrders, tradeInfo);

        Response<List<Long>> orderIdsR = orderWriteService.create(orderChain);
        if (!orderIdsR.isSuccess()) {
            throw new JsonResponseException(orderIdsR.getError());
        }

        postHandle(fatOrders);

        return orderIdsR.getResult();
    }

    /**
     * 创建订单后的逻辑,目前包含一些异步事件(移除购物车,减库存,加销量)
     */
    private void postHandle(List<FatOrder> fatOrders) {
        Map<Long, Integer> skuIdAndQuantity = new HashMap<>();
        List<Long> skuIds = new ArrayList<>();
        for (FatOrder fatOrder : fatOrders) {
            for (FatSku fatSku : fatOrder.getFatSkus()) {
                skuIdAndQuantity.put(fatSku.getSkuId(), fatSku.getQuantity());
                skuIds.add(fatSku.getSkuId());
            }
        }
        //publish remove cart event
        Long userId = UserUtil.getUserId();
        RemoveCartEvent removeCartEvent = new RemoveCartEvent(skuIds, userId);
        eventBus.post(removeCartEvent);

        //publish stock change event
        StockChangeEvent stockChangeEvent = new StockChangeEvent(skuIdAndQuantity);
        eventBus.post(stockChangeEvent);
    }

    private OrderChain preHandle(List<FatOrder> fatOrders, String tradeInfo) {
        //root
        OrderChain root = new OrderChain();
        List<OrderChain> shopOrderChains = new ArrayList<>();

        for (FatOrder fatOrder : fatOrders) {
            //交易流程相关
            ShopOrder shopOrder = new ShopOrder();
            //// TODO: 16/3/8 流程id如何确定
            shopOrder.setFlowId(1L);
            Response<OrderNodeInstance> entranceR = orderNodeReadService.getEntranceByFlowId(1L);
            if (!entranceR.isSuccess()) {
                throw new JsonResponseException(entranceR.getError());
            }
            OrderNodeInstance entrance = entranceR.getResult();
            shopOrder.setNodeInstanceId(entrance.getId());
            shopOrder.setNextActionInstanceIds(getNextActionJson(entrance.getId()));
            //费用相关
            shopOrder.setOriginFee(0);
            shopOrder.setFee(0);
            shopOrder.setDiscount(0);
            shopOrder.setShipFee(0);
            //买家信息
            BaseUser baseUser = UserUtil.getCurrentUser();
            shopOrder.setBuyerId(baseUser.getId());
            shopOrder.setBuyerName(baseUser.getName());
            //商家信息
            shopOrder.setShopId(shopOrder.getShopId());
            Response<Shop> shopR = shopReadService.findById(fatOrder.getShopId());
            if (!shopR.isSuccess()) {
                throw new JsonResponseException(shopR.getError());
            }
            Shop shop = shopR.getResult();
            shopOrder.setShopName(shop.getName());
            shopOrder.setShopId(shop.getId());

            shopOrder.setPayType(fatOrder.getPayType());
            //// TODO: 16/3/5 现在是写死,没有业务逻辑
            shopOrder.setDeliverType(1);
            shopOrder.setChannel(1);
            shopOrder.setType(OrderType.SHOP_ORDER.value());

            OrderChain shopOrderChain = new OrderChain();
            shopOrderChain.setOrder(shopOrder);
            //add order extra to context
            OrderExtra orderExtra = new OrderExtra();
            orderExtra.setOrderType(OrderType.SHOP_ORDER.value());
            orderExtra.setTradeInfo(tradeInfo);
            orderExtra.setInvoice(fatOrder.getInvoiceJson());
            orderExtra.setBuyerNotes(fatOrder.getBuyerNotes());
            Map<String, Serializable> context = new HashMap<>();
            context.put("orderExtra", orderExtra);
            shopOrderChain.setContext(context);
            shopOrderChains.add(shopOrderChain);

            List<OrderChain> skuOrderChains = new ArrayList<>();
            for (FatSku fatSku : fatOrder.getFatSkus()) {
                SkuOrder skuOrder = new SkuOrder();
                //sku校验
                Response<Sku> skuR = skuReadService.findSkuById(fatSku.getSkuId());
                if (!skuR.isSuccess()) {
                    throw new JsonResponseException(skuR.getError());
                }
                Sku sku = skuR.getResult();
                if (!Objects.equals(sku.getStatus(), 1)) {
                    log.warn("sku (id={}) status not support create order", sku.getId());
                    throw new JsonResponseException("sku.not.onshelf");
                }
                if (fatSku.getQuantity() > sku.getStockQuantity()) {
                    log.warn("sku (id={}) stock not enough", sku.getId());
                    throw new JsonResponseException("sku.stock.not.enough");
                }
                skuOrder.setFlowId(shopOrder.getFlowId());
                skuOrder.setNodeInstanceId(15L);
                skuOrder.setNextActionInstanceIds(getNextActionJson(15L));
                skuOrder.setType(OrderType.SKU_ORDER.value());
                skuOrder.setChannel(shopOrder.getChannel());
                skuOrder.setPayType(shopOrder.getPayType());

                //费用相关
                skuOrder.setOriginFee(sku.getPrice() * fatSku.getQuantity());
                shopOrder.setOriginFee(shopOrder.getOriginFee() + skuOrder.getOriginFee());
                skuOrder.setFee(skuOrder.getOriginFee());
                shopOrder.setFee(shopOrder.getFee() + skuOrder.getFee());
                skuOrder.setDiscount(0);
                shopOrder.setDiscount(shopOrder.getDiscount() + skuOrder.getDiscount());
                //// TODO: 16/3/5 订单运费需要具体计算逻辑
                skuOrder.setShipFee(0);
                shopOrder.setShipFee(shopOrder.getShipFee() + skuOrder.getShipFee());
                //买家相关
                skuOrder.setBuyerId(shopOrder.getBuyerId());
                skuOrder.setBuyerName(shopOrder.getBuyerName());
                //店铺相关
                skuOrder.setShopId(shopOrder.getShopId());
                skuOrder.setShopName(shopOrder.getShopName());
                //sku相关
                skuOrder.setSkuId(fatSku.getSkuId());
                skuOrder.setSkuAttributes(sku.getAttrsJson());
                skuOrder.setQuantity(fatSku.getQuantity());
                //商品相关
                Response<Item> itemR = itemReadService.findById(sku.getItemId());
                if (!itemR.isSuccess()) {
                    log.warn("fail to find item by id {} when user creating order, cause:{}",
                            sku.getItemId(), itemR.getError());
                    throw new JsonResponseException(itemR.getError());
                }
                Item item = itemR.getResult();
                skuOrder.setItemId(item.getId());
                skuOrder.setItemName(item.getName());
                skuOrder.setItemImage(item.getMainImage());
                //TODO 生成快照考虑异步实现
                skuOrder.setItemSnapshotId(getItemSnapshotId(item));

                OrderChain skuOrderChain = new OrderChain();
                skuOrderChain.setOrder(skuOrder);
                skuOrderChain.setContext(new HashMap<String, Serializable>());
                skuOrderChains.add(skuOrderChain);
            }
            shopOrderChain.setChildren(skuOrderChains);
        }
        root.setChildren(shopOrderChains);
        return root;
    }

    private Long getItemSnapshotId(Item item) {

        if (!Strings.isNullOrEmpty(item.getItemInfoMd5())) {
            Response<ItemSnapshot> itemSnapshotResp = itemSnapshotReadService.findByItemIdAndItemInfoMd5(item.getId(), item.getItemInfoMd5());
            if (!itemSnapshotResp.isSuccess()) {
                log.error("fail to find item snapshot by itemId={} and md5={},cause:{}", item.getId(), item.getItemInfoMd5(), itemSnapshotResp.getError());
                return null;
            }

            ItemSnapshot itemSnapshot = itemSnapshotResp.getResult();
            if (itemSnapshot != null) {
                return itemSnapshot.getId();
            }
        }

        Response<FullItem> fullItemInfoResp = itemReadService.findFullInfoByItemId(item.getId());
        if (!fullItemInfoResp.isSuccess()) {
            log.error("fail to find full item info by itemId:{},cause:{}", item.getId(), fullItemInfoResp.getError());
            return null;
        }
        FullItem fullItem = fullItemInfoResp.getResult();

        ItemAttribute itemAttribute = new ItemAttribute();
        itemAttribute.setSkuAttrs(fullItem.getGroupedSkuAttributes());
        itemAttribute.setOtherAttrs(fullItem.getGroupedOtherAttributes());
        Response<Long> createItemSnapshotResp = itemSnapshotWriteService.create(item, fullItem.getItemDetail(), itemAttribute);
        if (!createItemSnapshotResp.isSuccess()) {
            log.error("fail to create item snapshot with item:{},itemDetail:{},itemAttribute:{},cause:{}", item, fullItem.getItemDetail(), itemAttribute, createItemSnapshotResp.getError());
            return null;
        }
        return createItemSnapshotResp.getResult();
    }

    private String getNextActionJson(Long nodeInstanceId) {
        Response<Map<Integer, List<OrderActionInstance>>> userTypeAndActionsR =
                orderActionReadService.findExcludedActionsGroupByUserType(nodeInstanceId);
        if (!userTypeAndActionsR.isSuccess()) {
            throw new JsonResponseException(userTypeAndActionsR.getError());
        }
        Map<Integer, List<OrderActionInstance>> userTypeAndActionMap = userTypeAndActionsR.getResult();
        return ParseJson.getNextActionsJson(userTypeAndActionMap);
    }
}
