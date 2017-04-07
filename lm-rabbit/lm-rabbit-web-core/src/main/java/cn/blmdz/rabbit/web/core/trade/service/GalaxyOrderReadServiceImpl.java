package cn.blmdz.rabbit.web.core.trade.service;

import static cn.blmdz.rabbit.common.util.UserRoleUtil.isAdmin;
import static cn.blmdz.rabbit.common.util.UserRoleUtil.isOperator;
import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Function;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.rabbit.order.constant.SkuOrderExtraKeys;
import cn.blmdz.rabbit.order.dto.OrderDetail;
import cn.blmdz.rabbit.order.dto.OrderItemSnapshotDetail;
import cn.blmdz.rabbit.order.dto.PreComment;
import cn.blmdz.rabbit.order.dto.PreOrder;
import cn.blmdz.rabbit.order.dto.PreOrderItem;
import cn.blmdz.rabbit.order.dto.PrePay;
import cn.blmdz.rabbit.order.dto.RichShopOrder;
import cn.blmdz.rabbit.order.dto.RichSkuOrder;
import cn.blmdz.rabbit.order.dto.SkuOrderDetail;
import cn.blmdz.rabbit.order.enums.OrderAction;
import cn.blmdz.rabbit.order.enums.OrderType;
import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.rabbit.order.service.OrderExtraReadService;
import cn.blmdz.rabbit.web.core.trade.util.ParseJson;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.order.model.OrderNodeInstance;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderNodeReadService;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.parana.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemSnapshot;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import cn.blmdz.wolf.parana.item.service.ItemSnapshotReadService;
import cn.blmdz.wolf.parana.item.service.SkuReadService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.parana.shop.service.ShopReadService;
import cn.blmdz.wolf.user.model.LoginType;
import cn.blmdz.wolf.user.model.User;
import cn.blmdz.wolf.user.service.UserReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Desc: bbc 解决方案订单读服务实现类
 * Mail: F@terminus.io
 * Data: 16/3/5
 * Author: yangzefeng
 */
@Service @Primary @Slf4j
public class GalaxyOrderReadServiceImpl implements GalaxyOrderReadService {

    private final ItemReadService itemReadService;

    private final SkuReadService skuReadService;

    private final ShopReadService shopReadService;

    private final UserReadService<User> userReadService;

    private final OrderReadService orderReadService;

    private final OrderNodeReadService orderNodeReadService;

    private final ItemSnapshotReadService itemSnapshotReadService;

    private final OrderExtraReadService orderExtraReadService;

    private final static JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();

    @Autowired
    public GalaxyOrderReadServiceImpl(ItemReadService itemReadService,
                                      SkuReadService skuReadService,
                                      ShopReadService shopReadService,
                                      UserReadService<User> userReadService,
                                      OrderReadService orderReadService,
                                      OrderNodeReadService orderNodeReadService,
                                      ItemSnapshotReadService itemSnapshotReadService,
                                      OrderExtraReadService orderExtraReadService) {
        this.itemReadService = itemReadService;
        this.skuReadService = skuReadService;
        this.shopReadService = shopReadService;
        this.userReadService = userReadService;
        this.orderReadService = orderReadService;
        this.orderNodeReadService = orderNodeReadService;
        this.itemSnapshotReadService = itemSnapshotReadService;
        this.orderExtraReadService = orderExtraReadService;
    }

    @Override
    public Response<List<PreOrder>> preOrder(String skus) {
        try {
            Map<Long, Integer> skuIdAndQuantity =
                    JSON_MAPPER.fromJson(skus, JSON_MAPPER.createCollectionType(HashMap.class, Long.class, Integer.class));
            if (CollectionUtils.isEmpty(skuIdAndQuantity)) {
                return Response.fail("sku.parse.fail");
            }
            Multimap<Long, PreOrderItem> grouped = groupByShopId(skuIdAndQuantity);
            List<PreOrder> preOrders = Lists.newArrayListWithCapacity(grouped.keys().size());
            Map<Long, Shop> shopMap = getShopMapFromIds(grouped.keys());

            for(Long shopId : grouped.keySet()) {
                PreOrder preOrder = makePreOrder(grouped.get(shopId), shopMap.get(shopId));
                if(preOrder != null) {
                    preOrders.add(preOrder);
                }
            }

            return Response.ok(preOrders);
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to pre view order by skus {}, cause:{}",
                    skus, Throwables.getStackTraceAsString(e));
            return Response.fail("pre.view.order.fail");
        }
    }

    protected Multimap<Long, PreOrderItem> groupByShopId(Map<Long, Integer> skuIdAndQuantity) {

        Multimap<Long, PreOrderItem> grouped = HashMultimap.create();

        List<Long> skuIds = new ArrayList<>();
        skuIds.addAll(skuIdAndQuantity.keySet());
        Response<List<Sku>> skusR = skuReadService.findSkusByIds(skuIds);
        if (!skusR.isSuccess()) {
            throw new ServiceException(skusR.getError());
        }
        List<Sku> skus = skusR.getResult();

        Map<Long, Item> itemMap = getItemMapFromSkus(skus);

        for (Sku sku : skus) {
            if (null == sku) {
                log.warn("sku not found when pre view order, skip");
                continue;
            }
            if (!Objects.equals(sku.getStatus(), 1)) {
                log.warn("sku id {} not on shelf when pre view order, skip", sku.getId());
                continue;
            }
            PreOrderItem preOrderItem = new PreOrderItem();
            preOrderItem.setSku(sku);
            preOrderItem.setQuantity(skuIdAndQuantity.get(sku.getId()));
            preOrderItem.setFee(sku.getPrice() * preOrderItem.getQuantity());
            Item item = itemMap.get(sku.getItemId());
            if (null == item) {
                log.warn("sku id {} can not find item when pre view order, skip", sku.getId());
                continue;
            }
            preOrderItem.setItemName(item.getName());
            preOrderItem.setItemImage(item.getMainImage());
            grouped.put(sku.getShopId(), preOrderItem);
        }

        return grouped;
    }

    protected Map<Long, Item> getItemMapFromSkus(Iterable<Sku> skus) {
        //for dubbo serialize
        List<Long> itemIds = new ArrayList<>();
        for (Sku sku : skus) {
            itemIds.add(sku.getItemId());
        }
        Response<List<Item>> itemsR = itemReadService.findByIds(itemIds);
        if (!itemsR.isSuccess()) {
            throw new ServiceException(itemsR.getError());
        }
        List<Item> items = itemsR.getResult();
        Map<Long, Item> itemMap = new HashMap<>();
        for (Item item : items) {
            itemMap.put(item.getId(), item);
        }
        return itemMap;
    }

    protected Map<Long, Shop> getShopMapFromIds(Iterable<Long> shopIds) {
        //for dubbo serialize
        List<Long> shopIdList = new ArrayList<>();
        for (Long shopId : shopIds) {
            shopIdList.add(shopId);
        }
        Response<List<Shop>> shopsR = shopReadService.findByIds(shopIdList);
        if (!shopsR.isSuccess()) {
            throw new ServiceException(shopsR.getError());
        }
        List<Shop> shops = shopsR.getResult();
        Map<Long, Shop> shopMap = new HashMap<>();
        for (Shop shop : shops) {
            shopMap.put(shop.getId(), shop);
        }
        return shopMap;
    }

    protected PreOrder makePreOrder(Iterable<PreOrderItem> preOrderItems, Shop shop) {
        PreOrder preOrder = new PreOrder();

        List<PreOrderItem> preOrderItemsList = new ArrayList<>();

        for (PreOrderItem preOrderItem : preOrderItems) {
            preOrderItemsList.add(preOrderItem);
        }

        preOrder.setPreOrderItems(preOrderItemsList);
        preOrder.setShopId(shop.getId());
        preOrder.setShopName(shop.getName());
        return preOrder;
    }

    @Override
    public Response<Paging<RichShopOrder>> sellerOrderList(BaseUser baseUser,
                                                     String buyerName, String buyerMobile, String buyerEmail,
                                                     Long orderId, String nids,
                                                     String startAt, String endAt,
                                                     Integer pageNo, Integer size) {
        try {
            //trans to buyerId
            Long buyerId = tranToBuyerId(buyerName, buyerMobile, buyerEmail);
            Long shopId = ((ParanaUser) baseUser).getShopId();
            List<Long> orderIds = tranToShopOrderIds(orderId);

            Response<Paging<ShopOrder>> orderPR =
                    orderReadService.findShopOrderBy(buyerId, shopId, nids, orderIds, startAt, endAt, pageNo, size);
            if (!orderPR.isSuccess()) {
                return Response.fail(orderPR.getError());
            }

            List<RichShopOrder> richShopOrders = buildRichShopOrders(orderPR.getResult().getData());
            return Response.ok(new Paging<>(
                    orderPR.getResult().getTotal(),
                    richShopOrders
            ));
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to query order list by seller {}, cause:{}",
                    baseUser, Throwables.getStackTraceAsString(e));
            return Response.fail("order.query.fail");
        }
    }

    @Override
    public Response<Paging<RichShopOrder>> buyerOrderList(BaseUser baseUser, String shopName, Long orderId,
                                                          String nids, String startAt, String endAt,
                                                          Integer pageNo, Integer size) {
        try {
            //trans to shopId
            Long shopId = transToShopId(shopName);
            List<Long> orderIds = tranToShopOrderIds(orderId);

            Response<Paging<ShopOrder>> orderPR =
                    orderReadService.findShopOrderBy(baseUser.getId(), shopId, nids, orderIds, startAt, endAt, pageNo, size);
            if (!orderPR.isSuccess()) {
                return Response.fail(orderPR.getError());
            }

            List<RichShopOrder> richShopOrders = buildRichShopOrders(orderPR.getResult().getData());
            return Response.ok(new Paging<>(
                    orderPR.getResult().getTotal(),
                    richShopOrders
            ));
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to query order list by buyer {}, cause:{}",
                    baseUser, Throwables.getStackTraceAsString(e));
            return Response.fail("order.query.fail");
        }
    }

    @Override
    public Response<Paging<RichShopOrder>> adminOrderList(String buyerName, String buyerMobile,
                                                          String buyerEmail, String shopName, Long orderId,
                                                          String nids, String startAt, String endAt,
                                                          Integer pageNo, Integer size) {
        try {
            //trans to buyerId
            Long buyerId = tranToBuyerId(buyerName, buyerMobile, buyerEmail);
            Long shopId = transToShopId(shopName);

            List<Long> orderIds = tranToShopOrderIds(orderId);

            Response<Paging<ShopOrder>> orderPR =
                    orderReadService.findShopOrderBy(buyerId, shopId, nids, orderIds, startAt, endAt, pageNo, size);
            if (!orderPR.isSuccess()) {
                return Response.fail(orderPR.getError());
            }

            List<RichShopOrder> richShopOrders = buildRichShopOrders(orderPR.getResult().getData());
            return Response.ok(new Paging<>(
                    orderPR.getResult().getTotal(),
                    richShopOrders
            ));
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to query order list by admin, cause:{}", e);
            return Response.fail("order.query.fail");
        }
    }

    protected Long tranToBuyerId(String buyerName, String buyerMobile, String buyerEmail) {
        Long buyerId = null;
        if (!Strings.isNullOrEmpty(buyerName)) {
            Response<User> userR = userReadService.findBy(buyerName, LoginType.NAME);
            if (userR.isSuccess()) {
                buyerId = userR.getResult().getId();
            } else {
                log.warn("user query by name {} fail when query order list", buyerName);
            }
        } else if (!Strings.isNullOrEmpty(buyerMobile)) {
            Response<User> userR = userReadService.findBy(buyerMobile, LoginType.MOBILE);
            if (userR.isSuccess()) {
                buyerId = userR.getResult().getId();
            } else {
                log.warn("user query by mobile {} fail when query order list", buyerMobile);
            }
        } else if (!Strings.isNullOrEmpty(buyerEmail)) {
            Response<User> userR = userReadService.findBy(buyerEmail, LoginType.EMAIL);
            if (userR.isSuccess()) {
                buyerId = userR.getResult().getId();
            } else {
                log.warn("user query by email {} fail when query order list", buyerEmail);
            }
        }
        return buyerId;
    }

    protected Long transToShopId (String shopName) {
        Long shopId = null;
        if (!Strings.isNullOrEmpty(shopName)) {
            Response<Shop> shopR = shopReadService.findByName(shopName);
            if (shopR.isSuccess()) {
                shopId = shopR.getResult().getId();
            }
        }
        return shopId;
    }

    protected List<Long> tranToShopOrderIds(Long orderId) {
        List<Long> shopOrderIds = new ArrayList<>();
        if (null != orderId) {
            shopOrderIds.add(orderId);
            return shopOrderIds;
        }
        return null;
    }

    protected List<RichShopOrder> buildRichShopOrders(List<ShopOrder> shopOrders) {
        //for dubbo serialize
        List<Long> shopOrderIds = new ArrayList<>();
        List<Long> nodeInstanceIds = new ArrayList<>();
        for (ShopOrder shopOrder : shopOrders) {
            shopOrderIds.add(shopOrder.getId());
            nodeInstanceIds.add(shopOrder.getNodeInstanceId());
        }

        //find sku orders
        Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentIds(shopOrderIds);
        if (!skuOrdersR.isSuccess()) {
            throw new ServiceException(skuOrdersR.getError());
        }
        List<SkuOrder> skuOrders = skuOrdersR.getResult();

        //add sku node instance id to nodeInstanceIds
        for (SkuOrder skuOrder : skuOrders) {
            nodeInstanceIds.add(skuOrder.getNodeInstanceId());
        }

        //find shop node instances
        Response<List<OrderNodeInstance>> shopOrderNodesR = orderNodeReadService.findByIds(nodeInstanceIds);
        if (!shopOrderNodesR.isSuccess()) {
            throw new ServiceException(shopOrderNodesR.getError());
        }
        List<OrderNodeInstance> shopOrderNodes = shopOrderNodesR.getResult();

        //sku order group by shop order id
        ArrayListMultimap<Long, SkuOrder> shopOrderIdAndSkuOrders = ArrayListMultimap.create();
        for (SkuOrder skuOrder : skuOrders) {
            shopOrderIdAndSkuOrders.put(skuOrder.getParentId(), skuOrder);
        }
        //id -> nodeInstance map
        Map<Long, OrderNodeInstance> nodeInstanceIdAndInstance = Maps.uniqueIndex(
                shopOrderNodes, new Function<OrderNodeInstance, Long>() {
                    @Override
                    public Long apply(OrderNodeInstance orderNodeInstance) {
                        return orderNodeInstance.getId();
                    }
                }
        );

        //build rich order
        List<RichShopOrder> result = new ArrayList<>();
        for (ShopOrder shopOrder : shopOrders) {
            try {
                RichShopOrder richShopOrder = new RichShopOrder();
                richShopOrder.setShopOrder(shopOrder);
                if (!Strings.isNullOrEmpty(shopOrder.getNextActionInstanceIds())) {
                    //parse shop order json to java instance
                    richShopOrder.setShopOrderActions(ParseJson.parseToUserTypeAndActions(shopOrder.getNextActionInstanceIds()));
                }
                //parse node instance json to java map
                richShopOrder.setUserTypeAndStatus(
                        ParseJson.parseToStringStringMap(nodeInstanceIdAndInstance.get(shopOrder.getNodeInstanceId()).getNames()));

                //for dubbo serialize
                List<RichSkuOrder> skuOrderForParentId = new ArrayList<>();
                for (SkuOrder skuOrder : shopOrderIdAndSkuOrders.get(shopOrder.getId())) {
                    RichSkuOrder richSkuOrder = new RichSkuOrder();
                    richSkuOrder.setSkuOrder(skuOrder);
                    //parse sku order json to java instance
                    if (!Strings.isNullOrEmpty(skuOrder.getNextActionInstanceIds())) {
                        richSkuOrder.setSkuOrderActions(ParseJson.parseToUserTypeAndActions(skuOrder.getNextActionInstanceIds()));
                    }
                    //parse node instance json to java map
                    richSkuOrder.setUserTypeAndStatus(
                            ParseJson.parseToStringStringMap(nodeInstanceIdAndInstance.get(skuOrder.getNodeInstanceId()).getNames())
                    );
                    richSkuOrder.setHasComment(skuOrder.getExtra() != null && skuOrder.getExtra().containsKey(SkuOrderExtraKeys.HAS_COMMENT));
                    skuOrderForParentId.add(richSkuOrder);
                }
                richShopOrder.setSkuOrders(skuOrderForParentId);
                result.add(richShopOrder);
            }catch (Exception e) {
                log.error("fail to build rich order by shop order {}, cause:{}, skip",
                        shopOrder, Throwables.getStackTraceAsString(e));
            }
        }
        return result;
    }



    @Override
    public Response<OrderDetail> orderDetail(ParanaUser baseUser, Long shopOrderId) {
        try {
            List<Long> nodeInstanceIds = new ArrayList<>();
            //find shop order
            Response<ShopOrder> shopOrderRes = orderReadService.findShopOrderById(shopOrderId);
            if (!shopOrderRes.isSuccess()) {
                return Response.fail(shopOrderRes.getError());
            }
            ShopOrder shopOrder = shopOrderRes.getResult();
            //fill nodeInstanceIds
            nodeInstanceIds.add(shopOrder.getNodeInstanceId());

            //check user auth
            checkViewAuth(baseUser, shopOrder);

            //find sku order
            Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrderId);
            if (!skuOrdersR.isSuccess()) {
                return Response.fail(skuOrdersR.getError());
            }
            List<SkuOrder> skuOrders = skuOrdersR.getResult();
            //fill nodeInstanceIds
            nodeInstanceIds.addAll(Lists.transform(skuOrders, new Function<SkuOrder, Long>() {
                @Override
                public Long apply(SkuOrder input) {
                    return input.getNodeInstanceId();
                }
            }));
            //find List<OrderNodeInstance>
            Response<List<OrderNodeInstance>> nodeInstancesR = orderNodeReadService.findByIds(nodeInstanceIds);
            if (!nodeInstancesR.isSuccess()) {
                return Response.fail(nodeInstancesR.getError());
            }
            List<OrderNodeInstance> nodeInstances = nodeInstancesR.getResult();
            //get id -> nodeInstance object map
            Map<Long, OrderNodeInstance> nodeInstanceMap = Maps.uniqueIndex(nodeInstances, new Function<OrderNodeInstance, Long>() {
                @Override
                public Long apply(OrderNodeInstance input) {
                    return input.getId();
                }
            });

            OrderDetail orderDetail = new OrderDetail();
            orderDetail.setShopOrder(shopOrder);
            //parse to java map for render
            if (null != nodeInstanceMap.get(shopOrder.getNodeInstanceId())) {
                orderDetail.setShopOrderInstanceName(
                        ParseJson.parseToStringStringMap(nodeInstanceMap.get(shopOrder.getNodeInstanceId()).getNames())
                );
            }
            List<SkuOrderDetail> skuOrderDetails = new ArrayList<>();
            for (SkuOrder skuOrder : skuOrders) {
                SkuOrderDetail skuOrderDetail = new SkuOrderDetail();
                skuOrderDetail.setSkuOrder(skuOrder);
                //parse to java map for render
                if (null != nodeInstanceMap.get(skuOrder.getNodeInstanceId())) {
                    skuOrderDetail.setSkuOrderInstanceName(
                            ParseJson.parseToStringStringMap(nodeInstanceMap.get(skuOrder.getNodeInstanceId()).getNames())
                    );
                }
                skuOrderDetails.add(skuOrderDetail);
            }
            orderDetail.setSkuOrderList(skuOrderDetails);

            //find order extra
            Response<OrderExtra> orderExtraR =
                    orderExtraReadService.findExtraByOrderIdAndType(shopOrderId, OrderType.SHOP_ORDER.value());
            if (orderExtraR.isSuccess()) {
                orderDetail.setOrderExtra(orderExtraR.getResult());
            } else {
                log.error("fail to find order extra by shop order id {}, error code:{}",
                        shopOrderId, orderExtraR.getError());
            }

            return Response.ok(orderDetail);
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to query order detail by user {}, shop order id {}, cause:{}",
                    baseUser, shopOrderId, Throwables.getStackTraceAsString(e));
            return Response.fail("order.query.fail");
        }
    }

    @Override
    public Response<PrePay> prePay(BaseUser baseUser, String orderIds) {
        Response<PrePay> result = new Response<PrePay>();

        try {

            PrePay prePay = new PrePay();
            checkArgument(Arguments.notNull(baseUser),"user.not.login");
            checkArgument(!Strings.isNullOrEmpty(orderIds),"order.ids.invalid");

            List<Long> ids = Splitters.splitToLong(orderIds, Splitters.COMMA);

            Response<List<ShopOrder>> listRes = orderReadService.findShopOrderByIds(ids);
            checkState(listRes.isSuccess(),listRes.getError());
            prePay.setShopOrders(listRes.getResult());
            prePay.setAid(OrderAction.PRE_PAY.value());
            result.setResult(prePay);
        }catch (IllegalStateException e){
            log.error("find pre pay info fail,where orderIds: {},errro: {}",orderIds,e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("find pre pay info fail,where orderIds: {},cause: {}",orderIds,Throwables.getStackTraceAsString(e));
            result.setError("query.shop.order.fail");
        }
        return result;
    }

    @Override
    public Response<OrderItemSnapshotDetail> findItemSnapshotBy(BaseUser baseUser, Long skuOrderId) {
        try {
            Response<SkuOrder> findSkuOrderResp = orderReadService.findSkuOrderById(skuOrderId);
            if (!findSkuOrderResp.isSuccess()) {
                log.error("find to find sku order by id:{},cause:{}", skuOrderId, findSkuOrderResp.getError());
                return Response.fail(findSkuOrderResp.getError());
            }

            //TODO 检查权限

            SkuOrder skuOrder = findSkuOrderResp.getResult();
            Response<ItemSnapshot> snapshotResp = itemSnapshotReadService.findById(skuOrder.getItemSnapshotId());
            if (!snapshotResp.isSuccess()) {
                log.error("fail to find item snapshot by id:{},cause:{}", skuOrder.getItemSnapshotId(), snapshotResp.getError());
                return Response.fail(snapshotResp.getError());
            }

            ItemSnapshot itemSnapshot = snapshotResp.getResult();
            OrderItemSnapshotDetail orderItemSnapshotDetail = new OrderItemSnapshotDetail();
            BeanMapper.copy(itemSnapshot, orderItemSnapshotDetail);
            Integer skuOriginPrice = skuOrder.getOriginFee() / skuOrder.getQuantity();
            orderItemSnapshotDetail.setSkuOriginPrice(skuOriginPrice);
            orderItemSnapshotDetail.setSkuPrice(skuOriginPrice);
            orderItemSnapshotDetail.setAttributeMap(buildAttributes(skuOrder.getSkuAttributes()));

            return Response.ok(orderItemSnapshotDetail);
        } catch (Exception e) {
            log.error("fail to find item snapshot by skuOrderId:{},,cause:{}", skuOrderId, Throwables.getStackTraceAsString(e));
            return Response.fail("find.item.snapshot.fail");
        }
    }

    @Override
    public Response<PreComment> preComment(BaseUser baseUser, Long skuOrderId) {
        try {
            Response<SkuOrder> skuOrderResp = orderReadService.findSkuOrderById(skuOrderId);
            if (!skuOrderResp.isSuccess()) {
                log.error("fail to find sku order by id:{},cause:{}", skuOrderId, skuOrderResp.getError());
                return Response.fail(skuOrderResp.getError());
            }
            SkuOrder skuOrder = skuOrderResp.getResult();

            if (!Objects.equals(skuOrder.getBuyerId(), baseUser.getId())) {
                log.error("the sku order(id={}) is not belong to user(id={})", skuOrderId, baseUser.getId());
                return Response.fail("sku.order.is.not.belong.to.user");
            }

            PreComment preComment = new PreComment();
            preComment.setOrderItems(Lists.newArrayList(skuOrder));
            return Response.ok(preComment);
        } catch (Exception e) {
            log.error("fail to find pre comment where sku order id={},cause:{}", skuOrderId, Throwables.getStackTraceAsString(e));
            return Response.fail("find.pre.comment.fail");
        }
    }

    protected void checkViewAuth(ParanaUser baseUser, ShopOrder shopOrder) {
        if (!Objects.equals(baseUser.getId(), shopOrder.getBuyerId())
                &&!Objects.equals(baseUser.getShopId(), shopOrder.getShopId())
                &&!isAdmin(baseUser) && !isOperator(baseUser)) {
            throw new ServiceException("authorize.fail");
        }
    }

    private Map<String, String> buildAttributes(String skuAttributeJson) throws IOException {
        if (Strings.isNullOrEmpty(skuAttributeJson)) {
            return Collections.EMPTY_MAP;
        }
        List<SkuAttribute> skuAttributes = JsonMapper.nonEmptyMapper().getMapper().readValue(skuAttributeJson, new TypeReference<List<SkuAttribute>>() {
        });

        Map<String, String> attributes = new HashMap<>();
        for (SkuAttribute skuAttribute : skuAttributes) {
            attributes.put(skuAttribute.getAttrKey(), skuAttribute.getAttrVal());
        }
        return attributes;
    }
}
