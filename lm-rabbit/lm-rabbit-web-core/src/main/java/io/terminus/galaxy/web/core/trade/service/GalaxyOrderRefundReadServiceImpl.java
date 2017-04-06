package io.terminus.galaxy.web.core.trade.service;

import com.google.common.base.Function;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.dto.OrderRefundDetail;
import io.terminus.galaxy.order.dto.RichSkuOrderRefund;
import io.terminus.galaxy.web.core.trade.util.ParseJson;
import io.terminus.parana.common.model.ParanaUser;
import io.terminus.parana.order.model.OrderNodeInstance;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderNodeReadService;
import io.terminus.parana.order.service.OrderReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Mail: F@terminus.io
 * Data: 16/3/10
 * Author: yangzefeng
 */
@Service @Slf4j
public class GalaxyOrderRefundReadServiceImpl implements GalaxyOrderRefundReadService {

    private final OrderReadService orderReadService;

    private final OrderNodeReadService orderNodeReadService;

    @Autowired
    private GalaxyOrderRefundReadServiceImpl(OrderReadService orderReadService,
                                             OrderNodeReadService orderNodeReadService) {
        this.orderReadService = orderReadService;
        this.orderNodeReadService = orderNodeReadService;
    }

    @Override
    public Response<Paging<RichSkuOrderRefund>> buyerPagingSkuOrderRefund(BaseUser baseUser,
                                                                          String nids,
                                                                          Long orderId,
                                                                          Long skuOrderId,
                                                                          Long shopOrderId,
                                                                          String startAt, String endAt,
                                                                          Integer pageNo, Integer size) {
        try {
            List<Long> orderIds = transToOrderIds(orderId);
            List<Long> parentIds = transToParentIds(skuOrderId, shopOrderId);

            Response<Paging<SkuOrderRefund>> skuOrderRefundPR = orderReadService.findSkuOrderRefundBy(
                    baseUser.getId(), null, parentIds, nids, orderIds, startAt, endAt, pageNo, size
            );
            if (!skuOrderRefundPR.isSuccess()) {
                return Response.fail(skuOrderRefundPR.getError());
            }
            Paging<SkuOrderRefund> skuOrderRefundPaging = skuOrderRefundPR.getResult();
            return Response.ok(new Paging<>(skuOrderRefundPaging.getTotal(),
                    tranToRichSkuRefunds(skuOrderRefundPaging.getData())));
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to paging sku order refund, cause:{}", e);
            return Response.fail("sku.order.refund.query.fail");
        }
    }

    @Override
    public Response<Paging<RichSkuOrderRefund>> sellerPagingSkuOrderRefund(BaseUser baseUser, String nids,
                                                                       Long orderId, Long skuOrderId, Long shopOrderId,
                                                                       String startAt, String endAt,
                                                                       Integer pageNo, Integer size) {
        try {
            Long shopId = ((ParanaUser)baseUser).getShopId();
            List<Long> orderIds = transToOrderIds(orderId);
            List<Long> parentIds = transToParentIds(skuOrderId, shopOrderId);

            Response<Paging<SkuOrderRefund>> skuOrderRefundPR = orderReadService.findSkuOrderRefundBy(
                    null, shopId, parentIds, nids, orderIds, startAt, endAt, pageNo, size
            );
            if (!skuOrderRefundPR.isSuccess()) {
                return Response.fail(skuOrderRefundPR.getError());
            }
            Paging<SkuOrderRefund> skuOrderRefundPaging = skuOrderRefundPR.getResult();
            return Response.ok(new Paging<>(skuOrderRefundPaging.getTotal(),
                    tranToRichSkuRefunds(skuOrderRefundPaging.getData())));
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to paging sku order refund, cause:{}", e);
            return Response.fail("sku.order.refund.query.fail");
        }
    }

    @Override
    public Response<Paging<RichSkuOrderRefund>> adminPagingSkuOrderRefund(String nids,
                                                                      Long orderId, Long skuOrderId, Long shopOrderId,
                                                                      String startAt, String endAt,
                                                                      Integer pageNo, Integer size) {
        try {
            List<Long> orderIds = transToOrderIds(orderId);
            List<Long> parentIds = transToParentIds(skuOrderId, shopOrderId);

            Response<Paging<SkuOrderRefund>> skuOrderRefundPR = orderReadService.findSkuOrderRefundBy(
                    null, null, parentIds, nids, orderIds, startAt, endAt, pageNo, size
            );
            if (!skuOrderRefundPR.isSuccess()) {
                return Response.fail(skuOrderRefundPR.getError());
            }
            Paging<SkuOrderRefund> skuOrderRefundPaging = skuOrderRefundPR.getResult();
            return Response.ok(new Paging<>(skuOrderRefundPaging.getTotal(),
                    tranToRichSkuRefunds(skuOrderRefundPaging.getData())));
        }catch (ServiceException se) {
            return Response.fail(se.getMessage());
        }catch (Exception e) {
            log.error("fail to paging sku order refund, cause:{}", e);
            return Response.fail("sku.order.refund.query.fail");
        }
    }

    protected Long transToShopId(BaseUser baseUser) {
        ParanaUser user = (ParanaUser) baseUser;
        return user.getShopId();
    }

    protected List<Long> transToOrderIds(Long orderId) {
        List<Long> skuOrderRefundIds = new ArrayList<>();

        if (null != orderId) {
            skuOrderRefundIds.add(orderId);
            return skuOrderRefundIds;
        }
        return null;
    }

    protected List<Long> transToParentIds(Long skuOrderId, Long shopOrderId) {
        List<Long> parentIds = new ArrayList<>();

        if (null != skuOrderId) {
            parentIds.add(skuOrderId);
            return parentIds;
        }
        if (null != shopOrderId) {
            Response<List<SkuOrder>> skuOrdersR = orderReadService.findSkuOrderByParentId(shopOrderId);
            if (!skuOrdersR.isSuccess()) {
                throw new ServiceException(skuOrdersR.getError());
            }
            List<SkuOrder> skuOrders = skuOrdersR.getResult();
            //for dubbo serialize
            for (SkuOrder skuOrder : skuOrders) {
                parentIds.add(skuOrder.getId());
            }
            return parentIds;
        }
        return null;
    }

    protected List<RichSkuOrderRefund> tranToRichSkuRefunds(List<SkuOrderRefund> skuOrderRefunds) {
        List<RichSkuOrderRefund> richs = new ArrayList<>();
        List<Long> nids = Lists.transform(skuOrderRefunds, new Function<SkuOrderRefund, Long>() {
            @Override
            public Long apply(SkuOrderRefund input) {
                return input.getNodeInstanceId();
            }
        });
        Response<List<OrderNodeInstance>> nodeInstancesR = orderNodeReadService.findByIds(Lists.newArrayList(nids));
        if (!nodeInstancesR.isSuccess()) {
            throw new ServiceException(nodeInstancesR.getError());
        }
        Map<Long, OrderNodeInstance> nodeInstanceMap = Maps.uniqueIndex(nodeInstancesR.getResult(), new Function<OrderNodeInstance, Long>() {
            @Override
            public Long apply(OrderNodeInstance input) {
                return input.getId();
            }
        });
        for (SkuOrderRefund skuOrderRefund : skuOrderRefunds) {
            try {
                RichSkuOrderRefund richSkuOrderRefund = new RichSkuOrderRefund();
                richSkuOrderRefund.setSkuOrderRefund(skuOrderRefund);
                richSkuOrderRefund.setSkuOrderActions(ParseJson.parseToUserTypeAndActions(skuOrderRefund.getNextActionInstanceIds()));
                if (null != nodeInstanceMap.get(skuOrderRefund.getNodeInstanceId())) {
                    richSkuOrderRefund.setUserTypeAndStatus(
                            ParseJson.parseToStringStringMap(nodeInstanceMap.get(skuOrderRefund.getNodeInstanceId()).getNames())
                    );
                }
                richs.add(richSkuOrderRefund);
            }catch (Exception e) {
                log.error("fail to trans sku refund {}, cause:{}, skip",
                        skuOrderRefund, Throwables.getStackTraceAsString(e));
            }
        }
        return richs;
    }

    @Override
    public Response<OrderRefundDetail> skuOrderRefundDetail(Long skuOrderRefundId) {
        try {
            //find sku order refund
            Response<SkuOrderRefund> skuOrderRefundR =
                    orderReadService.findSkuOrderRefundById(skuOrderRefundId);
            if (!skuOrderRefundR.isSuccess()) {
                return Response.fail(skuOrderRefundR.getError());
            }
            SkuOrderRefund skuOrderRefund = skuOrderRefundR.getResult();

            //find sku order
            Long skuOrderId = skuOrderRefund.getParentId();
            Response<SkuOrder> skuOrderR = orderReadService.findSkuOrderById(skuOrderId);
            if (!skuOrderR.isSuccess()) {
                return Response.fail(skuOrderR.getError());
            }
            SkuOrder skuOrder = skuOrderR.getResult();

            //find order node instance
            Response<OrderNodeInstance> nodeInstanceR =
                    orderNodeReadService.findById(skuOrderRefund.getNodeInstanceId());
            if (!nodeInstanceR.isSuccess()) {
                return Response.fail(nodeInstanceR.getError());
            }
            OrderNodeInstance orderNodeInstance = nodeInstanceR.getResult();

            OrderRefundDetail orderRefundDetail = new OrderRefundDetail();
            orderRefundDetail.setSkuOrder(skuOrder);
            orderRefundDetail.setSkuOrderRefund(skuOrderRefund);
            orderRefundDetail.setSkuOrderRefundInstanceName(
                    ParseJson.parseToStringStringMap(orderNodeInstance.getNames())
            );

            return Response.ok(orderRefundDetail);
        }catch (Exception e) {
            log.error("fail to find sku refund order detail by skuOrderRefund id {}, cause:{}",
                    skuOrderRefundId, Throwables.getStackTraceAsString(e));
            return Response.fail("order.refund.detail.query.fail");
        }
    }
}
