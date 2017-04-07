package cn.blmdz.rabbit.order.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.rabbit.order.dao.OrderCommentDao;
import cn.blmdz.rabbit.order.model.OrderComment;
import cn.blmdz.rabbit.order.service.OrderCommentReadService;
import cn.blmdz.wolf.common.model.ParanaUser;
import lombok.extern.slf4j.Slf4j;

/**
 * Author:cp
 * Created on 4/22/16.
 */
@Slf4j
@Service
public class OrderCommentReadServiceImpl implements OrderCommentReadService {

    private final OrderCommentDao orderCommentDao;

    @Autowired
    public OrderCommentReadServiceImpl(OrderCommentDao orderCommentDao) {
        this.orderCommentDao = orderCommentDao;
    }

    @Override
    public Response<Paging<OrderComment>> buyerOrderCommentPaging(BaseUser baseUser, Long orderItemId, Long itemId,
                                                                  String itemName, Long shopId, String shopName,
                                                                  Boolean isReply, String reference,
                                                                  Integer status, String quality,
                                                                  String startAt, String endAt, Integer pageNo, Integer size) {

        try {
            Map<String, Object> params = new QueryParams.Builder().userId(baseUser.getId()).orderItemId(orderItemId).itemId(itemId)
                    .itemName(itemName).shopId(shopId).shopName(shopName).status(status).quality(quality).pageInfo(pageNo, size)
                    .startAt(startAt).endAt(endAt).reference(reference).build().toMap();

            Paging<OrderComment> orderCommentPaging = orderCommentDao.pagingFindBy(params);
            return Response.ok(orderCommentPaging);
        } catch (Exception e) {
            log.error("fail to find order comment paging by buyer id={},orderItemId={},itemId={},itemName={},shopId={},shopName={},cause:{}",
                    baseUser.getId(), orderItemId, itemId, itemName, shopId, shopName, Throwables.getStackTraceAsString(e));
            return Response.fail("order.comment.query.fail");
        }
    }

    @Override
    public Response<Paging<OrderComment>> sellerOrderCommentPaging(ParanaUser baseUser, String userName, Long orderItemId,
                                                                   Long itemId, String itemName, Boolean isReply, String reference,
                                                                   Integer status, String quality,
                                                                   String startAt, String endAt, Integer pageNo, Integer size) {

        try {
            Map<String, Object> params = new QueryParams.Builder().userName(userName).orderItemId(orderItemId)
                    .itemId(itemId).itemName(itemName).shopId(baseUser.getShopId()).status(status).quality(quality).isReply(isReply)
                    .startAt(startAt).endAt(endAt).reference(reference).pageInfo(pageNo, size).build().toMap();

            Paging<OrderComment> orderCommentPaging = orderCommentDao.pagingFindBy(params);
            return Response.ok(orderCommentPaging);
        } catch (Exception e) {
            log.error("fail to find order comment paging by shop id={},userName={},orderItemId={},itemId={},itemName={},cause:{}",
                    baseUser.getShopId(), userName, orderItemId, itemId, itemName, Throwables.getStackTraceAsString(e));
            return Response.fail("order.comment.query.fail");
        }
    }

    @Override
    public Response<Paging<OrderComment>> adminOrderCommentPaging(Long userId, String userName, Long orderItemId, Long itemId,
                                                                  String itemName, Long shopId, String shopName,
                                                                  Integer status, Boolean isReply, String reference, String quality,
                                                                  String startAt, String endAt, Integer pageNo, Integer size) {

        try {
            Map<String, Object> params = new QueryParams.Builder().userId(userId).userName(userName).orderItemId(orderItemId).itemId(itemId).itemName(itemName)
                    .shopId(shopId).shopName(shopName).status(status).quality(quality).isReply(isReply).reference(reference)
                    .startAt(startAt).endAt(endAt).pageInfo(pageNo, size).build().toMap();

            Paging<OrderComment> orderCommentPaging = orderCommentDao.pagingFindBy(params);
            return Response.ok(orderCommentPaging);
        } catch (Exception e) {
            log.error("fail to find order comment paging by userName={},orderItemId={},itemId={},itemName={},shopId={},shopName={},cause:{}",
                    userName, orderItemId, itemId, itemName, shopId, shopName, Throwables.getStackTraceAsString(e));
            return Response.fail("order.comment.query.fail");
        }
    }

    @Override
    public Response<Paging<OrderComment>> itemOrderCommentPaging(Long itemId, Integer pageNo, Integer size) {
        try {
            Map<String, Object> params = new QueryParams.Builder().itemId(itemId).status(OrderComment.Status.NORMAL.value()).pageInfo(pageNo, size).build().toMap();

            Paging<OrderComment> orderCommentPaging = orderCommentDao.pagingFindBy(params);
            return Response.ok(orderCommentPaging);
        } catch (Exception e) {
            log.error("fail to find order comment paging by item id={}, pageNo={}, size={}, cause:{}",
                    itemId, pageNo, size, Throwables.getStackTraceAsString(e));
            return Response.fail("order.comment.query.fail");
        }
    }

    @Override
    public Response<Long> itemOrderCommentNum(Long itemId) {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("itemId", itemId);

            Long num = orderCommentDao.count(params);
            return Response.ok(num);
        } catch (Exception e) {
            log.error("fail to get item order comment num by item id {},cause:{}",
                    itemId, Throwables.getStackTraceAsString(e));
            return Response.fail("order.comment.query.fail");
        }
    }

    public static class QueryParams {

        private Map<String, Object> params;

        private static final DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd");

        private QueryParams(Builder builder) {
            this.params = builder.params;
        }

        public static class Builder {
            private Map<String, Object> params;

            public Builder() {
                params = new HashMap<>();
            }

            public Builder type(Integer type) {
                if (!Arguments.isNull(type)) {
                    this.params.put("type", type);
                }
                return this;
            }

            public Builder status(Integer status) {
                if (!Arguments.isNull(status)) {
                    this.params.put("status", status);
                }
                return this;
            }

            public Builder sellerId(Long sellerId) {
                if (!Arguments.isNull(sellerId)) {
                    this.params.put("sellerId", sellerId);
                }
                return this;
            }

            public Builder startAt(String startAt) {
                if (!Strings.isNullOrEmpty(startAt)) {
                    DateTime start = DATE_TIME_FORMAT.parseDateTime(startAt).withTimeAtStartOfDay();
                    params.put("startAt", start.toDate());
                }
                return this;
            }

            public Builder endAt(String endAt) {
                if (!Strings.isNullOrEmpty(endAt)) {
                    DateTime end = DATE_TIME_FORMAT.parseDateTime(endAt).plusDays(1).withTimeAtStartOfDay();
                    params.put("endAt", end.toDate());
                }
                return this;
            }

            public Builder userId(Long userId) {
                if (!Arguments.isNull(userId)) {
                    params.put("userId", userId);
                }
                return this;
            }

            public Builder userName(String userName) {
                if (!Strings.isNullOrEmpty(userName)) {
                    params.put("userName", userName);
                }
                return this;
            }

            public Builder orderItemId(Long orderItemId) {
                if (!Arguments.isNull(orderItemId)) {
                    params.put("orderItemId", orderItemId);
                }
                return this;
            }

            public Builder itemId(Long itemId) {
                if (!Arguments.isNull(itemId)) {
                    params.put("itemId", itemId);
                }
                return this;
            }

            public Builder itemName(String itemName) {
                if (!Strings.isNullOrEmpty(itemName)) {
                    params.put("itemName", itemName);
                }
                return this;
            }

            public Builder shopId(Long shopId) {
                if (!Arguments.isNull(shopId)) {
                    params.put("shopId", shopId);
                }
                return this;
            }

            public Builder shopName(String shopName) {
                if (!Strings.isNullOrEmpty(shopName)) {
                    params.put("shopName", shopName);
                }
                return this;
            }

            public Builder isReply(Boolean isReply) {
                if (!Arguments.isNull(isReply)) {
                    params.put("isReply", isReply);
                }
                return this;
            }

            public Builder reference(String reference) {
                if (!Strings.isNullOrEmpty(reference)) {
                    params.put("reference", reference);
                }
                return this;
            }

            public Builder quality(String quality) {
                if (!Strings.isNullOrEmpty(quality)) {
                    List<Integer> qualities = Splitters.splitToInteger(quality, Splitters.COMMA);
                    params.put("quality", qualities);
                }
                return this;
            }

            public Builder pageInfo(Integer pageNo, Integer size) {
                PageInfo pageInfo = PageInfo.of(pageNo, size);
                params.put("offset", pageInfo.getOffset());
                params.put("limit", pageInfo.getLimit());
                return this;
            }

            public QueryParams build() {
                return new QueryParams(this);
            }
        }

        public Map<String, Object> toMap() {
            return this.params;
        }
    }

}
