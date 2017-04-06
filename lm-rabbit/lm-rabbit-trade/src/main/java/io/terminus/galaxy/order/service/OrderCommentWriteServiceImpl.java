package io.terminus.galaxy.order.service;

import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.constant.SkuOrderExtraKeys;
import io.terminus.galaxy.order.dao.OrderCommentDao;
import io.terminus.galaxy.order.internal.BadWordSensor;
import io.terminus.galaxy.order.model.OrderComment;
import io.terminus.parana.order.dao.SkuOrderDao;
import io.terminus.parana.order.model.SkuOrder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Author:cp
 * Created on 4/22/16.
 */
@Slf4j
@Service
public class OrderCommentWriteServiceImpl implements OrderCommentWriteService {

    private final OrderCommentDao orderCommentDao;

    private final BadWordSensor badWordSensor;

    private final SkuOrderDao skuOrderDao;

    @Autowired
    public OrderCommentWriteServiceImpl(OrderCommentDao orderCommentDao,
                                        BadWordSensor badWordSensor,
                                        SkuOrderDao skuOrderDao) {
        this.orderCommentDao = orderCommentDao;
        this.badWordSensor = badWordSensor;
        this.skuOrderDao = skuOrderDao;
    }

    @Override
    public Response<Map<Long, List<String>>> createOrderComment(OrderComment orderComment) {
        Response<Map<Long, List<String>>> result = new Response<>();

        try {
            Map<Long, List<String>> checkComment = Maps.newHashMap();

            //查询敏感词汇
            List<String> filterComments = badWordSensor.filterBadWords(orderComment.getContext());
            if (!CollectionUtils.isEmpty(filterComments)) {
                checkComment.put(orderComment.getItemId(), filterComments);
                return Response.ok(checkComment);
            }

            SkuOrder skuOrder = skuOrderDao.findById(orderComment.getOrderItemId());
            if (skuOrder == null) {
                log.error("sku order not found where id={}", orderComment.getOrderItemId());
                return Response.fail("sku.order.not.found");
            }

            Map<String, String> extra = skuOrder.getExtra() == null ? new HashMap<String,String>() : skuOrder.getExtra();
            if (extra.containsKey(SkuOrderExtraKeys.HAS_COMMENT)) {
                log.error("sku order(id={}) has comment already", orderComment.getOrderItemId());
                return Response.fail("sku.order.has.comment.already");
            }

            orderCommentDao.create(orderComment);

            //更新子订单为已评价
            extra.put(SkuOrderExtraKeys.HAS_COMMENT, "yes");
            SkuOrder toUpdated = new SkuOrder();
            toUpdated.setId(skuOrder.getId());
            toUpdated.setExtra(extra);
            skuOrderDao.update(toUpdated);

            result.setResult(checkComment);
            return result;
        } catch (Exception e) {
            log.error("fail to create order comment by {}, cause:{}", orderComment, Throwables.getStackTraceAsString(e));
            result.setError("order.comment.create.fail");
            return result;
        }
    }


    @Override
    public Response<Boolean> deleteOrderComment(Long orderCommentId) {
        Response<Boolean> result = new Response<>();

        try {
            orderCommentDao.delete(orderCommentId);
            result.setResult(Boolean.TRUE);
        } catch (Exception e) {
            log.error("fail to delete order comment by id {}, cause:{}",
                    orderCommentId, Throwables.getStackTraceAsString(e));
            result.setError("order.comment.delete.fail");
        }
        return result;
    }

    @Override
    public Response<List<String>> addCommentReply(Long orderCommentId, String reply, Long loginerId) {
        Response<List<String>> result = new Response<List<String>>();

        try {
            OrderComment exist = orderCommentDao.findById(orderCommentId);
            if (exist == null) {
                log.error("order comment id={} not exits when add reply", orderCommentId);
                result.setError("order.comment.not.found");
                return result;
            }
            if (!Objects.equal(exist.getSellerId(), loginerId)) {
                log.error("order comment id={} seller id is not current user id={}, auth fail when add reply",
                        orderCommentId, loginerId);
                result.setError("authorize.fail");
                return result;
            }
            OrderComment orderComment = new OrderComment();
            orderComment.setId(orderCommentId);
            orderComment.setIsReply(true);
            //todo:validate scripts
            orderComment.setReply(reply);

            List<String> checkComment = badWordSensor.filterBadWords(reply);
            if (checkComment != null && !checkComment.isEmpty()) {
                result.setResult(checkComment);
                return result;
            }

            orderCommentDao.update(orderComment);

            result.setResult(checkComment);
            return result;
        } catch (Exception e) {
            log.error("fail to add comment reply by order comment id={}, reply={}, cause:{}",
                    orderCommentId, reply, Throwables.getStackTraceAsString(e));
            result.setError("add.comment.reply.fail");
            return result;
        }
    }

    @Override
    public Response<Boolean> batchUpdateOrderComment(List<Long> orderCommentIds, Integer status) {
        Response<Boolean> result = new Response<>();

        try {
            orderCommentDao.batchUpdateStatus(orderCommentIds, status);
            result.setResult(Boolean.TRUE);
            return result;
        } catch (Exception e) {
            log.error("fail to batch update order comment status by ids {}, status {}, cause:{}",
                    orderCommentIds, status, Throwables.getStackTraceAsString(e));
            result.setError("update.order.comment.status.fail");
            return result;
        }
    }
}
