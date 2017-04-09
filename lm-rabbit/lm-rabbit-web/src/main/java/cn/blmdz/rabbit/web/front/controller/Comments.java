package cn.blmdz.rabbit.web.front.controller;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.order.model.OrderComment;
import cn.blmdz.rabbit.order.service.OrderCommentReadService;
import cn.blmdz.rabbit.order.service.OrderCommentWriteService;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.shop.model.Shop;
import cn.blmdz.wolf.shop.service.ShopReadService;
import cn.blmdz.wolf.user.model.UserProfile;
import cn.blmdz.wolf.user.service.UserProfileReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Author:cp
 * Created on 4/26/16.
 */
@Slf4j
@RestController
@RequestMapping("/api")
public class Comments {

    @Autowired
    private OrderCommentReadService orderCommentReadService;
    @Autowired
    private OrderCommentWriteService orderCommentWriteService;
    @Autowired
    private UserProfileReadService userProfileReadService;
    @Autowired
    private OrderReadService orderReadService;
    @Autowired
    private ShopReadService shopReadService;

    private static final JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();

    @RequestMapping(value = "/buyer/comment", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    public Map<Long, List<String>> createOrderComment(@RequestParam("data") String orderComments) {
        List<OrderComment> comments = JSON_MAPPER.fromJson(orderComments, JSON_MAPPER.createCollectionType(List.class, OrderComment.class));

        Long userId = UserUtil.getUserId();

        String avatar = null;
        Response<UserProfile> profileR = userProfileReadService.findProfileByUserId(userId);
        if (!profileR.isSuccess()) {
            log.warn("fail to find user profile by user id {} when create order comment, error code:{}",
                    userId, profileR.getError());
        } else {
            UserProfile userProfile = profileR.getResult();
            if (userProfile != null) {
                avatar = userProfile.getAvatar();
            }
        }

        for (OrderComment orderComment : comments) {
            orderComment.setUserId(userId);
            orderComment.setAvatar(avatar);
            makeOrderComment(orderComment);
        }

        Response<Map<Long, List<String>>> result = orderCommentWriteService.createOrderComment(comments.get(0));
        if (!result.isSuccess()) {
            log.error("fail to create order comments {}, error code:{}", comments, result.getError());
            throw new JsonResponseException(result.getError());
        }

        return result.getResult();
    }

    @RequestMapping(value = "/seller/comment/{id}/reply", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
    public List<String> addCommentReply(@PathVariable("id") Long orderCommentId,
                                        @RequestParam("reply") String reply) {
        Long userId = UserUtil.getUserId();

        Response<List<String>> result = orderCommentWriteService.addCommentReply(orderCommentId, reply, userId);
        if (!result.isSuccess()) {
            log.error("fail to add comment reply for comment id={}, reply={}, seller id={}, error code:{}",
                    orderCommentId, reply, userId, result.getError());
            throw new JsonResponseException(result.getError());
        }

        return result.getResult();
    }

    @RequestMapping(value = "/item/comment", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    public Paging<OrderComment> getItemOrderComment(@RequestParam("itemId") Long itemId,
                                                    @RequestParam(value = "pageNo", required = false) Integer pageNo,
                                                    @RequestParam(value = "size", required = false) Integer size) {

        Response<Paging<OrderComment>> result = orderCommentReadService.itemOrderCommentPaging(itemId, pageNo, size);

        if (!result.isSuccess()) {
            log.error("fail to get item order comment by item id={}, error code:{}",
                    itemId, result.getError());
            throw new JsonResponseException(result.getError());
        }

        return result.getResult();
    }

    @RequestMapping(value = "/item/comment/num", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    public Long getItemCommentNum(@RequestParam("itemId") Long itemId) {
        Response<Long> result = orderCommentReadService.itemOrderCommentNum(itemId);

        if (!result.isSuccess()) {
            throw new JsonResponseException(result.getError());
        }
        return result.getResult();
    }

    private OrderComment makeOrderComment(OrderComment orderComment) {
        BaseUser baseUser = UserUtil.getCurrentUser();
        orderComment.setUserName(baseUser.getName());

        Response<SkuOrder> skuOrderResp = orderReadService.findSkuOrderById(orderComment.getOrderItemId());
        if (!skuOrderResp.isSuccess()) {
            log.error("fail to find sku order by id:{},cause:{}", orderComment.getOrderItemId(), skuOrderResp.getError());
            throw new JsonResponseException(skuOrderResp.getError());
        }
        SkuOrder skuOrder = skuOrderResp.getResult();
        orderComment.setItemId(skuOrder.getItemId());
        orderComment.setItemName(skuOrder.getItemName());

        Response<Shop> shopR = shopReadService.findById(skuOrder.getShopId());
        if (!shopR.isSuccess() || shopR.getResult() == null) {
            log.error("fail to find shop by id={} when create shop comment", skuOrder.getShopId());
            throw new ServiceException("order.comment.create.fail");
        }
        Shop shop = shopR.getResult();

        orderComment.setShopId(shop.getId());
        orderComment.setShopName(shop.getName());
        orderComment.setSellerId(shop.getUserId());
        orderComment.setStatus(OrderComment.Status.NORMAL.value());
        orderComment.setIsReply(false);

        return orderComment;
    }
}
