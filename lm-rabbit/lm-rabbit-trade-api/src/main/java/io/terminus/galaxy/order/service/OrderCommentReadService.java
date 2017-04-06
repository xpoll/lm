package io.terminus.galaxy.order.service;

import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.order.model.OrderComment;
import io.terminus.pampas.client.Export;
import io.terminus.parana.common.model.ParanaUser;


/**
 * Author:cp
 * Created on 4/22/16.
 */
public interface OrderCommentReadService {


    /**
     * 买家根据多个维度的条件查询订单评价信息
     *
     * @param loginer     当前登录用户
     * @param orderItemId 子订单id
     * @param itemId      商品id
     * @param itemName    商品名称
     * @param shopId      店铺id
     * @param shopName    店铺名称
     * @return 评价分页信息
     */
    @Export(paramNames = {"loginer", "orderItemId", "itemId", "itemName", "shopId", "shopName", "isReply", "reference", "status", "quality", "startAt", "endAt", "pageNo", "size"})
    Response<Paging<OrderComment>> buyerOrderCommentPaging(BaseUser loginer, Long orderItemId, Long itemId,
                                                           String itemName, Long shopId, String shopName,
                                                           Boolean isReply, String reference,
                                                           Integer status, String quality,
                                                           String startAt, String endAt, Integer pageNo, Integer size);

    /**
     * 卖家根据多个维度的条件查询订单评价信息
     *
     * @param loginer     当前登录用户
     * @param userName    买家名称
     * @param orderItemId 子订单id
     * @param itemId      商品id
     * @param itemName    商品名称
     * @return 评价分页信息
     */
    @Export(paramNames = {"loginer", "userName", "orderItemId", "itemId", "itemName", "isReply", "reference", "status", "quality", "startAt", "endAt", "pageNo", "size"})
    Response<Paging<OrderComment>> sellerOrderCommentPaging(ParanaUser loginer, String userName, Long orderItemId,
                                                            Long itemId, String itemName, Boolean isReply, String reference,
                                                            Integer status, String quality,
                                                            String startAt, String endAt, Integer pageNo, Integer size);


    /**
     * 运营根据多个维度的条件查询订单评价信息
     *
     * @param userName    买家名称
     * @param orderItemId 子订单id
     * @param itemId      商品id
     * @param itemName    商品名称
     * @param shopId      店铺id
     * @param shopName    店铺名称
     * @param status      评价状态
     * @return 评价分页信息
     */
    @Export(paramNames = {"userId", "userName", "orderItemId", "itemId", "itemName", "shopId", "shopName", "status", "isReply", "reference", "quality", "startAt", "endAt", "pageNo", "size"})
    Response<Paging<OrderComment>> adminOrderCommentPaging(Long userId, String userName, Long orderItemId, Long itemId, String itemName,
                                                           Long shopId, String shopName, Integer status,
                                                           Boolean isReply, String reference, String quality,
                                                           String startAt, String endAt, Integer pageNo, Integer size);

    /**
     * 商品详情页评价列表
     *
     * @param itemId 商品id
     * @param pageNo 页码
     * @param size   每页大小
     * @return 分页的商品评价
     */
    Response<Paging<OrderComment>> itemOrderCommentPaging(Long itemId, Integer pageNo, Integer size);

    /**
     * 商品详情页评价数量
     *
     * @param itemId 商品id
     * @return 商品的数量
     */
    Response<Long> itemOrderCommentNum(Long itemId);

}
