package cn.blmdz.rabbit.web.core.trade.service;

import java.util.List;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.order.dto.OrderDetail;
import cn.blmdz.rabbit.order.dto.OrderItemSnapshotDetail;
import cn.blmdz.rabbit.order.dto.PreComment;
import cn.blmdz.rabbit.order.dto.PreOrder;
import cn.blmdz.rabbit.order.dto.PrePay;
import cn.blmdz.rabbit.order.dto.RichShopOrder;
import cn.blmdz.wolf.common.model.ParanaUser;

/**
 * Desc: bbc 解决方案订单读服务
 * Mail: F@terminus.io
 * Data: 16/3/5
 * Author: yangzefeng
 */
public interface GalaxyOrderReadService {

    /**
     * 返回下单预览页需要展示的信息
     *
     * @return 下单预览页需要展示的信息
     */
    @Export(paramNames = {"skus"})
    Response<List<PreOrder>> preOrder(String skus);


    /**
     * 订单列表页服务,卖家调用这个接口
     *
     * @param baseUser    当前登录用户
     * @param buyerName   买家nickName
     * @param buyerMobile 买家手机号
     * @param buyerEmail  买家邮箱
     * @param orderId     订单id
     * @param nids        订单状态,多个状态之间用,分割
     * @param startAt     订单创建时间
     * @param endAt       订单创建时间
     * @param pageNo      页码
     * @param size        每页大小
     * @return 分页的RichOrder, RichOrder里面包含了ShopOrder信息,以及和ShopOrder关联的SkuOrder列表
     */
    @Export(paramNames = {"baseUser", "buyerName", "buyerMobile", "buyerEmail", "orderId", "nids", "startAt", "endAt", "pageNo", "size"})
    Response<Paging<RichShopOrder>> sellerOrderList(BaseUser baseUser,
                                                    String buyerName,
                                                    String buyerMobile,
                                                    String buyerEmail,
                                                    Long orderId,
                                                    String nids,
                                                    String startAt, String endAt,
                                                    Integer pageNo, Integer size);

    /**
     * 订单列表页服务,买家调用这个接口
     *
     * @param baseUser 当前登录用户
     * @param orderId  订单id
     * @param nids     订单状态,多个状态之间用,分割
     * @param startAt  订单创建时间
     * @param endAt    订单创建时间
     * @param pageNo   页码
     * @param size     每页大小
     * @return 分页的RichOrder, RichOrder里面包含了ShopOrder信息,以及和ShopOrder关联的SkuOrder列表
     */
    @Export(paramNames = {"baseUser", "shopName", "orderId", "nids", "startAt", "endAt", "pageNo", "size"})
    Response<Paging<RichShopOrder>> buyerOrderList(BaseUser baseUser,
                                                   String shopName,
                                                   Long orderId,
                                                   String nids,
                                                   String startAt, String endAt,
                                                   Integer pageNo, Integer size);


    /**
     * 订单列表页服务,运营调用这个接口
     *
     * @param buyerName   买家nickName
     * @param buyerMobile 买家手机号
     * @param buyerEmail  买家邮箱
     * @param shopName    店铺名称
     * @param orderId     订单id
     * @param nids        订单状态,多个状态之间用,分割
     * @param startAt     订单创建时间
     * @param endAt       订单创建时间
     * @param pageNo      页码
     * @param size        每页大小
     * @return 分页的RichOrder, RichOrder里面包含了ShopOrder信息,以及和ShopOrder关联的SkuOrder列表
     */
    @Export(paramNames = {"buyerName", "buyerMobile", "buyerEmail", "shopName", "orderId", "nids", "startAt", "endAt", "pageNo", "size"})
    Response<Paging<RichShopOrder>> adminOrderList(String buyerName,
                                                   String buyerMobile,
                                                   String buyerEmail,
                                                   String shopName,
                                                   Long orderId,
                                                   String nids,
                                                   String startAt, String endAt,
                                                   Integer pageNo, Integer size);

    /**
     * 订单详情页, 各种脏东西都找一把
     *
     * @param baseUser    当前登录用户
     * @param shopOrderId 订单id
     * @return ShopOrder, List<SkuOrder>, TradeInfo, OrderTrack (后面可能会加入营销的逻辑)
     */
    @Export(paramNames = {"baseUser", "shopOrderId"})
    Response<OrderDetail> orderDetail(ParanaUser baseUser, Long shopOrderId);


    /**
     * 返回支付预览页需要展示的信息
     *
     * @return 支付预览页需要展示的信息
     */
    @Export(paramNames = {"baseUser", "orderIds"})
    Response<PrePay> prePay(BaseUser baseUser, String orderIds);

    /**
     * 查询订单指定商品的快照
     *
     * @param baseUser 当前登录用户
     * @param skuOrderId  子订单id
     * @return 商品快照
     */
    @Export(paramNames = {"baseUser", "skuOrderId"})
    Response<OrderItemSnapshotDetail> findItemSnapshotBy(BaseUser baseUser, Long skuOrderId);

    /**
     * 买家评论预览页
     *
     * @param baseUser   当前登录买家
     * @param skuOrderId 子订单id
     * @return 子订单信息
     */
    @Export(paramNames = {"baseUser", "skuOrderId"})
    Response<PreComment> preComment(BaseUser baseUser, Long skuOrderId);
}
