package cn.blmdz.rabbit.web.core.trade.service;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.order.dto.OrderRefundDetail;
import cn.blmdz.rabbit.order.dto.RichSkuOrderRefund;

/**
 * Mail: F@terminus.io
 * Data: 16/3/10
 * Author: yangzefeng
 */
public interface GalaxyOrderRefundReadService {

    /**
     * 查询退款单列表, 买家调用这个接口
     * @param baseUser 当前登录用户
     * @param nids 订单状态,多个状态之间用,分割
     * @param orderId sku退款单id
     * @param skuOrderId sku订单id
     * @param shopOrderId 店铺订单id
     * @param startAt 创建退款单时间
     * @param endAt 创建退款单时间
     * @return 退款单列表
     */
    @Export(paramNames = {"baseUser", "nids", "orderId", "skuOrderId", "shopOrderId", "startAt", "endAt", "pgeNo", "size"})
    Response<Paging<RichSkuOrderRefund>> buyerPagingSkuOrderRefund(BaseUser baseUser,
                                                                   String nids,
                                                                   Long orderId,
                                                                   Long skuOrderId,
                                                                   Long shopOrderId,
                                                                   String startAt,
                                                                   String endAt,
                                                                   Integer pageNo,
                                                                   Integer size);

    /**
     * 查询退款单列表, 卖家调用这个接口
     * @param baseUser 当前登录用户
     * @param nids 订单状态,多个状态之间用,分割
     * @param orderId sku退款单id
     * @param skuOrderId sku订单id
     * @param shopOrderId 店铺订单id
     * @param startAt 创建退款单时间
     * @param endAt 创建退款单时间
     * @return 退款单列表
     */
    @Export(paramNames = {"baseUser", "nids", "orderId", "skuOrderId", "shopOrderId", "startAt", "endAt", "pgeNo", "size"})
    Response<Paging<RichSkuOrderRefund>> sellerPagingSkuOrderRefund(BaseUser baseUser,
                                                               String nids,
                                                               Long orderId,
                                                               Long skuOrderId,
                                                               Long shopOrderId,
                                                               String startAt,
                                                               String endAt,
                                                               Integer pageNo,
                                                               Integer size);

    /**
     * 查询退款单列表, 运营调用这个接口
     * @param nids 订单状态,多个状态之间用,分割
     * @param orderId sku退款单id
     * @param skuOrderId sku订单id
     * @param shopOrderId 店铺订单id
     * @param startAt 创建退款单时间
     * @param endAt 创建退款单时间
     * @return 退款单列表
     */
    @Export(paramNames = {"nids", "orderId", "skuOrderId", "shopOrderId", "startAt", "endAt", "pgeNo", "size"})
    Response<Paging<RichSkuOrderRefund>> adminPagingSkuOrderRefund(String nids,
                                                                Long orderId,
                                                                Long skuOrderId,
                                                                Long shopOrderId,
                                                                String startAt,
                                                                String endAt,
                                                                Integer pageNo,
                                                                Integer size);

    /**
     * sku退款单详情
     * @param skuOrderRefundId sku退款单id
     * @return sku退款单详情
     */
    @Export(paramNames = {"skuOrderRefundId"})
    Response<OrderRefundDetail> skuOrderRefundDetail(Long skuOrderRefundId);
}
