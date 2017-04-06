package io.terminus.galaxy.order.model;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * Desc: 订单相关信息,一些时间节点,支付相关信息
 * Mail: F@terminus.io
 * Data: 16/3/10
 * Author: yangzefeng
 */
@Data
public class OrderExtra implements Serializable {
    private static final long serialVersionUID = 3702706885683967123L;

    /**
     * 自增id
     */
    private Long id;

    /**
     * 订单id
     */
    private Long orderId;

    /**
     * 订单类型
     */
    private Integer orderType;

    /**
     * 支付渠道 alipay, wechatpay
     */
    private String channel;

    /**
     * 交易流水号（合并支付多个订单相同）
     */
    private String tradeNo;

    /**
     * 退款流水号
     */
    private String batchNo;

    /**
     * 第三方交易流水号（合并支付多个订单相同）
     */
    private String paymentCode;
    /**
     * 系统内部流水号（合并支付多个订单相同）
     */
    private String systemNo;

    /**
     * 收货地址
     */
    private String tradeInfo;

    /**
     * 买家留言
     */
    private String buyerNotes;

    /**
     * 卖家留言
     */
    private String sellerNotes;

    /**
     * 发票信息
     */
    private String invoice;

    /**
     * 支付时间
     */
    private Date paidAt;

    /**
     * 买家取消订单时间
     */
    private Date buyerCancelAt;

    /**
     * 卖家取消订单时间
     */
    private Date sellerCancelAt;

    /**
     * 发货时间
     */
    private Date deliverAt;

    /**
     * 完成时间
     */
    private Date doneAt;

    /**
     * 申请时间
     */
    private Date applyAt;

    /**
     * 审核时间
     */
    private Date checkAt;

    /**
     * 买家发货时间
     */
    private Date buyerDeliverAt;

    /**
     * 卖家发货时间
     */
    private Date sellerDeliverAt;

    /**
     * 退款时间
     */
    private Date refundAt;

    /**
     * 创建时间
     */
    private Date createdAt;

    /**
     * 更新时间
     */
    private Date updatedAt;
}
