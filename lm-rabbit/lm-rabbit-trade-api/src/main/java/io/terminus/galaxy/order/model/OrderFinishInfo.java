package io.terminus.galaxy.order.model;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 完成节点时订单的信息
 * Created by cuiwentao
 * on 16/3/23
 */
@Data
public class OrderFinishInfo implements Serializable {

    private static final long serialVersionUID = -5754868996106314529L;
    /**
     * 自增id
     */
    private Long id;

    /**
     * 订单id
     */
    private Long orderId;

    /**
     * 商家id
     */
    private Long sellerId;

    /**
     * 商家名称
     */
    private String sellerName;

    /**
     * 店铺id
     */
    private Long shopId;

    /**
     * 店铺名称
     */
    private String shopName;

    /**
     * 买家id
     */
    private Long buyerId;

    /**
     * 买家名称
     */
    private String buyerName;

    /**
     * 订单类型
     */
    private Integer orderType;

    /**
     * 交易类型 1:普通,2:预售
     */
    private Integer type;

    /**
     * 支付方式 1:在线支付,2:货到付款,3:积分
     */
    private Integer payType;

    /**
     * 订单交易状态
     */
    private Integer orderStatus;

    /**
     * 系统内部流水号
     */
    private String systemNo;

    /**
     * 原价
     */
    private Long originFee;

    /**
     * 实付金额
     */
    private Long fee;

    /**
     * 优惠金额
     */
    private Long discount;

    /**
     * 运费
     */
    private Long shipFee;

    /**
     * 运费优惠金额
     */
    private Long shipFeeDiscount;

    /**
     * 积分减免金额
     */
    private Long integral;

    /**
     * 余额减免金额
     */
    private Long balance;

    /**
     * 消费税
     */
    private Long saleTax;

    /**
     * 运费中包含的消费税
     */
    private Long shipFeeSaleTax;

    /**
     * 是否已生成对应的结算单
     */
    private Boolean isSettlemented;

    /**
     * 订单结束时间(计算各种费用)
     */
    private Date finishedAt;

    /**
     * 创建时间
     */
    private Date createdAt;

    /**
     * 更新时间
     */
    private Date updatedAt;

}
