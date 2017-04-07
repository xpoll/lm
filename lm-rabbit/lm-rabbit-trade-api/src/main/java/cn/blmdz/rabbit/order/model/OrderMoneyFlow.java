package cn.blmdz.rabbit.order.model;

import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 订单资金流水
 * Created by cuiwentao
 * on 16/3/23
 */
@Data
public class OrderMoneyFlow implements Serializable {
    private static final long serialVersionUID = -8153048463429006115L;

    /**
     * 自增id
     */
    private Long id;

    /**
     * 关联订单id
     */
    private Long orderId;

    /**
     * 订单类型
     */
    private Integer orderType;

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
     * 业务类型 1:支付,2:售中退款,3:售后退款
     */
    private Integer type;

    /**
     * 支付方式 1:在线支付,2:货到付款,3:积分
     */
    private Integer payType;

    /**
     * 支付渠道
     */
    private String channel;

    /**
     * 系统内部流水号
     */
    private String systemNo;

    /**
     * 支付流水号,供支付使用
     */
    private String tradeNo;

    /**
     * 第三方流水号
     */
    private String paymentCode;

    /**
     * 退款批次号
     */
    private String batchNo;

    /**
     * 金额(实付或者退款)
     */
    private Long fee;

    /**
     * 备注信息
     */
    private String memo;

    /**
     * 付款或退款时间
     */
    private Date tradeAt;

    /**
     * 是否已生成对应的结算单
     */
    private Boolean isSettlemented;


    /**
     * 创建时间
     */
    private Date createdAt;

    /**
     * 更新时间
     */
    private Date updatedAt;

}
