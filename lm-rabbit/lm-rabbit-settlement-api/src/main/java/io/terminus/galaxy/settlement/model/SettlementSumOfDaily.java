/*
 * Copyright (c) 2014 杭州端点网络科技有限公司
 */

package io.terminus.galaxy.settlement.model;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.Date;

/**
 * 结算每日汇总 <br>
 *
 * Mail: xiao@terminus.io <br>
 * Date: 2014-09-11 1:51 PM  <br>
 * Author: xiao
 */
@ToString
@EqualsAndHashCode
public class SettlementSumOfDaily implements Serializable {

    private static final long serialVersionUID = -4551910996294905914L;

    @Getter
    @Setter
    private Long id;                            // 自增主键

    @Getter
    @Setter
    private Integer orderCount;                 // 交易笔数(订单数)

    @Getter
    @Setter
    private Long fee;                        // 交易金额

    /**
     * 原价（商品价格和）
     */
    @Getter
    @Setter
    private Long originFee;

    /**
     * 商家营销支出
     */
    @Getter
    @Setter
    private Long sellerDiscount;

    /**
     * 运费
     */
    @Getter
    @Setter
    private Long shipFee;

    /**
     * 应收货款（原价-商家营销支出）
     */
    @Getter
    @Setter
    private Long receivable;

    /**
     * 平台营销贴现
     */
    @Getter
    @Setter
    private Long platformDiscount;

    @Getter
    @Setter
    private Long refundFee;              // 交易总支出(退款)

    @Getter
    @Setter
    private Long commission;                    // 平台抽佣(根据具体的业务规则）



    @Getter
    @Setter
    private Long integral;                  // 积分收入

    @Getter
    @Setter
    private Long thirdPartyFee;                 // 支付平台佣金

    @Getter
    @Setter
    private Long saleTax;                       //消费税

    @Getter
    @Setter
    private Date sumAt;                     // 汇总时间（指汇总的哪一天的）

    @Getter
    @Setter
    private Date createdAt;                     // 创建时间

    @Getter
    @Setter
    private Date updatedAt;                     // 修改时间

}
