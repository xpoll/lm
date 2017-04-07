package cn.blmdz.rabbit.order.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 创建订单时前端传入对象
 * Mail: F@terminus.io
 * Data: 16/3/5
 * Author: yangzefeng
 */
@Data
public class FatOrder implements Serializable {
    private static final long serialVersionUID = -7006365077642283042L;

    /**
     * 店铺id
     */
    private Long shopId;

    /**
     * 交易流程有可能死外部决定,也有可能是营销平台决定
     * todo: 流程id是否由前端来传
     */
    private Long flowId;

    /**
     * 买家留言
     */
    private String buyerNotes;

    /**
     * 发票信息
     */
    private String invoiceJson;

    /**
     * 订单支付类型 1-在线支付 2-货到付款
     */
    private Integer payType;

    /**
     * sku级别的信息
     */
    private List<FatSku> fatSkus;

    /**
     * 这个字段主要给营销用
     */
    private Map<String, Object> params;
}
