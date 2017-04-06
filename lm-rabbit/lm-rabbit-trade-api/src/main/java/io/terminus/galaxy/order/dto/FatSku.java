package io.terminus.galaxy.order.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Map;

/**
 * Desc: 创建订单时前端传入的sku信息
 * Mail: F@terminus.io
 * Data: 16/3/5
 * Author: yangzefeng
 */
@Data
public class FatSku implements Serializable {
    private static final long serialVersionUID = 8791474533044717422L;

    /**
     * skuId
     */
    private Long skuId;

    /**
     * 购买数量
     */
    private Integer quantity;

    /**
     * 这个map里面的字段主要给营销用
     */
    private Map<String, Object> params;
}
