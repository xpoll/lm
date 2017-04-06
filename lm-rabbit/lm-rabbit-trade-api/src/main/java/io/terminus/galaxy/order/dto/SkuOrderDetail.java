package io.terminus.galaxy.order.dto;

import io.terminus.parana.order.model.SkuOrder;
import lombok.Data;

import java.io.Serializable;
import java.util.Map;

/**
 * Desc: 订单详情页sku订单的信息
 * Mail: F@terminus.io
 * Data: 16/3/11
 * Author: yangzefeng
 */
@Data
public class SkuOrderDetail implements Serializable {
    private static final long serialVersionUID = 7507551912749566222L;

    private SkuOrder skuOrder;

    /**
     * skuOrder应该展示的状态, 不同的用户类型对应一个entry
     */
    private Map<String, String> skuOrderInstanceName;
}
