package io.terminus.galaxy.order.dto;

import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import lombok.Data;

import java.io.Serializable;
import java.util.Map;

/**
 * Desc: 退款单详情页信息
 * Mail: F@terminus.io
 * Data: 16/3/18
 * Author: yangzefeng
 */
@Data
public class OrderRefundDetail implements Serializable {
    private static final long serialVersionUID = -2534750718053529005L;

    private SkuOrder skuOrder;

    private SkuOrderRefund skuOrderRefund;

    /**
     * sku退款单应该展示的状态, 不同的用户类型对应一个entry
     */
    private Map<String, String> skuOrderRefundInstanceName;
}
