package cn.blmdz.rabbit.order.dto;

import java.io.Serializable;
import java.util.Map;

import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import lombok.Data;

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
