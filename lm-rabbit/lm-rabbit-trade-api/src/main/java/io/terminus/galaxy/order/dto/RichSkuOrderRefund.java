package io.terminus.galaxy.order.dto;

import io.terminus.parana.order.dto.UserTypeAndAction;
import io.terminus.parana.order.model.SkuOrderRefund;
import lombok.Data;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: sku退款单列表页展示内容
 * Mail: F@terminus.io
 * Data: 16/3/11
 * Author: yangzefeng
 */
@Data
public class RichSkuOrderRefund implements Serializable {
    private static final long serialVersionUID = -2237726927709787479L;

    private SkuOrderRefund skuOrderRefund;

    /**
     * skuOrder接下来可做的操作
     */
    private List<UserTypeAndAction> skuOrderActions;

    /**
     * skuOrder应该展示的状态, 不同的用户类型对应一个entry
     */
    private Map<String, String> userTypeAndStatus;
}
