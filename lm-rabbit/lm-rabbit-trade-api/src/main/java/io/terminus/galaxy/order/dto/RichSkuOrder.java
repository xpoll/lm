package io.terminus.galaxy.order.dto;

import io.terminus.parana.order.dto.UserTypeAndAction;
import io.terminus.parana.order.model.SkuOrder;
import lombok.Data;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 订单列表sku订单展示信息
 * Mail: F@terminus.io
 * Data: 16/3/8
 * Author: yangzefeng
 */
@Data
public class RichSkuOrder implements Serializable {
    private static final long serialVersionUID = 1335455439729325735L;

    private SkuOrder skuOrder;

    /**
     * skuOrder接下来可做的操作
     */
    private List<UserTypeAndAction> skuOrderActions;

    /**
     * skuOrder应该展示的状态,不同的用户类型对应一个entry
     */
    private Map<String, String> userTypeAndStatus;

    /**
     * 是否已评价
     */
    private boolean hasComment;
}
