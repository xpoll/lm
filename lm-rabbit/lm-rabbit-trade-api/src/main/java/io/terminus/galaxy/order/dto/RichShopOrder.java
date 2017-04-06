package io.terminus.galaxy.order.dto;

import io.terminus.parana.order.dto.UserTypeAndAction;
import io.terminus.parana.order.model.ShopOrder;
import lombok.Data;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 订单列表展示信息
 * Mail: F@terminus.io
 * Data: 16/3/8
 * Author: yangzefeng
 */
@Data
public class RichShopOrder implements Serializable {
    private static final long serialVersionUID = -7844567159890516291L;

    private ShopOrder shopOrder;

    /**
     * shopOrder接下来可做的操作
     */
    private List<UserTypeAndAction> shopOrderActions;

    /**
     * shopOrder应该展示的状态, 不同的用户类型对应一个entry
     */
    private Map<String, String> userTypeAndStatus;

    private List<RichSkuOrder> skuOrders;
}
