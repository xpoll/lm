package cn.blmdz.rabbit.order.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.wolf.order.model.ShopOrder;
import lombok.Data;

/**
 * Desc: 订单详情页的信息
 * Mail: F@terminus.io
 * Data: 16/3/8
 * Author: yangzefeng
 */
@Data
public class OrderDetail implements Serializable {
    private static final long serialVersionUID = -6185634087032036288L;

    private ShopOrder shopOrder;

    /**
     * shopOrder应该展示的状态, 不同的用户类型对应一个entry
     */
    private Map<String, String> shopOrderInstanceName;
    
    private List<SkuOrderDetail> skuOrderList;

    private OrderExtra orderExtra;
}
