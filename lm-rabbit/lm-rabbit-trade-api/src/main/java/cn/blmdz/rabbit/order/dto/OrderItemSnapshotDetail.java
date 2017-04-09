package cn.blmdz.rabbit.order.dto;

import java.util.Map;

import cn.blmdz.wolf.item.model.ItemSnapshot;
import lombok.Getter;
import lombok.Setter;

public class OrderItemSnapshotDetail extends ItemSnapshot {

    @Setter
    @Getter
    private Integer skuOriginPrice;

    @Setter
    @Getter
    private Integer skuPrice;

    @Setter
    @Getter
    protected Map<String, String> attributeMap;

}