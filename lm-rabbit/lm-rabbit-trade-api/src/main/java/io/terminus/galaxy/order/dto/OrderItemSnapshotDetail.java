package io.terminus.galaxy.order.dto;

import io.terminus.parana.item.model.ItemSnapshot;
import lombok.Getter;
import lombok.Setter;

import java.util.Map;

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