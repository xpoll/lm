package io.terminus.galaxy.web.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.Serializable;
import java.util.Map;

/**
 * Desc: 下单后修改库存
 * Mail: F@terminus.io
 * Data: 16/4/14
 * Author: yangzefeng
 */
@AllArgsConstructor
public class StockChangeEvent implements Serializable {
    private static final long serialVersionUID = 6868328786641329780L;

    @Getter
    private Map<Long, Integer> skuIdAndQuantity;
}
