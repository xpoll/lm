package cn.blmdz.rabbit.web.core.trade.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.Serializable;
import java.util.Map;

/**
 * Desc:
 * Mail: F@terminus.io
 * Data: 16/6/28
 * Author: yangzefeng
 */
@AllArgsConstructor
public class RollbakStockEvent implements Serializable {
    private static final long serialVersionUID = 2669997071923961957L;

    @Getter
    private Map<Long, Integer> skuIdAndQuantity;
}
