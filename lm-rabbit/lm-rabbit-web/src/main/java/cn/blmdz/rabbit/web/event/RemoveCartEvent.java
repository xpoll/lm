package cn.blmdz.rabbit.web.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.Serializable;
import java.util.List;

/**
 * Desc: 创建订单之后,移除购物车
 * Mail: F@terminus.io
 * Data: 16/4/14
 * Author: yangzefeng
 */
@AllArgsConstructor
public class RemoveCartEvent implements Serializable {
    private static final long serialVersionUID = 5604087533277467616L;

    @Getter
    private List<Long> skuIds;
    @Getter
    private Long userId;
}
