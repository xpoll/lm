package cn.blmdz.rabbit.order.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Desc: 下单预览页shopOder信息
 * Mail: F@terminus.io
 * Data: 16/3/5
 * Author: yangzefeng
 */
@Data
public class PreOrder implements Serializable {
    private static final long serialVersionUID = 3070873038645144826L;

    /**
     * 店铺id
     */
    private Long shopId;

    /**
     * 店铺名称
     */
    private String shopName;

    /**
     * skuOrder列表
     */
    private List<PreOrderItem> preOrderItems;

    /**
     * 各个具体项目需要添加的信息放这里
     */
    private Map<String, Object> extraJson;
}
