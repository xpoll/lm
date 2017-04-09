package cn.blmdz.rabbit.order.dto;

import java.io.Serializable;
import java.util.Map;

import cn.blmdz.wolf.item.model.Sku;
import lombok.Data;

/**
 * Desc: 下单预览页skuOrder信息
 * Mail: F@terminus.io
 * Data: 16/3/5
 * Author: yangzefeng
 */
@Data
public class PreOrderItem implements Serializable {
    private static final long serialVersionUID = -3824449619351981034L;

    /**
     * sku信息
     */
    private Sku sku;

    /**
     * 商品名称
     */
    private String itemName;

    /**
     * 商品主图
     */
    private String itemImage;

    /**
     * 购买数量
     */
    private Integer quantity;

    /**
     * sku单价 * quantity, 帮前端算一把
     */
    private Integer fee;

    /**
     * 各个具体项目需要添加的信息放这里
     */
    private Map<String, Object> extraJson;
}
