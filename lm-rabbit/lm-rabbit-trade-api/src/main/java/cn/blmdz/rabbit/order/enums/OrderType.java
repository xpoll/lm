package cn.blmdz.rabbit.order.enums;

/**
 * Desc: 订单类型,每个行业解决方案可以重新定义订单类型
 * Mail: F@terminus.io
 * Data: 16/3/1
 * Author: yangzefeng
 */
public enum OrderType {

    MERGE_ORDER(1, "mergeOrder"),
    SHOP_ORDER(2,"shopOrder"),
    SKU_ORDER(3,"skuOrder"),
    MERGE_ORDER_REFUND(4, "mergeOrderRefund"),
    SHOP_ORDER_REFUND(5, "shopOrderRefund"),
    SKU_ORDER_REFUND(6, "skuOrderRefund");

    private final int value;

    private final String description;

    OrderType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int value() {
        return this.value;
    }


    public static OrderType from(int value) {
        for(OrderType ta: OrderType.values()) {
            if(ta.value==value) {
                return ta;
            }
        }

        return null;
    }


    @Override
    public String toString() {
        return description;
    }
}
