package io.terminus.galaxy.order.enums;

/**
 * Desc: 订单支付类型
 * Author: songrenfei
 */
public enum OrderPayType {

    ON_LINE(1, "在线支付"),
    OFF_ORDER(2,"货到付款"),
    INTEGRAL(3,"积分");

    private final int value;

    private final String description;

    OrderPayType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int value() {
        return this.value;
    }


    public static OrderPayType from(int value) {
        for(OrderPayType ta: OrderPayType.values()) {
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
