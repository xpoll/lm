package cn.blmdz.rabbit.order.enums;

/**
 * Desc: 订单流水类型
 * Author: songrenfei
 */
public enum OrderMoneyFlowType {

    PAID(1, "支付"),
    SALE_REFUND(2,"售中退款"),
    AFTER_REFUND(3,"售后退款");

    private final int value;

    private final String description;

    OrderMoneyFlowType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int value() {
        return this.value;
    }


    public static OrderMoneyFlowType from(int value) {
        for(OrderMoneyFlowType ta: OrderMoneyFlowType.values()) {
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
