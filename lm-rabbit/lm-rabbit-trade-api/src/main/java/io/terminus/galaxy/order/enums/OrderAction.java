package io.terminus.galaxy.order.enums;

/**
 * Desc: 订单操作枚举类,对应actionInstanceId
 * Mail: F@terminus.io
 * Data: 16/3/10
 * Author: yangzefeng
 */
public enum OrderAction {

//    shop order action
    PRE_PAY(1L, "订单支付"),
    DELIVE(2L, "订单发货"),
    CONFIRM(3L, "确认收货"),
    BUYER_CANCEL(4L, "买家取消订单"),
    SELLER_CANCEL(5L, "卖家取消订单"),
    APPLY_REFUND(6L, "申请退款"),
    AGREE_REFUND(7L, "同意退款"),
    REFUSE_REFUND(8L, "拒绝退款"),
    APPLY_RETURNS(9L, "申请退货"),
    AGREE_RETURNS(10L, "同意退货"),
    REFUSE_RETURNS(11L, "拒绝退货"),
    BUYER_DELIVE_RETURNS(12L, "买家发出退还货物"),
    SELLER_CONFIRM_RETURNS(13L, "卖家确认收到退还货物"),
    UNDO_REFUND(14L, "撤销退款请求"),
    UNDO_RETURNS_AFTER_APPLY(15L, "在申请后撤销退货申请"),
    UNDO_RETURNS_AFTER_AGREE(16L, "在卖家审核通过后撤销退货申请"),
    PAID(28L, "支付回调动作"),

//    sku order action
    SKU_ORDER_APPLY_REFUND(17L, "sku订单-申请退款"),
    SKU_ORDER_APPLY_RETURNS(20L, "sku订单-申请退货"),

//    sku order refund action
    SKU_ORDER_PRE_REFUND(18L, "sku退款单-同意退款"),
    SKU_ORDER_REFUSE_REFUND(19L, "拒绝退款"),
    SKU_ORDER_AGREE_RETURNS(21L, "同意退货"),
    SKU_ORDER_REFUSE_RETURNS(22L, "拒绝退货"),
    SKU_ORDER_BUYER_DELIVE_RETURNS(23L, "买家发出退还货物"),
    SKU_ORDER_PRE_RETURNS(24L, "卖家确认收到退还货物,发起退款"),
    SKU_ORDER_UNDO_REFUND(25L, "撤销退款申请"),
    SKU_ORDER_UNDO_RETURNS_AFTER_APPLY(26L, "在申请后撤销退货申请"),
    SKU_ORDER_UNDO_RETURNS_AFTER_AGREE(27L, "在卖家审核通过后撤销退货申请"),
    SKU_ORDER_REFUND_CALLBACK(29L, "退款回调"),
    SKU_ORDER_RETURNS_CALLBACK(30L, "退货退款回调");

    private final long value;

    private final String description;

    OrderAction(long value, String description) {
        this.value = value;
        this.description = description;
    }

    public long value() {
        return this.value;
    }


    @Override
    public String toString() {
        return description;
    }
}
