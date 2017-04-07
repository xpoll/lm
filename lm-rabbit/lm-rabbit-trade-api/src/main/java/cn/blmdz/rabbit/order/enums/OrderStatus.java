package cn.blmdz.rabbit.order.enums;

/**
 * Desc: 订单状态,枚举值对应的OrderNodeInstanceId
 * Mail: F@terminus.io
 * Data: 16/3/9
 * Author: yangzefeng
 */
public enum OrderStatus {

//    shop order status
    WAIT_FOR_PAY(1L, "等待买家付款"),
    WAIT_FOR_DELIVE(2L, "等待卖家发货"),
    DELIVED(3L, "卖家已发货"),
    DONE(4L, "交易完成"),
    BUYER_CANCEL(5L, "买家取消订单"),
    SELLER_CANCEL(6L, "卖家取消订单"),
    APPLY_REFUND(7L, "店铺订单-申请退款"),
    CANCEL_BY_REFUND(8L, "店铺订单-同意退款,退款成功"),
    CANCEL_BY_REFUND_CAUSE_BY_SKU_ORDER_REFUND(39L, "店铺订单-同意退款,退款成功,由skuOrder退款触发"),
    REFUSE_REFUND(9L, "店铺订单-拒绝退款"),
    APPLY_RETURNS(10L, "店铺订单-申请退货"),
    AGREE_RETURNS(11L, "店铺订单-同意退货"),
    REFUSE_RETURNS(12L, "店铺订单-拒绝退货"),
    DELIVE_RETURNS(13L, "店铺订单-已发还货物"),
    CANCEL_BY_RETURNS(14L, "店铺订单-确认收到退货,退款成功"),
    CANCEL_BY_RETURNS_CAUSE_BY_SKU_RETURNS(41L, "店铺订单-退货退款成功,由skuOrder触发的退款"),

//    sku order status
    SKU_ORDER_WAIT_FOR_PAY(15L, "sku订单-等待买家付款"),
    SKU_ORDER_WAIT_FOR_DELIVE(16L, "sku订单-等待卖家发货"),
    SKU_ORDER_DELIVED(17L, "sku订单-卖家已发货"),
    SKU_ORDER_DONE(18L, "sku订单-交易完成"),
    SKU_ORDER_CANCEL_BY_BUYER(19L, "sku订单-买家取消订单"),
    SKU_ORDER_CANCEL_BY_SELLER(20L, "sku订单-卖家取消订单"),
    SKU_ORDER_APPLY_REFUND(21L, "sku订单-申请退款"),
    SKU_ORDER_CANCEL_BY_REFUND(22L, "sku订单-同意退款,订单取消"),
    SKU_ORDER_CANCEL_BY_REFUND_CAUSE_BY_SKU_REFUND(43L, "sku订单-同意退款,由退款直接从等待发货跳转"),
    SKU_ORDER_REFUSE_REFUND(23L, "sku订单-拒绝退款"),
    SKU_ORDER_APPLY_RETURNS(24L, "sku订单-申请退货"),
    SKU_ORDER_AGREE_RETURNS(25L, "sku订单-同意退货"),
    SKU_ORDER_REFUSE_RETURNS(26L, "sku订单-拒绝退货"),
    SKU_ORDER_DELIVE_RETURNS(27L, "sku订单-已发退还货物"),
    SKU_ORDER_CANCEL_BY_RETURNS(28L, "sku订单-确认收到退货,退款成功"),
    SKU_ORDER_CANCEL_BY_RETURNS_CAUSE_BY_SKU_RETURNS(42, "sku订单-由退款直接从发货跳转"),

//    sku refund order status
    SKU_REFUND_APPLY_REFUND(29L, "sku退款单-申请退款"),
    SKU_REFUND_CANCEL_BY_REFUND(30L, "sku退款单-同意退款, 订单取消"),
    SKU_REFUND_REFUSE_REFUND(31L, "sku退款单-拒绝退款"),
    SKU_REFUND_APPLY_RETURNS(32L, "sku退款单-申请退货"),
    SKU_REFUND_AGREE_RETURNS(33L, "sku退款单-同意退货"),
    SKU_REFUND_REFUSE_RETURNS(34L, "sku退款单-拒绝退货"),
    SKU_REFUND_DELIVE_RETURNS(35L, "sku退款单-已发退款货物"),
    SKU_REFUND_CANCEL_BY_RETURNS(36L, "sku退款单-确认收到退货,退款成功"),
    SKU_REFUND_UNDO(37L, "sku退款单-撤销退款"),
    SKU_RETURNS_APPLY_UNDO(38L, "sku退款单-撤销退货, 申请之后"),
    SKU_RETURNS_AGREE_UNDO(40L, "sku退款单-撤销退货,卖家同意之后");

    private final long value;

    private final String description;

    OrderStatus(long value, String description) {
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
