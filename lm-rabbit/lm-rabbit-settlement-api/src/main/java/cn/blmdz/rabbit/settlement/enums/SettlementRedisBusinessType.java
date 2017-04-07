package cn.blmdz.rabbit.settlement.enums;

/**
 * 处理结算中不同业务类型账务 根据标记位筛出待处理的记录id保存到redis中供结算job使用
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/24/16
 * Time: 1:57 PM
 */
public enum SettlementRedisBusinessType {


    ORDER_MONEY_FLOW(1, "订单资金流水"),
    ORDER_FINISH_INFO(2,"完成订单信息"),
    PAY_CHANNEL(3,"支付渠道信息"),
    PAY_CHANNEL_DETAIL(4,"支付渠道账务信息"),
    PAY_CHANNEL_DETAIL_CHECK(5,"支付渠道账务信息-对账"),
    ORDER_SETTLEMENT_CHECK(6,"订单账务信息-对账"),
    AFTER_SETTLEMENT_CHECK(7,"售后账务信息-对账");



    private final int value;

    private final String description;

    private SettlementRedisBusinessType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public static SettlementRedisBusinessType from(int value) {
        for (SettlementRedisBusinessType ta : SettlementRedisBusinessType.values()) {
            if (ta.value == value) {
                return ta;
            }
        }
        return null;
    }

    public int value() {
        return this.value;
    }


    @Override
    public String toString() {
        return description;
    }
}
