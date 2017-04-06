package io.terminus.galaxy.order.enums;

/**
 * Desc: 订单动作类型
 * Mail: F@terminus.io
 * Data: 16/3/4
 * Author: yangzefeng
 */
public enum OrderActionType {

    NORMAL(1, "普通任务"),
    SCHEDULE(2, "定时任务"),
    NOTIFY(3, "通知");

    private final int value;

    private final String description;

    OrderActionType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int value() {
        return this.value;
    }

    @Override
    public String toString() {
        return description;
    }
}
