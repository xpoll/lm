package io.terminus.galaxy.settlement.enums;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/24/16
 * Time: 1:57 PM
 */
public enum CommissionBusinessType {


    SHOP(1, "店铺"),
    CATEGORY(2,"类目"),
    BRAND(3,"品牌");



    private final int value;

    private final String description;

    private CommissionBusinessType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public static CommissionBusinessType from(int value) {
        for (CommissionBusinessType ta : CommissionBusinessType.values()) {
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
