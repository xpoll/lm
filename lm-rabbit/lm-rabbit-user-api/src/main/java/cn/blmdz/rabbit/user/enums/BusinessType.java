package cn.blmdz.rabbit.user.enums;



/**
 * Created by liushaofei on 16/7/30.
 */
public enum  BusinessType {
    MERGE_ORDER(1, "declare"),
    SHOP_ORDER(2,"book"),
    SKU_ORDER(3,"others");

    private final int value;

    private final String description;

    BusinessType(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public int value() {
        return this.value;
    }

    public String getDescription(){
        return this.description;
    }

    public static BusinessType from(long value) {
        for(BusinessType ta: BusinessType.values()) {
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
