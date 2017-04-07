package cn.blmdz.rabbit.item.Enums;

/**
 * @Author xgs.
 * @Email xgs@terminus.io
 * @Date 16/8/3
 */
public enum Size {


    SIZE_20GP("20GP", "20GP"),
    SIZE_40GP("40GP","40GP"),
    SIZE_40HC("40HC","40HC"),
    SIZE_45HC("45HC", "45HC");

    private final String value;

    private final String description;

    Size(String value, String description) {
        this.value = value;
        this.description = description;
    }

    public String value() {
        return this.value;
    }


    public static Size from(String value) {
        for(Size ta: Size.values()) {
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
