package cn.blmdz.rabbit.item.dto;

import lombok.Data;

import java.io.Serializable;
import java.util.Map;

/**
 * @Author xgs.
 * @Email xgs@terminus.io
 * @Date 16/8/3
 */

@Data
public class SearchItemDto implements Serializable{
    public static final long serialVersionUID = -7006364224642283042L;

    public static final String FROM="起始港";

    public static final String TO="目的港";

    public static final String COMPANY = "承运公司";

    public static final String VALIDITY = "有效期";


    private Long itemId;

    /**
     * 起始港
     */
    private String from;

    /**
     * 目的港
     */
    private String to;

    /**
     * key:20GP, 40GP, 40HC, 45HC
     */
    private Map<String, Integer> price;

    /**
     * 承运公司
     */
    private String company;


    /**
     * 有效期
     */
    private String validity;

}
