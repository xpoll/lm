package cn.blmdz.rabbit.order.dto;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.wolf.order.model.SkuOrder;
import lombok.Getter;
import lombok.Setter;

/**
 * Author:cp
 * Created on 4/28/16.
 */
public class PreComment implements Serializable {

    private static final long serialVersionUID = -6401684680148070991L;

    @Setter
    @Getter
    private List<SkuOrder> orderItems;
}