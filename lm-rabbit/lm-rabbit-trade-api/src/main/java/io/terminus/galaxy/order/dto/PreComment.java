package io.terminus.galaxy.order.dto;

import io.terminus.parana.order.model.SkuOrder;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

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