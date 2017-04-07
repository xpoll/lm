package cn.blmdz.rabbit.order.dto;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.wolf.order.model.ShopOrder;
import lombok.Data;

/**
 *  支付预览页
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/11/16
 * Time: 10:13 AM
 */
@Data
public class PrePay implements Serializable {


    private static final long serialVersionUID = -1646868231457684349L;


    private Long aid;

    private List<ShopOrder> shopOrders;

}
