package cn.blmdz.rabbit.web.core.trade.action.onsalereturns;

import com.google.common.collect.ImmutableSet;

import cn.blmdz.rabbit.order.enums.OrderStatus;

/**
 * DATE: 16/3/12 下午2:31 <br>
 * MAIL: zhanghecheng@terminus.io <br>
 * AUTHOR: zhanghecheng
 */
public class OnSaleReturnActionBase {

    /**
     * 售中退款相关的订单状态
     */
    protected ImmutableSet<Long> AllOnSaleReturnStatus=ImmutableSet.of(
            OrderStatus.SKU_REFUND_APPLY_RETURNS.value(),
            OrderStatus.SKU_REFUND_AGREE_RETURNS.value(),
            OrderStatus.SKU_REFUND_DELIVE_RETURNS.value(),
            OrderStatus.SKU_REFUND_CANCEL_BY_RETURNS.value(),
            OrderStatus.SKU_REFUND_REFUSE_RETURNS.value(),
            OrderStatus.SKU_RETURNS_AGREE_UNDO.value(),
            OrderStatus.SKU_RETURNS_APPLY_UNDO.value()
    );

    /**
     * 售中退款中的叶子状态
     */
    protected  ImmutableSet<Long> LeafOnSaleReturnStatus=ImmutableSet.of(
            OrderStatus.SKU_REFUND_CANCEL_BY_RETURNS.value(),
            OrderStatus.SKU_REFUND_REFUSE_RETURNS.value(),
            OrderStatus.SKU_RETURNS_AGREE_UNDO.value(),
            OrderStatus.SKU_RETURNS_APPLY_UNDO.value()
    );
}
