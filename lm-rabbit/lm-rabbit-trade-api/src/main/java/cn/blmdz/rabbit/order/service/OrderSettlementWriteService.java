package cn.blmdz.rabbit.order.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.model.OrderMoneyFlow;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/23/16
 * Time: 8:16 PM
 */
public interface OrderSettlementWriteService {

    /**
     * 创建订单资金流水
     * @param flow 流水
     * @return 是否创建成功
     */
    Response<Boolean> createOrderMoneyFlow(OrderMoneyFlow flow);

    /**
     * 更新订单资金流水为结算完成
     * @param flowId 流水id
     * @return 是否更新成功
     */
    Response<Boolean> updateOrderMoneyFlowSettlemented(Long flowId);

    /**
     * 创建订单完成节点信息
     * @param info 节点信息
     * @return 是否创建成功
     */
    Response<Boolean> createOrderFinishInfo(OrderFinishInfo info);

    /**
     * 更新完成订单的结算状态
     * @param infoId 主键
     * @return 是否更新成功
     */
    Response<Boolean> updateOrderFinishInfoSettlemented(Long infoId);
}
