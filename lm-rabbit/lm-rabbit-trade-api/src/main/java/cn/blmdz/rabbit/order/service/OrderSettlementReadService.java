package cn.blmdz.rabbit.order.service;

import java.util.List;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.model.OrderMoneyFlow;
import cn.blmdz.wolf.order.model.SkuOrderRefund;

/**
 * 订单结算相关读服务
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/23/16
 * Time: 4:56 PM
 */
public interface OrderSettlementReadService {


    /**
     * 获取需要参与结算的订单资金流水明细
     * @pageNo 当前页
     * @size    每页显示条数
     * @return 支付渠道信息
     */
    Response<Paging<OrderMoneyFlow>> findNeedSettlementOrderMoneyFlows( Integer pageNo, Integer size);

    /**
     * 获取需要参与结算的 完成订单
     * @pageNo 当前页
     * @size    每页显示条数
     * @return 支付渠道信息
     */
    Response<Paging<OrderFinishInfo>> findNeedSettlementOrderFinishInfos( Integer pageNo, Integer size);

    /**
     * 查询订单所属的退款单
     * @param shopOrderId 店铺单id
     * @param type
     * @return 售中退款单
     */
    Response<List<SkuOrderRefund>> findRefundOrder(Long shopOrderId,Integer type);

    Response<OrderMoneyFlow> findOrderMoneyFlowById(Long flowId);

    Response<OrderFinishInfo> findOrderFinishInfoById(Long infoId);
}
