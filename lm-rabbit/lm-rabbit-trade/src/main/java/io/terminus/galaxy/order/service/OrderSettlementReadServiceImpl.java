package io.terminus.galaxy.order.service;

import com.google.common.base.Function;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import io.terminus.common.model.PageInfo;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.galaxy.order.dao.OrderFinishInfoDao;
import io.terminus.galaxy.order.dao.OrderMoneyFlowDao;
import io.terminus.galaxy.order.enums.OrderStatus;
import io.terminus.galaxy.order.model.OrderFinishInfo;
import io.terminus.galaxy.order.model.OrderMoneyFlow;
import io.terminus.parana.order.model.SkuOrder;
import io.terminus.parana.order.model.SkuOrderRefund;
import io.terminus.parana.order.service.OrderReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Nullable;
import java.util.List;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/23/16
 * Time: 4:59 PM
 */
@Component
@Slf4j
public class OrderSettlementReadServiceImpl implements OrderSettlementReadService {


    @Autowired
    private OrderMoneyFlowDao orderMoneyFlowDao;

    @Autowired
    private OrderFinishInfoDao orderFinishInfoDao;

    @Autowired
    private OrderReadService orderReadService;


    @Override
    public Response<Paging<OrderMoneyFlow>> findNeedSettlementOrderMoneyFlows( Integer pageNo, Integer size) {
        Response<Paging<OrderMoneyFlow>> result = new Response<Paging<OrderMoneyFlow>>();
        try {
            PageInfo pageInfo = new PageInfo(pageNo, size);
            result.setResult(orderMoneyFlowDao.findNeedSettlements(pageInfo.getOffset(), pageInfo.getLimit()));
        }catch (Exception e){
            log.error("query need sum channel details fail cause:{}", Throwables.getStackTraceAsString(e));
            result.setError("find.pay.channel.fail");
        }

        return result;
    }

    @Override
    public Response<Paging<OrderFinishInfo>> findNeedSettlementOrderFinishInfos( Integer pageNo, Integer size) {
        Response<Paging<OrderFinishInfo>> result = new Response<Paging<OrderFinishInfo>>();
        try {
            PageInfo pageInfo = new PageInfo(pageNo, size);
            result.setResult(orderFinishInfoDao.findNeedSettlements(pageInfo.getOffset(), pageInfo.getLimit()));
        } catch (Exception e){
            log.error("query need sum channel details fail cause:{}", Throwables.getStackTraceAsString(e));
            result.setError("find.pay.channel.fail");
        }

        return result;
    }

    @Override
    public Response<List<SkuOrderRefund>> findRefundOrder(Long shopOrderId,Integer type) {
        Response<List<SkuOrderRefund>> result = new Response<List<SkuOrderRefund>>();

        try {
            checkArgument(Arguments.notNull(shopOrderId),"shop.id.invalid");
            checkArgument(Arguments.notNull(type),"refund.type.invalid");
            Response<List<SkuOrder>> skuOrderRes = orderReadService.findSkuOrderByParentId(shopOrderId);
            checkState(skuOrderRes.isSuccess(),skuOrderRes.getError());

            List<Long> skuOrderIds = Lists.transform(skuOrderRes.getResult(), new Function<SkuOrder, Long>() {
                @Nullable
                @Override
                public Long apply(SkuOrder skuOrder) {
                    return skuOrder.getId();
                }
            });

            Response<List<SkuOrderRefund>> refundRes = orderReadService.findSkuOrderRefundByParentIds(skuOrderIds);
            checkState(refundRes.isSuccess(),refundRes.getError());
            List<SkuOrderRefund> refunds = refundRes.getResult();

            if(type.equals(1)){
                result.setResult(getSalRefundOrder(refunds));
            }else {
                result.setResult(getAfterSaleRefundOrder(refunds));
            }

        }catch (IllegalArgumentException | IllegalStateException e){
            log.error("find refund order by shop order id: {} type: {} fail, error: {}",shopOrderId,type,e.getMessage());
            result.setError(e.getMessage());

        }catch (Exception e){
            log.error("find refund order by shop order id: {} type: {} fail, cause: {}",shopOrderId,type,Throwables.getStackTraceAsString(e));
            result.setError("find.refund.order.fail");

        }
        return result;
    }

    @Override
    public Response<OrderMoneyFlow> findOrderMoneyFlowById(Long flowId) {
        Response<OrderMoneyFlow> result = new Response<OrderMoneyFlow>();
        try {
            checkArgument(Arguments.notNull(flowId),"order.money.flow.id.invalid");

            OrderMoneyFlow flow = orderMoneyFlowDao.findById(flowId);
            checkState(Arguments.notNull(flow),"order.money.flow.not.exist");
            result.setResult(flow);
        }catch (IllegalArgumentException | IllegalStateException e){
            log.error("find order money flow by id: {} fail, error:{} ",flowId,e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("find order money flow by id: {} fail, cause:{} ",flowId,Throwables.getStackTraceAsString(e));
            result.setError("query.order.money.flow.fail");
        }
        return result;
    }

    @Override
    public Response<OrderFinishInfo> findOrderFinishInfoById(Long infoId) {
        Response<OrderFinishInfo> result = new Response<OrderFinishInfo>();
        try {
            checkArgument(Arguments.notNull(infoId),"order.finish.info.id.invalid");

            OrderFinishInfo info = orderFinishInfoDao.findById(infoId);
            checkState(Arguments.notNull(info),"order.finish.info.not.exist");
            result.setResult(info);
        }catch (IllegalArgumentException | IllegalStateException e){
            log.error("find order finish info by id: {} fail, error:{} ",infoId,e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("find order finish info by id: {} fail, cause:{} ",infoId,Throwables.getStackTraceAsString(e));
            result.setError("query.order.finish.info.fail");
        }
        return result;
    }


    //售中退款
    private List<SkuOrderRefund> getSalRefundOrder(List<SkuOrderRefund> refunds){
        List<SkuOrderRefund> refundList = Lists.newArrayList();
        if(Arguments.isNullOrEmpty(refunds)){
            return refundList;
        }
        for (SkuOrderRefund refund : refunds){
            if(Lists.newArrayList(OrderStatus.SKU_REFUND_CANCEL_BY_REFUND.value(),OrderStatus.SKU_REFUND_CANCEL_BY_RETURNS.value()).contains(refund.getNodeInstanceId())){
                refundList.add(refund);
            }
        }
        return refundList;
    }

    //售后退款
    private List<SkuOrderRefund> getAfterSaleRefundOrder(List<SkuOrderRefund> refunds){

        //todo
        return null;
    }
}
