package cn.blmdz.rabbit.order.service;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkState;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.rabbit.order.dao.OrderFinishInfoDao;
import cn.blmdz.rabbit.order.dao.OrderMoneyFlowDao;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.model.OrderMoneyFlow;
import cn.blmdz.rabbit.order.service.OrderSettlementWriteService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/23/16
 * Time: 8:23 PM
 */
@Component
@Slf4j
public class OrderSettlementWriteServiceImpl implements OrderSettlementWriteService {

    @Autowired
    private OrderMoneyFlowDao orderMoneyFlowDao;
    @Autowired
    private OrderFinishInfoDao orderFinishInfoDao;

    @Override
    public Response<Boolean> createOrderMoneyFlow(OrderMoneyFlow flow) {
        Response<Boolean> result = new Response<Boolean>();
        try {

            orderMoneyFlowDao.create(flow);
            result.setResult(Boolean.TRUE);
        }catch (Exception e){
            log.error("crate order money flow: {} fail,cause: {}",flow,Throwables.getStackTraceAsString(e));
            result.setError("create.order.money.flow.fail");
        }
        return result;
    }

    @Override
    public Response<Boolean> updateOrderMoneyFlowSettlemented(Long flowId) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            checkArgument(Arguments.notNull(flowId),"order.money.flow.id.invalid");
            OrderMoneyFlow exist = orderMoneyFlowDao.findById(flowId);
            checkState(Arguments.notNull(exist),"order.money.flow.not.exist");

            orderMoneyFlowDao.updateSettlemented(flowId);
            result.setResult(Boolean.TRUE);
        }catch (IllegalArgumentException | IllegalStateException e){
            log.error("update order money flow id: {} settlemented fail,error: {}",flowId, e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("update order money flow id: {} settlemented fail,cause: {}",flowId, Throwables.getStackTraceAsString(e));
            result.setError("update.order.money.flow.fail");
        }
        return result;
    }

    @Override
    public Response<Boolean> createOrderFinishInfo(OrderFinishInfo info) {
        Response<Boolean> result = new Response<Boolean>();
        try {

            orderFinishInfoDao.create(info);
            result.setResult(Boolean.TRUE);
        }catch (Exception e){
            log.error("crate order finish info: {} fail,cause: {}",info,Throwables.getStackTraceAsString(e));
            result.setError("create.order.finish.info.fail");
        }
        return result;    }

    @Override
    public Response<Boolean> updateOrderFinishInfoSettlemented(Long infoId) {
        Response<Boolean> result = new Response<Boolean>();
        try {
            checkArgument(Arguments.notNull(infoId),"order.finish.info.id.invalid");
            OrderFinishInfo exist = orderFinishInfoDao.findById(infoId);
            checkState(Arguments.notNull(exist),"order.finish.info.not.exist");

            orderFinishInfoDao.updateSettlemented(infoId);
            result.setResult(Boolean.TRUE);
        }catch (IllegalArgumentException | IllegalStateException e){
            log.error("update order finish info id: {} settlemented fail,error: {}",infoId, e.getMessage());
            result.setError(e.getMessage());
        }catch (Exception e){
            log.error("update order finish info id: {} settlemented fail,cause: {}",infoId, Throwables.getStackTraceAsString(e));
            result.setError("update.order.finish.info.fail");
        }
        return result;
    }
}
