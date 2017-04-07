package cn.blmdz.rabbit.schedule.service;

import static com.google.common.base.Preconditions.checkState;

import java.util.List;

import javax.annotation.Nullable;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Function;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.model.OrderMoneyFlow;
import cn.blmdz.rabbit.order.service.OrderSettlementReadService;
import cn.blmdz.rabbit.settlement.enums.SettlementRedisBusinessType;
import cn.blmdz.rabbit.settlement.service.GalaxySettlementWriteService;
import cn.blmdz.wolf.pay.model.PayChannel;
import cn.blmdz.wolf.pay.service.PayReadService;
import cn.blmdz.wolf.settlement.model.AfterSalesSettlement;
import cn.blmdz.wolf.settlement.model.OrderSettlement;
import cn.blmdz.wolf.settlement.model.PayChannelDetail;
import cn.blmdz.wolf.settlement.service.SettlementReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/28/16
 * Time: 1:49 PM
 */
@Slf4j @Component
public class SettlementRedisJobService {

    @Autowired
    private PayReadService payReadService;
    @Autowired
    private GalaxySettlementWriteService galaxySettlementWriteService;
    @Autowired
    private SettlementReadService settlementReadService;
    @Autowired
    private OrderSettlementReadService orderSettlementReadService;



    private final Integer BATCH_SIZE = 100;     // 批处理数量



    public Response<Boolean> handlePayChannelRedisIds() {

        Response<Boolean> result = new Response<Boolean>();


        try {

            int pageNo = 1;
            boolean next = batcHandlePayChannelRedisIds(pageNo, BATCH_SIZE);
            while (next) {
                 pageNo++;
                next = batcHandlePayChannelRedisIds(pageNo, BATCH_SIZE);
            }

            result.setResult(Boolean.TRUE);

        } catch (Exception e) {
            log.error("handle pay channel redis ids fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }

    @SuppressWarnings("unchecked")
    private boolean batcHandlePayChannelRedisIds(final int pageNo, int size) {

        Response<Paging<PayChannel>> payChannelRes = payReadService.pagingNeedGenerateChannelDetails(pageNo, size);
        checkState(payChannelRes.isSuccess(), payChannelRes.getError());

        Paging<PayChannel> paging = payChannelRes.getResult();
        List<PayChannel> channels = paging.getData();

        if (paging.getTotal().equals(0L) || CollectionUtils.isEmpty(channels)) {

            return Boolean.FALSE;
        }

        List<Long> ids = Lists.transform(channels, new Function<PayChannel, Long>() {
            @Nullable
            @Override
            public Long apply(PayChannel payChannel) {
                return payChannel.getId();
            }
        });

        try {
            Response<Boolean> redisIdsRes = galaxySettlementWriteService.saveSettlementRedisIds(SettlementRedisBusinessType.PAY_CHANNEL, ids);

            checkState(redisIdsRes.isSuccess(),redisIdsRes.getError());

            }catch (IllegalStateException e){
                log.error("handle pay channel redis ids fail,error: {}",e.getMessage());
            }catch (Exception e){
                log.error("handle pay channel redis ids fail,cause: {}",Throwables.getStackTraceAsString(e));
            }

        int current = channels.size();
        return current == size;  // 判断是否存在下一个要处理的批次
    }





    public Response<Boolean> handlePayChannelDetailCheckRedisIds() {

        Response<Boolean> result = new Response<Boolean>();

        try {

            int pageNo = 1;
            boolean next = batcHandlePayChannelDetailCheckRedisIds(pageNo, BATCH_SIZE);
            while (next) {
                pageNo++;
                next = batcHandlePayChannelDetailCheckRedisIds(pageNo, BATCH_SIZE);
            }

            result.setResult(Boolean.TRUE);

        } catch (Exception e) {
            log.error("handle pay channel detail check redis ids fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }

    @SuppressWarnings("unchecked")
    private boolean batcHandlePayChannelDetailCheckRedisIds(final int pageNo, int size) {

        Response<Paging<PayChannelDetail>> paychannelDetailRes = settlementReadService.findPaychannelDetailNeedCheck(pageNo, size);
        checkState(paychannelDetailRes.isSuccess(), paychannelDetailRes.getError());

        Paging<PayChannelDetail> paging = paychannelDetailRes.getResult();
        List<PayChannelDetail> channels = paging.getData();

        if (paging.getTotal().equals(0L) || CollectionUtils.isEmpty(channels)) {

            return Boolean.FALSE;
        }

        List<Long> ids = Lists.transform(channels, new Function<PayChannelDetail, Long>() {
            @Nullable
            @Override
            public Long apply(PayChannelDetail payChannel) {
                return payChannel.getId();
            }
        });

        try {
            Response<Boolean> redisIdsRes = galaxySettlementWriteService.saveSettlementRedisIds(SettlementRedisBusinessType.PAY_CHANNEL_DETAIL_CHECK, ids);

            checkState(redisIdsRes.isSuccess(),redisIdsRes.getError());

        }catch (IllegalStateException e){
            log.error("handle pay channel detail check redis ids fail,error: {}",e.getMessage());
        }catch (Exception e){
            log.error("handle pay channel detail check redis ids fail,cause: {}",Throwables.getStackTraceAsString(e));
        }

        int current = channels.size();
        return current == size;  // 判断是否存在下一个要处理的批次
    }





    public Response<Boolean> handleOrderMoneyFlowRedisIds() {

        Response<Boolean> result = new Response<Boolean>();

        try {

            int pageNo = 1;
            boolean next = batcHandleOrderMoneyFlowRedisIds(pageNo, BATCH_SIZE);
            while (next) {
                pageNo++;
                next = batcHandleOrderMoneyFlowRedisIds(pageNo, BATCH_SIZE);
            }

            result.setResult(Boolean.TRUE);

        } catch (Exception e) {
            log.error("handle order money flow redis ids fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }

    @SuppressWarnings("unchecked")
    private boolean batcHandleOrderMoneyFlowRedisIds(final int pageNo, int size) {

        Response<Paging<OrderMoneyFlow>> orderMoneyFlowRes = orderSettlementReadService.findNeedSettlementOrderMoneyFlows(pageNo, size);
        checkState(orderMoneyFlowRes.isSuccess(), orderMoneyFlowRes.getError());

        Paging<OrderMoneyFlow> paging = orderMoneyFlowRes.getResult();
        List<OrderMoneyFlow> orderMoneyFlows = paging.getData();

        if (paging.getTotal().equals(0L) || CollectionUtils.isEmpty(orderMoneyFlows)) {

            return Boolean.FALSE;
        }

        List<Long> ids = Lists.transform(orderMoneyFlows, new Function<OrderMoneyFlow, Long>() {
            @Nullable
            @Override
            public Long apply(OrderMoneyFlow orderMoneyFlow) {
                return orderMoneyFlow.getId();
            }
        });

        try {
            Response<Boolean> redisIdsRes = galaxySettlementWriteService.saveSettlementRedisIds(SettlementRedisBusinessType.ORDER_MONEY_FLOW, ids);

            checkState(redisIdsRes.isSuccess(),redisIdsRes.getError());

        }catch (IllegalStateException e){
            log.error("handle order money flow redis ids fail,error: {}",e.getMessage());
        }catch (Exception e){
            log.error("handle order money flow  redis ids fail,cause: {}",Throwables.getStackTraceAsString(e));
        }

        int current = orderMoneyFlows.size();
        return current == size;  // 判断是否存在下一个要处理的批次
    }





    public Response<Boolean> handleOrderFinishInfoRedisIds() {

        Response<Boolean> result = new Response<Boolean>();
        try {

            int pageNo = 1;
            boolean next = batcHandleOrderFinishInfoRedisIds(pageNo, BATCH_SIZE);
            while (next) {
                pageNo++;
                next = batcHandleOrderFinishInfoRedisIds(pageNo, BATCH_SIZE);
            }

            result.setResult(Boolean.TRUE);

        } catch (Exception e) {
            log.error("handle order finish info redis ids fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }

    @SuppressWarnings("unchecked")
    private boolean batcHandleOrderFinishInfoRedisIds(final int pageNo, int size) {

        Response<Paging<OrderFinishInfo>> orderFinishInfoRes = orderSettlementReadService.findNeedSettlementOrderFinishInfos(pageNo, size);
        checkState(orderFinishInfoRes.isSuccess(), orderFinishInfoRes.getError());

        Paging<OrderFinishInfo> paging = orderFinishInfoRes.getResult();
        List<OrderFinishInfo> orderFinishInfos = paging.getData();

        if (paging.getTotal().equals(0L) || CollectionUtils.isEmpty(orderFinishInfos)) {

            return Boolean.FALSE;
        }

        List<Long> ids = Lists.transform(orderFinishInfos, new Function<OrderFinishInfo, Long>() {
            @Nullable
            @Override
            public Long apply(OrderFinishInfo orderFinishInfo) {
                return orderFinishInfo.getId();
            }
        });

        try {
            Response<Boolean> redisIdsRes = galaxySettlementWriteService.saveSettlementRedisIds(SettlementRedisBusinessType.ORDER_FINISH_INFO, ids);

            checkState(redisIdsRes.isSuccess(),redisIdsRes.getError());

        }catch (IllegalStateException e){
            log.error("handle order finish info redis ids fail,error: {}",e.getMessage());
        }catch (Exception e){
            log.error("handle order finish info  redis ids fail,cause: {}",Throwables.getStackTraceAsString(e));
        }

        int current = orderFinishInfos.size();
        return current == size;  // 判断是否存在下一个要处理的批次
    }






    public Response<Boolean> handleAfterSettlementRedisIds() {

        Response<Boolean> result = new Response<Boolean>();
        try {

            int pageNo = 1;
            boolean next = batcHandleAfterSettlementRedisIds(pageNo, BATCH_SIZE);
            while (next) {
                pageNo++;
                next = batcHandleAfterSettlementRedisIds(pageNo, BATCH_SIZE);
            }

            result.setResult(Boolean.TRUE);

        } catch (Exception e) {
            log.error("handle after sale settlement check redis ids fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }

    @SuppressWarnings("unchecked")
    private boolean batcHandleAfterSettlementRedisIds(final int pageNo, int size) {

        Response<Paging<AfterSalesSettlement>> afterRes = settlementReadService.findAfterSalesNeendCheck(pageNo, size);
        checkState(afterRes.isSuccess(), afterRes.getError());

        Paging<AfterSalesSettlement> paging = afterRes.getResult();
        List<AfterSalesSettlement> afterSalesSettlements = paging.getData();

        if (paging.getTotal().equals(0L) || CollectionUtils.isEmpty(afterSalesSettlements)) {

            return Boolean.FALSE;
        }

        List<Long> ids = Lists.transform(afterSalesSettlements, new Function<AfterSalesSettlement, Long>() {
            @Nullable
            @Override
            public Long apply(AfterSalesSettlement afterSalesSettlement) {
                return afterSalesSettlement.getId();
            }
        });

        try {
            Response<Boolean> redisIdsRes = galaxySettlementWriteService.saveSettlementRedisIds(SettlementRedisBusinessType.AFTER_SETTLEMENT_CHECK, ids);

            checkState(redisIdsRes.isSuccess(),redisIdsRes.getError());

        }catch (IllegalStateException e){
            log.error("handle after sale settlement redis ids fail,error: {}",e.getMessage());
        }catch (Exception e){
            log.error("handle after sale settlement  redis ids fail,cause: {}",Throwables.getStackTraceAsString(e));
        }

        int current = afterSalesSettlements.size();
        return current == size;  // 判断是否存在下一个要处理的批次
    }





    public Response<Boolean> handleSettlementCheckRedisIds() {

        Response<Boolean> result = new Response<Boolean>();
        try {

            int pageNo = 1;
            boolean next = batcHandleSettlementCheckRedisIds(pageNo, BATCH_SIZE);
            while (next) {
                pageNo++;
                next = batcHandleSettlementCheckRedisIds(pageNo, BATCH_SIZE);
            }

            result.setResult(Boolean.TRUE);

        } catch (Exception e) {
            log.error("handle settlement check redis ids fail,cause:{}", Throwables.getStackTraceAsString(e));

        }
        return result;

    }

    @SuppressWarnings("unchecked")
    private boolean batcHandleSettlementCheckRedisIds(final int pageNo, int size) {

        Response<Paging<OrderSettlement>> settlementRes = settlementReadService.pagingSettlementByIsChecked(pageNo, size, Boolean.FALSE);
        checkState(settlementRes.isSuccess(), settlementRes.getError());

        Paging<OrderSettlement> paging = settlementRes.getResult();
        List<OrderSettlement> orderSettlements = paging.getData();

        if (paging.getTotal().equals(0L) || CollectionUtils.isEmpty(orderSettlements)) {

            return Boolean.FALSE;
        }

        List<Long> ids = Lists.transform(orderSettlements, new Function<OrderSettlement, Long>() {
            @Nullable
            @Override
            public Long apply(OrderSettlement orderSettlement) {
                return orderSettlement.getId();
            }
        });

        try {
            Response<Boolean> redisIdsRes = galaxySettlementWriteService.saveSettlementRedisIds(SettlementRedisBusinessType.ORDER_SETTLEMENT_CHECK, ids);

            checkState(redisIdsRes.isSuccess(),redisIdsRes.getError());

        }catch (IllegalStateException e){
            log.error("handle order settlement check redis ids fail,error: {}",e.getMessage());
        }catch (Exception e){
            log.error("handle order  settlement check  redis ids fail,cause: {}",Throwables.getStackTraceAsString(e));
        }

        int current = orderSettlements.size();
        return current == size;  // 判断是否存在下一个要处理的批次
    }











}
